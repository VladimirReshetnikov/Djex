"""Pure guards for the extended corpus's exact observed search inventory."""
import unittest

from pathlib import Path
from types import SimpleNamespace

from behavior_extended_probe import (commands, observed_inventory,
                                     validate_cell_settings)
import behavior_extended_spec as spec

# Literal fresh Djinn browse rows, independent of the implementation allowlist.
BUILTIN_ROWS = (
    "data [] v0 = [] | (:) v0 [v0]",
    "data () = ()",
)
NUMERIC_ROWS = (
    "type Int :: Type",
    "ExtendedNumeric.numericZero :: Int",
    "ExtendedNumeric.numericSuccessor :: Int -> Int",
)


def transcript(engine, declarations):
    header = "-- Djinn" if engine == "djinn" else "-- Current source scope"
    return "\n".join([header, *declarations, "backend = " + engine, ""])


class ExtendedInventoryTests(unittest.TestCase):
    def test_exact_djinn_builtins_are_recorded_without_ordinary_providers(self):
        for operation in ("either", None):
            for declarations in (list(BUILTIN_ROWS), list(reversed(BUILTIN_ROWS))):
                with self.subTest(operation=operation, declarations=declarations):
                    result = observed_inventory(transcript("djinn", declarations), "djinn", operation)
                    self.assertEqual(result["declarations"], declarations)
                    self.assertEqual(result["builtin_declarations"], declarations)
                    self.assertEqual(result["values"], [])
                    self.assertEqual(result["type_aliases"], [])

    def test_existing_empty_inventory_remains_valid_for_both_engines(self):
        for engine in ("djinn", "exference"):
            for operation in ("either", None):
                with self.subTest(engine=engine, operation=operation):
                    result = observed_inventory(transcript(engine, ["(no declarations)"]), engine, operation)
                    self.assertEqual(result["builtin_declarations"], [])
                    self.assertEqual(result["values"], [])

    def test_unknown_values_and_changed_constructor_families_fail(self):
        declarations = list(BUILTIN_ROWS)
        invalid = [
            declarations + ["extendedWitness_either :: forall a. a -> a"],
            declarations + ["id :: forall a. a -> a"],
            declarations + ["data Extra = Extra"],
            ["data [] v0 = [] | (:) v0 v0", declarations[1]],
            ["data [] v0 = [] | (:) v0 [v0] | Hidden", declarations[1]],
            ["data [] a = [] | (:) a [a]", declarations[1]],
            [declarations[0], "data () = Hidden"],
            [declarations[0], "data () = () | Hidden"],
        ]
        for operation in ("either", None):
            for inventory in invalid:
                with self.subTest(operation=operation, inventory=inventory), self.assertRaises(ValueError):
                    observed_inventory(transcript("djinn", inventory), "djinn", operation)

    def test_incomplete_duplicate_empty_or_wrong_engine_inventory_fails(self):
        declarations = list(BUILTIN_ROWS)
        for inventory in ([], declarations[:1], declarations[1:],
                          declarations + declarations[:1], declarations + ["(no declarations)"]):
            with self.subTest(inventory=inventory), self.assertRaises(ValueError):
                observed_inventory(transcript("djinn", inventory), "djinn", "either")
        for operation, suffix in (("either", []), (None, []), ("length", list(NUMERIC_ROWS))):
            with self.subTest(operation=operation), self.assertRaises(ValueError):
                observed_inventory(transcript("exference", declarations + suffix), "exference", operation)

    def test_length_preserves_exact_numeric_providers_and_existing_exference_behavior(self):
        for engine, prefix in (("djinn", []), ("djinn", list(BUILTIN_ROWS)), ("exference", [])):
            with self.subTest(engine=engine, prefix=prefix):
                declarations = prefix + list(NUMERIC_ROWS)
                result = observed_inventory(transcript(engine, declarations), engine, "length")
                self.assertEqual(result["declarations"], declarations)
                self.assertEqual(result["builtin_declarations"], prefix)
                self.assertEqual(result["values"], {"numericZero": "Int", "numericSuccessor": "Int -> Int"})
                self.assertEqual(result["type_aliases"], [])
                self.assertEqual(result["abstract_types"], list(NUMERIC_ROWS[:1]))

    def test_builtin_allowance_does_not_relax_numeric_provider_isolation(self):
        numeric = list(NUMERIC_ROWS)
        invalid = [
            [], numeric[:-1], numeric + numeric[-1:],
            numeric + ["oracle :: forall a. a -> a"],
            numeric[:-1] + ["numericSuccessor :: Int -> Int -> Int"],
            numeric[:-1] + ["numericSuccessor :: Bool -> Int"],
            numeric[:-2] + ["numericZero :: Bool", numeric[-1]],
            numeric + ["data Extra = Extra"],
            numeric + ["type Int :: Type"],
            ["type Int :: Type -> Type", *numeric[1:]],
            ["data Int", *numeric[1:]],
            ["data Int = FakeInt", *numeric[1:]],
            ["type NumericInt = Int", *numeric],
        ]
        for engine, prefix in (("djinn", list(BUILTIN_ROWS)), ("exference", [])):
            for inventory in invalid:
                with self.subTest(engine=engine, inventory=inventory), self.assertRaises(ValueError):
                    observed_inventory(transcript(engine, prefix + inventory), engine, "length")


class ExtendedNativeIntFixtureTests(unittest.TestCase):
    def setUp(self):
        self.settings = SimpleNamespace(window=65536, budget=500000,
            djinn_strategy="interleave", steps=100000, queue=8192,
            process_timeout=900)

    def test_only_djinn_length_enables_the_exact_value_axioms(self):
        for engine in ("djinn", "exference"):
            for operation in (*spec.OPERATIONS, None):
                with self.subTest(engine=engine, operation=operation):
                    source, row = commands(engine, operation, self.settings, Path("ExtendedNumeric.hs"))
                    expected = "on" if (engine, operation) == ("djinn", "length") else "off"
                    self.assertEqual([line for line in source.splitlines()
                                      if line.startswith(":set djinn-axioms ")],
                                     [":set djinn-axioms " + expected])
                    self.assertEqual(row["djinn_axioms"], expected)
                    self.assertEqual(row["search_type"], row["type"])
                    self.assertEqual(row["provider_inventory"],
                        [] if operation is None else spec.REQUIRED_PROVIDERS[operation]["haskell"])

    def test_settings_require_actual_on_only_in_numeric_djinn_cell(self):
        for engine, operation, expected in (("djinn", "length", "on"),
                                           ("djinn", "foldl", "off"),
                                           ("djinn", None, "off"),
                                           ("exference", "length", "off")):
            fields = {"backend": engine, "ranking": "balanced", "select": "first",
                "render": "definition", "prompt": '\"\"', "allow-unused": "on",
                "djinn-axioms": expected, "djinn-strategy": "interleave",
                "candidate-limit": "65536", "quality-window": "65536",
                "choice-budget": "500000", "max-steps": "100000"}
            snapshot = "Active backend: " + engine + "\n" + "\n".join(
                key + " = " + value for key, value in fields.items()) + "\n"
            with self.subTest(engine=engine, operation=operation):
                self.assertEqual(validate_cell_settings(snapshot, engine, operation, self.settings), fields)
                for altered in (snapshot.replace("djinn-axioms = " + expected,
                        "djinn-axioms = " + ("off" if expected == "on" else "on")),
                        snapshot + "djinn-axioms = " + expected + "\n",
                        snapshot.replace("choice-budget = 500000", "choice-budget = 500001"),
                        snapshot + "[DJEX_REPL_TYPE_SCOPE] unresolved Int\n"):
                    with self.assertRaises(ValueError):
                        validate_cell_settings(altered, engine, operation, self.settings)

    def test_djinn_cannot_accept_values_without_the_registered_int_head(self):
        with self.assertRaises(ValueError):
            observed_inventory(transcript("djinn", [*BUILTIN_ROWS, *NUMERIC_ROWS[1:]]),
                               "djinn", "length")


if __name__ == "__main__":
    unittest.main()
