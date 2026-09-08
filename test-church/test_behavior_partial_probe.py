"""Pure guards for the partial corpus's exact observed search inventory."""
import unittest

from pathlib import Path
from types import SimpleNamespace

from behavior_partial_probe import (EXPECTED_PRIMITIVE_TYPE, NUMERIC_SOURCE,
    commands, controls_for, observed_inventory, replay_sources, validate_cell_settings)
import behavior_partial_spec as spec

# Exact fresh Djinn browse rows captured by partial-behavior-head-djinn-v1.
# Keep this fixture independent of the implementation's allowlist.
BUILTIN_ROWS = (
    "data [] v0 = [] | (:) v0 [v0]",
    "data () = ()",
)


def transcript(engine, declarations):
    header = "-- Djinn" if engine == "djinn" else "-- Current source scope"
    return "\n".join([header, *declarations, "backend = " + engine, ""])


class PartialInventoryTests(unittest.TestCase):
    def test_exact_djinn_builtins_are_recorded_without_ordinary_providers(self):
        for operation in ("head", None):
            for declarations in (list(BUILTIN_ROWS), list(reversed(BUILTIN_ROWS))):
                with self.subTest(operation=operation, declarations=declarations):
                    result = observed_inventory(transcript("djinn", declarations), "djinn", operation)
                    self.assertEqual(result["declarations"], declarations)
                    self.assertEqual(result["builtin_declarations"], declarations)
                    self.assertEqual(result["values"], [])
                    self.assertEqual(result["type_aliases"], [])

    def test_existing_empty_inventory_remains_valid_for_both_engines(self):
        for engine in ("djinn", "exference"):
            for operation in ("head", None):
                with self.subTest(engine=engine, operation=operation):
                    result = observed_inventory(transcript(engine, ["(no declarations)"]), engine, operation)
                    self.assertEqual(result["builtin_declarations"], [])
                    self.assertEqual(result["values"], [])

    def test_unknown_values_and_changed_constructor_families_fail(self):
        declarations = list(BUILTIN_ROWS)
        bad_inventories = [
            declarations + ["oracleWitness_head :: forall a. a -> a"],
            declarations + ["id :: forall a. a -> a"],
            declarations + ["data Extra = Extra"],
            ["data [] v0 = [] | (:) v0 v0", declarations[1]],
            ["data [] v0 = [] | (:) v0 [v0] | Hidden", declarations[1]],
            ["data [] a = [] | (:) a [a]", declarations[1]],
            [declarations[0], "data () = Hidden"],
            [declarations[0], "data () = () | Hidden"],
        ]
        for operation in ("head", None):
            for inventory in bad_inventories:
                with self.subTest(operation=operation, inventory=inventory), self.assertRaises(ValueError):
                    observed_inventory(transcript("djinn", inventory), "djinn", operation)

    def test_incomplete_duplicate_empty_or_wrong_engine_inventory_fails(self):
        declarations = list(BUILTIN_ROWS)
        for inventory in ([], declarations[:1], declarations[1:],
                          declarations + declarations[:1], declarations + ["(no declarations)"]):
            with self.subTest(inventory=inventory), self.assertRaises(ValueError):
                observed_inventory(transcript("djinn", inventory), "djinn", "head")
        with self.assertRaises(ValueError):
            observed_inventory(transcript("exference", declarations), "exference", "head")

    def test_numeric_provider_keeps_its_complete_scheme_and_separate_builtins(self):
        # The native Int row is from extended-djinn-length-native-int-v3;
        # this primitive scheme still awaits the partial-at live query.
        primitive = "PartialNumeric.partialIntCase :: forall r. Int -> r -> r -> (Int -> r) -> r"
        for engine, prefix, abstract in (("djinn", [], ["type Int :: Type"]),
                ("djinn", list(BUILTIN_ROWS), ["type Int :: Type"]), ("exference", [], [])):
            numeric = abstract + [primitive]
            with self.subTest(engine=engine, prefix=prefix):
                result = observed_inventory(transcript(engine, prefix + numeric), engine, "at")
                self.assertEqual(result["builtin_declarations"], prefix)
                self.assertEqual(set(result["values"]), {"partialIntCase"})
                self.assertEqual(result["values"]["partialIntCase"]["normalized_type"], EXPECTED_PRIMITIVE_TYPE)
                self.assertEqual(result["type_aliases"], [])
                self.assertEqual(result["abstract_types"], abstract)

    def test_builtin_allowance_does_not_relax_numeric_provider_isolation(self):
        scheme = "forall r. Int -> r -> r -> (Int -> r) -> r"
        primitive = "partialIntCase :: " + scheme
        numeric = ["type Int :: Type", primitive]
        invalid = [[], numeric[:1], numeric + [primitive],
            numeric + ["oracle :: forall a. a -> a"],
            [numeric[0], primitive.replace("(Int -> r)", "Int -> r")],
            [numeric[0], "partialIntCase :: " + scheme.replace("Int", "Bool")],
            numeric + ["type Int :: Type"],
            ["type Int :: Type -> Type", primitive],
            ["data Int", primitive], ["data Int = FakeInt", primitive],
            ["type Prelude.Int :: Type", primitive],
            numeric + ["type PartialNumeric.IndexInt = Prelude.Int"],
            [numeric[0], "partialIntCase :: " + scheme.replace("Int", "IndexInt")],
            [numeric[0], "partialIntCase :: " + scheme.replace("Int", "PartialNumeric.IndexInt")],
        ]
        for engine, prefix in (("djinn", list(BUILTIN_ROWS)), ("exference", [])):
            for inventory in invalid:
                with self.subTest(engine=engine, inventory=inventory), self.assertRaises(ValueError):
                    observed_inventory(transcript(engine, prefix + inventory), engine, "at")
        with self.assertRaises(ValueError):
            observed_inventory(transcript("djinn", [BUILTIN_ROWS[0], "data () = Hidden", *numeric]), "djinn", "at")

    def test_djinn_requires_the_registered_native_int_head(self):
        values = ["partialIntCase :: forall r. Int -> r -> r -> (Int -> r) -> r"]
        with self.assertRaises(ValueError):
            observed_inventory(transcript("djinn", [*BUILTIN_ROWS, *values]), "djinn", "at")

    def test_scheme_normalization_preserves_polymorphism_and_callback_grouping(self):
        good = ["forall result. Prelude.Int -> result -> result -> (Prelude.Int -> result) -> result",
                "Int -> a -> a -> (Int -> a) -> a"]
        bad = ["Int -> Int -> Int -> (Int -> Int) -> Int",
               "forall r extra. Int -> r -> r -> (Int -> r) -> r",
               "forall r. Int -> r -> r -> Int -> r -> r",
               "forall r s. Int -> r -> r -> (Int -> s) -> r"]
        for scheme in good + bad:
            output = transcript("djinn", [*BUILTIN_ROWS, "type Int :: Type", "partialIntCase :: " + scheme])
            with self.subTest(scheme=scheme):
                if scheme in good:
                    result = observed_inventory(output, "djinn", "at")
                    self.assertEqual(result["values"]["partialIntCase"]["normalized_type"], EXPECTED_PRIMITIVE_TYPE)
                else:
                    with self.assertRaises(ValueError):
                        observed_inventory(output, "djinn", "at")


class PartialNativeIntFixtureTests(unittest.TestCase):
    def setUp(self):
        self.settings = SimpleNamespace(window=65536, budget=500000,
            djinn_strategy="interleave", steps=100000, queue=8192, process_timeout=300)

    def test_only_djinn_at_enables_the_exact_value_axiom(self):
        for engine in ("djinn", "exference"):
            for operation in (*spec.OPERATIONS, None):
                with self.subTest(engine=engine, operation=operation):
                    program, row = commands(engine, operation, self.settings, Path("PartialNumeric.hs"))
                    expected = "on" if (engine, operation) == ("djinn", "at") else "off"
                    self.assertEqual([line for line in program.splitlines() if line.startswith(":set djinn-axioms ")],
                                     [":set djinn-axioms " + expected])
                    self.assertEqual(row["djinn_axioms"], expected)
                    self.assertEqual(row["search_type"], row["type"])
                    self.assertEqual(row["provider_inventory"], [] if operation is None else spec.REQUIRED_PROVIDERS[operation]["haskell"])
                    self.assertEqual(":load " in program, operation == "at")
                    self.assertEqual(row["predicate"], "Prelude.False" if operation is None else spec.haskell_predicate(operation, row["name"]))
                    if operation is not None:
                        self.assertEqual(row["type"], spec.HASKELL_TYPES[operation])
                        self.assertEqual(row["fixture_ids"], [fixture["id"] for fixture in spec.FIXTURES[operation]])

    def test_settings_require_the_actual_cell_policy_and_unchanged_limits(self):
        for engine in ("djinn", "exference"):
            for operation in (*spec.OPERATIONS, None):
                expected = "on" if (engine, operation) == ("djinn", "at") else "off"
                fields = {"backend": engine, "ranking": "balanced", "select": "first",
                    "render": "definition", "prompt": '\"\"', "allow-unused": "on",
                    "djinn-axioms": expected, "djinn-strategy": "interleave",
                    "candidate-limit": "65536", "quality-window": "65536",
                    "choice-budget": "500000", "max-steps": "100000"}
                snapshot = "Active backend: " + engine + "\n" + "\n".join(
                    key + " = " + value for key, value in fields.items()) + "\n"
                with self.subTest(engine=engine, operation=operation):
                    self.assertEqual(validate_cell_settings(snapshot, engine, operation, self.settings), fields)
                    invalid = [snapshot.replace("djinn-axioms = " + expected,
                               "djinn-axioms = " + ("off" if expected == "on" else "on")),
                               snapshot + "djinn-axioms = " + expected + "\n",
                               snapshot + "[DJEX_REPL_TYPE_SCOPE] unresolved Int\n"]
                    for key in ("candidate-limit", "quality-window", "choice-budget", "max-steps"):
                        invalid.append(snapshot.replace(key + " = " + fields[key], key + " = " + str(int(fields[key]) + 1)))
                    for altered in invalid:
                        with self.assertRaises(ValueError):
                            validate_cell_settings(altered, engine, operation, self.settings)

    def test_displayed_candidate_replay_keeps_full_type_and_only_the_primitive_import(self):
        _, row = commands("djinn", "at", self.settings, Path("PartialNumeric.hs"))
        row["definition"] = row["name"] + " = " + spec.HASKELL_PROVIDER_WITNESSES["at"]
        sources, typed_file, _ = replay_sources(row, 2, candidate=True)
        candidate = sources[typed_file]
        self.assertIn(row["name"] + " :: " + spec.HASKELL_TYPES["at"] + "\n" + row["definition"] + "\n", candidate)
        self.assertEqual([line for line in candidate.splitlines() if line.startswith("import ")],
            ["import Prelude (Int)", "import qualified Prelude (Int)", "import PartialNumeric (partialIntCase)"])
        self.assertEqual(sources["PartialNumeric.hs"], NUMERIC_SOURCE)
        self.assertNotIn("IndexInt", candidate)

    def test_both_existing_primitive_controls_replay_the_exact_shared_source(self):
        controls = [row for row in controls_for(["at"]) if row["kind"] == "generic_primitive_control"]
        self.assertEqual([row["id"] for row in controls],
                         ["primitive-int_negative_zero_positive", "primitive-int_polymorphic_boolean"])
        self.assertEqual([row["assertion"] for row in controls],
                         [assertion for _, assertion in spec.provider_control_source("haskell")[1]])
        for row in controls:
            sources, typed_file, _ = replay_sources(row, 2)
            self.assertEqual(typed_file, "PartialNumeric.hs")
            self.assertEqual(sources[typed_file], NUMERIC_SOURCE)
            self.assertIn(row["assertion"], sources["Main.hs"])


if __name__ == "__main__":
    unittest.main()
