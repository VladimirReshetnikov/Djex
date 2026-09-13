"""Evidence-index regressions: stale, misattributed and missing evidence."""
import copy
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
import zipfile

import behavior_ledger as ledger


class LedgerTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.specs = {
            "extended": SimpleNamespace(OPERATIONS=["foldr"] + [f"e{i}" for i in range(12)], SOURCE_CASE_IDS={}),
            "supplied_default": SimpleNamespace(OPERATIONS=[f"p{i}" for i in range(19)], SOURCE_CASE_IDS={}),
        }
        self.row = {"id": "djinn-foldr", "operation": "foldr", "status": "passed",
                    "accepted": True, "replay": {"status": "passed"}}

    def catalog(self, rows=None):
        raw = json.dumps({"cells": rows if rows is not None else [self.row],
                          "settings": {"window": 32}}).encode()
        (self.root / "receipt.json").write_bytes(raw)
        return {"scope": "test receipts", "collections": [{
            "id": "fixture", "source": {"repo": "Djex", "path": "receipt.json", "sha256": ledger.sha256(raw)},
            "rows": "/cells", "metadata": {"settings": "/settings"},
            "group": "extended", "language": "haskell", "engine": "djinn"}]}

    def build(self, catalog):
        return ledger.build_ledger(self.specs, catalog, {"Djex": self.root})

    def test_complete_cross_product_without_current_acceptance(self):
        result = self.build(self.catalog())
        self.assertEqual(160, len({r["id"] for r in result["cells"]}))
        self.assertFalse(result["current_complete"])
        self.assertEqual(1, result["historical_counts"]["acceptance_recorded"])
        self.assertEqual(159, result["historical_counts"]["no_recorded_evidence"])
        self.assertTrue(all(r["current_validation"] == "not_established_by_historical_index"
                            for r in result["cells"]))

    def test_false_and_reference_controls_are_not_operation_cells(self):
        controls = [{"operation": "reject_all", "expected": "no_candidate", "status": "passed"},
                    {"operation": "foldr", "kind": "positive_oracle_witness", "status": "passed"},
                    {"operation": None, "kind": "false_oracle_query", "status": "passed"}]
        result = self.build(self.catalog([self.row] + controls))
        self.assertEqual(1, sum(len(r["history"]) for r in result["cells"]))

    def test_failed_receipt_does_not_erase_earlier_acceptance(self):
        failure = {"id": "djinn-foldr", "operation": "foldr", "status": "failed", "accepted": False}
        result = self.build(self.catalog([self.row, failure]))
        cell = result["cells"][0]
        self.assertEqual("acceptance_recorded", cell["historical_status"])
        self.assertEqual(["historical_accepted", "failed_unclassified"],
                         [r["recorded_outcome"] for r in cell["history"]])

    def test_acceptance_requires_replay(self):
        self.row.pop("replay")
        with self.assertRaisesRegex(ValueError, "lacks successful candidate replay"):
            self.build(self.catalog())

    def test_tampered_receipt_is_refused(self):
        catalog = self.catalog()
        (self.root / "receipt.json").write_text("{}")
        with self.assertRaisesRegex(ValueError, "hash mismatch"):
            self.build(catalog)

    def test_archive_member_has_its_own_pin(self):
        catalog = self.catalog()
        source = catalog["collections"][0]["source"]
        with zipfile.ZipFile(self.root / "receipt.zip", "w") as archive:
            archive.writestr("results.json", (self.root / "receipt.json").read_bytes())
        source.update(path="receipt.zip", member="results.json", member_sha256="0" * 64,
                      sha256=ledger.sha256((self.root / "receipt.zip").read_bytes()))
        with self.assertRaisesRegex(ValueError, "member hash mismatch"):
            self.build(catalog)

    def test_duplicate_receipt_observation_is_refused(self):
        catalog = self.catalog()
        other = copy.deepcopy(catalog["collections"][0])
        other["id"] = "same-observation"
        catalog["collections"].append(other)
        with self.assertRaisesRegex(ValueError, "duplicate receipt observation"):
            self.build(catalog)

    def test_wrong_engine_is_refused(self):
        catalog = self.catalog()
        catalog["collections"][0]["engine"] = "exference"
        with self.assertRaisesRegex(ValueError, "contradicts receipt engine"):
            self.build(catalog)

    def test_unknown_operation_is_refused(self):
        self.row["operation"] = "unreviewed-operation"
        with self.assertRaisesRegex(ValueError, "unknown operation/mode"):
            self.build(self.catalog())

    def test_missing_metadata_pointer_is_refused(self):
        catalog = self.catalog()
        catalog["collections"][0]["metadata"]["sources"] = "/absent"
        with self.assertRaises(KeyError):
            self.build(catalog)

    def test_lean_oracle_exception_cannot_authorize_candidate_axioms(self):
        candidate = "BehaviorExtendedReplay.candidate"
        oracle = "BehaviorExtendedReplay.candidate_passes_original_oracle"
        inventories = {candidate: [], oracle: ["propext"]}
        row = {"operation": "maybeEither", "expected": "candidate", "status": "passed",
               "replay": {"status": "passed", "expected_axiom_inventories": inventories,
                          "actual_axiom_inventories": copy.deepcopy(inventories)}}
        self.assertEqual("historical_accepted", ledger.recorded_outcome(row, "lean"))
        row["replay"]["actual_axiom_inventories"][candidate] = ["propext"]
        with self.assertRaisesRegex(ValueError, "axiom policy"):
            ledger.recorded_outcome(row, "lean")

    def test_receipt_cannot_escape_repository(self):
        with self.assertRaisesRegex(ValueError, "escapes repository"):
            ledger.inside(self.root, "../outside.json")

    def test_lean_explicit_rejection_survives_passing_individual_replay(self):
        inventories = {"BehaviorPartialReplay.candidate": [],
                       "BehaviorPartialReplay.candidate_passes_original_oracle": []}
        row = {"operation": "head", "expected": "candidate", "status": "passed", "accepted": False,
               "replay": {"status": "passed", "expected_axiom_inventories": inventories,
                          "actual_axiom_inventories": inventories}}
        self.assertEqual("unaccepted_record", ledger.recorded_outcome(row, "lean", "supplied_default"))
        row["accepted"] = True
        self.assertEqual("historical_accepted", ledger.recorded_outcome(row, "lean", "supplied_default"))

    def test_partial_replay_cannot_use_extended_oracle_exception(self):
        inventories = {"BehaviorPartialReplay.candidate": [],
                       "BehaviorPartialReplay.candidate_passes_original_oracle": ["propext"]}
        row = {"operation": "head", "expected": "candidate", "status": "passed", "accepted": True,
               "replay": {"status": "passed", "expected_axiom_inventories": inventories,
                          "actual_axiom_inventories": inventories}}
        with self.assertRaisesRegex(ValueError, "axiom policy"):
            ledger.recorded_outcome(row, "lean", "supplied_default")


if __name__ == "__main__":
    unittest.main()
