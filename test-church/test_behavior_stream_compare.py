"""Pure paired-receipt gates; no compiler, synthesis, or replay is executed."""
import copy
import hashlib
from pathlib import Path
import tempfile
import unittest
from unittest import mock

from behavior_stream_compare import cell_latency, compare_matrices, load_matrix
from behavior_spec import OPERATIONS


def cell(seconds=2.0):
    return {"profile": {"settings": {"window": 256}, "spec_sha256": "oracle"},
            "type": "T", "finite_observation_count": 2, "candidate": "candidate",
            "timing": {"basis": "process-run entry to accepted output line",
                       "poll_interval_seconds": 0.02, "first_success_seconds": seconds}}


def full_matrix(seconds=2.0):
    return {(host, engine, operation): cell(seconds)
            for host, engines in [("haskell", ("djinn", "exference")),
                                  ("lean", ("djinn", "exference", "both"))]
            for engine in engines for operation in OPERATIONS}


class ComparisonTests(unittest.TestCase):
    def test_full_thirty_cells_compare_without_requiring_same_candidate_spelling(self):
        before, after = full_matrix(), full_matrix(1.0)
        after[("lean", "djinn", "not")]["candidate"] = "different checked candidate"
        result = compare_matrices(before, after)
        self.assertEqual(result["cell_count"], 30)
        self.assertEqual(len(result["summaries"]), 5)
        self.assertEqual(sum(row["candidate_spelling_changed"] for row in result["cells"]), 1)
        self.assertTrue(all(row["first_success_ratio_before_over_after"] == 2.0 for row in result["cells"]))

    def test_missing_cells_need_explicit_subset_and_matching_inventory(self):
        single = {("haskell", "djinn", "not"): cell()}
        with self.assertRaisesRegex(ValueError, "30 cells"):
            compare_matrices(single, single)
        self.assertEqual(compare_matrices(single, single, allow_subset=True)["coverage"], "explicit subset")
        with self.assertRaisesRegex(ValueError, "same nonempty"):
            compare_matrices(single, {}, allow_subset=True)

    def test_changed_scope_budget_or_timing_basis_is_not_a_speed_comparison(self):
        before = {("haskell", "djinn", "not"): cell()}
        for mutation in (
            lambda value: value["profile"]["settings"].update(window=512),
            lambda value: value["profile"].update(spec_sha256="different oracle"),
            lambda value: value.update(type="different goal"),
            lambda value: value["timing"].update(basis="query echo"),
            lambda value: value["timing"].update(poll_interval_seconds=0.1),
        ):
            after = copy.deepcopy(before)
            mutation(next(iter(after.values())))
            with self.assertRaises(ValueError):
                compare_matrices(before, after, allow_subset=True)

    def test_duplicate_receipt_cannot_inflate_coverage(self):
        single = {("haskell", "djinn", "not"): cell()}
        with mock.patch("behavior_stream_compare.load_receipt", return_value=single):
            with self.assertRaisesRegex(ValueError, "more than once"):
                load_matrix(["one", "one"])

    def test_visibility_is_bound_to_exact_line_offsets_and_candidate(self):
        with tempfile.TemporaryDirectory(prefix="behavior-latency-compare-") as directory:
            path = Path(directory) / "stdout.txt"
            query = {"name": "answer", "command": ":synth answer : Nat where True",
                     "candidate": "0"}
            first = ("λ> " + query["command"] + "\r\n").encode()
            second = b"  it1  0\r\n"
            path.write_bytes(first + second)
            events = [{"kind": kind, "query_id": "answer", "observed_seconds": seconds,
                       "stdout_byte_offset": offset,
                       "line_sha256": hashlib.sha256(line.rstrip(b"\r\n")).hexdigest()}
                      for kind, seconds, offset, line in
                      [("query_echo", 5.0, 0, first), ("accepted_output_line", 7.0, len(first), second)]]
            observation = {"clock": "time.monotonic", "poll_interval_seconds": 0.02,
                           "queries": [{"id": "answer"}], "events": events,
                           "observer_poll_seconds": 0.001, "poll_count": 5, "resolution_note": "observed"}
            process = {"label": "live", "wall_seconds": 9.0, "stdout_path": str(path),
                       "output_observations": observation}
            timing = cell_latency([process], query, "lean")
            self.assertEqual(timing["first_success_seconds"], 2.0)
            self.assertEqual(timing["process_start_to_first_success_seconds"], 7.0)
            self.assertEqual(timing["live_process_seconds"], 9.0)
            query["candidate"] = "1"
            with self.assertRaisesRegex(ValueError, "differs from checked candidate"):
                cell_latency([process], query, "lean")


if __name__ == "__main__":
    unittest.main()
