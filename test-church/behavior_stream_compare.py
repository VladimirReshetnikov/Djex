#!/usr/bin/env python3
"""Compare completed behavioral receipts without running synthesis or replay.

The default comparison requires all 30 host/engine/operation cells. Visibility
times are single-run observations, not CPU time or statistical guarantees.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import statistics

from behavior_runtime import OPERATIONS, prepare_output_directory, sha256, write_json


def require(condition, message):
    if not condition:
        raise ValueError(message)


def checked_hash(path, expected):
    require(sha256(path) == expected, "saved artifact hash changed: " + str(path))


def checked_process(row):
    require(row["status"] == "completed" and row["exit_code"] == 0
            and row["timed_out"] is False, "process did not complete successfully: " + row["label"])
    for field in ("input", "stdout", "stderr"):
        if field + "_path" in row:
            checked_hash(row[field + "_path"], row[field + "_sha256"])


def cell_latency(processes, case, host):
    matches = [(process, process["output_observations"])
               for process in processes if "output_observations" in process
               and any(query["id"] == case["name"]
                       for query in process["output_observations"]["queries"])]
    require(len(matches) == 1, "missing or ambiguous timing for " + case["name"])
    process, observation = matches[0]
    require(observation["clock"] == "time.monotonic", "unsupported observation clock")
    require(0 < observation["poll_interval_seconds"] <= 1, "invalid poll interval")
    events = [event for event in observation["events"] if event["query_id"] == case["name"]]
    stdout = Path(process["stdout_path"]).read_bytes()
    previous = -1.0
    for event in events:
        elapsed = event["observed_seconds"]
        require(math.isfinite(elapsed) and previous <= elapsed <= process["wall_seconds"],
                "invalid or nonmonotonic visibility timestamp")
        previous = elapsed
        offset = event["stdout_byte_offset"]
        require(isinstance(offset, int) and 0 <= offset < len(stdout)
                and (offset == 0 or stdout[offset - 1] == 10), "invalid captured line offset")
        line = stdout[offset:].split(b"\n", 1)[0].rstrip(b"\r")
        require(hashlib.sha256(line).hexdigest() == event["line_sha256"], "observed line hash changed")
        text = line.decode("utf-8")
        if event["kind"] == "query_echo":
            require(text == "λ> " + case["command"], "observed query echo differs from checked query")
        elif event["kind"] == "accepted_output_line":
            if host == "haskell":
                text = re.sub(r"^djex\[(?:djinn|exference|both)\]> ?", "", text)
                require(text == case["definition"].splitlines()[0], "observed definition differs from checked candidate")
            else:
                match = re.fullmatch(r"[ \t]+it1[ \t]+(.+)", text)
                require(match is not None and match[1] == case["candidate"].splitlines()[0],
                        "observed it1 differs from checked candidate")
        else:
            raise ValueError("unknown visibility event")
    starts = [event for event in events if event["kind"] == "query_echo"]
    accepted = [event for event in events if event["kind"] == "accepted_output_line"]
    require(len(accepted) == 1 and len(starts) == (host == "lean"), "incorrect query/success visibility inventory")
    first = accepted[0]["observed_seconds"]
    origin = starts[0]["observed_seconds"] if starts else 0.0
    require(first >= origin, "success precedes its query echo")
    return {"first_success_seconds": first - origin,
            "basis": "query echo to accepted output line" if starts else "process-run entry to accepted output line",
            "process_start_to_first_success_seconds": first,
            "live_process_seconds": process["wall_seconds"],
            "live_process_label": process["label"],
            "poll_interval_seconds": observation["poll_interval_seconds"],
            "observer_poll_seconds": observation["observer_poll_seconds"],
            "observer_poll_count": observation["poll_count"],
            "resolution_note": observation["resolution_note"]}


def load_receipt(path):
    path = Path(path).resolve()
    report = json.loads(path.read_text(encoding="utf-8"))
    require(report["status"] == "passed" and report["executable_unchanged"] is True,
            "incomplete or unstable behavioral receipt: " + str(path))
    require(report.get("latency_observation_enabled") is True,
            "receipt has no live visibility timestamps; old completed captures cannot supply them")
    host = "haskell" if "compiler_exit_code" in report else "lean"
    require(host != "haskell" or (report["compiler_exit_code"] == report["execution_exit_code"] == 0),
            "Haskell replay failed")
    require(host != "lean" or (report["kernel_exit_code"] == 0
            and report["kernel_inventory_count"] == report["empty_axiom_inventory_count"]), "Lean replay failed")
    require(report["named_synthesis_providers"] == [], "oracle isolation inventory changed")
    checked_hash(report["executable_path"], report["executable_sha256"])
    provenance = report["provenance"]
    checked_hash(provenance["manifest_path"], provenance["manifest_sha256"])
    source = Path(provenance["church_source_path"]).read_text(encoding="utf-8-sig")
    require(hashlib.sha256(source.encode()).hexdigest() == provenance["church_source_canonical_lf_sha256"],
            "Church source changed")
    checked_hash(report["oracle_control_source"], report["oracle_control_sha256"])
    processes = report["processes"]
    for process in processes:
        checked_process(process)
    if host == "haskell":
        for module in report["replay_modules"]:
            checked_hash(module["path"], module["sha256"])
        cases = report["cases"]
        negatives = report["false_oracle_results"]
        require([row["engine"] for row in negatives] == report["engines"], "missing Haskell False controls")
        for negative in negatives:
            counts = negative["observations"]
            require(counts["true"] == counts["error"] == counts["timeout"] == 0
                    and counts["false"] == counts["checked"] > 0, "False was not actually rejected")
    else:
        for module in report["kernel_replays"]:
            checked_hash(module["path"], module["source_sha256"])
            require(module["status"] == "passed" and module["exit_code"] == 0
                    and module["empty_axiom_inventory_count"] == len(module["declarations"]),
                    "isolated Lean inventory failed")
        cases = [row for row in report["results"] if row["status"] == "candidate"]
        negatives = [row for row in report["results"] if row["status"] == "no_candidate"]
        require([row["engine"] for row in negatives] == report["engines"], "missing Lean False controls")
        for negative in negatives:
            counts = negative["observations"]
            require(counts["passed"] == counts["inconclusive"] == 0 and counts["falsified"] > 0,
                    "False was not actually falsified")
    expected = {(engine, operation) for engine in report["engines"] for operation in report["operations"]}
    actual = [(row["engine"], row["operation"]) for row in cases]
    require(len(actual) == len(expected) == report["candidate_count"] and set(actual) == expected,
            "missing or duplicated selected matrix cell")
    module_rows = report["replay_modules"] if host == "haskell" else report["kernel_replays"]
    for index, case in enumerate(cases):
        filename = f"BehaviorCandidate{index}." + ("hs" if host == "haskell" else "lean")
        modules = [Path(row["path"]) for row in module_rows if Path(row["path"]).name == filename]
        require(len(modules) == 1, "missing isolated candidate replay module: " + filename)
        if host == "haskell":
            fragment = f"{case['name']} :: {case['type']}\n{case['definition']}\n"
        else:
            fragment = (f"def BehaviorCandidates.{case['name']} : {case['type']} :=\n"
                        + "\n".join("  " + line for line in case["candidate"].splitlines()) + "\n")
        require(fragment in modules[0].read_text(encoding="utf-8"),
                "checked candidate/type differs from its isolated replay module")
    cells = {}
    for case in cases:
        key = (host, case["engine"], case["operation"])
        observations = case["behavioral_observations"] if host == "haskell" else case["observations"]
        require(observations["true" if host == "haskell" else "passed"] >= 1, "candidate lacks a passed predicate")
        cells[key] = {"type": case["type"], "candidate": case["definition" if host == "haskell" else "candidate"],
                      "finite_observation_count": case["finite_observation_count"],
                      "predicate_observations": observations,
                      "timing": cell_latency(processes, case, host),
                      "profile": {"settings": report["settings"], "runner_sha256": report["runner_sha256"],
                                  "spec_sha256": report["spec_sha256"],
                                  "oracle_control_sha256": report["oracle_control_sha256"],
                                  "manifest_sha256": provenance["manifest_sha256"],
                                  "source_sha256": provenance["church_source_canonical_lf_sha256"]},
                      "receipt_path": str(path), "receipt_sha256": sha256(path),
                      "executable_path": report["executable_path"], "executable_sha256": report["executable_sha256"]}
    return cells


def load_matrix(paths):
    matrix = {}
    for path in paths:
        incoming = load_receipt(path)
        require(not matrix.keys() & incoming.keys(), "a matrix cell was supplied more than once")
        matrix.update(incoming)
    return matrix


def compare_matrices(before, after, *, allow_subset=False):
    full = {(host, engine, operation) for host, engines in
            [("haskell", ["djinn", "exference"]), ("lean", ["djinn", "exference", "both"])]
            for engine in engines for operation in OPERATIONS}
    require(before and before.keys() == after.keys(), "baseline and after must have the same nonempty cells")
    require(set(before) <= full and (allow_subset or set(before) == full), "comparison requires all 30 cells")
    rows = []
    for key in sorted(before):
        old, new = before[key], after[key]
        require(old["profile"] == new["profile"], "source, oracle, harness or settings changed: " + repr(key))
        require(old["type"] == new["type"] and old["finite_observation_count"] == new["finite_observation_count"],
                "source target or finite oracle coverage changed")
        a, b = old["timing"], new["timing"]
        require(a["basis"] == b["basis"] and a["poll_interval_seconds"] == b["poll_interval_seconds"],
                "incomparable timing methods")
        rows.append({"host": key[0], "engine": key[1], "operation": key[2],
                     "baseline": old, "after": new, "candidate_spelling_changed": old["candidate"] != new["candidate"],
                     "first_success_seconds_saved": a["first_success_seconds"] - b["first_success_seconds"],
                     "first_success_ratio_before_over_after": a["first_success_seconds"] / b["first_success_seconds"]
                     if b["first_success_seconds"] > 0 else None})
    summaries = []
    for host, engine in sorted({key[:2] for key in before}):
        group = [row for row in rows if (row["host"], row["engine"]) == (host, engine)]
        summaries.append({"host": host, "engine": engine, "cell_count": len(group),
                          "median_first_success_seconds_before": statistics.median(row["baseline"]["timing"]["first_success_seconds"] for row in group),
                          "median_first_success_seconds_after": statistics.median(row["after"]["timing"]["first_success_seconds"] for row in group)})
    return {"status": "passed", "validation_mode": "offline_paired_behavioral_visibility_comparison",
            "coverage": "explicit subset" if allow_subset else "complete 30-cell matrix",
            "cell_count": len(rows), "summaries": summaries, "cells": rows,
            "measurement_limits": ["One observed run per cell; no statistical latency guarantee.",
                                   "Visibility includes child buffering and 20 ms polling plus OS scheduling.",
                                   "Haskell includes process setup/startup; Lean uses its actual query echo.",
                                   "Live process time excludes independent replay and repeats across Lean cells sharing a process.",
                                   "Predicate observations are not raw proof counts or choice-point consumption.",
                                   "Different executable identities are retained separately from equal source/oracle/limit requirements."]}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", action="append", type=Path, required=True, help="completed baseline results.json; repeat for each host/engine profile")
    parser.add_argument("--after", action="append", type=Path, required=True, help="completed after results.json; repeat for each host/engine profile")
    parser.add_argument("--allow-subset", action="store_true", help="explicitly label a diagnostic subset; default requires all 30 cells")
    parser.add_argument("--output", type=Path, required=True, help="fresh empty comparison directory")
    args = parser.parse_args()
    output = prepare_output_directory(args.output)
    try:
        report = compare_matrices(load_matrix(args.baseline), load_matrix(args.after), allow_subset=args.allow_subset)
    except (KeyError, OSError, TypeError, ValueError) as error:
        report = {"status": "failed", "failure": str(error)}
    write_json(output / "results.json", report)
    print(json.dumps({"status": report["status"], "cell_count": report.get("cell_count"), "receipt": str(output / "results.json")}))
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    raise SystemExit(main())
