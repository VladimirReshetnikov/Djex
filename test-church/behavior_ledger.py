"""Index pinned historical behavior receipts; never run or infer synthesis.

The catalog explicitly selects receipt collections. Unknown cells mean no
indexed evidence, not never attempted. Historical passes never validate HEAD.
"""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import zipfile


MODES = (("haskell", "djinn"), ("haskell", "exference"),
         ("lean", "djinn"), ("lean", "exference"), ("lean", "both"))


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def pointer(document, path):
    """Resolve an explicit RFC 6901 JSON pointer, refusing missing evidence."""
    if path == "":
        return document
    if not path.startswith("/"):
        raise ValueError(f"invalid JSON pointer: {path}")
    for part in path[1:].split("/"):
        key = part.replace("~1", "/").replace("~0", "~")
        document = document[int(key)] if isinstance(document, list) else document[key]
    return document


def inside(root, relative):
    path = (root / relative).resolve()
    if not path.is_relative_to(root.resolve()):
        raise ValueError(f"receipt escapes repository: {relative}")
    return path


def read_pinned(roots, source):
    raw = inside(roots[source["repo"]], source["path"]).read_bytes()
    if sha256(raw) != source["sha256"]:
        raise ValueError(f"receipt hash mismatch: {source['path']}")
    if "member" in source:
        with zipfile.ZipFile(inside(roots[source["repo"]], source["path"])) as archive:
            raw = archive.read(source["member"])
        if sha256(raw) != source["member_sha256"]:
            raise ValueError(f"archive member hash mismatch: {source['member']}")
    return json.loads(raw)


def load_specifications(root):
    directory = root / "test-church"
    sys.path.insert(0, str(directory))
    modules = []
    for name in ("behavior_extended_spec", "behavior_partial_spec"):
        spec = importlib.util.spec_from_file_location(name, directory / f"{name}.py")
        module = importlib.util.module_from_spec(spec)
        sys.modules[name] = module
        spec.loader.exec_module(module)
        modules.append(module)
    extended, partial = modules
    if len(extended.OPERATIONS) != 13 or len(partial.OPERATIONS) != 19:
        raise ValueError("review changed Church operation inventory")
    if set(extended.OPERATIONS) & set(partial.OPERATIONS):
        raise ValueError("extended and supplied-default operation overlap")
    return {"extended": extended, "supplied_default": partial}


def recorded_outcome(row, language, group="extended"):
    """Classify the receipt's claim, not the present implementation."""
    replay = row.get("replay") or {}
    accepted = row.get("accepted") is True
    if language == "lean":
        accepted = (row.get("expected") == "candidate" and row.get("status") == "passed"
                    and row.get("accepted") is not False)
    if accepted:
        replay_passed = (replay.get("status") == "passed" or
                         replay.get("exact_full_type_definition_and_predicate_checked") is True)
        if row.get("status") != "passed" or not replay_passed:
            raise ValueError("acceptance claim lacks successful candidate replay")
        if language == "lean":
            expected = replay.get("expected_axiom_inventories")
            actual = replay.get("actual_axiom_inventories")
            namespace = "BehaviorPartialReplay" if group == "supplied_default" else "BehaviorExtendedReplay"
            candidate = namespace + ".candidate"
            oracle = namespace + ".candidate_passes_original_oracle"
            def permitted(name, axioms):
                if not axioms:
                    return True
                if group == "extended" and row.get("operation") == "maybeEither":
                    return name == oracle and axioms == ["propext"]
                if group == "supplied_default" and row.get("operation") == "atKey":
                    return (name in {"BehaviorPartial.check_atKey", oracle}
                            and axioms == ["Classical.choice", "Quot.sound", "propext"])
                return False
            if (not expected or expected != actual or actual.get(candidate) != []
                    or oracle not in actual
                    or any(not permitted(name, value) for name, value in actual.items())):
                raise ValueError("Lean acceptance violates recorded candidate/oracle axiom policy")
        return "historical_accepted"
    # These classifications are explicit in the indexed audited receipts.
    classification = row.get("classification", "")
    return {
        "numeric_provider_source_projection_rejected_before_behavior": "source_refused",
        "outer_owned_process_timeout_without_completed_behavioral_summary": "outer_timeout",
        "completed_finite_observation_window_no_survivor": "bounded_miss",
    }.get(classification, "failed_unclassified" if row.get("status") == "failed"
          else "unaccepted_record")


def build_ledger(specifications, catalog, roots):
    cells = {}
    for group, spec in specifications.items():
        for operation in spec.OPERATIONS:
            for language, engine in MODES:
                key = f"{group}/{operation}/{language}/{engine}"
                cells[key] = {
                    "id": key, "group": group, "operation": operation,
                    "language": language, "engine": engine,
                    "source_case_id": spec.SOURCE_CASE_IDS.get(operation),
                    "partiality": "supplied_default" if group == "supplied_default" else "total",
                    "current_validation": "not_established_by_historical_index",
                    "history": [],
                }
    if len(cells) != 160:
        raise ValueError("expected exactly 160 operation/mode cells")
    sources = {}
    used = set()
    for collection in catalog["collections"]:
        source = collection["source"]
        source_id = collection["id"]
        if source_id in sources:
            raise ValueError(f"duplicate evidence collection: {source_id}")
        document = read_pinned(roots, source)
        # Validate all declared links before recording any acceptance.
        metadata = {}
        for name, path in collection["metadata"].items():
            value = pointer(document, path)
            encoded = json.dumps(value, sort_keys=True, ensure_ascii=False).encode("utf-8")
            metadata[name] = {"value_sha256": sha256(encoded)}
            if name in ("settings", "runtime", "source_revision"):
                metadata[name]["recorded_value"] = value
        rows = pointer(document, collection["rows"])
        if not isinstance(rows, list):
            raise ValueError("evidence collection is not a list")
        sources[source_id] = {"source": source, "rows": collection["rows"],
                              "metadata_pointers": collection["metadata"],
                              "recorded_metadata": metadata, "control_rows": []}
        for index, row in enumerate(rows):
            operation = row.get("operation")
            if operation is None or row.get("expected") in ("reject", "no_candidate"):
                sources[source_id]["control_rows"].append({
                    "pointer": f"{collection['rows']}/{index}", "recorded_status": row.get("status")})
                continue
            if row.get("kind") in ("false_oracle_query", "positive_oracle_witness", "wrong_control"):
                continue
            engine = collection.get("engine") or row.get("engine")
            if engine is None:
                engine = row["id"].split("-", 1)[0]
            observed_engine = row.get("engine") or row.get("id", "").split("-", 1)[0]
            if observed_engine in {"djinn", "exference", "both"} and observed_engine != engine:
                raise ValueError("catalog engine contradicts receipt engine")
            group, language = collection["group"], collection["language"]
            key = f"{group}/{operation}/{language}/{engine}"
            if key not in cells:
                raise ValueError(f"unknown operation/mode in receipt: {key}")
            path = f"{collection['rows']}/{index}"
            identity = (source["repo"], source["path"], source.get("member"), path)
            if identity in used:
                raise ValueError(f"duplicate receipt observation: {identity}")
            used.add(identity)
            cells[key]["history"].append({
                "collection": source_id, "pointer": path,
                "recorded_outcome": recorded_outcome(row, language, group),
                "recorded_status": row.get("status"),
                "recorded_classification": row.get("classification"),
                "replay_pointer": path + "/replay" if row.get("replay") else None,
            })
    for cell in cells.values():
        cell["historical_status"] = (
            "acceptance_recorded" if any(x["recorded_outcome"] == "historical_accepted"
                                         for x in cell["history"])
            else "attempts_without_recorded_acceptance" if cell["history"]
            else "no_recorded_evidence")
    return {
        "schema": "church-behavior-ledger-v1",
        "scope": "13 extended and 19 supplied-default operations across five language/engine modes",
        "validation": "offline pinned-receipt indexing; no synthesis or compiler replay performed",
        "catalog_scope": catalog["scope"],
        "current_complete": False,
        "cell_count": len(cells),
        "historical_counts": dict(sorted(Counter(c["historical_status"] for c in cells.values()).items())),
        "sources": sources,
        "cells": list(cells.values()),
    }


def render_markdown(ledger):
    lines = ["# Church behavior evidence ledger", "",
             "Generated from pinned historical receipts. This is not a current-revision acceptance run.",
             "A = historical acceptance recorded; F = attempts without indexed acceptance; ? = no indexed evidence.",
             "The ? marker does not assert that a case was never run. Controls and replays are additional to the 160 cells.",
             "See `behavior-ledger.json` for individual receipt pointers, hashes, limits and runtime/source metadata.", "",
             "| Group | Operation | Haskell Djinn | Haskell Exference | Lean Djinn | Lean Exference | Lean Both |",
             "| --- | --- | --- | --- | --- | --- | --- |"]
    symbols = {"acceptance_recorded": "A", "attempts_without_recorded_acceptance": "F", "no_recorded_evidence": "?"}
    cells = ledger["cells"]
    for start in range(0, len(cells), 5):
        row = cells[start:start + 5]
        lines.append("| " + " | ".join([row[0]["group"], row[0]["operation"]] +
                                       [symbols[c["historical_status"]] for c in row]) + " |")
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--djex-root", type=Path, required=True)
    parser.add_argument("--leant-root", type=Path, required=True)
    parser.add_argument("--catalog", type=Path, default=Path(__file__).with_name("behavior-ledger-catalog.json"))
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--check", action="store_true", help="require committed outputs to match; do not write")
    args = parser.parse_args()
    catalog = json.loads(args.catalog.read_text(encoding="utf-8"))
    ledger = build_ledger(load_specifications(args.djex_root), catalog,
                          {"Djex": args.djex_root, "Leant": args.leant_root})
    outputs = {"behavior-ledger.json": json.dumps(ledger, indent=2, ensure_ascii=False) + "\n",
               "behavior-ledger.md": render_markdown(ledger)}
    for name, contents in outputs.items():
        path = args.output / name
        if args.check:
            if path.read_text(encoding="utf-8") != contents:
                raise ValueError(f"stale generated ledger: {path}")
        else:
            args.output.mkdir(parents=True, exist_ok=True)
            path.write_text(contents, encoding="utf-8", newline="\n")
    print(json.dumps({"cells": ledger["cell_count"], "historical_counts": ledger["historical_counts"],
                      "current_complete": False}))


if __name__ == "__main__":
    main()
