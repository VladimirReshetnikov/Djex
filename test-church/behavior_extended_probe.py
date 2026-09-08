#!/usr/bin/env python3
"""Opt-in live Haskell acceptance for behavior_extended_spec.

No build is performed. Each synthesis query, false control, and independent
replay owns a fresh process. Every cell retains its outcome when another
fails. Candidate modules have NoImplicitPrelude and cannot import observation
adapters, witnesses, other candidates, or ordinary Prelude value providers.

Defaults match the accepted six-operation receipts: Djinn interleave,
500000 choices and window 65536; Exference 100000 steps and window 256.
These are finite bounded experiments, not completeness claims.
"""
from __future__ import annotations

import argparse
import math
from pathlib import Path
import re
import shutil
import sys
from types import SimpleNamespace
import json

from behavior_runtime import Processes, prepare_output_directory, sha256, validate_limits, write_json
from behavior_probe import behavioral_observations, displayed_definition
import behavior_extended_spec as spec

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
NUMERIC_MODULE = "ExtendedNumeric"
# refreshDjinnProjection retains these exact checked Haskell syntax families
# independently of module imports. They are not ordinary value providers.
DJINN_BUILTIN_DECLARATIONS = (
    "data [] v0 = [] | (:) v0 [v0]",
    "data () = ()",
)
# No local Int declaration or replacement numeric domain is introduced.
# In the source-only workspace, implicit package Prelude contributes no
# declarations, so the checked source inventory retains the unqualified
# external Int and Djinn registers its inferred proper-kind abstract stub.
# In the isolated GHC worker/replay, ordinary implicit Prelude supplies the
# actual native Int and (+). Only these two explicit values are exported.
NUMERIC_SOURCE = "\n".join([
    "module ExtendedNumeric (numericZero, numericSuccessor) where",
    *(line.replace("Prelude.Int", "Int").replace("Prelude.+", "+")
      for line in spec.HASKELL_NUMERIC_PROVIDERS),
    "",
])


def profile(args, engine):
    return SimpleNamespace(
        window=args.djinn_window if engine == "djinn" else args.exference_window,
        budget=args.djinn_budget if engine == "djinn" else args.exference_budget,
        djinn_strategy=args.djinn_strategy if engine == "djinn" else "depth-first",
        steps=args.steps, process_timeout=args.process_timeout, queue=args.queue,
    )


def target_for_search(operation):
    # Preserve the original full signature, including native Int for length.
    return spec.HASKELL_TYPES[operation]


def djinn_axiom_setting(engine, operation):
    return "on" if engine == "djinn" and operation == "length" else "off"


def validate_cell_settings(output, engine, operation, settings):
    """Exact settings, including the sole cell with two permitted axioms."""
    text = re.sub(r"(?m)^djex\[(?:djinn|exference|both)\]> ?", "", output)
    if re.search(r"\[DJEX_REPL_(?:SETTING|PARSE|TYPE|UNKNOWN|BACKEND|COMMAND|SOURCE|PROJECTION|IO)", text):
        raise ValueError("REPL setup or query parsing failed")
    if re.findall(r"(?m)^Active backend: (.+)$", text) != [engine]:
        raise ValueError("requested backend was not acknowledged exactly once")
    expected = {"backend": engine, "ranking": "balanced", "select": "first",
                "render": "definition", "prompt": '\"\"', "allow-unused": "on",
                "djinn-axioms": djinn_axiom_setting(engine, operation),
                "djinn-strategy": settings.djinn_strategy,
                "candidate-limit": str(settings.window),
                "quality-window": str(settings.window),
                "choice-budget": str(settings.budget),
                "max-steps": str(settings.steps)}
    for key, value in expected.items():
        if re.findall(r"(?m)^" + re.escape(key) + r" = (.*)$", text) != [value]:
            raise ValueError("requested setting was not retained: " + key)
    return expected


def commands(engine, operation, settings, numeric_path):
    is_false = operation is None
    name = f"extended_{engine}_" + ("reject_all" if is_false else operation)
    lines = [':set prompt ""', ":backend " + engine]
    if operation == "length":
        lines += [":load " + json.dumps(str(numeric_path.resolve())),
                  ":module " + NUMERIC_MODULE]
    lines += [
        ":set ranking balanced", ":set select first", ":set render definition",
        ":set allow-unused on", ":set djinn-axioms " + djinn_axiom_setting(engine, operation),
        ":set djinn-strategy " + settings.djinn_strategy,
        f":set candidate-limit {settings.window}",
        f":set quality-window {settings.window}",
        f":set choice-budget {settings.budget}",
        f":set max-steps {settings.steps}",
        f":set max-queue {settings.queue}",
        ":browse", ":show settings",
    ]
    target = "forall a. a -> a" if is_false else target_for_search(operation)
    predicate = "Prelude.False" if is_false else spec.haskell_predicate(operation, name)
    lines += [f":synth {name} :: {target} where {predicate}", ":quit", ""]
    return "\n".join(lines), {
        "engine": engine, "operation": operation, "name": name,
        "type": "forall a. a -> a" if is_false else spec.HASKELL_TYPES[operation],
        "search_type": target, "djinn_axioms": djinn_axiom_setting(engine, operation),
        "predicate": predicate,
        "kind": "false_oracle_query" if is_false else "synthesis",
        "finite_observation_count": 1 if is_false else spec.OBSERVATIONS[operation],
        "provider_inventory": [] if is_false else spec.REQUIRED_PROVIDERS[operation]["haskell"],
        "source_case_id": None if is_false else spec.SOURCE_CASE_IDS.get(operation),
        "provenance": None if is_false else spec.OPERATION_PROVENANCE[operation],
        "settings": vars(settings),
    }


def observed_inventory(output, engine, operation):
    """Inspect the actual checked search surface, not only the fixture file."""
    text = re.sub(r"(?m)^djex\[(?:djinn|exference|both)\]> ?", "", output)
    header = "-- Djinn" if engine == "djinn" else "-- Current source scope"
    matches = list(re.finditer(r"(?m)^" + re.escape(header) + r"\s*$", text))
    if len(matches) != 1:
        raise ValueError("missing or duplicate checked inventory browse")
    tail = text[matches[0].end():]
    end = re.search(r"(?m)^backend = ", tail)
    if end is None:
        raise ValueError("browse inventory has no settings boundary")
    declarations = [line.strip() for line in tail[:end.start()].splitlines() if line.strip()]
    builtins = [line for line in declarations if line in DJINN_BUILTIN_DECLARATIONS]
    if builtins and (engine != "djinn" or len(builtins) != len(DJINN_BUILTIN_DECLARATIONS)
                     or set(builtins) != set(DJINN_BUILTIN_DECLARATIONS)):
        raise ValueError("unexpected, incomplete, or duplicate builtin declaration inventory")
    search_declarations = [line for line in declarations if line not in DJINN_BUILTIN_DECLARATIONS]
    if operation != "length":
        if not ((not builtins and search_declarations == ["(no declarations)"])
                or (builtins and not search_declarations)):
            raise ValueError("providers-off query exposed declarations: " + repr(declarations))
        return {"declarations": declarations, "builtin_declarations": builtins,
                "values": [], "type_aliases": []}
    values, abstract_types = {}, []
    for line in search_declarations:
        value = re.fullmatch(r"((?:ExtendedNumeric\.)?numeric(?:Zero|Successor))\s*::\s*(.+)", line)
        if value:
            name = value.group(1).split(".")[-1]
            if name in values:
                raise ValueError("duplicate numeric provider in checked inventory")
            ty = re.sub(r"\bPrelude\.Int\b", "Int", value.group(2))
            values[name] = " ".join(ty.replace("(", "").replace(")", "").split())
            continue
        # A backend may retain the external primitive only as an abstract type;
        # no constructor or additional function is permitted by this branch.
        if line == "type Int :: Type":
            if abstract_types:
                raise ValueError("duplicate native Int abstract declaration")
            abstract_types.append(line)
            continue
        raise ValueError("unexpected declaration in numeric search inventory: " + line)
    if engine == "djinn" and abstract_types != ["type Int :: Type"]:
        raise ValueError("Djinn numeric inventory lacks its checked native Int abstract declaration")
    if values != {"numericZero": "Int", "numericSuccessor": "Int -> Int"}:
        raise ValueError("numeric provider names/types differ from the exact inventory")
    return {"declarations": declarations, "builtin_declarations": builtins,
            "values": values, "type_aliases": [], "abstract_types": abstract_types}


def replay_sources(name, target, definition, assertion, label, evaluation_timeout, *, numeric):
    """Candidate's full type is checked in a module with no oracle imports."""
    candidate = [
        "{-# LANGUAGE NoImplicitPrelude, RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}",
        f"module Candidate ({name}) where",
        "import Prelude (Int)",
        "import qualified Prelude (Int)",
    ]
    if numeric:
        candidate += [
            "import ExtendedNumeric (numericZero, numericSuccessor)",
        ]
    candidate += [name + " :: " + target, definition, ""]
    runtime = [
        "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}",
        "module Main (main) where",
        "import Prelude", "import qualified Prelude", "import qualified Candidate",
        "import qualified Control.Exception as E",
        "import qualified System.Timeout as T",
        "import qualified System.Exit as Exit",
        "main :: IO ()", "main = do",
        f"  outcome <- T.timeout {int(evaluation_timeout * 1000000)} (E.try (E.evaluate ({assertion})) :: IO (Either E.SomeException Bool))",
        "  case outcome of",
        '    Just (Right True) -> putStrLn ' + json.dumps("PASS " + label),
        '    _ -> putStrLn (' + json.dumps("FAIL " + label + ": ") + " ++ show outcome) >> Exit.exitFailure",
        "",
    ]
    sources = {"Candidate.hs": "\n".join(candidate), "Main.hs": "\n".join(runtime)}
    if numeric:
        sources[NUMERIC_MODULE + ".hs"] = NUMERIC_SOURCE
    return sources


def controls_for(operations):
    rows = []
    for operation in operations:
        rows.append({
            "id": "positive-" + operation, "operation": operation,
            "kind": "positive_oracle_witness", "type": spec.HASKELL_TYPES[operation],
            "definition": "generated = " + spec.HASKELL_WITNESSES[operation],
            "assertion": spec.haskell_predicate(operation, "Candidate.generated"),
        })
        for label, term in spec.HASKELL_WRONG[operation].items():
            rows.append({
                "id": "wrong-" + label, "operation": operation,
                "kind": spec.CONTROL_KINDS[label], "type": spec.HASKELL_TYPES[operation],
                "definition": "generated = " + term,
                "assertion": "Prelude.not (" + spec.haskell_predicate(operation, "Candidate.generated") + ")",
            })
    if "either" in operations:
        declarations, assertions = spec.haskell_control_source()
        signature = next(line for line in declarations if line.startswith("extendedWrong_eitherMono :: "))
        definition = next(line for line in declarations if line.startswith("extendedWrong_eitherMono onLeft"))
        assertion = dict(assertions)["either_specialized_wrong_handler"]
        rows.append({
            "id": "wrong-either-specialized-handler", "operation": "either",
            "kind": spec.CONTROL_KINDS["either_specialized_wrong_handler"],
            "type": signature.split(" :: ", 1)[1],
            "definition": definition.replace("extendedWrong_eitherMono", "generated", 1),
            "assertion": assertion.replace("extendedWrong_eitherMono", "Candidate.generated"),
        })
    return rows


def write_replay(directory, row, evaluation_timeout, *, candidate=False):
    directory.mkdir()
    name = row["name"] if candidate else "generated"
    assertion = (spec.haskell_predicate(row["operation"], "Candidate." + name)
                 if candidate else row["assertion"])
    sources = replay_sources(name, row["type"], row["definition"], assertion,
                             row["id"], evaluation_timeout, numeric=row["operation"] == "length")
    files = []
    for filename, source in sources.items():
        path = directory / filename
        path.write_text(source, encoding="utf-8")
        files.append({"path": str(path.resolve()), "sha256": sha256(path)})
    row["replay"] = {
        "directory": str(directory.resolve()), "sources": files,
        "candidate_import_policy": "NoImplicitPrelude; native Int types only, plus exactly zero/successor for length",
        "fully_typed_source": str((directory / "Candidate.hs").resolve()),
        "fully_typed_source_sha256": sha256(directory / "Candidate.hs"),
        "expected_pass_labels": [row["id"]],
        "status": "prepared",
    }


def execute_replay(processes, compiler, row, label_prefix):
    replay = row["replay"]
    directory = Path(replay["directory"])
    executable = directory / ("replay.exe" if sys.platform == "win32" else "replay")
    before = {entry["path"]: entry["sha256"] for entry in replay["sources"]}
    replay["status"] = "running"
    try:
        command = [compiler, "-v0", "-O0", "-fforce-recomp",
                   "-i", "-i" + str(directory), "-outputdir", directory,
                   directory / "Main.hs", "-o", executable]
        compiled = processes.run(label_prefix + "-ghc", command, cwd=directory)
        replay["compiler_exit_code"] = compiled.returncode
        if compiled.returncode:
            raise ValueError("independent full-signature GHC compilation failed")
        digest = sha256(executable)
        replay.update(executable_path=str(executable), executable_sha256=digest)
        executed = processes.run(label_prefix + "-execute", [executable], cwd=directory)
        actual = [line[5:] for line in executed.stdout.splitlines() if line.startswith("PASS ")]
        replay.update(execution_exit_code=executed.returncode, observed_pass_labels=actual,
                      executable_unchanged=sha256(executable) == digest)
        if (executed.returncode or actual != replay["expected_pass_labels"]
                or any(line.startswith("FAIL ") for line in executed.stdout.splitlines())
                or not replay["executable_unchanged"]):
            raise ValueError("independent predicate failed or pass evidence was omitted/duplicated")
        replay["status"] = "passed"
    except Exception as failure:
        replay.update(status="failed", failure=str(failure))
    finally:
        replay["sources_unchanged"] = all(Path(path).is_file() and sha256(path) == digest
                                          for path, digest in before.items())
        if not replay["sources_unchanged"]:
            replay.update(status="failed", failure="replay source changed during independent checking")
    return replay["status"] == "passed"


def resolve_executable(value, label):
    resolved = shutil.which(str(value))
    if resolved is None:
        path = Path(value).resolve()
        if not path.is_file():
            raise ValueError(label + " executable was not found: " + str(value))
        resolved = str(path)
    return Path(resolved).resolve()


def persist(output, report, processes=None):
    if processes is not None:
        report["processes"] = processes.rows
    write_json(output / "results.json", report)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--djex", type=Path)
    parser.add_argument("--ghc", default="ghc")
    parser.add_argument("--manifest", type=Path, default=HERE / "manifest.json")
    parser.add_argument("--output", type=Path, required=True,
                        help="fresh or empty directory; earlier receipts are never overwritten")
    parser.add_argument("--engine", action="append", choices=("djinn", "exference"))
    parser.add_argument("--operation", action="append", choices=spec.OPERATIONS)
    parser.add_argument("--djinn-window", type=int, default=65536)
    parser.add_argument("--djinn-budget", type=int, default=500000)
    parser.add_argument("--djinn-strategy", choices=("depth-first", "interleave"), default="interleave")
    parser.add_argument("--exference-window", type=int, default=256)
    parser.add_argument("--exference-budget", type=int, default=100000)
    parser.add_argument("--steps", type=int, default=100000)
    parser.add_argument("--queue", type=int, default=8192,
                        help="explicit shared Exference queue cap; matches the established CLI default")
    parser.add_argument("--process-timeout", type=float, default=300)
    parser.add_argument("--evaluation-timeout", type=float, default=2)
    parser.add_argument("--prepare-only", action="store_true")
    args = parser.parse_args()
    engines = args.engine or ["djinn", "exference"]
    operations = args.operation or list(spec.OPERATIONS)
    if len(engines) != len(set(engines)) or len(operations) != len(set(operations)):
        parser.error("duplicate selections do not constitute coverage")
    for engine in engines:
        validate_limits(profile(args, engine))
    if args.queue < 0:
        parser.error("queue must be nonnegative")
    if not math.isfinite(args.evaluation_timeout) or not 0 < args.evaluation_timeout <= 60:
        parser.error("evaluation-timeout must be in (0,60] seconds")
    if not args.prepare_only and args.djex is None:
        parser.error("--djex is required unless --prepare-only")
    provenance = spec.source_provenance(args.manifest)
    output = prepare_output_directory(args.output).resolve()
    empty = output / "empty-environment"
    empty.mkdir()
    fixtures = output / "numeric-fixture"
    fixtures.mkdir()
    numeric_path = fixtures / (NUMERIC_MODULE + ".hs")
    numeric_path.write_text(NUMERIC_SOURCE, encoding="utf-8")
    source_files = [Path(__file__), HERE / "behavior_extended_spec.py",
                    HERE / "behavior_probe.py", HERE / "behavior_runtime.py",
                    HERE / "behavior_spec.py", Path(args.manifest).resolve(),
                    Path(provenance["church_source_path"]), numeric_path]
    source_hashes = {str(path.resolve()): sha256(path) for path in source_files}
    report = {
        "status": "prepared", "validation_mode": "prepared_only",
        "provenance": provenance, "engines": engines, "operations": operations,
        "settings": {engine: vars(profile(args, engine)) for engine in engines},
        "independent_evaluation_seconds": args.evaluation_timeout,
        "expected_synthesis_cells": len(engines) * len(operations),
        "expected_false_controls": len(engines),
        "numeric_fixture": {"path": str(numeric_path), "sha256": sha256(numeric_path),
                            "native_type": "Int (GHC implicit Prelude; checked unqualified external source type)",
                            "value_inventory": spec.REQUIRED_PROVIDERS["length"]["haskell"]},
        "source_hashes": source_hashes, "cells": [], "oracle_controls": [],
        "partial_case_coverage": "registered in provenance only; never executed or counted as acceptance",
    }
    plans = []
    for engine in engines:
        for operation in [*operations, None]:
            source, cell = commands(engine, operation, profile(args, engine), numeric_path)
            cell["id"] = engine + "-" + (operation if operation else "reject-all")
            cell["status"] = "prepared"
            command_path = output / (cell["id"] + ".commands.txt")
            command_path.write_text(source, encoding="utf-8")
            cell.update(command_path=str(command_path), command_sha256=sha256(command_path))
            report["cells"].append(cell)
            plans.append((cell, source))
    for control in controls_for(operations):
        control["status"] = "prepared"
        write_replay(output / ("control-" + control["id"]), control, args.evaluation_timeout)
        report["oracle_controls"].append(control)
    persist(output, report)
    if args.prepare_only:
        print(f"Prepared {report['expected_synthesis_cells']} synthesis cells, "
              f"{len(engines)} false controls, and {len(report['oracle_controls'])} isolated oracle controls; no processes run.")
        return 0

    processes = Processes(output, args.process_timeout)
    executable_hashes = {}
    try:
        djex = resolve_executable(args.djex, "Djex")
        ghc = resolve_executable(args.ghc, "GHC")
        live_ghc = resolve_executable("ghc", "behavioral worker's PATH-resolved GHC")
        executable_hashes = {str(path): sha256(path) for path in (djex, ghc, live_ghc)}
        report.update(status="running", validation_mode="live_synthesis_and_isolated_exact_source_ghc_execution",
                      executable_hashes=executable_hashes, djex_path=str(djex),
                      independent_ghc_path=str(ghc), live_path_ghc=str(live_ghc))
        persist(output, report, processes)
        # Controls run in separate modules/processes and are never loaded into
        # a synthesis workspace. Failure remains local; other cells still run.
        for control in report["oracle_controls"]:
            control["status"] = "running"
            persist(output, report, processes)
            passed = execute_replay(processes, ghc, control, "control-" + control["id"])
            control["status"] = "passed" if passed else "failed"
            persist(output, report, processes)
        for cell, source in plans:
            cell["status"] = "running"
            persist(output, report, processes)
            try:
                if any(empty.iterdir()):
                    raise ValueError("providers-off environment is no longer empty")
                if sha256(numeric_path) != report["numeric_fixture"]["sha256"]:
                    raise ValueError("numeric source fixture changed")
                live = processes.run(cell["id"] + "-synthesis",
                    [djex, "repl", "--ignore-startup", "--environment", empty],
                    source=source, cwd=ROOT)
                cell["synthesis_exit_code"] = live.returncode
                if live.returncode:
                    raise ValueError("synthesis process exited unsuccessfully")
                cell["validated_settings"] = validate_cell_settings(
                    live.stdout + live.stderr, cell["engine"], cell["operation"],
                    profile(args, cell["engine"]))
                settings_output = re.sub(r"(?m)^djex\[(?:djinn|exference|both)\]> ?", "", live.stdout)
                if re.findall(r"(?m)^max-queue = (.*)$", settings_output) != [str(args.queue)]:
                    raise ValueError("requested Exference queue bound was not acknowledged exactly once")
                cell["validated_settings"]["max-queue"] = str(args.queue)
                cell["observed_inventory"] = observed_inventory(
                    live.stdout, cell["engine"], cell["operation"])
                is_false = cell["kind"] == "false_oracle_query"
                cell["behavioral_observations"] = behavioral_observations(
                    live.stdout + live.stderr, expect_success=not is_false,
                    window=cell["settings"]["window"])
                if is_false:
                    clean = re.sub(r"(?m)^djex\[(?:djinn|exference|both)\]> ?", "", live.stdout)
                    if re.search(r"(?m)^" + re.escape(cell["name"]) + r"(?:\s|=)[^\n]*=", clean):
                        raise ValueError("where False displayed a successful definition")
                    cell["status"] = "passed"
                else:
                    cell["definition"] = displayed_definition(live.stdout, cell["name"])
                    cell["status"] = "synthesized"
                    write_replay(output / ("candidate-" + cell["id"]), cell,
                                 args.evaluation_timeout, candidate=True)
                    persist(output, report, processes)
                    passed = execute_replay(processes, ghc, cell, "candidate-" + cell["id"])
                    cell["status"] = "passed" if passed else "failed"
                    if not passed:
                        cell["failure"] = cell["replay"].get("failure", "independent replay failed")
            except Exception as failure:
                cell.update(status="failed", failure=str(failure))
            persist(output, report, processes)
        controls_passed = all(row["status"] == "passed" for row in report["oracle_controls"])
        for cell in report["cells"]:
            cell["oracle_controls_passed"] = (all(
                row["status"] == "passed" for row in report["oracle_controls"]
                if row["operation"] == cell["operation"])
                if cell["operation"] is not None else True)
            cell["accepted"] = cell["status"] == "passed" and cell["oracle_controls_passed"]
        report["status"] = ("passed" if controls_passed and
                            all(row["accepted"] for row in report["cells"]) else "failed")
    except Exception as failure:
        report.update(status="failed", failure=str(failure))
        for row in [*report["cells"], *report["oracle_controls"]]:
            if row["status"] in ("prepared", "running"):
                row.update(status="not_run", reason="run-level failure: " + str(failure))
    finally:
        report["sources_unchanged"] = all(Path(path).is_file() and sha256(path) == digest
                                          for path, digest in source_hashes.items())
        report["executables_unchanged"] = bool(executable_hashes) and all(
            Path(path).is_file() and sha256(path) == digest for path, digest in executable_hashes.items())
        if not report["sources_unchanged"] or not report["executables_unchanged"]:
            report.update(status="failed", integrity_failure="source or executable changed, or executable evidence was unavailable")
        synthesis = [row for row in report["cells"] if row["kind"] == "synthesis"]
        report["passed_synthesis_cells"] = sum(row.get("accepted", False) for row in synthesis)
        report["failed_synthesis_cells"] = [row["id"] for row in synthesis if not row.get("accepted", False)]
        report["passed_false_controls"] = sum(row["status"] == "passed" for row in report["cells"]
                                               if row["kind"] == "false_oracle_query")
        report["passed_oracle_controls"] = sum(row["status"] == "passed" for row in report["oracle_controls"])
        persist(output, report, processes)
    print(f"Extended Haskell corpus: {report['status']}; "
          f"{report.get('passed_synthesis_cells', 0)}/{report['expected_synthesis_cells']} accepted synthesis cells.")
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    for stream in (sys.stdout, sys.stderr):
        if hasattr(stream, "reconfigure"):
            stream.reconfigure(encoding="utf-8")
    raise SystemExit(main())
