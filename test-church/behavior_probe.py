#!/usr/bin/env python3
"""Synthesize six total Church operations using named Haskell where clauses.

No build is performed. Exact displayed definitions are separately compiled and
executed against the finite predicates. Oracle witnesses and wrong candidates
exist only in that independent replay, never the synthesis environment.
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
import re
import sys

from behavior_runtime import (OutputMilestones, Processes, prepare_output_directory, sha256,
                              source_provenance, validate_limits, write_json)
from behavior_spec import (OPERATIONS, OBSERVATIONS, HASKELL_TYPES,
                           haskell_predicate, haskell_control_source)

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent


def displayed_definition(output, name):
    """Keep the actual equation, including argument binders and continuations."""
    lines = re.sub(r"(?m)^djex\[(?:djinn|exference|both)\]> ?", "", output).splitlines()
    starts = [i for i, line in enumerate(lines)
              if re.match(r"^" + re.escape(name) + r"(?:\s|=)", line)
              and "=" in line and not re.match(r"^" + re.escape(name) + r"\s*::", line)]
    if len(starts) != 1:
        raise ValueError(f"expected one displayed definition for {name}, got {len(starts)}")
    result = [lines[starts[0]]]
    for line in lines[starts[0] + 1:]:
        if not line or not line[0].isspace():
            break
        result.append(line)
    definition = "\n".join(result)
    if re.search(r"\b(?:undefined|error|unsafePerformIO)\b", definition):
        raise ValueError("partial or unsafe candidate in the total behavioral corpus")
    return definition


def commands(engine, operations, args, targets, *, include_false=True):
    lines = [':set prompt ""', ":backend " + engine,
             ":set ranking balanced", ":set select first", ":set render definition",
             ":set allow-unused on", ":set djinn-axioms off",
             f":set djinn-strategy {args.djinn_strategy}",
             f":set candidate-limit {args.window}", f":set quality-window {args.window}",
             f":set choice-budget {args.budget}", f":set max-steps {args.steps}",
             ":show settings"]
    cases = []
    for operation in operations:
        name = f"behavior_{engine}_{operation}"
        command = f":synth {name} :: {targets[operation]} where {haskell_predicate(operation, name)}"
        lines.append(command)
        cases.append({"engine": engine, "operation": operation, "name": name,
                      "type": targets[operation], "command": command,
                      "finite_observation_count": OBSERVATIONS[operation]})
    # No candidate may be displayed merely because its type is inhabited.
    if include_false:
        lines.append(f":synth behavior_{engine}_reject_all :: forall a. a -> a where Prelude.False")
    lines += [":quit", ""]
    return "\n".join(lines), cases


def behavioral_observations(output, *, expect_success, window):
    if "[DJEX_REPL_BEHAVIORAL_PREFLIGHT]" in output:
        raise ValueError("behavioral preflight failed; no predicate outcome established")
    if output.count("[DJEX_REPL_BEHAVIORAL_OBSERVATIONS]") != 1:
        raise ValueError("missing or duplicate behavioral observation summary")
    summaries = re.findall(r"checked=(\d+),\s*true=(\d+),\s*false=(\d+),\s*error=(\d+),\s*timeout=(\d+),\s*window=(\d+)", output)
    if len(summaries) != 1:
        raise ValueError("malformed behavioral observation counts")
    counts = dict(zip(("checked", "true", "false", "error", "timeout", "window"), map(int, summaries[0])))
    if (counts["window"] != window or not 0 < counts["checked"] <= window
            or counts["checked"] != sum(counts[key] for key in ("true", "false", "error", "timeout"))):
        raise ValueError("inconsistent behavioral observation accounting")
    no_match = output.count("[DJEX_REPL_BEHAVIORAL_NO_MATCH]")
    if expect_success:
        if counts["true"] < 1 or no_match:
            raise ValueError("displayed success lacks a successful behavioral observation")
    elif no_match != 1 or counts["true"] or not counts["false"] or counts["error"] or counts["timeout"]:
        raise ValueError("false-oracle control did not establish actual predicate rejection")
    return counts


def validate_settings(output, engine, args):
    """Require the requested engine and limits, not merely successful parsing."""
    text = re.sub(r"(?m)^djex\[(?:djinn|exference|both)\]> ?", "", output)
    if re.search(r"\[DJEX_REPL_(?:SETTING|PARSE|TYPE|UNKNOWN|BACKEND|COMMAND|SOURCE|PROJECTION|IO)", text):
        raise ValueError("REPL setup or query parsing failed")
    if re.findall(r"(?m)^Active backend: (.+)$", text) != [engine]:
        raise ValueError("requested backend was not acknowledged exactly once")
    expected = {"backend": engine, "ranking": "balanced", "select": "first",
                "render": "definition", "prompt": '\"\"', "allow-unused": "on",
                "djinn-axioms": "off", "djinn-strategy": args.djinn_strategy,
                "candidate-limit": str(args.window),
                "quality-window": str(args.window), "choice-budget": str(args.budget),
                "max-steps": str(args.steps)}
    for key, value in expected.items():
        if re.findall(r"(?m)^" + re.escape(key) + r" = (.*)$", text) != [value]:
            raise ValueError("requested setting was not retained: " + key)
    return expected


def latency_observer(cases):
    """This frontend does not echo queries; retain the process-start origin."""
    if not cases:
        return None
    if len(cases) != 1:
        raise ValueError("Haskell latency observation requires one query per process")
    name = cases[0]["name"]
    return OutputMilestones([{
        "id": name, "start_pattern": None,
        "success_pattern": (r"^(?:djex\[(?:djinn|exference|both)\]> ?)?" + re.escape(name)
                            + r"(?=\s|=)(?![^\n]*::)[^\n]*="),
    }])


def replay_source(cases, evaluation_seconds, *, module_name="Main"):
    """One candidate OR the oracle controls; never grant either extra names."""
    if len(cases) > 1:
        raise ValueError("candidate replay modules must be isolated per query")
    controls, control_assertions = haskell_control_source() if not cases else ([], [])
    assertions = [(case["name"], haskell_predicate(case["operation"], case["name"])) for case in cases]
    entry = "main" if module_name == "Main" else "run"
    lines = ["{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}",
             f"module {module_name} ({entry}) where", "import Prelude", "import qualified Prelude",
             "import qualified Control.Exception as E", "import qualified System.Timeout as T",
             "import qualified System.Exit as Exit"]
    for case in cases:
        lines += [case["name"] + " :: " + case["type"], case["definition"]]
    lines += controls
    lines += ["check :: String -> Bool -> IO ()", "check label assertion = do",
              f"  outcome <- T.timeout {int(evaluation_seconds * 1000000)} (E.try (E.evaluate assertion) :: IO (Either E.SomeException Bool))",
              '  case outcome of', '    Just (Right True) -> putStrLn ("PASS " ++ label)',
              '    _ -> putStrLn ("FAIL " ++ label ++ ": " ++ show outcome) >> Exit.exitFailure',
              f"{entry} :: IO ()", f"{entry} = do"]
    for label, assertion in [*assertions, *control_assertions]:
        lines.append("  check " + json.dumps(label) + " (" + assertion + ")")
    return "\n".join(lines) + "\n", [label for label, _ in [*assertions, *control_assertions]]


def isolated_replay_sources(cases, evaluation_seconds):
    """One compilation graph; candidate modules cannot import each other."""
    sources, labels, modules = {}, [], []
    for index, case in enumerate(cases):
        module = "BehaviorCandidate" + str(index)
        source, case_labels = replay_source([case], evaluation_seconds, module_name=module)
        sources[module + ".hs"] = source
        modules.append(module)
        labels.extend(case_labels)
    controls, control_labels = replay_source([], evaluation_seconds, module_name="BehaviorOracleControls")
    sources["BehaviorOracleControls.hs"] = controls
    modules.append("BehaviorOracleControls")
    labels.extend(control_labels)
    sources["BehaviorCandidates.hs"] = "\n".join([
        "module Main (main) where",
        *[f"import qualified {module}" for module in modules],
        "main :: IO ()", "main = do", *[f"  {module}.run" for module in modules], ""])
    return sources, labels


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--djex", type=Path)
    parser.add_argument("--output", type=Path, default=HERE / "results/behavior",
                        help="new or empty directory; prior receipts are never overwritten")
    parser.add_argument("--manifest", type=Path, default=HERE / "manifest.json")
    parser.add_argument("--engine", action="append", choices=("djinn", "exference"))
    parser.add_argument("--operation", action="append", choices=OPERATIONS)
    parser.add_argument("--window", type=int, default=256,
                        help="sets both Djinn raw candidate-limit and behavioral quality-window (default: 256; calibration is explicit)")
    parser.add_argument("--djinn-strategy", choices=("depth-first", "interleave"), default="depth-first",
                        help="explicit Djinn branch strategy; preserves ordinary depth-first default")
    parser.add_argument("--steps", type=int, default=100000)
    parser.add_argument("--budget", type=int, default=100000)
    parser.add_argument("--process-timeout", type=float, default=300)
    parser.add_argument("--observe-latency", action="store_true",
                        help="observe first accepted output visibility by 20 ms monotonic file polling; includes process startup and reports observer overhead")
    parser.add_argument("--evaluation-timeout", type=float, default=2)
    parser.add_argument("--ghc", default="ghc")
    parser.add_argument("--prepare-only", action="store_true")
    args = parser.parse_args()
    validate_limits(args)
    if not math.isfinite(args.evaluation_timeout) or args.evaluation_timeout <= 0 or args.evaluation_timeout > 60:
        parser.error("evaluation-timeout must be in (0,60] seconds")
    if not args.prepare_only and args.djex is None:
        parser.error("--djex is required unless --prepare-only")
    engines = args.engine or ["djinn", "exference"]
    operations = args.operation or list(OPERATIONS)
    if len(set(engines)) != len(engines) or len(set(operations)) != len(operations):
        parser.error("duplicate engine/operation selections are not coverage")
    args.output = prepare_output_directory(args.output)
    environment = args.output / "empty-environment"
    environment.mkdir(exist_ok=True)
    if any(environment.iterdir()):
        raise ValueError("behavioral synthesis requires an empty source environment")
    provenance = source_provenance(args.manifest)
    targets = {row["name"]: row["haskell_target"] for row in provenance["operations"]}
    control_source, control_labels = replay_source([], args.evaluation_timeout)
    control_path = args.output / "OracleControls.hs"
    control_path.write_text(control_source, encoding="utf-8")
    report = {"status": "prepared", "validation_mode": "prepared_only",
              "provenance": provenance, "runner_sha256": sha256(__file__),
              "spec_sha256": sha256(HERE / "behavior_spec.py"),
              "engines": engines, "operations": operations,
              "expected_query_count": len(engines) * len(operations),
              "settings": {"ranking": "balanced", "select": "first", "window": args.window,
                           "djinn_strategy": args.djinn_strategy,
                           "steps": args.steps, "choice_budget": args.budget,
                           "independent_evaluation_seconds": args.evaluation_timeout,
                           "separate_process_guard_seconds": args.process_timeout},
              "named_synthesis_providers": [], "cases": [],
              "latency_observation_enabled": args.observe_latency,
              "oracle_control_source": str(control_path.resolve()),
              "oracle_control_sha256": sha256(control_path),
              "oracle_control_assertion_count": len(control_labels)}
    planned = []
    for engine in engines:
        for operation in operations:
            source, cases = commands(engine, [operation], args, targets, include_false=False)
            label = engine + "-" + operation
            path = args.output / f"{label}.commands.txt"
            path.write_text(source, encoding="utf-8")
            planned.append((engine, label, source, cases))
        source, cases = commands(engine, [], args, targets)
        label = engine + "-reject-all"
        (args.output / f"{label}.commands.txt").write_text(source, encoding="utf-8")
        planned.append((engine, label, source, cases))
    write_json(args.output / "results.json", report)
    if args.prepare_only:
        print(f"Prepared {report['expected_query_count']} Haskell behavioral queries; no processes run.")
        return 0
    digest = sha256(args.djex)
    report.update(status="running", validation_mode="live_synthesis_then_exact_ghc_execution",
                  executable_path=str(args.djex.resolve()), executable_sha256=digest)
    processes = Processes(args.output, args.process_timeout)
    try:
        for engine, label, source, cases in planned:
            observer = latency_observer(cases) if args.observe_latency else None
            live = processes.run(label, [args.djex.resolve(), "repl", "--ignore-startup", "--environment", environment.resolve()], source=source, cwd=ROOT, observe=observer)
            if live.returncode:
                raise ValueError(f"{engine} live process failed: {live.returncode}")
            # Semantic rejection has its own diagnostic; syntax/settings/type
            # failures must never be mistaken for the deliberate false oracle.
            settings = validate_settings(live.stdout + live.stderr, engine, args)
            observations = behavioral_observations(live.stdout + live.stderr,
                                                   expect_success=bool(cases), window=args.window)
            for case in cases:
                case["definition"] = displayed_definition(live.stdout, case["name"])
                case["behavioral_observations"] = observations
                case["validated_settings"] = settings
                report["cases"].append(case)
            if observer is not None:
                observer.validate_counts({case["name"]: 1 for case in cases})
            if not cases:
                report.setdefault("false_oracle_results", []).append({"engine": engine, "observations": observations})
            if re.search(r"(?m)^behavior_" + engine + r"_reject_all(?:\s|=).*?=", live.stdout):
                raise ValueError("where False displayed a successful definition")
            write_json(args.output / "results.json", report)
        expected_cells = {(engine, operation) for engine in engines for operation in operations}
        if (len(report["cases"]) != len(expected_cells)
                or {(case["engine"], case["operation"]) for case in report["cases"]} != expected_cells
                or [row["engine"] for row in report.get("false_oracle_results", [])] != engines):
            raise ValueError("candidate or false-oracle matrix differs from its exact selected inventory")
        sources, labels = isolated_replay_sources(report["cases"], args.evaluation_timeout)
        report["replay_modules"] = []
        for filename, source in sources.items():
            path = args.output / filename
            path.write_text(source, encoding="utf-8")
            report["replay_modules"].append({"path": str(path.resolve()), "sha256": sha256(path)})
        replay = args.output / "BehaviorCandidates.hs"
        executable = args.output / "behavior-candidates.exe"
        compiler = processes.run("ghc", [args.ghc, "-v0", "-O0", "-fforce-recomp", "-i" + str(args.output.resolve()), "-outputdir", args.output.resolve(), replay.resolve(), "-o", executable.resolve()], cwd=ROOT)
        report.update(replay_source_sha256=sha256(replay), compiler_exit_code=compiler.returncode)
        if compiler.returncode:
            raise ValueError("independent candidate/control GHC compilation failed")
        execution = processes.run("behavior", [executable.resolve()], cwd=ROOT)
        actual = [line.removeprefix("PASS ") for line in execution.stdout.splitlines() if line.startswith("PASS ")]
        if execution.returncode or actual != labels:
            raise ValueError("independent behavioral assertions failed or were omitted/duplicated")
        report.update(status="passed", execution_exit_code=execution.returncode,
                      candidate_count=len(report["cases"]), independent_assertion_count=len(labels),
                      exact_type_wrong_controls=10, oracle_positive_controls=6,
                      specialized_swap_control=1, false_oracle_queries=len(engines))
    except Exception as failure:
        report.update(status="failed", failure=str(failure))
    finally:
        report["executable_unchanged"] = sha256(args.djex) == digest
        if not report["executable_unchanged"]:
            report.update(status="failed", failure="executable changed during acceptance")
        report["processes"] = processes.rows
        write_json(args.output / "results.json", report)
    print(f"Haskell behavioral corpus: {report['status']}; {len(report['cases'])}/{report['expected_query_count']} candidates")
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    for stream in (sys.stdout, sys.stderr):
        if hasattr(stream, "reconfigure"):
            stream.reconfigure(encoding="utf-8")
    raise SystemExit(main())
