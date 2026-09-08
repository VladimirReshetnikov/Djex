#!/usr/bin/env python3
"""Haskell acceptance runner for all 19 explicit-default Church signatures.

This runner performs no build. Every engine/operation, false query, oracle
control, primitive control, and candidate replay uses fresh owned processes.
behavior_runtime.Processes owns process-tree cleanup and raw captures; the
existing extended runner owns the common independent GHC replay executor.
No old specification or runner globals are changed.

Search uses no Prelude providers. Only at loads PartialNumeric, whose exact
value inventory is the generic partialIntCase primitive. Its implicit Prelude
retains native Int in both the checked source inventory and the isolated GHC
worker. Candidate replay uses the original manifest-expanded signature WITH
the documented default inserted, including every original leading type binder.

Oracle definitions, witnesses, wrong controls, and observation helpers are
never search providers. Candidate.hs has NoImplicitPrelude and cannot import
any oracle module. Controls are independently compiled in separate directories
with their complete types. A cell is accepted only after its actual synthesis,
displayed-source GHC execution, operation controls, engine false control, and
all integrity checks pass. Failures remain in the receipt without hiding other
cells. A finite accepted matrix is not a completeness or parametricity proof.

--prepare-only writes commands, isolated control modules, inventories, and an
honestly prepared receipt; it invokes no compiler or synthesis process.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import sys

from behavior_runtime import Processes, prepare_output_directory, sha256, validate_limits, write_json
from behavior_probe import behavioral_observations, displayed_definition, latency_observer
from behavior_extended_probe import execute_replay, profile, resolve_executable
import behavior_partial_spec as spec

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
NUMERIC_MODULE = "PartialNumeric"
# refreshDjinnProjection retains these exact checked Haskell syntax families
# independently of module imports. They are not ordinary value providers.
DJINN_BUILTIN_DECLARATIONS = (
    "data [] v0 = [] | (:) v0 [v0]",
    "data () = ()",
)


def numeric_source():
    providers = spec.REQUIRED_PROVIDERS["at"]["haskell"]
    if len(providers) != 1 or providers[0]["name"] != "partialIntCase":
        raise ValueError("the partial spec changed its exact generic Int inventory")
    provider = providers[0]
    # Match the accepted native-Int source boundary: package Prelude does not
    # add ordinary value declarations to this source-only workspace. The
    # isolated GHC worker resolves the same Int and branch operators through
    # implicit Prelude. Only the exact specification primitive is exported.
    return "\n".join([
        "{-# LANGUAGE RankNTypes #-}",
        f"module {NUMERIC_MODULE} (partialIntCase) where",
        "partialIntCase :: " + provider["type"],
        "partialIntCase = " + provider["definition"], "",
    ])


NUMERIC_SOURCE = numeric_source()


def target_for_search(operation):
    target = spec.HASKELL_TYPES[operation]
    # Keep every original binder and the native Int argument unchanged.
    if operation != "at" and re.search(r"\bInt\b", target):
        raise ValueError("an unexpected source Int requirement needs a reviewed provider inventory: " + operation)
    return target


def djinn_axiom_setting(engine, operation):
    return "on" if engine == "djinn" and operation == "at" else "off"


def validate_cell_settings(output, engine, operation, settings):
    """Require the actual cell policy; only Djinn at admits its one value axiom."""
    text = _clean_output(output)
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
                "choice-budget": str(settings.budget), "max-steps": str(settings.steps)}
    for key, value in expected.items():
        if re.findall(r"(?m)^" + re.escape(key) + r" = (.*)$", text) != [value]:
            raise ValueError("requested setting was not retained: " + key)
    return expected


def commands(engine, operation, settings, numeric_path):
    is_false = operation is None
    name = f"partial_{engine}_" + ("reject_all" if is_false else operation)
    lines = [':set prompt ""', ":backend " + engine]
    if operation == "at":
        lines += [":load " + json.dumps(str(numeric_path.resolve())),
                  ":module " + NUMERIC_MODULE]
    lines += [
        ":set ranking balanced", ":set select first", ":set render definition",
        ":set allow-unused on", ":set djinn-axioms " + djinn_axiom_setting(engine, operation),
        ":set djinn-strategy " + settings.djinn_strategy,
        f":set candidate-limit {settings.window}", f":set quality-window {settings.window}",
        f":set choice-budget {settings.budget}", f":set max-steps {settings.steps}",
        f":set max-queue {settings.queue}", ":browse", ":show settings",
    ]
    target = "forall a. a -> a" if is_false else target_for_search(operation)
    predicate = "Prelude.False" if is_false else spec.haskell_predicate(operation, name)
    lines += [f":synth {name} :: {target} where {predicate}", ":quit", ""]
    provenance = None if is_false else spec.PARTIAL_CASES[spec.SOURCE_CASE_IDS[operation]]
    return "\n".join(lines), {
        "id": engine + "-" + (operation if operation else "reject-all"),
        "engine": engine, "operation": operation, "name": name,
        "kind": "false_oracle_query" if is_false else "synthesis",
        "type": "forall a. a -> a" if is_false else spec.HASKELL_TYPES[operation],
        "search_type": target, "predicate": predicate,
        "djinn_axioms": djinn_axiom_setting(engine, operation),
        "source_case_id": None if is_false else spec.SOURCE_CASE_IDS[operation],
        "original_source_signature": None if is_false else provenance["source_signature"],
        "original_expanded_type": None if is_false else spec.ORIGINAL_HASKELL_TYPES[operation],
        "defaulted_expanded_type": None if is_false else spec.DEFAULTED_TYPES[operation],
        "source_default_type": None if is_false else provenance["source_default_type"],
        "manifest_default_type": None if is_false else provenance["manifest_default_type"],
        "default_argument_position": None if is_false else provenance["default_argument_position"],
        "original_binder_order": None if is_false else provenance["original_binder_order"],
        "finite_observation_count": 1 if is_false else spec.OBSERVATIONS[operation],
        "fixture_ids": [] if is_false else [row["id"] for row in spec.FIXTURES[operation]],
        "provider_inventory": [] if is_false else spec.REQUIRED_PROVIDERS[operation]["haskell"],
        "settings": vars(settings), "status": "prepared", "accepted": False,
    }


def _clean_output(output):
    return re.sub(r"(?m)^djex\[(?:djinn|exference|both)\]> ?", "", output)


def _provider_type(text):
    """Parse this primitive's arrow/forall fragment without erasing grouping.

    Parentheses around Int -> r must survive: dropping them would confuse
    (Int -> r) -> r with Int -> r -> r. Only harmless binder alpha-renaming and
    native Int qualification are normalized.
    """
    token_pattern = r"forall\b|∀|->|→|[().]|[A-Za-z_][A-Za-z_0-9']*(?:\.[A-Za-z_][A-Za-z_0-9']*)*"
    tokens = re.findall(token_pattern, text)
    if "".join(tokens) != re.sub(r"\s+", "", text):
        raise ValueError("unsupported token in checked primitive type: " + text)
    position = 0

    def parse_type():
        nonlocal position
        if position < len(tokens) and tokens[position] in ("forall", "∀"):
            position += 1
            binders = []
            while position < len(tokens) and tokens[position] != ".":
                binder = tokens[position]
                if not re.fullmatch(r"[a-z][A-Za-z_0-9']*", binder):
                    raise ValueError("invalid primitive forall binder")
                binders.append(binder)
                position += 1
            if not binders or len(set(binders)) != len(binders) or position == len(tokens):
                raise ValueError("missing or duplicated primitive forall binders")
            position += 1
            return ("forall", tuple(binders), parse_type())
        left = atom()
        if position < len(tokens) and tokens[position] in ("->", "→"):
            position += 1
            return ("arrow", left, parse_type())
        return left

    def atom():
        nonlocal position
        if position >= len(tokens):
            raise ValueError("incomplete primitive type")
        token = tokens[position]
        position += 1
        if token == "(":
            result = parse_type()
            if position >= len(tokens) or tokens[position] != ")":
                raise ValueError("unbalanced primitive type")
            position += 1
            return result
        if not re.fullmatch(r"[A-Za-z_][A-Za-z_0-9']*(?:\.[A-Za-z_][A-Za-z_0-9']*)*", token):
            raise ValueError("invalid primitive type atom")
        return ("name", token)

    parsed = parse_type()
    if position != len(tokens):
        raise ValueError("trailing primitive type syntax")
    # Browse may print an implicit top-level scheme. Only its one free type
    # variable can be closed; extra variables and vacuous foralls fail equality.
    if parsed[0] != "forall":
        free = []

        def collect(node):
            if node[0] == "name" and re.fullmatch(r"[a-z][A-Za-z_0-9']*", node[1]):
                if node[1] not in free:
                    free.append(node[1])
            elif node[0] == "arrow":
                collect(node[1])
                collect(node[2])
            elif node[0] == "forall":
                raise ValueError("unexpected nested primitive quantifier")

        collect(parsed)
        parsed = ("forall", tuple(free), parsed)

    def normalize(node, scope):
        if node[0] == "forall":
            nested = dict(scope)
            for binder in node[1]:
                nested[binder] = len(nested)
            return ("forall", len(node[1]), normalize(node[2], nested))
        if node[0] == "arrow":
            return ("arrow", normalize(node[1], scope), normalize(node[2], scope))
        name = node[1]
        if name in scope:
            return ("variable", scope[name])
        if name in ("Int", "Prelude.Int"):
            return ("native", "Int")
        raise ValueError("unexpected free type or constructor in primitive scheme: " + name)

    return normalize(parsed, {})


EXPECTED_PRIMITIVE_TYPE = _provider_type(spec.REQUIRED_PROVIDERS["at"]["haskell"][0]["type"])


def observed_inventory(output, engine, operation):
    text = _clean_output(output)
    header = "-- Djinn" if engine == "djinn" else "-- Current source scope"
    matches = list(re.finditer(r"(?m)^" + re.escape(header) + r"\s*$", text))
    if len(matches) != 1:
        raise ValueError("missing or duplicate checked inventory browse")
    tail = text[matches[0].end():]
    boundary = re.search(r"(?m)^backend = ", tail)
    if boundary is None:
        raise ValueError("checked inventory has no settings boundary")
    declarations = [line.strip() for line in tail[:boundary.start()].splitlines() if line.strip()]
    builtins = [line for line in declarations if line in DJINN_BUILTIN_DECLARATIONS]
    if builtins and (engine != "djinn" or len(builtins) != len(DJINN_BUILTIN_DECLARATIONS)
                     or set(builtins) != set(DJINN_BUILTIN_DECLARATIONS)):
        raise ValueError("unexpected, incomplete, or duplicate builtin declaration inventory")
    search_declarations = [line for line in declarations if line not in DJINN_BUILTIN_DECLARATIONS]
    if operation != "at":
        if not ((not builtins and search_declarations == ["(no declarations)"])
                or (builtins and not search_declarations)):
            raise ValueError("providers-off query exposed declarations: " + repr(declarations))
        return {"declarations": declarations, "builtin_declarations": builtins,
                "values": [], "type_aliases": []}
    values, abstracts = {}, []
    for line in search_declarations:
        value = re.fullmatch(r"((?:PartialNumeric\.)?partialIntCase)\s*::\s*(.+)", line)
        if value:
            if values:
                raise ValueError("duplicate generic Int provider")
            actual = _provider_type(value.group(2))
            if actual != EXPECTED_PRIMITIVE_TYPE:
                raise ValueError("checked generic Int scheme differs from the complete required type")
            values["partialIntCase"] = {"displayed_type": value.group(2), "normalized_type": actual}
        elif line == "type Int :: Type":
            if abstracts:
                raise ValueError("duplicate native Int abstract declaration")
            abstracts.append(line)
        else:
            raise ValueError("unexpected declaration in generic Int search inventory: " + line)
    if set(values) != {"partialIntCase"}:
        raise ValueError("generic Int provider inventory is missing")
    if engine == "djinn" and abstracts != ["type Int :: Type"]:
        raise ValueError("Djinn generic Int inventory lacks its checked native Int abstract declaration")
    return {"declarations": declarations, "builtin_declarations": builtins, "values": values,
            "type_aliases": [], "abstract_types": abstracts}


def controls_for(operations):
    rows = []
    for operation in operations:
        rows.append({
            "id": "positive-" + operation, "operation": operation,
            "kind": "positive_oracle_witness", "type": spec.HASKELL_TYPES[operation],
            "definition": "generated = " + spec.HASKELL_WITNESSES[operation],
            "assertion": spec.haskell_predicate(operation, "Oracle.generated"),
            "uses_primitive": False,
        })
        if operation in spec.HASKELL_PROVIDER_WITNESSES:
            rows.append({
                "id": "positive-primitive-" + operation, "operation": operation,
                "kind": "positive_primitive_witness", "type": spec.HASKELL_TYPES[operation],
                "definition": "generated = " + spec.HASKELL_PROVIDER_WITNESSES[operation],
                "assertion": spec.haskell_predicate(operation, "Oracle.generated"),
                "uses_primitive": True,
            })
        for label, term in spec.HASKELL_WRONG[operation].items():
            rows.append({
                "id": "wrong-" + operation + "-" + label, "operation": operation,
                "kind": "fully_typed_wrong_control", "type": spec.HASKELL_TYPES[operation],
                "definition": "generated = " + term,
                "assertion": "Prelude.not (" + spec.haskell_predicate(operation, "Oracle.generated") + ")",
                "separating_fixture": spec.CONTROL_COUNTEREXAMPLES[operation][label],
                "uses_primitive": False,
            })
    if "at" in operations:
        _, assertions = spec.provider_control_source("haskell")
        for name, assertion in assertions:
            rows.append({
                "id": "primitive-" + name, "operation": "at",
                "kind": "generic_primitive_control", "assertion": assertion,
                "uses_primitive": True,
            })
    for row in rows:
        row.update(status="prepared", source_case_id=spec.SOURCE_CASE_IDS[row["operation"]])
    return rows


def runtime_source(assertion, label, evaluation_seconds, imports):
    return "\n".join([
        "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}",
        "module Main (main) where", "import Prelude", "import qualified Prelude",
        *imports, "import qualified Control.Exception as E",
        "import qualified System.Timeout as T", "import qualified System.Exit as Exit",
        "main :: IO ()", "main = do",
        f"  outcome <- T.timeout {int(evaluation_seconds * 1000000)} (E.try (E.evaluate ({assertion})) :: IO (Either E.SomeException Bool))",
        "  case outcome of", '    Just (Right True) -> putStrLn ' + json.dumps("PASS " + label),
        '    _ -> putStrLn (' + json.dumps("FAIL " + label + ": ") + " ++ show outcome) >> Exit.exitFailure", "",
    ])


def replay_sources(row, evaluation_seconds, *, candidate=False):
    if candidate:
        operation, name = row["operation"], row["name"]
        lines = [
            "{-# LANGUAGE NoImplicitPrelude, RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}",
            f"module Candidate ({name}) where",
            "import Prelude (Int)", "import qualified Prelude (Int)",
        ]
        if operation == "at":
            lines += ["import PartialNumeric (partialIntCase)"]
        # No rewriting of the displayed equation: even its annotations and
        # qualified primitive references cross GHC exactly as displayed.
        lines += [name + " :: " + spec.HASKELL_TYPES[operation], row["definition"], ""]
        sources = {
            "Candidate.hs": "\n".join(lines),
            "Main.hs": runtime_source(spec.haskell_predicate(operation, "Candidate." + name),
                                      row["id"], evaluation_seconds, ["import qualified Candidate"]),
        }
        if operation == "at":
            sources[NUMERIC_MODULE + ".hs"] = NUMERIC_SOURCE
        return sources, "Candidate.hs", (
            "NoImplicitPrelude; native Int type only; at additionally imports exactly partialIntCase. No oracle, witness, observation helper, other candidate, or Prelude value imports."
        )
    if row["kind"] == "generic_primitive_control":
        return {
            NUMERIC_MODULE + ".hs": NUMERIC_SOURCE,
            "Main.hs": runtime_source(row["assertion"], row["id"], evaluation_seconds,
                                     ["import PartialNumeric (partialIntCase)"]),
        }, NUMERIC_MODULE + ".hs", "Independent generic primitive module and native assertions; no target/witness loaded."
    oracle = [
        "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}",
        "module Oracle (generated) where", "import Prelude", "import qualified Prelude",
    ]
    if row["uses_primitive"]:
        oracle += ["import PartialNumeric (partialIntCase)"]
    oracle += [*spec.HASKELL_ORACLE_PRELUDE, "generated :: " + row["type"], row["definition"], ""]
    sources = {
        "Oracle.hs": "\n".join(oracle),
        "Main.hs": runtime_source(row["assertion"], row["id"], evaluation_seconds, ["import qualified Oracle"]),
    }
    if row["uses_primitive"]:
        sources[NUMERIC_MODULE + ".hs"] = NUMERIC_SOURCE
    return sources, "Oracle.hs", "Isolated fully typed control with native oracle helpers; this directory is never loaded into synthesis."


def write_replay(directory, row, evaluation_seconds, source_hashes, *, candidate=False):
    directory.mkdir()
    sources, typed_filename, policy = replay_sources(row, evaluation_seconds, candidate=candidate)
    files = []
    for filename, source in sources.items():
        path = (directory / filename).resolve()
        path.write_text(source, encoding="utf-8")
        digest = sha256(path)
        files.append({"path": str(path), "sha256": digest})
        source_hashes[str(path)] = digest
    row["replay"] = {
        "directory": str(directory.resolve()), "sources": files,
        "fully_typed_source": str((directory / typed_filename).resolve()),
        "fully_typed_source_sha256": sha256(directory / typed_filename),
        "candidate_import_policy": policy, "expected_pass_labels": [row["id"]], "status": "prepared",
    }
    if candidate:
        row["replay"].update(
            full_requested_signature=spec.HASKELL_TYPES[row["operation"]],
            original_partial_signature=spec.ORIGINAL_HASKELL_TYPES[row["operation"]],
            default_contract=spec.PARTIAL_CASES[row["source_case_id"]]["default_argument_position"],
            displayed_definition_sha256=hashlib.sha256(row["definition"].encode("utf-8")).hexdigest(),
        )


def persist(output, report, processes=None):
    if processes is not None:
        report["processes"] = processes.rows
    write_json(output / "results.json", report)


def unchanged(hashes):
    try:
        return bool(hashes) and all(Path(path).is_file() and sha256(path) == digest
                                    for path, digest in hashes.items())
    except OSError:
        return False


def finalize(report, source_hashes, executable_hashes, *, environment_empty, interrupted=False):
    report["sources_unchanged"] = unchanged(source_hashes)
    report["executables_unchanged"] = unchanged(executable_hashes)
    report["providers_off_environment_still_empty"] = environment_empty
    integrity = report["sources_unchanged"] and report["executables_unchanged"] and environment_empty
    false_by_engine = {row["engine"]: row for row in report["cells"] if row["kind"] == "false_oracle_query"}
    for row in report["cells"]:
        controls = [control for control in report["oracle_controls"] if control["operation"] == row["operation"]]
        row["oracle_controls_passed"] = (
            bool(controls) and all(control["status"] == "passed" for control in controls)
            if row["kind"] == "synthesis" else True)
        row["engine_false_control_passed"] = false_by_engine[row["engine"]]["status"] == "passed"
        row["accepted"] = (row["status"] == "passed" and row["oracle_controls_passed"]
                           and row["engine_false_control_passed"] and integrity)
    synthesis = [row for row in report["cells"] if row["kind"] == "synthesis"]
    report["passed_synthesis_cells"] = sum(row["accepted"] for row in synthesis)
    report["failed_synthesis_cells"] = [row["id"] for row in synthesis if not row["accepted"]]
    report["passed_false_controls"] = sum(row["status"] == "passed" for row in false_by_engine.values())
    report["passed_oracle_controls"] = sum(row["status"] == "passed" for row in report["oracle_controls"])
    passed = (len(synthesis) == report["expected_synthesis_cells"]
              and len(false_by_engine) == report["expected_false_controls"]
              and all(row["accepted"] for row in report["cells"])
              and all(row["status"] == "passed" for row in report["oracle_controls"]) and integrity)
    report["status"] = "interrupted" if interrupted else "passed" if passed else "failed"
    if not integrity:
        report["integrity_failure"] = "source/executable changed or unavailable, or providers-off environment was populated"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--djex", type=Path)
    parser.add_argument("--ghc", default="ghc")
    parser.add_argument("--output", type=Path, required=True, help="fresh or empty directory")
    parser.add_argument("--engine", action="append", choices=("djinn", "exference"))
    parser.add_argument("--operation", action="append", choices=spec.OPERATIONS)
    parser.add_argument("--djinn-window", type=int, default=65536)
    parser.add_argument("--djinn-budget", type=int, default=500000)
    parser.add_argument("--djinn-strategy", choices=("depth-first", "interleave"), default="interleave")
    parser.add_argument("--exference-window", type=int, default=256)
    parser.add_argument("--exference-budget", type=int, default=100000)
    parser.add_argument("--steps", type=int, default=100000)
    parser.add_argument("--queue", type=int, default=8192)
    parser.add_argument("--process-timeout", type=float, default=300)
    parser.add_argument("--evaluation-timeout", type=float, default=2)
    parser.add_argument("--observe-latency", action="store_true")
    parser.add_argument("--prepare-only", action="store_true")
    args = parser.parse_args()
    engines = args.engine or ["djinn", "exference"]
    operations = args.operation or list(spec.OPERATIONS)
    if len(set(engines)) != len(engines) or len(set(operations)) != len(operations):
        parser.error("duplicate selections do not constitute coverage")
    for engine in engines:
        validate_limits(profile(args, engine))
    if args.queue <= 0:
        parser.error("queue must be positive")
    if not math.isfinite(args.evaluation_timeout) or not 0 < args.evaluation_timeout <= 60:
        parser.error("evaluation-timeout must be in (0,60] seconds")
    if not args.prepare_only and args.djex is None:
        parser.error("--djex is required unless --prepare-only")
    provenance = spec.source_provenance()
    output = prepare_output_directory(args.output).resolve()
    empty = output / "empty-environment"
    empty.mkdir()
    fixture_directory = output / "numeric-fixture"
    fixture_directory.mkdir()
    numeric_path = fixture_directory / (NUMERIC_MODULE + ".hs")
    numeric_path.write_text(NUMERIC_SOURCE, encoding="utf-8")
    dependencies = [
        Path(__file__), HERE / "behavior_partial_spec.py", HERE / "behavior_extended_probe.py",
        HERE / "behavior_extended_spec.py", HERE / "behavior_probe.py", HERE / "behavior_runtime.py",
        HERE / "behavior_spec.py", HERE / "manifest.json", ROOT / provenance["source"], numeric_path,
    ]
    source_hashes = {str(path.resolve()): sha256(path) for path in dependencies}
    report = {
        "status": "prepared", "validation_mode": "prepared_only", "accepted": False,
        "provenance": provenance, "engines": engines, "operations": operations,
        "settings": {engine: vars(profile(args, engine)) for engine in engines},
        "independent_evaluation_seconds": args.evaluation_timeout,
        "latency_observations_enabled": args.observe_latency,
        "expected_synthesis_cells": len(engines) * len(operations),
        "expected_false_controls": len(engines),
        "source_hashes": source_hashes, "cells": [], "oracle_controls": [], "processes": [],
        "scope": "Haskell only; all selected original partial signatures use their explicit manifest-documented supplied default. No original partial signature or Lean case is promoted to acceptance.",
        "numeric_fixture": {
            "path": str(numeric_path), "sha256": sha256(numeric_path),
            "loaded_for_operations": ["at"] if "at" in operations else [],
            "native_type": "Int (GHC implicit Prelude; checked unqualified external source type)",
            "value_inventory": spec.REQUIRED_PROVIDERS["at"]["haskell"],
            "native_Int_semantics": provenance["int_scope"],
        },
    }
    plans = []
    for engine in engines:
        for operation in [*operations, None]:
            source, cell = commands(engine, operation, profile(args, engine), numeric_path)
            command_path = output / (cell["id"] + ".commands.txt")
            command_path.write_text(source, encoding="utf-8")
            digest = sha256(command_path)
            cell.update(command_path=str(command_path), command_sha256=digest)
            source_hashes[str(command_path)] = digest
            report["cells"].append(cell)
            plans.append((cell, source))
    for control in controls_for(operations):
        write_replay(output / ("control-" + control["id"]), control, args.evaluation_timeout, source_hashes)
        report["oracle_controls"].append(control)
    report["expected_oracle_controls"] = len(report["oracle_controls"])
    persist(output, report)
    if args.prepare_only:
        print(f"Prepared {report['expected_synthesis_cells']} defaulted Haskell synthesis cells, "
              f"{report['expected_false_controls']} false controls, and "
              f"{report['expected_oracle_controls']} isolated controls; no processes run.")
        return 0

    processes = Processes(output, args.process_timeout)
    executable_hashes = {}
    interrupted = False
    try:
        djex = resolve_executable(args.djex, "Djex")
        ghc = resolve_executable(args.ghc, "independent GHC")
        live_ghc = resolve_executable("ghc", "behavioral worker's PATH-resolved GHC")
        executable_hashes = {str(path): sha256(path) for path in (djex, ghc, live_ghc)}
        report.update(status="running", validation_mode="live_synthesis_and_isolated_exact_source_ghc_execution",
                      executable_hashes=executable_hashes, djex_path=str(djex),
                      independent_ghc_path=str(ghc), live_path_ghc=str(live_ghc))
        persist(output, report, processes)
        for control in report["oracle_controls"]:
            control["status"] = "running"
            persist(output, report, processes)
            try:
                passed = execute_replay(processes, ghc, control, "control-" + control["id"])
                control["status"] = "passed" if passed else "failed"
                if not passed:
                    control["failure"] = control["replay"].get("failure", "independent oracle replay failed")
            except Exception as failure:
                control.update(status="failed", failure=str(failure))
            persist(output, report, processes)
        for cell, source in plans:
            cell["status"] = "running"
            persist(output, report, processes)
            observer = latency_observer([cell]) if args.observe_latency else None
            try:
                if any(empty.iterdir()):
                    raise ValueError("providers-off environment is no longer empty")
                if sha256(numeric_path) != report["numeric_fixture"]["sha256"]:
                    raise ValueError("generic Int source fixture changed")
                if not unchanged(executable_hashes):
                    raise ValueError("a pinned executable changed before synthesis")
                live = processes.run(cell["id"] + "-synthesis",
                    [djex, "repl", "--ignore-startup", "--environment", empty],
                    source=source, cwd=ROOT, observe=observer)
                cell["synthesis_exit_code"] = live.returncode
                if live.returncode:
                    raise ValueError("synthesis process exited unsuccessfully")
                cell["validated_settings"] = validate_cell_settings(
                    live.stdout + live.stderr, cell["engine"], cell["operation"], profile(args, cell["engine"]))
                if re.findall(r"(?m)^max-queue = (.*)$", _clean_output(live.stdout)) != [str(args.queue)]:
                    raise ValueError("requested queue bound was not acknowledged exactly once")
                cell["validated_settings"]["max-queue"] = str(args.queue)
                cell["observed_inventory"] = observed_inventory(live.stdout, cell["engine"], cell["operation"])
                is_false = cell["kind"] == "false_oracle_query"
                cell["behavioral_observations"] = behavioral_observations(
                    live.stdout + live.stderr, expect_success=not is_false, window=cell["settings"]["window"])
                if is_false:
                    if re.search(r"(?m)^" + re.escape(cell["name"]) + r"(?:\s|=)[^\n]*=", _clean_output(live.stdout)):
                        raise ValueError("where False displayed a successful definition")
                    if observer is not None:
                        observer.validate_counts({cell["name"]: 0})
                    cell["status"] = "passed"
                else:
                    cell["definition"] = displayed_definition(live.stdout, cell["name"])
                    cell["definition_sha256"] = hashlib.sha256(cell["definition"].encode("utf-8")).hexdigest()
                    if observer is not None:
                        observer.validate_counts({cell["name"]: 1})
                    cell["status"] = "synthesized"
                    write_replay(output / ("candidate-" + cell["id"]), cell,
                                 args.evaluation_timeout, source_hashes, candidate=True)
                    persist(output, report, processes)
                    passed = execute_replay(processes, ghc, cell, "candidate-" + cell["id"])
                    cell["status"] = "passed" if passed else "failed"
                    if not passed:
                        cell["failure"] = cell["replay"].get("failure", "independent displayed-source replay failed")
            except Exception as failure:
                cell.update(status="failed", failure=str(failure))
            finally:
                if observer is not None:
                    cell["latency_observations"] = observer.receipt()
            persist(output, report, processes)
    except KeyboardInterrupt:
        interrupted = True
        report["failure"] = "interrupted; owned process cleanup and earlier captures retained"
    except Exception as failure:
        report["failure"] = str(failure)
    finally:
        for row in [*report["cells"], *report["oracle_controls"]]:
            if row["status"] in ("prepared", "running", "synthesized"):
                row.update(status="not_run", reason=report.get("failure", "run did not complete this cell"))
        finalize(report, source_hashes, executable_hashes,
                 environment_empty=empty.is_dir() and not any(empty.iterdir()), interrupted=interrupted)
        report["accepted"] = report["status"] == "passed"
        persist(output, report, processes)
    print(f"Supplied-default Haskell corpus: {report['status']}; "
          f"{report['passed_synthesis_cells']}/{report['expected_synthesis_cells']} accepted synthesis cells.")
    return 130 if interrupted else 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    for stream in (sys.stdout, sys.stderr):
        if hasattr(stream, "reconfigure"):
            stream.reconfigure(encoding="utf-8")
    raise SystemExit(main())
