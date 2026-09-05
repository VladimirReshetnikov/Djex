#!/usr/bin/env python3
"""Exercise ranking CLI/REPL options on a built executable, without rebuilding.

All successful one-shot outputs are replayed by GHC. Invalid options must fail
before synthesis; rejected REPL settings must preserve the last valid state.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]
POLICIES = ("legacy", "balanced", "compact", "diverse")


def emitted_definition(output, name):
    # The initial default prompt has already been printed when `:set prompt
    # ""` takes effect. It can therefore prefix the first actual output line.
    # Accept only that exact prompt shape, not arbitrary text before a name.
    matches = re.findall(r"(?m)^(?:djex\[(?:djinn|exference|both)\]> )?"
                         + re.escape(name) + r"\s*=\s*(.+)$", output)
    if len(matches) != 1:
        raise ValueError(f"missing or duplicate provider output: {name}")
    return matches[0]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--djex", required=True, type=Path)
    parser.add_argument("--output", type=Path, default=ROOT / "test-church/results/quality-cli")
    parser.add_argument("--process-timeout", type=float, default=60,
                        help="wall-clock guard for each CLI or compiler process (seconds)")
    args = parser.parse_args()
    if not math.isfinite(args.process_timeout) or args.process_timeout <= 0:
        parser.error("--process-timeout must be finite and positive")
    args.output.mkdir(parents=True, exist_ok=True)
    environment = args.output / "empty-environment"
    environment.mkdir(exist_ok=True)
    digest = hashlib.sha256(args.djex.read_bytes()).hexdigest()
    records = []

    def run(label, arguments, source=""):
        started = time.monotonic()
        try:
            process = subprocess.run([str(args.djex.resolve()), *arguments], input=source,
                                     capture_output=True, text=True, encoding="utf-8", cwd=ROOT,
                                     timeout=args.process_timeout)
        except subprocess.TimeoutExpired as failure:
            # subprocess.run terminates and waits for its owned process. Keep
            # partial output as diagnostics, never as a compiler acceptance.
            for suffix, output in [("stdout", failure.stdout), ("stderr", failure.stderr)]:
                if isinstance(output, bytes):
                    output = output.decode("utf-8", errors="replace")
                (args.output / (label + "." + suffix + ".txt")).write_text(
                    output or "", encoding="utf-8")
            records.append({"label": label, "arguments": arguments, "timeout": True,
                            "wall_seconds": time.monotonic() - started})
            (args.output / "processes.json").write_text(json.dumps(records, indent=2) + "\n", encoding="utf-8")
            raise
        (args.output / (label + ".stdout.txt")).write_text(process.stdout, encoding="utf-8")
        (args.output / (label + ".stderr.txt")).write_text(process.stderr, encoding="utf-8")
        records.append({"label": label, "arguments": arguments, "exit_code": process.returncode,
                        "wall_seconds": time.monotonic() - started})
        (args.output / "processes.json").write_text(json.dumps(records, indent=2) + "\n", encoding="utf-8")
        return process

    declarations = ["{-# LANGUAGE RankNTypes #-}", "module QualityCli where"]
    for engine in ("djinn", "exference"):
        for policy in POLICIES:
            limits = (["--choice-budget", "10000", "--candidate-limit", "12"]
                      if engine == "djinn" else
                      ["--max-steps", "10000", "--allow-unused", "--environment", str(environment.resolve())])
            name = engine + "_" + policy
            process = run(name, [engine, "--ranking", policy, "--quality-window", "12",
                                "--provider-cost", "unused=7", "--render", "expression",
                                "--select", "first", *limits, "a -> a -> a"])
            if process.returncode != 0 or not process.stdout.strip():
                raise ValueError(f"valid policy failed: {name}")
            declarations.extend([name + " :: forall a. a -> a -> a", name + " = " + process.stdout.strip()])
        invalid = [(["--ranking", "unknown"], "expected legacy, balanced, compact, or diverse"),
                   (["--quality-window", "0"], "positive integer"),
                   (["--quality-window", "-1"], "positive integer"),
                   (["--provider-cost", "x=-1"], "non-negative"),
                   (["--provider-cost", "x=1", "--provider-cost", "x=2"], "each exact name only once")]
        for index, (options, diagnostic) in enumerate(invalid):
            process = run(f"{engine}_invalid{index}", [engine, *options, "a -> a"])
            if process.returncode == 0 or process.stdout.strip() or diagnostic not in process.stderr:
                raise ValueError(f"invalid option was not rejected precisely: {engine} {options}")
    lines = [':set prompt ""', ":show settings"]
    for policy in POLICIES:
        lines += [":set ranking " + policy, ":show settings"]
    lines += [":set quality-window 7", ":set provider-cost chosen=13", ":show settings",
              ":set ranking unknown", ":set quality-window 0", ":set provider-cost chosen=-1",
              ":show settings", ":unset ranking", ":unset quality-window", ":unset provider-cost",
              ":show settings", ":quit"]
    source = "\n".join(lines) + "\n"
    (args.output / "repl.txt").write_text(source, encoding="utf-8")
    process = run("repl", ["repl", "--ignore-startup", "--environment", str(environment.resolve())], source)
    rankings = re.findall(r"(?m)^ranking = (.*)$", process.stdout)
    windows = re.findall(r"(?m)^quality-window = (.*)$", process.stdout)
    costs = re.findall(r"(?m)^provider-cost = *(.*)$", process.stdout)
    if (process.returncode != 0 or rankings != ["balanced", *POLICIES, "diverse", "diverse", "balanced"]
            or windows != ["60"] * 5 + ["7", "7", "60"]
            or costs != [""] * 5 + ["chosen=13", "chosen=13", ""]
            or process.stderr.count("[DJEX_REPL_SETTING]") != 3):
        raise ValueError("REPL ranking/cost/window state did not round-trip or reject invalid input atomically")
    # Both engines see the same canonical source declarations. Djinn renames
    # their providers to prompt names, so qualified costs must be projected at
    # each query rather than copied once into a stale backend-specific map.
    provider_environment = args.output / "provider-environment"
    provider_environment.mkdir(exist_ok=True)
    (provider_environment / "CostProviders.hs").write_text("\n".join([
        "module CostProviders (Token, cheap, expensive) where",
        "data Token = Token", "cheap :: () -> Token", "cheap _ = Token",
        "expensive :: Token", "expensive = Token", ""]), encoding="utf-8")
    (provider_environment / "OtherProviders.hs").write_text("\n".join([
        "module OtherProviders (Token, cheap, expensive) where",
        "import CostProviders (Token)", "import qualified CostProviders as C",
        "cheap :: () -> Token", "cheap = C.cheap",
        "expensive :: Token", "expensive = C.expensive", ""]), encoding="utf-8")
    provider_lines = [':set prompt ""', ":set ranking balanced", ":set select first",
                      ":set render definition", ":set djinn-axioms on", ":set allow-unused on",
                      ":set candidate-limit 12", ":set quality-window 12",
                      ":set choice-budget 10000", ":set max-steps 10000",
                      ":module", "import CostProviders",
                      ":set provider-cost CostProviders.cheap=0",
                      ":set provider-cost CostProviders.expensive=40"]
    provider_cases = []
    for stage, scope, preferred in [(0, "CostProviders", "cheap"),
                                     (1, "CostProviders", "expensive"),
                                     (2, "OtherProviders", "cheap")]:
        if stage == 1:
            provider_lines += [":set provider-cost CostProviders.cheap=40",
                               ":set provider-cost CostProviders.expensive=0", ":reload",
                               # Reload restores the automatic starred target
                               # context. Re-establish the export-only scope so
                               # the hidden Token constructor is not a candidate.
                               ":module", "import CostProviders"]
        if stage == 2:
            provider_lines += [":module", "import OtherProviders",
                               ":set provider-cost OtherProviders.cheap=0",
                               ":set provider-cost OtherProviders.expensive=40"]
        for engine in ("djinn", "exference"):
            name = f"provider{stage}_{engine}"
            provider_cases.append((name, scope, preferred))
            provider_lines += [":set target " + name, ":" + engine + " Token"]
    provider_source = "\n".join(provider_lines + [":quit", ""])
    (args.output / "provider-repl.txt").write_text(provider_source, encoding="utf-8")
    process = run("provider-repl", ["repl", "--ignore-startup", "--environment",
                                    str(provider_environment.resolve())], provider_source)
    if process.returncode != 0 or "[DJEX_REPL_SETTING]" in process.stderr:
        raise ValueError("qualified-provider REPL setup failed")
    provider_replays = []
    for name, scope, preferred in provider_cases:
        expression = emitted_definition(process.stdout, name)
        avoided = "expensive" if preferred == "cheap" else "cheap"
        if not re.search(r"\b" + preferred + r"\b", expression) or re.search(r"\b" + avoided + r"\b", expression):
            raise ValueError(f"qualified costs did not select {preferred} for {name}: {expression}")
        replay_path = args.output / (name + ".hs")
        replay_path.write_text("\n".join([
            "module " + name[0].upper() + name[1:] + " where",
            "import " + scope, "import qualified CostProviders", "import qualified OtherProviders",
            name + " :: CostProviders.Token", name + " = " + expression, ""]), encoding="utf-8")
        provider_replays.append(replay_path)
    path = args.output / "QualityCli.hs"
    path.write_text("\n".join(declarations) + "\n", encoding="utf-8")
    compiler = subprocess.run(["ghc", "-v0", "-fno-code", "-fno-write-interface", "-fforce-recomp",
                               "-i" + str(provider_environment.resolve()), str(path),
                               *[str(replay) for replay in provider_replays]],
                              capture_output=True, text=True, encoding="utf-8", cwd=ROOT,
                              timeout=args.process_timeout)
    (args.output / "ghc.txt").write_text(compiler.stdout + compiler.stderr, encoding="utf-8")
    stable = hashlib.sha256(args.djex.read_bytes()).hexdigest() == digest
    passed = compiler.returncode == 0 and stable
    (args.output / "results.json").write_text(json.dumps({
        "executable_sha256": digest, "executable_unchanged": stable,
        "runs": records, "ghc_exit_code": compiler.returncode, "passed": passed,
        "repl_rankings": rankings, "repl_windows": windows, "repl_provider_costs": costs,
    }, indent=2) + "\n", encoding="utf-8")
    print(f"{'PASS' if passed else 'FAIL'}: 14 compiler-replayed outputs, 10 option rejections, REPL round-trip/reload/scope")
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
