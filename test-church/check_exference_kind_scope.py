"""Reproduce the Exference lexical-kind checker prerequisite and regression gates.

This checks internal kind transport and independent expression checking.
It does not claim public kinded synthesis acceptance.
"""
from pathlib import Path
import argparse
import os
import subprocess
import sys

import behavior_runtime as rt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--timeout", type=float, default=2400)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    out = rt.prepare_output_directory(args.out.resolve())
    sources = [
        Path(__file__).resolve(), root / "test-church/behavior_runtime.py",
        root / "test-church/behavior_spec.py", root / "djex.cabal",
        root / "exference/src-core/Language/Haskell/Exference/Core/Internal/KindScope.hs",
        root / "exference/src-core/Language/Haskell/Exference/Core/Internal/Polytype.hs",
        root / "exference/src-core/Language/Haskell/Exference/Core/Internal/ExpressionCheck.hs",
        root / "exference/test-engine/KindScopeSpec.hs",
        root / "exference/test-engine/EngineSpec.hs",
    ]
    source_hashes = {str(path): rt.sha256(path) for path in sources}
    for path in sources:
        destination = out / "source" / path.relative_to(root)
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(path.read_bytes())
    report = {
        "status": "running", "scope": __doc__,
        "source_hashes": source_hashes, "executable_hashes": {},
        "base_commit": subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=root, text=True).strip(),
    }
    runner = rt.Processes(out, args.timeout)
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1", djex_datadir=str(root))
    suites = [("exference-engine-tests", 120), ("exference-tests", 515)]

    def save():
        report["processes"] = runner.rows
        rt.write_json(out / "results.json", report)

    try:
        save()
        result = runner.run("strict-build",
            ["cabal", "build", *(f"djex:test:{name}" for name, _ in suites),
             "--ghc-options=-Werror", "-j1"], cwd=root, env=env)
        if result.returncode:
            raise RuntimeError("strict build failed")
        for name, count in suites:
            executable = Path(subprocess.check_output(
                ["cabal", "list-bin", f"djex:test:{name}"], cwd=root, text=True).strip())
            report["executable_hashes"][str(executable)] = rt.sha256(executable)
            save()
            result = runner.run(name, [str(executable), "--num-threads", "1"],
                                cwd=root, env=env)
            if result.returncode or f"All {count} tests passed" not in result.stdout:
                raise RuntimeError(f"{name} did not pass its complete {count}-test gate")
        report["status"] = "passed"
    except BaseException as error:
        report.update(status="failed", failure=repr(error))
    finally:
        report["sources_unchanged"] = all(
            rt.sha256(Path(path)) == expected for path, expected in source_hashes.items())
        report["runtimes_unchanged"] = all(
            rt.sha256(Path(path)) == expected
            for path, expected in report["executable_hashes"].items())
        if not report["sources_unchanged"] or not report["runtimes_unchanged"]:
            report["status"] = "failed"
        save()
    print(report["status"], report.get("failure", ""), flush=True)
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    sys.exit(main())
