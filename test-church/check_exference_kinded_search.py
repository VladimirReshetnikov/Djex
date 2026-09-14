"""Build and check the affected Exference kinded-search integration suites.

Run public_kinded_source.py separately for each engine to establish public
acceptance with exact GHC replay. This gate captures source and runtime
identities, complete suite inventories, and process transcripts.
"""
from pathlib import Path
import argparse
import os
import re
import subprocess
import sys

import behavior_runtime as rt


SUITES = (
    "exference-engine-tests", "exference-tests", "djinn-tests",
    "djex-tests", "djex-api-tests", "synthesis-tests", "synthesis-certificate-tests",
)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--timeout", type=float, default=2400)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    out = rt.prepare_output_directory(args.out.resolve())
    paths = subprocess.check_output(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
        cwd=root).decode().split("\0")
    sources = sorted({p for p in paths if p and (root / p).is_file()
                      and Path(p).suffix in (".hs", ".cabal", ".py")})
    hashes = {p: rt.sha256(root / p) for p in sources}
    for path in sources:
        destination = out / "source" / path
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes((root / path).read_bytes())
    report = dict(status="running", scope=__doc__, source_sha256=hashes,
                  runtime_sha256={}, suites=[],
                  base_commit=subprocess.check_output(
                      ["git", "rev-parse", "HEAD"], cwd=root, text=True).strip())
    runner = rt.Processes(out, args.timeout)
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1", djex_datadir=str(root))

    def save():
        report["processes"] = runner.rows
        rt.write_json(out / "results.json", report)

    try:
        save()
        build = runner.run("strict-build", ["cabal", "build", "djex:exe:djex",
            *(f"djex:test:{name}" for name in SUITES), "--ghc-options=-Werror", "-j1"],
            cwd=root, env=env)
        if build.returncode:
            raise RuntimeError("strict build failed")
        executable = Path(subprocess.check_output(
            ["cabal", "list-bin", "djex:exe:djex"], cwd=root, text=True).strip())
        report["runtime_sha256"][str(executable)] = rt.sha256(executable)
        for name in SUITES:
            executable = Path(subprocess.check_output(
                ["cabal", "list-bin", f"djex:test:{name}"], cwd=root, text=True).strip())
            report["runtime_sha256"][str(executable)] = rt.sha256(executable)
            inventory = runner.run(name + "-inventory", [str(executable), "--list-tests"],
                                   cwd=root, env=env)
            count = len(inventory.stdout.splitlines())
            if inventory.returncode or not count:
                raise RuntimeError(name + " has no valid test inventory")
            row = dict(name=name, count=count, passed=False)
            report["suites"].append(row)
            save()
            tested = runner.run(name, [str(executable), "--num-threads", "1"], cwd=root, env=env)
            row["passed"] = tested.returncode == 0 and re.findall(
                r"All (\d+) tests passed", tested.stdout) == [str(count)]
            save()
            if not row["passed"]:
                raise RuntimeError(name + " failed its complete test inventory")
        report["status"] = "passed"
    except BaseException as failure:
        report.update(status="failed", failure=repr(failure))
    finally:
        report["sources_unchanged"] = all(rt.sha256(root / p) == h for p, h in hashes.items())
        report["runtimes_unchanged"] = all(rt.sha256(Path(p)) == h
                                         for p, h in report["runtime_sha256"].items())
        if not report["sources_unchanged"] or not report["runtimes_unchanged"]:
            report["status"] = "failed"
        save()
    print(report["status"], report.get("failure", ""), flush=True)
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    sys.exit(main())
