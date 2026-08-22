#!/usr/bin/env python3
"""Fail-closed end-to-end screen for the SelectBest candidate pipeline.

The runner does not build either revision.  It accepts two already-built,
explicitly hashed executables from clean worktrees at the frozen commits.  A
single invocation performs trace preflights, warmups, the unreplaced Williams
screen, semantic checks, metric gates, and an atomic HOLD/KEEP decision.
"""

from __future__ import annotations

import argparse
import ast
import csv
import hashlib
import json
import math
import os
import platform
import re
import shutil
import signal
import statistics
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Sequence


SCHEMA = "djex-select-best-candidate-pipeline-screen/v1"
ROW_SCHEMA = "djex-select-best-candidate-pipeline-row/v1"
BASELINE_COMMIT = "0716144502a7dcd8bfa8755f57fd9ced58bf3b83"
CANDIDATE_COMMIT = "aff7a5e8d0fe81f50b05c5073ff05f77e1ab68ca"
BASELINE_BINARY_SHA256 = "9d8bf7d37ee13e7933bfef61cb44b85fee0fe4807f44cb1e58b29baa4d9316b0"
CANDIDATE_BINARY_SHA256 = "37b7e3c25cdba445244c7253fc9c78d9007c77c994d6858ab7842c612aca1dac"
BASELINE_BUILD_ID = "8bb56dc58402bba8b692ca0c4a8b52264a58034f"
CANDIDATE_BUILD_ID = "bfec9cf7e0bb7099c692a37894c04677427ba0ef"
BASELINE_PLAN_SHA256 = "8d98e5dfaea42e20b76667bd50e1d63fe629beac29bb9b017e5cbf90da20ef2a"
CANDIDATE_PLAN_SHA256 = "5aade84865cb49c05eec811718be5d2d2e7aa1c3d9922fd106c78984246a49d6"
NORMALIZED_PLAN_SHA256 = "586af7ef87a6d144ef2fd6deadabbf215be03aeb989180696cf58dbe3f77da68"
NORMALIZED_PLAN_SIZE = 72_323
NORMALIZED_PLAN_ROOT = "<WORKTREE>"
FROZEN_SOURCE_IDENTITIES = {
    BASELINE_COMMIT: {
        "tree": "7f7a068b1a50d4ebe28a6567cdc91121f3d4e825",
        "archive_sha256": "68cc2830b0dc87f109d03bf4c60b0722b5a3ea32cb2f8a3361e78a73048e42e9",
    },
    CANDIDATE_COMMIT: {
        "tree": "c87265e59f4b2b2fd9814a7dd76e94c56b28ee4e",
        "archive_sha256": "3188206125975e224660994aeee5c1d45cec40eaf53bcdee8509ef58f43d68c9",
    },
}
PINNED_Z3 = Path("/tmp/djex-z3-4.8.12.5tlMuM/root/usr/bin/z3")
PINNED_Z3_SHA256 = (
    "e555c27efbbbdd63b6cb6d54abb4a7aeabacba8184593bb917c4a7c16cb6056c"
)
PINNED_Z3_DEB = Path(
    "/tmp/djex-z3-4.8.12.5tlMuM/z3_4.8.12-1_amd64.deb"
)
PINNED_Z3_DEB_SHA256 = "6742d8addd8a39df4d48945ef2323179966595d4c9249f4e92f44d83ad3a2ab3"
PINNED_Z3_LIBRARY_SHA256 = {
    "8d06f393f4a93bcf9b81145a259524d66a95522a646bf8d7e05b6ffdf2e63dcc",
    "ff0825e113603c3866680d5d52216bc6d8eedf3a59f52a0aef67ff01994db128",
    "df621c68dbfed7e843434ef2faedb9f4d4b0543ad161e9a55eaf4d4ce2443176",
    "fc9d43b2f6c20e53b009238f767c5b949d202389e20de9e202ea684b4ba3729a",
    "e01b1ce7be2987f3b8560e26d0df2623f9dd5cec17be923ae28a785bc0d32d50",
}
PINNED_DJEX_LIBRARY_SHA256 = {
    "8d06f393f4a93bcf9b81145a259524d66a95522a646bf8d7e05b6ffdf2e63dcc",
    "df621c68dbfed7e843434ef2faedb9f4d4b0543ad161e9a55eaf4d4ce2443176",
    "e01b1ce7be2987f3b8560e26d0df2623f9dd5cec17be923ae28a785bc0d32d50",
    "4dc20a901c6951e678e216e959da2534bcef7053e6efdf1492509baf142282b0",
    "bcd7edc10f0e3df476c0738280cd60c751c11b5b99fa13ded70f5439cb3a8ce9",
}
PINNED_TOOLS = {
    "/usr/bin/strace": "38a5c75cb29dd85ddd7780d54f5bf595554d7a1b5c42524b23065f5dc4c4b01d",
    "/usr/bin/git": "587ef21868c948b883993e23209b86a72a6ddc06aab1545c697ffc31075acd4a",
    "/usr/bin/readelf": "04db0000749aff89e4af21429340b00b536fc6f80e811c872c006507881a5560",
    "/usr/bin/ldd": "f6aeb1a1d2ba1463dec8a3760194dd680a5126db4d6449f0f6e5ec4daee35ad2",
    "/usr/bin/dpkg-deb": "25ecb9e986bbbf6184f13617c2a31d8afc556d8b0fd5b764cd1eef506f9c9e73",
}
PINNED_GHC_SHA256 = "8dc33cbd62c93a53174c281ede19b82851b8156f89b9c2bbccb4a39922789193"
PINNED_GHC_COMPILER_SHA256 = "29b0a853efd81eeed37f5d5ffe8add38f3bd0daa87a8838deaf088ec458e99fc"
PINNED_CABAL_SHA256 = "ebee54be14bd81783d37ef6e6b9a5f99c263d35e0c1f0bdb74d0969730fae69f"
PINNED_GHC_VERSION = "9.12.4"
PINNED_CABAL_VERSION = "3.16.1.0"
PINNED_PYTHON = Path("/usr/bin/python3.10")
PINNED_PYTHON_SHA256 = "7d51cd6b48b521277f5caa4610a82126e315fa2be4df069823a8b1eeb5bd4a86"
PINNED_PYTHON_VERSION = "3.10.12"
PINNED_Z3_PACKAGE = ("z3", "4.8.12-1", "amd64")
O2_ATTESTATION = Path(
    "/tmp/djex-pipeline-screen-root-20260822.QUs8ub/o2-attestation-manifest.json"
)
O2_ATTESTATION_SHA256 = "158931c7fcc6dd3c40943b959be11479baae17a7a01df90dd3783e9fd8cc426d"
O2_LOG_SHA256 = {
    "baseline": "02c851d967a57ec9e82aa340415139e3d8e1a79b2eba41df74943ec7971eeb25",
    "candidate": "bdcee1ccc50f6e99e2e69b62f6f0cc522562434aaf773617ccaaaed2f4e92131",
}
SAMPLER_MIN_WALL_COVERAGE = 0.98
SAMPLER_MAX_MEAN_INTERVAL_MULTIPLIER = 3.0
SAMPLER_MAX_INTERVAL_MULTIPLIER = 20.0
SAMPLER_MAX_PASS_MULTIPLIER = 20.0
# Filled only after the workload/schema files are otherwise frozen.  Unlike
# benchmark.py itself, these artifacts can safely carry non-self-referential
# preregistered hashes in the runner.
FROZEN_ARTIFACT_SHA256 = {
    "result-schema.tsv": "b92d017dac8fb5bbeb4a008a7e9701fa47fb17adea666256a5bd996ca66f8a30",
    "workloads/w1-scalar.repl.in": "7896c035e06dc72527bc05e63abe13507126337b0fad63ac14bd95f9dc837384",
    "workloads/w2-product.repl.in": "4be4e139dbf4e1b34f94b7c2c8740dc2dca19e5ba9ca4f5b736c536f78c17f50",
}
NONCE_RE = re.compile(rb"djex-smtlib-frame/v1/[0-9a-f]{64}")
ECHO_COMMAND_RE = re.compile(
    rb'\(echo "(djex-smtlib-frame/v1/[0-9a-f]{64})"\)\n'
)
ECHO_RESPONSE_RE = re.compile(
    rb'"(djex-smtlib-frame/v1/[0-9a-f]{64})"\r?\n'
)
DIAGNOSTIC_RE = re.compile(rb"\[([A-Z0-9_]+)\]")
OR_SEPARATOR = b"\n\n-- or\n\n"
RESULT_COLUMNS = [
    "schema", "run_id", "phase", "workload", "sample", "williams_row",
    "position", "cell", "revision", "commit", "jobs", "capabilities",
    "input_sha256", "wall_ns", "cpu_ns", "peak_rss_bytes",
    "allocated_bytes", "exit_code", "stdout_sha256", "stderr_sha256",
    "transcript_sha256", "semantic_sha256", "stats_sha256",
    "rendered_candidates", "truncation_count", "solver_sessions_observed",
    "solver_queries_observed", "solver_accepted_observed",
    "solver_rejected_observed", "solver_image_sha256",
    "query_vector_sha256", "sampler_samples", "sampler_span_ns",
    "sampler_initial_delay_ns", "sampler_terminal_gap_ns",
    "sampler_coverage_ratio", "sampler_mean_interval_ns",
    "sampler_max_interval_ns", "sampler_max_pass_ns", "process_tree_sha256",
    "cleanup_ok", "artifact_dir",
]


class HarnessFailure(RuntimeError):
    """A fail-closed provenance, semantic, topology, or cleanup failure."""


class TerminationCoordinator:
    def __init__(self) -> None:
        self.requests: list[str] = []
        self.screen: Any | None = None
        self.finalization_active = False
        self.outcome_commit_monotonic_ns: int | None = None

    def bind_screen(self, screen: Any) -> None:
        self.screen = screen

    def record(self, signum: int, _frame: Any = None) -> None:
        self.requests.append(signal.Signals(signum).name)
        if self.screen is not None and self.screen.run_active:
            self.screen.request_termination(signum)

    def publish_finalization(self, publication_hook: Any | None = None) -> None:
        self.finalization_active = True
        if publication_hook is not None:
            publication_hook()

    def block_for_outcome_commit(
        self,
        handled_signals: Sequence[signal.Signals],
        publication_hook: Any | None = None,
        *,
        restore_for_test: bool = False,
    ) -> list[str]:
        require(
            hasattr(signal, "pthread_sigmask") and hasattr(signal, "sigpending"),
            "Linux pthread signal masking is required for outcome commit",
        )
        previous_mask = signal.pthread_sigmask(signal.SIG_BLOCK, handled_signals)
        self.outcome_commit_monotonic_ns = time.monotonic_ns()
        try:
            # Let any CPython-level handler already queued before the mask run;
            # newly arriving OS signals remain pending behind the mask.
            time.sleep(0)
            if publication_hook is not None:
                publication_hook()
            pending = signal.sigpending()
            for value in handled_signals:
                if value in pending:
                    self.requests.append(f"{value.name}:pending-at-outcome-commit")
            return list(self.requests)
        finally:
            if restore_for_test:
                signal.pthread_sigmask(signal.SIG_SETMASK, previous_mask)


def apply_termination_veto(
    decision: dict[str, Any], requests: Sequence[str],
) -> dict[str, Any]:
    result = dict(decision)
    result["termination_requests"] = list(requests)
    result["termination_veto"] = bool(requests)
    if requests:
        result["verdict"] = "HOLD"
        result.setdefault(
            "primary_failure",
            f"catchable termination requested: {list(requests)}",
        )
        if "meaningful_over_1_10" in result:
            result["meaningful_over_1_10"] = False
    return result


@dataclass(frozen=True)
class Cell:
    name: str
    revision: str
    commit: str
    jobs: int
    capabilities: int


CELLS = {
    "A": Cell("A", "baseline", BASELINE_COMMIT, 1, 1),
    "B": Cell("B", "baseline", BASELINE_COMMIT, 1, 2),
    "C": Cell("C", "baseline", BASELINE_COMMIT, 2, 1),
    "D": Cell("D", "baseline", BASELINE_COMMIT, 2, 2),
    "E": Cell("E", "candidate", CANDIDATE_COMMIT, 1, 1),
    "F": Cell("F", "candidate", CANDIDATE_COMMIT, 1, 2),
    "G": Cell("G", "candidate", CANDIDATE_COMMIT, 2, 1),
    "H": Cell("H", "candidate", CANDIDATE_COMMIT, 2, 2),
}

WILLIAMS_ROWS = [
    tuple("ABHCGDFE"),
    tuple("BCADHEGF"),
    tuple("CDBEAFHG"),
    tuple("DECFBGAH"),
    tuple("EFDGCHBA"),
    tuple("FGEHDACB"),
    tuple("GHFAEBDC"),
    tuple("HAGBFCED"),
]


@dataclass(frozen=True)
class Workload:
    name: str
    template: str
    assessments: int
    accepted: int
    rejected: int
    rendered: int


SCRIPT_DIR = Path(__file__).resolve().parent
WORKLOADS = {
    "W1": Workload(
        "W1", "workloads/w1-scalar.repl.in", assessments=24,
        accepted=24, rejected=0, rendered=24,
    ),
    "W2": Workload(
        "W2", "workloads/w2-product.repl.in", assessments=48,
        accepted=24, rejected=24, rendered=6,
    ),
}


SMT_PREAMBLE = (
    b"(reset)\n"
    b"(set-option :print-success false)\n"
    b"(set-option :produce-models true)\n"
    b"(set-option :random-seed 1)\n"
    b"(set-logic QF_LIA)\n"
    b"(define-fun djex_nat_monus ((x Int) (y Int)) Int "
    b"(ite (<= y x) (- x y) 0))\n"
    b"(define-fun djex_nat_min ((x Int) (y Int)) Int "
    b"(ite (<= x y) x y))\n"
    b"(define-fun djex_nat_max ((x Int) (y Int)) Int "
    b"(ite (<= x y) y x))\n"
)
CAPABILITY_PROGRAM_ZERO = (
    SMT_PREAMBLE
    + b"(declare-const djex_capability_input Int)\n"
    + b"(assert (= djex_capability_input 0))\n"
    + b"(check-sat)\n"
)
CAPABILITY_PROGRAM_CONTRADICTION = (
    SMT_PREAMBLE
    + b"(declare-const djex_capability_input Int)\n"
    + b"(assert (= djex_capability_input 0))\n"
    + b"(assert (= djex_capability_input 1))\n"
    + b"(check-sat)\n"
)
W1_PROGRAM_PREFIX = (
    SMT_PREAMBLE
    + b"(declare-const djex_length_input_0 Int)\n"
    + b"(declare-const djex_length_modulo_quotient_0 Int)\n"
    + b"(declare-const djex_length_modulo_remainder_0 Int)\n"
    + b"(assert (<= 0 djex_length_input_0))\n"
    + b"(assert (<= 0 djex_length_modulo_quotient_0))\n"
    + b"(assert (<= 0 djex_length_modulo_remainder_0))\n"
    + b"(assert (<= djex_length_modulo_remainder_0 3))\n"
    + b"(assert (= djex_length_input_0 (+ (* 4 "
      b"djex_length_modulo_quotient_0) djex_length_modulo_remainder_0)))\n"
)
W1_PROGRAM = (
    W1_PROGRAM_PREFIX
    + b"(assert (<= (+ djex_length_input_0 4) "
      b"(+ djex_length_input_0 djex_length_modulo_remainder_0)))\n"
    + b"(check-sat)\n"
)
W2_PROGRAM_PREFIX = (
    SMT_PREAMBLE
    + b"(declare-const djex_length_input_0 Int)\n"
    + b"(declare-const djex_length_input_1 Int)\n"
    + b"(declare-const djex_length_modulo_quotient_0 Int)\n"
    + b"(declare-const djex_length_modulo_remainder_0 Int)\n"
    + b"(assert (<= 0 djex_length_input_0))\n"
    + b"(assert (<= 0 djex_length_input_1))\n"
    + b"(assert (<= 0 djex_length_modulo_quotient_0))\n"
    + b"(assert (<= 0 djex_length_modulo_remainder_0))\n"
    + b"(assert (<= djex_length_modulo_remainder_0 2))\n"
    + b"(assert (= djex_length_input_1 (+ (* 3 "
      b"djex_length_modulo_quotient_0) djex_length_modulo_remainder_0)))\n"
)
W2_CASES = {
    "input_1_plus_3": (
        W2_PROGRAM_PREFIX
        + b"(assert (<= (+ djex_length_input_1 3) "
          b"(+ djex_length_input_1 djex_length_modulo_remainder_0)))\n"
        + b"(check-sat)\n"
    ),
    "input_0_plus_3": (
        W2_PROGRAM_PREFIX
        + b"(assert (<= (+ djex_length_input_0 3) "
          b"(+ djex_length_input_1 djex_length_modulo_remainder_0)))\n"
        + b"(check-sat)\n"
    ),
    "input_1_plus_4": (
        W2_PROGRAM_PREFIX
        + b"(assert (<= (+ djex_length_input_1 4) "
          b"(+ djex_length_input_1 djex_length_modulo_remainder_0)))\n"
        + b"(check-sat)\n"
    ),
    "input_0_plus_4": (
        W2_PROGRAM_PREFIX
        + b"(assert (<= (+ djex_length_input_0 4) "
          b"(+ djex_length_input_1 djex_length_modulo_remainder_0)))\n"
        + b"(check-sat)\n"
    ),
}
EXPECTED_QUERY_SEQUENCE = {
    "W1": [("input_0_plus_4", "unsat")] * 24,
    "W2": (
        [("input_1_plus_3", "unsat")] * 6
        + [("input_0_plus_3", "sat")] * 6
        + [("input_1_plus_4", "unsat")] * 18
        + [("input_0_plus_4", "sat")] * 18
    ),
}


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def run_identifier(output: Path) -> str:
    return sha256_bytes(os.fsencode(str(output.resolve())))


def canonical_json(value: Any) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n").encode()


def write_json(path: Path, value: Any) -> None:
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_bytes(canonical_json(value))
    os.replace(temporary, path)


def non_directory_residue(root: Path) -> list[str]:
    if not root.exists() and not root.is_symlink():
        return []
    return sorted(
        str(path.relative_to(root))
        for path in root.rglob("*")
        if path.is_symlink() or not path.is_dir()
    )


def require(condition: bool, message: str) -> None:
    if not condition:
        raise HarnessFailure(message)


def require_sha256(source: str, label: str) -> str:
    value = source.lower()
    require(bool(re.fullmatch(r"[0-9a-f]{64}", value)), f"{label} is not SHA-256")
    return value


def command_output(arguments: Sequence[str], *, cwd: Path | None = None) -> bytes:
    completed = subprocess.run(
        list(arguments), cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        check=False,
    )
    require(
        completed.returncode == 0,
        f"command failed ({completed.returncode}): {arguments!r}: "
        f"{completed.stderr.decode(errors='replace')}",
    )
    return completed.stdout


def git_value(root: Path, *arguments: str) -> str:
    return command_output(["/usr/bin/git", "-C", str(root), *arguments]).decode().strip()


def verify_source_root(root: Path, expected_commit: str, label: str) -> dict[str, Any]:
    root = root.resolve()
    require(root.is_dir(), f"{label} source root is not a directory: {root}")
    actual = git_value(root, "rev-parse", "HEAD")
    require(actual == expected_commit, f"{label} HEAD {actual} != {expected_commit}")
    tracked = subprocess.run(
        [
            "/usr/bin/git", "-C", str(root), "status", "--porcelain",
            "--untracked-files=no",
        ],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False,
    )
    require(tracked.returncode == 0, f"cannot inspect {label} tracked worktree")
    require(tracked.stdout == b"", f"{label} worktree has tracked changes")
    archive = command_output([
        "/usr/bin/git", "-C", str(root), "archive", "--format=tar",
        expected_commit,
    ])
    tree = git_value(root, "rev-parse", f"{expected_commit}^{{tree}}")
    archive_sha256 = sha256_bytes(archive)
    frozen = FROZEN_SOURCE_IDENTITIES[expected_commit]
    require(tree == frozen["tree"], f"{label} Git tree identity drifted")
    require(
        archive_sha256 == frozen["archive_sha256"],
        f"{label} Git archive identity drifted",
    )
    return {
        "root": str(root),
        "head": actual,
        "tree": tree,
        "archive_sha256": archive_sha256,
        "tracked_clean": True,
    }


def verify_protocol_repository() -> dict[str, Any]:
    root = SCRIPT_DIR.parent.resolve()
    require(
        Path(git_value(root, "rev-parse", "--show-toplevel")).resolve() == root,
        f"protocol source is not rooted in the expected repository: {root}",
    )
    status = command_output([
        "/usr/bin/git", "-C", str(root), "status", "--porcelain",
        "--untracked-files=no",
    ])
    require(status == b"", "protocol repository has tracked changes")
    tracked_paths = [
        Path(__file__).resolve(),
        SCRIPT_DIR / "README.md",
        SCRIPT_DIR / "result-schema.tsv",
        *(SCRIPT_DIR / workload.template for workload in WORKLOADS.values()),
        root / "djex.cabal",
    ]
    relative_paths: list[str] = []
    for path in tracked_paths:
        relative = str(path.relative_to(root))
        tracked = command_output([
            "/usr/bin/git", "-C", str(root), "ls-files", "--error-unmatch",
            "--", relative,
        ]).decode().strip()
        require(tracked == relative, f"protocol artifact is not singly tracked: {relative}")
        relative_paths.append(relative)
    head = git_value(root, "rev-parse", "HEAD")
    archive = command_output([
        "/usr/bin/git", "-C", str(root), "archive", "--format=tar", head,
    ])
    return {
        "root": str(root),
        "head": head,
        "tree": git_value(root, "rev-parse", f"{head}^{{tree}}"),
        "archive_sha256": sha256_bytes(archive),
        "tracked_clean": True,
        "tracked_protocol_paths": relative_paths,
    }


def normalized_plan_bytes(plan: Any, root: Path) -> bytes:
    root_text = str(root.resolve())

    def replace(value: Any) -> Any:
        if isinstance(value, str):
            return value.replace(root_text, NORMALIZED_PLAN_ROOT)
        if isinstance(value, list):
            return [replace(item) for item in value]
        if isinstance(value, dict):
            normalized: dict[str, Any] = {}
            for key, item in value.items():
                require(isinstance(key, str), "build plan contains a non-string key")
                normalized_key = key.replace(root_text, NORMALIZED_PLAN_ROOT)
                require(
                    normalized_key not in normalized,
                    f"build-plan key collision after root normalization: {normalized_key}",
                )
                normalized[normalized_key] = replace(item)
            return normalized
        require(
            value is None or isinstance(value, (bool, int, float)),
            f"build plan contains unsupported JSON value {type(value).__name__}",
        )
        return value

    return (
        json.dumps(replace(plan), sort_keys=True, indent=2) + "\n"
    ).encode()


def normalized_plan_identity(plan: Any, root: Path, label: str) -> dict[str, Any]:
    normalized = normalized_plan_bytes(plan, root)
    digest = sha256_bytes(normalized)
    require(
        len(normalized) == NORMALIZED_PLAN_SIZE,
        f"{label} normalized plan size {len(normalized)} != {NORMALIZED_PLAN_SIZE}",
    )
    require(
        digest == NORMALIZED_PLAN_SHA256,
        f"{label} normalized plan SHA-256 {digest} != {NORMALIZED_PLAN_SHA256}",
    )
    return {
        "sha256": digest,
        "size": len(normalized),
        "root_replacement": NORMALIZED_PLAN_ROOT,
        "serialization": "json.dumps(sort_keys=True, indent=2) + newline; UTF-8/default ASCII",
    }


def build_plan_identity(
    root: Path, binary: Path, expected_sha256: str, label: str
) -> dict[str, Any]:
    root = root.resolve()
    binary = binary.resolve()
    plan_path = root / "dist-newstyle/cache/plan.json"
    require(plan_path.is_file(), f"{label} build plan is missing: {plan_path}")
    digest = sha256_file(plan_path)
    require(digest == expected_sha256, f"{label} plan SHA-256 {digest} != {expected_sha256}")
    try:
        plan = json.loads(plan_path.read_bytes())
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as failure:
        raise HarnessFailure(f"cannot parse {label} build plan: {failure}") from failure
    require(plan.get("compiler-id") == f"ghc-{PINNED_GHC_VERSION}", f"{label} compiler drifted")
    require(plan.get("cabal-version") == PINNED_CABAL_VERSION, f"{label} Cabal drifted")
    require(plan.get("os") == "linux" and plan.get("arch") == "x86_64", f"{label} build platform drifted")
    units = [
        unit for unit in plan.get("install-plan", [])
        if unit.get("style") == "local" and unit.get("component-name") == "exe:djex"
    ]
    require(len(units) == 1, f"{label} plan has {len(units)} local exe:djex units")
    unit = units[0]
    source = unit.get("pkg-src", {})
    require(source.get("type") == "local", f"{label} exe:djex source is not local")
    require(
        Path(source.get("path", "")).resolve() == root,
        f"{label} exe:djex plan source does not bind the supplied root",
    )
    dist_dir = Path(unit.get("dist-dir", "")).resolve()
    expected_binary = (dist_dir / "build/djex/djex").resolve()
    require(binary == expected_binary, f"{label} binary {binary} != plan output {expected_binary}")
    require(dist_dir.name == "opt", f"{label} exe:djex is not from Cabal's opt layout")
    normalized = normalized_plan_identity(plan, root, label)
    return {
        "path": str(plan_path),
        "sha256": digest,
        "compiler_id": plan["compiler-id"],
        "cabal_version": plan["cabal-version"],
        "os": plan["os"],
        "arch": plan["arch"],
        "unit_id": unit.get("id"),
        "component_name": unit.get("component-name"),
        "source_root": str(root),
        "dist_dir": str(dist_dir),
        "expected_binary": str(expected_binary),
        "flags": unit.get("flags", {}),
        "normalized": normalized,
    }


def executable_identity(
    path: Path,
    expected_sha256: str,
    label: str,
    *,
    expected_build_id: str | None = None,
    expected_library_hashes: set[str] | None = None,
) -> dict[str, Any]:
    path = path.resolve()
    require(path.is_absolute() and path.is_file(), f"{label} is not an absolute file: {path}")
    require(os.access(path, os.X_OK), f"{label} is not executable: {path}")
    digest = sha256_file(path)
    require(digest == expected_sha256, f"{label} SHA-256 {digest} != {expected_sha256}")
    stat = path.stat()
    readelf = command_output(["/usr/bin/readelf", "-n", str(path)]).decode(errors="replace")
    build_ids = re.findall(r"Build ID:\s*([0-9a-fA-F]+)", readelf)
    require(len(build_ids) == 1, f"{label} does not have exactly one ELF build ID")
    build_id = build_ids[0].lower()
    if expected_build_id is not None:
        require(build_id == expected_build_id, f"{label} build ID {build_id} != {expected_build_id}")
    ldd = command_output(["/usr/bin/ldd", str(path)]).decode(errors="replace")
    libraries: list[dict[str, str]] = []
    seen: set[str] = set()
    for line in ldd.splitlines():
        matches = re.findall(r"(/[^\s()]+)", line)
        for source in matches:
            library = str(Path(source).resolve())
            if library in seen or not Path(library).is_file():
                continue
            seen.add(library)
            libraries.append({"path": library, "sha256": sha256_file(Path(library))})
    require(libraries, f"{label} dynamic dependency identity is empty")
    libraries.sort(key=lambda value: value["path"])
    if expected_library_hashes is not None:
        actual_library_hashes = {value["sha256"] for value in libraries}
        require(
            actual_library_hashes == expected_library_hashes,
            f"{label} dynamic library hashes {sorted(actual_library_hashes)} "
            f"!= {sorted(expected_library_hashes)}",
        )
    return {
        "path": str(path),
        "sha256": digest,
        "size": stat.st_size,
        "device": stat.st_dev,
        "inode": stat.st_ino,
        "build_id": build_id,
        "dynamic_libraries": libraries,
        "ldd_sha256": sha256_bytes(ldd.encode()),
    }


def tool_identities(strace: str) -> dict[str, Any]:
    require(Path(strace).resolve() == Path("/usr/bin/strace"), "strace path differs from frozen path")
    identities: dict[str, Any] = {}
    for source, expected_sha256 in PINNED_TOOLS.items():
        path = Path(source)
        require(path.is_file() and os.access(path, os.X_OK), f"required tool unavailable: {path}")
        actual = sha256_file(path)
        require(actual == expected_sha256, f"tool SHA-256 drifted for {path}: {actual}")
        identities[source] = {"sha256": actual}
    ghc_source = shutil.which("ghc")
    cabal_source = shutil.which("cabal")
    require(ghc_source is not None and cabal_source is not None, "GHC/Cabal unavailable")
    ghc = Path(ghc_source).resolve()
    cabal = Path(cabal_source).resolve()
    require(sha256_file(ghc) == PINNED_GHC_SHA256, f"GHC binary hash drifted: {ghc}")
    require(sha256_file(cabal) == PINNED_CABAL_SHA256, f"Cabal binary hash drifted: {cabal}")
    ghc_version = command_output([str(ghc), "--numeric-version"]).decode().strip()
    cabal_version = command_output([str(cabal), "--numeric-version"]).decode().strip()
    require(ghc_version == PINNED_GHC_VERSION, f"GHC version drifted: {ghc_version}")
    require(cabal_version == PINNED_CABAL_VERSION, f"Cabal version drifted: {cabal_version}")
    ghc_libdir = Path(
        command_output([str(ghc), "--print-libdir"]).decode().strip()
    ).resolve()
    ghc_compiler = ghc_libdir.parent / "bin" / f"ghc-{PINNED_GHC_VERSION}"
    require(ghc_compiler.is_file(), f"GHC compiler unavailable: {ghc_compiler}")
    require(
        sha256_file(ghc_compiler) == PINNED_GHC_COMPILER_SHA256,
        f"GHC compiler hash drifted: {ghc_compiler}",
    )
    identities["ghc"] = {
        "path": str(ghc),
        "sha256": PINNED_GHC_SHA256,
        "version": ghc_version,
        "compiler_path": str(ghc_compiler),
        "compiler_sha256": PINNED_GHC_COMPILER_SHA256,
    }
    identities["cabal"] = {
        "path": str(cabal), "sha256": PINNED_CABAL_SHA256,
        "version": cabal_version,
    }
    return identities


def python_identity() -> dict[str, Any]:
    executable = Path(sys.executable).resolve()
    require(
        executable == PINNED_PYTHON,
        f"Python interpreter path {executable} != {PINNED_PYTHON}",
    )
    digest = sha256_file(executable)
    require(
        digest == PINNED_PYTHON_SHA256,
        f"Python interpreter SHA-256 {digest} != {PINNED_PYTHON_SHA256}",
    )
    version = platform.python_version()
    require(
        version == PINNED_PYTHON_VERSION,
        f"Python interpreter version {version} != {PINNED_PYTHON_VERSION}",
    )
    return {
        "path": str(executable),
        "sha256": digest,
        "version": version,
        "version_detail": sys.version,
    }


def z3_package_identity() -> dict[str, Any]:
    require(PINNED_Z3_DEB.is_file(), f"pinned Z3 package is missing: {PINNED_Z3_DEB}")
    digest = sha256_file(PINNED_Z3_DEB)
    require(digest == PINNED_Z3_DEB_SHA256, f"Z3 package SHA-256 drifted: {digest}")
    fields = [
        command_output([
            "/usr/bin/dpkg-deb", "-f", str(PINNED_Z3_DEB), field
        ]).decode().strip()
        for field in ("Package", "Version", "Architecture")
    ]
    require(tuple(fields) == PINNED_Z3_PACKAGE, f"Z3 package metadata drifted: {fields}")
    version = command_output([str(PINNED_Z3), "-version"]).decode().strip()
    require(version == "Z3 version 4.8.12 - 64 bit", f"Z3 version output drifted: {version}")
    return {
        "path": str(PINNED_Z3_DEB),
        "sha256": digest,
        "package": fields[0],
        "version": fields[1],
        "architecture": fields[2],
        "z3_version_output": version,
    }


def o2_attestation_identity(
    baseline_root: Path, candidate_root: Path
) -> dict[str, Any]:
    require(O2_ATTESTATION.is_file(), f"O2 attestation is missing: {O2_ATTESTATION}")
    digest = sha256_file(O2_ATTESTATION)
    require(digest == O2_ATTESTATION_SHA256, f"O2 attestation SHA-256 drifted: {digest}")
    try:
        manifest = json.loads(O2_ATTESTATION.read_bytes())
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as failure:
        raise HarnessFailure(f"cannot parse O2 attestation: {failure}") from failure
    require(
        manifest.get("schema") == "djex-candidate-pipeline-o2-attestation/v1",
        "O2 attestation schema drifted",
    )
    require(
        manifest.get("invocation")
        == [
            "/home/codex/.ghcup/bin/cabal",
            "build",
            "exe:djex",
            "-O2",
            "--ghc-options=-Werror",
            "-j1",
        ],
        "O2 attestation invocation drifted",
    )
    require(
        manifest.get("cabal")
        == {
            "path": "/home/codex/.ghcup/bin/cabal",
            "version": PINNED_CABAL_VERSION,
            "sha256": PINNED_CABAL_SHA256,
        },
        "O2 attestation Cabal identity drifted",
    )
    require(
        manifest.get("ghc")
        == {
            "version": PINNED_GHC_VERSION,
            "sha256": PINNED_GHC_COMPILER_SHA256,
        },
        "O2 attestation GHC identity drifted",
    )
    expected = {
        "baseline": {
            "root": baseline_root.resolve(),
            "commit": BASELINE_COMMIT,
            "plan": BASELINE_PLAN_SHA256,
            "binary": BASELINE_BINARY_SHA256,
        },
        "candidate": {
            "root": candidate_root.resolve(),
            "commit": CANDIDATE_COMMIT,
            "plan": CANDIDATE_PLAN_SHA256,
            "binary": CANDIDATE_BINARY_SHA256,
        },
    }
    logs: dict[str, Any] = {}
    for label, values in expected.items():
        entry = manifest.get(label, {})
        require(
            Path(entry.get("working_directory", "")).resolve() == values["root"],
            f"{label} O2 attestation root drifted",
        )
        require(entry.get("commit") == values["commit"], f"{label} O2 commit drifted")
        require(entry.get("exit_code") == 0, f"{label} O2 attestation failed")
        require(entry.get("result") == "Up to date", f"{label} O2 result drifted")
        require(entry.get("tracked_clean_after") is True, f"{label} O2 tree was not clean")
        require(
            entry.get("plan_sha256_after") == values["plan"],
            f"{label} O2 plan hash drifted",
        )
        require(
            entry.get("binary_sha256_after") == values["binary"],
            f"{label} O2 binary hash drifted",
        )
        log = Path(entry.get("log", ""))
        require(log.is_file(), f"{label} O2 log is missing: {log}")
        log_digest = sha256_file(log)
        require(
            log_digest == O2_LOG_SHA256[label]
            and entry.get("log_sha256") == log_digest,
            f"{label} O2 log hash drifted",
        )
        require(b"Up to date" in log.read_bytes(), f"{label} O2 log lacks result")
        logs[label] = {"path": str(log), "sha256": log_digest}
    return {
        "path": str(O2_ATTESTATION),
        "sha256": digest,
        "manifest": manifest,
        "logs": logs,
    }


def host_identity() -> dict[str, Any]:
    affinity = sorted(os.sched_getaffinity(0)) if hasattr(os, "sched_getaffinity") else []
    require(len(affinity) >= 2, "the screen requires at least two CPUs in its affinity")
    status_lines = {}
    for line in Path("/proc/self/status").read_text().splitlines():
        if line.startswith(("Cpus_allowed_list:", "Mems_allowed_list:")):
            key, value = line.split(":", 1)
            status_lines[key] = value.strip()
    governors = {}
    for cpu in affinity:
        path = Path(f"/sys/devices/system/cpu/cpu{cpu}/cpufreq/scaling_governor")
        governors[str(cpu)] = path.read_text().strip() if path.is_file() else None
    return {
        "uname": list(platform.uname()),
        "os_release": Path("/etc/os-release").read_text() if Path("/etc/os-release").is_file() else None,
        "cpuinfo_sha256": sha256_file(Path("/proc/cpuinfo")),
        "affinity": affinity,
        "proc_status_limits": status_lines,
        "governors": governors,
        "clock_ticks_per_second": os.sysconf("SC_CLK_TCK"),
        "page_size": os.sysconf("SC_PAGE_SIZE"),
        "python": sys.version,
    }


def decode_mountinfo_field(source: str) -> str:
    return re.sub(
        r"\\([0-7]{3})",
        lambda match: chr(int(match.group(1), 8)),
        source,
    )


def cgroup2_mount_identity() -> dict[str, Any]:
    records: list[dict[str, Any]] = []
    for line in Path("/proc/self/mountinfo").read_text().splitlines():
        if " - " not in line:
            continue
        left, right = line.split(" - ", 1)
        before = left.split()
        after = right.split()
        require(
            len(before) >= 6 and len(after) >= 3,
            f"malformed /proc/self/mountinfo record: {line}",
        )
        if after[0] != "cgroup2":
            continue
        mount_point = Path(decode_mountinfo_field(before[4])).resolve()
        stat = mount_point.stat()
        records.append({
            "mount_id": int(before[0]),
            "parent_mount_id": int(before[1]),
            "major_minor": before[2],
            "root": decode_mountinfo_field(before[3]),
            "mount_point": str(mount_point),
            "mount_options": before[5],
            "optional_fields": before[6:],
            "filesystem_type": after[0],
            "source": decode_mountinfo_field(after[1]),
            "super_options": after[2],
            "device": stat.st_dev,
            "inode": stat.st_ino,
        })
    require(len(records) == 1, f"expected one cgroup2 mount, found {len(records)}")
    return records[0]


def parse_id_set(source: str, label: str) -> list[int]:
    require(bool(source), f"{label} is empty")
    result: set[int] = set()
    for part in source.split(","):
        match = re.fullmatch(r"([0-9]+)(?:-([0-9]+))?", part)
        require(match is not None, f"malformed {label}: {source}")
        first = int(match.group(1))
        last = int(match.group(2)) if match.group(2) is not None else first
        require(first <= last, f"descending range in {label}: {part}")
        values = set(range(first, last + 1))
        require(result.isdisjoint(values), f"overlapping range in {label}: {part}")
        result.update(values)
    return sorted(result)


def parse_cpu_max(source: str, label: str) -> dict[str, Any]:
    fields = source.split()
    require(len(fields) == 2, f"malformed {label}: {source}")
    quota: str | int
    if fields[0] == "max":
        quota = "max"
    else:
        require(fields[0].isdigit() and int(fields[0]) > 0, f"malformed {label}: {source}")
        quota = int(fields[0])
    require(fields[1].isdigit() and int(fields[1]) > 0, f"malformed {label}: {source}")
    return {"quota": quota, "period": int(fields[1])}


def parse_cpu_stat(source: str, label: str) -> dict[str, int]:
    values: dict[str, int] = {}
    for line in source.splitlines():
        fields = line.split()
        require(
            len(fields) == 2
            and re.fullmatch(r"[a-z0-9_]+", fields[0]) is not None
            and fields[1].isdigit(),
            f"malformed {label}: {line}",
        )
        require(fields[0] not in values, f"duplicate {label} field: {fields[0]}")
        values[fields[0]] = int(fields[1])
    require(
        {"usage_usec", "user_usec", "system_usec"}.issubset(values),
        f"incomplete {label}",
    )
    return values


def optional_cgroup_control(
    path: Path, label: str, parser_function: Any,
) -> dict[str, Any] | None:
    if not path.exists():
        return None
    raw = path.read_text().strip()
    require(bool(raw), f"empty {label}: {path}")
    return {"raw": raw, "parsed": parser_function(raw, label)}


def host_control_snapshot() -> dict[str, Any]:
    affinity = sorted(os.sched_getaffinity(0))
    membership_raw = Path("/proc/self/cgroup").read_text().strip()
    memberships = []
    for line in membership_raw.splitlines():
        fields = line.split(":", 2)
        require(len(fields) == 3, f"malformed /proc/self/cgroup record: {line}")
        if fields[0] == "0" and fields[1] == "":
            memberships.append(fields[2])
    require(
        len(memberships) == 1 and memberships[0].startswith("/"),
        f"expected one unified cgroup membership, found {memberships}",
    )
    membership = memberships[0]
    mount = cgroup2_mount_identity()
    mount_point = Path(mount["mount_point"])
    mount_root = mount["root"].rstrip("/") or "/"
    if mount_root == "/":
        relative = membership.lstrip("/")
    else:
        require(
            membership == mount_root or membership.startswith(mount_root + "/"),
            "unified cgroup membership is outside the visible cgroup2 mount root",
        )
        relative = membership[len(mount_root) :].lstrip("/")
    leaf = mount_point.joinpath(*([part for part in relative.split("/") if part]))
    leaf = leaf.resolve()
    require(leaf.is_dir(), f"unified cgroup leaf is unavailable: {leaf}")
    require(
        leaf == mount_point or mount_point in leaf.parents,
        f"unified cgroup leaf escaped mount: {leaf}",
    )
    paths: list[Path] = []
    current = leaf
    while True:
        paths.append(current)
        if current == mount_point:
            break
        require(current.parent != current, "cgroup ancestor walk escaped its mount")
        current = current.parent
    ancestors: list[dict[str, Any]] = []
    for path in paths:
        relative_path = (
            "/" if path == mount_point
            else "/" + path.relative_to(mount_point).as_posix()
        )
        stat = path.stat()
        cpu_stat_path = path / "cpu.stat"
        require(cpu_stat_path.exists(), f"cgroup ancestor lacks cpu.stat: {path}")
        cpu_stat_raw = cpu_stat_path.read_text().strip()
        ancestors.append({
            "relative_path": relative_path,
            "absolute_path": str(path),
            "directory_identity": {
                "device": stat.st_dev,
                "inode": stat.st_ino,
            },
            "cpuset_cpus_effective": optional_cgroup_control(
                path / "cpuset.cpus.effective", "cpuset.cpus.effective",
                parse_id_set,
            ),
            "cpuset_mems_effective": optional_cgroup_control(
                path / "cpuset.mems.effective", "cpuset.mems.effective",
                parse_id_set,
            ),
            "cpu_max": optional_cgroup_control(
                path / "cpu.max", "cpu.max", parse_cpu_max,
            ),
            "cpu_stat": {
                "raw": cpu_stat_raw,
                "parsed": parse_cpu_stat(cpu_stat_raw, "cpu.stat"),
            },
        })
    nearest: dict[str, dict[str, Any]] = {}
    for key in (
        "cpuset_cpus_effective", "cpuset_mems_effective", "cpu_max",
    ):
        matches = [
            {"relative_path": item["relative_path"], "value": item[key]}
            for item in ancestors if item[key] is not None
        ]
        require(matches, f"no ancestor exposes {key}")
        nearest[key] = matches[0]
    pressure = {
        kind: Path(f"/proc/pressure/{kind}").read_text().strip()
        for kind in ("cpu", "memory", "io")
    }
    return {
        "schema": "djex-host-control-snapshot/v1",
        "captured_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "monotonic_ns": time.monotonic_ns(),
        "pid": os.getpid(),
        "sched_affinity": affinity,
        "proc_self_cgroup": {
            "raw": membership_raw,
            "unified_path": membership,
        },
        "cgroup2_mount": mount,
        "ancestors_leaf_to_root": ancestors,
        "nearest_effective_controls": nearest,
        "diagnostics_only": {
            "loadavg": Path("/proc/loadavg").read_text().strip(),
            "pressure": pressure,
        },
    }


def attest_host_control_start(snapshot: dict[str, Any]) -> dict[str, Any]:
    affinity = snapshot.get("sched_affinity", [])
    nearest = snapshot.get("nearest_effective_controls", {})
    effective = (
        nearest.get("cpuset_cpus_effective", {}).get("value", {}).get("parsed")
    )
    throttle_paths = [
        item.get("relative_path")
        for item in snapshot.get("ancestors_leaf_to_root", [])
        if {"nr_throttled", "throttled_usec"}.issubset(
            item.get("cpu_stat", {}).get("parsed", {})
        )
    ]
    partial_throttle_paths = [
        item.get("relative_path")
        for item in snapshot.get("ancestors_leaf_to_root", [])
        if any(
            key in item.get("cpu_stat", {}).get("parsed", {})
            for key in ("nr_throttled", "throttled_usec")
        )
        and not {"nr_throttled", "throttled_usec"}.issubset(
            item.get("cpu_stat", {}).get("parsed", {})
        )
    ]
    gates = [
        {
            "name": "affinity_has_at_least_two_cpus",
            "pass": len(affinity) >= 2,
            "observed": affinity,
        },
        {
            "name": "affinity_equals_inherited_effective_cpuset",
            "pass": affinity == effective,
            "observed": {"affinity": affinity, "effective_cpuset": effective},
        },
        {
            "name": "throttle_counter_coverage",
            "pass": bool(throttle_paths),
            "observed": throttle_paths,
        },
        {
            "name": "throttle_counter_pairs_complete",
            "pass": not partial_throttle_paths,
            "observed": partial_throttle_paths,
        },
    ]
    return {
        "schema": "djex-host-control-start-attestation/v1",
        "verdict": "PASS" if all(item["pass"] for item in gates) else "HOLD",
        "gates": gates,
    }


def attest_host_control_window(
    start: dict[str, Any], end: dict[str, Any],
) -> dict[str, Any]:
    gates: list[dict[str, Any]] = []

    def gate(name: str, passed: bool, start_value: Any, end_value: Any) -> None:
        gates.append({
            "name": name,
            "pass": bool(passed),
            "start": start_value,
            "end": end_value,
        })

    gate("schema", start.get("schema") == end.get("schema"), start.get("schema"), end.get("schema"))
    gate("pid", start.get("pid") == end.get("pid"), start.get("pid"), end.get("pid"))
    for key in (
        "sched_affinity", "proc_self_cgroup", "cgroup2_mount",
        "nearest_effective_controls",
    ):
        gate(key, start.get(key) == end.get(key), start.get(key), end.get(key))
    start_ancestors = start.get("ancestors_leaf_to_root", [])
    end_ancestors = end.get("ancestors_leaf_to_root", [])
    start_paths = [item.get("relative_path") for item in start_ancestors]
    end_paths = [item.get("relative_path") for item in end_ancestors]
    gate("ancestor_paths", start_paths == end_paths, start_paths, end_paths)
    end_by_path = {item.get("relative_path"): item for item in end_ancestors}
    throttle_deltas: dict[str, dict[str, int]] = {}
    throttle_paths = 0
    for initial in start_ancestors:
        path = initial.get("relative_path")
        final = end_by_path.get(path)
        gate(f"{path}.present", final is not None, True, final is not None)
        if final is None:
            continue
        for key in (
            "absolute_path", "directory_identity", "cpuset_cpus_effective",
            "cpuset_mems_effective", "cpu_max",
        ):
            gate(
                f"{path}.{key}", initial.get(key) == final.get(key),
                initial.get(key), final.get(key),
            )
        initial_stat = initial.get("cpu_stat", {}).get("parsed", {})
        final_stat = final.get("cpu_stat", {}).get("parsed", {})
        gate(
            f"{path}.cpu_stat_fields",
            set(initial_stat) == set(final_stat), sorted(initial_stat),
            sorted(final_stat),
        )
        initial_has_pair = {
            "nr_throttled", "throttled_usec"
        }.issubset(initial_stat)
        final_has_pair = {
            "nr_throttled", "throttled_usec"
        }.issubset(final_stat)
        partial = any(
            key in initial_stat or key in final_stat
            for key in ("nr_throttled", "throttled_usec")
        )
        gate(
            f"{path}.throttle_counter_pair",
            initial_has_pair == final_has_pair and (not partial or initial_has_pair),
            initial_has_pair, final_has_pair,
        )
        if initial_has_pair and final_has_pair:
            throttle_paths += 1
            deltas = {
                key: final_stat[key] - initial_stat[key]
                for key in ("nr_throttled", "throttled_usec")
            }
            throttle_deltas[path] = deltas
            for key, delta in deltas.items():
                gate(f"{path}.{key}_delta", delta == 0, 0, delta)
    gate(
        "throttle_counter_coverage", throttle_paths > 0, ">=1 ancestor",
        throttle_paths,
    )
    passed = all(item["pass"] for item in gates)
    return {
        "schema": "djex-host-control-attestation/v1",
        "verdict": "PASS" if passed else "HOLD",
        "throttle_deltas": throttle_deltas,
        "diagnostic_start": start.get("diagnostics_only"),
        "diagnostic_end": end.get("diagnostics_only"),
        "gates": gates,
    }


def finalize_host_control_evidence(
    output: Path,
    start: dict[str, Any],
    start_sha256: str | None,
    start_attestation_sha256: str | None,
) -> dict[str, Any]:
    result: dict[str, Any] = {
        "end_sha256": None,
        "attestation_sha256": None,
        "failure": None,
    }
    try:
        end_snapshot = host_control_snapshot()
        end_path = output / "host-control-end.json"
        write_json(end_path, end_snapshot)
        result["end_sha256"] = sha256_file(end_path)
    except BaseException as failure:
        result["failure"] = (
            "host-control end-snapshot failure: "
            f"{type(failure).__name__}: {failure}"
        )
        return result
    try:
        start_path = output / "host-control-start.json"
        require(
            start_sha256 is not None
            and start_path.is_file()
            and sha256_file(start_path) == start_sha256,
            "host-control start snapshot changed before finalization",
        )
        start_attestation_path = output / "host-control-start-attestation.json"
        require(
            start_attestation_sha256 is not None
            and start_attestation_path.is_file()
            and sha256_file(start_attestation_path) == start_attestation_sha256,
            "host-control start attestation changed before finalization",
        )
        host_attestation = attest_host_control_window(start, end_snapshot)
        host_attestation.update({
            "start_snapshot": {
                "path": str(start_path),
                "sha256": start_sha256,
            },
            "start_attestation_sha256": start_attestation_sha256,
            "end_snapshot": {
                "path": str(end_path),
                "sha256": result["end_sha256"],
            },
        })
        host_attestation_path = output / "host-control-attestation.json"
        write_json(host_attestation_path, host_attestation)
        result["attestation_sha256"] = sha256_file(host_attestation_path)
        if host_attestation["verdict"] != "PASS":
            failed_gates = [
                gate["name"] for gate in host_attestation["gates"]
                if not gate["pass"]
            ]
            result["failure"] = (
                f"host-control attestation failed: {failed_gates}"
            )
    except BaseException as failure:
        result["failure"] = (
            "host-control attestation failure after end snapshot: "
            f"{type(failure).__name__}: {failure}"
        )
    return result


def framed_transcript(exit_code: int, stdout: bytes, stderr: bytes) -> str:
    digest = hashlib.sha256()
    for value in (str(exit_code).encode(), stdout, stderr):
        digest.update(len(value).to_bytes(8, "big"))
        digest.update(value)
    return digest.hexdigest()


def render_input(workload: Workload, jobs: int, z3: Path, z3_sha256: str) -> bytes:
    path = SCRIPT_DIR / workload.template
    source = path.read_text()
    require(source.count("@JOBS@") == 1, f"{path} has a drifting jobs placeholder")
    require(source.count("@Z3_PATH@") == 1, f"{path} has a drifting Z3 path placeholder")
    require(source.count("@Z3_SHA256@") == 1, f"{path} has a drifting Z3 hash placeholder")
    rendered = (
        source.replace("@JOBS@", str(jobs))
        .replace("@Z3_PATH@", str(z3))
        .replace("@Z3_SHA256@", z3_sha256)
    )
    require("@" not in rendered, f"{path} contains an unexpanded placeholder")
    return rendered.encode()


def validate_transcript(workload: Workload, exit_code: int, stdout: bytes, stderr: bytes) -> dict[str, Any]:
    require(exit_code == 0, f"{workload.name} exited {exit_code}")
    codes = [value.decode() for value in DIAGNOSTIC_RE.findall(stderr)]
    require(
        codes == ["DJEX_SEARCH_TRUNCATED"],
        f"{workload.name} diagnostics {codes!r} != sole truncation warning",
    )
    lowered = stderr.lower()
    for forbidden in (b"unassessed", b"unknown", b"length_where_session", b"search_timeout"):
        require(forbidden not in lowered, f"{workload.name} emitted forbidden {forbidden!r}")
    separators = stdout.count(OR_SEPARATOR)
    rendered = separators + 1 if stdout else 0
    require(
        rendered == workload.rendered,
        f"{workload.name} rendered {rendered}, expected {workload.rendered}",
    )
    require(b"\x00" not in stdout + stderr, f"{workload.name} transcript contains NUL")
    semantic = {
        "schema": "djex-select-best-observable-semantics/v1",
        "workload": workload.name,
        "rendered_candidates": rendered,
        "separator_count": separators,
        "diagnostics": codes,
        "stdout_sha256": sha256_bytes(stdout),
        "stderr_sha256": sha256_bytes(stderr),
    }
    return {
        "rendered": rendered,
        "truncations": 1,
        "semantic_sha256": sha256_bytes(canonical_json(semantic)),
    }


def parse_allocated_bytes(stats: bytes) -> int:
    matches = re.findall(rb"^\s*([0-9][0-9,]*)\s+bytes allocated in the heap\s*$", stats, re.MULTILINE)
    require(len(matches) == 1, "RTS statistics do not contain exactly one allocation total")
    return int(matches[0].replace(b",", b""))


def proc_stat(pid: int) -> tuple[int, int, int, int, int, int] | None:
    try:
        source = Path(f"/proc/{pid}/stat").read_text()
    except (FileNotFoundError, ProcessLookupError, PermissionError):
        return None
    close = source.rfind(")")
    if close < 0:
        return None
    fields = source[close + 2 :].split()
    if len(fields) < 22:
        return None
    return (
        int(fields[1]), int(fields[2]), int(fields[11]), int(fields[12]),
        int(fields[19]), int(fields[21]),
    )


def process_tgid(pid: int) -> int | None:
    try:
        source = Path(f"/proc/{pid}/status").read_text()
    except (FileNotFoundError, ProcessLookupError, PermissionError):
        return None
    match = re.search(r"(?m)^Tgid:\s*([0-9]+)\s*$", source)
    return int(match.group(1)) if match is not None else None


class ProcessTreeSampler:
    def __init__(
        self, root_pid: int, solver_sha256: str, solver_argv0: str,
        interval: float,
    ):
        self.root_pid = root_pid
        self.solver_sha256 = solver_sha256
        self.solver_argv0 = solver_argv0.encode()
        self.interval = interval
        self.stop_event = threading.Event()
        self.thread = threading.Thread(target=self._loop, name="djex-benchmark-proc-sampler")
        self.lock = threading.Lock()
        self.cpu_ticks: dict[int, int] = {}
        self.start_times: dict[int, int] = {}
        self.peak_rss_pages = 0
        self.known = {root_pid}
        self.parents: dict[int, int | None] = {root_pid: None}
        self.solver_pids: set[int] = set()
        self.checked_images: dict[int, str] = {}
        self.solver_images: dict[int, dict[str, Any]] = {}
        self.sample_count = 0
        self.first_sample_ns: int | None = None
        self.last_sample_ns: int | None = None
        self.last_sample_finished_ns: int | None = None
        self.interval_sum_ns = 0
        self.max_interval_ns = 0
        self.max_pass_ns = 0
        self.error: str | None = None

    def start(self) -> None:
        self.thread.start()

    def stop(self) -> None:
        self.stop_event.set()
        interrupted: BaseException | None = None
        while self.thread.is_alive():
            try:
                self.thread.join()
            except BaseException as failure:
                if interrupted is None:
                    interrupted = failure
        if self.error is not None:
            raise HarnessFailure(f"process sampler failed: {self.error}")
        if interrupted is not None:
            raise interrupted

    def _loop(self) -> None:
        try:
            while True:
                sample_started = time.monotonic_ns()
                self._sample()
                sample_finished = time.monotonic_ns()
                with self.lock:
                    if self.last_sample_ns is not None:
                        interval = sample_started - self.last_sample_ns
                        self.interval_sum_ns += interval
                        self.max_interval_ns = max(self.max_interval_ns, interval)
                    else:
                        self.first_sample_ns = sample_started
                    self.last_sample_ns = sample_started
                    self.last_sample_finished_ns = sample_finished
                    self.sample_count += 1
                    self.max_pass_ns = max(
                        self.max_pass_ns, sample_finished - sample_started
                    )
                if self.stop_event.is_set():
                    break
                self.stop_event.wait(self.interval)
        except BaseException as failure:
            self.error = repr(failure)

    def _sample(self) -> None:
        with self.lock:
            frontier = list(self.known)
        visited: set[int] = set()
        while frontier:
            parent = frontier.pop()
            if parent in visited:
                continue
            visited.add(parent)
            task_root = Path(f"/proc/{parent}/task")
            try:
                tasks = list(task_root.iterdir())
            except (FileNotFoundError, ProcessLookupError, PermissionError):
                continue
            for task in tasks:
                try:
                    children = (task / "children").read_text().split()
                except (FileNotFoundError, ProcessLookupError, PermissionError):
                    continue
                for source in children:
                    observed = int(source)
                    child = process_tgid(observed)
                    if child is None:
                        continue
                    with self.lock:
                        unseen = child not in self.known
                        if unseen:
                            self.known.add(child)
                            self.parents[child] = parent
                    if unseen:
                        frontier.append(child)
        aggregate_rss = 0
        with self.lock:
            known = list(self.known)
        for pid in known:
            value = proc_stat(pid)
            if value is None:
                continue
            _ppid, _pgrp, user, system, started, rss = value
            with self.lock:
                prior_start = self.start_times.get(pid)
                if prior_start is not None and prior_start != started:
                    raise HarnessFailure(
                        f"PID {pid} was reused during one sampled process tree"
                    )
                self.start_times[pid] = started
                self.cpu_ticks[pid] = max(
                    self.cpu_ticks.get(pid, 0), user + system
                )
            aggregate_rss += max(0, rss)
            try:
                image_path = Path(f"/proc/{pid}/exe")
                target = os.readlink(image_path)
            except (FileNotFoundError, ProcessLookupError, PermissionError, OSError):
                continue
            with self.lock:
                image_unchecked = self.checked_images.get(pid) != target
            if not image_unchecked:
                continue
            if target != "/memfd:djex-z3-main-image (deleted)":
                with self.lock:
                    self.checked_images[pid] = target
                continue
            try:
                command_line = Path(f"/proc/{pid}/cmdline").read_bytes()
                environment = Path(f"/proc/{pid}/environ").read_bytes()
                arguments = command_line.rstrip(b"\0").split(b"\0")
                signature = arguments == [
                    self.solver_argv0,
                    b"-in",
                    b"-smt2",
                    b"smtlib2_compliant=true",
                    b"timeout=1000",
                    b"rlimit=100000",
                ] and environment == b""
                if not signature:
                    continue
                descriptor = os.open(image_path, os.O_RDONLY | os.O_CLOEXEC)
                stat = os.fstat(descriptor)
            except (FileNotFoundError, ProcessLookupError, PermissionError, OSError):
                continue
            with self.lock:
                if pid in self.solver_images:
                    os.close(descriptor)
                    continue
                self.checked_images[pid] = target
                self.solver_pids.add(pid)
                self.solver_images[pid] = {
                    "target": target,
                    "sha256": None,
                    "device": stat.st_dev,
                    "inode": stat.st_ino,
                    "start_time": started,
                    "argv": [value.decode("ascii") for value in arguments],
                    "environment_bytes": 0,
                    "descriptor": descriptor,
                }
        with self.lock:
            self.peak_rss_pages = max(self.peak_rss_pages, aggregate_rss)

    def metrics(self) -> tuple[int, int, int]:
        with self.lock:
            ticks = sum(self.cpu_ticks.values())
            peak_rss_pages = self.peak_rss_pages
            solver_count = len(self.solver_pids)
        cpu_ns = ticks * 1_000_000_000 // os.sysconf("SC_CLK_TCK")
        rss_bytes = peak_rss_pages * os.sysconf("SC_PAGE_SIZE")
        return cpu_ns, rss_bytes, solver_count

    def identity_snapshot(self) -> dict[int, int]:
        with self.lock:
            return dict(self.start_times)

    def solver_snapshot(self) -> dict[int, dict[str, Any]]:
        with self.lock:
            images = {pid: dict(value) for pid, value in self.solver_images.items()}
            for value in self.solver_images.values():
                value["descriptor"] = None
        failures: list[str] = []
        descriptors = {
            pid: value.get("descriptor") for pid, value in images.items()
            if value.get("descriptor") is not None
        }
        try:
            for pid, descriptor in descriptors.items():
                value = images[pid]
                digest = hashlib.sha256()
                try:
                    os.lseek(descriptor, 0, os.SEEK_SET)
                    while True:
                        block = os.read(descriptor, 1024 * 1024)
                        if not block:
                            break
                        digest.update(block)
                    value["sha256"] = digest.hexdigest()
                except BaseException as failure:
                    failures.append(
                        f"cannot hash solver image for PID {pid}: "
                        f"{type(failure).__name__}: {failure}"
                    )
                del value["descriptor"]
                with self.lock:
                    if pid in self.solver_images:
                        self.solver_images[pid] = dict(value)
        finally:
            for pid, descriptor in descriptors.items():
                try:
                    os.close(descriptor)
                except BaseException as failure:
                    failures.append(
                        f"cannot close solver image for PID {pid}: "
                        f"{type(failure).__name__}: {failure}"
                    )
        require(not failures, "; ".join(failures))
        return images

    def diagnostics(self) -> dict[str, Any]:
        with self.lock:
            sample_count = self.sample_count
            first = self.first_sample_ns
            last = self.last_sample_ns
            interval_sum = self.interval_sum_ns
            return {
                "sample_count": sample_count,
                "span_ns": 0 if first is None or last is None else last - first,
                "first_sample_ns": first,
                "last_sample_start_ns": last,
                "last_sample_finished_ns": self.last_sample_finished_ns,
                "mean_interval_ns": (
                    0 if sample_count < 2 else interval_sum // (sample_count - 1)
                ),
                "max_interval_ns": self.max_interval_ns,
                "max_pass_ns": self.max_pass_ns,
                "known_pids": sorted(self.known),
                "parents": {str(pid): parent for pid, parent in self.parents.items()},
                "start_times": dict(self.start_times),
                "cpu_ticks": dict(self.cpu_ticks),
                "peak_rss_pages": self.peak_rss_pages,
            }


def process_group_members(group: int) -> list[int]:
    members = []
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit():
            continue
        value = proc_stat(int(entry.name))
        if value is not None and value[1] == group:
            members.append(int(entry.name))
    return members


def matching_processes(identities: dict[int, int]) -> list[int]:
    matched = []
    for pid, expected_start in identities.items():
        value = proc_stat(pid)
        if value is not None and value[4] == expected_start:
            matched.append(pid)
    return matched


def terminate_owned_processes(group: int, identities: dict[int, int]) -> None:
    try:
        os.killpg(group, signal.SIGTERM)
    except ProcessLookupError:
        pass
    for pid in matching_processes(identities):
        try:
            os.kill(pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
    deadline = time.monotonic() + 0.5
    while time.monotonic() < deadline and (
        process_group_members(group) or matching_processes(identities)
    ):
        time.sleep(0.01)
    if process_group_members(group):
        try:
            os.killpg(group, signal.SIGKILL)
        except ProcessLookupError:
            pass
    for pid in matching_processes(identities):
        try:
            os.kill(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass


def validate_sampler_quality(
    diagnostics: dict[str, Any],
    wall_started_ns: int,
    wall_finished_ns: int,
    target_interval_ms: float,
) -> dict[str, Any]:
    require(diagnostics.get("sample_count", 0) >= 2, "process sampler has fewer than two samples")
    require(diagnostics.get("span_ns", 0) > 0, "process sampler has no observed span")
    first_sample = diagnostics.get("first_sample_ns")
    last_sample_start = diagnostics.get("last_sample_start_ns")
    last_sample_finished = diagnostics.get("last_sample_finished_ns")
    require(
        isinstance(first_sample, int)
        and isinstance(last_sample_start, int)
        and isinstance(last_sample_finished, int),
        "process sampler wall coverage endpoints are incomplete",
    )
    require(
        wall_started_ns <= first_sample <= last_sample_start <= last_sample_finished,
        "process sampler monotonic endpoints are inconsistent",
    )
    require(
        diagnostics["span_ns"] == last_sample_start - first_sample,
        "process sampler span is inconsistent",
    )
    require(
        diagnostics["mean_interval_ns"]
        == diagnostics["span_ns"] // (diagnostics["sample_count"] - 1),
        "process sampler mean interval is inconsistent",
    )
    wall_duration = wall_finished_ns - wall_started_ns
    require(wall_duration > 0, "subprocess wall duration is not positive")
    covered_duration = max(
        0,
        min(wall_finished_ns, last_sample_finished)
        - max(wall_started_ns, first_sample),
    )
    coverage = covered_duration / wall_duration
    target_interval_ns = int(target_interval_ms * 1_000_000)
    initial_delay = max(0, first_sample - wall_started_ns)
    terminal_gap = max(0, wall_finished_ns - last_sample_finished)
    require(
        initial_delay <= target_interval_ns * SAMPLER_MAX_INTERVAL_MULTIPLIER,
        "process sampler initial delay exceeded its preregistered bound",
    )
    require(
        terminal_gap <= target_interval_ns * SAMPLER_MAX_INTERVAL_MULTIPLIER,
        "process sampler terminal gap exceeded its preregistered bound",
    )
    require(
        coverage >= SAMPLER_MIN_WALL_COVERAGE,
        f"process sampler covered only {coverage:.6f} of wall time",
    )
    require(
        diagnostics["mean_interval_ns"]
        <= target_interval_ns * SAMPLER_MAX_MEAN_INTERVAL_MULTIPLIER,
        "process sampler mean interval exceeded its preregistered bound",
    )
    require(
        diagnostics["max_interval_ns"]
        <= target_interval_ns * SAMPLER_MAX_INTERVAL_MULTIPLIER,
        "process sampler maximum interval exceeded its preregistered bound",
    )
    require(
        diagnostics["max_pass_ns"]
        <= target_interval_ns * SAMPLER_MAX_PASS_MULTIPLIER,
        "process sampler maximum pass duration exceeded its preregistered bound",
    )
    return {
        "coverage_ratio": coverage,
        "initial_delay_ns": initial_delay,
        "terminal_gap_ns": terminal_gap,
    }


def c_string_literals(source: str) -> list[bytes]:
    literals = re.findall(r'"(?:\\.|[^"\\])*"', source)
    decoded = []
    for literal in literals:
        try:
            value = ast.literal_eval(literal)
        except (SyntaxError, ValueError):
            continue
        if isinstance(value, str):
            try:
                decoded.append(value.encode("latin1"))
            except UnicodeEncodeError as failure:
                raise HarnessFailure(
                    f"traced string is not an exact byte string: {literal}"
                ) from failure
    return decoded


@dataclass(frozen=True)
class TraceEvent:
    timestamp: float
    pid: int
    sequence: int
    syscall: str
    descriptor: int | None
    pipe: int | None
    source: str
    payload: bytes
    returned: int | None


def parse_trace_event(source: str, pid: int, sequence: int) -> TraceEvent | None:
    match = re.match(
        r"([0-9]+(?:\.[0-9]+)?)\s+([A-Za-z0-9_]+)\(", source
    )
    if match is None:
        return None
    timestamp = float(match.group(1))
    syscall = match.group(2)
    descriptor_match = re.search(
        rf"{re.escape(syscall)}\(([0-9]+)(?:<[^>]*>)?", source
    )
    pipe_match = re.search(rf"{re.escape(syscall)}\([0-9]+<pipe:\[([0-9]+)\]>", source)
    descriptor = (
        int(descriptor_match.group(1)) if descriptor_match is not None else None
    )
    pipe = int(pipe_match.group(1)) if pipe_match is not None else None
    returned: int | None = None
    payload = b""
    if syscall in ("read", "readv", "write", "writev"):
        result_match = re.search(r"\)\s+=\s+(-?[0-9]+)(?:\s|$)", source)
        require(result_match is not None, f"unparseable traced IO result: {source}")
        returned = int(result_match.group(1))
        if returned > 0:
            literals = c_string_literals(source)
            require(literals, f"successful traced IO has no byte buffer: {source}")
            requested = b"".join(literals)
            require(
                returned <= len(requested),
                "traced IO buffer was truncated: "
                f"returned {returned}, decoded {len(requested)}: {source}",
            )
            payload = requested[:returned]
    return TraceEvent(
        timestamp, pid, sequence, syscall, descriptor, pipe, source, payload,
        returned,
    )


def trace_file_events(path: Path, pid: int) -> list[TraceEvent]:
    events: list[TraceEvent] = []
    pending: tuple[str, str] | None = None
    for sequence, line in enumerate(path.read_text(errors="strict").splitlines()):
        timestamp = re.match(r"([0-9]+(?:\.[0-9]+)?)\s+", line)
        if "<unfinished ...>" in line:
            require(pending is None, f"nested unfinished strace record: {line}")
            require(timestamp is not None, f"unfinished record has no timestamp: {line}")
            body = line[timestamp.end() :].replace(" <unfinished ...>", "")
            syscall = re.match(r"([A-Za-z0-9_]+)\(", body)
            require(syscall is not None, f"unparseable unfinished record: {line}")
            pending = (syscall.group(1), body)
            continue
        resumed = re.search(r"<\.\.\. ([A-Za-z0-9_]+) resumed>", line)
        if resumed is not None:
            require(pending is not None, f"orphan resumed strace record: {line}")
            require(pending[0] == resumed.group(1), f"mismatched resumed record: {line}")
            require(timestamp is not None, f"resumed record has no timestamp: {line}")
            tail = line[resumed.end() :]
            combined = f"{timestamp.group(1)} {pending[1]}{tail}"
            event = parse_trace_event(combined, pid, sequence)
            if event is not None:
                events.append(event)
            pending = None
            continue
        event = parse_trace_event(line, pid, sequence)
        if event is not None:
            events.append(event)
    require(pending is None, f"unresolved unfinished strace record in {path}")
    return events


def split_framed_stream(
    source: bytes, pattern: re.Pattern[bytes], label: str
) -> list[tuple[bytes, bytes, bytes]]:
    frames: list[tuple[bytes, bytes, bytes]] = []
    offset = 0
    for match in pattern.finditer(source):
        frames.append((match.group(1), source[offset : match.end()], source[offset : match.start()]))
        offset = match.end()
    require(frames, f"{label} has no echo frames")
    require(offset == len(source), f"{label} has unframed trailing bytes")
    return frames


def validate_symbolic_program(workload: Workload, program: bytes) -> str:
    if workload.name == "W1":
        require(program == W1_PROGRAM, "W1 canonical symbolic program drifted")
        return "input_0_plus_4"
    matches = [name for name, expected in W2_CASES.items() if program == expected]
    require(len(matches) == 1, "W2 canonical symbolic program drifted")
    return matches[0]


def parse_trace(
    prefix: Path, workload: Workload, solver_pid: int
) -> dict[str, Any]:
    files = sorted(prefix.parent.glob(prefix.name + ".*"))
    require(files, f"strace emitted no files at {prefix}")
    solver_file: Path | None = None
    for path in files:
        suffix = path.name[len(prefix.name) + 1 :]
        require(suffix.isdigit(), f"unexpected strace artifact {path.name}")
        if int(suffix) == solver_pid:
            solver_file = path
    require(solver_file is not None, f"strace has no file for sealed solver PID {solver_pid}")
    file_source = solver_file.read_text(errors="strict")
    successful_execs = [
        line for line in file_source.splitlines()
        if re.search(r"\bexecve(?:at)?\(", line) is not None
        and re.search(r"\) = 0(?:\s|$)", line) is not None
    ]
    expected_exec_literals = [
        b"",
        str(PINNED_Z3).encode(),
        b"-in",
        b"-smt2",
        b"smtlib2_compliant=true",
        b"timeout=1000",
        b"rlimit=100000",
    ]
    solver_execs = [
        line for line in successful_execs
        if "execveat(" in line
        and "</memfd:djex-z3-main-image (deleted)>" in line
        and "AT_EMPTY_PATH" in line
        and "/* 0 vars */" in line
        and c_string_literals(line) == expected_exec_literals
    ]
    require(len(successful_execs) == 1, f"sealed solver PID has {len(successful_execs)} successful exec records")
    require(len(solver_execs) == 1, "sealed solver successful execveat vector drifted")
    events = trace_file_events(solver_file, solver_pid)
    events.sort(key=lambda event: (event.timestamp, event.pid, event.sequence))
    for event in events:
        if (
            event.syscall in ("read", "readv") and event.descriptor == 0
            and event.returned is not None and event.returned > 0
        ):
            require(event.pipe is not None, "solver fd 0 input is not a pipe")
        if (
            event.syscall in ("write", "writev") and event.descriptor == 1
            and event.returned is not None and event.returned > 0
        ):
            require(event.pipe is not None, "solver fd 1 output is not a pipe")
    stdin_pipes = {
        event.pipe for event in events
        if event.pid == solver_pid and event.descriptor == 0
        and event.syscall in ("read", "readv") and event.payload
    }
    stdout_pipes = {
        event.pipe for event in events
        if event.pid == solver_pid and event.descriptor == 1
        and event.syscall in ("write", "writev") and event.payload
    }
    require(len(stdin_pipes) == 1, f"solver stdin pipe topology {stdin_pipes} drifted")
    require(len(stdout_pipes) == 1, f"solver stdout pipe topology {stdout_pipes} drifted")
    require(stdin_pipes.isdisjoint(stdout_pipes), "solver stdin/stdout share one pipe")
    inbound = b"".join(
        event.payload for event in events
        if event.pid == solver_pid and event.syscall in ("read", "readv")
        and event.descriptor == 0 and event.pipe is not None and event.payload
    )
    outbound = b"".join(
        event.payload for event in events
        if event.pid == solver_pid and event.syscall in ("write", "writev")
        and event.descriptor == 1 and event.pipe is not None and event.payload
    )
    command_frames = split_framed_stream(inbound, ECHO_COMMAND_RE, "solver stdin")
    response_frames = split_framed_stream(outbound, ECHO_RESPONSE_RE, "solver stdout")
    require(len(command_frames) == len(response_frames), "command/response frame count differs")
    require(
        len({frame[0] for frame in command_frames}) == len(command_frames),
        "echo nonce was reused within one solver session",
    )
    require(
        [frame[0] for frame in command_frames] == [frame[0] for frame in response_frames],
        "command/response echo nonce order differs",
    )
    require(len(command_frames) >= 4, "capability handshake is incomplete")
    capability_commands = [frame[2] for frame in command_frames[:4]]
    capability_responses = [frame[2] for frame in response_frames[:4]]
    require(
        capability_commands[0] == b"(set-option :print-success false)\n",
        "startup capability frame drifted",
    )
    require(capability_commands[1] == CAPABILITY_PROGRAM_ZERO, "capability check frame drifted")
    require(
        capability_commands[2] == b"(get-value (djex_capability_input))\n",
        "capability value frame drifted",
    )
    require(
        capability_commands[3] == CAPABILITY_PROGRAM_CONTRADICTION,
        "capability contradiction frame drifted",
    )
    require(capability_responses[0] == b"", "startup echo response drifted")
    require(capability_responses[1] == b"sat\n", "capability sat response drifted")
    require(
        capability_responses[2] == b"((djex_capability_input 0))\n",
        "capability value response drifted",
    )
    require(capability_responses[3] == b"unsat\n", "capability unsat response drifted")
    transactions: list[dict[str, Any]] = []
    command_index = 4
    while command_index < len(command_frames):
        command_nonce, command_full, program = command_frames[command_index]
        response_nonce, response_full, status_bytes = response_frames[command_index]
        require(command_nonce == response_nonce, "query status nonce mismatch")
        status_match = re.fullmatch(rb"(sat|unsat|unknown)\r?\n", status_bytes)
        require(status_match is not None, "query status frame is not canonical")
        status = status_match.group(1)
        require(status != b"unknown", f"{workload.name} returned unknown")
        symbolic_case = validate_symbolic_program(workload, program)
        transaction: dict[str, Any] = {
            "program_sha256": sha256_bytes(NONCE_RE.sub(b"<NONCE>", command_full)),
            "status": status.decode(),
            "status_frame_sha256": sha256_bytes(
                NONCE_RE.sub(b"<NONCE>", response_full)
            ),
            "get_value_program_sha256": None,
            "value_frame_sha256": None,
            "symbolic_case": symbolic_case,
        }
        command_index += 1
        if status == b"sat":
            require(command_index < len(command_frames), "sat query lacks get-value frame")
            value_nonce, value_command_full, value_program = command_frames[command_index]
            value_response_nonce, value_response_full, value_bytes = response_frames[command_index]
            require(value_nonce == value_response_nonce, "query value nonce mismatch")
            expected_value_program = (
                b"(get-value (djex_length_input_0 djex_length_input_1))\n"
            )
            expected_value = (
                b"((djex_length_input_0 0)\n (djex_length_input_1 2))\n"
            )
            require(workload.name == "W2", "W1 unexpectedly requested a query model")
            require(value_program == expected_value_program, "sat query get-value frame drifted")
            require(value_bytes == expected_value, "sat query value response drifted")
            transaction["get_value_program_sha256"] = sha256_bytes(
                NONCE_RE.sub(b"<NONCE>", value_command_full)
            )
            transaction["value_frame_sha256"] = sha256_bytes(
                NONCE_RE.sub(b"<NONCE>", value_response_full)
            )
            command_index += 1
        transactions.append(transaction)
    require(
        len(transactions) == workload.assessments,
        f"{workload.name} query count {len(transactions)} != {workload.assessments}",
    )
    observed_sequence = [
        (item["symbolic_case"], item["status"]) for item in transactions
    ]
    require(
        observed_sequence == EXPECTED_QUERY_SEQUENCE[workload.name],
        f"{workload.name} ordered symbolic/status vector drifted",
    )
    accepted = sum(item["status"] == "unsat" for item in transactions)
    rejected = sum(item["status"] == "sat" for item in transactions)
    require(accepted == workload.accepted, f"{workload.name} accepted count {accepted} drifted")
    require(rejected == workload.rejected, f"{workload.name} rejected count {rejected} drifted")
    distinct_programs = len({item["program_sha256"] for item in transactions})
    require(
        distinct_programs == (4 if workload.name == "W2" else 1),
        f"{workload.name} distinct symbolic program count {distinct_programs} drifted",
    )
    vector = {
        "schema": "djex-select-best-query-vector/v1",
        "transactions": transactions,
    }
    return {
        "solver_pid": solver_pid,
        "solver_sessions": 1,
        "queries": workload.assessments,
        "accepted": accepted,
        "rejected": rejected,
        "distinct_programs": distinct_programs,
        "query_vector_sha256": sha256_bytes(canonical_json(vector)),
        "query_vector": vector,
        "trace_files": [path.name for path in files],
    }


@dataclass
class RunOutcome:
    row: dict[str, Any]
    transcript_key: tuple[int, bytes, bytes]
    query_vector: dict[str, Any] | None


class Screen:
    def __init__(self, arguments: argparse.Namespace, output: Path):
        self.arguments = arguments
        self.output = output
        self.raw = output / "raw"
        self.raw.mkdir()
        self.environment = output / "stable-empty-environment"
        self.results_path = output / "results.tsv"
        self.results_handle = self.results_path.open("w", newline="")
        self.writer = csv.DictWriter(self.results_handle, RESULT_COLUMNS, delimiter="\t", lineterminator="\n")
        self.writer.writeheader()
        self.results_handle.flush()
        os.fsync(self.results_handle.fileno())
        self.run_id = run_identifier(output)
        self.transcripts: dict[str, tuple[int, bytes, bytes]] = {}
        self.query_vectors: dict[str, dict[str, Any]] = {}
        self.rows: list[dict[str, Any]] = []
        self.binary = {
            "baseline": Path(arguments.baseline_binary).resolve(),
            "candidate": Path(arguments.candidate_binary).resolve(),
        }
        self.z3 = Path(arguments.z3).resolve()
        self.z3_sha256 = arguments.z3_sha256.lower()
        self.run_active = False
        self.active_process: subprocess.Popen[bytes] | None = None
        self.termination_requests: list[str] = []
        self.termination_delivery_errors: list[str] = []
        self.external_termination_requests: list[str] = []

    def close(self) -> None:
        if not self.results_handle.closed:
            self.results_handle.close()

    def request_termination(self, signum: int) -> None:
        self.termination_requests.append(signal.Signals(signum).name)
        process = self.active_process
        if process is None:
            return
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        except BaseException as failure:
            self.termination_delivery_errors.append(
                f"{type(failure).__name__}: {failure}"
            )

    def begin_run(self, publication_hook: Any | None = None) -> None:
        require(not self.run_active, "a benchmark invocation is already active")
        self.active_process = None
        self.termination_requests.clear()
        self.termination_delivery_errors.clear()
        # Publish last: once the signal handler can defer into this run, no
        # subsequent initialization may erase its termination request.
        self.run_active = True
        if publication_hook is not None:
            publication_hook()

    def append(self, outcome: RunOutcome) -> None:
        row = outcome.row
        self.writer.writerow({column: row.get(column, "") for column in RESULT_COLUMNS})
        self.results_handle.flush()
        os.fsync(self.results_handle.fileno())
        self.rows.append(row)
        expected = self.transcripts.setdefault(row["workload"], outcome.transcript_key)
        require(outcome.transcript_key == expected, f"{row['workload']} transcript differs in {row['artifact_dir']}")
        if outcome.query_vector is not None:
            query_expected = self.query_vectors.setdefault(row["workload"], outcome.query_vector)
            require(outcome.query_vector == query_expected, f"{row['workload']} query vector differs in {row['cell']}")

    def execute(
        self,
        phase: str,
        workload: Workload,
        cell: Cell,
        position: int,
        sample: int | None = None,
        williams_row: int | None = None,
    ) -> RunOutcome:
        parts = [phase, workload.name]
        if sample is not None:
            parts.append(f"sample-{sample:02d}")
        parts.append(f"pos-{position:02d}-{cell.name}")
        artifact = self.raw.joinpath(*parts)
        artifact.mkdir(parents=True)
        self.begin_run()
        private = artifact / "private"
        temporary = private / "tmp"
        home = private / "home"
        work = private / "work"
        stats_path = artifact / "rts.stats"
        stdout_path = artifact / "stdout.bin"
        stderr_path = artifact / "stderr.bin"
        input_bytes = b""
        trace_prefix: Path | None = None
        process: subprocess.Popen[bytes] | None = None
        sampler: ProcessTreeSampler | None = None
        communicated = False
        stdout = b""
        stderr = b""
        timed_out = False
        failures: list[str] = []
        started: int | None = None
        finished: int | None = None
        solver_images: dict[int, dict[str, Any]] = {}
        sampler_diagnostics: dict[str, Any] = {}
        process_tree_sha256 = ""
        try:
            require(
                not self.external_termination_requests,
                "outer termination requested before invocation: "
                f"{self.external_termination_requests}",
            )
            require(
                not self.environment.exists(),
                f"stable empty environment survived a prior run: {self.environment}",
            )
            self.environment.mkdir()
            for path in (
                temporary,
                home,
                work,
                home / ".config",
                home / ".cache",
                home / ".local/share",
            ):
                path.mkdir(parents=True, exist_ok=True)
            input_bytes = render_input(
                workload, cell.jobs, self.z3, self.z3_sha256
            )
            (artifact / "input.repl").write_bytes(input_bytes)
            executable = self.binary[cell.revision]
            command = [
                str(executable),
                "repl",
                "--environment",
                str(self.environment),
                "--ignore-startup",
                "+RTS",
                f"-N{cell.capabilities}",
                f"-s{stats_path}",
                "-RTS",
            ]
            if phase == "preflight":
                trace_prefix = artifact / "strace"
                command = [
                    "/usr/bin/strace",
                    "-ff",
                    "-ttt",
                    "-T",
                    "-yy",
                    "-s",
                    "4096",
                    "-o",
                    str(trace_prefix),
                    "-e",
                    "trace=clone,clone3,fork,vfork,execve,execveat,wait4,"
                    "waitid,exit,exit_group,kill,read,write,readv,writev,close",
                    *command,
                ]
            child_environment = {
                "HOME": str(home),
                "LANG": "C.UTF-8",
                "LC_ALL": "C.UTF-8",
                "PATH": "/usr/bin:/bin",
                "TERM": "dumb",
                "TMPDIR": str(temporary),
                "TZ": "UTC",
                "XDG_CACHE_HOME": str(home / ".cache"),
                "XDG_CONFIG_HOME": str(home / ".config"),
                "XDG_DATA_HOME": str(home / ".local/share"),
            }
            (artifact / "command.json").write_bytes(
                canonical_json({"argv": command, "env": child_environment})
            )
            require(
                not self.termination_requests,
                f"termination requested during setup: {self.termination_requests}",
            )
            started = time.monotonic_ns()
            process = subprocess.Popen(
                command, cwd=work, env=child_environment, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                start_new_session=True,
            )
            self.active_process = process
            require(
                not self.termination_requests,
                f"termination requested during launch: {self.termination_requests}",
            )
            sampler = ProcessTreeSampler(
                process.pid, self.z3_sha256, str(self.z3),
                self.arguments.sample_interval_ms / 1000.0,
            )
            sampler.start()
            try:
                stdout, stderr = process.communicate(
                    input=input_bytes, timeout=self.arguments.outer_timeout
                )
                finished = time.monotonic_ns()
                communicated = True
            except subprocess.TimeoutExpired:
                timed_out = True
                terminate_owned_processes(
                    process.pid, sampler.identity_snapshot()
                )
                try:
                    stdout, stderr = process.communicate(timeout=5)
                    finished = time.monotonic_ns()
                    communicated = True
                except subprocess.TimeoutExpired:
                    failures.append("pipes remained open after timeout cleanup")
        except BaseException as failure:
            failures.append(f"execution failure: {type(failure).__name__}: {failure}")
        finally:
            try:
                if process is not None and process.poll() is None:
                    live_identities = (
                        sampler.identity_snapshot() if sampler is not None else {}
                    )
                    terminate_owned_processes(process.pid, live_identities)
            except BaseException as failure:
                failures.append(
                    f"initial process cleanup failure: {type(failure).__name__}: {failure}"
                )
            if process is not None and not communicated:
                try:
                    recovered_stdout, recovered_stderr = process.communicate(timeout=5)
                    stdout += recovered_stdout
                    stderr += recovered_stderr
                    if finished is None:
                        finished = time.monotonic_ns()
                    communicated = True
                except BaseException as failure:
                    failures.append(
                        f"subprocess collection failure: {type(failure).__name__}: {failure}"
                    )
                    for handle in (process.stdin, process.stdout, process.stderr):
                        if handle is not None:
                            try:
                                handle.close()
                            except OSError:
                                pass
            if sampler is not None and sampler.thread.ident is not None:
                try:
                    sampler.stop()
                except BaseException as failure:
                    failures.append(f"sampler failure: {type(failure).__name__}: {failure}")
                try:
                    solver_images = sampler.solver_snapshot()
                except BaseException as failure:
                    failures.append(
                        "solver image preservation failure: "
                        f"{type(failure).__name__}: {failure}"
                    )
                try:
                    sampler_diagnostics = sampler.diagnostics()
                except BaseException as failure:
                    failures.append(
                        f"sampler diagnostics failure: {type(failure).__name__}: {failure}"
                    )
            try:
                identities = (
                    sampler.identity_snapshot() if sampler is not None else {}
                )
            except BaseException as failure:
                identities = {}
                failures.append(
                    f"process identity snapshot failure: {type(failure).__name__}: {failure}"
                )
            group = process.pid if process is not None else -1
            try:
                lingering_group = (
                    process_group_members(group) if process is not None else []
                )
                lingering_known = matching_processes(identities)
            except BaseException as failure:
                lingering_group = []
                lingering_known = []
                failures.append(
                    f"descendant audit failure: {type(failure).__name__}: {failure}"
                )
            if process is not None and (lingering_group or lingering_known):
                try:
                    terminate_owned_processes(group, identities)
                except BaseException as failure:
                    failures.append(
                        f"descendant termination failure: "
                        f"{type(failure).__name__}: {failure}"
                    )
            try:
                still_group = (
                    process_group_members(group) if process is not None else []
                )
                still_known = matching_processes(identities)
            except BaseException as failure:
                still_group = []
                still_known = []
                failures.append(
                    f"post-cleanup descendant audit failure: "
                    f"{type(failure).__name__}: {failure}"
                )
            if lingering_group or lingering_known:
                failures.append(
                    "descendants survived normal exit: "
                    f"group={lingering_group}, known={lingering_known}"
                )
            if still_group or still_known:
                failures.append(
                    f"descendant cleanup incomplete: group={still_group}, known={still_known}"
                )
            try:
                stdout_path.write_bytes(stdout)
                stderr_path.write_bytes(stderr)
            except BaseException as failure:
                failures.append(f"cannot preserve transcript: {failure}")
            try:
                unexpected_private = non_directory_residue(private)
            except BaseException as failure:
                unexpected_private = [f"<cannot inspect private tree: {failure}>"]
            try:
                unexpected_environment = non_directory_residue(self.environment)
            except BaseException as failure:
                unexpected_environment = [
                    f"<cannot inspect stable environment: {failure}>"
                ]
            try:
                (artifact / "private-residue.json").write_bytes(
                    canonical_json({
                        "private": unexpected_private,
                        "stable_environment": unexpected_environment,
                    })
                )
            except BaseException as failure:
                failures.append(f"cannot preserve private residue manifest: {failure}")
            if private.exists() or private.is_symlink():
                try:
                    shutil.rmtree(private)
                except BaseException as failure:
                    failures.append(f"cannot remove private tree: {failure}")
                    try:
                        if private.exists() or private.is_symlink():
                            shutil.rmtree(private)
                    except BaseException as retry_failure:
                        failures.append(
                            f"cannot retry private cleanup: {retry_failure}"
                        )
            if self.environment.exists() or self.environment.is_symlink():
                try:
                    shutil.rmtree(self.environment)
                except BaseException as failure:
                    failures.append(f"cannot remove stable environment: {failure}")
                    try:
                        if self.environment.exists() or self.environment.is_symlink():
                            shutil.rmtree(self.environment)
                    except BaseException as retry_failure:
                        failures.append(
                            f"cannot retry stable environment cleanup: {retry_failure}"
                        )
            if unexpected_private:
                failures.append(f"unexpected private files: {unexpected_private}")
            if unexpected_environment:
                failures.append(
                    f"unexpected environment files: {unexpected_environment}"
                )
            if private.exists() or private.is_symlink():
                failures.append("private tree still exists after cleanup")
            if self.environment.exists() or self.environment.is_symlink():
                failures.append("stable environment still exists after cleanup")
            if sampler is not None:
                try:
                    cpu_ns, peak_rss_bytes, solver_sessions = sampler.metrics()
                    process_tree = {
                        "schema": "djex-benchmark-process-tree/v1",
                        "root_pid": process.pid if process is not None else None,
                        "cpu_ns": cpu_ns,
                        "peak_rss_bytes": peak_rss_bytes,
                        "solver_sessions": solver_sessions,
                        "wall_started_ns": started,
                        "wall_finished_ns": finished,
                        "sampler": sampler_diagnostics,
                        "solver_images": {
                            str(pid): value for pid, value in solver_images.items()
                        },
                    }
                    process_tree_path = artifact / "process-tree.json"
                    process_tree_path.write_bytes(canonical_json(process_tree))
                    process_tree_sha256 = sha256_file(process_tree_path)
                except BaseException as failure:
                    failures.append(
                        "process tree preservation failure: "
                        f"{type(failure).__name__}: {failure}"
                    )
            self.active_process = None
            self.run_active = False
        require(
            not self.termination_requests,
            f"catchable termination requested: {self.termination_requests}; "
            f"delivery_errors={self.termination_delivery_errors}",
        )
        require(not timed_out, f"outer safety timeout in {artifact.relative_to(self.output)}")
        require(not failures, "; ".join(failures))
        require(process is not None and process.returncode is not None, "subprocess returned no status")
        require(sampler is not None, "process sampler was not constructed")
        require(started is not None and finished is not None, "subprocess wall endpoints are incomplete")
        cpu_ns, peak_rss_bytes, solver_sessions = sampler.metrics()
        sampling_quality = validate_sampler_quality(
            sampler_diagnostics,
            started,
            finished,
            self.arguments.sample_interval_ms,
        )
        require(len(solver_images) == 1, f"observed {len(solver_images)} sealed solver images")
        solver_pid, solver_image = next(iter(solver_images.items()))
        require(
            sampler.solver_sha256 == self.z3_sha256
            and solver_image.get("sha256") == sampler.solver_sha256,
            f"sealed solver image SHA-256 {solver_image.get('sha256')} "
            f"!= {sampler.solver_sha256}",
        )
        require(
            sampler_diagnostics.get("start_times", {}).get(solver_pid)
            == solver_image.get("start_time"),
            "sealed solver PID/start-time identity drifted",
        )
        require(stats_path.is_file(), f"missing RTS statistics in {artifact}")
        stats = stats_path.read_bytes()
        allocated = parse_allocated_bytes(stats)
        semantic = validate_transcript(workload, process.returncode, stdout, stderr)
        require(solver_sessions == 1, f"observed {solver_sessions} Z3 sessions in {artifact}")
        trace = parse_trace(trace_prefix, workload, solver_pid) if trace_prefix is not None else None
        row = {
            "schema": ROW_SCHEMA,
            "run_id": self.run_id,
            "phase": phase,
            "workload": workload.name,
            "sample": sample if sample is not None else "",
            "williams_row": williams_row if williams_row is not None else "",
            "position": position,
            "cell": cell.name,
            "revision": cell.revision,
            "commit": cell.commit,
            "jobs": cell.jobs,
            "capabilities": cell.capabilities,
            "input_sha256": sha256_bytes(input_bytes),
            "wall_ns": finished - started,
            "cpu_ns": cpu_ns,
            "peak_rss_bytes": peak_rss_bytes,
            "allocated_bytes": allocated,
            "exit_code": process.returncode,
            "stdout_sha256": sha256_bytes(stdout),
            "stderr_sha256": sha256_bytes(stderr),
            "transcript_sha256": framed_transcript(process.returncode, stdout, stderr),
            "semantic_sha256": semantic["semantic_sha256"],
            "stats_sha256": sha256_bytes(stats),
            "rendered_candidates": semantic["rendered"],
            "truncation_count": semantic["truncations"],
            "solver_sessions_observed": solver_sessions,
            "solver_queries_observed": trace["queries"] if trace else "",
            "solver_accepted_observed": trace["accepted"] if trace else "",
            "solver_rejected_observed": trace["rejected"] if trace else "",
            "solver_image_sha256": solver_image["sha256"],
            "query_vector_sha256": trace["query_vector_sha256"] if trace else "",
            "sampler_samples": sampler_diagnostics["sample_count"],
            "sampler_span_ns": sampler_diagnostics["span_ns"],
            "sampler_initial_delay_ns": sampling_quality["initial_delay_ns"],
            "sampler_terminal_gap_ns": sampling_quality["terminal_gap_ns"],
            "sampler_coverage_ratio": sampling_quality["coverage_ratio"],
            "sampler_mean_interval_ns": sampler_diagnostics["mean_interval_ns"],
            "sampler_max_interval_ns": sampler_diagnostics["max_interval_ns"],
            "sampler_max_pass_ns": sampler_diagnostics["max_pass_ns"],
            "process_tree_sha256": process_tree_sha256,
            "cleanup_ok": "true",
            "artifact_dir": str(artifact.relative_to(self.output)),
        }
        if trace is not None:
            (artifact / "query-vector.json").write_bytes(canonical_json(trace))
        return RunOutcome(row, (process.returncode, stdout, stderr), trace["query_vector"] if trace else None)


def measured_schedule() -> list[tuple[int, str, int, tuple[str, ...]]]:
    schedule = []
    for sample in range(1, 9):
        workload_order = ("W1", "W2") if sample % 2 else ("W2", "W1")
        for workload_name in workload_order:
            row_index = sample - 1 if workload_name == "W1" else sample % 8
            schedule.append((sample, workload_name, row_index + 1, WILLIAMS_ROWS[row_index]))
    return schedule


def percentile_nearest_rank(values: Sequence[float], fraction: float) -> float:
    require(bool(values), "percentile of empty samples")
    ordered = sorted(values)
    index = max(1, math.ceil(fraction * len(ordered))) - 1
    return ordered[index]


def median_metric(rows: Sequence[dict[str, Any]], workload: str, cell: str, metric: str) -> float:
    values = [float(row[metric]) for row in rows if row["phase"] == "measured" and row["workload"] == workload and row["cell"] == cell]
    require(len(values) == 8, f"{workload}/{cell}/{metric} has {len(values)} samples")
    return statistics.median(values)


def p95_metric(rows: Sequence[dict[str, Any]], workload: str, cell: str, metric: str) -> float:
    values = [float(row[metric]) for row in rows if row["phase"] == "measured" and row["workload"] == workload and row["cell"] == cell]
    require(len(values) == 8, f"{workload}/{cell}/{metric} has {len(values)} samples")
    return percentile_nearest_rank(values, 0.95)


def ratio(numerator: float, denominator: float, label: str) -> float:
    require(numerator > 0 and denominator > 0, f"non-positive metric in {label}")
    return numerator / denominator


def analyze(rows: Sequence[dict[str, Any]]) -> dict[str, Any]:
    workload_reports: dict[str, Any] = {}
    gates: list[dict[str, Any]] = []

    def gate(name: str, passed: bool, observed: Any, requirement: str) -> None:
        gates.append({"name": name, "pass": bool(passed), "observed": observed, "requirement": requirement})

    for workload in WORKLOADS:
        wall = {cell: median_metric(rows, workload, cell, "wall_ns") for cell in CELLS}
        wall_p95 = {cell: p95_metric(rows, workload, cell, "wall_ns") for cell in CELLS}
        metrics = {
            "pipeline_n2_F_over_H": ratio(wall["F"], wall["H"], "F/H"),
            "pipeline_n1_E_over_G": ratio(wall["E"], wall["G"], "E/G"),
            "canonical_shipped_B_over_H": ratio(wall["B"], wall["H"], "B/H"),
            "matched_shipped_D_over_H": ratio(wall["D"], wall["H"], "D/H"),
            "diff_in_diff": ratio(
                ratio(wall["F"], wall["H"], "F/H"),
                ratio(wall["B"], wall["D"], "B/D"), "diff-in-diff",
            ),
            "route_n1_cost_G_over_E": ratio(wall["G"], wall["E"], "G/E"),
            "route_n1_p95_cost_G_over_E": ratio(wall_p95["G"], wall_p95["E"], "G/E p95"),
            "serial_drift_E_over_A": ratio(wall["E"], wall["A"], "E/A"),
            "serial_drift_F_over_B": ratio(wall["F"], wall["B"], "F/B"),
            "baseline_jobs_C_over_A": ratio(wall["C"], wall["A"], "C/A"),
            "baseline_jobs_D_over_B": ratio(wall["D"], wall["B"], "D/B"),
            "H_p95_over_F": ratio(wall_p95["H"], wall_p95["F"], "H/F p95"),
            "H_p95_over_D": ratio(wall_p95["H"], wall_p95["D"], "H/D p95"),
        }
        for name in ("pipeline_n2_F_over_H", "canonical_shipped_B_over_H", "matched_shipped_D_over_H", "diff_in_diff"):
            gate(f"{workload}.{name}", metrics[name] > 1.0, metrics[name], "> 1.0")
        for name in ("route_n1_cost_G_over_E", "route_n1_p95_cost_G_over_E", "H_p95_over_F", "H_p95_over_D"):
            gate(f"{workload}.{name}", metrics[name] <= 1.05, metrics[name], "<= 1.05")
        for name in ("serial_drift_E_over_A", "serial_drift_F_over_B", "baseline_jobs_C_over_A", "baseline_jobs_D_over_B"):
            gate(f"{workload}.{name}", 0.95 <= metrics[name] <= 1.05, metrics[name], "0.95 <= ratio <= 1.05")
        resource_ratios: dict[str, dict[str, float]] = {}
        for metric, maximum in (("allocated_bytes", 1.10), ("cpu_ns", 1.25), ("peak_rss_bytes", 1.25)):
            medians = {cell: median_metric(rows, workload, cell, metric) for cell in CELLS}
            comparisons = {
                "G_over_E": ratio(medians["G"], medians["E"], f"{metric} G/E"),
                "H_over_F": ratio(medians["H"], medians["F"], f"{metric} H/F"),
                "G_over_C": ratio(medians["G"], medians["C"], f"{metric} G/C"),
                "H_over_D": ratio(medians["H"], medians["D"], f"{metric} H/D"),
            }
            resource_ratios[metric] = comparisons
            for name, value in comparisons.items():
                gate(f"{workload}.{metric}.{name}", value <= maximum, value, f"<= {maximum}")
        workload_reports[workload] = {
            "median_wall_ns": wall,
            "p95_wall_ns": wall_p95,
            "ratios": metrics,
            "resource_ratios": resource_ratios,
        }
    pipeline_geomean = math.exp(statistics.mean(math.log(workload_reports[name]["ratios"]["pipeline_n2_F_over_H"]) for name in WORKLOADS))
    shipped_geomean = math.exp(statistics.mean(math.log(workload_reports[name]["ratios"]["canonical_shipped_B_over_H"]) for name in WORKLOADS))
    gate("geomean.pipeline_n2_F_over_H", pipeline_geomean > 1.10, pipeline_geomean, "> 1.10")
    gate("geomean.canonical_shipped_B_over_H", shipped_geomean > 1.10, shipped_geomean, "> 1.10")
    strong_tier = all(
        workload_reports[name]["ratios"]["pipeline_n2_F_over_H"] >= 1.25
        and workload_reports[name]["ratios"]["canonical_shipped_B_over_H"] >= 1.25
        for name in WORKLOADS
    )
    keep = all(item["pass"] for item in gates)
    return {
        "schema": "djex-select-best-candidate-pipeline-decision/v1",
        "verdict": "KEEP" if keep else "HOLD",
        "keep_threshold": "> 1.10 geomean with all fail-closed controls",
        "pipeline_geomean": pipeline_geomean,
        "canonical_shipped_geomean": shipped_geomean,
        "meaningful_over_1_10": keep and pipeline_geomean > 1.10,
        "strong_1_25_tier": strong_tier,
        "workloads": workload_reports,
        "gates": gates,
    }


def collect_provenance(arguments: argparse.Namespace) -> dict[str, Any]:
    baseline_sha = require_sha256(arguments.baseline_binary_sha256, "baseline binary hash")
    candidate_sha = require_sha256(arguments.candidate_binary_sha256, "candidate binary hash")
    z3_sha = require_sha256(arguments.z3_sha256, "Z3 hash")
    require(baseline_sha == BASELINE_BINARY_SHA256, "baseline binary hash differs from frozen hash")
    require(candidate_sha == CANDIDATE_BINARY_SHA256, "candidate binary hash differs from frozen hash")
    require(Path(arguments.z3).resolve() == PINNED_Z3.resolve(), "Z3 path differs from the frozen path")
    require(z3_sha == PINNED_Z3_SHA256, "Z3 hash differs from the frozen hash")
    tools = tool_identities(arguments.strace)
    frozen_artifacts: dict[str, Any] = {}
    for relative, expected in FROZEN_ARTIFACT_SHA256.items():
        require_sha256(expected, f"frozen {relative} hash")
        path = SCRIPT_DIR / relative
        actual = sha256_file(path)
        require(actual == expected, f"frozen artifact SHA-256 drifted for {relative}: {actual}")
        frozen_artifacts[relative] = {"path": str(path), "sha256": actual}
    templates = {
        name: {
            "path": workload.template,
            "sha256": sha256_file(SCRIPT_DIR / workload.template),
            "assessments": workload.assessments,
            "accepted": workload.accepted,
            "rejected": workload.rejected,
            "rendered": workload.rendered,
        }
        for name, workload in WORKLOADS.items()
    }
    baseline_root = Path(arguments.baseline_root).resolve()
    candidate_root = Path(arguments.candidate_root).resolve()
    baseline_binary = Path(arguments.baseline_binary).resolve()
    candidate_binary = Path(arguments.candidate_binary).resolve()
    baseline_plan = build_plan_identity(
        baseline_root, baseline_binary, BASELINE_PLAN_SHA256, "baseline"
    )
    candidate_plan = build_plan_identity(
        candidate_root, candidate_binary, CANDIDATE_PLAN_SHA256, "candidate"
    )
    require(
        baseline_plan["normalized"] == candidate_plan["normalized"],
        "baseline/candidate normalized build plans differ",
    )
    return {
        "schema": SCHEMA,
        "created_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "protocol_source": verify_protocol_repository(),
        "baseline_source": verify_source_root(baseline_root, BASELINE_COMMIT, "baseline"),
        "candidate_source": verify_source_root(candidate_root, CANDIDATE_COMMIT, "candidate"),
        "baseline_build_plan": baseline_plan,
        "candidate_build_plan": candidate_plan,
        "normalized_build_plan_equivalence": baseline_plan["normalized"],
        "baseline_binary": executable_identity(
            baseline_binary, baseline_sha, "baseline binary",
            expected_build_id=BASELINE_BUILD_ID,
            expected_library_hashes=PINNED_DJEX_LIBRARY_SHA256,
        ),
        "candidate_binary": executable_identity(
            candidate_binary, candidate_sha, "candidate binary",
            expected_build_id=CANDIDATE_BUILD_ID,
            expected_library_hashes=PINNED_DJEX_LIBRARY_SHA256,
        ),
        "o2_attestation": o2_attestation_identity(
            baseline_root, candidate_root
        ),
        "z3": executable_identity(
            Path(arguments.z3), z3_sha, "Z3",
            expected_library_hashes=PINNED_Z3_LIBRARY_SHA256,
        ),
        "z3_package": z3_package_identity(),
        "tools": tools,
        "python": python_identity(),
        "runner": {"path": str(Path(__file__).resolve()), "sha256": sha256_file(Path(__file__).resolve())},
        "readme": {
            "path": str(SCRIPT_DIR / "README.md"),
            "sha256": sha256_file(SCRIPT_DIR / "README.md"),
        },
        "result_schema": frozen_artifacts["result-schema.tsv"],
        "frozen_artifacts": frozen_artifacts,
        "templates": templates,
        "host": host_identity(),
        "sample_interval_ms": arguments.sample_interval_ms,
        "sampler_quality_gates": {
            "minimum_wall_coverage": SAMPLER_MIN_WALL_COVERAGE,
            "maximum_mean_interval_multiplier": (
                SAMPLER_MAX_MEAN_INTERVAL_MULTIPLIER
            ),
            "maximum_interval_multiplier": SAMPLER_MAX_INTERVAL_MULTIPLIER,
            "maximum_pass_multiplier": SAMPLER_MAX_PASS_MULTIPLIER,
        },
        "outer_timeout_seconds": arguments.outer_timeout,
        "notes": arguments.notes,
    }


def verify_frozen_artifacts(provenance: dict[str, Any]) -> None:
    for key in ("baseline_binary", "candidate_binary", "z3"):
        identity = provenance[key]
        require(sha256_file(Path(identity["path"])) == identity["sha256"], f"{key} changed during screen")
        stat = Path(identity["path"]).stat()
        require((stat.st_dev, stat.st_ino) == (identity["device"], identity["inode"]), f"{key} inode changed during screen")
        for library in identity["dynamic_libraries"]:
            require(
                sha256_file(Path(library["path"])) == library["sha256"],
                f"{key} dynamic library changed during screen: {library['path']}",
            )
    require(
        verify_protocol_repository() == provenance["protocol_source"],
        "protocol repository changed during screen",
    )
    require(
        verify_source_root(
            Path(provenance["baseline_source"]["root"]), BASELINE_COMMIT,
            "baseline",
        ) == provenance["baseline_source"],
        "baseline source changed during screen",
    )
    require(
        verify_source_root(
            Path(provenance["candidate_source"]["root"]), CANDIDATE_COMMIT,
            "candidate",
        ) == provenance["candidate_source"],
        "candidate source changed during screen",
    )
    for label, expected_sha in (
        ("baseline", BASELINE_PLAN_SHA256),
        ("candidate", CANDIDATE_PLAN_SHA256),
    ):
        identity = provenance[f"{label}_build_plan"]
        actual_identity = build_plan_identity(
            Path(provenance[f"{label}_source"]["root"]),
            Path(identity["expected_binary"]),
            expected_sha,
            label,
        )
        require(
            actual_identity == identity,
            f"{label} build plan identity changed during screen",
        )
    require(
        provenance["baseline_build_plan"]["normalized"]
        == provenance["candidate_build_plan"]["normalized"]
        == provenance["normalized_build_plan_equivalence"],
        "normalized build-plan equivalence changed during screen",
    )
    require(
        o2_attestation_identity(
            Path(provenance["baseline_source"]["root"]),
            Path(provenance["candidate_source"]["root"]),
        ) == provenance["o2_attestation"],
        "O2 attestation changed during screen",
    )
    require(
        z3_package_identity() == provenance["z3_package"],
        "Z3 package changed during screen",
    )
    require(
        tool_identities("/usr/bin/strace") == provenance["tools"],
        "tool identity changed during screen",
    )
    require(
        python_identity() == provenance["python"],
        "Python interpreter identity changed during screen",
    )
    for name, workload in WORKLOADS.items():
        require(sha256_file(SCRIPT_DIR / workload.template) == provenance["templates"][name]["sha256"], f"{name} template changed during screen")
    for relative, identity in provenance["frozen_artifacts"].items():
        require(
            sha256_file(SCRIPT_DIR / relative) == identity["sha256"],
            f"frozen artifact changed during screen: {relative}",
        )
    require(
        sha256_file(SCRIPT_DIR / "README.md") == provenance["readme"]["sha256"],
        "benchmark README changed during screen",
    )
    require(sha256_file(Path(__file__).resolve()) == provenance["runner"]["sha256"], "runner changed during screen")


def run_screen(arguments: argparse.Namespace) -> int:
    output = Path(arguments.output).resolve()
    require(not output.exists(), f"one-shot output already exists: {output}")
    screen: Screen | None = None
    provenance: dict[str, Any] | None = None
    analyzed_decision: dict[str, Any] | None = None
    primary_failure: str | None = None
    finalization_failures: list[str] = []
    host_start: dict[str, Any] | None = None
    host_start_sha256: str | None = None
    host_start_attestation_sha256: str | None = None
    host_end_sha256: str | None = None
    host_attestation_sha256: str | None = None
    termination = TerminationCoordinator()
    handled_signals = (
        signal.SIGHUP, signal.SIGINT, signal.SIGTERM, signal.SIGQUIT
    )
    previous_handlers = {
        value: signal.getsignal(value) for value in handled_signals
    }
    installed_signals: list[signal.Signals] = []

    def termination_requested(signum: int, _frame: Any) -> None:
        termination.record(signum, _frame)

    try:
        try:
            for value in handled_signals:
                signal.signal(value, termination_requested)
                installed_signals.append(value)
            output.mkdir(parents=True)
            provenance = collect_provenance(arguments)
            write_json(output / "schedule.json", {
                "schema": "djex-select-best-candidate-pipeline-schedule/v1",
                "preflight": {
                    name: list(WILLIAMS_ROWS[index])
                    for index, name in enumerate(("W1", "W2"))
                },
                "warmup": {
                    "W1": list(WILLIAMS_ROWS[0]),
                    "W2": list(WILLIAMS_ROWS[1]),
                },
                "measured": [
                    {
                        "sample": sample, "workload": workload, "row": row,
                        "order": list(order),
                    }
                    for sample, workload, row, order in measured_schedule()
                ],
            })
            screen = Screen(arguments, output)
            termination.bind_screen(screen)
            screen.external_termination_requests = termination.requests
            host_start = host_control_snapshot()
            start_path = output / "host-control-start.json"
            write_json(start_path, host_start)
            host_start_sha256 = sha256_file(start_path)
            start_attestation = attest_host_control_start(host_start)
            start_attestation_path = output / "host-control-start-attestation.json"
            write_json(start_attestation_path, start_attestation)
            host_start_attestation_sha256 = sha256_file(start_attestation_path)
            provenance["host_control"] = {
                "policy": {
                    "affinity": "exactly inherited effective CPU cpuset; at least two CPUs",
                    "stable": [
                        "unified membership", "cgroup2 mount identity",
                        "ancestor topology", "sched affinity",
                        "cpuset.cpus.effective presence/content",
                        "cpuset.mems.effective presence/content",
                        "cpu.max presence/content", "cpu.stat field presence",
                    ],
                    "throttling": (
                        "exact zero nr_throttled and throttled_usec deltas at "
                        "every exposing ancestor"
                    ),
                    "diagnostic_only": ["loadavg", "cpu/memory/io PSI"],
                },
                "start": {
                    "path": str(start_path),
                    "sha256": host_start_sha256,
                },
                "start_attestation": {
                    "path": str(start_attestation_path),
                    "sha256": host_start_attestation_sha256,
                },
            }
            write_json(output / "provenance.json", provenance)
            require(
                start_attestation["verdict"] == "PASS",
                "host-control start attestation failed",
            )
            for workload_name, row_index in (("W1", 0), ("W2", 1)):
                workload = WORKLOADS[workload_name]
                for position, cell_name in enumerate(WILLIAMS_ROWS[row_index], 1):
                    screen.append(screen.execute(
                        "preflight", workload, CELLS[cell_name], position,
                    ))
            for workload_name, row_index in (("W1", 0), ("W2", 1)):
                workload = WORKLOADS[workload_name]
                for position, cell_name in enumerate(WILLIAMS_ROWS[row_index], 1):
                    screen.append(screen.execute(
                        "warmup", workload, CELLS[cell_name], position,
                    ))
            for sample, workload_name, row_number, order in measured_schedule():
                workload = WORKLOADS[workload_name]
                for position, cell_name in enumerate(order, 1):
                    screen.append(screen.execute(
                        "measured", workload, CELLS[cell_name], position,
                        sample=sample, williams_row=row_number,
                    ))
            screen.close()
            verify_frozen_artifacts(provenance)
            analyzed_decision = analyze(screen.rows)
        except BaseException as failure:
            primary_failure = f"{type(failure).__name__}: {failure}"
        finally:
            termination.publish_finalization()
            if screen is not None:
                try:
                    screen.close()
                except BaseException as failure:
                    finalization_failures.append(
                        f"results close failure: {type(failure).__name__}: {failure}"
                    )
            if host_start is not None:
                host_finalization = finalize_host_control_evidence(
                    output,
                    host_start,
                    host_start_sha256,
                    host_start_attestation_sha256,
                )
                host_end_sha256 = host_finalization["end_sha256"]
                host_attestation_sha256 = host_finalization[
                    "attestation_sha256"
                ]
                if host_finalization["failure"] is not None:
                    finalization_failures.append(host_finalization["failure"])
        evidence_hash_failures: list[str] = []

        def optional_evidence_hash(path: Path) -> str | None:
            if not path.is_file():
                return None
            try:
                return sha256_file(path)
            except BaseException as failure:
                evidence_hash_failures.append(
                    f"cannot hash evidence {path.name}: "
                    f"{type(failure).__name__}: {failure}"
                )
                return None

        results_sha256 = optional_evidence_hash(
            screen.results_path if screen is not None
            else output / "results.tsv"
        )
        provenance_sha256 = optional_evidence_hash(output / "provenance.json")
        schedule_sha256 = optional_evidence_hash(output / "schedule.json")
        finalization_failures.extend(evidence_hash_failures)
        failures = ([primary_failure] if primary_failure is not None else []) + finalization_failures
        common = {
            "run_id": screen.run_id if screen is not None else run_identifier(output),
            "completed_rows": len(screen.rows) if screen is not None else 0,
            "results_sha256": results_sha256,
            "provenance_sha256": provenance_sha256,
            "schedule_sha256": schedule_sha256,
            "host_control_start_sha256": host_start_sha256,
            "host_control_start_attestation_sha256": (
                host_start_attestation_sha256
            ),
            "host_control_end_sha256": host_end_sha256,
            "host_control_attestation_sha256": host_attestation_sha256,
            "completed_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        }
        if failures or analyzed_decision is None:
            decision = {
                "schema": "djex-select-best-candidate-pipeline-decision/v1",
                "verdict": "HOLD",
                **common,
                "primary_failure": (
                    failures[0] if failures
                    else "internal failure: screen produced no decision"
                ),
                "finalization_failures": failures[1:] if failures else [],
            }
        else:
            decision = analyzed_decision
            decision.update(common)
        outcome_requests = termination.block_for_outcome_commit(
            handled_signals,
        )
        decision = apply_termination_veto(decision, outcome_requests)
        decision["outcome_commit"] = {
            "monotonic_ns": termination.outcome_commit_monotonic_ns,
            "signals_blocked_through_process_exit": [
                value.name for value in handled_signals
            ],
        }
        decision_path = output / "decision.json"
        if output.is_dir():
            write_json(decision_path, decision)
        destination = sys.stdout if decision["verdict"] == "KEEP" else sys.stderr
        print(json.dumps(decision, indent=2, sort_keys=True), file=destination)
        return 0 if decision["verdict"] == "KEEP" else 2
    finally:
        for value in reversed(installed_signals):
            handler = previous_handlers[value]
            try:
                signal.signal(value, handler)
            except BaseException as failure:
                print(
                    f"benchmark signal-handler restoration failure: {failure}",
                    file=sys.stderr,
                )


def synthetic_rows() -> list[dict[str, Any]]:
    wall = {
        "A": 120, "B": 120, "C": 120, "D": 120,
        "E": 120, "F": 116, "G": 120, "H": 100,
    }
    rows = []
    for workload in WORKLOADS:
        for cell in CELLS:
            for sample in range(1, 9):
                rows.append({
                    "phase": "measured", "workload": workload, "cell": cell,
                    "wall_ns": wall[cell], "allocated_bytes": 100,
                    "cpu_ns": 100, "peak_rss_bytes": 100,
                })
    return rows


def strace_string(value: bytes) -> str:
    return json.dumps(value.decode("ascii"))


def check_synthetic_trace_parser() -> None:
    workload = WORKLOADS["W2"]
    commands: list[bytes] = []
    responses: list[bytes] = []

    def append_frame(command: bytes, response: bytes) -> None:
        nonce = f"{len(commands) + 1:064x}".encode()
        commands.append(
            command + b'(echo "djex-smtlib-frame/v1/' + nonce + b'")\n'
        )
        responses.append(
            response + b'"djex-smtlib-frame/v1/' + nonce + b'"\n'
        )

    append_frame(b"(set-option :print-success false)\n", b"")
    append_frame(CAPABILITY_PROGRAM_ZERO, b"sat\n")
    append_frame(
        b"(get-value (djex_capability_input))\n",
        b"((djex_capability_input 0))\n",
    )
    append_frame(CAPABILITY_PROGRAM_CONTRADICTION, b"unsat\n")
    value_program = b"(get-value (djex_length_input_0 djex_length_input_1))\n"
    value_response = b"((djex_length_input_0 0)\n (djex_length_input_1 2))\n"
    for symbolic_case, status in EXPECTED_QUERY_SEQUENCE["W2"]:
        append_frame(W2_CASES[symbolic_case], status.encode() + b"\n")
        if status == "sat":
            append_frame(value_program, value_response)

    inbound = b"".join(commands)
    outbound = b"".join(responses)
    with tempfile.TemporaryDirectory(prefix="djex-pipeline-trace-self-check-") as source:
        root = Path(source)
        prefix = root / "strace"
        solver_pid = 222
        executable_arguments = ", ".join(
            strace_string(value)
            for value in (
                str(PINNED_Z3).encode(),
                b"-in",
                b"-smt2",
                b"smtlib2_compliant=true",
                b"timeout=1000",
                b"rlimit=100000",
            )
        )
        first = 173
        second = 811
        outbound_first = 97
        lines = [
            "0.500000 execveat(7</memfd:djex-z3-main-image (deleted)>, "
            f'"", [{executable_arguments}], 0x1234 /* 0 vars */, '
            "AT_EMPTY_PATH) = 0 <0.000001>",
            "0.750000 write(2<pipe:[99]>, "
            f"{strace_string(commands[0])}, {len(commands[0])}) = "
            f"{len(commands[0])} <0.000001>",
            "1.000000 read(0<pipe:[11]>, "
            f"{strace_string(inbound[:first] + b'POISON')}, "
            f"{first + 6}) = {first} <0.000001>",
            "1.100000 read(0<pipe:[11]>,  <unfinished ...>",
            "1.200000 <... read resumed>"
            f"{strace_string(inbound[first:second])}, 4096) = "
            f"{second - first} <0.000001>",
            "1.300000 readv(0<pipe:[11]>, "
            f"[{{iov_base={strace_string(inbound[second:second + 401])}, "
            f"iov_len=401}}, {{iov_base={strace_string(inbound[second + 401:])}, "
            f"iov_len={len(inbound) - second - 401}}}], 2) = "
            f"{len(inbound) - second} <0.000001>",
            "2.000000 write(1<pipe:[12]>, "
            f"{strace_string(outbound[:outbound_first] + b'POISON')}, "
            f"{outbound_first + 6}) = {outbound_first} <0.000001>",
            "2.100000 writev(1<pipe:[12]>, "
            f"[{{iov_base={strace_string(outbound[outbound_first:outbound_first + 233])}, "
            f"iov_len=233}}, {{iov_base={strace_string(outbound[outbound_first + 233:])}, "
            f"iov_len={len(outbound) - outbound_first - 233}}}], 2) = "
            f"{len(outbound) - outbound_first} <0.000001>",
        ]
        (root / f"strace.{solver_pid}").write_text("\n".join(lines) + "\n")
        (root / "strace.111").write_text(
            "0.600000 write(9<pipe:[77]>, \"(check-sat)\", 11) = 11\n"
        )
        parsed = parse_trace(prefix, workload, solver_pid)
        require(parsed["queries"] == 48, "synthetic trace query count")
        require(parsed["accepted"] == 24, "synthetic trace accepted count")
        require(parsed["rejected"] == 24, "synthetic trace rejected count")
        require(parsed["distinct_programs"] == 4, "synthetic symbolic programs")

        bad_prefix = root / "bad"
        (root / f"bad.{solver_pid}").write_text(
            lines[0] + "\n3.000000 read(0<pipe:[11]>,  <unfinished ...>\n"
        )
        try:
            parse_trace(bad_prefix, workload, solver_pid)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure("synthetic unresolved trace record was accepted")


def check_exception_cleanup_and_stable_environment() -> None:
    with tempfile.TemporaryDirectory(prefix="djex-pipeline-cleanup-self-check-") as source:
        output = Path(source)
        raw = output / "raw"
        raw.mkdir()
        screen = object.__new__(Screen)
        screen.arguments = argparse.Namespace(
            outer_timeout=1, sample_interval_ms=1.0
        )
        screen.output = output
        screen.raw = raw
        screen.environment = output / "stable-empty-environment"
        screen.binary = {
            "baseline": Path("/definitely-not-a-djex-benchmark-binary"),
            "candidate": Path("/definitely-not-a-djex-benchmark-binary"),
        }
        screen.z3 = PINNED_Z3
        screen.z3_sha256 = PINNED_Z3_SHA256
        screen.run_active = False
        screen.active_process = None
        screen.termination_requests = []
        screen.termination_delivery_errors = []
        screen.external_termination_requests = []
        screen.termination_requests.append("STALE")
        screen.termination_delivery_errors.append("STALE")
        screen.begin_run(
            lambda: screen.request_termination(signal.SIGTERM)
        )
        require(
            screen.termination_requests == ["SIGTERM"],
            "publication-boundary termination request was erased",
        )
        require(
            screen.termination_delivery_errors == [],
            "publication-boundary reset retained stale delivery errors",
        )
        screen.run_active = False
        screen.termination_requests.clear()
        observed_environments = []
        for position in (1, 2):
            try:
                screen.execute(
                    "warmup", WORKLOADS["W1"], CELLS["A"], position
                )
            except HarnessFailure:
                pass
            else:
                raise HarnessFailure("synthetic missing executable was accepted")
            artifact = raw / "warmup" / "W1" / f"pos-{position:02d}-A"
            command = json.loads((artifact / "command.json").read_bytes())
            environment_index = command["argv"].index("--environment") + 1
            observed_environments.append(command["argv"][environment_index])
            require(not (artifact / "private").exists(), "private cleanup self-check")
            require(
                not screen.environment.exists(),
                "stable environment cleanup self-check",
            )
            residue = json.loads((artifact / "private-residue.json").read_bytes())
            require(
                residue == {"private": [], "stable_environment": []},
                "clean setup interruption reported residue",
            )
        require(
            observed_environments
            == [str(screen.environment), str(screen.environment)],
            "stable environment path drifted between invocations",
        )
        fake = output / "synthetic-long-running-djex"
        fake.write_text(
            "#!/usr/bin/python3\nimport time\ntime.sleep(10)\n"
        )
        fake.chmod(0o700)
        screen.binary = {"baseline": fake, "candidate": fake}
        termination_observed: list[bool] = []

        def terminate_active_run() -> None:
            deadline = time.monotonic() + 2
            while time.monotonic() < deadline:
                if screen.active_process is not None:
                    screen.request_termination(signal.SIGTERM)
                    termination_observed.append(True)
                    return
                time.sleep(0.001)

        terminator = threading.Thread(target=terminate_active_run)
        terminator.start()
        try:
            screen.execute("warmup", WORKLOADS["W1"], CELLS["A"], 3)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure("synthetic termination request was accepted")
        finally:
            terminator.join()
        require(termination_observed == [True], "synthetic termination was not delivered")
        require(not screen.run_active, "termination left Screen active")
        require(screen.active_process is None, "termination left an active process")
        require(not screen.environment.exists(), "termination left stable environment")
        require(
            not (raw / "warmup" / "W1" / "pos-03-A" / "private").exists(),
            "termination left private paths",
        )
        residue_root = output / "residue-predicate"
        residue_root.mkdir()
        fifo = residue_root / "fifo"
        os.mkfifo(fifo)
        require(
            non_directory_residue(residue_root) == ["fifo"],
            "FIFO residue did not fail closed",
        )
        fifo.unlink()


def self_check() -> int:
    python_identity()
    require_sha256(NORMALIZED_PLAN_SHA256, "normalized build-plan hash")
    require(NORMALIZED_PLAN_SIZE == 72_323, "normalized build-plan size")
    for relative, expected in FROZEN_ARTIFACT_SHA256.items():
        require(
            sha256_file(SCRIPT_DIR / relative) == expected,
            f"frozen artifact hash self-check for {relative}",
        )
    with (SCRIPT_DIR / "result-schema.tsv").open(newline="") as schema_handle:
        schema_rows = list(csv.DictReader(schema_handle, delimiter="\t"))
    require(
        [row["column"] for row in schema_rows] == RESULT_COLUMNS,
        "result schema columns differ from runner",
    )
    normalized_fixture = normalized_plan_bytes(
        {
            "absolute": "/synthetic/worktree/dist",
            "nested": [
                "prefix:/synthetic/worktree/suffix",
                {"key/synthetic/worktree": "/synthetic/worktree"},
            ],
            "plain": 1,
        },
        Path("/synthetic/worktree"),
    )
    require(
        normalized_fixture
        == (
            b'{\n  "absolute": "<WORKTREE>/dist",\n  "nested": [\n'
            b'    "prefix:<WORKTREE>/suffix",\n    {\n'
            b'      "key<WORKTREE>": "<WORKTREE>"\n    }\n'
            b'  ],\n  "plain": 1\n}\n'
        ),
        "normalized build-plan replacement/serialization drifted",
    )
    try:
        normalized_plan_bytes(
            {"/synthetic/worktree": 1, "<WORKTREE>": 2},
            Path("/synthetic/worktree"),
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("normalized build-plan key collision was accepted")
    for workload in WORKLOADS.values():
        require(
            workload.accepted + workload.rejected == workload.assessments,
            f"{workload.name} assessment partition",
        )
        expected_sequence = EXPECTED_QUERY_SEQUENCE[workload.name]
        require(
            len(expected_sequence) == workload.assessments,
            f"{workload.name} frozen query sequence length",
        )
        require(
            sum(status == "unsat" for _case, status in expected_sequence)
            == workload.accepted,
            f"{workload.name} frozen accepted sequence count",
        )
        require(
            sum(status == "sat" for _case, status in expected_sequence)
            == workload.rejected,
            f"{workload.name} frozen rejected sequence count",
        )
    require(len(WILLIAMS_ROWS) == 8, "Williams row count")
    require(all(set(row) == set(CELLS) and len(row) == 8 for row in WILLIAMS_ROWS), "Williams row permutation")
    for cell in CELLS:
        positions = [row.index(cell) for row in WILLIAMS_ROWS]
        require(sorted(positions) == list(range(8)), f"Williams position balance for {cell}")
    schedule = measured_schedule()
    require(len(schedule) == 16, "measured workload schedule length")
    for workload in WORKLOADS:
        rows = [row for _sample, name, row, _order in schedule if name == workload]
        require(sorted(rows) == list(range(1, 9)), f"{workload} row coverage")
    stats = b"      12,345 bytes allocated in the heap\n"
    require(parse_allocated_bytes(stats) == 12345, "allocation parser")
    sampler_fixture = {
        "sample_count": 1999,
        "span_ns": 1_998_000_000,
        "first_sample_ns": 1_000_000,
        "last_sample_start_ns": 1_999_000_000,
        "last_sample_finished_ns": 2_000_000_000,
        "mean_interval_ns": 1_000_000,
        "max_interval_ns": 4_000_000,
        "max_pass_ns": 500_000,
    }
    sampler_quality = validate_sampler_quality(
        sampler_fixture, 0, 2_000_000_000, 1.0
    )
    require(sampler_quality["coverage_ratio"] > 0.99, "sampler coverage parser")
    delayed_sampler = dict(sampler_fixture)
    delayed_sampler["first_sample_ns"] = 25_000_000
    delayed_sampler["span_ns"] = (
        delayed_sampler["last_sample_start_ns"]
        - delayed_sampler["first_sample_ns"]
    )
    delayed_sampler["mean_interval_ns"] = (
        delayed_sampler["span_ns"] // (delayed_sampler["sample_count"] - 1)
    )
    try:
        validate_sampler_quality(delayed_sampler, 0, 2_000_000_000, 1.0)
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("excessive initial sampler delay was accepted")
    live_host_control = host_control_snapshot()
    require(
        attest_host_control_start(live_host_control)["verdict"] == "PASS",
        "live host-control start attestation",
    )
    synthetic_host_end = json.loads(canonical_json(live_host_control))
    synthetic_host_end["diagnostics_only"]["loadavg"] = "diagnostic drift"
    for ancestor in synthetic_host_end["ancestors_leaf_to_root"]:
        ancestor["cpu_stat"]["parsed"]["usage_usec"] += 1
    require(
        attest_host_control_window(
            live_host_control, synthetic_host_end,
        )["verdict"] == "PASS",
        "diagnostic and usage-only host-control drift was rejected",
    )
    throttled_host_end = json.loads(canonical_json(synthetic_host_end))
    for ancestor in throttled_host_end["ancestors_leaf_to_root"]:
        stat = ancestor["cpu_stat"]["parsed"]
        if "nr_throttled" in stat:
            stat["nr_throttled"] += 1
            break
    else:
        raise HarnessFailure("host-control self-check found no throttle counter")
    require(
        attest_host_control_window(
            live_host_control, throttled_host_end,
        )["verdict"] == "HOLD",
        "nonzero cgroup throttling delta was accepted",
    )
    with tempfile.TemporaryDirectory(
        prefix="djex-host-control-evidence-self-check-"
    ) as source:
        evidence_root = Path(source)
        for damage in ("missing", "corrupt"):
            evidence = evidence_root / damage
            evidence.mkdir()
            start_path = evidence / "host-control-start.json"
            write_json(start_path, live_host_control)
            start_sha256 = sha256_file(start_path)
            start_attestation_path = (
                evidence / "host-control-start-attestation.json"
            )
            write_json(
                start_attestation_path,
                attest_host_control_start(live_host_control),
            )
            start_attestation_sha256 = sha256_file(start_attestation_path)
            if damage == "missing":
                start_path.unlink()
            else:
                start_path.write_bytes(b"corrupt\n")
            finalization = finalize_host_control_evidence(
                evidence,
                live_host_control,
                start_sha256,
                start_attestation_sha256,
            )
            end_path = evidence / "host-control-end.json"
            require(
                finalization["failure"] is not None,
                f"{damage} start evidence did not force HOLD",
            )
            require(
                finalization["end_sha256"] is not None
                and end_path.is_file()
                and sha256_file(end_path) == finalization["end_sha256"],
                f"{damage} start evidence suppressed the post snapshot",
            )
            require(
                finalization["attestation_sha256"] is None,
                f"{damage} start evidence produced a window attestation",
            )
    validate_run_configuration(argparse.Namespace(
        outer_timeout=600, sample_interval_ms=1.0,
    ))
    for bad_configuration in (
        argparse.Namespace(outer_timeout=599, sample_interval_ms=1.0),
        argparse.Namespace(outer_timeout=600, sample_interval_ms=5.0),
    ):
        try:
            validate_run_configuration(bad_configuration)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure("non-frozen run configuration was accepted")
    finalization_termination = TerminationCoordinator()
    finalization_termination.publish_finalization(
        lambda: finalization_termination.record(signal.SIGTERM)
    )
    require(
        finalization_termination.finalization_active
        and finalization_termination.requests == ["SIGTERM"],
        "finalization-publication termination request was lost",
    )
    outcome_termination = TerminationCoordinator()
    outcome_requests = outcome_termination.block_for_outcome_commit(
        (signal.SIGHUP, signal.SIGINT, signal.SIGTERM, signal.SIGQUIT),
        lambda: outcome_termination.record(signal.SIGQUIT),
        restore_for_test=True,
    )
    require(
        outcome_requests == ["SIGQUIT"],
        "outcome-commit termination request was lost",
    )
    vetoed = apply_termination_veto(
        {"verdict": "KEEP", "meaningful_over_1_10": True},
        outcome_requests,
    )
    require(
        vetoed["verdict"] == "HOLD"
        and not vetoed["meaningful_over_1_10"],
        "outcome-commit termination did not veto KEEP",
    )
    already_held = apply_termination_veto(
        {"verdict": "HOLD", "primary_failure": "synthetic"},
        outcome_requests,
    )
    require(
        already_held["termination_requests"] == ["SIGQUIT"],
        "preexisting HOLD omitted its termination request",
    )
    check_synthetic_trace_parser()
    check_exception_cleanup_and_stable_environment()
    for workload in WORKLOADS.values():
        stdout = b"banner\n" + OR_SEPARATOR.join([b"candidate"] * workload.rendered)
        stderr = b"warning: [DJEX_SEARCH_TRUNCATED] bounded search\n"
        result = validate_transcript(workload, 0, stdout, stderr)
        require(result["rendered"] == workload.rendered, f"{workload.name} transcript parser")
    decision = analyze(synthetic_rows())
    require(decision["verdict"] == "KEEP", "synthetic KEEP analysis")
    require(decision["pipeline_geomean"] > 1.10, "synthetic meaningful threshold")
    require(not decision["strong_1_25_tier"], "1.25 tier remains separate")
    print("candidate-pipeline benchmark self-check: PASS")
    return 0


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    commands.add_parser("self-check", help="run static and synthetic protocol checks only")
    run = commands.add_parser("run", help="execute the one-shot screen")
    run.add_argument("--baseline-root", required=True)
    run.add_argument("--baseline-binary", required=True)
    run.add_argument("--baseline-binary-sha256", required=True)
    run.add_argument("--candidate-root", required=True)
    run.add_argument("--candidate-binary", required=True)
    run.add_argument("--candidate-binary-sha256", required=True)
    run.add_argument("--z3", default=str(PINNED_Z3))
    run.add_argument("--z3-sha256", default=PINNED_Z3_SHA256)
    run.add_argument("--strace", default="/usr/bin/strace")
    run.add_argument("--output", required=True, help="new, never-reused evidence directory")
    run.add_argument(
        "--outer-timeout", type=int, default=600,
        help="frozen safety timeout; must equal 600",
    )
    run.add_argument(
        "--sample-interval-ms", type=float, default=1.0,
        help="frozen process-tree sampler target; must equal 1.0",
    )
    run.add_argument("--notes", default="")
    return result


def validate_run_configuration(arguments: argparse.Namespace) -> None:
    require(
        arguments.outer_timeout == 600,
        "outer timeout must equal the frozen 600 seconds",
    )
    require(
        arguments.sample_interval_ms == 1.0,
        "sample interval must equal the frozen 1.0 ms",
    )


def main() -> int:
    arguments = parser().parse_args()
    if arguments.command == "self-check":
        return self_check()
    validate_run_configuration(arguments)
    return run_screen(arguments)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except HarnessFailure as failure:
        print(f"benchmark HOLD: {failure}", file=sys.stderr)
        raise SystemExit(2)
