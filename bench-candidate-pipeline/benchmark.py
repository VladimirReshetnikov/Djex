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
import errno
import hashlib
import inspect
import json
import math
import os
import platform
import re
import shutil
import signal
import stat as stat_module
import statistics
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Sequence


SCHEMA = "djex-select-best-candidate-pipeline-screen/v2"
ROW_SCHEMA = "djex-select-best-candidate-pipeline-row/v2"
CALIBRATION_ROW_SCHEMA = "djex-solver-sampler-calibration-row/v1"
DECISION_SCHEMA = "djex-select-best-candidate-pipeline-decision/v2"
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
PINNED_Z3_SOURCE_MODE = 0o755
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
SAMPLER_MAX_INTERVAL_MULTIPLIER = 50.0
SAMPLER_MAX_PASS_MULTIPLIER = 50.0
SAMPLER_SESSION_SCAN_POST_CAPTURE_NS = 50_000_000
SAMPLER_SESSION_SCAN_MAX_START_GAP_NS = 100_000_000
SAMPLER_SESSION_SCAN_MAX_DURATION_NS = 50_000_000
SAMPLER_SESSION_SCAN_MAX_TGIDS = 65_536
SEALED_SOLVER_TARGET = "/memfd:djex-z3-main-image (deleted)"
# The Linux REPL selects the plain descriptor-bound launcher.  That launcher's
# sealer copies the source executable's ordinary rwx bits, so the exact staged
# mode is derived from (and must remain equal to) the pinned Z3 source mode.
SEALED_SOLVER_MODE = PINNED_Z3_SOURCE_MODE & 0o777
# Filled only after the workload/schema files are otherwise frozen.  Unlike
# benchmark.py itself, these artifacts can safely carry non-self-referential
# preregistered hashes in the runner.
FROZEN_ARTIFACT_SHA256 = {
    "result-schema.tsv": "f21b3a7d63ff7b2590a46c69db379b55957c156135d478a00f9082c412dfed6a",
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
    "sampler_max_interval_ns", "sampler_max_pass_ns",
    "sampler_session_scans", "sampler_session_scan_max_gap_ns",
    "sampler_session_scan_max_duration_ns",
    "solver_observation_sha256", "process_tree_sha256",
    "cleanup_ok", "artifact_dir",
]


class HarnessFailure(RuntimeError):
    """A fail-closed provenance, semantic, topology, or cleanup failure."""


class ProcStatMalformed(RuntimeError):
    """A categorized malformed `/proc/PID/stat` byte record."""

    def __init__(self, reason: str):
        super().__init__(reason)
        self.reason = reason


class CaptureRetry(RuntimeError):
    """Internal control flow for a categorized, retryable capture rejection."""


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
        if not result.get("primary_failure"):
            result["primary_failure"] = (
                f"catchable termination requested: {list(requests)}"
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


def fsync_directory(path: Path) -> None:
    descriptor = os.open(path, os.O_RDONLY | os.O_DIRECTORY | os.O_CLOEXEC)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def write_json(path: Path, value: Any) -> None:
    temporary = path.with_suffix(path.suffix + ".tmp")
    with temporary.open("wb") as handle:
        handle.write(canonical_json(value))
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, path)
    fsync_directory(path.parent)


def sampler_scheduling_policy(interval_ns: int) -> dict[str, Any]:
    return {
        "policy": "fixed_rate_monotonic_deadlines",
        "target_interval_ns": interval_ns,
        "missed_ticks": "coalesce_to_one_immediate_catch_up",
        "catch_up_successor": "catch_up_finish_plus_target_interval",
        "maximum_consecutive_immediate_catch_ups": 1,
        "deadline_equality": "on_time",
    }


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


def derive_plain_descriptor_bound_sealed_mode(source_mode: int) -> int:
    require(
        source_mode == PINNED_Z3_SOURCE_MODE,
        f"pinned Z3 source mode {source_mode:#o} != "
        f"{PINNED_Z3_SOURCE_MODE:#o}",
    )
    derived_mode = source_mode & 0o777
    require(
        derived_mode == SEALED_SOLVER_MODE == 0o755,
        "plain descriptor-bound sealed mode derivation drifted",
    )
    return derived_mode


def plain_descriptor_bound_mode_identity(root: Path) -> dict[str, Any]:
    """Bind the production launch selection and its source-mode derivation."""
    root = root.resolve()
    relative_sources = {
        "linux_repl_selection": Path("src/Language/Haskell/Djex/REPL.hs"),
        "haskell_staged_sealer": Path(
            "synthesis/internal/Language/Haskell/Synthesis/Internal/SMTLib/"
            "Z3/Process.hs"
        ),
        "native_staged_sealer": Path(
            "synthesis/cbits/z3_descriptor_spawn.c"
        ),
    }
    sources: dict[str, dict[str, Any]] = {}
    contents: dict[str, str] = {}
    for label, relative in relative_sources.items():
        path = root / relative
        require(path.is_file(), f"plain descriptor-bound source is missing: {path}")
        try:
            contents[label] = path.read_text()
        except (OSError, UnicodeDecodeError) as failure:
            raise HarnessFailure(
                f"cannot read plain descriptor-bound source {path}: {failure}"
            ) from failure
        sources[label] = {
            "path": str(path),
            "relative_path": relative.as_posix(),
            "sha256": sha256_file(path),
        }
    require(
        "#if defined(linux_HOST_OS)\n"
        "buildLengthZ3ExecutionConfig = "
        "mkLengthSMTLibDescriptorBoundExecutionConfig\n"
        in contents["linux_repl_selection"],
        "Linux REPL no longer selects the plain descriptor-bound launch",
    )
    require(
        "(c_sealStagedExecutable (fdToCInt descriptor)\n"
        "      (descriptorMetadataMode metadata) (fromIntegral count))\n"
        "    SealedImageModeCopiedFromSource descriptor count"
        in contents["haskell_staged_sealer"],
        "plain descriptor-bound Haskell sealer no longer forwards/copies "
        "source mode",
    )
    require(
        "if (fchmod(descriptor, (mode_t) (source_mode & 0777U)) < 0)"
        in contents["native_staged_sealer"],
        "plain descriptor-bound native sealer no longer copies ordinary "
        "source rwx bits",
    )
    source_stat = PINNED_Z3.stat()
    source_mode = stat_module.S_IMODE(source_stat.st_mode)
    require(
        stat_module.S_ISREG(source_stat.st_mode),
        f"pinned Z3 source is not regular: {PINNED_Z3}",
    )
    require(
        sha256_file(PINNED_Z3) == PINNED_Z3_SHA256,
        "pinned Z3 source bytes drifted while deriving sealed mode",
    )
    derived_mode = derive_plain_descriptor_bound_sealed_mode(source_mode)
    return {
        "schema": "djex-plain-descriptor-bound-mode-identity/v1",
        "source_root": str(root),
        "launch_selection": "linux REPL plain descriptor-bound",
        "mode_derivation": "pinned Z3 source st_mode & 0777",
        "pinned_z3_source": {
            "path": str(PINNED_Z3),
            "sha256": PINNED_Z3_SHA256,
            "mode": source_mode,
        },
        "expected_sealed_mode": derived_mode,
        "production_sources": sources,
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
    expected_mode: int | None = None,
) -> dict[str, Any]:
    path = path.resolve()
    require(path.is_absolute() and path.is_file(), f"{label} is not an absolute file: {path}")
    require(os.access(path, os.X_OK), f"{label} is not executable: {path}")
    digest = sha256_file(path)
    require(digest == expected_sha256, f"{label} SHA-256 {digest} != {expected_sha256}")
    stat = path.stat()
    mode = stat_module.S_IMODE(stat.st_mode)
    if expected_mode is not None:
        require(
            mode == expected_mode,
            f"{label} mode {mode:#o} != {expected_mode:#o}",
        )
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
        "mode": mode,
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


@dataclass(frozen=True)
class ProcStat:
    ppid: int
    pgrp: int
    session: int
    state: str
    user_ticks: int
    system_ticks: int
    start_time: int
    rss_pages: int


def parse_proc_stat_bytes(source: bytes) -> ProcStat:
    # `comm` is arbitrary bytes, may contain `)`, and is not required to be
    # UTF-8.  The kernel terminates it with the final `) ` before the
    # single-byte state and numeric tail.
    close = source.rfind(b") ")
    if close < 0:
        raise ProcStatMalformed("missing_comm_delimiter")
    fields = source[close + 2 :].split()
    if len(fields) < 22:
        raise ProcStatMalformed("short_tail")
    if len(fields[0]) != 1 or fields[0] not in b"RSDZTWtXxKWPIN":
        raise ProcStatMalformed("invalid_state")
    numeric_indices = (1, 2, 3, 11, 12, 19, 21)
    values: dict[int, int] = {}
    for index in numeric_indices:
        try:
            values[index] = int(fields[index])
        except (ValueError, OverflowError) as failure:
            raise ProcStatMalformed(f"invalid_integer_{index}") from failure
    return ProcStat(
        ppid=values[1],
        pgrp=values[2],
        session=values[3],
        state=fields[0].decode("ascii"),
        user_ticks=values[11],
        system_ticks=values[12],
        start_time=values[19],
        rss_pages=values[21],
    )


def read_proc_stat(pid: int) -> ProcStat:
    return parse_proc_stat_bytes(Path(f"/proc/{pid}/stat").read_bytes())


def proc_stat(pid: int) -> ProcStat | None:
    try:
        return read_proc_stat(pid)
    except (OSError, ProcStatMalformed):
        return None


class ProcessTreeSampler:
    def __init__(
        self, root_pid: int, solver_sha256: str, solver_argv0: str,
        interval: float,
    ):
        self.root_pid = root_pid
        self.session_id = root_pid
        self.solver_sha256 = solver_sha256
        self.solver_argv0 = solver_argv0.encode()
        self.solver_size = Path(solver_argv0).stat().st_size
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
        self.solver_images: dict[int, dict[str, Any]] = {}
        self.cleanup_descriptors: set[int] = set()
        self.pid_telemetry: dict[int, dict[str, Any]] = {}
        self.discovery_errors: dict[str, dict[str, int]] = {}
        self.session_scan_count = 0
        self.session_scan_forced_count = 0
        self.session_scan_first_ns: int | None = None
        self.session_scan_last_ns: int | None = None
        self.session_scan_last_finished_ns: int | None = None
        self.session_scan_max_gap_ns = 0
        self.session_scan_max_duration_ns = 0
        self.session_scan_tgids_total = 0
        self.session_scan_max_tgids = 0
        self.sample_count = 0
        self.first_sample_ns: int | None = None
        self.last_sample_ns: int | None = None
        self.last_sample_finished_ns: int | None = None
        self.interval_sum_ns = 0
        self.max_interval_ns = 0
        self.max_pass_ns = 0
        self.error: str | None = None

    def start(self) -> None:
        try:
            root = self._read_proc_stat(self.root_pid)
            require(
                root.session == self.session_id
                and root.start_time > 0
                and root.state != "Z",
                "sampler root is not a live leader of its own session",
            )
            self._register_process(
                self.root_pid, None, "root", self._monotonic_ns(), root,
            )
            # Synchronous inspection closes the launch-to-thread scheduling
            # window.  The thread then waits one target interval before its
            # next pass.
            self._record_sample(force_session_scan=True)
            self.thread.start()
        except BaseException as primary:
            try:
                self._discard_solver_descriptors()
            except BaseException as cleanup:
                raise HarnessFailure(
                    "sampler start failed and descriptor cleanup also failed: "
                    f"primary={type(primary).__name__}: {primary}; "
                    f"cleanup={type(cleanup).__name__}: {cleanup}"
                ) from primary
            raise

    def stop(self) -> None:
        self.stop_event.set()
        interrupted: BaseException | None = None
        while self.thread.is_alive():
            try:
                self.thread.join()
            except BaseException as failure:
                if interrupted is None:
                    interrupted = failure
        if self.error is None:
            try:
                # The process may already have exited.  This final forced scan
                # closes session-scan coverage and records any still-live
                # same-session process before cleanup.
                self._record_sample(force_session_scan=True)
            except BaseException as failure:
                self.error = repr(failure)
        if self.error is not None:
            raise HarnessFailure(f"process sampler failed: {self.error}")
        if interrupted is not None:
            raise interrupted

    def _loop(self) -> None:
        try:
            self._run_fixed_rate_samples()
        except BaseException as failure:
            self.error = repr(failure)

    def _run_fixed_rate_samples(self) -> None:
        """Sample on a fixed-rate clock with one bounded catch-up pass.

        A pass that starts after its deadline coalesces every missed tick into
        that one immediate catch-up.  Its successor is then scheduled from
        the catch-up finish plus one full period.  This preserves fixed-rate
        starts while ordinary passes are shorter than the period without
        allowing an overrun to create an unbounded zero-wait loop.
        """
        interval_ns = round(self.interval * 1_000_000_000)
        require(interval_ns > 0, "process sampler interval is not positive")
        with self.lock:
            previous_start = self.last_sample_ns
        require(
            previous_start is not None,
            "process sampler fixed-rate loop lacks its synchronous sample",
        )
        deadline_ns = previous_start + interval_ns
        while True:
            now_ns = self._monotonic_ns()
            # Equality is an on-time tick, not an overrun.  wait(0) remains a
            # stop check and makes a stop before an immediate catch-up
            # observable without sampling again.
            catch_up = now_ns > deadline_ns
            wait_ns = max(0, deadline_ns - now_ns)
            if self.stop_event.wait(wait_ns / 1_000_000_000):
                return
            self._record_sample()
            with self.lock:
                sample_finished = self.last_sample_finished_ns
            require(
                sample_finished is not None,
                "process sampler pass lacks its finish timestamp",
            )
            if catch_up:
                # Never chase more than one missed tick.  Even when this
                # catch-up pass itself overruns, its successor waits a whole
                # period measured from this finish.
                deadline_ns = sample_finished + interval_ns
            else:
                deadline_ns += interval_ns

    def _monotonic_ns(self) -> int:
        return time.monotonic_ns()

    def _read_proc_stat(self, pid: int) -> ProcStat:
        return read_proc_stat(pid)

    def _readlink(self, path: Path) -> str:
        return os.readlink(path)

    def _read_bytes(self, path: Path) -> bytes:
        return path.read_bytes()

    def _open_executable(self, path: Path) -> int:
        return os.open(path, os.O_RDONLY | os.O_CLOEXEC)

    def _fstat(self, descriptor: int) -> os.stat_result:
        return os.fstat(descriptor)

    def _stat_executable(self, path: Path) -> os.stat_result:
        return os.stat(path)

    def _close_executable_descriptor(self, descriptor: int) -> None:
        os.close(descriptor)

    def _emergency_close_executable_descriptor(self, descriptor: int) -> None:
        """Retry an uncertain descriptor close through an injectable seam."""
        os.close(descriptor)

    def _close_descriptor_with_emergency(
        self, descriptor: int,
    ) -> str | None:
        """Close an owned fd, retaining ownership unless closure is certain.

        The injectable close seam may fail either before or after the kernel
        close.  A direct retry closes the former and EBADF confirms the latter.
        The original failure is still reported so evidence remains HOLD.
        """
        try:
            self._close_executable_descriptor(descriptor)
        except BaseException as primary:
            emergency_failure: BaseException | None = None
            try:
                self._emergency_close_executable_descriptor(descriptor)
            except OSError as emergency:
                if emergency.errno != errno.EBADF:
                    emergency_failure = emergency
            except BaseException as emergency:
                emergency_failure = emergency
            if emergency_failure is None:
                with self.lock:
                    self.cleanup_descriptors.discard(descriptor)
                return (
                    f"{type(primary).__name__}: {primary}; emergency close "
                    "confirmed descriptor closed"
                )
            with self.lock:
                self.cleanup_descriptors.add(descriptor)
            return (
                f"{type(primary).__name__}: {primary}; emergency close "
                f"failed: {type(emergency_failure).__name__}: "
                f"{emergency_failure}; descriptor retained for retry"
            )
        with self.lock:
            self.cleanup_descriptors.discard(descriptor)
        return None

    def _discard_transfer_hook(self) -> None:
        """Pure-test seam after local discard ownership is complete."""

    def _snapshot_transfer_hook(self) -> None:
        """Pure-test seam at the descriptor ownership-transfer boundary."""

    def _transfer_solver_image(
        self, pid: int, image: dict[str, Any], start_time: int, now: int,
    ) -> None:
        with self.lock:
            prior_start = self.start_times.get(pid)
            require(
                prior_start is None or prior_start == start_time,
                f"solver PID {pid} was reused during capture",
            )
            record = self._pid_record_locked(pid, now)
            record["captures"] += 1
            if record["capture_first_ns"] is None:
                record["capture_first_ns"] = now
            record["capture_last_ns"] = now
            self._increment(record["capture_sources"], image["capture_source"])
            self._increment(record["gate_successes"], "captured")
            self.start_times[pid] = start_time
            self.solver_pids.add(pid)
            self.solver_images[pid] = image

    def _top_level_tgids(self) -> list[int]:
        entries = [
            int(entry.name) for entry in Path("/proc").iterdir()
            if entry.name.isdigit()
        ]
        require(
            len(entries) <= SAMPLER_SESSION_SCAN_MAX_TGIDS,
            "top-level /proc TGID scan exceeded its preregistered bound",
        )
        return sorted(entries)

    def _task_children(self, parent: int, now: int) -> list[int]:
        task_root = Path(f"/proc/{parent}/task")
        try:
            tasks = list(task_root.iterdir())
        except (FileNotFoundError, ProcessLookupError, PermissionError, OSError) as failure:
            self._note_error(parent, "task_list", failure, now)
            return []
        result: list[int] = []
        for task in tasks:
            try:
                children = (task / "children").read_text().split()
            except (FileNotFoundError, ProcessLookupError, PermissionError, OSError) as failure:
                self._note_error(parent, "task_children", failure, now)
                continue
            for source in children:
                require(
                    source.isdigit(),
                    f"malformed task-children PID: {source!r}",
                )
                result.append(int(source))
        return sorted(set(result))

    @staticmethod
    def _errno_name(failure: BaseException) -> str:
        if isinstance(failure, ProcStatMalformed):
            return f"PROC_STAT_{failure.reason.upper()}"
        if isinstance(failure, OSError) and failure.errno is not None:
            return errno.errorcode.get(failure.errno, f"ERRNO_{failure.errno}")
        return type(failure).__name__

    def _pid_record_locked(self, pid: int, now: int) -> dict[str, Any]:
        record = self.pid_telemetry.get(pid)
        if record is None:
            record = {
                "first_observed_ns": now,
                "last_observed_ns": now,
                "discovery_counts": {},
                "inspection_attempts": 0,
                "states": {},
                "session_ids": {},
                "targets": {},
                "gate_successes": {},
                "cmdline_shapes": {},
                "signature_mismatches": {},
                "argv_first_observation": None,
                "argv_first_observation_repetitions": 0,
                "argv_other_observation_count": 0,
                "consistency_mismatches": {},
                "executable_mode_first": None,
                "executable_mode_last": None,
                "executable_mode_counts": {},
                "errors": {},
                "captures": 0,
                "capture_first_ns": None,
                "capture_last_ns": None,
                "capture_sources": {},
            }
            self.pid_telemetry[pid] = record
        record["last_observed_ns"] = now
        return record

    @staticmethod
    def _increment(mapping: dict[str, int], key: str) -> None:
        mapping[key] = mapping.get(key, 0) + 1

    def _note_error(
        self, pid: int | None, stage: str, failure: BaseException, now: int,
    ) -> None:
        category = self._errno_name(failure)
        with self.lock:
            if pid is None:
                stages = self.discovery_errors.setdefault(stage, {})
            else:
                record = self._pid_record_locked(pid, now)
                stages = record["errors"].setdefault(stage, {})
            self._increment(stages, category)

    def _note_mismatch(
        self, pid: int, category: str, value: str, now: int,
    ) -> None:
        with self.lock:
            record = self._pid_record_locked(pid, now)
            values = record[category]
            self._increment(values, value)

    def _note_gate_success(self, pid: int, stage: str, now: int) -> None:
        with self.lock:
            record = self._pid_record_locked(pid, now)
            self._increment(record["gate_successes"], stage)

    def _note_executable_mode(self, pid: int, mode: int, now: int) -> None:
        with self.lock:
            record = self._pid_record_locked(pid, now)
            if record["executable_mode_first"] is None:
                record["executable_mode_first"] = mode
            record["executable_mode_last"] = mode
            self._increment(record["executable_mode_counts"], str(mode))

    def _solver_command_line_shape(self, command_line: bytes) -> str:
        prefix = [self.solver_argv0, b"-in", b"-smt2"]
        settings = [
            (b"smtlib2_compliant", b"true"),
            (b"timeout", b"1000"),
            (b"rlimit", b"100000"),
        ]
        exact_arguments = prefix + [key + b"=" + value for key, value in settings]
        exact = b"\0".join(exact_arguments) + b"\0"
        parsed_arguments = prefix + [
            item for setting in settings for item in setting
        ]
        parsed = b"\0".join(parsed_arguments) + b"\0"
        if command_line == exact:
            return "exec_exact"
        if command_line == parsed:
            return "z3_4_8_12_parsed_exact"
        for split_count in (1, 2):
            intermediate_arguments = list(prefix)
            for position, (key, value) in enumerate(settings):
                intermediate_arguments.extend(
                    (key, value) if position < split_count
                    else (key + b"=" + value,)
                )
            if command_line == b"\0".join(intermediate_arguments) + b"\0":
                return "intermediate"
        return "other"

    def _note_argv_observation(
        self, pid: int, command_line: bytes, arguments: list[bytes],
        expected_arguments: list[bytes], shape: str, now: int,
    ) -> None:
        digest = sha256_bytes(command_line)
        expected_bytes = b"\0".join(expected_arguments) + b"\0"
        constrained_ascii = (
            len(command_line) <= len(expected_bytes)
            and len(arguments) <= 16
            and all(
                re.fullmatch(rb"[A-Za-z0-9_./:=+<>-]*", argument)
                is not None
                for argument in arguments
            )
        )
        observation = {
            "argc": len(arguments),
            "arguments_ascii": (
                [argument.decode("ascii") for argument in arguments]
                if constrained_ascii else None
            ),
            "arguments_redacted": not constrained_ascii,
            "cmdline_bytes": len(command_line),
            "cmdline_sha256": digest,
            "nul_count": command_line.count(b"\0"),
            "token_lengths": [len(argument) for argument in arguments[:16]],
            "token_lengths_truncated": max(0, len(arguments) - 16),
        }
        with self.lock:
            record = self._pid_record_locked(pid, now)
            self._increment(record["cmdline_shapes"], shape)
            first = record["argv_first_observation"]
            if first is None:
                record["argv_first_observation"] = observation
                record["argv_first_observation_repetitions"] = 1
            elif first["cmdline_sha256"] == digest:
                record["argv_first_observation_repetitions"] += 1
            else:
                record["argv_other_observation_count"] += 1

    def _register_process(
        self, pid: int, parent: int | None, source: str, now: int,
        value: ProcStat,
    ) -> bool:
        with self.lock:
            prior_start = self.start_times.get(pid)
            if prior_start is not None and prior_start != value.start_time:
                raise HarnessFailure(
                    f"PID {pid} was reused during one sampled process session"
                )
            unseen = pid not in self.known
            self.known.add(pid)
            if unseen:
                self.parents[pid] = parent
            self.start_times[pid] = value.start_time
            self.cpu_ticks[pid] = max(
                self.cpu_ticks.get(pid, 0),
                value.user_ticks + value.system_ticks,
            )
            record = self._pid_record_locked(pid, now)
            self._increment(record["discovery_counts"], source)
            self._increment(record["states"], value.state)
            self._increment(record["session_ids"], str(value.session))
        return unseen

    def _read_stat_for_discovery(
        self, pid: int, stage: str, now: int, *, global_if_unknown: bool = False,
    ) -> ProcStat | None:
        try:
            return self._read_proc_stat(pid)
        except (OSError, ProcStatMalformed) as first_failure:
            with self.lock:
                known = pid in self.known
            if global_if_unknown and not known:
                # An arbitrary top-level TGID can disappear or expose a
                # transiently malformed stat record while /proc is scanned.
                # Retry it immediately once.  Only a successfully resolved
                # malformed record is benign; an unresolved malformed record
                # could have hidden a same-session exact-target process and is
                # retained under an explicitly fail-closed stage.
                try:
                    resolved = self._read_proc_stat(pid)
                except (OSError, ProcStatMalformed) as retry_failure:
                    self._note_error(
                        None, f"{stage}_unresolved_initial",
                        first_failure, now,
                    )
                    self._note_error(
                        None, f"{stage}_unresolved_retry",
                        retry_failure, now,
                    )
                    return None
                self._note_error(
                    None, f"{stage}_resolved", first_failure, now,
                )
                return resolved
            self._note_error(pid, stage, first_failure, now)
            return None

    def _capture_operation(
        self, pid: int, stage: str, now: int, action: Any,
    ) -> Any | None:
        try:
            value = action()
            self._note_gate_success(pid, stage, now)
            return value
        except (OSError, ProcStatMalformed) as failure:
            self._note_error(pid, stage, failure, now)
            return None

    def _record_sample(self, *, force_session_scan: bool = False) -> None:
        sample_started = self._monotonic_ns()
        self._sample(sample_started, force_session_scan=force_session_scan)
        sample_finished = self._monotonic_ns()
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

    def _sample(self, now: int, *, force_session_scan: bool = False) -> None:
        inspected: set[int] = set()
        with self.lock:
            frontier = list(self.known)
        visited: set[int] = set()
        while frontier:
            parent = frontier.pop()
            if parent in visited:
                continue
            visited.add(parent)
            for observed in self._task_children(parent, now):
                value = self._read_stat_for_discovery(
                    observed, "children_stat", now,
                )
                if value is None:
                    continue
                unseen = self._register_process(
                    observed, value.ppid, "children", now, value,
                )
                self._inspect_candidate(observed, "children", now, value)
                inspected.add(observed)
                if unseen:
                    frontier.append(observed)

        with self.lock:
            captured = bool(self.solver_images)
            last_scan = self.session_scan_last_ns
        scan_due = (
            force_session_scan
            or not captured
            or last_scan is None
            or now - last_scan >= SAMPLER_SESSION_SCAN_POST_CAPTURE_NS
        )
        if scan_due:
            self._scan_session(now, inspected, forced=force_session_scan)

        aggregate_rss = 0
        with self.lock:
            known = list(self.known)
        for pid in known:
            value = self._read_stat_for_discovery(pid, "known_stat", now)
            if value is None:
                continue
            self._register_process(pid, value.ppid, "known", now, value)
            aggregate_rss += max(0, value.rss_pages)
            if pid in inspected:
                continue
            self._inspect_candidate(pid, "known", now, value)
        with self.lock:
            self.peak_rss_pages = max(self.peak_rss_pages, aggregate_rss)

    def _scan_session(
        self, now: int, inspected: set[int], *, forced: bool,
    ) -> None:
        started = self._monotonic_ns()
        try:
            tgids = self._top_level_tgids()
        except BaseException as failure:
            self._note_error(None, "session_scan", failure, now)
            raise
        for pid in tgids:
            value = self._read_stat_for_discovery(
                pid, "session_scan_stat", now, global_if_unknown=True,
            )
            if value is None or value.session != self.session_id:
                continue
            self._register_process(
                pid, None if pid == self.root_pid else value.ppid,
                "session_scan", now, value,
            )
            self._inspect_candidate(pid, "session_scan", now, value)
            inspected.add(pid)
        finished = self._monotonic_ns()
        with self.lock:
            if self.session_scan_last_ns is not None:
                self.session_scan_max_gap_ns = max(
                    self.session_scan_max_gap_ns,
                    started - self.session_scan_last_ns,
                )
            else:
                self.session_scan_first_ns = started
            self.session_scan_last_ns = started
            self.session_scan_last_finished_ns = finished
            self.session_scan_count += 1
            self.session_scan_forced_count += int(forced)
            self.session_scan_max_duration_ns = max(
                self.session_scan_max_duration_ns, finished - started,
            )
            self.session_scan_tgids_total += len(tgids)
            self.session_scan_max_tgids = max(
                self.session_scan_max_tgids, len(tgids)
            )

    def _inspect_candidate(
        self, pid: int, source: str, now: int, before: ProcStat,
    ) -> None:
        with self.lock:
            record = self._pid_record_locked(pid, now)
            record["inspection_attempts"] += 1
        if before.session != self.session_id:
            self._note_mismatch(
                pid, "consistency_mismatches", "session_before", now,
            )
            return
        self._note_gate_success(pid, "session_before", now)
        if before.state == "Z":
            self._note_mismatch(
                pid, "consistency_mismatches", "zombie_before", now,
            )
            return
        self._note_gate_success(pid, "non_z_before", now)
        image_path = Path(f"/proc/{pid}/exe")
        try:
            target_before = self._readlink(image_path)
        except (FileNotFoundError, ProcessLookupError, PermissionError, OSError) as failure:
            self._note_error(pid, "readlink_before", failure, now)
            return
        with self.lock:
            record = self._pid_record_locked(pid, now)
            self._increment(record["targets"], target_before)
            already_captured = pid in self.solver_images
            captured_identity = (
                None if not already_captured else (
                    self.solver_images[pid]["device"],
                    self.solver_images[pid]["inode"],
                )
            )
        if already_captured:
            require(
                target_before == SEALED_SOLVER_TARGET,
                f"captured solver PID {pid} executable target drifted",
            )
            live_captured = self._capture_operation(
                pid, "captured_stat_exe", now,
                lambda: self._stat_executable(image_path),
            )
            if live_captured is None:
                return
            live_identity = (live_captured.st_dev, live_captured.st_ino)
            if live_identity != captured_identity:
                self._note_mismatch(
                    pid, "consistency_mismatches",
                    "captured_live_identity", now,
                )
                raise HarnessFailure(
                    f"captured solver PID {pid} executable identity drifted"
                )
            self._note_gate_success(pid, "captured_live_identity", now)
            return
        if target_before != SEALED_SOLVER_TARGET:
            return
        self._note_gate_success(pid, "target_exact", now)

        # Retain the executable first.  cmdline and environ are mutable procfs
        # views and must not sit between target recognition and the only
        # attempt to retain the image.
        try:
            descriptor = self._open_executable(image_path)
        except OSError as failure:
            self._note_error(pid, "open_exe", failure, now)
            return
        transferred = False
        image: dict[str, Any] | None = None
        primary_failure: BaseException | None = None
        close_failure: BaseException | None = None
        try:
            # Descriptor ownership begins before telemetry: even an injected
            # gate-recording failure must take the owner-finally close path.
            self._note_gate_success(pid, "open_exe", now)

            def required_step(stage: str, action: Any) -> Any:
                value = self._capture_operation(pid, stage, now, action)
                if value is None:
                    raise CaptureRetry(stage)
                return value

            held_before = required_step(
                "fstat_before", lambda: self._fstat(descriptor)
            )
            command_line = required_step(
                "cmdline_read",
                lambda: self._read_bytes(Path(f"/proc/{pid}/cmdline")),
            )
            environment = required_step(
                "environ_read",
                lambda: self._read_bytes(Path(f"/proc/{pid}/environ")),
            )
            after = required_step(
                "stat_after", lambda: self._read_proc_stat(pid)
            )
            target_after = required_step(
                "target_after", lambda: self._readlink(image_path)
            )
            live_after = required_step(
                "stat_exe_after", lambda: self._stat_executable(image_path)
            )
            held_after = required_step(
                "fstat_after", lambda: self._fstat(descriptor)
            )
            held_mode = stat_module.S_IMODE(held_after.st_mode)
            self._note_executable_mode(pid, held_mode, now)

            arguments = command_line.rstrip(b"\0").split(b"\0")
            expected_arguments = [
                self.solver_argv0,
                b"-in",
                b"-smt2",
                b"smtlib2_compliant=true",
                b"timeout=1000",
                b"rlimit=100000",
            ]
            command_line_shape = self._solver_command_line_shape(command_line)
            self._note_argv_observation(
                pid, command_line, arguments, expected_arguments,
                command_line_shape, now,
            )
            if command_line_shape not in {
                "exec_exact", "z3_4_8_12_parsed_exact",
            }:
                self._note_mismatch(
                    pid, "signature_mismatches",
                    f"argv_{command_line_shape}", now,
                )
                raise CaptureRetry("cmdline_shape")
            self._note_gate_success(pid, "cmdline_allowed", now)
            if environment != b"":
                self._note_mismatch(
                    pid, "signature_mismatches", "environment", now,
                )
                raise CaptureRetry("environment")
            self._note_gate_success(pid, "environ_empty", now)

            mismatches = []
            if after.start_time != before.start_time:
                mismatches.append("start_time")
            else:
                self._note_gate_success(pid, "start_stable", now)
            if after.session != before.session or after.session != self.session_id:
                mismatches.append("session")
            else:
                self._note_gate_success(pid, "session_stable", now)
            if after.state == "Z":
                mismatches.append("zombie_after")
            else:
                self._note_gate_success(pid, "non_z_after", now)
            if target_after != target_before or target_after != SEALED_SOLVER_TARGET:
                mismatches.append("target")
            else:
                self._note_gate_success(pid, "target_stable", now)
            before_identity = (held_before.st_dev, held_before.st_ino)
            after_identity = (held_after.st_dev, held_after.st_ino)
            live_identity = (live_after.st_dev, live_after.st_ino)
            if before_identity != after_identity:
                mismatches.append("held_fstat")
            else:
                self._note_gate_success(pid, "held_stable", now)
            if after_identity != live_identity:
                mismatches.append("live_fstat")
            else:
                self._note_gate_success(pid, "live_identity", now)
            if not stat_module.S_ISREG(held_after.st_mode):
                mismatches.append("file_type")
            else:
                self._note_gate_success(pid, "regular", now)
            if held_after.st_size != self.solver_size:
                mismatches.append("size")
            else:
                self._note_gate_success(pid, "size", now)
            if held_mode != SEALED_SOLVER_MODE:
                mismatches.append("mode")
            else:
                self._note_gate_success(pid, "mode", now)
            if mismatches:
                for mismatch in mismatches:
                    self._note_mismatch(
                        pid, "consistency_mismatches", mismatch, now,
                    )
                raise CaptureRetry("identity_consistency")

            image = {
                "target": target_after,
                "sha256": None,
                "device": held_after.st_dev,
                "inode": held_after.st_ino,
                "size": held_after.st_size,
                "mode": held_mode,
                "start_time": after.start_time,
                "session_id": after.session,
                "state_before": before.state,
                "state_after": after.state,
                "argv": [value.decode("ascii") for value in arguments],
                "cmdline_shape": command_line_shape,
                "environment_bytes": 0,
                "capture_source": source,
                "descriptor": descriptor,
            }
            self._transfer_solver_image(
                pid, image, after.start_time, now,
            )
            transferred = True
        except CaptureRetry:
            pass
        except BaseException as failure:
            primary_failure = failure
        finally:
            if image is not None:
                # `_transfer_solver_image` is an injected seam in the pure
                # tests.  If it raises after installing the exact object, the
                # sampler—not this frame—owns the descriptor.  Detect that
                # state before deciding who must close it.
                with self.lock:
                    transferred = self.solver_images.get(pid) is image
            if not transferred:
                close_report = self._close_descriptor_with_emergency(
                    descriptor
                )
                if close_report is not None:
                    close_failure = HarnessFailure(close_report)
        if primary_failure is not None:
            if close_failure is not None:
                raise HarnessFailure(
                    "solver capture failed and retained-descriptor close also "
                    f"failed: primary={type(primary_failure).__name__}: "
                    f"{primary_failure}; close={type(close_failure).__name__}: "
                    f"{close_failure}"
                ) from primary_failure
            raise primary_failure
        if close_failure is not None:
            raise HarnessFailure(
                "cannot close rejected solver executable descriptor: "
                f"{type(close_failure).__name__}: {close_failure}"
            ) from close_failure

    def _discard_solver_descriptors(self) -> None:
        descriptors: set[int] = set()
        transfer_started = False
        primary_failure: BaseException | None = None
        try:
            with self.lock:
                descriptors = {
                    descriptor
                    for value in self.solver_images.values()
                    if isinstance(
                        (descriptor := value.get("descriptor")), int,
                    )
                } | set(self.cleanup_descriptors)
                transfer_started = True
                self.cleanup_descriptors.update(descriptors)
                self._discard_transfer_hook()
                for value in self.solver_images.values():
                    if value.get("descriptor") in descriptors:
                        value["descriptor"] = None
        except BaseException as failure:
            primary_failure = failure
        failures: list[str] = []
        if transfer_started:
            # Even if an injected exception interrupted the clear loop, the
            # complete local set owns every fd and clears any matching stale
            # registry entry before attempting close/retry.
            with self.lock:
                for value in self.solver_images.values():
                    if value.get("descriptor") in descriptors:
                        value["descriptor"] = None
            for descriptor in sorted(descriptors):
                report = self._close_descriptor_with_emergency(descriptor)
                if report is not None:
                    failures.append(f"fd {descriptor}: {report}")
        if primary_failure is not None:
            if failures:
                raise HarnessFailure(
                    "descriptor discard transfer failed and close reporting "
                    f"also failed: primary={type(primary_failure).__name__}: "
                    f"{primary_failure}; close={'; '.join(failures)}"
                ) from primary_failure
            raise primary_failure
        if failures:
            with self.lock:
                retained = sorted(self.cleanup_descriptors)
            raise HarnessFailure(
                "cannot discard retained solver descriptors: "
                + "; ".join(failures)
                + f"; retry_registry={retained}"
            )

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
        images: dict[int, dict[str, Any]] = {}
        descriptors: dict[int, int] = {}
        failures: list[str] = []
        transfer_started = False
        primary_failure: BaseException | None = None
        try:
            with self.lock:
                images = {
                    pid: dict(value)
                    for pid, value in self.solver_images.items()
                }
                descriptors = {
                    pid: descriptor
                    for pid, value in images.items()
                    if isinstance(
                        (descriptor := value.get("descriptor")), int,
                    )
                }
                # From this point the outer finally owns every descriptor in
                # the complete map, even if an injected exception interrupts
                # the internal metadata clearing loop.
                transfer_started = True
                self._snapshot_transfer_hook()
                for value in self.solver_images.values():
                    value["descriptor"] = None
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
        except BaseException as failure:
            primary_failure = failure
        finally:
            if transfer_started:
                with self.lock:
                    for pid, descriptor in descriptors.items():
                        current = self.solver_images.get(pid)
                        if (
                            current is not None
                            and current.get("descriptor") == descriptor
                        ):
                            current["descriptor"] = None
                for pid, descriptor in descriptors.items():
                    report = self._close_descriptor_with_emergency(descriptor)
                    if report is not None:
                        failures.append(
                            f"cannot close solver image for PID {pid}: "
                            f"{report}"
                        )
        if primary_failure is not None:
            if failures:
                raise HarnessFailure(
                    "solver snapshot failed and descriptor finalization also "
                    f"failed: primary={type(primary_failure).__name__}: "
                    f"{primary_failure}; finalization={'; '.join(failures)}"
                ) from primary_failure
            raise primary_failure
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
                "scheduling": sampler_scheduling_policy(
                    round(self.interval * 1_000_000_000)
                ),
                "sampler_error": self.error,
                "cleanup_descriptors": sorted(self.cleanup_descriptors),
                "known_pids": sorted(self.known),
                "parents": {str(pid): parent for pid, parent in self.parents.items()},
                "start_times": dict(self.start_times),
                "cpu_ticks": dict(self.cpu_ticks),
                "peak_rss_pages": self.peak_rss_pages,
                "session_id": self.session_id,
                "session_scan": {
                    "policy": {
                        "before_capture": "every_sampler_pass",
                        "post_capture_interval_ns": (
                            SAMPLER_SESSION_SCAN_POST_CAPTURE_NS
                        ),
                        "maximum_start_gap_ns": (
                            SAMPLER_SESSION_SCAN_MAX_START_GAP_NS
                        ),
                        "maximum_duration_ns": (
                            SAMPLER_SESSION_SCAN_MAX_DURATION_NS
                        ),
                        "maximum_tgids_per_scan": (
                            SAMPLER_SESSION_SCAN_MAX_TGIDS
                        ),
                    },
                    "count": self.session_scan_count,
                    "forced_count": self.session_scan_forced_count,
                    "first_start_ns": self.session_scan_first_ns,
                    "last_start_ns": self.session_scan_last_ns,
                    "last_finished_ns": self.session_scan_last_finished_ns,
                    "max_start_gap_ns": self.session_scan_max_gap_ns,
                    "max_duration_ns": self.session_scan_max_duration_ns,
                    "tgids_total": self.session_scan_tgids_total,
                    "max_tgids": self.session_scan_max_tgids,
                    "errors": json.loads(canonical_json(
                        self.discovery_errors
                    )),
                },
                "pid_telemetry": {
                    str(pid): json.loads(canonical_json(record))
                    for pid, record in sorted(self.pid_telemetry.items())
                },
            }


def require_single_solver_image(
    images: dict[int, dict[str, Any]],
) -> tuple[int, dict[str, Any]]:
    require(
        len(images) == 1,
        f"observed {len(images)} sealed solver images",
    )
    return next(iter(images.items()))


def exact_target_identity_attestation(
    diagnostics: dict[str, Any], images: dict[int, dict[str, Any]],
) -> dict[str, Any]:
    start_times = diagnostics.get("start_times", {})
    observed: set[tuple[int, int]] = set()
    malformed: list[dict[str, Any]] = []
    for pid_source, telemetry in diagnostics.get("pid_telemetry", {}).items():
        if telemetry.get("targets", {}).get(SEALED_SOLVER_TARGET, 0) <= 0:
            continue
        try:
            pid = int(pid_source)
        except (TypeError, ValueError):
            malformed.append({"pid": pid_source, "reason": "invalid_pid"})
            continue
        start_time = start_times.get(pid, start_times.get(str(pid)))
        if not isinstance(start_time, int) or start_time <= 0:
            malformed.append({
                "pid": pid,
                "reason": "missing_positive_start_time",
                "start_time": start_time,
            })
            continue
        observed.add((pid, start_time))
    captured: set[tuple[int, int]] = set()
    for pid_source, image in images.items():
        try:
            pid = int(pid_source)
        except (TypeError, ValueError):
            malformed.append({"pid": pid_source, "reason": "invalid_image_pid"})
            continue
        start_time = image.get("start_time")
        if not isinstance(start_time, int) or start_time <= 0:
            malformed.append({
                "pid": pid,
                "reason": "invalid_image_start_time",
                "start_time": start_time,
            })
            continue
        captured.add((pid, start_time))
    passed = not malformed and len(observed) == 1 and observed == captured
    return {
        "schema": "djex-exact-target-identity-attestation/v1",
        "pass": passed,
        "required_cardinality": 1,
        "observed_exact_target_identities": [
            {"pid": pid, "start_time": start_time}
            for pid, start_time in sorted(observed)
        ],
        "captured_image_identities": [
            {"pid": pid, "start_time": start_time}
            for pid, start_time in sorted(captured)
        ],
        "malformed_identities": malformed,
    }


def finalize_sampler_images(
    sampler: ProcessTreeSampler, failures: list[str], label: str,
) -> dict[int, dict[str, Any]]:
    images: dict[int, dict[str, Any]] = {}
    snapshot_failed = False
    try:
        images = sampler.solver_snapshot()
    except BaseException as failure:
        snapshot_failed = True
        failures.append(
            f"{label} solver snapshot failure: "
            f"{type(failure).__name__}: {failure}"
        )
    try:
        # A rejected capture can leave an fd in the sampler's explicit retry
        # registry even when snapshotting the accepted images succeeds.  Drain
        # and assert that registry on both paths; snapshot failure remains the
        # first reported failure when both operations fail.
        sampler._discard_solver_descriptors()
    except BaseException as cleanup:
        failures.append(
            f"{label} solver descriptor discard failure after "
            f"{'snapshot failure' if snapshot_failed else 'successful snapshot'}: "
            f"{type(cleanup).__name__}: {cleanup}"
        )
    return {} if snapshot_failed else images


def process_group_members(group: int) -> list[int]:
    members = []
    for entry in Path("/proc").iterdir():
        if not entry.name.isdigit():
            continue
        value = proc_stat(int(entry.name))
        if value is not None and value.pgrp == group:
            members.append(int(entry.name))
    return members


def matching_processes(identities: dict[int, int]) -> list[int]:
    matched = []
    for pid, expected_start in identities.items():
        value = proc_stat(pid)
        if value is not None and value.start_time == expected_start:
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
    target_interval_ns = round(target_interval_ms * 1_000_000)
    require(
        target_interval_ns > 0,
        "process sampler target interval is not positive",
    )
    require(
        diagnostics.get("scheduling")
        == sampler_scheduling_policy(target_interval_ns),
        "process sampler scheduling policy drifted",
    )
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
    session_scan = diagnostics.get("session_scan")
    require(isinstance(session_scan, dict), "session scan diagnostics are missing")
    require(
        session_scan.get("policy")
        == {
            "before_capture": "every_sampler_pass",
            "post_capture_interval_ns": SAMPLER_SESSION_SCAN_POST_CAPTURE_NS,
            "maximum_start_gap_ns": SAMPLER_SESSION_SCAN_MAX_START_GAP_NS,
            "maximum_duration_ns": SAMPLER_SESSION_SCAN_MAX_DURATION_NS,
            "maximum_tgids_per_scan": SAMPLER_SESSION_SCAN_MAX_TGIDS,
        },
        "session scan policy drifted",
    )
    require(session_scan.get("count", 0) >= 2, "fewer than two session scans")
    require(
        2 <= session_scan.get("forced_count", -1)
        <= session_scan.get("count", 0),
        "session scan lacks immediate and forced-final observations",
    )
    scan_first = session_scan.get("first_start_ns")
    scan_last = session_scan.get("last_start_ns")
    scan_finished = session_scan.get("last_finished_ns")
    require(
        isinstance(scan_first, int)
        and isinstance(scan_last, int)
        and isinstance(scan_finished, int)
        and wall_started_ns <= scan_first <= scan_last <= scan_finished,
        "session scan endpoints are incomplete or inconsistent",
    )
    require(
        scan_first - wall_started_ns <= SAMPLER_SESSION_SCAN_MAX_START_GAP_NS,
        "initial session scan exceeded its preregistered gap",
    )
    require(
        max(0, wall_finished_ns - scan_finished)
        <= SAMPLER_SESSION_SCAN_MAX_START_GAP_NS,
        "terminal session scan exceeded its preregistered gap",
    )
    require(
        session_scan.get("max_start_gap_ns", -1)
        <= SAMPLER_SESSION_SCAN_MAX_START_GAP_NS,
        "session scan start gap exceeded its preregistered bound",
    )
    require(
        0 <= session_scan.get("max_duration_ns", -1)
        <= SAMPLER_SESSION_SCAN_MAX_DURATION_NS,
        "session scan duration exceeded its preregistered bound",
    )
    require(
        0 <= session_scan.get("max_tgids", -1) <= SAMPLER_SESSION_SCAN_MAX_TGIDS,
        "session scan TGID count exceeded its preregistered bound",
    )
    require(
        diagnostics.get("sampler_error") is None,
        f"process sampler error: {diagnostics.get('sampler_error')}",
    )
    require(
        diagnostics.get("cleanup_descriptors", []) == [],
        "process sampler retained executable descriptors for cleanup retry",
    )
    allowed_transient_errors = {"ENOENT", "ESRCH"}
    unexpected_errors: list[dict[str, Any]] = []
    error_sources = [("session_scan", session_scan.get("errors", {}))]
    error_sources.extend(
        (f"pid:{pid}", telemetry.get("errors", {}))
        for pid, telemetry in diagnostics.get("pid_telemetry", {}).items()
    )
    for owner, stages in error_sources:
        for stage, categories in stages.items():
            for category, count in categories.items():
                globally_resolved_malformed = (
                    owner == "session_scan"
                    and stage.endswith("_resolved")
                    and category.startswith("PROC_STAT_")
                )
                if (
                    category not in allowed_transient_errors
                    and not globally_resolved_malformed
                    and count
                ):
                    unexpected_errors.append({
                        "owner": owner,
                        "stage": stage,
                        "category": category,
                        "count": count,
                    })
    require(
        not unexpected_errors,
        f"unexpected process sampler errors: {unexpected_errors}",
    )
    return {
        "coverage_ratio": coverage,
        "initial_delay_ns": initial_delay,
        "terminal_gap_ns": terminal_gap,
        "session_scan_count": session_scan["count"],
        "session_scan_max_gap_ns": session_scan["max_start_gap_ns"],
        "session_scan_max_duration_ns": session_scan["max_duration_ns"],
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
    successful_sealed_execs: list[tuple[int, str]] = []
    for path in files:
        suffix = path.name[len(prefix.name) + 1 :]
        require(suffix.isdigit(), f"unexpected strace artifact {path.name}")
        traced_pid = int(suffix)
        if traced_pid == solver_pid:
            solver_file = path
        for line in path.read_text(errors="strict").splitlines():
            if (
                re.search(r"\bexecveat\(", line) is not None
                and f"<{SEALED_SOLVER_TARGET}>" in line
                and "AT_EMPTY_PATH" in line
                and re.search(r"\) = 0(?:\s|$)", line) is not None
            ):
                successful_sealed_execs.append((traced_pid, line))
    require(solver_file is not None, f"strace has no file for sealed solver PID {solver_pid}")
    require(
        len(successful_sealed_execs) == 1,
        "all strace files contain "
        f"{len(successful_sealed_execs)} successful exact-target sealed "
        "execveat records",
    )
    require(
        successful_sealed_execs[0][0] == solver_pid,
        "the sole successful exact-target sealed execveat does not belong "
        "to the captured solver identity",
    )
    file_source = solver_file.read_text(errors="strict")
    successful_process_groups = [
        line for line in file_source.splitlines()
        if re.search(r"\bsetpgid\(0, 0\)\s+=\s+0(?:\s|$)", line)
        is not None
    ]
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
    require(
        len(successful_process_groups) == 1,
        "sealed solver did not establish exactly one private process group",
    )
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


def validate_result_row_semantics(
    row: dict[str, Any], *, calibration_enabled: bool,
) -> None:
    phase = row.get("phase")
    if phase == "calibration":
        require(
            calibration_enabled,
            "diagnostic calibration row entered a release screen",
        )
        require(
            row.get("schema") == CALIBRATION_ROW_SCHEMA,
            "diagnostic calibration row schema drifted",
        )
        require(
            row.get("workload") == "W1"
            and row.get("cell") == "B"
            and row.get("revision") == "baseline"
            and row.get("commit") == BASELINE_COMMIT
            and row.get("jobs") == 1
            and row.get("capabilities") == 2,
            "diagnostic calibration treatment drifted",
        )
        require(
            row.get("sample", "") == ""
            and row.get("williams_row", "") == ""
            and isinstance(row.get("position"), int)
            and 1 <= row["position"] <= 64,
            "diagnostic calibration row position/sample fields drifted",
        )
        require(
            all(
                row.get(field, "") == ""
                for field in (
                    "solver_queries_observed",
                    "solver_accepted_observed",
                    "solver_rejected_observed",
                    "query_vector_sha256",
                )
            ),
            "diagnostic calibration row contains release-preflight fields",
        )
        return
    require(
        phase in {"preflight", "warmup", "measured"},
        f"unknown release result phase: {phase!r}",
    )
    require(
        row.get("schema") == ROW_SCHEMA,
        "release result row schema drifted",
    )


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
        fsync_directory(output)
        self.run_id = run_identifier(output)
        self.transcripts: dict[str, tuple[int, bytes, bytes]] = {}
        self.query_vectors: dict[str, dict[str, Any]] = {}
        self.rows: list[dict[str, Any]] = []
        self.failure_attempts: list[dict[str, Any]] = []
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
        self.calibration_enabled = bool(
            getattr(arguments, "diagnostic_calibration", False)
        )

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
        validate_result_row_semantics(
            row, calibration_enabled=self.calibration_enabled,
        )
        self.writer.writerow({column: row.get(column, "") for column in RESULT_COLUMNS})
        self.results_handle.flush()
        os.fsync(self.results_handle.fileno())
        self.rows.append(row)
        expected = self.transcripts.setdefault(row["workload"], outcome.transcript_key)
        require(outcome.transcript_key == expected, f"{row['workload']} transcript differs in {row['artifact_dir']}")
        if outcome.query_vector is not None:
            query_expected = self.query_vectors.setdefault(row["workload"], outcome.query_vector)
            require(outcome.query_vector == query_expected, f"{row['workload']} query vector differs in {row['cell']}")

    def _artifact_path(
        self,
        phase: str,
        workload: Workload,
        cell: Cell,
        position: int,
        sample: int | None,
    ) -> Path:
        parts = [phase, workload.name]
        if sample is not None:
            parts.append(f"sample-{sample:02d}")
        parts.append(f"pos-{position:02d}-{cell.name}")
        return self.raw.joinpath(*parts)

    def _failure_artifact_identity(
        self, path: Path,
    ) -> dict[str, Any]:
        relative = str(path.relative_to(self.output))
        try:
            metadata = path.stat()
        except FileNotFoundError:
            return {
                "path": relative,
                "present": False,
                "size": None,
                "sha256": None,
            }
        require(
            stat_module.S_ISREG(metadata.st_mode),
            f"failure-attempt artifact is not a regular file: {relative}",
        )
        return {
            "path": relative,
            "present": True,
            "size": metadata.st_size,
            "sha256": sha256_file(path),
        }

    def _persist_failure_attempt(
        self,
        phase: str,
        workload: Workload,
        cell: Cell,
        position: int,
        sample: int | None,
        williams_row: int | None,
        primary: BaseException,
    ) -> dict[str, Any]:
        artifact = self._artifact_path(
            phase, workload, cell, position, sample,
        )
        artifact.mkdir(parents=True, exist_ok=True)
        manifest_path = artifact / "failure-attempt-manifest.json"
        require(
            not manifest_path.exists() and not manifest_path.is_symlink(),
            "failure-attempt manifest already exists: "
            f"{manifest_path.relative_to(self.output)}",
        )
        named_paths = {
            "command": artifact / "command.json",
            "input": artifact / "input.repl",
            "stdout": artifact / "stdout.bin",
            "stderr": artifact / "stderr.bin",
            "stats": artifact / "rts.stats",
            "process_tree": artifact / "process-tree.json",
            "residue": artifact / "private-residue.json",
            "query_vector": artifact / "query-vector.json",
        }
        traces = sorted(
            (
                path for path in artifact.glob("strace*")
                if path.is_file() and not path.is_symlink()
            ),
            key=lambda path: os.fsencode(path.name),
        )
        manifest = {
            "schema": "djex-benchmark-failure-attempt/v1",
            "run_id": self.run_id,
            "attempt": {
                "phase": phase,
                "workload": workload.name,
                "cell": cell.name,
                "revision": cell.revision,
                "commit": cell.commit,
                "jobs": cell.jobs,
                "capabilities": cell.capabilities,
                "position": position,
                "sample": sample,
                "williams_row": williams_row,
                "artifact_dir": str(artifact.relative_to(self.output)),
            },
            "failure": {
                "type": type(primary).__name__,
                "message": str(primary),
            },
            "artifacts": {
                name: self._failure_artifact_identity(path)
                for name, path in named_paths.items()
            },
            "traces": [
                self._failure_artifact_identity(path) for path in traces
            ],
        }
        write_json(manifest_path, manifest)
        reference = {
            **manifest["attempt"],
            "path": str(manifest_path.relative_to(self.output)),
            "size": manifest_path.stat().st_size,
            "sha256": sha256_file(manifest_path),
        }
        self.failure_attempts.append(reference)
        return reference

    def failure_attempt_summary(self) -> dict[str, Any]:
        references = json.loads(canonical_json(self.failure_attempts))
        verification_failures: list[str] = []
        for reference in references:
            path = self.output / reference["path"]
            try:
                metadata = path.stat()
                require(
                    stat_module.S_ISREG(metadata.st_mode)
                    and metadata.st_size == reference["size"]
                    and sha256_file(path) == reference["sha256"],
                    "failure-attempt manifest identity drifted",
                )
            except BaseException as failure:
                verification_failures.append(
                    f"{reference['path']}: {type(failure).__name__}: "
                    f"{failure}"
                )
        return {
            "schema": "djex-benchmark-failure-attempt-set/v1",
            "count": len(references),
            "manifests": references,
            "manifests_sha256": sha256_bytes(canonical_json(references)),
            "verification_pass": not verification_failures,
            "verification_failures": verification_failures,
        }

    def execute(
        self,
        phase: str,
        workload: Workload,
        cell: Cell,
        position: int,
        sample: int | None = None,
        williams_row: int | None = None,
    ) -> RunOutcome:
        try:
            return self._execute_once(
                phase, workload, cell, position, sample, williams_row,
            )
        except BaseException as primary:
            try:
                self._persist_failure_attempt(
                    phase, workload, cell, position, sample, williams_row,
                    primary,
                )
            except BaseException as durability_failure:
                raise HarnessFailure(
                    "screen invocation failed and failure-attempt manifest "
                    "persistence also failed: "
                    f"primary={type(primary).__name__}: {primary}; "
                    "persistence="
                    f"{type(durability_failure).__name__}: "
                    f"{durability_failure}"
                ) from primary
            raise

    def _execute_once(
        self,
        phase: str,
        workload: Workload,
        cell: Cell,
        position: int,
        sample: int | None = None,
        williams_row: int | None = None,
    ) -> RunOutcome:
        allowed_phases = {"preflight", "warmup", "measured"}
        if self.calibration_enabled:
            allowed_phases.add("calibration")
        require(
            phase in allowed_phases,
            f"screen phase is not enabled: {phase!r}",
        )
        require(
            phase != "calibration"
            or (
                workload.name == "W1"
                and cell.name == "B"
                and sample is None
                and williams_row is None
                and 1 <= position <= 64
            ),
            "diagnostic calibration invocation shape drifted",
        )
        artifact = self._artifact_path(
            phase, workload, cell, position, sample,
        )
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
        exact_target_attestation: dict[str, Any] = {}
        solver_observation_sha256 = ""
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
                    "trace=clone,clone3,fork,vfork,execve,execveat,setpgid,wait4,"
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
                solver_images = finalize_sampler_images(
                    sampler, failures, "production run",
                )
                try:
                    sampler_diagnostics = sampler.diagnostics()
                except BaseException as failure:
                    failures.append(
                        f"sampler diagnostics failure: {type(failure).__name__}: {failure}"
                    )
            if sampler is not None:
                try:
                    exact_target_attestation = exact_target_identity_attestation(
                        sampler_diagnostics, solver_images,
                    )
                    if not exact_target_attestation["pass"]:
                        failures.append(
                            "exact-target identity cardinality failure: "
                            + canonical_json(
                                exact_target_attestation
                            ).decode().strip()
                        )
                except BaseException as failure:
                    failures.append(
                        "exact-target identity attestation failure: "
                        f"{type(failure).__name__}: {failure}"
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
                    solver_observation = {
                        "schema": "djex-solver-observation/v2",
                        "session_id": sampler_diagnostics.get("session_id"),
                        "session_scan": sampler_diagnostics.get("session_scan"),
                        "pid_telemetry": sampler_diagnostics.get("pid_telemetry"),
                        "exact_target_identity_attestation": (
                            exact_target_attestation
                        ),
                    }
                    solver_observation_sha256 = sha256_bytes(
                        canonical_json(solver_observation)
                    )
                    process_tree = {
                        "schema": "djex-benchmark-process-tree/v2",
                        "root_pid": process.pid if process is not None else None,
                        "cpu_ns": cpu_ns,
                        "peak_rss_bytes": peak_rss_bytes,
                        "solver_sessions": solver_sessions,
                        "wall_started_ns": started,
                        "wall_finished_ns": finished,
                        "sampler": sampler_diagnostics,
                        "solver_observation": solver_observation,
                        "solver_observation_sha256": (
                            solver_observation_sha256
                        ),
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
        require(
            exact_target_attestation.get("pass") is True,
            "exact-target identity cardinality did not pass",
        )
        require(started is not None and finished is not None, "subprocess wall endpoints are incomplete")
        cpu_ns, peak_rss_bytes, solver_sessions = sampler.metrics()
        sampling_quality = validate_sampler_quality(
            sampler_diagnostics,
            started,
            finished,
            self.arguments.sample_interval_ms,
        )
        solver_pid, solver_image = require_single_solver_image(solver_images)
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
        require(
            solver_image.get("session_id") == process.pid
            and solver_image.get("state_before") != "Z"
            and solver_image.get("state_after") != "Z"
            and solver_image.get("size") == sampler.solver_size
            and solver_image.get("mode") == SEALED_SOLVER_MODE,
            "sealed solver live identity or executable metadata drifted",
        )
        require(stats_path.is_file(), f"missing RTS statistics in {artifact}")
        stats = stats_path.read_bytes()
        allocated = parse_allocated_bytes(stats)
        semantic = validate_transcript(workload, process.returncode, stdout, stderr)
        require(solver_sessions == 1, f"observed {solver_sessions} Z3 sessions in {artifact}")
        trace = parse_trace(trace_prefix, workload, solver_pid) if trace_prefix is not None else None
        row = {
            "schema": (
                CALIBRATION_ROW_SCHEMA
                if phase == "calibration" else ROW_SCHEMA
            ),
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
            "sampler_session_scans": sampling_quality["session_scan_count"],
            "sampler_session_scan_max_gap_ns": sampling_quality[
                "session_scan_max_gap_ns"
            ],
            "sampler_session_scan_max_duration_ns": sampling_quality[
                "session_scan_max_duration_ns"
            ],
            "solver_observation_sha256": solver_observation_sha256,
            "process_tree_sha256": process_tree_sha256,
            "cleanup_ok": "true",
            "artifact_dir": str(artifact.relative_to(self.output)),
        }
        if trace is not None:
            (artifact / "query-vector.json").write_bytes(canonical_json(trace))
        return RunOutcome(row, (process.returncode, stdout, stderr), trace["query_vector"] if trace else None)


def screen_failure_attempt_summary(
    screen: Screen | None,
) -> dict[str, Any]:
    if screen is not None:
        return screen.failure_attempt_summary()
    references: list[dict[str, Any]] = []
    return {
        "schema": "djex-benchmark-failure-attempt-set/v1",
        "count": 0,
        "manifests": references,
        "manifests_sha256": sha256_bytes(canonical_json(references)),
        "verification_pass": True,
        "verification_failures": [],
    }


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
        "schema": DECISION_SCHEMA,
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
            expected_mode=PINNED_Z3_SOURCE_MODE,
        ),
        "plain_descriptor_bound_mode": {
            "baseline": plain_descriptor_bound_mode_identity(baseline_root),
            "candidate": plain_descriptor_bound_mode_identity(candidate_root),
        },
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
            "scheduling": sampler_scheduling_policy(
                round(arguments.sample_interval_ms * 1_000_000)
            ),
            "minimum_wall_coverage": SAMPLER_MIN_WALL_COVERAGE,
            "maximum_mean_interval_multiplier": (
                SAMPLER_MAX_MEAN_INTERVAL_MULTIPLIER
            ),
            "maximum_interval_multiplier": SAMPLER_MAX_INTERVAL_MULTIPLIER,
            "maximum_pass_multiplier": SAMPLER_MAX_PASS_MULTIPLIER,
            "session_scan": {
                "session_id": "subprocess_root_pid",
                "before_capture": "every_sampler_pass",
                "post_capture_interval_ns": (
                    SAMPLER_SESSION_SCAN_POST_CAPTURE_NS
                ),
                "maximum_start_gap_ns": (
                    SAMPLER_SESSION_SCAN_MAX_START_GAP_NS
                ),
                "maximum_duration_ns": SAMPLER_SESSION_SCAN_MAX_DURATION_NS,
                "maximum_tgids_per_scan": SAMPLER_SESSION_SCAN_MAX_TGIDS,
            },
        },
        "outer_timeout_seconds": arguments.outer_timeout,
        "notes": arguments.notes,
    }


def verify_frozen_artifacts(provenance: dict[str, Any]) -> None:
    for key in ("baseline_binary", "candidate_binary", "z3"):
        identity = provenance[key]
        require(sha256_file(Path(identity["path"])) == identity["sha256"], f"{key} changed during screen")
        stat = Path(identity["path"]).stat()
        require(
            (
                stat.st_dev,
                stat.st_ino,
                stat.st_size,
                stat_module.S_IMODE(stat.st_mode),
            )
            == (
                identity["device"], identity["inode"], identity["size"],
                identity["mode"],
            ),
            f"{key} executable metadata changed during screen",
        )
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
        {
            "baseline": plain_descriptor_bound_mode_identity(
                Path(provenance["baseline_source"]["root"])
            ),
            "candidate": plain_descriptor_bound_mode_identity(
                Path(provenance["candidate_source"]["root"])
            ),
        }
        == provenance["plain_descriptor_bound_mode"],
        "plain descriptor-bound mode identity changed during screen",
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
        failure_attempts = screen_failure_attempt_summary(screen)
        if not failure_attempts["verification_pass"]:
            finalization_failures.append(
                "failure-attempt manifest verification failed: "
                + repr(failure_attempts["verification_failures"])
            )
        if provenance is not None:
            provenance["failure_attempts"] = failure_attempts
            try:
                write_json(output / "provenance.json", provenance)
            except BaseException as failure:
                finalization_failures.append(
                    "failure-attempt provenance anchoring failed: "
                    f"{type(failure).__name__}: {failure}"
                )
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
            "failure_attempts": failure_attempts,
            "completed_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        }
        if failures or analyzed_decision is None:
            decision = {
                "schema": DECISION_SCHEMA,
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
            "0.400000 setpgid(0, 0) = 0 <0.000001>",
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

        extra_trace = root / "strace.333"
        extra_trace.write_text(lines[1] + "\n")
        try:
            parse_trace(prefix, workload, solver_pid)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure(
                "second successful exact-target exec in another strace file "
                "was accepted"
            )
        finally:
            extra_trace.unlink()

        bad_prefix = root / "bad"
        (root / f"bad.{solver_pid}").write_text(
            "\n".join(lines[:2])
            + "\n3.000000 read(0<pipe:[11]>,  <unfinished ...>\n"
        )
        try:
            parse_trace(bad_prefix, workload, solver_pid)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure("synthetic unresolved trace record was accepted")


def check_proc_stat_parser() -> None:
    tail = [
        b"S", b"11", b"12", b"13", b"0", b"0", b"0", b"0",
        b"0", b"0", b"0", b"17", b"19", b"0", b"0", b"20",
        b"0", b"1", b"0", b"12345", b"4096", b"23",
    ]
    parsed = parse_proc_stat_bytes(
        b"321 (non-utf8-\xff-and-) delimiter) " + b" ".join(tail) + b"\n"
    )
    require(
        parsed == ProcStat(11, 12, 13, "S", 17, 19, 12345, 23),
        "byte-safe proc-stat parser drifted",
    )
    malformed = {
        b"321 no-delimiter": "missing_comm_delimiter",
        b"321 (comm) S 1 2": "short_tail",
        b"321 (comm) ? " + b" ".join(tail[1:]): "invalid_state",
        b"321 (comm) " + b" ".join(
            tail[:11] + [b"not-an-integer"] + tail[12:]
        ): "invalid_integer_11",
    }
    for source, reason in malformed.items():
        try:
            parse_proc_stat_bytes(source)
        except ProcStatMalformed as failure:
            require(
                failure.reason == reason,
                f"proc-stat malformed reason {failure.reason} != {reason}",
            )
        else:
            raise HarnessFailure(f"malformed proc-stat {reason} was accepted")


def check_sampler_fixed_rate_scheduler() -> None:
    class FakeStopEvent:
        def __init__(
            self, owner: "FixedRateSampler", *,
            stop_on_positive_wait: int | None = None,
        ) -> None:
            self.owner = owner
            self.stop_on_positive_wait = stop_on_positive_wait
            self.positive_waits = 0
            self.wait_calls: list[int] = []
            self.pending_wait: int | None = None
            self.stopped = False

        def set(self) -> None:
            self.stopped = True

        def wait(self, seconds: float) -> bool:
            wait_ns = round(seconds * 1_000_000_000)
            self.wait_calls.append(wait_ns)
            if self.stopped:
                return True
            if wait_ns > 0:
                self.positive_waits += 1
                if self.positive_waits == self.stop_on_positive_wait:
                    self.stopped = True
                    return True
            self.owner.clock += wait_ns
            self.pending_wait = wait_ns
            return False

    class FixedRateSampler(ProcessTreeSampler):
        def __init__(
            self, pass_durations: Sequence[int], *,
            stop_after: int | None = None,
            stop_on_positive_wait: int | None = None,
        ) -> None:
            # The fixed-rate loop needs only these explicitly modeled fields;
            # no procfs, process, thread, or wall clock enters this fixture.
            self.interval = 0.001
            self.lock = threading.Lock()
            self.clock = 0
            self.last_sample_ns: int | None = 0
            self.last_sample_finished_ns: int | None = 0
            self.first_sample_ns: int | None = 0
            self.sample_count = 1
            self.interval_sum_ns = 0
            self.max_interval_ns = 0
            self.max_pass_ns = 0
            self.pass_durations = list(pass_durations)
            self.stop_after = (
                len(self.pass_durations)
                if stop_after is None else stop_after
            )
            self.starts = [0]
            self.waits_before_samples: list[int] = []
            self.stop_event = FakeStopEvent(
                self, stop_on_positive_wait=stop_on_positive_wait,
            )

        def _monotonic_ns(self) -> int:
            return self.clock

        def _record_sample(
            self, *, force_session_scan: bool = False,
        ) -> None:
            require(
                not force_session_scan,
                "fixed-rate fixture received a forced session scan",
            )
            require(
                self.stop_event.pending_wait is not None,
                "fixed-rate sample lacks its preceding stop check",
            )
            wait_ns = self.stop_event.pending_wait
            self.stop_event.pending_wait = None
            self.waits_before_samples.append(wait_ns)
            sample_started = self.clock
            require(
                bool(self.pass_durations),
                "fixed-rate fixture exhausted its pass durations",
            )
            sample_duration = self.pass_durations.pop(0)
            self.clock += sample_duration
            previous_start = self.last_sample_ns
            require(
                previous_start is not None,
                "fixed-rate fixture lost its preceding sample",
            )
            interval = sample_started - previous_start
            self.interval_sum_ns += interval
            self.max_interval_ns = max(self.max_interval_ns, interval)
            self.last_sample_ns = sample_started
            self.last_sample_finished_ns = self.clock
            self.sample_count += 1
            self.max_pass_ns = max(self.max_pass_ns, sample_duration)
            self.starts.append(sample_started)
            if len(self.starts) - 1 >= self.stop_after:
                self.stop_event.set()

        def measured_mean_interval_ns(self) -> int:
            return self.interval_sum_ns // (self.sample_count - 1)

    sub_period = FixedRateSampler([200_000] * 5)
    sub_period._run_fixed_rate_samples()
    require(
        sub_period.starts
        == [0, 1_000_000, 2_000_000, 3_000_000, 4_000_000, 5_000_000]
        and sub_period.waits_before_samples
        == [1_000_000, 800_000, 800_000, 800_000, 800_000],
        "sub-period fixed-rate sampling accumulated relative-delay drift",
    )
    require(
        sub_period.measured_mean_interval_ns() == 1_000_000,
        "fixed-rate measured mean interval drifted",
    )

    one_overrun = FixedRateSampler([3_500_000, 100_000, 100_000])
    one_overrun._run_fixed_rate_samples()
    require(
        one_overrun.waits_before_samples == [1_000_000, 0, 1_000_000]
        and one_overrun.starts == [0, 1_000_000, 4_500_000, 5_600_000],
        "one overrun did not coalesce into exactly one immediate catch-up",
    )

    repeated_overruns = FixedRateSampler([1_500_000] * 6)
    repeated_overruns._run_fixed_rate_samples()
    repeated_waits = repeated_overruns.waits_before_samples
    require(
        repeated_waits == [1_000_000, 0, 1_000_000, 0, 1_000_000, 0]
        and all(
            not (left == 0 and right == 0)
            for left, right in zip(repeated_waits, repeated_waits[1:])
        ),
        "repeated overruns created consecutive immediate catch-ups",
    )

    stop_during_wait = FixedRateSampler(
        [100_000], stop_on_positive_wait=1,
    )
    stop_during_wait._run_fixed_rate_samples()
    require(
        stop_during_wait.starts == [0]
        and stop_during_wait.stop_event.wait_calls == [1_000_000],
        "stop during a scheduled wait allowed another sample",
    )

    stop_before_catch_up = FixedRateSampler(
        [1_500_000, 100_000], stop_after=1,
    )
    stop_before_catch_up._run_fixed_rate_samples()
    require(
        stop_before_catch_up.starts == [0, 1_000_000]
        and stop_before_catch_up.stop_event.wait_calls
        == [1_000_000, 0],
        "stop before an immediate catch-up allowed another sample",
    )

    exact_equality = FixedRateSampler([1_000_000, 100_000, 100_000])
    exact_equality._run_fixed_rate_samples()
    require(
        exact_equality.starts == [0, 1_000_000, 2_000_000, 3_000_000]
        and exact_equality.waits_before_samples
        == [1_000_000, 0, 900_000],
        "exact-deadline equality was treated as an overrun",
    )


def check_sampler_capture_protocol() -> None:
    with tempfile.TemporaryDirectory(
        prefix="djex-solver-sampler-self-check-"
    ) as source:
        root = Path(source)
        image_bytes = b"synthetic sealed solver image\n"
        image = root / "solver-image"
        alternate = root / "alternate-image"
        retained = root / "retained-image"
        wrong_mode = root / "wrong-mode-image"
        for path, contents in (
            (image, image_bytes),
            (alternate, b"different sealed solver bytes\n"),
            (retained, image_bytes),
            (wrong_mode, image_bytes),
        ):
            require(
                len(contents) == len(image_bytes),
                "synthetic sampler image sizes drifted",
            )
            path.write_bytes(contents)
            path.chmod(SEALED_SOLVER_MODE)
        wrong_mode.chmod(0o500)

        class SyntheticSampler(ProcessTreeSampler):
            def __init__(
                self, executable: Path, solver_pids: Sequence[int] = (200,),
            ) -> None:
                super().__init__(
                    100, sha256_file(executable), str(executable), 0.001,
                )
                self.executable = executable
                self.alternate = alternate
                self.clock = 0
                self.tgids = [100, *solver_pids]
                self.stats: dict[int, ProcStat] = {
                    100: ProcStat(1, 100, 100, "S", 1, 1, 10, 5),
                    **{
                        pid: ProcStat(100, 700 + pid, 100, "S", 2, 3, pid, 7)
                        for pid in solver_pids
                    },
                }
                self.readlink_calls: dict[int, int] = {}
                self.fail_cmdline = 0
                self.signature_mismatch = 0
                self.intermediate_cmdline_shapes: list[int] = []
                self.parsed_cmdline = False
                self.stat_calls: dict[int, int] = {}
                self.malformed_stat_after = False
                self.malformed_pids: dict[int, str] = {}
                self.transient_malformed_pids: dict[int, tuple[int, str]] = {}
                self.reexec_after_open = False
                self.live_inode_mismatch = False
                self.persistent_bad_cmdline_pids: set[int] = set()
                self.raise_open_telemetry = False
                self.raise_post_open_gate = False
                self.raise_argv_telemetry = False
                self.raise_transfer = False
                self.raise_after_transfer = False
                self.raise_before_close = False
                self.raise_after_close = False
                self.fail_emergency_close = 0
                self.raise_discard_transfer = False
                self.raise_snapshot_transfer = False

            def _monotonic_ns(self) -> int:
                self.clock += 100_000
                return self.clock

            def _task_children(self, parent: int, now: int) -> list[int]:
                # Force discovery to depend entirely on the SID fallback.
                return []

            def _top_level_tgids(self) -> list[int]:
                return list(self.tgids)

            def _read_proc_stat(self, pid: int) -> ProcStat:
                calls = self.stat_calls.get(pid, 0) + 1
                self.stat_calls[pid] = calls
                transient = self.transient_malformed_pids.get(pid)
                if transient is not None and transient[0] > 0:
                    remaining, reason = transient
                    self.transient_malformed_pids[pid] = (
                        remaining - 1, reason,
                    )
                    raise ProcStatMalformed(reason)
                if pid in self.malformed_pids:
                    raise ProcStatMalformed(self.malformed_pids[pid])
                if self.malformed_stat_after and pid != 100 and calls == 2:
                    raise ProcStatMalformed("synthetic_stat_after")
                try:
                    return self.stats[pid]
                except KeyError as failure:
                    raise FileNotFoundError(
                        errno.ENOENT, "synthetic process exited"
                    ) from failure

            @staticmethod
            def _pid_from_proc_path(path: Path) -> int:
                return int(path.parts[-2])

            def _readlink(self, path: Path) -> str:
                pid = self._pid_from_proc_path(path)
                calls = self.readlink_calls.get(pid, 0) + 1
                self.readlink_calls[pid] = calls
                if pid == 100:
                    return "/synthetic/root"
                if self.reexec_after_open and calls > 1:
                    return "/synthetic/reexec"
                return SEALED_SOLVER_TARGET

            def _read_bytes(self, path: Path) -> bytes:
                pid = self._pid_from_proc_path(path)
                require(pid != 100, "synthetic root metadata was inspected")
                if path.name == "cmdline":
                    if self.fail_cmdline:
                        self.fail_cmdline -= 1
                        raise PermissionError(errno.EACCES, "synthetic EACCES")
                    arguments = [
                        self.solver_argv0,
                        b"-in",
                        b"-smt2",
                        b"smtlib2_compliant=true",
                        b"timeout=1000",
                        b"rlimit=100000",
                    ]
                    if self.signature_mismatch:
                        self.signature_mismatch -= 1
                        arguments[-1] = b"rlimit=99999"
                    if pid in self.persistent_bad_cmdline_pids:
                        arguments[-1] = b"rlimit=persistent-bad"
                    if self.parsed_cmdline:
                        arguments = arguments[:3] + [
                            b"smtlib2_compliant", b"true",
                            b"timeout", b"1000", b"rlimit", b"100000",
                        ]
                    elif self.intermediate_cmdline_shapes:
                        split_count = self.intermediate_cmdline_shapes.pop(0)
                        settings = (
                            (b"smtlib2_compliant", b"true"),
                            (b"timeout", b"1000"),
                            (b"rlimit", b"100000"),
                        )
                        arguments = arguments[:3]
                        for position, (key, value) in enumerate(settings):
                            arguments.extend(
                                (key, value) if position < split_count
                                else (key + b"=" + value,)
                            )
                    return b"\0".join(arguments) + b"\0"
                require(path.name == "environ", "synthetic proc metadata path")
                return b""

            def _open_executable(self, path: Path) -> int:
                return os.open(self.executable, os.O_RDONLY | os.O_CLOEXEC)

            def _fstat(self, descriptor: int) -> os.stat_result:
                if self.raise_post_open_gate:
                    raise HarnessFailure("synthetic post-open gate failure")
                return super()._fstat(descriptor)

            def _note_gate_success(
                self, pid: int, stage: str, now: int,
            ) -> None:
                if self.raise_open_telemetry and stage == "open_exe":
                    raise HarnessFailure(
                        "synthetic open telemetry failure"
                    )
                super()._note_gate_success(pid, stage, now)

            def _note_argv_observation(
                self, pid: int, command_line: bytes,
                arguments: list[bytes], expected_arguments: list[bytes],
                shape: str, now: int,
            ) -> None:
                if self.raise_argv_telemetry:
                    raise HarnessFailure("synthetic argv telemetry failure")
                super()._note_argv_observation(
                    pid, command_line, arguments, expected_arguments,
                    shape, now,
                )

            def _transfer_solver_image(
                self, pid: int, captured: dict[str, Any],
                start_time: int, now: int,
            ) -> None:
                if self.raise_transfer:
                    raise HarnessFailure("synthetic ownership transfer failure")
                super()._transfer_solver_image(
                    pid, captured, start_time, now,
                )
                if self.raise_after_transfer:
                    raise HarnessFailure(
                        "synthetic post-transfer telemetry failure"
                    )

            def _close_executable_descriptor(self, descriptor: int) -> None:
                if self.raise_before_close:
                    raise OSError(
                        errno.EIO, "synthetic pre-close failure"
                    )
                super()._close_executable_descriptor(descriptor)
                if self.raise_after_close:
                    raise OSError(errno.EIO, "synthetic close report failure")

            def _emergency_close_executable_descriptor(
                self, descriptor: int,
            ) -> None:
                if self.fail_emergency_close:
                    self.fail_emergency_close -= 1
                    raise OSError(
                        errno.EIO, "synthetic emergency-close failure"
                    )
                super()._emergency_close_executable_descriptor(descriptor)

            def _discard_transfer_hook(self) -> None:
                if self.raise_discard_transfer:
                    raise HarnessFailure(
                        "synthetic discard transfer failure"
                    )

            def _snapshot_transfer_hook(self) -> None:
                if self.raise_snapshot_transfer:
                    raise HarnessFailure(
                        "synthetic snapshot transfer failure"
                    )

            def _stat_executable(self, path: Path) -> os.stat_result:
                return os.stat(
                    self.alternate if self.live_inode_mismatch
                    else self.executable
                )

        fallback = SyntheticSampler(image)
        fallback._record_sample(force_session_scan=True)
        require(
            len(fallback.solver_images) == 1,
            "session fallback did not capture the solver",
        )
        fallback_diagnostics = fallback.diagnostics()
        solver_telemetry = fallback_diagnostics["pid_telemetry"]["200"]
        require(
            solver_telemetry["capture_sources"] == {"session_scan": 1}
            and solver_telemetry["discovery_counts"].get("children", 0) == 0,
            "session fallback capture source drifted",
        )
        cadence_last = fallback.session_scan_last_ns
        require(cadence_last is not None, "synthetic cadence lacks first scan")
        cadence_count = fallback.session_scan_count
        fallback._sample(
            cadence_last + SAMPLER_SESSION_SCAN_POST_CAPTURE_NS - 1
        )
        require(
            fallback.session_scan_count == cadence_count,
            "post-capture session scan ran below its exact cadence bound",
        )
        fallback.clock = (
            cadence_last + SAMPLER_SESSION_SCAN_POST_CAPTURE_NS - 100_000
        )
        fallback._sample(
            cadence_last + SAMPLER_SESSION_SCAN_POST_CAPTURE_NS
        )
        require(
            fallback.session_scan_count == cadence_count + 1,
            "post-capture session scan did not run at its exact cadence bound",
        )
        fallback_diagnostics = fallback.diagnostics()
        solver_telemetry = fallback_diagnostics["pid_telemetry"]["200"]
        require(
            canonical_json(fallback_diagnostics)
            == canonical_json(fallback.diagnostics()),
            "sampler telemetry serialization is unstable",
        )
        fallback_snapshot = fallback.solver_snapshot()
        _, fallback_image = require_single_solver_image(fallback_snapshot)
        require(
            fallback_image["sha256"] == sha256_bytes(image_bytes),
            "synthetic fallback image hash drifted",
        )
        require(
            exact_target_identity_attestation(
                fallback.diagnostics(), fallback_snapshot,
            )["pass"],
            "exact source-derived mode did not pass identity attestation",
        )
        require(
            fallback_image["cmdline_shape"] == "exec_exact"
            and fallback_image["mode"] == SEALED_SOLVER_MODE
            and solver_telemetry["cmdline_shapes"] == {"exec_exact": 1}
            and solver_telemetry["executable_mode_first"]
            == SEALED_SOLVER_MODE
            and solver_telemetry["executable_mode_last"]
            == SEALED_SOLVER_MODE
            and solver_telemetry["executable_mode_counts"]
            == {str(SEALED_SOLVER_MODE): 1}
            and solver_telemetry["gate_successes"].get("captured") == 1,
            "pristine exec cmdline or exact source-derived mode drifted",
        )

        parsed = SyntheticSampler(image)
        parsed.parsed_cmdline = True
        parsed._record_sample(force_session_scan=True)
        _, parsed_image = require_single_solver_image(parsed.solver_snapshot())
        require(
            parsed_image["cmdline_shape"] == "z3_4_8_12_parsed_exact"
            and parsed.diagnostics()["pid_telemetry"]["200"]
            ["cmdline_shapes"] == {"z3_4_8_12_parsed_exact": 1},
            "fully parsed Z3 4.8.12 cmdline shape was rejected",
        )

        rejected_fixed_mode = SyntheticSampler(wrong_mode)
        descriptors_before = len(os.listdir("/proc/self/fd"))
        rejected_fixed_mode._record_sample(force_session_scan=True)
        rejected_fixed_snapshot = rejected_fixed_mode.solver_snapshot()
        rejected_fixed_telemetry = rejected_fixed_mode.diagnostics()[
            "pid_telemetry"
        ]["200"]
        require(
            not rejected_fixed_mode.solver_images
            and rejected_fixed_telemetry["consistency_mismatches"]
            == {"mode": 1}
            and rejected_fixed_telemetry["executable_mode_first"] == 0o500
            and rejected_fixed_telemetry["executable_mode_last"] == 0o500
            and rejected_fixed_telemetry["executable_mode_counts"]
            == {str(0o500): 1}
            and not exact_target_identity_attestation(
                rejected_fixed_mode.diagnostics(), rejected_fixed_snapshot,
            )["pass"]
            and len(os.listdir("/proc/self/fd")) == descriptors_before,
            "access-checked fixed mode 0500 was accepted by the plain-launch "
            "sampler",
        )

        for split_count in (1, 2):
            intermediate = SyntheticSampler(image)
            intermediate.intermediate_cmdline_shapes = [split_count]
            intermediate._record_sample(force_session_scan=True)
            require(
                not intermediate.solver_images,
                f"intermediate-{split_count} Z3 cmdline mutation was accepted",
            )
            intermediate._record_sample(force_session_scan=True)
            intermediate_telemetry = intermediate.diagnostics()[
                "pid_telemetry"
            ]["200"]
            require(
                intermediate_telemetry["cmdline_shapes"]
                == {"exec_exact": 1, "intermediate": 1}
                and intermediate_telemetry["signature_mismatches"]
                == {"argv_intermediate": 1},
                f"intermediate-{split_count} Z3 cmdline mutation was not "
                "retried/categorized",
            )
            intermediate_snapshot = intermediate.solver_snapshot()
            require_single_solver_image(intermediate_snapshot)
            require(
                exact_target_identity_attestation(
                    intermediate.diagnostics(), intermediate_snapshot,
                )["pass"],
                f"intermediate-{split_count} cmdline was not limited to its "
                "eventual identity",
            )

        transient = SyntheticSampler(image)
        transient.fail_cmdline = 1
        transient._record_sample(force_session_scan=True)
        require(not transient.solver_images, "transient procfs failure was ignored")
        transient._record_sample(force_session_scan=True)
        transient_telemetry = transient.diagnostics()["pid_telemetry"]["200"]
        require(
            transient_telemetry["errors"]
            == {"cmdline_read": {"EACCES": 1}},
            "procfs errno telemetry drifted",
        )
        require_single_solver_image(transient.solver_snapshot())

        signature = SyntheticSampler(image)
        signature.signature_mismatch = 1
        signature._record_sample(force_session_scan=True)
        require(not signature.solver_images, "signature mismatch was accepted")
        signature._record_sample(force_session_scan=True)
        signature_telemetry = signature.diagnostics()["pid_telemetry"]["200"]
        require(
            signature_telemetry["signature_mismatches"]
            == {"argv_other": 1}
            and signature_telemetry["cmdline_shapes"]
            == {"exec_exact": 1, "other": 1},
            "signature mismatch telemetry drifted",
        )
        require_single_solver_image(signature.solver_snapshot())

        second_bad_identity = SyntheticSampler(image, (200, 201))
        second_bad_identity.persistent_bad_cmdline_pids.add(201)
        second_bad_identity._record_sample(force_session_scan=True)
        second_bad_snapshot = second_bad_identity.solver_snapshot()
        require(
            len(second_bad_snapshot) == 1,
            "persistent bad-argv identity changed accepted capture count",
        )
        second_bad_attestation = exact_target_identity_attestation(
            second_bad_identity.diagnostics(), second_bad_snapshot,
        )
        require(
            not second_bad_attestation["pass"]
            and len(second_bad_attestation[
                "observed_exact_target_identities"
            ]) == 2
            and len(second_bad_attestation["captured_image_identities"]) == 1,
            "second exact-target identity escaped cardinality HOLD",
        )

        malformed_after = SyntheticSampler(image)
        malformed_after.malformed_stat_after = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        malformed_after._record_sample(force_session_scan=True)
        require(
            not malformed_after.solver_images
            and malformed_after.diagnostics()["pid_telemetry"]["200"]
            ["errors"]
            == {"stat_after": {"PROC_STAT_SYNTHETIC_STAT_AFTER": 1}}
            and len(os.listdir("/proc/self/fd")) == descriptors_before,
            "malformed stat-after was not categorized/retried without fd leak",
        )
        malformed_after._record_sample(force_session_scan=True)
        require_single_solver_image(malformed_after.solver_snapshot())

        global_races = SyntheticSampler(image, ())
        global_races.tgids.extend((400, 401))
        global_races.stats[400] = ProcStat(
            1, 400, 999, "S", 1, 1, 400, 1,
        )
        global_races.transient_malformed_pids[400] = (
            1, "synthetic_global_tail",
        )
        global_races._record_sample(force_session_scan=True)
        global_diagnostics = global_races.diagnostics()
        require(
            "400" not in global_diagnostics["pid_telemetry"]
            and "401" not in global_diagnostics["pid_telemetry"]
            and global_diagnostics["session_scan"]["errors"]
            == {
                "session_scan_stat_resolved": {
                    "PROC_STAT_SYNTHETIC_GLOBAL_TAIL": 1,
                },
                "session_scan_stat_unresolved_initial": {"ENOENT": 1},
                "session_scan_stat_unresolved_retry": {"ENOENT": 1},
            },
            "foreign global-stat races were not bounded/categorized",
        )

        unresolved_global = SyntheticSampler(image, ())
        unresolved_global.tgids.append(402)
        unresolved_global.malformed_pids[402] = "synthetic_unresolved_tail"
        unresolved_global._record_sample(force_session_scan=True)
        require(
            unresolved_global.diagnostics()["session_scan"]["errors"]
            == {
                "session_scan_stat_unresolved_initial": {
                    "PROC_STAT_SYNTHETIC_UNRESOLVED_TAIL": 1,
                },
                "session_scan_stat_unresolved_retry": {
                    "PROC_STAT_SYNTHETIC_UNRESOLVED_TAIL": 1,
                },
            },
            "unresolved malformed global TGID was not fail-closed telemetry",
        )

        zombie = SyntheticSampler(image)
        zombie.stats[200] = ProcStat(100, 900, 100, "Z", 2, 3, 200, 0)
        zombie._record_sample(force_session_scan=True)
        try:
            require_single_solver_image(zombie.solver_snapshot())
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure("zombie-only solver observation was accepted")
        require(
            zombie.diagnostics()["pid_telemetry"]["200"]
            ["consistency_mismatches"].get("zombie_before", 0) >= 1,
            "zombie observation was not classified",
        )

        reused = SyntheticSampler(image)
        reused._record_sample(force_session_scan=True)
        reused.stats[200] = ProcStat(100, 900, 100, "S", 2, 3, 999, 7)
        try:
            reused._record_sample(force_session_scan=True)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure("PID reuse was accepted")
        reused._discard_solver_descriptors()

        inode_drift = SyntheticSampler(image)
        inode_drift.live_inode_mismatch = True
        inode_drift._record_sample(force_session_scan=True)
        require(
            not inode_drift.solver_images
            and inode_drift.diagnostics()["pid_telemetry"]["200"]
            ["consistency_mismatches"] == {"live_fstat": 1},
            "same-target/different-inode capture was accepted",
        )

        captured_inode_drift = SyntheticSampler(image)
        captured_inode_drift._record_sample(force_session_scan=True)
        captured_inode_drift.live_inode_mismatch = True
        try:
            captured_inode_drift._record_sample(force_session_scan=True)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure(
                "captured same-target/different-inode re-exec was accepted"
            )
        require(
            captured_inode_drift.diagnostics()["pid_telemetry"]["200"]
            ["consistency_mismatches"]
            == {"captured_live_identity": 1},
            "captured executable identity drift was not categorized",
        )
        captured_inode_drift._discard_solver_descriptors()

        reexec = SyntheticSampler(image)
        reexec.reexec_after_open = True
        reexec._record_sample(force_session_scan=True)
        require(
            not reexec.solver_images
            and reexec.diagnostics()["pid_telemetry"]["200"]
            ["consistency_mismatches"] == {"target": 1},
            "mid-capture re-exec was accepted",
        )

        sessions = SyntheticSampler(image, (200, 300))
        sessions.stats[300] = ProcStat(100, 100, 999, "S", 1, 1, 300, 4)
        sessions._record_sample(force_session_scan=True)
        session_snapshot = sessions.solver_snapshot()
        require(
            set(session_snapshot) == {200},
            "different-session process entered solver capture",
        )

        duplicate = SyntheticSampler(image, (200, 201))
        duplicate._record_sample(force_session_scan=True)
        duplicate_snapshot = duplicate.solver_snapshot()
        try:
            require_single_solver_image(duplicate_snapshot)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure("two sealed solver images were accepted")

        retained_sampler = SyntheticSampler(retained)
        retained_sampler._record_sample(force_session_scan=True)
        retained_descriptor = retained_sampler.solver_images[200]["descriptor"]
        retained.unlink()
        del retained_sampler.stats[200]
        retained_sampler.tgids.remove(200)
        retained_snapshot = retained_sampler.solver_snapshot()
        _, retained_image = require_single_solver_image(retained_snapshot)
        require(
            retained_image["sha256"] == sha256_bytes(image_bytes),
            "held executable descriptor did not survive process/image exit",
        )
        try:
            os.fstat(retained_descriptor)
        except OSError as failure:
            require(failure.errno == errno.EBADF, "retained descriptor close errno")
        else:
            raise HarnessFailure("solver snapshot leaked its held descriptor")

        for attribute, expected_text in (
            ("raise_open_telemetry", "synthetic open telemetry failure"),
            ("raise_post_open_gate", "synthetic post-open gate failure"),
            ("raise_argv_telemetry", "synthetic argv telemetry failure"),
            ("raise_transfer", "synthetic ownership transfer failure"),
            (
                "raise_after_transfer",
                "synthetic post-transfer telemetry failure",
            ),
        ):
            injected = SyntheticSampler(image)
            setattr(injected, attribute, True)
            descriptors_before = len(os.listdir("/proc/self/fd"))
            try:
                injected._record_sample(force_session_scan=True)
            except HarnessFailure as failure:
                require(
                    expected_text in str(failure),
                    f"{attribute} primary failure precedence drifted",
                )
            else:
                raise HarnessFailure(f"{attribute} failure was accepted")
            finally:
                injected._discard_solver_descriptors()
            require(
                len(os.listdir("/proc/self/fd")) == descriptors_before,
                f"{attribute} leaked a retained executable descriptor",
            )

        close_only = SyntheticSampler(image)
        close_only.signature_mismatch = 1
        close_only.raise_after_close = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        try:
            close_only._record_sample(force_session_scan=True)
        except HarnessFailure as failure:
            require(
                "cannot close rejected solver executable descriptor"
                in str(failure),
                "descriptor-close-only failure was not explicit",
            )
        else:
            raise HarnessFailure("descriptor close failure was accepted")
        require(
            len(os.listdir("/proc/self/fd")) == descriptors_before,
            "reported descriptor close failure leaked an fd",
        )

        primary_and_close = SyntheticSampler(image)
        primary_and_close.raise_post_open_gate = True
        primary_and_close.raise_after_close = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        try:
            primary_and_close._record_sample(force_session_scan=True)
        except HarnessFailure as failure:
            require(
                "synthetic post-open gate failure" in str(failure)
                and "synthetic close report failure" in str(failure),
                "primary/close failure aggregation drifted",
            )
        else:
            raise HarnessFailure("primary plus close failure was accepted")
        require(
            len(os.listdir("/proc/self/fd")) == descriptors_before,
            "primary plus reported close failure leaked an fd",
        )

        cleanup_registry_retry = SyntheticSampler(image)
        cleanup_registry_retry.signature_mismatch = 1
        cleanup_registry_retry.raise_before_close = True
        cleanup_registry_retry.fail_emergency_close = 1
        descriptors_before = len(os.listdir("/proc/self/fd"))
        try:
            cleanup_registry_retry._record_sample(force_session_scan=True)
        except HarnessFailure as failure:
            require(
                "synthetic pre-close failure" in str(failure)
                and "synthetic emergency-close failure" in str(failure)
                and len(cleanup_registry_retry.cleanup_descriptors) == 1
                and len(os.listdir("/proc/self/fd"))
                == descriptors_before + 1,
                "uncertain rejected-capture close was not retained",
            )
        else:
            raise HarnessFailure(
                "uncertain rejected-capture close was accepted"
            )
        cleanup_registry_retry.raise_before_close = False
        cleanup_retry_failures: list[str] = []
        require(
            finalize_sampler_images(
                cleanup_registry_retry, cleanup_retry_failures,
                "production run",
            ) == {}
            and not cleanup_retry_failures
            and not cleanup_registry_retry.cleanup_descriptors
            and len(os.listdir("/proc/self/fd")) == descriptors_before,
            "successful snapshot did not drain the descriptor retry registry",
        )

        class SnapshotFailureSampler(SyntheticSampler):
            def solver_snapshot(self) -> dict[int, dict[str, Any]]:
                raise HarnessFailure("synthetic production snapshot failure")

        snapshot_failure = SnapshotFailureSampler(image)
        descriptors_before = len(os.listdir("/proc/self/fd"))
        snapshot_failure._record_sample(force_session_scan=True)
        snapshot_failures: list[str] = []
        require(
            finalize_sampler_images(
                snapshot_failure, snapshot_failures, "production run",
            ) == {}
            and len(snapshot_failures) == 1
            and "synthetic production snapshot failure"
            in snapshot_failures[0]
            and len(os.listdir("/proc/self/fd")) == descriptors_before,
            "production snapshot failure did not fail-safe close its fd",
        )

        snapshot_transfer_failure = SyntheticSampler(image)
        snapshot_transfer_failure._record_sample(force_session_scan=True)
        snapshot_transfer_failure.raise_snapshot_transfer = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        snapshot_transfer_failures: list[str] = []
        require(
            finalize_sampler_images(
                snapshot_transfer_failure, snapshot_transfer_failures,
                "production run",
            ) == {}
            and len(snapshot_transfer_failures) == 1
            and "synthetic snapshot transfer failure"
            in snapshot_transfer_failures[0]
            and len(os.listdir("/proc/self/fd")) == descriptors_before - 1,
            "snapshot ownership-transfer failure leaked or lost its fd",
        )

        snapshot_transfer_and_close = SyntheticSampler(image)
        snapshot_transfer_and_close._record_sample(
            force_session_scan=True
        )
        snapshot_transfer_and_close.raise_snapshot_transfer = True
        snapshot_transfer_and_close.raise_after_close = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        snapshot_transfer_close_failures: list[str] = []
        require(
            finalize_sampler_images(
                snapshot_transfer_and_close,
                snapshot_transfer_close_failures,
                "production run",
            ) == {}
            and len(snapshot_transfer_close_failures) == 1
            and "synthetic snapshot transfer failure"
            in snapshot_transfer_close_failures[0]
            and "synthetic close report failure"
            in snapshot_transfer_close_failures[0]
            and len(os.listdir("/proc/self/fd")) == descriptors_before - 1,
            "snapshot transfer/close primary precedence or fd cleanup drifted",
        )

        snapshot_cleanup_failure = SnapshotFailureSampler(image)
        snapshot_cleanup_failure.raise_after_close = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        snapshot_cleanup_failure._record_sample(force_session_scan=True)
        snapshot_cleanup_failures: list[str] = []
        require(
            finalize_sampler_images(
                snapshot_cleanup_failure, snapshot_cleanup_failures,
                "production run",
            ) == {}
            and len(snapshot_cleanup_failures) == 2
            and "synthetic production snapshot failure"
            in snapshot_cleanup_failures[0]
            and "solver descriptor discard failure"
            in snapshot_cleanup_failures[1]
            and len(os.listdir("/proc/self/fd")) == descriptors_before,
            "snapshot/discard failure precedence or fd cleanup drifted",
        )

        snapshot_preclose_failure = SnapshotFailureSampler(image)
        snapshot_preclose_failure.raise_before_close = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        snapshot_preclose_failure._record_sample(force_session_scan=True)
        snapshot_preclose_failures: list[str] = []
        require(
            finalize_sampler_images(
                snapshot_preclose_failure, snapshot_preclose_failures,
                "production run",
            ) == {}
            and len(snapshot_preclose_failures) == 2
            and "synthetic production snapshot failure"
            in snapshot_preclose_failures[0]
            and "synthetic pre-close failure"
            in snapshot_preclose_failures[1]
            and not snapshot_preclose_failure.cleanup_descriptors
            and len(os.listdir("/proc/self/fd")) == descriptors_before,
            "pre-close discard failure lost ownership or leaked its fd",
        )

        snapshot_discard_transfer = SnapshotFailureSampler(image)
        snapshot_discard_transfer.raise_discard_transfer = True
        descriptors_before = len(os.listdir("/proc/self/fd"))
        snapshot_discard_transfer._record_sample(force_session_scan=True)
        snapshot_discard_transfer_failures: list[str] = []
        require(
            finalize_sampler_images(
                snapshot_discard_transfer,
                snapshot_discard_transfer_failures,
                "production run",
            ) == {}
            and len(snapshot_discard_transfer_failures) == 2
            and "synthetic production snapshot failure"
            in snapshot_discard_transfer_failures[0]
            and "synthetic discard transfer failure"
            in snapshot_discard_transfer_failures[1]
            and not snapshot_discard_transfer.cleanup_descriptors
            and len(os.listdir("/proc/self/fd")) == descriptors_before,
            "discard ownership-transfer failure lost or leaked its fd",
        )

        execute_source = inspect.getsource(Screen._execute_once)
        require(
            "finalize_sampler_images(" in execute_source
            and "sampler.solver_snapshot()" not in execute_source,
            "production Screen bypassed fail-safe solver finalization",
        )

        finalization_sampler = SyntheticSampler(image)
        finalization_sampler._record_sample(force_session_scan=True)
        finalization_descriptor = finalization_sampler.solver_images[200][
            "descriptor"
        ]
        synthetic_primary = HarnessFailure("synthetic later finalization failure")
        finalization_failures: list[str] = []
        finalization_images = finalize_sampler_images(
            finalization_sampler, finalization_failures,
            "synthetic finalization",
        )
        require(
            synthetic_primary.args == ("synthetic later finalization failure",)
            and not finalization_failures
            and len(finalization_images) == 1,
            "captured-image finalization changed primary precedence",
        )
        try:
            os.fstat(finalization_descriptor)
        except OSError as failure:
            require(
                failure.errno == errno.EBADF,
                "finalization descriptor close errno",
            )
        else:
            raise HarnessFailure(
                "captured image leaked after later finalization failure"
            )


def check_live_sealed_memfd_session_fallback() -> None:
    helper = r'''
import ctypes
import os
import sys
import time

source = sys.argv[1]
short = sys.argv[2] == "short"
descriptor = os.memfd_create(
    "djex-z3-main-image", os.MFD_CLOEXEC | os.MFD_ALLOW_SEALING
)
with open(source, "rb") as handle:
    while True:
        block = handle.read(1024 * 1024)
        if not block:
            break
        view = memoryview(block)
        while view:
            written = os.write(descriptor, view)
            if written <= 0:
                raise RuntimeError("short memfd write")
            view = view[written:]
# Match the production Linux REPL's plain descriptor-bound launch: copy the
# pinned source executable's ordinary rwx mode (0755), rather than the fixed
# 0500 transport mode used only by the access-checked launch variants.
os.fchmod(descriptor, 0o755)
import fcntl
fcntl.fcntl(descriptor, 1033, 0x0001 | 0x0002 | 0x0004 | 0x0008)
input_read, input_write = os.pipe()
pid = os.fork()
if pid == 0:
    os.setpgid(0, 0)
    os.close(input_write)
    os.dup2(input_read, 0)
    if input_read != 0:
        os.close(input_read)
    values = [
        source.encode(), b"-in", b"-smt2",
        b"smtlib2_compliant=true", b"timeout=1000", b"rlimit=100000",
    ]
    argv = (ctypes.c_char_p * (len(values) + 1))(*values, None)
    envp = (ctypes.c_char_p * 1)(None)
    libc = ctypes.CDLL(None, use_errno=True)
    libc.syscall(322, descriptor, b"", argv, envp, 0x1000)
    os._exit(127)
os.close(descriptor)
os.close(input_read)
target = "/memfd:djex-z3-main-image (deleted)"
deadline = time.monotonic() + 5
while True:
    waited, status = os.waitpid(pid, os.WNOHANG)
    if waited == pid:
        print(f"child-exited-before-ready:{status}", file=sys.stderr, flush=True)
        os.close(input_write)
        raise SystemExit(1)
    try:
        if os.readlink(f"/proc/{pid}/exe") == target:
            break
    except (FileNotFoundError, ProcessLookupError, PermissionError):
        pass
    if time.monotonic() >= deadline:
        print("child-readiness-timeout", file=sys.stderr, flush=True)
        os.close(input_write)
        os.kill(pid, 9)
        os.waitpid(pid, 0)
        raise SystemExit(1)
    time.sleep(0.001)
print(pid, flush=True)
if short:
    os.close(input_write)
    os.waitpid(pid, 0)
    print("exited", flush=True)
while True:
    time.sleep(1)
'''

    class SessionOnlySampler(ProcessTreeSampler):
        def _task_children(self, parent: int, now: int) -> list[int]:
            return []

    def launch(short: bool) -> subprocess.Popen[bytes]:
        return subprocess.Popen(
            [
                str(PINNED_PYTHON), "-B", "-c", helper,
                str(PINNED_Z3), "short" if short else "live",
            ],
            stdin=subprocess.DEVNULL if short else subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            start_new_session=True,
        )

    def bounded_failure_snapshot(
        sampler: ProcessTreeSampler, child_pid: int,
    ) -> str:
        diagnostics = sampler.diagnostics()
        telemetry = diagnostics["pid_telemetry"]
        return canonical_json({
            "child_pid": child_pid,
            "child": telemetry.get(str(child_pid)),
            "root_pid": sampler.root_pid,
            "root": telemetry.get(str(sampler.root_pid)),
            "known_pids": diagnostics["known_pids"],
            "sampler_error": diagnostics["sampler_error"],
            "session_scan": diagnostics["session_scan"],
        }).decode().strip()

    def raise_fixture_failures(
        label: str, primary: BaseException | None,
        finalization: list[str], snapshot: str | None,
        helper_stderr: bytes,
    ) -> None:
        details = list(finalization)
        if snapshot is not None:
            details.append(f"sampler_snapshot={snapshot}")
        if helper_stderr:
            bounded = helper_stderr[:4096]
            details.append(
                f"helper_stderr_bytes={len(helper_stderr)} "
                f"helper_stderr_sha256={sha256_bytes(helper_stderr)} "
                f"helper_stderr_prefix={bounded!r}"
            )
        suffix = "" if not details else "; finalization=" + repr(details)
        if primary is not None:
            raise HarnessFailure(
                f"{label} primary failure: {type(primary).__name__}: "
                f"{primary}{suffix}"
            ) from primary
        require(not details, f"{label} finalization failure: {details}")

    live = launch(False)
    live_sampler: SessionOnlySampler | None = None
    live_sampler_stopped = False
    live_child_pid = -1
    live_primary: BaseException | None = None
    live_finalization: list[str] = []
    live_failure_snapshot: str | None = None
    live_stderr = b""
    try:
        require(live.stdout is not None, "live memfd helper lacks stdout")
        child_line = live.stdout.readline()
        require(
            child_line.strip().isdigit(),
            f"live memfd helper did not report its child: {child_line!r}",
        )
        live_child_pid = int(child_line)
        live_sampler = SessionOnlySampler(
            live.pid, PINNED_Z3_SHA256, str(PINNED_Z3), 0.001,
        )
        live_sampler.start()
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            if live_sampler.metrics()[2] == 1:
                break
            if live_sampler.error is not None:
                raise HarnessFailure(
                    "live session sampler failed early: "
                    + bounded_failure_snapshot(live_sampler, live_child_pid)
                )
            time.sleep(0.001)
        require(
            live_sampler.metrics()[2] == 1,
            "live session fallback did not capture sealed Z3: "
            + bounded_failure_snapshot(live_sampler, live_child_pid),
        )
        # Exercise the fixed 50 ms post-capture full-SID audit cadence.
        time.sleep(0.06)
        live_sampler.stop()
        live_sampler_stopped = True
    except BaseException as failure:
        live_primary = failure
        if live_sampler is not None:
            try:
                live_failure_snapshot = bounded_failure_snapshot(
                    live_sampler, live_child_pid,
                )
            except BaseException as snapshot_failure:
                live_finalization.append(
                    "pre-cleanup diagnostics failure: "
                    f"{type(snapshot_failure).__name__}: {snapshot_failure}"
                )
    finally:
        if live_sampler is not None and not live_sampler_stopped:
            try:
                live_sampler.stop()
                live_sampler_stopped = True
            except BaseException as failure:
                live_finalization.append(
                    f"sampler stop failure: {type(failure).__name__}: {failure}"
                )
        if live_sampler is not None:
            try:
                terminate_owned_processes(
                    live.pid, live_sampler.identity_snapshot(),
                )
            except BaseException as failure:
                live_finalization.append(
                    "descendant termination failure: "
                    f"{type(failure).__name__}: {failure}"
                )
        elif live.poll() is None:
            try:
                os.killpg(live.pid, signal.SIGKILL)
            except BaseException as failure:
                live_finalization.append(
                    f"process-group kill failure: {type(failure).__name__}: {failure}"
                )
        try:
            _live_stdout, live_stderr = live.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(live.pid, signal.SIGKILL)
            except BaseException as failure:
                live_finalization.append(
                    f"timeout kill failure: {type(failure).__name__}: {failure}"
                )
            try:
                _live_stdout, live_stderr = live.communicate(timeout=5)
            except BaseException as failure:
                live_finalization.append(
                    f"post-kill reap failure: {type(failure).__name__}: {failure}"
                )
        except BaseException as failure:
            live_finalization.append(
                f"helper reap failure: {type(failure).__name__}: {failure}"
            )
    live_snapshot = (
        {} if live_sampler is None else finalize_sampler_images(
            live_sampler, live_finalization, "live fixture",
        )
    )
    raise_fixture_failures(
        "live sealed-memfd fixture", live_primary, live_finalization,
        live_failure_snapshot, live_stderr,
    )
    require(live_sampler is not None, "live sampler was not constructed")
    live_pid, live_image = require_single_solver_image(live_snapshot)
    live_diagnostics = live_sampler.diagnostics()
    live_telemetry = live_diagnostics["pid_telemetry"][str(live_pid)]
    require(
        live_image["sha256"] == PINNED_Z3_SHA256
        and live_image["capture_source"] == "session_scan"
        and live_image["cmdline_shape"] == "z3_4_8_12_parsed_exact"
        and live_telemetry["capture_sources"] == {"session_scan": 1}
        and live_diagnostics["session_scan"]["forced_count"] == 2
        and live_diagnostics["session_scan"]["count"] >= 3
        and live_diagnostics["session_scan"]["last_start_ns"]
        >= live_telemetry["capture_first_ns"],
        "live retained session-fallback capture drifted",
    )

    short = launch(True)
    short_sampler: SessionOnlySampler | None = None
    short_sampler_stopped = False
    short_child_pid = -1
    short_primary: BaseException | None = None
    short_finalization: list[str] = []
    short_failure_snapshot: str | None = None
    short_stderr = b""
    try:
        require(short.stdout is not None, "short memfd helper lacks stdout")
        child_line = short.stdout.readline()
        exit_line = short.stdout.readline()
        require(
            child_line.strip().isdigit() and exit_line == b"exited\n",
            "short memfd helper did not complete before sampling",
        )
        short_child_pid = int(child_line)
        short_sampler = SessionOnlySampler(
            short.pid, PINNED_Z3_SHA256, str(PINNED_Z3), 0.001,
        )
        short_sampler.start()
        time.sleep(0.02)
        short_sampler.stop()
        short_sampler_stopped = True
    except BaseException as failure:
        short_primary = failure
        if short_sampler is not None:
            try:
                short_failure_snapshot = bounded_failure_snapshot(
                    short_sampler, short_child_pid,
                )
            except BaseException as snapshot_failure:
                short_finalization.append(
                    "pre-cleanup diagnostics failure: "
                    f"{type(snapshot_failure).__name__}: {snapshot_failure}"
                )
    finally:
        if short_sampler is not None and not short_sampler_stopped:
            try:
                short_sampler.stop()
                short_sampler_stopped = True
            except BaseException as failure:
                short_finalization.append(
                    f"sampler stop failure: {type(failure).__name__}: {failure}"
                )
        if short_sampler is not None:
            try:
                terminate_owned_processes(
                    short.pid, short_sampler.identity_snapshot(),
                )
            except BaseException as failure:
                short_finalization.append(
                    "descendant termination failure: "
                    f"{type(failure).__name__}: {failure}"
                )
        elif short.poll() is None:
            try:
                os.killpg(short.pid, signal.SIGKILL)
            except BaseException as failure:
                short_finalization.append(
                    f"process-group kill failure: {type(failure).__name__}: {failure}"
                )
        try:
            _short_stdout, short_stderr = short.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(short.pid, signal.SIGKILL)
            except BaseException as failure:
                short_finalization.append(
                    f"timeout kill failure: {type(failure).__name__}: {failure}"
                )
            try:
                _short_stdout, short_stderr = short.communicate(timeout=5)
            except BaseException as failure:
                short_finalization.append(
                    f"post-kill reap failure: {type(failure).__name__}: {failure}"
                )
        except BaseException as failure:
            short_finalization.append(
                f"helper reap failure: {type(failure).__name__}: {failure}"
            )
    short_snapshot = (
        {} if short_sampler is None else finalize_sampler_images(
            short_sampler, short_finalization, "short fixture",
        )
    )
    raise_fixture_failures(
        "short sealed-memfd fixture", short_primary, short_finalization,
        short_failure_snapshot, short_stderr,
    )
    require(short_sampler is not None, "short sampler was not constructed")
    try:
        require_single_solver_image(short_snapshot)
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("already-exited sealed solver was accepted")


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
        screen.run_id = run_identifier(output)
        screen.failure_attempts = []
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
        screen.calibration_enabled = False
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
    mode_identity = plain_descriptor_bound_mode_identity(SCRIPT_DIR.parent)
    require(
        mode_identity["pinned_z3_source"]["mode"]
        == mode_identity["expected_sealed_mode"]
        == SEALED_SOLVER_MODE
        == 0o755,
        "plain descriptor-bound exact-mode characterization drifted",
    )
    try:
        derive_plain_descriptor_bound_sealed_mode(0o500)
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("pinned Z3 source-mode drift was accepted")
    live_fixture_source = Path(__file__).read_text()
    require(
        "os.fchmod(descriptor, 0o755)" in live_fixture_source,
        "live fixture no longer emulates the plain descriptor-bound mode",
    )
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
    phase_schema = next(row for row in schema_rows if row["column"] == "phase")
    require(
        "calibration" in phase_schema["meaning"]
        and "diagnostic-only" in phase_schema["meaning"],
        "result schema does not distinguish diagnostic calibration rows",
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
        "scheduling": sampler_scheduling_policy(1_000_000),
        "sampler_error": None,
        "pid_telemetry": {},
        "session_scan": {
            "policy": {
                "before_capture": "every_sampler_pass",
                "post_capture_interval_ns": (
                    SAMPLER_SESSION_SCAN_POST_CAPTURE_NS
                ),
                "maximum_start_gap_ns": (
                    SAMPLER_SESSION_SCAN_MAX_START_GAP_NS
                ),
                "maximum_duration_ns": SAMPLER_SESSION_SCAN_MAX_DURATION_NS,
                "maximum_tgids_per_scan": SAMPLER_SESSION_SCAN_MAX_TGIDS,
            },
            "count": 201,
            "forced_count": 2,
            "first_start_ns": 1_000_000,
            "last_start_ns": 1_999_000_000,
            "last_finished_ns": 2_000_000_000,
            "max_start_gap_ns": 10_000_000,
            "max_duration_ns": 12_000_000,
            "tgids_total": 603,
            "max_tgids": 3,
            "errors": {},
        },
    }
    sampler_quality = validate_sampler_quality(
        sampler_fixture, 0, 2_000_000_000, 1.0
    )
    require(sampler_quality["coverage_ratio"] > 0.99, "sampler coverage parser")
    missing_scheduling = json.loads(canonical_json(sampler_fixture))
    del missing_scheduling["scheduling"]
    try:
        validate_sampler_quality(
            missing_scheduling, 0, 2_000_000_000, 1.0,
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("missing sampler scheduling policy was accepted")
    drifted_scheduling = json.loads(canonical_json(sampler_fixture))
    drifted_scheduling["scheduling"][
        "maximum_consecutive_immediate_catch_ups"
    ] = 2
    try:
        validate_sampler_quality(
            drifted_scheduling, 0, 2_000_000_000, 1.0,
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("drifted sampler scheduling policy was accepted")
    missing_forced_scan = json.loads(canonical_json(sampler_fixture))
    missing_forced_scan["session_scan"]["forced_count"] = 1
    try:
        validate_sampler_quality(
            missing_forced_scan, 0, 2_000_000_000, 1.0,
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("missing forced-final session scan was accepted")
    global_malformed = json.loads(canonical_json(sampler_fixture))
    global_malformed["session_scan"]["errors"] = {
        "session_scan_stat_resolved": {"PROC_STAT_SHORT_TAIL": 1},
    }
    validate_sampler_quality(global_malformed, 0, 2_000_000_000, 1.0)
    unresolved_global_malformed = json.loads(canonical_json(sampler_fixture))
    unresolved_global_malformed["session_scan"]["errors"] = {
        "session_scan_stat_unresolved_retry": {
            "PROC_STAT_SHORT_TAIL": 1,
        },
    }
    try:
        validate_sampler_quality(
            unresolved_global_malformed, 0, 2_000_000_000, 1.0,
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure(
            "unresolved malformed global proc-stat was accepted"
        )
    known_malformed = json.loads(canonical_json(sampler_fixture))
    known_malformed["pid_telemetry"] = {
        "123": {
            "errors": {"known_stat": {"PROC_STAT_SHORT_TAIL": 1}},
        },
    }
    try:
        validate_sampler_quality(
            known_malformed, 0, 2_000_000_000, 1.0
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("known-PID malformed proc-stat was accepted")

    for field, multiplier in (
        ("max_interval_ns", SAMPLER_MAX_INTERVAL_MULTIPLIER),
        ("max_pass_ns", SAMPLER_MAX_PASS_MULTIPLIER),
    ):
        exact = json.loads(canonical_json(sampler_fixture))
        exact[field] = int(1_000_000 * multiplier)
        validate_sampler_quality(exact, 0, 2_000_000_000, 1.0)
        over = json.loads(canonical_json(exact))
        over[field] += 1
        try:
            validate_sampler_quality(over, 0, 2_000_000_000, 1.0)
        except HarnessFailure:
            pass
        else:
            raise HarnessFailure(f"{field} accepted bound plus one")

    exact_coverage = json.loads(canonical_json(sampler_fixture))
    exact_coverage["first_sample_ns"] = 40_000_000
    exact_coverage["span_ns"] = (
        exact_coverage["last_sample_start_ns"]
        - exact_coverage["first_sample_ns"]
    )
    exact_coverage["mean_interval_ns"] = (
        exact_coverage["span_ns"]
        // (exact_coverage["sample_count"] - 1)
    )
    require(
        validate_sampler_quality(
            exact_coverage, 0, 2_000_000_000, 1.0,
        )["coverage_ratio"] == SAMPLER_MIN_WALL_COVERAGE,
        "exact sampler coverage bound drifted",
    )
    below_coverage = json.loads(canonical_json(exact_coverage))
    below_coverage["first_sample_ns"] += 1
    below_coverage["span_ns"] -= 1
    below_coverage["mean_interval_ns"] = (
        below_coverage["span_ns"]
        // (below_coverage["sample_count"] - 1)
    )
    try:
        validate_sampler_quality(
            below_coverage, 0, 2_000_000_000, 1.0,
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("sampler coverage accepted bound minus one ns")

    terminal_bound = json.loads(canonical_json(sampler_fixture))
    terminal_bound.update({
        "last_sample_start_ns": 2_949_000_000,
        "last_sample_finished_ns": 2_950_000_000,
    })
    terminal_bound["span_ns"] = (
        terminal_bound["last_sample_start_ns"]
        - terminal_bound["first_sample_ns"]
    )
    terminal_bound["mean_interval_ns"] = (
        terminal_bound["span_ns"]
        // (terminal_bound["sample_count"] - 1)
    )
    terminal_bound["session_scan"].update({
        "last_start_ns": 2_999_000_000,
        "last_finished_ns": 3_000_000_000,
    })
    quality_at_terminal_bound = validate_sampler_quality(
        terminal_bound, 0, 3_000_000_000, 1.0,
    )
    require(
        quality_at_terminal_bound["terminal_gap_ns"] == 50_000_000,
        "exact sampler terminal-gap bound drifted",
    )
    terminal_over = json.loads(canonical_json(terminal_bound))
    terminal_over["last_sample_finished_ns"] -= 1
    try:
        validate_sampler_quality(
            terminal_over, 0, 3_000_000_000, 1.0,
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("sampler terminal gap accepted bound plus one")
    delayed_sampler = dict(sampler_fixture)
    delayed_sampler["first_sample_ns"] = 51_000_000
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
    sparse_session_scan = json.loads(canonical_json(sampler_fixture))
    sparse_session_scan["session_scan"]["max_start_gap_ns"] = (
        SAMPLER_SESSION_SCAN_MAX_START_GAP_NS + 1
    )
    try:
        validate_sampler_quality(
            sparse_session_scan, 0, 2_000_000_000, 1.0
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("excessive session-scan gap was accepted")
    slow_session_scan = json.loads(canonical_json(sampler_fixture))
    slow_session_scan["session_scan"]["max_duration_ns"] = (
        SAMPLER_SESSION_SCAN_MAX_DURATION_NS + 1
    )
    try:
        validate_sampler_quality(
            slow_session_scan, 0, 2_000_000_000, 1.0
        )
    except HarnessFailure:
        pass
    else:
        raise HarnessFailure("excessive session-scan duration was accepted")
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
        and not vetoed["meaningful_over_1_10"]
        and vetoed["primary_failure"]
        == "catchable termination requested: ['SIGQUIT']",
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
    check_proc_stat_parser()
    check_sampler_fixed_rate_scheduler()
    check_sampler_capture_protocol()
    check_calibration_protocol()
    check_failure_attempt_manifest_protocol()
    check_live_sealed_memfd_session_fallback()
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


CALIBRATION_IDENTITY_FIELDS = (
    "protocol_source",
    "baseline_source",
    "baseline_build_plan",
    "baseline_binary",
    "z3",
    "z3_package",
    "tools",
    "python",
    "runner",
    "readme",
    "result_schema",
    "workloads",
    "sampler_protocol",
)


def calibration_executable_identity(
    path: Path, expected_sha256: str, label: str,
    *,
    expected_build_id: str | None = None,
    expected_library_hashes: set[str] | None = None,
    expected_mode: int | None = None,
) -> dict[str, Any]:
    identity = executable_identity(
        path, expected_sha256, label,
        expected_build_id=expected_build_id,
        expected_library_hashes=expected_library_hashes,
        expected_mode=expected_mode,
    )
    # ldd prints randomized load addresses.  The exact resolved library paths
    # and their byte hashes are the durable identity; do not make ASLR output
    # an impossible start/end equality gate.
    identity.pop("ldd_sha256")
    identity["dynamic_linkage_identity"] = (
        "sorted resolved library paths and exact SHA-256 values"
    )
    return identity


def calibration_identity_snapshot(
    baseline_root: Path, baseline_binary: Path,
) -> dict[str, Any]:
    frozen_artifacts: dict[str, dict[str, Any]] = {}
    for relative, expected in FROZEN_ARTIFACT_SHA256.items():
        path = SCRIPT_DIR / relative
        actual = sha256_file(path)
        require(
            actual == expected,
            f"calibration frozen artifact drifted for {relative}: {actual}",
        )
        frozen_artifacts[relative] = {
            "path": str(path), "sha256": actual,
        }
    runner_path = Path(__file__).resolve()
    runner_stat = runner_path.stat()
    baseline_source = verify_source_root(
        baseline_root, BASELINE_COMMIT, "calibration baseline",
    )
    baseline_plan = build_plan_identity(
        baseline_root, baseline_binary, BASELINE_PLAN_SHA256,
        "calibration baseline",
    )
    return {
        "schema": "djex-solver-sampler-calibration-identity/v1",
        "protocol_source": verify_protocol_repository(),
        "baseline_source": baseline_source,
        "baseline_build_plan": baseline_plan,
        "baseline_binary": calibration_executable_identity(
            baseline_binary, BASELINE_BINARY_SHA256,
            "calibration baseline binary",
            expected_build_id=BASELINE_BUILD_ID,
            expected_library_hashes=PINNED_DJEX_LIBRARY_SHA256,
        ),
        "z3": calibration_executable_identity(
            PINNED_Z3, PINNED_Z3_SHA256, "calibration Z3",
            expected_library_hashes=PINNED_Z3_LIBRARY_SHA256,
            expected_mode=PINNED_Z3_SOURCE_MODE,
        ),
        "z3_package": z3_package_identity(),
        "tools": tool_identities("/usr/bin/strace"),
        "python": python_identity(),
        "runner": {
            "path": str(runner_path),
            "sha256": sha256_file(runner_path),
            "size": runner_stat.st_size,
            "device": runner_stat.st_dev,
            "inode": runner_stat.st_ino,
            "mode": stat_module.S_IMODE(runner_stat.st_mode),
        },
        "readme": {
            "path": str(SCRIPT_DIR / "README.md"),
            "sha256": sha256_file(SCRIPT_DIR / "README.md"),
        },
        "result_schema": {
            **frozen_artifacts["result-schema.tsv"],
            "release_row_schema": ROW_SCHEMA,
            "calibration_row_schema": CALIBRATION_ROW_SCHEMA,
            "columns": list(RESULT_COLUMNS),
        },
        "workloads": {
            name: {
                "path": str(SCRIPT_DIR / workload.template),
                "sha256": frozen_artifacts[workload.template]["sha256"],
                "assessments": workload.assessments,
                "accepted": workload.accepted,
                "rejected": workload.rejected,
                "rendered": workload.rendered,
            }
            for name, workload in WORKLOADS.items()
        },
        "sampler_protocol": {
            "target_interval_ms": 1.0,
            "scheduling": sampler_scheduling_policy(1_000_000),
            "minimum_wall_coverage": SAMPLER_MIN_WALL_COVERAGE,
            "maximum_mean_interval_multiplier": (
                SAMPLER_MAX_MEAN_INTERVAL_MULTIPLIER
            ),
            "maximum_interval_multiplier": SAMPLER_MAX_INTERVAL_MULTIPLIER,
            "maximum_pass_multiplier": SAMPLER_MAX_PASS_MULTIPLIER,
            "post_capture_session_scan_interval_ns": (
                SAMPLER_SESSION_SCAN_POST_CAPTURE_NS
            ),
            "maximum_session_scan_start_gap_ns": (
                SAMPLER_SESSION_SCAN_MAX_START_GAP_NS
            ),
            "maximum_session_scan_duration_ns": (
                SAMPLER_SESSION_SCAN_MAX_DURATION_NS
            ),
            "maximum_session_scan_tgids": SAMPLER_SESSION_SCAN_MAX_TGIDS,
            "accepted_cmdline_shapes": [
                "exec_exact", "z3_4_8_12_parsed_exact",
            ],
            "exact_target": SEALED_SOLVER_TARGET,
            "exact_mode": SEALED_SOLVER_MODE,
            "mode_identity": plain_descriptor_bound_mode_identity(
                baseline_root
            ),
        },
    }


def calibration_identity_attestation(
    start: dict[str, Any] | None,
    end: dict[str, Any] | None,
    *,
    start_failure: str | None = None,
    end_failure: str | None = None,
) -> dict[str, Any]:
    gates: list[dict[str, Any]] = []
    key_sets_match = (
        start is not None
        and end is not None
        and set(start) == set(end)
        and set(CALIBRATION_IDENTITY_FIELDS).issubset(start)
    )
    gates.append({
        "name": "identity_field_set",
        "pass": key_sets_match,
        "requirement": (
            "start/end fields equal and contain every frozen calibration "
            "identity field"
        ),
        "start_fields": [] if start is None else sorted(start),
        "end_fields": [] if end is None else sorted(end),
    })
    gates.append({
        "name": "whole_snapshot_exact",
        "pass": start is not None and end is not None and start == end,
        "requirement": "the complete canonical end snapshot equals start",
    })
    for field in CALIBRATION_IDENTITY_FIELDS:
        equal = (
            start is not None
            and end is not None
            and field in start
            and field in end
            and start[field] == end[field]
        )
        gates.append({
            "name": field,
            "pass": equal,
            "start_sha256": (
                None if start is None or field not in start
                else sha256_bytes(canonical_json(start[field]))
            ),
            "end_sha256": (
                None if end is None or field not in end
                else sha256_bytes(canonical_json(end[field]))
            ),
            "requirement": "end identity exactly equals start identity",
        })
    passed = (
        start_failure is None
        and end_failure is None
        and all(gate["pass"] for gate in gates)
    )
    return {
        "schema": "djex-solver-sampler-calibration-identity-attestation/v1",
        "pass": passed,
        "start_capture_failure": start_failure,
        "end_capture_failure": end_failure,
        "start_snapshot_sha256": (
            None if start is None else sha256_bytes(canonical_json(start))
        ),
        "end_snapshot_sha256": (
            None if end is None else sha256_bytes(canonical_json(end))
        ),
        "gates": gates,
    }


def calibration_row_attestation(
    rows: Sequence[dict[str, Any]], required: int,
) -> dict[str, Any]:
    positions = [row.get("position") for row in rows]
    gates = [
        {
            "name": "row_count",
            "pass": len(rows) == required,
            "observed": len(rows),
            "requirement": required,
        },
        {
            "name": "positions",
            "pass": positions == list(range(1, required + 1)),
            "observed": positions,
            "requirement": f"exact ordered integers 1 through {required}",
        },
        {
            "name": "diagnostic_row_schema",
            "pass": all(
                row.get("schema") == CALIBRATION_ROW_SCHEMA
                and row.get("phase") == "calibration"
                for row in rows
            ),
            "observed": sorted({
                (row.get("schema"), row.get("phase")) for row in rows
            }),
            "requirement": (
                f"{CALIBRATION_ROW_SCHEMA}/calibration for every row"
            ),
        },
        {
            "name": "treatment",
            "pass": all(
                row.get("workload") == "W1"
                and row.get("cell") == "B"
                and row.get("revision") == "baseline"
                and row.get("commit") == BASELINE_COMMIT
                and row.get("jobs") == 1
                and row.get("capabilities") == 2
                for row in rows
            ),
            "observed": sorted({
                (
                    row.get("workload"), row.get("cell"),
                    row.get("revision"), row.get("commit"),
                    row.get("jobs"), row.get("capabilities"),
                )
                for row in rows
            }),
            "requirement": "W1/B baseline jobs=1 -N2 only",
        },
        {
            "name": "exact_solver_image_hash",
            "pass": all(
                row.get("solver_image_sha256") == PINNED_Z3_SHA256
                for row in rows
            ),
            "observed": sum(
                row.get("solver_image_sha256") == PINNED_Z3_SHA256
                for row in rows
            ),
            "requirement": required,
        },
        {
            "name": "cleanup",
            "pass": all(row.get("cleanup_ok") == "true" for row in rows),
            "observed": sum(
                row.get("cleanup_ok") == "true" for row in rows
            ),
            "requirement": required,
        },
    ]
    return {
        "schema": "djex-solver-sampler-calibration-row-attestation/v1",
        "pass": all(gate["pass"] for gate in gates),
        "gates": gates,
    }


def calibration_passes(
    failures: Sequence[str],
    identity_attestation: dict[str, Any],
    row_attestation: dict[str, Any],
    evidence_hashes: Sequence[str | None],
) -> bool:
    return (
        not failures
        and identity_attestation.get("pass") is True
        and row_attestation.get("pass") is True
        and all(digest is not None for digest in evidence_hashes)
    )


def close_calibration_screen(
    screen: Screen | None, finalization_failures: list[str],
) -> None:
    if screen is None:
        return
    try:
        screen.close()
    except BaseException as failure:
        finalization_failures.append(
            "calibration results close failure: "
            f"{type(failure).__name__}: {failure}"
        )


def check_calibration_protocol() -> None:
    identity = {
        "schema": "djex-solver-sampler-calibration-identity/v1",
        **{
            field: {"synthetic_identity": field}
            for field in CALIBRATION_IDENTITY_FIELDS
        },
    }
    require(
        calibration_identity_attestation(identity, identity)["pass"],
        "equal synthetic calibration identities did not attest",
    )
    drifted_identity = json.loads(canonical_json(identity))
    drifted_identity["runner"]["synthetic_identity"] = "drifted"
    require(
        not calibration_identity_attestation(
            identity, drifted_identity,
        )["pass"],
        "synthetic calibration end-identity drift was accepted",
    )

    rows = []
    for position in range(1, 65):
        rows.append({
            "schema": CALIBRATION_ROW_SCHEMA,
            "phase": "calibration",
            "workload": "W1",
            "sample": "",
            "williams_row": "",
            "position": position,
            "cell": "B",
            "revision": "baseline",
            "commit": BASELINE_COMMIT,
            "jobs": 1,
            "capabilities": 2,
            "solver_image_sha256": PINNED_Z3_SHA256,
            "cleanup_ok": "true",
        })
    require(
        calibration_row_attestation(rows, 64)["pass"],
        "synthetic 64-row calibration did not attest",
    )
    drifted_rows = json.loads(canonical_json(rows))
    drifted_rows[-1]["position"] = 65
    require(
        not calibration_row_attestation(drifted_rows, 64)["pass"],
        "synthetic calibration position drift was accepted",
    )
    required_hashes: list[str | None] = ["0" * 64] * 5
    require(
        calibration_passes(
            [], {"pass": True}, {"pass": True}, required_hashes,
        ),
        "complete synthetic calibration evidence did not pass",
    )
    for missing in range(len(required_hashes)):
        absent = list(required_hashes)
        absent[missing] = None
        require(
            not calibration_passes(
                [], {"pass": True}, {"pass": True}, absent,
            ),
            f"missing calibration evidence hash {missing} was accepted",
        )

    with tempfile.TemporaryDirectory(
        prefix="djex-calibration-durability-self-check-"
    ) as source:
        output = Path(source) / "evidence"
        output.mkdir()
        arguments = argparse.Namespace(
            baseline_binary="/synthetic/baseline",
            candidate_binary="/synthetic/baseline",
            z3=str(PINNED_Z3),
            z3_sha256=PINNED_Z3_SHA256,
            diagnostic_calibration=True,
        )
        screen = Screen(arguments, output)
        row = dict(rows[0])
        row.update({
            "run_id": screen.run_id,
            "artifact_dir": "raw/calibration/W1/pos-01-B",
        })
        outcome = RunOutcome(row, (0, b"stdout", b"stderr"), None)
        try:
            screen.append(outcome)
            with screen.results_path.open(newline="") as handle:
                durable_rows = list(csv.DictReader(handle, delimiter="\t"))
            require(
                len(screen.rows) == 1
                and len(durable_rows) == 1
                and durable_rows[0]["schema"] == CALIBRATION_ROW_SCHEMA
                and durable_rows[0]["phase"] == "calibration",
                "calibration append was not durably visible before close",
            )
        finally:
            screen.close()

    class FailingClose:
        def close(self) -> None:
            raise OSError(errno.EIO, "synthetic calibration close failure")

    synthetic_primary = "synthetic primary failure"
    close_failures: list[str] = []
    close_calibration_screen(FailingClose(), close_failures)  # type: ignore[arg-type]
    require(
        synthetic_primary == "synthetic primary failure"
        and len(close_failures) == 1
        and "synthetic calibration close failure" in close_failures[0],
        "calibration close failure suppressed primary precedence",
    )

    calibration_source = inspect.getsource(run_sampler_calibration)
    require(
        "screen.append(outcome)" in calibration_source
        and "rows.append(outcome.row)" not in calibration_source
        and "rows = [] if screen is None else list(screen.rows)"
        in calibration_source,
        "calibration durable-row source characterization drifted",
    )


def check_failure_attempt_manifest_protocol() -> None:
    with tempfile.TemporaryDirectory(
        prefix="djex-failure-attempt-self-check-"
    ) as source:
        output = Path(source) / "evidence"
        raw = output / "raw"
        raw.mkdir(parents=True)
        screen = object.__new__(Screen)
        screen.output = output
        screen.raw = raw
        screen.run_id = run_identifier(output)
        screen.rows = []
        screen.failure_attempts = []
        artifact = screen._artifact_path(
            "warmup", WORKLOADS["W1"], CELLS["B"], 7, None,
        )
        artifact.mkdir(parents=True)
        payloads = {
            "command.json": b'{"synthetic":"command"}\n',
            "input.repl": b"synthetic input\n",
            "stdout.bin": b"synthetic stdout\n",
            "stderr.bin": b"synthetic stderr\n",
            "rts.stats": b"synthetic stats\n",
            "process-tree.json": b'{"synthetic":"tree"}\n',
            "private-residue.json": b'{"synthetic":"residue"}\n',
            "strace.4242": b"synthetic trace\n",
        }
        for name, contents in payloads.items():
            (artifact / name).write_bytes(contents)

        def fail_after_artifacts(*_arguments: Any, **_keywords: Any) -> Any:
            raise HarnessFailure("synthetic post-artifact cadence gate")

        screen._execute_once = fail_after_artifacts  # type: ignore[method-assign]
        try:
            screen.execute(
                "warmup", WORKLOADS["W1"], CELLS["B"], 7,
            )
        except HarnessFailure as failure:
            require(
                str(failure) == "synthetic post-artifact cadence gate",
                "failure-attempt persistence replaced the primary failure",
            )
        else:
            raise HarnessFailure(
                "synthetic post-artifact failure was accepted"
            )
        require(
            screen.rows == [],
            "failure-attempt persistence fabricated a result row",
        )
        manifest_path = artifact / "failure-attempt-manifest.json"
        require(
            manifest_path.is_file() and len(screen.failure_attempts) == 1,
            "post-artifact failure manifest was not durably exposed",
        )
        manifest = json.loads(manifest_path.read_bytes())
        require(
            manifest["attempt"]["phase"] == "warmup"
            and manifest["attempt"]["workload"] == "W1"
            and manifest["attempt"]["cell"] == "B"
            and manifest["attempt"]["position"] == 7
            and manifest["attempt"]["sample"] is None
            and manifest["failure"] == {
                "type": "HarnessFailure",
                "message": "synthetic post-artifact cadence gate",
            },
            "failure-attempt invocation identity drifted",
        )
        for key, name in (
            ("command", "command.json"),
            ("input", "input.repl"),
            ("stdout", "stdout.bin"),
            ("stderr", "stderr.bin"),
            ("stats", "rts.stats"),
            ("process_tree", "process-tree.json"),
            ("residue", "private-residue.json"),
        ):
            identity = manifest["artifacts"][key]
            require(
                identity["present"] is True
                and identity["size"] == len(payloads[name])
                and identity["sha256"] == sha256_bytes(payloads[name]),
                f"failure-attempt {key} identity drifted",
            )
        require(
            len(manifest["traces"]) == 1
            and manifest["traces"][0]["size"]
            == len(payloads["strace.4242"])
            and manifest["traces"][0]["sha256"]
            == sha256_bytes(payloads["strace.4242"]),
            "failure-attempt trace identity drifted",
        )
        summary = screen_failure_attempt_summary(screen)
        require(
            summary["count"] == 1
            and summary["verification_pass"] is True
            and summary["manifests"][0]["sha256"]
            == sha256_file(manifest_path)
            and summary["manifests_sha256"]
            == sha256_bytes(canonical_json(summary["manifests"])),
            "failure-attempt set did not cryptographically anchor manifest",
        )
        for record_name in ("provenance", "decision"):
            record_path = output / f"synthetic-{record_name}.json"
            write_json(record_path, {"failure_attempts": summary})
            restored = json.loads(record_path.read_bytes())
            require(
                restored["failure_attempts"]["manifests"][0]["sha256"]
                == sha256_file(manifest_path),
                f"synthetic {record_name} lost failure-attempt anchor",
            )
        require(
            'provenance["failure_attempts"] = failure_attempts'
            in inspect.getsource(run_screen)
            and '"failure_attempts": failure_attempts'
            in inspect.getsource(run_screen)
            and '"failure_attempts": failure_attempts'
            in inspect.getsource(run_sampler_calibration),
            "release/calibration failure-attempt anchor wiring drifted",
        )


def run_sampler_calibration(arguments: argparse.Namespace) -> int:
    output = Path(arguments.output).resolve()
    require(
        not output.exists() and not output.is_symlink(),
        f"calibration output already exists: {output}",
    )
    screen: Screen | None = None
    start_identity: dict[str, Any] | None = None
    end_identity: dict[str, Any] | None = None
    start_failure: str | None = None
    end_failure: str | None = None
    primary_failure: str | None = None
    finalization_failures: list[str] = []
    termination = TerminationCoordinator()
    handled_signals = (
        signal.SIGHUP, signal.SIGINT, signal.SIGTERM, signal.SIGQUIT,
    )
    previous_handlers = {
        value: signal.getsignal(value) for value in handled_signals
    }
    installed_signals: list[signal.Signals] = []
    provenance = {
        "schema": "djex-solver-sampler-calibration/v1",
        "state": "initializing",
        "diagnostic_only": True,
        "release_evidence": False,
        "cell": "B",
        "workload": "W1",
        "requested_iterations": arguments.iterations,
        "required_iterations": 64,
        "run_id": run_identifier(output),
        "results_row_schema": CALIBRATION_ROW_SCHEMA,
        "notes": arguments.notes,
    }

    def termination_requested(signum: int, _frame: Any) -> None:
        termination.record(signum, _frame)

    try:
        try:
            for value in handled_signals:
                signal.signal(value, termination_requested)
                installed_signals.append(value)
            output.mkdir(parents=True)
            fsync_directory(output.parent)
            write_json(output / "calibration-provenance.json", provenance)
            validate_run_configuration(arguments)
            require(
                arguments.iterations == 64,
                "diagnostic sampler calibration must contain exactly 64 "
                "invocations",
            )
            require(
                require_sha256(
                    arguments.baseline_binary_sha256,
                    "calibration baseline binary SHA-256",
                ) == BASELINE_BINARY_SHA256,
                "calibration baseline binary hash is not the frozen baseline "
                "hash",
            )
            require(
                Path(arguments.z3).resolve() == PINNED_Z3
                and require_sha256(
                    arguments.z3_sha256, "calibration Z3 SHA-256",
                ) == PINNED_Z3_SHA256,
                "calibration Z3 identity drifted",
            )
            baseline_root = Path(arguments.baseline_root).resolve()
            baseline_binary = Path(arguments.baseline_binary).resolve()
            try:
                start_identity = calibration_identity_snapshot(
                    baseline_root, baseline_binary,
                )
            except BaseException as failure:
                start_failure = (
                    f"{type(failure).__name__}: {failure}"
                )
                raise
            start_path = output / "calibration-identity-start.json"
            write_json(start_path, start_identity)
            provenance.update({
                "state": "running",
                "start_identity": {
                    "path": str(start_path),
                    "sha256": sha256_file(start_path),
                    "snapshot_sha256": sha256_bytes(
                        canonical_json(start_identity)
                    ),
                },
                "configuration": {
                    "outer_timeout_seconds": arguments.outer_timeout,
                    "sample_interval_ms": arguments.sample_interval_ms,
                },
            })
            write_json(output / "calibration-provenance.json", provenance)
            screen_arguments = argparse.Namespace(
                baseline_binary=str(baseline_binary),
                candidate_binary=str(baseline_binary),
                z3=str(PINNED_Z3),
                z3_sha256=PINNED_Z3_SHA256,
                outer_timeout=arguments.outer_timeout,
                sample_interval_ms=arguments.sample_interval_ms,
                diagnostic_calibration=True,
            )
            screen = Screen(screen_arguments, output)
            termination.bind_screen(screen)
            screen.external_termination_requests = termination.requests
            for iteration in range(1, arguments.iterations + 1):
                outcome = screen.execute(
                    "calibration", WORKLOADS["W1"], CELLS["B"], iteration,
                )
                # append() writes, flushes, and fsyncs before its semantic
                # parity checks, so even the first failing completed row is
                # immutable partial diagnostic evidence.
                screen.append(outcome)
                require(
                    outcome.row["solver_image_sha256"] == PINNED_Z3_SHA256,
                    "calibration solver image drifted at invocation "
                    f"{iteration}",
                )
        except BaseException as observed:
            primary_failure = f"{type(observed).__name__}: {observed}"
        finally:
            termination.publish_finalization()
            close_calibration_screen(screen, finalization_failures)

        rows = [] if screen is None else list(screen.rows)
        if start_identity is not None:
            try:
                end_identity = calibration_identity_snapshot(
                    Path(arguments.baseline_root).resolve(),
                    Path(arguments.baseline_binary).resolve(),
                )
                end_path = output / "calibration-identity-end.json"
                write_json(end_path, end_identity)
            except BaseException as failure:
                end_failure = f"{type(failure).__name__}: {failure}"
                finalization_failures.append(
                    "calibration end-identity capture failure: "
                    + end_failure
                )
        else:
            end_failure = "start identity was unavailable"
        identity_attestation = calibration_identity_attestation(
            start_identity, end_identity,
            start_failure=start_failure, end_failure=end_failure,
        )
        identity_attestation_path = (
            output / "calibration-identity-attestation.json"
        )
        try:
            write_json(identity_attestation_path, identity_attestation)
        except BaseException as failure:
            finalization_failures.append(
                "calibration identity-attestation write failure: "
                f"{type(failure).__name__}: {failure}"
            )
        if not identity_attestation["pass"] and primary_failure is None:
            primary_failure = "calibration end-identity attestation failed"
        row_attestation = calibration_row_attestation(
            rows, 64,
        )
        if not row_attestation["pass"] and primary_failure is None:
            primary_failure = "calibration row attestation failed"
        if termination.requests and primary_failure is None:
            primary_failure = (
                "catchable termination requested during calibration: "
                f"{list(termination.requests)}"
            )
        failure_attempts = screen_failure_attempt_summary(screen)
        if not failure_attempts["verification_pass"]:
            finalization_failures.append(
                "calibration failure-attempt manifest verification failed: "
                + repr(failure_attempts["verification_failures"])
            )

        def required_evidence_hash(path: Path) -> str | None:
            if not path.is_file():
                finalization_failures.append(
                    f"required calibration evidence is missing: {path.name}"
                )
                return None
            try:
                return sha256_file(path)
            except BaseException as failure:
                finalization_failures.append(
                    f"cannot hash calibration evidence {path.name}: "
                    f"{type(failure).__name__}: {failure}"
                )
                return None

        results_path = (
            screen.results_path if screen is not None
            else output / "results.tsv"
        )
        results_sha256 = required_evidence_hash(results_path)
        start_identity_sha256 = required_evidence_hash(
            output / "calibration-identity-start.json"
        )
        end_identity_sha256 = required_evidence_hash(
            output / "calibration-identity-end.json"
        )
        identity_attestation_sha256 = required_evidence_hash(
            identity_attestation_path
        )
        provenance.update({
            "state": (
                "complete" if primary_failure is None
                and not finalization_failures else "hold"
            ),
            "completed_invocations": len(rows),
            "results": {
                "path": str(results_path),
                "sha256": results_sha256,
                "rows_fsynced": len(rows),
            },
            "end_identity": {
                "path": str(output / "calibration-identity-end.json"),
                "sha256": end_identity_sha256,
                "snapshot_sha256": identity_attestation[
                    "end_snapshot_sha256"
                ],
            },
            "end_identity_attestation": {
                "path": str(identity_attestation_path),
                "sha256": identity_attestation_sha256,
                "pass": identity_attestation["pass"],
            },
            "row_attestation": row_attestation,
            "failure_attempts": failure_attempts,
            "termination_requests_before_outcome_commit": list(
                termination.requests
            ),
            "primary_failure": primary_failure,
            "finalization_failures": list(finalization_failures),
        })
        try:
            write_json(output / "calibration-provenance.json", provenance)
        except BaseException as failure:
            finalization_failures.append(
                "calibration provenance finalization failure: "
                f"{type(failure).__name__}: {failure}"
            )
        provenance_sha256 = required_evidence_hash(
            output / "calibration-provenance.json"
        )
        failures = (
            ([primary_failure] if primary_failure is not None else [])
            + finalization_failures
        )
        passed = calibration_passes(
            failures,
            identity_attestation,
            row_attestation,
            (
                results_sha256,
                provenance_sha256,
                start_identity_sha256,
                end_identity_sha256,
                identity_attestation_sha256,
            ),
        )
        decision = {
            "schema": "djex-solver-sampler-calibration-decision/v1",
            "diagnostic_only": True,
            "release_evidence": False,
            "verdict": "PASS" if passed else "HOLD",
            "run_id": run_identifier(output),
            "completed_invocations": len(rows),
            "required_invocations": 64,
            "exact_solver_image_captures": sum(
                row.get("solver_image_sha256") == PINNED_Z3_SHA256
                for row in rows
            ),
            "row_attestation": row_attestation,
            "end_identity_attestation": identity_attestation,
            "results_sha256": results_sha256,
            "provenance_sha256": provenance_sha256,
            "start_identity_sha256": start_identity_sha256,
            "end_identity_sha256": end_identity_sha256,
            "identity_attestation_sha256": (
                identity_attestation_sha256
            ),
            "failure_attempts": failure_attempts,
            "rows": rows,
            "primary_failure": failures[0] if failures else None,
            "finalization_failures": failures[1:] if failures else [],
        }
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
        write_json(output / "calibration-decision.json", decision)
        destination = (
            sys.stdout if decision["verdict"] == "PASS" else sys.stderr
        )
        print(json.dumps(decision, indent=2, sort_keys=True), file=destination)
        return 0 if decision["verdict"] == "PASS" else 2
    finally:
        for value in reversed(installed_signals):
            try:
                signal.signal(value, previous_handlers[value])
            except BaseException as failure:
                print(
                    "calibration signal-handler restoration failure: "
                    f"{failure}",
                    file=sys.stderr,
                )


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    commands.add_parser(
        "self-check",
        help=(
            "run static/synthetic checks plus two live sealed-memfd helper "
            "sessions; no Djex workload"
        ),
    )
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
    calibration = commands.add_parser(
        "calibrate-sampler",
        help=(
            "run 64 diagnostic-only W1/B sampler captures; never release "
            "or replacement evidence"
        ),
    )
    calibration.add_argument("--baseline-root", required=True)
    calibration.add_argument("--baseline-binary", required=True)
    calibration.add_argument("--baseline-binary-sha256", required=True)
    calibration.add_argument("--z3", default=str(PINNED_Z3))
    calibration.add_argument("--z3-sha256", default=PINNED_Z3_SHA256)
    calibration.add_argument("--output", required=True)
    calibration.add_argument("--iterations", type=int, default=64)
    calibration.add_argument("--outer-timeout", type=int, default=600)
    calibration.add_argument("--sample-interval-ms", type=float, default=1.0)
    calibration.add_argument("--notes", default="")
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
    if arguments.command == "calibrate-sampler":
        return run_sampler_calibration(arguments)
    validate_run_configuration(arguments)
    return run_screen(arguments)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except HarnessFailure as failure:
        print(f"benchmark HOLD: {failure}", file=sys.stderr)
        raise SystemExit(2)
