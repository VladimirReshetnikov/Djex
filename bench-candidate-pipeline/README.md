# SelectBest candidate-pipeline screen

This directory contains the one-shot, fail-closed, end-to-end screen for the
behavioral `SelectBest` candidate pipeline. It is benchmark-only: it imports no
private production module and adds no production or calibration
instrumentation.

The comparison is frozen to:

- baseline commit `0716144502a7dcd8bfa8755f57fd9ced58bf3b83`, binary
  SHA-256 `9d8bf7d37ee13e7933bfef61cb44b85fee0fe4807f44cb1e58b29baa4d9316b0`,
  ELF Build ID `8bb56dc58402bba8b692ca0c4a8b52264a58034f`, and plan SHA-256
  `8d98e5dfaea42e20b76667bd50e1d63fe629beac29bb9b017e5cbf90da20ef2a`;
  its Git tree is `7f7a068b1a50d4ebe28a6567cdc91121f3d4e825` and Git-archive
  SHA-256 is
  `68cc2830b0dc87f109d03bf4c60b0722b5a3ea32cb2f8a3361e78a73048e42e9`;
- candidate commit `aff7a5e8d0fe81f50b05c5073ff05f77e1ab68ca`, binary
  SHA-256 `37b7e3c25cdba445244c7253fc9c78d9007c77c994d6858ab7842c612aca1dac`,
  ELF Build ID `bfec9cf7e0bb7099c692a37894c04677427ba0ef`, and plan SHA-256
  `5aade84865cb49c05eec811718be5d2d2e7aa1c3d9922fd106c78984246a49d6`;
  its Git tree is `c87265e59f4b2b2fd9814a7dd76e94c56b28ee4e` and Git-archive
  SHA-256 is
  `3188206125975e224660994aeee5c1d45cec40eaf53bcdee8509ef58f43d68c9`;
- Z3 `/tmp/djex-z3-4.8.12.5tlMuM/root/usr/bin/z3`, SHA-256
  `e555c27efbbbdd63b6cb6d54abb4a7aeabacba8184593bb917c4a7c16cb6056c`;
- Debian package
  `/tmp/djex-z3-4.8.12.5tlMuM/z3_4.8.12-1_amd64.deb`, SHA-256
  `6742d8addd8a39df4d48945ef2323179966595d4c9249f4e92f44d83ad3a2ab3`,
  package/version/architecture `z3`/`4.8.12-1`/`amd64`.

The runner does not build. The detached binaries were rechecked with the exact
invocation `cabal build exe:djex -O2 --ghc-options=-Werror -j1`; both returned
`Up to date` without changing the frozen plan or binary hashes. The recorded
local evidence is
`/tmp/djex-pipeline-screen-root-20260822.QUs8ub/o2-attestation-manifest.json`,
SHA-256
`158931c7fcc6dd3c40943b959be11479baae17a7a01df90dd3783e9fd8cc426d`.
The runner verifies that manifest and its two exact logs. It also verifies the
plan compiler/Cabal/platform fields and binds each supplied binary to the sole
local `exe:djex` plan output. This is the mechanical `-O2` attestation; the
plan's `opt` directory name alone is not treated as proof.

The two raw plan hashes are also required to be structurally equivalent after
worktree-path normalization. The runner recursively replaces the corresponding
absolute detached root in every JSON string (including keys) with the literal
`<WORKTREE>`, then serializes with
`json.dumps(object, sort_keys=True, indent=2) + "\n"` using the default ASCII
behavior. Each normalized plan must be exactly 72,323 bytes with SHA-256
`586af7ef87a6d144ef2fd6deadabbf215be03aeb989180696cf58dbe3f77da68`;
that shared identity is recorded in provenance and rechecked after the screen.

The runner also pins the complete `ldd` object-hash sets for both Djex
executables and Z3, plus the exact `/usr/bin` Git, strace, readelf, ldd, and
dpkg-deb tools and the GHC/Cabal driver and compiler payload identities. All are
recorded and rehashed after the screen. The executing interpreter is likewise
pinned to resolved path `/usr/bin/python3.10`, version `3.10.12`, and SHA-256
`7d51cd6b48b521277f5caa4610a82126e315fa2be4df069823a8b1eeb5bd4a86`;
the documented launcher is `/usr/bin/python3`, which resolves to that payload.

Before the expensive run, commit the harness and `djex.cabal` additions. The
runner requires its repository to be tracked-clean, verifies that every
protocol artifact is tracked, and records the protocol repository HEAD, tree,
and Git-archive hash. Untracked files are deliberately ignored, including a
caller-owned ReplayLedger. The runner records and rechecks its own source hash;
the committed repository identity provides the non-self-referential binding.
The workload templates and result schema additionally have embedded,
preregistered hashes.

Run the cheap static and synthetic checks first:

```sh
PYTHONDONTWRITEBYTECODE=1 /usr/bin/python3 -B \
  bench-candidate-pipeline/benchmark.py self-check
```

Then invoke the screen exactly once, after the harness commit, with a new
absolute evidence path:

```sh
PYTHONDONTWRITEBYTECODE=1 /usr/bin/python3 -B \
  bench-candidate-pipeline/benchmark.py run \
  --baseline-root /tmp/djex-pipeline-screen-root-20260822.QUs8ub/baseline \
  --baseline-binary /tmp/djex-pipeline-screen-root-20260822.QUs8ub/baseline/dist-newstyle/build/x86_64-linux/ghc-9.12.4/djex-2026.7.17/x/djex/opt/build/djex/djex \
  --baseline-binary-sha256 9d8bf7d37ee13e7933bfef61cb44b85fee0fe4807f44cb1e58b29baa4d9316b0 \
  --candidate-root /tmp/djex-pipeline-screen-root-20260822.QUs8ub/candidate \
  --candidate-binary /tmp/djex-pipeline-screen-root-20260822.QUs8ub/candidate/dist-newstyle/build/x86_64-linux/ghc-9.12.4/djex-2026.7.17/x/djex/opt/build/djex/djex \
  --candidate-binary-sha256 37b7e3c25cdba445244c7253fc9c78d9007c77c994d6858ab7842c612aca1dac \
  --output /absolute/new-evidence-directory
```

The apparent timing options are not protocol knobs: `--outer-timeout` must be
exactly `600` seconds and `--sample-interval-ms` must be exactly `1.0`.
Supplying any other values is HOLD before the screen can run.

The output path must not exist. A failed or interrupted invocation is immutable
HOLD evidence; never replace a sample or reuse its output directory. A HOLD
decision records the primary failure, run ID, completed-row count, and hashes
of the flushed partial results, provenance, and schedule when present. Raw
artifacts remain for audit. The run ID is the SHA-256 of the absolute evidence
path. Catchable `SIGHUP`, `SIGINT`, `SIGTERM`, and `SIGQUIT` queue a termination
request, terminate the active process group when one is active, allow the
signal-deferred cleanup and host-control finalization sequence to finish, and
then take the same HOLD path. At the outcome-commit boundary the runner blocks
those four signals, folds every queued or already-pending request into the
decision, atomically publishes `decision.json`, and intentionally keeps them
blocked through one-shot process exit. `SIGKILL` and host failure are
inherently outside that guarantee.

## Frozen workloads and calibration

Both templates explicitly select the Exference backend and `best` selection,
use cell-specific `jobs`, set queue capacity 2048, disable unused arguments,
residual constraints, and multi-constructor patterns, set constraint deferral
to zero, use no REPL timeout, and load one empty environment. The 600-second
outer timeout is only a harness safety bound.

W1 is frozen at 128 search steps:

```text
:exference --where length result > length arg4 + mod (length arg4) 4 -- a -> a -> a -> a -> [a] -> [a]
```

It performs 24 assessments, all 24 are accepted by UNSAT, and SelectBest
renders 24 candidates before the sole truncation warning.

W2 is frozen at 192 search steps. Its corrected predicate is specifically on
`snd result` and `arg4`:

```text
:exference --where length (snd result) > length arg4 + mod (length arg4) 3 -- a -> a -> a -> [a] -> [a] -> ([a], [a])
```

It performs 48 assessments: 24 are accepted by UNSAT and 24 produce SAT models
and are replay-refuted. SelectBest renders the best six candidates before the
sole truncation warning. Assessment acceptance and the rendered best-tier count
are intentionally separate invariants.

The preregistration is backed by pinned real-Z3 baseline traces:

- W1 solver trace `strace.480432`, SHA-256
  `b8034f2fad5442f9894f45690fcf36673d9c70f6a0c91f687641ad8ea9697c22`;
- W2 solver trace `strace.479715`, SHA-256
  `80a3036759f33b1052eabc6327caef9fad37009075521cda271c76a3a02e6be4`.

Detached candidate `jobs=2`/`N2` calibration produced the same normalized
vectors:

- W1 trace `strace.484218`, SHA-256
  `cb7d94f4763f1b2ee5059332a8cf46f3d63f6b8ad825df53899e42a3526fcffd`;
- W2 trace `strace.484285`, SHA-256
  `87ca7ab91fa751140289c84eee075b387f4238d301a7ba01932d7421f6e2c58c`.

The normalized W1 vector SHA-256 is
`532772f41a2cfe8a362a20530239875f091700d2350880878ed6dee07681d883`;
W2 is
`b0c1d2d38e5de857b2e58a6b4beafb639b0db03691218b12b70c5541b0b679dc`.
Two real-Z3 repetitions and the candidate `jobs=2`/`N2` checks had exact
observable parity: W1 stdout SHA-256
`d551f8afbd6543077595d715a44f9de805b65e6a2636fd7eca489d14a920f554`,
W2 stdout SHA-256
`3d6d190cead367a38eb7524573400ea892321660966028a76aada810243eae80`,
and all six stderr SHA-256
`fa2baab14653b34ca38319e6a9c5b7977722d5ffa06d0b587d0b06ade47d6a62`.
The screen itself still requires byte-identical exit/stdout/stderr across every
cell and phase; calibration does not waive that check.

## Trace and process validation

Every workload/cell gets an untimed trace preflight. The process sampler first
identifies exactly one descendant whose executable target is the sealed
`/memfd:djex-z3-main-image (deleted)`, whose full command line is the frozen Z3
vector, and whose environment is empty. It opens `/proc/PID/exe` while the
process is alive, stops the subprocess wall clock and sampler, and only then
hashes the held executable descriptor. Thus each ordinary run obtains
cryptographic image identity without hashing 16 MiB inside the timed interval.

For preflights, the exact sampled solver PID selects one `strace -ff -yy` file.
The parser requires one successful sealed `execveat(AT_EMPTY_PATH)` with the
full six-argument vector and zero environment variables. Failed executable
access probes do not count. It reconstructs only successful returned byte
prefixes from that PID's fd-0 pipe `read`/`readv` stream and fd-1 pipe
`write`/`writev` stream, joining unfinished/resumed calls and tolerating split
or partial writes. Other processes and file descriptors cannot contaminate the
protocol.

Exact echo nonces frame the four-command opening capability handshake, every
assessment status, and every conditional get-value/model exchange. W1 requires
24 repetitions of its exact modulo-4 symbolic program followed by UNSAT. W2
requires this exact ordered case/status vector:

```text
6  input_1 + 3  UNSAT
6  input_0 + 3  SAT + exact two-input model
18 input_1 + 4  UNSAT
18 input_0 + 4  SAT + exact two-input model
```

All W2 modulo witnesses and right-hand sides use `input_1`, the symbolic
mapping for `arg4`. The full canonical preamble, declarations, ordered
assertions, checks, get-value commands, models, barriers, and response order
are validated; arbitrary `(check-sat)` traffic or capability probes cannot
satisfy the workload contract. The post-callback readiness check is a
non-writing process-state check, so there is no second capability handshake.

The 1 ms sampler recursively discovers children from every known process task,
records explicit parent lineage, sampled process-tree CPU, aggregate peak RSS,
sample count, achieved intervals, and maximum pass duration. It avoids a global
`/proc` scan during timing. Every run must cover at least 0.98 of its wall
interval inside the first-start through last-finish observation window; the
initial and terminal gaps, maximum interval, and maximum pass duration must
each be at most 20 times the configured target, while the mean interval must be
at most 3 times the target. Sparse CPU/RSS sampling is therefore HOLD rather
than silently under-counted. Cleanup rejects any observed descendant identity or
same-process-group survivor and every non-directory private node, including
FIFO, socket, device, and symlink nodes. A very fast detached double-fork that
is neither sampled nor left in the original process group is outside this
literal cleanup guarantee; the evidence claims observed recursive lineage plus
same-group cleanup, not a cgroup/subreaper guarantee.

Immediately before the first preflight, the runner writes and hashes a
cgroup-v2 host-control snapshot. It records the process scheduling affinity,
the exact `/proc/self/cgroup` membership, the sole cgroup-v2 mount identity,
and every directory from the membership leaf through the visible mount root.
For each ancestor it preserves directory identity and the presence and content
of `cpuset.cpus.effective`, `cpuset.mems.effective`, `cpu.max`, and the complete
parsed `cpu.stat`. The task affinity must contain at least two CPUs and must
equal the nearest inherited effective CPU cpuset; a narrower taskset is not an
unregistered benchmark treatment.

After the last run has cleaned up—or on any HOLD path after the start snapshot
exists—the outer signal-deferred finalizer writes the corresponding post
snapshot before comparing it. Membership, mount, affinity, ancestor topology,
control-file presence/content, and `cpu.stat` field presence must be unchanged.
Every ancestor exposing `nr_throttled` and `throttled_usec` must expose them as
a pair with exact zero deltas, and at least one ancestor must expose the pair.
This walks beyond the leaf: on the preregistered host the effective controls
are inherited from `/sys/fs/cgroup/user.slice`, including CPU set `0-5`, memory
node `0`, and `cpu.max` value `max 100000`. Pre/post `/proc/loadavg` and
CPU/memory/I/O pressure-stall data are preserved as diagnostics only, with no
post-hoc threshold. The start, start-attestation, end, and window-attestation
hashes are bound into provenance/evidence and `decision.json`.

The primary wall interval starts immediately before `Popen` and ends when
`communicate` establishes child exit and drained pipes. Sampler shutdown,
sealed-image hashing, descendant audits, artifact writes, residue traversal,
and cleanup occur after that endpoint. `+RTS -sFILE` records allocation outside
application stderr.

Every invocation gets fresh per-run `HOME`, `TMPDIR`, XDG roots, and work
directory. One stable screen-wide absolute module-environment path is freshly
created empty before each sequential invocation and removed and verified after
it. This keeps Djex's printed Source-workspace banner byte-identical without
normalizing application output. Exact input, command/environment, stdout,
stderr, RTS statistics, process-tree manifest, traces, and query vectors remain
in the evidence directory.

## Cells and scheduling

| Cell | Binary | jobs | RTS |
|---|---|---:|---:|
| A | baseline | 1 | N1 |
| B | baseline | 1 | N2 |
| C | baseline | 2 | N1 |
| D | baseline | 2 | N2 |
| E | candidate | 1 | N1 |
| F | candidate | 1 | N2 |
| G | candidate | 2 | N1 |
| H | candidate | 2 | N2 |

After preflights, every workload/cell receives one untimed warmup. The eight
measured samples are never replaced. W1 uses Williams rows 1 through 8; W2 uses
rows 2 through 8 and then row 1. Odd samples run W1 first and even samples run
W2 first.

```text
1  A B H C G D F E
2  B C A D H E G F
3  C D B E A F H G
4  D E C F B G A H
5  E F D G C H B A
6  F G E H D A C B
7  G H F A E B D C
8  H A G B F C E D
```

`result-schema.tsv` defines every raw results column. P95 is nearest-rank; with
eight samples it is the maximum. Medians and gates use unrounded values.

## Decision rule

For each workload, using median wall costs:

```text
pipeline N2       = F / H
pipeline N1       = E / G
canonical shipped = B / H
matched shipped   = D / H
diff in diff      = (F / H) / (B / D)
serial drift      = E / A, F / B
baseline jobs     = C / A, D / B
```

KEEP requires all of the following:

- geometric means of `F/H` and `B/H` across W1 and W2 are strictly greater
  than 1.10;
- each workload's `F/H`, `B/H`, `D/H`, and difference-in-differences is
  strictly greater than 1.0;
- N1 route-on median and P95 costs `G/E` are at most 1.05;
- serial drift and baseline job controls remain within `[0.95, 1.05]`;
- H P95 is at most 1.05 times both F and D P95;
- median allocation ratios `G/E`, `H/F`, `G/C`, and `H/D` are at most 1.10;
- the corresponding process-tree CPU and aggregate-RSS ratios are at most
  1.25;
- every provenance, semantics, trace, topology, and cleanup invariant passes.

A genuine result such as 1.165x clears the substantive threshold and is worth
keeping when the other gates pass. The separately reported 1.25x tier requires
both workloads independently to reach at least 1.25x for both pipeline and
canonical-shipped comparisons. Missing that stronger tier does not turn a
greater-than-1.10 KEEP into speculation.

Producer-only and assessment-only phase balance was considered as a workload-
design heuristic but was not implemented and is not a release gate. No private
phase instrumentation was added. The decision rests on unreplaced production
end-to-end `F/H` and `B/H > 1.10` results together with the preregistered
semantic, route, drift, tail, allocation, CPU, RSS, provenance, and cleanup
controls above.
