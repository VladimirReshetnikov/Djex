# Bounded behavioral SelectBest candidate pipeline

Date: 2026-08-22

## Current outcome

Djex now has a narrowly routed producer/consumer pipeline for behavioral
Exference queries using `select = best` and `jobs >= 2`.  Search can prepare
one typed candidate ahead while the owner thread performs the existing serial
Length/Z3 assessment.  Candidate admission, solver traffic, warnings, ranking,
batch commitment, and presentation remain on that single owner thread and use
one live Z3 session.

The implementation and semantic gates are green, but this report makes no
speed claim yet.  The first preregistered performance screen stopped before
measurement because its process sampler failed to retain one baseline Z3
image.  That invocation is immutable `HOLD` evidence: it completed all sixteen
trace preflights and one warmup, but zero measured rows.  A separately named,
reviewed screen is required before the route receives a performance decision.

The production connection is commit
`aff7a5e8d0fe81f50b05c5073ff05f77e1ab68ca`.  Its private foundation is commit
`0716144502a7dcd8bfa8755f57fd9ced58bf3b83`.  The initial benchmark harness is
commit `a02054006d388645102e33e77fdab946e249e5d3`.

## Exact production boundary

The route is deliberately smaller than general parallel candidate checking:

- only behavioral Exference `SelectBest` queries are eligible;
- the configured `jobs` value, not RTS capabilities, controls admission;
- `jobs = 1`, `SelectFirst`, `SelectAll`, and unconstrained queries call the
  historical serial presenter literally;
- the producer advances the lazy typed-result trace and evaluates only
  `typedCandidateCompatibility` to weak-head normal form;
- it never forces the typed term graph, residual constraints, or an `NFData`
  instance;
- one permit bounds preparation to exactly one candidate ahead;
- explicit nonempty-batch events preserve the historical rule that the owner
  assesses a whole batch before ranking it; and
- the owner alone calls `admitLengthWhereCandidate`, uses the live solver,
  emits diagnostics, ranks candidates, and presents the selected result.

The pipeline's `withAsync` scope remains inside the unchanged live-session
bracket, which remains inside the unchanged outer query timeout.  A timeout,
caller exception, or owner exit therefore cancels and joins the pure producer
before the live-session finalizer owns solver cleanup.  No second Z3 process or
public API was introduced.

The package-private carrier is in
`Language.Haskell.Djex.REPL.CandidatePipeline`.  It is a Cabal
`other-modules` entry, not an exposed module.  The REPL connection exports only
the already-existing package-private `presentExferenceSelection` helper from
`Language.Haskell.Djex.Command`; the public facades remain unchanged.

## Demand and ordering proof

The producer acquires a single candidate permit before demanding the next
result.  FIFO events carry candidates, nonempty batch boundaries, and one
terminal outcome.  Empty result batches can be traversed without consuming a
second permit, while a later nonempty batch cannot finish before its first
candidate has been dequeued.  The queue therefore remains bounded by one
prepared candidate plus its adjacent control/terminal information.

The owner consumes events in encounter order.  It admits every candidate in a
batch before applying the existing rank function, retaining stable tie order.
Producer exceptions are observed only when the serial trace would next be
demanded; a rank from a completed earlier batch wins before a poisoned later
progress payload.  The terminal `Maybe Progress` payload stays lazy, matching
the old `SelectBest` presenter.

The foundation suite pins one-ahead backpressure, empty batches, stable ties,
owner-versus-producer thread identity, producer and owner exception precedence,
lazy progress, and cancellation/join behavior.  The connected CLI gate uses a
fresh fake Z3 process and checks byte-identical jobs-one/jobs-two output,
repeated jobs-two output, jobs-two behavior under `+RTS -N1`, and exact query
ordinals `0` through `9`.  A status-phase hang produces the established timeout
diagnostic, no value request or candidate rendering, and a healthy follow-up
query in the same REPL.  Independent strict validation passed 40/40 parallel
tests and 95/95 CLI tests, plus repeated `-N1` and `-N2` runs without a leaked
Djex or fake-Z3 process.

## Frozen performance protocol

The benchmark harness compares eight cells: baseline and candidate binaries,
each at jobs one and two and RTS capabilities one and two.  It uses two fixed
real-Z3 workloads:

- W1 performs 24 symbolic modulo/length assessments, all accepted, and renders
  24 tied best candidates; and
- W2 performs 48 assessments, split 24 accepted and 24 replay-refuted, and
  renders the best six.

Every cell first gets a traced semantic/topology preflight and one warmup.
Eight unreplaced measurements per workload/cell then use an eight-row Williams
schedule with every directed carryover exactly once.  The runner binds exact
Git trees, optimized binaries and build IDs, Cabal plans, the Python payload,
the pinned Z3 Debian package and executable, dynamic libraries, workload bytes,
and host cgroup/affinity controls.  It reconstructs actual returned SMT-LIB
bytes from the solver's `strace` file for preflight rather than trusting counts
or fabricated solver output.

A result is worth keeping when both geometric-mean end-to-end comparisons are
strictly greater than `1.10`, every workload-level treatment comparison is
positive, and the preregistered tail, serial-drift, allocation, process-tree
CPU, RSS, semantic, provenance, and cleanup gates pass.  This is a substantive
threshold: for example, a valid `1.165x` result is meaningful evidence, not
speculation.  The separately reported `1.25x` tier is stronger but is not the
minimum KEEP threshold.

The complete protocol and frozen workload details live in the
[benchmark README](../../bench-candidate-pipeline/README.md).

## Attempt 1: instrumentation HOLD

The first one-shot output is preserved at
`/tmp/djex-candidate-pipeline-screen-one-shot-20260822-b9f0ab0c`.  It started
from a tracked-clean harness commit with only the unrelated, caller-owned
`ReplayLedger.hs` untracked.  The runner exited `2` with decision schema
`djex-select-best-candidate-pipeline-decision/v1` and primary failure:

```text
HarnessFailure: observed 0 sealed solver images
```

The failure was the second W1 warmup, cell B: baseline commit
`0716144502a7dcd8bfa8755f57fd9ced58bf3b83`, `jobs = 1`, and `+RTS -N2`.
The completed partial result contains exactly sixteen preflight rows and W1
warmup cell A.  No measured row exists, so the attempt supplies neither a KEEP
nor a production-revert signal.

The failed invocation itself has complete input, stdout, stderr, RTS statistics,
cleanup, and process-tree artifacts.  Its transcript is byte-identical to the
valid W1/B preflight and W1/A warmup.  The sampler tracked the direct child PID,
start time, and 11 CPU ticks across 679 passes with approximately 99.94% wall
coverage, but retained no `/proc/PID/exe` descriptor.  Because the initial
sampler silently discarded procfs read/open failures and signature mismatches,
the exact missed observation cannot be recovered after process exit.  This is
why the fail-closed outcome is correct even though no candidate semantic fault
was observed.

The decision binds these top-level hashes:

| Evidence | SHA-256 |
| --- | --- |
| Decision file | `e1398907ce7a8ec26c056862007ade9b5cc130c985590671ca8b74b5961a81ee` |
| Partial results | `238f476f230dd40ca4bfe683c7ce1dd7d33b319dab8874f0259d94c176e74c64` |
| Provenance | `f9a2aa2449b4648e8e1a60cd5d3d87e505b3aa57e6fddebe708358a8ad610433` |
| Schedule | `82b3075105fce3c6b84cfd6161c9b346391a9fd5e771335d0b6c5a1fa80c5b47` |
| Host-control start | `6001c33150366d603a12972bf569652f3a84e247afd807782c47c60a933a1a66` |
| Host-control start attestation | `183a8d431f71adb4b0009c6a4faa17ee67c7be489f9fde2b4439747086cae027` |
| Host-control end | `ad90bea75d5384a53ce9a5e675393c33a1a1a131b26a709ca98a22865b1e845d` |
| Host-control window attestation | `314fce7cfe72c5d9b40f1b60b92556e07641ff49317f884914167a7bc3d89302` |

Independent audit recomputed every decision-linked hash, all 38 provenance
path/hash references, every partial-row metric, and all sixteen query vectors.
The W1 vector remained 24 transactions with SHA-256
`532772f41a2cfe8a362a20530239875f091700d2350880878ed6dee07681d883`;
W2 remained 48 transactions with SHA-256
`b0c1d2d38e5de857b2e58a6b4beafb639b0db03691218b12b70c5541b0b679dc`.
Host affinity and cgroup controls were stable, and both exposing ancestors had
zero `nr_throttled` and `throttled_usec` deltas.  No private residue or live
process remained. The preserved tree contains 315 files totaling 677,149,306
bytes. Its full content-manifest SHA-256 is
`c93ed82e6ad7ecd0b70ebe536699b6bf4486fce688462bd816ed912022a94051`:
the audit sorted all absolute file paths as NUL-delimited bytes, ran GNU
`sha256sum` on each in that order, and hashed the resulting 315 textual records.
The separately hashed ordered path list is
`7d9d8f566ef09a9f77099295aab0af31701b074deb52115b4d7f187adcd8d0ea`.

## Next gate

Attempt 1 will not be rerun, altered, or reused.  Before another full screen,
the sampler must retain exact live-image identity while making discovery and
capture failures classifiable.  The planned repair adds a session-ID fallback
beside the cheap child walk, opens the executable descriptor before reading
mutable process metadata, rechecks PID/start/session/image identity around the
capture, and records categorized per-PID observation telemetry.  Deterministic
sealed-memfd, injected-failure, retained-descriptor, and unexpected-second-
solver tests must pass, followed by an explicitly non-decision single-cell
calibration.

Only after that repair is committed and independently audited may a fresh,
separately named full screen run once.  Until then the connected route is
provisional, the old serial path remains available with `jobs = 1`, and no
release acceleration claim is made.
