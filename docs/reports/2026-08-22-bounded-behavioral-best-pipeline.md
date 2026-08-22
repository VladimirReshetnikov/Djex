# Bounded behavioral SelectBest candidate pipeline

Date: 2026-08-22

## Final outcome: HOLD and production revert

The corrected, preregistered release screen completed successfully, but the
one-candidate-ahead route did not meet its performance and direction gates.
The immutable evidence is at
`/tmp/djex-candidate-pipeline-screen-one-shot-20260822-ea369674-v2-02`.
It contains all 160 planned rows: 16 traced preflights, 16 warmups, and 128
unreplaced measurements. There were no failure attempts, finalization
failures, termination requests or vetoes, cleanup failures, or residue.

Semantic transcripts, query vectors, solver topology, host controls, serial
drift, tail latency, allocation, process-tree CPU, aggregate RSS, provenance,
and cleanup all passed. The decisive geometric means did not:

| Comparison | Final ratio | Required |
| --- | ---: | ---: |
| Pipeline, `F/H` | `1.0180039235801621` | strictly greater than `1.10` |
| Canonical shipped, `B/H` | `1.000683278814482` | strictly greater than `1.10` |

The workload-level direction controls also failed for W1: `B/H` was
`0.9905676361` and `D/H` was `0.9940672572`. The stronger `1.25x` tier was
false. A valid positive result above `1.10x` would have been substantive and
worth keeping; this result simply did not reach that threshold.

The production connection introduced at
`aff7a5e8d0fe81f50b05c5073ff05f77e1ab68ca` was therefore reverted exactly at
`73ec1891db0c9362d11f6a35e2eeeeea5c031241`. The tested private foundation from
`0716144502a7dcd8bfa8755f57fd9ced58bf3b83` remains package-private, with no
production call site and no public API.

Current production behavior is consequently simple: every behavioral
Exference query uses the serial candidate presenter for every `select` and
`jobs` value, owns one live Length/Z3 session, and performs all admission,
solver traffic, diagnostics, ranking, and presentation on its owner thread.
`jobs >= 2` still admits the independent paired-backend scheduler for eligible
unconstrained Djinn-plus-Exference queries; it no longer changes behavioral
candidate preparation.

## Experimental production boundary

The reverted route was deliberately smaller than general parallel candidate
checking:

- only behavioral Exference `SelectBest` queries were eligible;
- the configured `jobs` value, not RTS capabilities, controlled admission;
- `jobs = 1`, `SelectFirst`, `SelectAll`, and unconstrained queries called the
  historical serial presenter literally;
- the producer advanced the lazy typed-result trace and evaluated only
  `typedCandidateCompatibility` to weak-head normal form;
- it never forced the typed term graph or residual constraints and required no
  `NFData` instance;
- one permit bounded preparation to exactly one candidate ahead;
- explicit nonempty-batch events preserved whole-batch assessment before
  ranking; and
- the owner alone called `admitLengthWhereCandidate`, used the live solver,
  emitted diagnostics, ranked candidates, and presented the selected result.

The pipeline's `withAsync` scope was inside the unchanged live-session bracket,
which was inside the unchanged outer query timeout. A timeout, caller
exception, or owner exit therefore cancelled and joined the pure producer
before the live-session finalizer owned solver cleanup. No second Z3 process
or public API was introduced.

The retained package-private carrier is
`Language.Haskell.Djex.REPL.CandidatePipeline`, a Cabal `other-modules` entry.
Its producer acquired a single permit before demanding the next result. FIFO
candidate, nonempty-batch, and terminal events preserved stable tie order,
progress laziness, and serial exception precedence. Focused tests pin one-ahead
backpressure, empty batches, stable ties, owner-versus-producer identity,
producer and owner exception precedence, lazy progress, and cancellation/join
behavior. Those tests remain useful foundation evidence; they do not connect
the carrier to production.

While connected, the CLI gate used a fresh fake Z3 process and checked
byte-identical jobs-one/jobs-two output, repeated jobs-two output, jobs-two
behavior under `+RTS -N1`, and exact query ordinals `0` through `9`. A
status-phase hang produced the established timeout diagnostic, made no value
request, rendered no candidate, and allowed a healthy follow-up query in the
same REPL. Independent strict validation passed 40/40 parallel tests and 95/95
CLI tests, plus repeated `-N1` and `-N2` runs without leaked Djex or fake-Z3
processes.

## Frozen performance protocol

The benchmark compared eight cells: baseline and candidate binaries, each at
jobs one and two and RTS capabilities one and two. It used two fixed real-Z3
workloads:

- W1 performed 24 symbolic modulo/length assessments, accepted all 24, and
  rendered 24 tied best candidates; and
- W2 performed 48 assessments, split 24 accepted and 24 replay-refuted, and
  rendered the best six.

Every cell received a traced semantic/topology preflight and one warmup. Eight
unreplaced measurements per workload/cell then followed an eight-row Williams
schedule with every directed carryover exactly once. The runner bound exact Git
trees, optimized binaries and build IDs, Cabal plans, Python payload, pinned Z3
package and executable, dynamic libraries, workload bytes, and host
cgroup/affinity controls. Preflight reconstructed the actual returned SMT-LIB
bytes from solver `strace` files.

KEEP required both geometric-mean end-to-end comparisons, `F/H` and `B/H`, to
be strictly greater than `1.10`; every workload's `F/H`, `B/H`, `D/H`, and
difference-in-differences to be positive; and all preregistered tail,
serial-drift, allocation, process-tree CPU, RSS, semantic, provenance, and
cleanup controls to pass. The separately reported `1.25x` tier was stronger,
not the minimum KEEP threshold. The complete frozen protocol is in the
[benchmark README](../../bench-candidate-pipeline/README.md).

## Attempt 1: instrumentation HOLD

Protocol v1 ran exactly once at
`/tmp/djex-candidate-pipeline-screen-one-shot-20260822-b9f0ab0c`. It completed
all 16 preflights and W1/A warmup, then stopped before appending W1/B warmup
with `HarnessFailure: observed 0 sealed solver images`. Its 17 completed rows
contained zero measured rows and therefore supplied neither a KEEP nor a
revert signal.

The failing baseline W1/B invocation had complete input, stdout, stderr, RTS,
cleanup, and process-tree artifacts. Its transcript was byte-identical to the
valid W1/B preflight and W1/A warmup. The sampler tracked the direct child PID,
start time, and CPU ticks but retained no executable descriptor. Z3 4.8.12 had
mutated its `key=value` argv strings into a deterministic NUL-separated parsed
state, while v1 accepted only the pristine argv. Incomplete task-children
hints, delayed inspection, and silently discarded procfs/signature telemetry
were additional instrumentation gaps. None of those findings promotes the
attempt.

| Evidence | SHA-256 |
| --- | --- |
| Decision | `e1398907ce7a8ec26c056862007ade9b5cc130c985590671ca8b74b5961a81ee` |
| Partial results | `238f476f230dd40ca4bfe683c7ce1dd7d33b319dab8874f0259d94c176e74c64` |
| Provenance | `f9a2aa2449b4648e8e1a60cd5d3d87e505b3aa57e6fddebe708358a8ad610433` |
| Schedule | `82b3075105fce3c6b84cfd6161c9b346391a9fd5e771335d0b6c5a1fa80c5b47` |
| Full 315-file content manifest | `c93ed82e6ad7ecd0b70ebe536699b6bf4486fce688462bd816ed912022a94051` |

The immutable tree contains 315 files totaling 677,149,306 bytes. Independent
audit recomputed the decision-linked evidence, partial-row metrics, 16 query
vectors, host controls, cleanup state, and full manifest. No private residue or
live process remained.

## Sampler calibration v2-01: mode HOLD

The first diagnostic v2 calibration ran once at
`/tmp/djex-solver-sampler-calibration-20260822-7920655b-v2-01`. It stopped in
its first baseline W1/B invocation with zero completed rows and zero captures.
The sampler observed the exact parsed Z3 argv 212 times and passed target,
open, regular-file, size, and live device/inode gates each time. Its sole
consistency mismatch was mode: production staged the pinned ordinary executable
as `0755`, while v2-01 incorrectly required `0500`. Exact-target cardinality
correctly forced diagnostic HOLD.

| Evidence | SHA-256 |
| --- | --- |
| Decision | `e4546cc010a2b7353629160278ece9614acc2406b799cdad2dbd6cca3b1087be` |
| Header-only results | `504ec031e2ae8882af356d4e8a02ae18e4557a19298c16b354a34e69cad9c2a4` |
| Provenance | `859b97ecec96402c43ee6b790b517164503e06a4e353f7e402834d7fdf345feb` |
| First process tree | `612d718522538966717f086c2764b8cee7268c6f2f35fdb62b9fa5882d8654e8` |
| Start/end identity | `ca33ebebad5c0da1b07bf7ce2291c88ce6ae58918421bff34865bc76a3a069a0` |
| Identity attestation | `46e2f74e1f42cc3906f735caa9b4142679e47f08ae297c2eaac8092607cc8f86` |

The immutable tree contains 13 regular files totaling 40,648 bytes. End
identity passed and there were no finalization failures.

## Sampler calibration v2-02: cadence HOLD

After the exact `0755` source-mode repair, the second diagnostic calibration
ran once at
`/tmp/djex-solver-sampler-calibration-20260822-c4a76370-v2-02`. It durably
appended 42 valid W1/B rows. Invocation 43 retained the exact sealed image,
passed singleton identity and cleanup, but its 3,101,996 ns mean interval
exceeded the unchanged 3,000,000 ns gate. It failed before a row was built, so
no exit-status or semantic PASS is inferred for that invocation.

The cause was a relative sleep after every sampling pass, which added the
pass's own work to the nominal interval. The repaired scheduler uses fixed
monotonic pass-start deadlines, coalesces missed ticks into at most one
immediate catch-up, and schedules a full interval after that catch-up. The
3 ms quality gate was not relaxed. This attempt also exposed a durability gap:
its unappended failing artifacts were not top-level cryptographically anchored.
The repair added a canonical, fsynced failure-attempt manifest without
fabricating a result row.

| Evidence | SHA-256 |
| --- | --- |
| Decision | `2852a3d5325bf8483842770db2d304628078108eb0c5e40d74db5fd91ee18e07` |
| 42-row results | `d79296795821e4db6863d0ed10d52f00cac815e4ab72da6483784e7f2c9a553d` |
| Provenance | `10aacd6cd628a7aa2d4b1f434a13e502c02745afb791a2d862fe250f135ca64e` |
| Invocation-43 process tree | `c2136e5edb64201db5ca67d18cc45881005fbbbf1fc83ff7269c6ee226dadf56` |
| Start/end identity | `44c53f45a2f23c0bbcc247c0fd56d000539b1a95583d1618af5a5bd3d44fcf68` |
| Identity attestation | `7abc50295a1179188876c56e324183e0ab8d84d2d4738eb490eb4fbbc1dd0274` |

The immutable tree contains 307 regular files totaling 672,508 bytes. It is
diagnostic HOLD evidence only and remains unchanged.

## Sampler calibration v2-03: diagnostic PASS

The independently frozen fixed-rate repair ran exactly one diagnostic
calibration at
`/tmp/djex-solver-sampler-calibration-20260822-ea369674-v2-03`. All 64 required
baseline W1/B invocations were appended and fsynced, each retained exactly one
sealed Z3 image, and the decision recorded `diagnostic_only: true` and
`release_evidence: false`.

Sampler mean intervals ranged from 1,968,576 ns to 2,315,338 ns, with median
2,096,867.5 ns and nearest-rank P95 2,264,496 ns. The worst row retained
684,662 ns, or 22.82%, of headroom below the unchanged 3,000,000 ns gate.
There were no failure attempts, finalization failures, termination requests or
vetoes, cleanup failures, or residue.

| Evidence | SHA-256 |
| --- | --- |
| Decision | `094d3252f6a4ce12d08b6b85df0608a87061abb2722a24ba236bd79a1d141e19` |
| 64-row results | `7395a8294b149eb6ee5197240278ac32db6e9a21d1a70c63186547b22718d719` |
| Provenance | `21ff55ad26902a4353f44b163866353fcdd96c0fdcc7dcb49b3d34f60372e057` |
| Start/end identity | `71064aa354cfe27e2e14dfed041537f67ecc134b35e3cedf4dd9a441d99323ce` |
| Identity attestation | `23a5eab4916557643311ef0f7299da9ace6c09e9eaa815322d1f462fe8b7f49a` |
| Relative-path full manifest | `a7896198ca064dd9fa7e33f6413bb980b286da3e685caabb08237109b03b42fe` |

The immutable tree contains 454 regular files totaling 1,008,901 bytes. This
PASS validated the sampler only; it neither replaced earlier HOLD evidence nor
made a performance decision.

## Final release-screen evidence

With the corrected sampler frozen, the release screen ran exactly once at
`/tmp/djex-candidate-pipeline-screen-one-shot-20260822-ea369674-v2-02`.
All rows and all non-performance controls passed. The complete comparison was:

| Metric | W1 | W2 | Geometric mean where applicable |
| --- | ---: | ---: | ---: |
| Pipeline `F/H` | `1.0117322535` | `1.0243144714` | `1.0180039235801621` |
| Canonical shipped `B/H` | `0.9905676361` | `1.0109022221` | `1.000683278814482` |
| Matched shipped `D/H` | `0.9940672572` | `1.0189985382` | — |
| Difference in differences | `1.0153066480` | `1.0325182062` | — |

The pipeline and canonical geometric means missed `>1.10`, while W1 canonical
and matched comparisons were not positive. Resource ratios remained within
their gates, the serial and host controls passed, and the separate `1.25x`
tier was false. The outcome is therefore final behavioral candidate-pipeline
HOLD, not KEEP.

| Evidence | SHA-256 |
| --- | --- |
| Decision | `28f2aa788aaf4199e9c509c8693ac6443f6077727ef5f4cddbe2a87a550a58b5` |
| 160-row results | `4d7e0e24e81d6ebba12cf25de8cf19565e54c5df80f37db2ac1e8702abdea868` |
| Provenance | `df998b0357795f91ee8793e3cb7cd4d4b7b58e06561125d0ec5d8209567cf51c` |
| Schedule | `82b3075105fce3c6b84cfd6161c9b346391a9fd5e771335d0b6c5a1fa80c5b47` |
| Host-control window attestation | `a37b54a25b2148f3b8a39bff7b2e0ace5b338f5ef4dc7b3c76dadfc2dff70faf` |
| Full 1,308-file content manifest | `dea351c97bacc0984062133b7ab78139b3d8fcccbc0199600485a0c76100cff7` |
| Raw 1,300-file content manifest | `041137d15df91542f171c10942188ad8a78c3550a3c53a5c736410631ef88f9b` |

The full manifest covers 1,308 files totaling 679,203,342 bytes; the raw
manifest covers 1,300 files totaling 679,006,506 bytes. Independent audit
recomputed the frozen evidence and confirmed the absence of failed attempts,
finalization or termination faults, cleanup failures, and residue.

## Disposition

Attempt 1, calibrations v2-01 and v2-02, diagnostic PASS v2-03, and the final
release screen remain immutable at their distinct paths. No sample was
replaced or promoted across protocols. The experiment is complete: retain the
private tested foundation, retain the benchmark and audit history, keep
behavioral production serial, and do not claim an acceleration from this
route.
