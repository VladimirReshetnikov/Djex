# Incremental Djinn behavioral synthesis

Status: the current three-family streaming implementation is under validation.
The focused ten-test group, complete Djex suites, and both Haskell behavioral
profiles passed. Final Leant integration and its live matrix remain pending.
This report does not claim completion or publication.

## Scope and acceptance requirements

The goal is to deliver Djinn candidates through behavioral checking without
collecting the remaining bounded search pool first, in both the Haskell Djex
frontend and the Lean Leant frontend.

Acceptance requires:

1. A source-aware typed stream that validates its exact request and provider
   evidence before search and preserves each candidate's own source graph.
2. Early delivery without global sorting or forcing an unselected proof,
   candidate, graph, error, or final search verdict.
3. One cumulative raw-proof cutoff and choice allowance, including discarded
   proofs and duplicates, with deterministic depth-first and interleaved
   progression and explicit completion/truncation diagnostics.
4. Haskell behavioral first-selection stops after success; bounded best/all
   selection continues correctly. Late errors do not invalidate an earlier
   checked candidate and are not demanded after early success.
5. Lean named queries resume the same stream through existing verification,
   success quotas, deadlines, and Both scheduling, retaining exact source
   ownership. False and inconclusive outcomes cannot consume success slots.
6. Ordinary unnamed synthesis retains its batch selection and sound negative
   evidence. New empty terminal observations do not repeat positive evidence
   or manufacture a refutation from a truncated search.
7. Current builds and focused/full affected tests pass, including independently
   checked graph erasure, scoped provider cases, and real frontend controls.
8. All six Church behavioral operations pass at their recorded per-engine
   settings in both Haskell engines and all three Lean modes, with exact
   emitted-term compiler/kernel replay and actual false-predicate controls.
9. Before/after measurements distinguish visible-success latency, complete
   process time, executable identity, and observation overhead. They do not
   substitute timing for semantic acceptance.
10. Public API and frontend documentation describe discovery-order streaming,
    unchanged ordinary batch behavior, resource limits, and measured results.

The supplementary Leant diagnostics distinguish graph absence from target-only
reconstruction over selected rendered groups. They carry no source or logical
authority and must not force an unselected candidate tail.

## Current implementation

The additive `runDjinnTypedQueryStream` family returns eager request failure
or a lazy list of per-observation `Either` results. All three provider-evidence
forms have corresponding stream runners. A candidate observation is a
singleton `Continuing` typed batch; completion is one empty terminal batch.
Existing resumable proof cursors supply the stream, and graph identities are
distinct across all its candidate occurrences.

Streaming uses deterministic discovery order. Existing local strategy and
provider costs remain search inputs, while whole-pool ranking stays with the
ordinary batch API. The first passing definition can therefore differ from
the historical pooled result. Incremental deduplication retains compact
eta-normal expressions, not consumed typed graphs or proof trees.

Explicit interleaved streaming now rotates among three independent families:

- historical plans and their retained cursors;
- existing exact-demand instantiation groups and singleton bridges;
- the remaining carrier groups, including function-valued accumulator plans.

Each family owns a FIFO of active cursors and lazy plan sources. Increasing the
historical family's plan population cannot dilute another family's turn
frequency. Empty families lend their turns without examining a proof cursor.
The first turn stays historical, and a turn ends after one raw proof or the
existing 64-choice quantum. All observed choices and raw proofs spend the one
query allowance; scheduling supplies no additional fuel.

Demand selection compares exact final-result formulae from the admitted
instantiation inventory. A singleton bridge can be reused for two
alpha-equivalent quantified inputs; it is not suppressed merely because an
earlier plan produced a projection or constant. Common groups keep their
original symbol indices, member order, visible arguments, and source evidence.
Carrier-only proofs must actually use their admitted bridge. No new axiom is
invented by this scheduling preference, and every added plan remains
positive-only.

Within a carrier context whose final result matches the goal, including a
function-valued accumulator ending at that result, the producer preserves the
historical first proof and then favors increasing-size normal forms. Its
finite inner work quanta are 64 choices for the LJT tail and 4,096 for the
normal-form stream; either stream yields its turn immediately after a proof.
The outer family scheduler still preempts after its own 64-choice quantum.
Both continuations remain live, and every step is charged individually before
continuation. This is effort allocation, not a larger search limit or a claim
of exhaustive normal-form enumeration. Ordinary batch, depth-first, and
first-only search schedules retain their established policies.

Djex's named behavioral path retains the typed candidate handle while rendering
its compatibility projection for GHC. Leant maps each observation through its
existing exact source renderer and checks a group before resuming the stream.
Each frontend retains its own observation window and behavior-selection rules.
For Djinn's singleton observations, best-lookahead counts observations without
an improvement. Its first-selection path does not inspect a later graph,
candidate, error, or terminal verdict after success.

At a raw cutoff, streaming reports that cutoff without looking beyond it.
The historical depth-first batch path probes the overflow tail and can instead
encounter choice exhaustion there. The stream deliberately avoids that extra
work; candidate-set parity is checked separately from the exact consumed
cursor-prefix status. Neither incomplete result provides negative evidence.

## Frozen baseline

The baseline executables were copied before the streaming build:

| Frontend | Published source revision | Executable SHA-256 |
| --- | --- | --- |
| Djex | `4051f41f6d3b4886fa99627d2c6bf5a48e046e90` | `1682dfa30bddbd956997bf7ca4c7f9e02551cae0cb884d1b06ced557a8aff765` |
| Leant | `62f7b536172b0a8ba7dfe9a6251c4b26d576c4aa` | `bc041b71eb86e7787e5bd4461635702ed34fdf97804c7c26d5e9994d4b7e15fc` |

The opt-in runner observes complete stdout lines in the unchanged direct
capture file using a monotonic clock and a 20-millisecond polling interval.
Observed time includes buffering and operating-system scheduling. Haskell
reports process-entry-to-definition visibility, including startup; Lean also
records echoed-query-to-result visibility. Final exact transcript parsing and
independent replay must agree with the observed success-line inventory.

The fresh Haskell Djinn baseline passed all six operations, its actual false
control, and 23 independent execution assertions. Each accepted definition,
predicate and replay module matched the earlier source-graph acceptance run.
The full baseline receipt is retained locally under
`dist-newstyle/streaming-acceptance/baseline-haskell-djinn/results.json`, SHA-256
`59439a755c7bb7b48c74bd47b22eb297717a1b61a68224d6b75435f5e7ca330f`.
The fresh Lean Djinn baseline also passed six operations and its false control,
with 45 empty kernel axiom inventories across the isolated candidate and oracle
modules. The other three frozen baseline profiles also passed: Haskell
Exference, Lean Exference, and Lean Both. Together the five baselines cover all
30 host/engine/operation cells, with actual false-predicate controls and
independent execution or kernel replay. Current-executable acceptance remains
open.

## Current validation matrix

The current focused streaming group passed **10 tests in 1.84 seconds**, with
its receipt at `dist-newstyle/streaming-acceptance/carrier-core-focused.log`.
It covers early delivery with a poisoned global-ranking field and large tail,
both strategies' cumulative allowances, graph identity and erasure, terminal
evidence, contextual authority, eager request validation, a reused singleton
bridge, cooperating schemes, a function-valued accumulator, and finite inner
proof fairness with exact choice accounting. The composition-prefix checks
are focused calibration requirements, not universal latency bounds.

The final strict Djex build passed with GHC 9.12.4 and `-Werror`. The complete
serial suites passed 132 core tests in 59.86 seconds, 98 shared-facade tests in
12.72 seconds, and 98 CLI tests in 71.22 seconds. Their source/executable hashes
and process records are retained in
`dist-newstyle/streaming-acceptance/carrier-djex-units/results.json`.

The current Haskell Djinn profile passed all six operations, its actual false
control, and **23 independent execution assertions**. Both compiler and
execution exit codes were zero, and the executable remained unchanged. The
receipt is `dist-newstyle/streaming-acceptance/carrier-haskell-djinn/results.json`,
SHA-256 `e839d16b05099a4268598a870435721f757f334400534008fa89f72bf700f258`.
Its executable SHA-256 is
`2a500d9cb6cffb55ed714ee81af2041d0239e181d46470566584c30d999724f7`.

| Haskell Djinn operation | Frozen baseline first visible (s) | Current first visible (s) | Current predicate attempts | Current candidate-check errors |
| --- | ---: | ---: | ---: | ---: |
| `not` | 69.599 | 1.724 | 6 | 0 |
| `swap` | 30.825 | 1.219 | 1 | 0 |
| `map` | 39.562 | 1.251 | 2 | 0 |
| `append` | 114.598 | 2.232 | 109 | 0 |
| `reverse` | 77.990 | 4.370 | 242 | 1 |
| `filter` | 20.622 | 2.442 | 106 | 0 |

The independent receipt audit found all six exact definitions and all eight
replay modules equal to the frozen baseline. Median first-visible time across
the six operations changed from 54.581 to 1.978 seconds. These are single-run
visibility observations including startup, not a statistical speed guarantee.
Positive-query attempts changed from 503 to 466 and candidate-check errors from
11 to 1, with no predicate timeouts. These attempts are behavioral observations,
not counts of raw proofs or search choices. The improvement is not uniform:
reverse used 242 observations compared with the baseline's 78.

The current Haskell Exference profile also reports six passes, its false
control, and 23 independent execution assertions for the same executable.
Its receipt is `dist-newstyle/streaming-acceptance/carrier-haskell-exference/results.json`,
SHA-256 `fe3f1ac4c293e8380fcc2b87bf9edc1ef761a84c75dbcfa015a8e6f14b40f8ff`.
Its independent paired audit passed: all six definitions, all eight replay
modules, commands, limits, and oracle/harness hashes match the baseline.
Positive-query counters remain 196 checked, 6 true, and 190 false, with no
errors or timeouts. The actual false control again rejected 256 candidates.

| Current implementation gate | Status |
| --- | --- |
| Focused ten-test streaming group | Passed; receipt above |
| Final strict Djex build and complete affected suites | Passed: 132 core, 98 facade, 98 CLI |
| Final strict Leant build, full suite, and vendor integration | Pending final receipts |
| Haskell Djinn: six operations, false control, independent GHC execution | Passed for the executable identified above |
| Haskell Exference: six operations, false control, independent GHC execution | Passed; independent paired audit passed |
| Lean Djinn, Exference, and Both: six operations each, false controls, independent kernel replay | Pending current-executable matrix |
| Paired current-versus-frozen timing and identity checks | Pending complete matrix |
| Final documentation and repository integration | Pending |

Final acceptance must bind the exact tested executables and source inputs to
the complete 30-cell host/engine/operation matrix. The historical receipts
below are retained to explain the regression and the repair; their compiler
and runtime passes do not certify the revised scheduler.

## Historical phase-one evidence

The initial streaming implementation passed strict GHC 9.12.4 `-Werror`
builds and the following tests before the scheduler repairs:

| Historical test gate | Result |
| --- | --- |
| Djex core | 128 passed in 126.91 seconds, including six streaming tests |
| Djex shared facade | 98 passed in 12.19 seconds |
| Djex CLI | 98 passed in 120.97 seconds |
| Leant focused streaming/observability | 32 passed |
| Leant full suite | 615 passed in 430.06 seconds |
| Observer/comparison harnesses | 17 Djex observer, 9 Lean observer, and 5 paired-receipt tests passed |

The historical Leant executable SHA-256 was
`0c22c7ab765490bbd266003bb02f1af29a54a32d5af435ab2bff56126f7bc9cf`;
its test executable was
`2bc2b11fe08f273cc7632fd8fd2c5d3c348237e0f8448db117c589eff781451a`.
These identities are phase-one evidence, not the current build identities.

The first live Lean Djinn profile exposed an acceptance regression: `append`
and `filter` reached their unchanged **120-second** command deadlines after
**240** and **293 falsified candidates**, respectively. Four other operations
displayed passing results, and the real false control recorded **five
falsifications**. The harness rejected the incomplete six-operation profile
before independent kernel replay. It is therefore **not a passed Lean
acceptance result**. The receipt remains at
`dist-newstyle/streaming-acceptance/after-lean-djinn/results.json` in Leant.

The initial Haskell Djinn profile did pass six operations, the real false
control, and 23 independent execution assertions. Its receipt is
`dist-newstyle/streaming-acceptance/after-haskell-djinn/results.json`, SHA-256
`1b22717a311d9fc0910d0a028323e923b56803f360fff12d1e21f9e6b1ab150b`.
Its executable SHA-256 was
`a4e013f6ed5bfee8cf90a323965b2aab313e95cab107674f49b9dfc1a6a2043a`.
Exact queries, limits, corpus sources, and oracle/harness hashes matched the
frozen baseline. The following timings belong only to that initial profile:

| Operation | Frozen baseline first visible (s) | Historical phase-one first visible (s) |
| --- | ---: | ---: |
| `not` | 69.599 | 1.335 |
| `swap` | 30.825 | 1.039 |
| `map` | 39.562 | 1.073 |
| `append` | 114.598 | 25.634 |
| `reverse` | 77.990 | 14.136 |
| `filter` | 20.622 | 23.484 |

These single-run visibility measurements include startup and do not establish
a statistical performance guarantee. Positive-query predicate attempts rose
from 503 to 2,367, including an increase from 11 to 109 candidate-check errors;
all six accepted results still passed their independent checks, and neither
run had a predicate timeout. Predicate attempts do not measure raw proof or
choice work. Discovery order changed the accepted spelling of `not`; the
other five exact definitions and replay modules were unchanged.

Later demand-first append/filter diagnostics retained only the first three
errors per query and identified GHC polytype-inference and skolem failures
among those samples. They do not establish graph availability for the failed
candidates or a defect in the source checker. The concrete follow-up is
[source-evidence-driven Haskell elaboration](2026-09-06-synthesis-next-priorities.md#1-source-evidence-driven-haskell-elaboration).

The three-family scheduler and finite carrier normal-form priority address
work allocation before behavioral checking. Their final acceptance and
performance results belong in the current matrix above. No publication result
is claimed for this change.
