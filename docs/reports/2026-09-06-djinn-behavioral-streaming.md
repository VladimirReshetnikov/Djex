# Incremental Djinn behavioral synthesis

Status: **acceptance passed** for the three-family streaming implementation.
Strict builds, complete affected suites, the 30-cell behavioral matrix, and
the three additional Lean control profiles passed. This report identifies the
tested source and executables; it does not claim repository publication.
The [compact final receipt](../../test-church/receipts/behavior-streaming-final.json)
records the acceptance identities and supporting receipt hashes.

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

## Accepted implementation

The additive `runDjinnTypedQueryStream` family returns eager request failure
or a lazy list of per-observation `Either` results. All three provider-evidence
forms have corresponding stream runners. A candidate observation is a
singleton `Continuing` typed batch; completion is one empty terminal batch.
Existing resumable proof cursors supply the stream, and graph identities are
distinct across all its candidate occurrences.

Streaming uses deterministic discovery order. Existing local strategy and
provider costs remain search inputs, while whole-pool ranking stays with the
ordinary batch API. A first passing definition can therefore differ from the
historical pooled result, although all 30 accepted spellings in this acceptance
matrix were unchanged. Incremental deduplication retains compact eta-normal
expressions, not consumed typed graphs or proof trees.

Explicit interleaved streaming rotates among three independent families:

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
continuation. This allocates effort within the existing limit; it does not
establish exhaustive normal-form enumeration. Ordinary batch, depth-first,
and first-only search schedules retain their established policies.

Djex's named behavioral path retains the typed candidate handle while rendering
its compatibility projection for GHC. Leant maps each observation through its
existing exact source renderer and checks a group before resuming the stream.
Each frontend retains its own observation window and behavior-selection rules.
For Djinn's singleton observations, best-lookahead counts observations without
an improvement. First-selection does not inspect a later graph, candidate,
error, or terminal verdict after success.

At a raw cutoff, streaming reports that cutoff without looking beyond it.
The historical depth-first batch path probes the overflow tail and can instead
encounter choice exhaustion there. Candidate-set parity is checked separately
from the exact consumed cursor-prefix status. Neither incomplete result
provides negative evidence.

## Tested identities and frozen baseline

The baseline executables were copied before the streaming build. Leant was
built from the root source revision below with its Djex vendor checkout at
`6a964389`; the build record pins this combination separately. Subsequent
documentation and vendor-integration commits preserve that tested source.

| Frontend | Frozen baseline source | Accepted source |
| --- | --- | --- |
| Djex | `4051f41f6d3b4886fa99627d2c6bf5a48e046e90` | `6a964389` |
| Leant | `62f7b536172b0a8ba7dfe9a6251c4b26d576c4aa` | `00e9655` |

| Executable | SHA-256 |
| --- | --- |
| Frozen Djex | `1682dfa30bddbd956997bf7ca4c7f9e02551cae0cb884d1b06ced557a8aff765` |
| Accepted Djex | `2a500d9cb6cffb55ed714ee81af2041d0239e181d46470566584c30d999724f7` |
| Frozen Leant | `bc041b71eb86e7787e5bd4461635702ed34fdf97804c7c26d5e9994d4b7e15fc` |
| Accepted Leant | `9a55eb231c5dd6608eb6750e87a5f05e2dad76cae98e20be197d2e9cf5d94424` |

All five frozen baseline profiles passed their six operations, actual
false-predicate controls, and independent execution or kernel replay. The
fresh Haskell Djinn baseline receipt is retained under
`dist-newstyle/streaming-acceptance/baseline-haskell-djinn/results.json`,
SHA-256 `59439a755c7bb7b48c74bd47b22eb297717a1b61a68224d6b75435f5e7ca330f`.

## Validation results

Both final strict builds passed with GHC 9.12.4 and `-Werror`. The complete
serial Djex suites passed **132 core tests**, **98 shared-facade tests**, and
**98 CLI tests** in 59.86, 12.72, and 71.22 seconds respectively. The complete
Leant suite passed **615 tests in 534.77 seconds**. The Djex process records
are in `dist-newstyle/streaming-acceptance/carrier-djex-units/results.json`.

The focused streaming group passed **10 tests in 1.84 seconds** before final
comment-only cleanup and the strict build. It covers poisoned global ranking
and an unevaluated tail, both strategies' cumulative allowances, graph identity
and erasure, terminal evidence, contextual authority, eager request validation,
a reusable singleton bridge, cooperating schemes, a function-valued accumulator,
and finite inner proof fairness with exact choice accounting. Its
composition-prefix checks are calibration requirements, not universal latency
bounds. The unchanged observer/comparison harnesses previously passed **31
tests**: 17 Djex observer, nine Lean observer, and five paired-receipt tests.

Haskell late-error retention and first-selection tail avoidance were supported
by source review and actual first/best/all CLI checks. No Haskell presenter
fault-injection test with a poisoned later error is claimed. Lean additionally
has a poisoned-late-presenter regression test.

| Accepted behavioral profile | Six operations | Actual false control | Independent replay |
| --- | --- | --- | --- |
| Haskell Djinn | Passed | Passed | 23 execution assertions |
| Haskell Exference | Passed | Passed | 23 execution assertions |
| Lean Djinn | Passed | Passed | 45 empty kernel axiom inventories |
| Lean Exference | Passed | Passed | 45 empty kernel axiom inventories |
| Lean Both | Passed | Passed | 45 empty kernel axiom inventories |

Each profile covers `not`, `swap`, `map`, `append`, `reverse`, and
`filter`, with **626 finite observations** across those operations. Independent
paired audits passed for the complete **30-cell matrix**. All 30 exact accepted
spellings matched their frozen baseline cells; both Haskell profiles also
retained all eight exact replay modules. Recorded commands, corpus/oracle
inputs, and limits were checked for paired comparability. These are finite
behavioral checks and exact-term replay, not proofs of every possible
behavioral law.

Three additional live Leant profiles passed independent kernel replay:

| Control profile | Named queries | Retained terms, including warmup/recovery | Empty kernel axiom inventories |
| --- | ---: | ---: | ---: |
| Quotas, false and inconclusive checks, recovery | 10 | 9 | 20 |
| Djinn partial results at a 10-second deadline | 3 | 77 | 154 |
| Both partial results at a 10-second deadline | 3 | 3 | 6 |

The quota profile checks that false or inconclusive assertions do not fill
success slots, widening can reach the other projection, two distinct successes
can fill a quota, and a later query still succeeds. These claims concern Djinn
and Both mode behavior; the second projection in the Both control uses an
Exference fallback. They do not claim that each engine independently proved
every inconclusive case.

At the deadline, the Djinn target retained 75 accepted terms below quota 256;
the Both target retained one accepted term below its quota. Both reported zero
falsified and zero inconclusive assertions for their target queries, explicitly
reported incomplete further search, and then passed recovery. Source inputs
and executable identities remained unchanged across all three control runs.
The Leant receipts are under
`dist-newstyle/streaming-acceptance/carrier-controls-quota/results.json`,
`carrier-controls-partial-djinn/results.json`, and
`carrier-controls-partial-both/results.json`.

## Before/after measurements

The observer reads complete stdout lines in the unchanged direct capture file
using a monotonic clock and a 20-millisecond polling interval. Visibility
includes child buffering, polling, and operating-system scheduling. Haskell
uses process entry to accepted-definition visibility, including startup. Lean
uses the actual echoed query to accepted-result visibility and separately
records process-entry timing. Exact final transcript parsing and independent
replay agree with the observed success-line inventory.

| Profile | Median first visible before (s) | Median first visible after (s) | Timing basis |
| --- | ---: | ---: | --- |
| Haskell Djinn | 54.581 | 1.978 | Process entry |
| Haskell Exference | 1.240 | 1.001 | Process entry |
| Lean Djinn | 30.752 | 7.497 | Query echo |
| Lean Exference | 0.626 | 1.010 | Query echo |
| Lean Both | 0.611 | 1.215 | Query echo |

These are **one observed run per cell**, not statistical latency guarantees.
The Lean Djinn first `not` result remained effectively unchanged from process
entry, **94.241 to 94.168 seconds**; its query-relative time decreased because
the query echo occurred later. Lean Exference and Both had slower medians in
this run. The evidence supports a substantial Haskell Djinn visibility
improvement and a lower Lean Djinn query-relative median, not a general
frontend speedup or uniformly faster startup.

For Haskell Djinn, positive-query predicate observations changed from 503 to
466 and candidate-check errors from 11 to one, with no predicate timeouts.
Current attempts were 6, 1, 2, 109, 242, and 106 for the six operations in the
order listed above. Reverse required more observations than its baseline
(242 versus 78), so reduced checking work was not uniform. These counters do
not measure raw proof counts or consumed search choices.

The complete paired timing and identity data, including all five summaries,
cell detail, process durations, and observer costs, are retained in
`dist-newstyle/streaming-acceptance/carrier-comparison/results.json`,
SHA-256 `02f65196fbdb5f22639c4eb8bb3bdddb19417a1c593f322252c640e712368ad4`.
Live process durations exclude independent replay and repeat across Lean cells
that share a process.

## Historical regression and follow-up

The initial streaming implementation passed its then-current builds and
unit tests, but its first live Lean Djinn profile exposed an acceptance
regression: `append` and `filter` reached their unchanged **120-second**
command deadlines after **240** and **293 falsified candidates**, respectively.
Four other operations displayed passing results, and the actual false control
recorded five falsifications. The harness rejected the incomplete profile
before independent kernel replay. That historical run is **not a passed Lean
acceptance result**; its receipt remains in Leant under
`dist-newstyle/streaming-acceptance/after-lean-djinn/results.json`.

The corresponding initial Haskell Djinn profile passed its six operations and
23 independent execution assertions, but positive-query attempts rose from
503 to 2,367 and candidate-check errors from 11 to 109. Removing whole-pool
ranking exposed poor discovery order; admitting more plans alone did not
prevent useful carrier work from being diluted among historical cursors.
The accepted three-family rotation and finite carrier normal-form priority
repair that work allocation without requiring a future proof pool.

Later demand-first append/filter diagnostics retained only the first three
errors per query and identified GHC polytype-inference and skolem failures
among those samples. They do not establish graph availability for the failed
candidates or a defect in the source checker. The concrete next step remains
[source-evidence-driven Haskell elaboration](2026-09-06-synthesis-next-priorities.md#1-source-evidence-driven-haskell-elaboration):
capture the failed candidate's own handle and graph availability, then use
exact source evidence for annotations or type applications and independently
compile and behavior-check the exact displayed term.
