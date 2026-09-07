# Incremental Djinn behavioral synthesis

Status: implementation under validation. This report does not yet claim
completion or publication.

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

## Implemented boundary

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

Djex's named behavioral path retains the typed candidate handle while rendering
its compatibility projection for GHC. Leant maps each observation through its
existing exact source renderer and checks a group before resuming the stream.
Each frontend retains its own observation window and behavior-selection rules.

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
modules. Other engine profiles and the current Lean executable remain pending.

## Validation in progress

The strict Djex executable, core, facade, and CLI test build passed with GHC
9.12.4 and `-Werror`. The public stream facade tests passed. The core matrix
checks both strategies, three raw cutoffs and five choice allowances against
the exact consumed proof-cursor prefix. A test-only first-delivery boundary
check measures the actual cost of reaching its first proof. The complete core
suite passed 128 tests in 126.91 seconds, including all six streaming tests;
the complete shared-facade suite passed 98 tests in 12.19 seconds.

At a raw cutoff, streaming reports that cutoff without looking beyond it.
The historical depth-first batch path probes the overflow tail and can instead
encounter choice exhaustion there. The stream deliberately avoids that extra
work; candidate-set parity with the bounded batch remains independently
checked. Neither incomplete result provides negative evidence.

The observer and comparison Python suites passed 17 Djex observer tests,
9 Lean observer tests, and 5 paired-receipt tests. Harness checks are separate
from executable acceptance.

The current Haskell executable passed all six Djinn behavioral operations,
the real false-predicate control, and all 23 independent execution assertions.
Its receipt is `dist-newstyle/streaming-acceptance/after-haskell-djinn/results.json`,
SHA-256 `1b22717a311d9fc0910d0a028323e923b56803f360fff12d1e21f9e6b1ab150b`.
All exact queries, limits, corpus sources, and oracle/harness hashes match the
frozen baseline. The executable SHA-256 is
`a4e013f6ed5bfee8cf90a323965b2aab313e95cab107674f49b9dfc1a6a2043a`.

| Operation | Baseline first visible (s) | Streaming first visible (s) |
| --- | ---: | ---: |
| `not` | 69.599 | 1.335 |
| `swap` | 30.825 | 1.039 |
| `map` | 39.562 | 1.073 |
| `append` | 114.598 | 25.634 |
| `reverse` | 77.990 | 14.136 |
| `filter` | 20.622 | 23.484 |

These single-run visibility measurements include startup and do not establish
a statistical performance guarantee. Positive-query predicate attempts rose
from 503 to 2,367, including an increase from 11 to 109 oracle errors; all six
accepted results still passed their independent checks, and neither run had a
predicate timeout. Predicate attempts do not measure raw proof or choice work.
Discovery order changed the accepted spelling of `not`; the other five exact
definitions and replay modules were unchanged.

Full affected-suite, current Lean runtime, complete paired matrix, and repository
integration remain open. No publication result is claimed for this change.
