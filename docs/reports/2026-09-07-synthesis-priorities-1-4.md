# Implementation of synthesis priorities 1–4

Status: **in progress**. This work implements the first four items in the
[accepted roadmap](2026-09-06-synthesis-next-priorities.md). None of the four
priorities is declared complete by this checkpoint. The earlier streaming
acceptance receipt belongs to its recorded source revisions.

The [current re-triage](2026-09-07-synthesis-retriage.md) schedules the remaining
deliveries. It preserves every completion requirement below while moving the
bounded Lean simplification stage ahead of the larger contextual-evidence work
and making corpus expansion part of each capability's acceptance.

## Completion requirements

1. **Evidence-guided Haskell elaboration.** Capture a bounded sample of failed
   candidates with their observation identity, full requested type, exact
   expression, and graph availability. Use only the same candidate's source
   evidence for internal annotations and type applications. Independently
   compile and execute the exact displayed result. Retain missing-authority,
   escaped-skolem, lexical-scope, and source-identity rejection controls.
2. **Ordinary recursive data.** Support checked one-layer list and tree
   elimination, including `null`, `headOr`, `tailOr`, and shallow tree
   inspection. Then support supplied folds/recursors or structurally decreasing
   calls with source evidence and Lean termination checking. Validate full
   graphs, actual Haskell behavior, and independent Lean replay.
3. **Contextual provider evidence.** Implement exact constrained forwarding and
   dictionary-independent bodies under lexical givens, then methods,
   conditional providers, and superclass evidence. Reject sibling leakage,
   escaped skolems, changed provider identities, and invented authority.
   Independent GHC checking and Lean replay are both required.
4. **Behavioral coverage.** Extend Church behavior to naturals, options/eithers,
   folds, conversions, and all 19 supplied-default cases. Preserve explicit
   default/inhabitance assumptions. Extend Lean's proof attempts with bounded
   simplification, preserving exact candidate checking, deadlines, axiom
   inventories, actual false controls, and inconclusive outcomes.

## Haskell elaboration checkpoint

`Language.Haskell.Synthesis.TypedGenerated.Haskell` renders a selected closed
graph with scoped local forall signatures, typed lambda and case patterns,
application-domain annotations, and exact visible/implicit type selections.
It does not reconstruct a graph or create a checked association. Missing type
scope, an open root, unsupported forall evidence, or a hole produces an explicit
rendering failure. Local helper names avoid collisions with emitted globals and
term binders. GHC remains the independent checker of the complete source type.

Both named Haskell engine paths retry compilation failures using this renderer
when that candidate's graph is available. Successful compatibility expressions
retain their existing rendering. A retry stays in the same observation slot;
its extra compiler invocation is counted separately. The original attempt and
retry also share one 30-second candidate deadline. Accepted alternatives are
stored with their original candidate and displayed exactly as checked, including
after best/all selection. False, runtime-error, timeout, and unavailable-worker
outcomes do not initiate elaboration retries. The first three compilation
failures retain their own observation number, graph status/root, requested type,
original expression/error, proposed elaboration, and final check outcome.

The initial live `reverse` diagnostic passed synthesis and independent GHC
execution. It established that the remaining rejected candidate has no source
graph: source checking rejects a type mismatch involving an opened rigid
variable. It was therefore correctly not retried with annotations. This is
evidence about that occurrence, not every historical compilation failure.
Its saved receipt is `dist-newstyle/priority-haskell-reverse/results.json`.

Compiler/execution fixtures passed nested forall introductions, rank-N
input/output, explicit impredicative instantiation, higher-kinded binders, and
helper-name collisions. A separate fixture requires GHC to reject the erased
let-bound list of polymorphic identities, then compiles the graph-rendered
expression at `[forall a. a -> a]` and executes it at both `Bool` and `Int`.
The shared suite also rejects implicit quantification of an open graph root
and retains its existing escaped-skolem and sibling-scope rejection tests.
This positive compiler repair is a graph-renderer fixture; it does not yet
establish a successful retry through the complete live query path.

The full facade suite passed 100 tests in 10.52 seconds, and the shared
synthesis suite passed 466 tests in 0.11 seconds. The strict build uses
GHC 9.12.4 with `-Werror`. All 98 CLI tests passed in 60.80 seconds after the
shared-deadline change. Logs are retained at
`dist-newstyle/priority-facade-final.log`, `priority-synthesis-tests.log`, and
`priority-cli-final.log`; the final CLI strict build log is
`dist-newstyle/priority-shared-deadline-build.log`.

## Remaining work

- Finish priority 1's positive retry and negative evidence acceptance, then
  synchronize and validate the shared changes in Leant.
- Implement priority 2's case analysis and recursor/decreasing-call stages.
- Implement priority 3's dictionary representation and supported provider uses.
- Implement priority 4's broader corpus and bounded Lean simplification.
- Run the appropriate complete affected suites and live compiler/kernel
  acceptance, update capability documentation, and integrate both repositories.

The goal remains all four priorities; this checkpoint does not replace it with
the currently implemented rendering path.
