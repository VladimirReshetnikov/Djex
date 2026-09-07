# Next synthesis priorities in Djex and Leant

The [September 7 re-triage](2026-09-07-synthesis-retriage.md) updates the
execution order and implemented status below. This dated roadmap retains the
priority numbers used by the active implementation goal; its descriptions of
work still to begin reflect the September 6 checkpoint.

This re-triage separates accepted capabilities and proposed follow-ups.
It is a roadmap, not a claim that all synthesis goals or
behavioral laws have been proved.

## Current boundary

Rank-N and impredicative synthesis have the documented
[typing and search coverage](../rank-n.md), including the Church signature
corpus. The [Djinn source-typed graph milestone](../source-typed-evidence-graph.md)
and the earlier [six-operation behavioral corpus](../behavioral-synthesis.md)
have been accepted within their recorded scopes. A type-correct implementation
does not establish the operation suggested by its name; finite observations
do not establish a universal law.

**Djinn behavioral streaming has passed its recorded acceptance gates.** The
[streaming report](2026-09-06-djinn-behavioral-streaming.md) records the complete
paired 30-cell behavioral matrix, 943 affected unit tests, source ownership and
resource-bound checks, and live quota, inconclusive, timeout-retention, and
recovery controls. Every accepted corpus spelling matched the frozen baseline.
The measurements improve Djinn's median visibility in both hosts but do not
establish a general speedup across engines or remove cold Lean startup costs.

## Recommended order

### 1. Source-evidence-driven Haskell elaboration

Address the remaining gap between retained source
typing and Haskell output. The
[behavioral renderer](../../src/Language/Haskell/Djex/REPL.hs#L769) receives typed
candidates but renders their compatibility clauses. The
[worker](../../src/Language/Haskell/Djex/REPL/BehavioralWorker.hs#L170) supplies the
outer signature; the graph's implicit type selections and forall scopes do not
currently guide internal annotations or explicit applications.

The demand-first append/filter captures contain `BehavioralCompilationError`
examples involving polytype inference and skolem mismatch. Only the first
three error messages per query were retained, so these samples establish
neither the classification of every error nor graph availability for each
failed candidate. They do not show that the source checker accepted an invalid
term.

First retain a bounded sample of each failed candidate's own handle, requested
type, exact rendered expression, and graph availability without regenerating
the candidate. Distinguish source-graph absence from graph-present output that
GHC cannot elaborate. Add internal signatures or visible type applications
only when that same candidate's exact source evidence supplies the types and
scopes. Acceptance must compile the resulting rank-N/impredicative term at its
original full signature, execute its behavioral predicate, and display exactly
the implementation that was checked. Preserve rejection of escaped skolems
and unavailable authority. This is a focused supported-fragment improvement,
not a promise that every sampled failure is repairable.

### 2. Ordinary recursive data: one case split, then folds

The next capability priority is ordinary lists and trees. Djinn currently
supports bounded positive construction and forwarding, but does not synthesize
recursive input elimination, recursive calls, or induction
([current rule](../rank-n.md#declared-datatypes-structural-and-nominal-views)).
Leant already distinguishes Djinn's positive construction from Exference's
one-layer elimination in
[`ExactFamilyPlan`](https://github.com/VladimirReshetnikov/Leant/blob/main/src/Leant/Synth/Engine.hs#L3346).

First admit one checked case split while retaining recursive fields opaquely.
Accept ordinary `null`, `headOr`, `tailOr`, and one-layer tree inspection with
exact constructor identities, full source graphs, Haskell execution, and Lean
kernel replay. The existing private recursive List source-checker/Length
fixture is a foundation, not evidence that public search already does this.

Then add supplied folds/recursors or structurally decreasing calls as a
separate milestone. Require a checked decreasing-subterm or recursor witness
and Lean termination evidence. This staged extension has more immediate value
than increasing another occurrence-plan bound, while avoiding an open-ended
general-recursion search project.

### 3. Contextual providers and dictionary evidence

Djinn's graph checker still rejects source schemes requiring dictionaries
([source boundary](../../djinn/src-internal/Djinn/Internal/SourceGraph.hs#L283)).
Leant's Djinn provider projection also explicitly erases contexts, whereas
Exference retains them
([projection policy](https://github.com/VladimirReshetnikov/Leant/blob/main/src/Leant/Synth/Engine.hs#L3365)).

Begin with exact constrained forwarding and dictionary-independent bodies
under retained lexical givens. Next support methods, conditional providers,
and superclass evidence, keeping each obligation attached to its source scope.
Acceptance must reject sibling-scope leakage, escaped skolems, changed provider
identities, and invented dictionary authority. Independent GHC checking and
Lean replay remain separate gates. General contextual subsumption should
follow this representation work, rather than precede it.

### 4. Broader behavioral specifications and coverage

Extend the accepted six total Church operations to naturals, options/eithers,
folds, conversions, and the 19 supplied-default cases. Keep the agreed explicit
default/inhabitance assumptions for partial operations. Add independent finite
Haskell observations and false controls; do not load reference implementations
as synthesis providers.

Leant currently checks behavioral assertions and their negations using
[`by decide`](https://github.com/VladimirReshetnikov/Leant/blob/main/src/Leant/Synth/Behavioral.hs#L53),
through the exact candidate in
[`assessVariant`](https://github.com/VladimirReshetnikov/Leant/blob/main/src/Main.hs#L4142).
A useful first extension is bounded simplification after decision attempts,
covering universal laws that reduction and admitted simplification lemmas can
prove. Retain the exact emitted candidate, normal heartbeat/deadline bounds,
kernel checking, and axiom inventories. Failure to prove the assertion or its
negation stays inconclusive. Keep ordinary finite predicates as controls; do
not claim general induction or theorem discovery from this extension.

### 5. Native Windows Length support as an independent platform task

Leant's public Length file acquisition deliberately returns
`LengthFilePlatformUnsupported` on Windows
([implementation](https://github.com/VladimirReshetnikov/Leant/blob/main/src/Leant/Synth/Length/File/Acquire.hs#L176)).
This remains useful independent work for the native development platform.

Implement bounded Windows file acquisition with the existing identity,
deadline, decoding, and cleanup contract. Then validate the complete configured
solver launch, Length query, and independent replay route on Windows. Reading
the configuration successfully is only the first gate; it does not establish
successful solver-backed filtering. Keep this task separate from recursive
search and rank-N typing changes.

### 6. Optimize the measured streaming costs

Use the current acceptance measurements to select the next bottleneck before
changing defaults. Compare first accepted result latency, observed candidates,
search work, checking/rendering cost, and retained memory under identical
queries and settings. The final Haskell Djinn profile after the scheduler repair
passed the same six operations with 466 predicate attempts and one candidate-check
error, versus 503 attempts and 11 errors in the frozen baseline. Its median
first-visible time fell from 54.581 to 1.978 seconds, although `reverse` needed
more observations (78 to 242). These single-run measurements support the repair
but do not identify a remaining dominant cost. Lean Djinn's median query-relative
visibility also improved, while its first cold result stayed near 94 seconds
from process entry. Lean Exference and Both had higher medians in their control
runs. Separate initialization, search, rendering, and checking costs before
attributing those differences to the producer. The earlier bounded error
samples motivate priority 1 without establishing that every error has the same
cause or source-graph status.
Potential targets include the stream's
[linear alpha-equivalence scan](../../djinn/src-core/Djinn/Core.hs#L2841), repeated
plan preparation, and kind inference or sealing outside the source checker's
charged-step counter. None is yet a demonstrated dominant cost.

Accept an optimization only with unchanged evidence ownership and boundedness,
independent replay, and measured improvement on the affected cases. Smaller
default search windows require their own coverage evidence.

## Lower priority

Defer arbitrary frontier widening, general impredicative type enumeration,
persistent result caches, and internal parallel search until a concrete missing
program or measured bottleneck justifies them. Preserve the distinction between
unsupported search and proved non-inhabitation.

Leant already has an isolated-worker foundation, but its
[documented next stage](https://github.com/VladimirReshetnikov/Leant/blob/main/docs/synth-internals.md#L1685)
still requires command-environment snapshots, production routing, real-backend
parity, deadlines, memory measurements, and transcript equality. That is a
separate integration project, not a completed speed-up or the next prerequisite
for the capability work above.
