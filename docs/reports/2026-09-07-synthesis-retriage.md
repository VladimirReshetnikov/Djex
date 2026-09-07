# Synthesis re-triage after recursive-data execution and Lean replay

This is the current execution recommendation as of 2026-09-07. It updates the
[September 6 roadmap](2026-09-06-synthesis-next-priorities.md) without changing
the scope or numbering of the active **implement priorities 1–4** goal. The
[implementation register](2026-09-07-synthesis-priorities-1-4.md) remains the
completion checklist. None of those four priorities is complete.

## Current implementation evidence

The ordinary-data Haskell matrix now passes all **16 cells**: eight operations
in each of Djinn and Exference. Every observed candidate must retain its exact
source graph, and the accepted implementations compile and execute under their
full original signatures. Exference now retains complete constructor-case
evidence and uses two bounded queues when multiple independent recursive inputs
require both whole-value forwarding and inspection. Both queues share the
caller's total step and queue allowances. This closes the Haskell failures
described in the historical baseline below; fresh three-engine Lean replay and
the full Leant boundary gate remain open.

The [lexical-evidence increment](2026-09-07-contextual-evidence-design.md) now
passes the complete shared and private checker suites and four Haskell renderer
replay tests. Explicit Given identities preserve their exact introduction and
source slot. These graph/checker results do not by themselves establish search
reachability, method/instance/superclass evidence, or production Lean rendering.
The subsequent native Given matrix establishes five live Exference roles and
three Djinn roles, plus three leakage controls. The report retains its exact
inventory, constraint-pruning policy, original signatures, and GHC replay.
Djinn's constrained local/global search remains separate from those accepted
roles.

The final strict build passes, and the
[complete affected-suite receipt](../../test-integration/receipts/recursive-context-checkpoint.json)
records 2,090/2,092 passing tests. All ten non-Length suites pass in full.
Two existing short-deadline Length process tests fail in the 432-test run and
pass an [unchanged focused retry](../../test-integration/receipts/recursive-context-length-retry.json).
The retry preserves the same executable and internal limits; it is not an
unfiltered full-Length pass. The earlier seven-failure baseline and its separate
retry remain under `dist-newstyle/priority-baseline-suites-v1/` and
`priority-baseline-length-retry-v1/`.

The [generic fold composition increment](2026-09-07-supplied-recursor-composition.md)
now passes `map`, order-sensitive `append`, generalized `length`, and the
empty-input control in both Haskell engines within the original bounds. The
next immediate gate is vendoring this checkpoint for cross-engine Lean
acceptance. Expanded Church acceptance must cover additional total operations
and all nineteen partial cases with explicit supplied defaults. New source and
oracle controls must compile and run before those cells count as accepted
behavior.

## Earlier published baseline and resolved experiments

Djinn's [case-search checkpoint](2026-09-07-recursive-data-cases.md), committed
in `5fcc4a8b` and corrected in `3ce26cfd`, now passes eight Haskell execution
scenarios. Leant's working integration of the latter revision also passes all
eight public behavioral queries and independent Lean replay: sixteen empty
axiom inventories, plus an actually falsified control. This closes the earlier
`null`/`tailOr` experiment for Djinn. It does not establish cross-engine or
complete integration acceptance.

The next gate is **Exference parity and isolation between engine modes**.
At that same dependency revision, Exference misses `tailOr` within 100,000
steps and times out on the unary tuple-payload case. `both` also times out on
that payload case despite the accepted Djinn-only implementation. Search
latency can therefore hide an implementation already reachable in another
engine. This is now a concrete delivery issue, not a speculative performance
project. Fix the exposed search cases first; if `both` still delays an accepted
lane, measure and fix its scheduling separately.

The current Exference experiment makes recursive input splitting optional and
allows finite nonrecursive fields to be inspected while keeping recursive
descendants opaque. Its focused fixture exposed two evidence gaps: unused
binders were not represented as compatibility wildcards, and the checker only
retains a narrow recursive zero/step case returning the scrutinee's own type.
That latter restriction prevents a checked list-to-Bool graph even when the
search term is otherwise valid. Extend complete constructor-case evidence
from the actual declaration inventory, including finite tuple payloads; retain
exhaustiveness, lexical field scope, and exact graph/compatibility association.
A compatibility term alone is not acceptance.
Neither the search experiment nor the failed mixed-engine run is an accepted
capability. The failed run performed no independent kernel replays.

The unused-binder correction is committed separately in `02bfde77`; its strict
build and all 53 Exference engine tests pass. It emits checked wildcard patterns
for unused lambda and let binders without relaxing exact erasure equality.
The broader complete-case graph extension now has passing direct engine tests,
but remains uncommitted and has no complete cross-engine acceptance. The current
ordinary-data matrix still fails on two independently typed recursive inputs.
Search must retain both whole-value forwarding and complete input inspection;
simply changing their order does not overcome the score charged for all case
branches. A protected scheduling experiment is in progress. Its work must share
the existing step, queue, and depth limits. The earlier experimental forwarding
penalty changed depth accounting and is being replaced, not adopted as a default.

Leant's full post-integration suite also needs a clean rerun. The first run
had missing fake-Z3 setup and two genuine positive-constructor-bound failures;
`3ce26cfd` corrects the latter by activating case plans only for recursive
types in negative positions. A passing focused replay does not replace that
full-suite gate. Detailed receipts and settings are recorded in Leant's
companion re-triage and recursive-data acceptance directory.

Djex `6890bb5a8a56902c2baf137581e23c25a376fad0` includes source-graph
Haskell rendering, bounded same-candidate retries, and checked implicit local
type selections. The [implementation register](2026-09-07-synthesis-priorities-1-4.md#implicit-local-evidence-and-live-repair)
records a live Exference repair under `all` selection, exact displayed-source
replay in expression and definition modes, and a false control. Its five
affected suites passed 1,231 tests. The subsequent real-provider fixture now
passes first/best selection in both display modes, with exact displayed-source
GHC replay. All 100 CLI tests pass, including the existing all-selection and
false controls. This closes that selection fixture; the
[register](2026-09-07-synthesis-priorities-1-4.md#firstbest-repaired-candidate-selection)
records its precise limits and the concurrent unaccepted source changes.

The live `reverse` diagnostic's rejected occurrence had **no source graph**.
Its source checker reported a rigid-variable type mismatch. More annotations
cannot be justified for that occurrence. Classify such failures separately
from graph-present elaboration failures; a historical compiler-error count
does not measure the annotation renderer's remaining work.

Leant `aab110e99e3c3d96549a05d3975b26bea93dc6ef` pins that exact Djex
checkpoint. Its recorded integration passed a strict build, 615 boundary
tests, and the existing six-operation corpus under three engine modes
(18 cases), plus three false controls. Independent Lean replay recorded 69
empty axiom inventories. Integration of this checkpoint is done; it is not
an outstanding prerequisite. These are retained receipts inspected for this
re-triage, not new test runs or broader corpus coverage.

The earlier continuation-view experiment failed `null` and `tailOr`; an even
earlier direct-view experiment inspected constructed constants instead of the
input. These are resolved design experiments, retained as evidence that graph
availability alone does not establish useful behavior. They are no longer the
current blocking fixtures.

## Recommended execution order

| Order | Existing goal item | Deliverable | Acceptance and reason for its position |
| --- | --- | --- | --- |
| 1 | Priority 2, case stage; priority 4 acceptance | Complete cross-engine Leant integration | Retain the passing sixteen-cell Haskell matrix. Replay all eight operations in Djinn, Exference, and Both with false controls, then pass the full configured Leant suite. Preserve exact complete-case graphs, shared queue/step bounds, and positive-only construction bounds. |
| 2 | Priority 2, recursor stage; extend priority 4 corpus | Complete Lean acceptance and broaden supplied recursor composition | Retain the accepted Haskell `map`, `append`, and generalized `length` fixture in both engines. Replay supplied folds and tree operations in Lean, including actual termination checks; extend accumulator shapes where concrete programs expose a gap. |
| 3 | Priority 3 | Contextual providers with explicit dictionary evidence | First exact constrained forwarding and dictionary-independent bodies under lexical givens; then methods, conditional providers, and superclasses. This crosses search, graph, and host-projection boundaries and deserves its own staged acceptance. |
| 4 | Priority 4, corpus completion | Close the remaining Church behavioral coverage | Cover naturals, options/eithers, folds, conversions, and all 19 supplied-default cases. Start adding these tests during earlier steps; this row is the final coverage gate, not permission to postpone all behavioral work. |

Leant's bounded simplification milestone is now published in `990b7f3`. Its
[acceptance report](https://github.com/VladimirReshetnikov/Leant/blob/main/docs/reports/2026-09-07-bounded-behavioral-simplification.md)
records 16 isolated method controls, 15 live queries across all three engines,
and independent replay of six displayed results. Every candidate has an empty
axiom inventory; quantified simplification proofs use exactly `propext` and
`Quot.sound`. The full configured boundary run passed 619/620 tests, with one
existing provider-stage timeout; an unchanged focused retry passed. These
separate receipts do not claim an unfiltered 620-test pass.

The first/best CLI fixture is now accepted and should be retained at subsequent
integration checkpoints. The first row remains the principal cross-engine
integration gate. Bounded simplification's live acceptance is complete; retain
it as a regression gate while proceeding to recursors. The schedule separates
delivery size from importance. Dictionary support
remains a required capability; bounded simplification does not replace it.
Nor does accepting a recursor milestone complete recursive-data work without
the documented source-evidence and Lean checking gates.

## Implementation choices to make explicit

**Preserve the accepted recursive-data boundary.** Djinn now uses coherent
opaque datatype views for goals and providers, exact constructor premises,
and checked one-layer elimination. Keep that source authority and the
negative-occurrence activation gate. Exference's next acceptance must show that
a supplied default can remain whole while another recursive argument is
inspected, and that a finite tuple field is accessible without opening another
recursive layer. Include aliases, mutual recursion, multiple inputs, tuple
results, and unchanged positive-only bounds. A bounded case view must retain
its incomplete-search status and cannot authorize a non-inhabitation claim.

**Use existing evidence infrastructure for contexts.** The new
[`SourceGraph.hs`](../../djinn/src-internal/Djinn/Internal/SourceGraph.hs)
and Exference checker reconstruct direct lexical evidence. Extend search and
production projection around these checked identities. Carry provider identities
and lexical givens through the graph and the Lean projection. Test sibling-scope
leakage, escaped skolems, wrong providers, and unavailable dictionary authority.

**Behavior is a continuing acceptance requirement.** Add host execution and
false controls with each new program family. Keep total and supplied-default
cases separate. Finite observations establish only those observations;
universal claims require a kernel-checked proof of the stated proposition.
An unsuccessful proof attempt remains inconclusive.

## Other ideas, re-ranked

| Idea | Disposition | Concrete trigger for promotion |
| --- | --- | --- |
| Native Windows Length acquisition | Separate platform milestone after the capability deliveries, or sooner for a concrete Windows Length user task | Implement the complete bounded acquisition, solver execution, and independent replay route; configuration parsing alone is insufficient. |
| Cross-engine progress and cancellation | Promote the observed Both delay into the case-stage acceptance | Compare the same tuple-payload query in Djinn, Exference, and Both. An expensive lane must not hide an accepted result past the command deadline. First close the Exference search regression; introduce broader scheduling changes only if the delay persists. |
| Other search and checker performance | Instrument now; optimize a demonstrated cost | The Exference supplied-fold `[a] -> a` rejection fixture takes about 147 seconds at 100,000 steps; this is a concrete exhaustion-cost benchmark. Separate cold startup, first accepted result, search work, rendering/checking, and memory before changing defaults. The interpreted/default-deferral contextual failures and native/immediate-pruning successes changed two variables, so they are not a controlled performance comparison. |
| Failure diagnostics and capability receipts | Include in current deliveries | Keep graph absence, compiler rejection, behavioral falsehood, inconclusive checking, and exhausted search distinct. Retain per-engine results, actual replay status, source revision, settings, exact emitted source, and negative controls; a failed matrix must not obscure the accepted subset or imply unperformed kernel checks. |
| Semantic provider retrieval | Retain as the next scaling investigation | Show a useful provider excluded by inventory selection in a realistic project; measure retrieval recall as well as latency. Larger inventories alone are not an acceptance criterion. |
| Canonical duplicate keys and a shared subgoal DAG | Defer broad refactoring; allow a measured local optimization | A profile must identify duplicate comparison or repeated subgoal work as material. Preserve source identity, scope, budget charging, and replay. |
| Isolated-worker production routing | Separate integration project | Require environment snapshots, backend parity, transcript equality, bounded cancellation, and memory measurements. The existing foundation is not proof of a speedup. |
| Native Lean tactic integration | Separate product milestone | An editor or proof-mode workflow needing this entrance should define the acceptance fixture. It does not close the current synthesis capability gaps by itself. |
| Dependent/indexed refinement and residual-hole search | Defer until the ordinary-data and contextual milestones settle | Start from a concrete missing indexed program and explicit equality/transport obligations, rather than a general dependent-synthesis rewrite. |
| Arbitrary frontier widening, persistent caches, cooperative internal search, equality saturation, induction/invariant discovery | Defer | Require a missing program or measured bottleneck that the smaller accepted extensions cannot address. |

The older architectural proposals remain sources of design ideas. Their
statements about missing typed graphs and candidate evidence must be checked
against the implemented graph foundation before turning them into new work.
