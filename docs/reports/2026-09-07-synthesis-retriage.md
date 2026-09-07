# Synthesis re-triage after recursive-data execution and Lean replay

This is the current execution recommendation as of 2026-09-07. It updates the
[September 6 roadmap](2026-09-06-synthesis-next-priorities.md) without changing
the scope or numbering of the active **implement priorities 1–4** goal. The
[implementation register](2026-09-07-synthesis-priorities-1-4.md) remains the
completion checklist. None of those four priorities is complete.

## Evidence that changes the ordering

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
| 1 | Priority 2, case stage; priority 4 acceptance | Close Exference case parity and complete Leant integration | Retain all eight accepted Djinn cases; close Exference `tailOr` and tuple fields, extending checked case evidence beyond same-result zero/step spines. Require exact graphs and Haskell execution. Replay all eight in Djinn, Exference, and Both with false controls, then pass the full configured Leant suite. Preserve positive-only construction bounds. |
| 2 | Priority 4, Lean checker stage | Validate the implemented bounded simplification | Both decision polarities precede both simplification polarities. Require the 16 isolated method controls, 15 live queries across all three engines, exact candidate replay, false/inconclusive outcomes, and the full boundary suite. Record actual proof axioms separately from candidate axioms. |
| 3 | Priority 2, recursor stage; extend priority 4 corpus | Supplied folds and recursors for ordinary recursive programs | Prepared fixtures exercise `map`, `append`, and `length` using generic recursors, with tree operations in Lean. Implement coherent polymorphic instantiation where needed, then require exact source evidence, host behavior, and termination checking. Fixture preparation is not capability acceptance. |
| 4 | Priority 3 | Contextual providers with explicit dictionary evidence | First exact constrained forwarding and dictionary-independent bodies under lexical givens; then methods, conditional providers, and superclasses. This crosses search, graph, and host-projection boundaries and deserves its own staged acceptance. |
| 5 | Priority 4, corpus completion | Close the remaining Church behavioral coverage | Cover naturals, options/eithers, folds, conversions, and all 19 supplied-default cases. Start adding these tests during earlier steps; this row is the final coverage gate, not permission to postpone all behavioral work. |

The first/best CLI fixture is now accepted and should be retained at subsequent
integration checkpoints. The first row remains the principal cross-engine
capability gap; bounded simplification can be validated independently while its
search fix is developed. The schedule separates delivery size from importance. Dictionary support
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

**Use existing evidence infrastructure for contexts.** The current
[`SourceGraph.hs`](../../djinn/src-internal/Djinn/Internal/SourceGraph.hs)
rejects forall consumption requiring dictionary evidence. Reuse the shared
constraint representation and Exference's checked resolution where applicable;
do not create another context-erased shortcut. Carry provider identities and
lexical givens through the graph and the Lean projection. Test sibling-scope
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
| Other search and checker performance | Instrument now; optimize a demonstrated cost | Separate cold startup, first accepted result, search work, rendering/checking, and memory. Compare identical queries and budgets before changing defaults. |
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
