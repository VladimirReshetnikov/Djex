# Synthesis re-triage after integration and recursive-case experiments

This is the current execution recommendation as of 2026-09-07. It updates the
[September 6 roadmap](2026-09-06-synthesis-next-priorities.md) without changing
the scope or numbering of the active **implement priorities 1–4** goal. The
[implementation register](2026-09-07-synthesis-priorities-1-4.md) remains the
completion checklist. None of those four priorities is complete.

## Evidence that changes the ordering

Subsequent implementation: the [case-search report](2026-09-07-recursive-data-cases.md)
records eight passing Haskell execution scenarios and the affected regression
suites. The failing experiment described below is the historical evidence for
this re-triage. The next case-stage gate is now Leant integration and kernel
replay, followed by the remaining elaboration selection acceptance.

Djex `6890bb5a8a56902c2baf137581e23c25a376fad0` includes source-graph
Haskell rendering, bounded same-candidate retries, and checked implicit local
type selections. The [implementation register](2026-09-07-synthesis-priorities-1-4.md#implicit-local-evidence-and-live-repair)
records a live Exference repair under `all` selection, exact displayed-source
replay in expression and definition modes, and a false control. Its five
affected suites passed 1,231 tests. First/best selection of a repaired
candidate remains an acceptance gap; the renderer itself is implemented.

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

The uncommitted recursive-case experiment has a strict focused build but
fails its combined public-search/GHC-execution fixture. The latest log,
`dist-newstyle/priority-recursive-cps-behavior.log`, reports `headOr=True` and
`tree=True`, but `null=False` and `tailOr=False`. An earlier direct-view
experiment could produce checked case graphs while inspecting constructed
constants instead of the input. Thus graph availability alone missed a real
behavioral gap. The current experiment has neither complete affected-suite
acceptance nor Lean integration and must not be published as a capability.

## Recommended execution order

| Order | Existing goal item | Deliverable | Acceptance and reason for its position |
| --- | --- | --- | --- |
| 1 | Priority 2, case stage; start priority 4 corpus work | Close the failing one-layer list and tree experiment | Make all four execution targets pass, then validate source evidence, affected suites, and Lean replay. Fix the exposed input/result search relationship before broadening recursive search. |
| 2 | Priority 1 | Close the remaining elaboration acceptance | Require live first/best selection of a repaired candidate, exact checked/displayed text, rejection controls, and the shared deadline. Retain the accepted all-selection fixture and integrate any resulting changes into Leant. |
| 3 | Priority 4, Lean checker stage | Bounded simplification after decision attempts | Check the proposition and its negation by decision first, then try bounded simplification. Accept only completed kernel proofs, preserve deadlines and axiom inventories, and retain false and inconclusive controls. This is a smaller independent delivery than class-evidence representation. |
| 4 | Priority 2, recursor stage; extend priority 4 corpus | Supplied folds and recursors for ordinary recursive programs | Exercise `map`, `append`, and `length` as acceptance targets using generic recursors, not those target implementations as providers. Reuse checked recursion structure before adding synthesis of recursive definitions. Require independent host checking and termination evidence appropriate to the construction. |
| 5 | Priority 3 | Contextual providers with explicit dictionary evidence | First exact constrained forwarding and dictionary-independent bodies under lexical givens; then methods, conditional providers, and superclasses. This crosses search, graph, and host-projection boundaries and deserves its own staged acceptance. |
| 6 | Priority 4, corpus completion | Close the remaining Church behavioral coverage | Cover naturals, options/eithers, folds, conversions, and all 19 supplied-default cases. Start adding these tests during earlier steps; this row is the final coverage gate, not permission to postpone all behavioral work. |

The first row moves ahead of the small priority-1 acceptance closure because
it is active work with a concrete failing fixture. This is a bounded case
milestone: do not let general recursion design absorb the remaining selection
checks. The schedule separates delivery size from importance. Dictionary support
remains a required capability; bounded simplification does not replace it.
Nor does accepting a recursor milestone complete recursive-data work without
the documented source-evidence and Lean checking gates.

## Implementation choices to make explicit

**Recursive elimination needs a coherent view of both input and result.**
The committed `recursiveDataCanUnfold` in
[`TypeFormula.hs`](../../djinn/src-core/Djinn/Internal/TypeFormula.hs) refuses
negative recursive unfolding. For `tailOr`, exposing the input constructors
is insufficient if the retained opaque tail cannot inhabit the selected
result view. The continuation-view experiment succeeds for atomic results in
the current fixture but fails for the structural sum results of `null` and
`tailOr`. LJT introduces a fresh continuation atom when proving a sum; a
result view chosen before that step is a plausible cause, not yet a validated
diagnosis. The next design to test is a coherent opaque datatype view for
goals and providers, exact constructor premises, and checked one-layer
elimination. Do not mix differently lowered provider and result types or
substitute larger search budgets for this regression. Include aliases, mutual
recursion, multiple inputs, and tuple results in boundary checks. A bounded case view must retain its
incomplete-search status and must not authorize a non-inhabitation claim.

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
| Search and checker performance | Instrument now; optimize a demonstrated cost | Separate cold startup, first accepted result, search work, rendering/checking, and memory. Compare identical queries and budgets before changing defaults. |
| Failure diagnostics and capability receipts | Include in current deliveries | Keep graph absence, compiler rejection, behavioral falsehood, inconclusive checking, and exhausted search distinct. Each accepted family needs source revision, settings, exact emitted-source replay, and negative controls; a typed case graph does not establish useful elimination. |
| Semantic provider retrieval | Retain as the next scaling investigation | Show a useful provider excluded by inventory selection in a realistic project; measure retrieval recall as well as latency. Larger inventories alone are not an acceptance criterion. |
| Canonical duplicate keys and a shared subgoal DAG | Defer broad refactoring; allow a measured local optimization | A profile must identify duplicate comparison or repeated subgoal work as material. Preserve source identity, scope, budget charging, and replay. |
| Isolated-worker production routing | Separate integration project | Require environment snapshots, backend parity, transcript equality, bounded cancellation, and memory measurements. The existing foundation is not proof of a speedup. |
| Native Lean tactic integration | Separate product milestone | An editor or proof-mode workflow needing this entrance should define the acceptance fixture. It does not close the current synthesis capability gaps by itself. |
| Dependent/indexed refinement and residual-hole search | Defer until the ordinary-data and contextual milestones settle | Start from a concrete missing indexed program and explicit equality/transport obligations, rather than a general dependent-synthesis rewrite. |
| Arbitrary frontier widening, persistent caches, cooperative internal search, equality saturation, induction/invariant discovery | Defer | Require a missing program or measured bottleneck that the smaller accepted extensions cannot address. |

The older architectural proposals remain sources of design ideas. Their
statements about missing typed graphs and candidate evidence must be checked
against the implemented graph foundation before turning them into new work.
