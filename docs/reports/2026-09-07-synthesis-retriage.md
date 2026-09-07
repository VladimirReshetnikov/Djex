# Synthesis re-triage after the Haskell elaboration checkpoint

This is the current execution recommendation as of 2026-09-07. It updates the
[September 6 roadmap](2026-09-06-synthesis-next-priorities.md) without changing
the scope or numbering of the active **implement priorities 1–4** goal. The
[implementation register](2026-09-07-synthesis-priorities-1-4.md) remains the
completion checklist. None of those four priorities is complete.

## Evidence that changes the ordering

The [implementation register](2026-09-07-synthesis-priorities-1-4.md#implicit-local-evidence-and-live-repair)
now records a subsequent live Exference repair under `all` selection, including
exact displayed-source replay and a false control. The revision-specific
baseline below explains the original ordering; consult that register for the
new acceptance evidence and remaining work.

Djex `0a79d2311d966f7689c52a33c211f49f3a15bbea` implements source-graph
Haskell rendering and a compilation-failure retry in both named behavioral
engine paths. The retained logs record 100 facade, 466 shared-synthesis, and
98 CLI tests passing. These are checkpoint results, not a fresh test run for
this documentation change. The compiler fixture repairs an impredicative
let that GHC rejects before annotation, but successful repair through a live
public synthesis query is still an acceptance gap.

The live `reverse` diagnostic's rejected occurrence had **no source graph**.
Its source checker reported a rigid-variable type mismatch. More annotations
cannot be justified for that occurrence. Classify such failures separately
from graph-present elaboration failures; a historical compiler-error count
does not measure the annotation renderer's remaining work.

Leant `769369d4f85f07341bc8d444234a18e330d0edbf` still pins Djex
`7ea70a8e594762872d6511ecd0891294bd053022`. The new renderer is therefore a
Djex checkpoint awaiting Leant integration and validation. The earlier
[streaming acceptance](2026-09-06-djinn-behavioral-streaming.md) remains useful
baseline evidence at its recorded revisions.

## Recommended execution order

| Order | Existing goal item | Deliverable | Acceptance and reason for its position |
| --- | --- | --- | --- |
| 1 | Priority 1 | Finish elaboration acceptance and integrate the shared change into Leant | Demonstrate a graph-present repair through the real query/worker/display path; check first/best/all retain the exact accepted term, rejection controls, the shared deadline, and affected host tests. Finish the existing implementation before expanding it. |
| 2 | Priority 2, case stage; start priority 4 corpus work | Checked one-layer list and tree elimination | Synthesize `null`, `headOr`, `tailOr`, and shallow tree inspection through public search, with exact constructor evidence, GHC execution, and Lean replay. These add useful programs beyond Church encodings. |
| 3 | Priority 4, Lean checker stage | Bounded simplification after decision attempts | Check the proposition and its negation by decision first, then try bounded simplification. Accept only completed kernel proofs, preserve deadlines and axiom inventories, and retain false and inconclusive controls. This is a smaller independent delivery than class-evidence representation. |
| 4 | Priority 2, recursor stage; extend priority 4 corpus | Supplied folds and recursors for ordinary recursive programs | Exercise `map`, `append`, and `length` as acceptance targets using generic recursors, not those target implementations as providers. Reuse checked recursion structure before adding synthesis of recursive definitions. Require independent host checking and termination evidence appropriate to the construction. |
| 5 | Priority 3 | Contextual providers with explicit dictionary evidence | First exact constrained forwarding and dictionary-independent bodies under lexical givens; then methods, conditional providers, and superclasses. This crosses search, graph, and host-projection boundaries and deserves its own staged acceptance. |
| 6 | Priority 4, corpus completion | Close the remaining Church behavioral coverage | Cover naturals, options/eithers, folds, conversions, and all 19 supplied-default cases. Start adding these tests during earlier steps; this row is the final coverage gate, not permission to postpone all behavioral work. |

The schedule separates delivery size from importance. Dictionary support
remains a required capability; bounded simplification does not replace it.
Nor does accepting a recursor milestone complete recursive-data work without
the documented source-evidence and Lean checking gates.

## Implementation choices to make explicit

**Recursive elimination needs a coherent view of both input and result.**
The current `recursiveDataCanUnfold` in
[`TypeFormula.hs`](../../djinn/src-core/Djinn/Internal/TypeFormula.hs) refuses
negative recursive unfolding. For `tailOr`, exposing the input constructors
is insufficient if the retained opaque tail cannot inhabit the selected
result view. Design and test that relationship, including aliases and mutual
recursion, before widening unfolding. A bounded case view must retain its
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
| Semantic provider retrieval | Retain as the next scaling investigation | Show a useful provider excluded by inventory selection in a realistic project; measure retrieval recall as well as latency. Larger inventories alone are not an acceptance criterion. |
| Canonical duplicate keys and a shared subgoal DAG | Defer broad refactoring; allow a measured local optimization | A profile must identify duplicate comparison or repeated subgoal work as material. Preserve source identity, scope, budget charging, and replay. |
| Isolated-worker production routing | Separate integration project | Require environment snapshots, backend parity, transcript equality, bounded cancellation, and memory measurements. The existing foundation is not proof of a speedup. |
| Native Lean tactic integration | Separate product milestone | An editor or proof-mode workflow needing this entrance should define the acceptance fixture. It does not close the current synthesis capability gaps by itself. |
| Dependent/indexed refinement and residual-hole search | Defer until the ordinary-data and contextual milestones settle | Start from a concrete missing indexed program and explicit equality/transport obligations, rather than a general dependent-synthesis rewrite. |
| Arbitrary frontier widening, persistent caches, cooperative internal search, equality saturation, induction/invariant discovery | Defer | Require a missing program or measured bottleneck that the smaller accepted extensions cannot address. |

The older architectural proposals remain sources of design ideas. Their
statements about missing typed graphs and candidate evidence must be checked
against the implemented graph foundation before turning them into new work.
