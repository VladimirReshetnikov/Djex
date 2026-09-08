# Synthesis re-triage: finish bounded deliveries, then extend evidence

This 2026-09-07 update changes the execution order, not the scope or numbering
of the active **implement priorities 1–4** goal. The
[original roadmap](2026-09-06-synthesis-next-priorities.md) and
[completion register](2026-09-07-synthesis-priorities-1-4.md) retain that scope.

## Current decision and next delivery gates

**Bounded local Lean contexts are accepted: all 39 public cells and the complete
680-test suite pass.** The validated dependency is Djex
`38435709bf4e70c4b53c541c462b0bbd35837bf2`. This completes the bounded integration
milestone within original priority 3; that priority remains open for richer
contextual evidence. Original priority 1's supported-fragment Haskell elaboration
is complete. Priorities 2–4 retain their original requirements.

| Order | Delivery | Required acceptance |
| --- | --- | --- |
| 1 | Make native verification repeatable and correlate requests with candidates | Use the recorded append/length workloads and unchanged bounds. Correlate each exact candidate, verification stage and backend request through recovery; distinguish kernel work, scheduling, transport waits and deadline handling before selecting a fix. Require positive/False controls, exact full-type replay, cancellation, owned cleanup and recovery of a context-dependent command. No transport fix follows from timing alone. |
| 2 | Execute broader Church behavior independently | Run additional totals and all 19 explicit-default counterparts in both Haskell engines and all three Lean modes. Require controlled providers, actual False controls and independent exact full-signature replay. Record success, timeout and inconclusive outcomes per cell. These cells need not wait for native fold repair. |
| 3 | Admit one described global contextual provider | Extend the existing `Type 0` route with one fully described global provider while retaining the current non-overlap guard. Preserve complete source metadata, exact owned graph/renderer evidence and payload-sensitive replay; do not broaden instances or universes as part of this increment. |
| 4 | Preserve selected dictionary identity before relaxing overlap guards | Carry the selected introduction occurrence and ordered slot through lowering and reconstruction. Require a forced equal-predicate outer/inner fixture whose faithful scoped capture preserves distinct payload behavior in Haskell and Lean replay. Graph IDs or successful compilation alone are insufficient. Methods, superclass evidence and further provider schemes remain separate extensions. |

These are delivery steps, not replacement priority numbers: step 1 advances
priority 2, step 2 advances priority 4, and steps 3–4 advance priority 3.
Independent cells may proceed while heavy runtime work remains serialized.
Tree folds and accumulator programs depend on reliable native verification,
not on completing dictionary selection.

## Current evidence and its limits

The [contextual acceptance receipt](https://github.com/VladimirReshetnikov/Leant/blob/469a822da0abfdc7a4d8228a203af0a0b7d2b59b/test-context/receipts/ordinary-context-streaming.json) records a **fresh complete 39/39
public matrix**: 18 exact displayed outputs with full-type replay, 72 finite
payload observations and 144 empty replay inventories, three actual False
controls, and 18 explicit universe/global-metadata refusals. Ordinary and named-`where` commands pass in Djinn, Exference and Both.
Each accepted variant retains its own source graph, renderer alternative and
engine. The directly invoked kernel, source, executable, commands and replay
inputs remained unchanged during the run. The 14 pure production-runner controls
also pass.

The strict build and 54 focused tests pass. The corrected unfiltered unit suite
passes **680/680 in 293.73 seconds**, with an owned process time of 293.86 seconds,
matching its complete unique inventory and preserving source/executable identity.
Ordinary contextual collection now verifies bounded groups before demanding a
complete pool; all four formerly timed-out cells pass in the fresh matrix.
The linked receipt preserves the earlier 35/39 composite, universe-fixture
correction, 678-test checkpoint and subsequent three stale source-assertion
failures. Those historical failures do not replace the fresh complete results.

The accepted route is a local `Type 0` subset. It refuses unrecorded universe
arguments, selected polytypes without complete metadata, and global/caller
premises without source packets. The current provider map is empty and rejects
typed globals. Equal active dictionaries remain unsupported. Rank-N constrained
forwarding is covered; richer nested forall/Given combinations outside the
accepted bounded forms remain open. This acceptance does not establish methods, superclass
search, global contextual providers, arbitrary universes or complete recursion.

The [native request comparison](https://github.com/VladimirReshetnikov/Leant/blob/469a822da0abfdc7a4d8228a203af0a0b7d2b59b/test-recursive/receipts/native-request-traces.json) records a passing length query
(17.744 seconds) with exact kernel replay and a passing False control (14
falsifications, zero behavioral inconclusive checks). That False control passed
its behavioral gate despite two transport/type-verification timeouts; these
are different outcome counts. Append's positive run
failed with ten falsifications and two inconclusive checks; its separate False
control also failed with eleven falsifications and two inconclusive checks.
All four traces retain their owning process and report zero dropped events.
Writes, queue operations and parsing take milliseconds, but the longer response
waits do not identify a transport defect. Candidate 4's negative-decision check
is associated with the first timeout only by sequential inference: trace v1
records no request roles or payload tags. Its replay records the actual launcher
command, not a resolved kernel-executable hash. Repeatable native acceptance
and precise request correlation remain open.

Both partial Church oracle preflights pass: 20 Lean files with 491 exact
inventories (487 empty and four named observer/proof allowances), and 63 Haskell
controls. All 19 explicit defaults and 736 observations remain represented.
Supplied-default `head` passes live synthesis, exact GHC execution and False
controls in both Haskell engines. Most expanded live cells remain open;
oracle preflight and the earlier 350-signature inhabitation corpus do not
establish behavioral synthesis. The exact named axiom allowances remain in the corpus receipts.

Original priority 1 is complete within its supported fragment. The
[completion register](2026-09-07-synthesis-priorities-1-4.md#haskell-elaboration-checkpoint) and [timeout-sample receipt](../../test-integration/receipts/elaboration-timeout-samples.json) record the
strict build and complete 101-test CLI suite: first/best/all repairs, exact GHC
replay and retention of the original failed candidate across a same-deadline
retry timeout. These remain regression gates, not a claim that every compiler
error is repairable.

## Accepted baselines and diagnostic history

Published Djex `922c55580eadec156ba9ef447b300f43e0953ed7` establishes the
Haskell case, supplied-fold, and lexical-Given baselines below. Leant's current
integration now also passes **24 ordinary-data positive cells** across Djinn,
Exference, and Both, three false controls, and 24 independent replays of the
exact displayed source at its full type. All **48 axiom inventories are empty**.
The [Leant receipt](https://github.com/VladimirReshetnikov/Leant/blob/main/test-recursive/receipts/all-engines.json)
records 180.78 seconds for the live phase and unchanged source/executable hashes.
Its strict build and eighteen focused streaming tests pass. The
[full configured Leant integration run](https://github.com/VladimirReshetnikov/Leant/blob/main/test-recursive/receipts/unit.json)
passes **all 647 tests in 343.79 seconds**, with matching unfiltered inventory
and summary counts and unchanged source, test-executable, and fake-Z3 helper
hashes. The aggregate integration gate is complete.

The ordinary-data Haskell matrix passes all **16 cells**: eight operations
in each of Djinn and Exference. Every observed candidate must retain its exact
source graph, and the accepted implementations compile and execute under their
full original signatures. Exference now retains complete constructor-case
evidence and uses two bounded queues when multiple independent recursive inputs
require both whole-value forwarding and inspection. Both queues share the
caller's total step and queue allowances. The Haskell and live Lean matrices
close the case failures described in the historical baseline below. The full
Leant boundary suite also passes, with its own separate receipt.

The [lexical-evidence increment](2026-09-07-contextual-evidence-design.md) now
passes the complete shared and private checker suites and four Haskell renderer
replay tests. Explicit Given identities preserve their exact introduction and
source slot. These graph/checker results do not by themselves establish search
reachability, method/instance/superclass evidence, or production Lean rendering.
The subsequent native Given matrix establishes five live Exference roles and
three Djinn roles, plus three leakage controls. The report retains its exact
inventory, constraint-pruning policy, original signatures, and GHC replay.
Djinn's constrained local/global search remains outside that published matrix.
The [conditional-Given increment](2026-09-07-djinn-conditional-givens.md) adds
forced Djinn local/global use under exact root dictionaries, checked erasure,
and shared batch/stream budgets. All 92 private tests pass, including 26
conditional-kind/proof controls and eleven direct-erasure controls. The expanded
sixteen-test Given target passes in 23.56 seconds, including GHC replay,
sibling-scope rejection, root/nested omitted-method evidence, and preservation
of unconstrained refutations. Ten budget tests cover zero/tiny allowances,
unchanged windows, exact source ownership, and an observed duplicate cutoff.
The increment remains absent from Leant's `922c5558` dependency; arbitrary
nested uses, duplicate equal Givens, methods, and partial constrained
instantiation remain separate work.

The conditional-Given strict builds pass with `-Werror -j1`. Its
[current aggregate receipt](../../test-integration/receipts/conditional-givens-checkpoint.json)
records **2,168 passing tests across twelve complete affected suites**:
502 shared, 94 certificate, 10 graph-fingerprint, 133 Djinn unit, 59 public and
92 private Djinn graph, 513 Exference, 70 private Exference, 139 facade,
100 Djex CLI, 24 historical Djinn CLI, and 432 Length tests. This is a
complementary set of complete suites across two runs, not one unfiltered
twelve-suite invocation. Only three test files changed between the runs to
correct stale expectations of unsound method-based refutations; production
source stayed identical. The complete corrected Djinn unit, facade, and
historical CLI suites pass. The first run's full Length suite passes in
60.39 seconds. Receipts preserve both the initial failures and the corrected
complete reruns, exact commands, source hashes, and executable hashes.

The earlier `922c5558` strict build passed, and its
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
next capability gate is cross-engine Lean acceptance of these fold compositions,
including generic provider termination and actual provider inventories.
Leant's fresh native Djinn fold run, after correcting its settings validator,
accepts `map` through live synthesis and independent replay. Append exhausts
the raw window and generalized length reaches the command deadline, although
their correct terms already occur in the debug stream. Independent Lean replay
accepts those exact terms, original predicates, and wrong-result controls with
eleven empty axiom inventories. Their live failures are at the backend-request
boundary, so request timing and recovery now precede larger search windows or
corpus expansion. The false control records eleven falsifications and one
inconclusive check; the strict matrix remains incomplete. See the
[live receipt](https://github.com/VladimirReshetnikov/Leant/blob/main/test-recursive/receipts/native-folds-incomplete.json)
and [separate witness replay](https://github.com/VladimirReshetnikov/Leant/blob/main/test-recursive/receipts/native-fold-witnesses.json).

The contextual audit found a separate correctness gate: an exhaustive search
that omitted class methods must not refute the complete qualified source type.
The correction suppresses negative authorization for retained
qualifications and separately supplied contexts, while preserving positive
checks and genuine unconstrained refutations. Root and nested method-source
regressions pass; the aggregate receipts retain the necessary correction of
older tests that expected those unsound refutations. This correction
does not add class methods to the search vocabulary.
Expanded Church acceptance must cover additional total operations
and all nineteen partial cases with explicit supplied defaults. New source and
oracle controls must compile and run before those cells count as accepted
behavior.

## Earlier published baseline and resolved experiments

The implementation-progress wording in this historical section describes the
experiments that led to the published checkpoint above. Its Haskell and live
Lean case failures are resolved, and the full Leant boundary suite now passes.
The historical next-step language below records the earlier plan, not the
current implementation status.

Djinn's [case-search checkpoint](2026-09-07-recursive-data-cases.md), committed
in `5fcc4a8b` and corrected in `3ce26cfd`, passed eight Haskell execution
scenarios. Leant's working integration of the latter revision also passed all
eight public behavioral queries and independent Lean replay: sixteen empty
axiom inventories, plus an actually falsified control. This closes the earlier
`null`/`tailOr` experiment for Djinn. It does not establish cross-engine or
complete integration acceptance.

The next gate at that point was **Exference parity and isolation between engine modes**.
At that dependency revision, Exference missed `tailOr` within 100,000
steps and timed out on the unary tuple-payload case. `both` also timed out on
that payload case despite the accepted Djinn-only implementation. Search
latency can therefore hide an implementation already reachable in another
engine. This made scheduling a concrete delivery issue. The plan was to fix
the exposed search cases, then measure and correct any remaining Both delay.

The ensuing Exference experiment made recursive input splitting optional and
allowed finite nonrecursive fields to be inspected while keeping recursive
descendants opaque. Its focused fixture exposed two evidence gaps: unused
binders were not represented as compatibility wildcards, and the checker only
retained a narrow recursive zero/step case returning the scrutinee's own type.
That restriction prevented a checked list-to-Bool graph even when the search
term was otherwise valid. Declaration-backed complete-case evidence later
closed the gap while retaining exhaustiveness, lexical field scope, and exact
graph/compatibility association. The experiment and failed mixed-engine run did
not themselves establish acceptance; the failed run performed no independent
kernel replays.

The unused-binder correction is committed separately in `02bfde77`; its strict
build and all 53 Exference engine tests pass. It emits checked wildcard patterns
for unused lambda and let binders without relaxing exact erasure equality.
The broader complete-case graph extension first passed direct engine tests
while the ordinary-data matrix still failed on two independently typed recursive
inputs. Search needed both whole-value forwarding and complete input inspection;
changing their order did not overcome the score charged for all case branches.
The accepted protected scheduling shares the existing step, queue, and depth
limits. An earlier experimental forwarding penalty changed depth accounting
and was discarded.

Leant's full post-integration suite also needed a clean rerun. The first run
had missing fake-Z3 setup and two genuine positive-constructor-bound failures;
`3ce26cfd` corrects the latter by activating case plans only for recursive
types in negative positions. A passing focused replay does not replace that
full-suite gate. Detailed historical receipts and settings are recorded in Leant's
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

## Earlier fold-first schedule and its remaining gates

The decision table at the top supersedes this earlier delivery order. Keep
the acceptance requirements below; the nested dictionary audit adds an earlier
correctness gate, and corpus execution now accompanies each delivery.

The observed false-negative-evidence gate is now corrected and validated in
canonical Djex; carry it into the next Leant dependency integration. Before
capability expansion, resolve Lean's verification-request failure on an
already-generated correct length candidate.
The latter times out at the five-second request boundary with 71 seconds still
available to the command; increasing the raw search window cannot recover that
skipped occurrence. Keep per-request and whole-command timing separate.
Leant's later direct five-second control reproduces the timeout without any
search or serializer preparation, after a separate 91-second startup delay.
The earlier identical direct program completed in 0.736 seconds under a
60-second diagnostic guard. Thus search load is not a necessary cause; startup
and request variability need measurement at the process/IO boundary before
choosing a scheduling fix or changing defaults.

| Order | Existing goal item | Deliverable | Acceptance and reason for its position |
| --- | --- | --- | --- |
| 1 | Priority 2, recursor stage; extend priority 4 corpus | Complete Lean acceptance and broaden supplied recursor composition | Resolve the observed verification-request failure, then retain the accepted Haskell `map`, `append`, and generalized `length` fixture in both engines. Replay supplied folds and tree operations in Lean, including provider-inventory and termination checks; extend accumulator shapes where concrete programs expose a gap. |
| 2 | Priority 3 | Complete lexical-Given production synthesis, then evidence derivation | Integrate the checked Djinn root-Given increment with Lean source metadata/preparation/routing. Extend forced nested uses and actual slot association, then methods, conditional instances, superclasses, and contextual certificates. The isolated renderer and bounded Haskell acceptance do not establish production Lean synthesis. |
| 3 | Priority 4, corpus completion | Close the remaining Church behavioral coverage | Execute the prepared extended total and all-19 supplied-default fixtures: oracle preflight, fresh processes, controlled providers, false controls, exact full-signature replay, hashes, and actual axioms. Add this coverage during the earlier deliveries; fixture preparation alone is not acceptance. |

Leant's bounded simplification milestone is now published in `990b7f3`. Its
[acceptance report](https://github.com/VladimirReshetnikov/Leant/blob/main/docs/reports/2026-09-07-bounded-behavioral-simplification.md)
records 16 isolated method controls, 15 live queries across all three engines,
and independent replay of six displayed results. Every candidate has an empty
axiom inventory; quantified simplification proofs use exactly `propext` and
`Quot.sound`. The full configured boundary run passed 619/620 tests, with one
existing provider-stage timeout; an unchanged focused retry passed. These
separate receipts do not claim an unfiltered 620-test pass.

The first/best CLI fixture is accepted and should be retained at subsequent
integration checkpoints. The full configured Leant suite now passes. The
ordinary-data matrix and bounded simplification's live
acceptance are complete at their recorded boundaries; retain
it as a regression gate while proceeding to recursors. The schedule separates
delivery size from importance. Dictionary support
remains a required capability; bounded simplification does not replace it.
Nor does accepting a recursor milestone complete recursive-data work without
the documented source-evidence and Lean checking gates.

The earlier full Leant run passed 645/647 tests in 297.72 seconds; its two stale
expectations concerned typed wildcard authority and the first spelling of a
Nat-case candidate. The refreshed assertions preserve the semantic case in the
original 1,024-step/default-12 bounds, while the wildcard fixture keeps its
128-step bound. Both focused tests pass in 0.41 seconds without increasing
budgets. Strict build v6, the isolated renderer replay, and the full 647-test
rerun now pass. The
[historical failed aggregate](https://github.com/VladimirReshetnikov/Leant/blob/main/test-recursive/receipts/unit-before-fixture-refresh.json)
remains separate evidence.

## Implementation choices to make explicit

**Preserve the accepted recursive-data boundary.** Djinn now uses coherent
opaque datatype views for goals and providers, exact constructor premises,
and checked one-layer elimination. Keep that source authority and the
negative-occurrence activation gate. The accepted matrices now show that a
supplied default can remain whole while another recursive argument is inspected,
and that a finite tuple field is accessible without opening another recursive
layer. Retain aliases, multiple inputs, tuple results, and unchanged
positive-only bounds; broader mutual-recursion coverage still needs its own
fixtures. A bounded case view must retain
its incomplete-search status and cannot authorize a non-inhabitation claim.

**Use existing evidence infrastructure for contexts.** The new
[`SourceGraph.hs`](../../djinn/src-internal/Djinn/Internal/SourceGraph.hs)
and Exference checker reconstruct direct lexical evidence. Extend search and
production projection around these checked identities. Carry provider identities
and lexical givens through the graph and the Lean projection. Test sibling-scope
leakage, escaped skolems, wrong providers, and unavailable dictionary authority.

Leant's isolated direct renderer has now passed 21 focused tests and independent
Lean replay of seven exact generated implementations, seven payload observations,
and two wrong-result controls, with sixteen empty axiom inventories. Its
[companion report](https://github.com/VladimirReshetnikov/Leant/blob/main/docs/reports/2026-09-07-synthesis-retriage.md)
distinguishes this component evidence from production routing. Complete that
route with versioned full source metadata, exact binder visibility and
`Prop`/`Type u`/`Sort u` domains, separate class/nominal authority, and rooted
propagation through the actual graph openings and substitutions. An initial
`Type 0` subset must reject richer domains explicitly; selected polytypes must
retain their own source metadata. Preserve rank-N callback structure, but
initially reject impredicative selections until their resulting Lean universes
can be checked: `Type 0` binder domains do not put the whole polymorphic type
in `Type 0`. Missing metadata must not fall back to context erasure or
replacement instance search. Retain the Djinn increment's kind, erasure, budget,
and source-negative-evidence controls alongside this work, then methods,
instances, superclasses, and contextual certificate association.

Within contextual synthesis, forced use beneath a nested Given should precede
general partial constrained instantiation: it establishes the scoped dictionary
ownership both need. Duplicate equal Givens require the actual proof-selected
slot to survive erasure and source-graph construction; removing the current
duplicate guard alone is insufficient. Add a live higher-kind constrained
provider regression alongside the existing original-kind controls. Vacuous
type slots can use a private entrance that performs kind validation and
structural preparation together; the callback-only entrance must remain
conservative. Methods need owned method schemes and dictionary application
evidence before they enter search, followed by conditional instances,
superclasses, and residual qualification under partial type application.

**Behavior is a continuing acceptance requirement.** Add host execution and
false controls with each new program family. Keep total and supplied-default
cases separate. Finite observations establish only those observations;
universal claims require a kernel-checked proof of the stated proposition.
An unsuccessful proof attempt remains inconclusive.

## Other ideas, re-ranked

| Idea | Disposition | Concrete trigger for promotion |
| --- | --- | --- |
| Exact accepted-candidate provenance and reproducible replay | Complete 39-cell matrix and 680-test suite accepted | Retain own-Variant graph/renderer/engine observations, non-forcing diagnostics and direct kernel pinning. Add exact native request-role/candidate correlation for reliability work; defer a general tracing framework. |
| Routing-test maintenance | Incremental when a boundary is touched | Replace brittle source-text counts with executable routing or boundary controls where practical, preserving coverage. Do not turn this into a broad cleanup milestone. |
| Selected dictionary occurrence and complete context metadata | Next narrow global-provider increment under the accepted guard; exact selection before relaxing overlap guards | Admit one fully described global contextual provider under the existing `Type 0` non-overlap guard; the current provider map is empty and rejects every typed global. Separately demonstrate differing outer/inner payload behavior under faithful scoped capture in Haskell and Lean. Graph identities or successful GHC compilation alone do not establish selected dictionary ownership. Extend further schemes or evidence derivations individually. |
| Tree folds and accumulator programs | Next recursion extension after live verification is reliable | Select concrete missing corpus programs, retaining exact recursor inventories, full-signature behavior and termination checking. |
| Native Windows Length acquisition | Conditional milestone for a concrete Windows Length workflow | Implement the complete bounded acquisition, solver execution, and independent replay route; configuration parsing and the 432 existing Length tests do not establish this missing native acquisition path. |
| Cross-engine progress and cancellation | Retain the accepted request-policy and raw-slot alternation fixes as regressions | Leant now passes the tuple/default cases in all three modes without increasing the recorded bounds. These fixes do not preempt a single engine step. Promote broader scheduling only for a measured remaining latency or cancellation failure. |
| Other search and checker performance | Measure rejected contextual proofs and the existing exhaustion case next; optimize only a demonstrated cost | The Exference supplied-fold `[a] -> a` rejection fixture takes about 147 seconds at 100,000 steps. Nested introduction helpers can also consume slots with unsupported proofs. Measure those costs separately from cold startup, first accepted result, rendering/checking, and memory. Preserve original charges; do not silently refill a window or raise defaults. |
| Failure diagnostics and capability receipts | Required delivery infrastructure; promote precise stage diagnostics, not another framework rewrite | Keep graph absence, unsupported proof admission, compiler rejection, behavioral falsehood, inconclusive checking, and exhausted search distinct. Retain per-operation and per-engine results, actual replay status, source revision, settings, exact emitted source, and negative controls. Freeze a small repeated validation matrix before expanding diagnostics further. |
| Semantic provider retrieval | Retain as the next scaling investigation | Show a useful provider excluded by inventory selection in a realistic project; measure retrieval recall as well as latency. Larger inventories alone are not an acceptance criterion. |
| Canonical duplicate keys and a shared subgoal DAG | Defer broad refactoring; allow a measured local optimization | A profile must identify duplicate comparison or repeated subgoal work as material. Preserve source identity, scope, budget charging, and replay. |
| Isolated-worker production routing | Separate integration project | Require environment snapshots, backend parity, transcript equality, bounded cancellation, and memory measurements. The existing foundation is not proof of a speedup. |
| Native Lean tactic integration or a Lean implementation of the engine | Defer broad migration; allow a small independently replayed vertical slice when a workflow requires it | An editor or proof-mode workflow must define the fixture. Keeping elaborated `Expr` values could remove translation boundaries, but a rewrite does not establish equivalent search coverage, budgets, or behavior and should not displace the current integration work. |
| Dependent/indexed refinement and residual-hole search | Defer until the ordinary-data and contextual milestones settle | Start from a concrete missing indexed program and explicit equality/transport obligations, rather than a general dependent-synthesis rewrite. |
| Arbitrary frontier widening, persistent caches, cooperative internal search, equality saturation, induction/invariant discovery | Defer | Require a missing program or measured bottleneck that the smaller accepted extensions cannot address. |

The older architectural proposals remain sources of design ideas. Their
statements about missing typed graphs and candidate evidence must be checked
against the implemented graph foundation before turning them into new work.
