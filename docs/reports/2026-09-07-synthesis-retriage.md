# Synthesis re-triage: finish bounded deliveries, then extend evidence

This 2026-09-07 update changes the execution order, not the scope or numbering
of the active **implement priorities 1–4** goal. The
[original roadmap](2026-09-06-synthesis-next-priorities.md) and
[completion register](2026-09-07-synthesis-priorities-1-4.md) retain that scope.

## Decision after the latest acceptance and source review

**The guarded Djex contextual increment is accepted. Finish its bounded Lean
integration, preserve failed-candidate evidence across elaboration timeouts,
and resolve live fold verification before broadening search. Execute Church
behavior alongside these deliveries.** Exact selection between equal
dictionaries remains a separate capability extension.

The material change from the previous order is to separate a correctness
boundary that can be shipped now from the larger evidence extension that will
eventually remove it. Keep the original four priorities open until their own
completion requirements pass; this table is a delivery order, not new goal
numbering.

| Order | Delivery | Completion gate and boundary |
| --- | --- | --- |
| 1 | Publish bounded production Lean contexts | Integrate the accepted guarded dependency. Validate the prepared exact-candidate provenance path with a strict build, 52 focused tests, the expected full 678-test inventory, and 39 public cells across ordinary/named-`where` commands and Djinn/Exference/Both. Require 18 positive full-type/payload replays, three actual where-False controls, and 18 explicit universe/global-metadata refusals. Reconcile the actual unfiltered suite inventory and pin the directly invoked replay kernel. |
| 2 | Preserve failed-candidate evidence when elaboration times out | First/best/all live repair selection already passes. Record the original compilation failure before the repair, then retain its observation identity, full type, exact expression/error and evidence when the shared deadline interrupts the retry. Add a deterministic timeout control without granting another candidate slot or deadline; retain owned worker cleanup and rerun affected CLI tests. |
| 3 | Repair verification of already-found folds | Trace the actual native append/length workload under unchanged bounds; identify the failing request stage and fix that cause. Validate deadlines, cancellation, owned cleanup, recovery with a context-dependent command, false controls, and exact replay. Larger search windows and repeated fast standalone requests do not close this gate. |
| 4 | Execute the remaining Church behavior cells continuously | Both partial oracle preflights now pass. Run the additional total cases and all 19 explicit-default cases in both Haskell engines and all three Lean modes. Require controlled providers, real false controls, and independent replay of the exact displayed implementation at its full original or explicitly default-adjusted signature. Record each operation and engine separately; independent cells need not wait for fold repair. |
| 5 | Preserve the selected dictionary before relaxing guards | Carry the proof-selected introduction occurrence and ordered slot through lowering and reconstruction. First require a forced equal-predicate outer/inner fixture with distinct payloads and Haskell/Lean replay. Then extend one missing provider scheme or dictionary derivation at a time. Broaden supplied tree folds and accumulator programs only after native verification is reliable. |

Orders 1 and 5 advance original priority 3; order 2 advances priority 1;
order 3 advances priority 2; order 4 advances priority 4 and supplies regression
cases for every delivery. The small timeout repair and independent corpus
cells can proceed alongside integration while heavy validation stays serialized.
Full dictionary selection and fold-specific transport work need not block the
bounded contextual delivery.

## What changed since the preceding re-triage

- **Guarded nested-Given admission is accepted.** The final complete private
  and facade suites pass all 106 and 147 tests. The facade suite takes 234.23
  seconds and includes 24 contextual cases and independent GHC witnesses. Together
  with ten complete v2 suites, the checkpoint records 1,048 Tasty cases,
  700 Church cases and 100 scope queries across twelve complementary complete
  suites. Production sources stayed identical; only five fixture lines changed
  to remove unsupported explicit datatype/synonym parameter kinds. The
  [guarded increment report](2026-09-07-djinn-nested-givens.md) and
  [receipt](../../test-integration/receipts/guarded-nested-givens-checkpoint.json)
  retain the setup failures and corrected complete reruns. Equal active
  dictionaries and nested forall/Given interaction remain explicitly unsupported.
- **The Lean production route has its first public acceptance.** A strict
  executable/unit build and compilation of the exact emitted serializer pass.
  Djinn passes three named-`where` contextual queries and their exact full-type
  replays, with 24 empty declaration inventories. The false control records
  one falsification and no inconclusive checks. Ordinary queries, Exference,
  Both, metadata refusals, and the fresh full integration suite remain open.
  These are working-source results, not a published complete capability.
- **The stronger Lean acceptance path is implemented, not yet accepted.**
  The runner directly invokes and hashes the resolved kernel; each displayed
  verification Variant retains its own graph, renderer alternative and engine.
  Compatibility debug output does not force lazy origin recovery from a later
  engine stream. All 14 production-runner, 12 extended-corpus and six partial
  pure controls pass. The new Haskell strict build, 52 focused tests, complete
  678-test integration inventory and 39 live contextual cells remain pending.
- **Both partial oracle preflights pass.** All 20 Lean files pass with 491
  declaration inventories: 487 empty and four explicitly allowlisted
  observer/proofs. The repaired `at` inline predicate preserves its complete
  observation set. Haskell passes all 63 controls. All 19 supplied-default
  signatures and 736 observations remain represented. Expanded live synthesis
  acceptance remains open; the oracle results do not close it.
- **Axiom exceptions stay local to observer proofs.** The extended total
  preflight passes 58 inventories, with only two comparison/control proofs
  using `propext`. The partial preflight permits exactly four named `atKey`
  observer/proofs to use `propext`, `Classical.choice` and `Quot.sound`, traced
  to standard `String.length`. Every candidate implementation still requires
  an empty inventory. Neither exception changes the behavioral contract.
- **The fold failure remains a live-workload investigation.** Opt-in request
  tracing has compiled and its focused tests pass. Standalone startup/request
  measurements vary, and some exact requests complete within five seconds.
  No traced live append/length acceptance or root-cause fix is established.
- **Elaboration selection is an accepted baseline.** The current 100-test CLI
  suite includes the live first/best/all repairs and exact GHC replay. The
  remaining concrete bug is loss of the original bounded failure sample if
  the shared candidate deadline interrupts its elaboration retry.

The [working-boundary audit](https://github.com/VladimirReshetnikov/Leant/blob/main/test-context/receipts/retriage-working-boundaries.json)
retains the earlier working results and their receipt hashes; the guarded
checkpoint above supersedes its pending-guard status. Audited starting
revisions for that earlier audit are Djex `9f45bef8` and Leant
`7f700f89`. Published Leant still pins Djex `922c5558`; the working dependency
is `90c88261` and does not contain the final nested guard.

The initial production context subset is local `Type 0`. It explicitly
refuses richer sorts, unrecorded constant universe vectors, selected polytypes
without their full metadata, and global/caller premises without source packets.
A result in `Type 0` does not establish that the declaration has no universe
arguments. Extend this subset only with exact metadata and independent replay.

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
| Exact accepted-candidate provenance and reproducible replay | Implemented; Haskell and live validation pending | The 14 pure production-runner controls pass. Validate own-Variant graph/renderer/engine observations, non-forcing compatibility diagnostics and direct kernel hashes through the prepared strict/focused/full/live gates. Defer a general tracing framework. |
| Selected dictionary occurrence and complete context metadata | Next capability extension after bounded Lean integration | First preserve and replay actual outer/inner ownership with differing payloads for equal predicates. Then extend one missing provider scheme or evidence derivation; equal predicate types alone do not identify a dictionary. |
| Tree folds and accumulator programs | Next recursion extension after live verification is reliable | Select concrete missing corpus programs, retaining exact recursor inventories, full-signature behavior and termination checking. |
| Native Windows Length acquisition | Next independent platform milestone after the current integration gates, or sooner for a concrete Windows Length user task | Implement the complete bounded acquisition, solver execution, and independent replay route; configuration parsing and the 432 existing Length tests do not establish this missing native acquisition path. |
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
