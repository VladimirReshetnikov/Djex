# Synthesis re-triage: finish bounded deliveries, then extend evidence

This report, updated on 2026-09-08, changes the execution order, not the scope
or numbering of the active **implement priorities 1–4** goal. The
[original roadmap](2026-09-06-synthesis-next-priorities.md) and
[completion register](2026-09-07-synthesis-priorities-1-4.md) retain that scope.

## Current decision and next delivery gates

The [latest re-triage](2026-09-08-synthesis-next-deliveries.md) records all ten
completed native integration gates at Leant production revision `43f1bc11` with
Djex `bfc3692e`, now pinned by Leant. Native List recursors now pass **12/12 at the original limits**,
including nine exact full-type replays and three completed False controls.
Extended Lean Exference behavior passes **13/13**, nested-result foralls pass
nine cells, and both native engine signature corpora pass **350/350** each
with kernel replay.
The accepted 702-test unit run, six method cells, nine method-control sessions,
39 local-context cells and 15 simplification queries are retained with their
fixture-version boundaries in the
[receipt](https://github.com/VladimirReshetnikov/Leant/blob/main/test-recursive/receipts/combined-native-integration.json).

The [terminal integration](2026-09-08-combined-native-integration.md) passes its
final unchanged-input check and closes the dependency release gate. The
historical 9/12 recursor failure is superseded by the new matrix. Native tree
and the complete Church behavioral target remain open.

| Order | Next delivery | Required evidence |
| --- | --- | --- |
| **1 — bounded transfer checks, P2/P4** | Run the native tree fixture and recheck Haskell Exference `foldl1` once at original bounds. | Actual synthesis, exact original-type replay, all observations, completed False controls; tree provider/axiom/termination checks. A miss gets one concrete diagnosis before the next independent batch. |
| **2 — Haskell frontend correctness, P3** | Implicit-root scope, one-shot contextual commands and typed list output, as separate increments. | Both engines through public ordinary and named-`where` queries; compile the exact displayed implementation without rewriting the original signature. Preserve binder order, nested scope and selected payloads. |
| **3 — full behavioral matrix, P4** | Remaining extended operations, selectors, nonempty reductions, extrema and native-`Int` indexing. | All 13 extended plus all 19 supplied-default operations across Haskell Djinn/Exference and Lean Djinn/Exference/Both. Separate current/historical passes, misses, timeouts, replay failures and unrun cells. |
| **4 — contextual evidence, P3** | Derived Haskell method schemes and mixed Lean inventories, then selected equal-predicate dictionaries, superclass/conditional evidence and richer universes. | Preserve source ownership and lexical dictionary payloads through search, rendering and replay. Retain existing guards until each extension passes. |

These are execution priorities, not a reduction of the original priorities 1–4.
The transfer checks need not block independent frontend or Church work. Heavy
validation stays serialized with frozen inputs. Re-run relevant complete suites
when production inputs change; repeated unchanged runs are not an implementation
strategy.

### Other ideas

The combined verifier, semantic provider filter, ordinary explicit-forall
contextual REPL output and canonical Exference tree carrier are delivered and
remain regressions. Further verification-request reduction leaves the immediate
queue now that all native recursor cells pass.

Exact duplicate suppression in best output is the next performance candidate
after frontend correctness. First establish identity using typed evidence,
source ownership and selected dictionaries; repeated printed text alone is
insufficient. Compare at the preserved original failing settings.

Keep diagnostics narrow and tied to a concrete failed derivation. Defer broad
ranking, memoization, semantic pruning, worker tuning and provider retrieval
until a current workload identifies their need. General recursion, indexed
synthesis, an engine rewrite, editor integration and Windows Length acquisition
remain separate expansions; the
[detailed decisions](2026-09-08-synthesis-next-deliveries.md#disposition-of-other-ideas)
state what would promote each.

The full behavioral worklist is **160 operation/mode cells**, with controls and
replay in addition. It is not a count of missing or accepted cells. The 19
supplied-default identities remain:

- Selectors: `head`, `last`, `fromJust`, `fromLeft`, `fromRight`, `atKey`.
- Nonempty reductions: `foldl1`, `foldr1`, `reduce`.
- Extrema: `maximumBy`, `maximumOn`, `minimumBy`, `minimumOn`, `minMaxBy`,
  `minmaxElement`, `maximum`, `minimum`, `minMax`.
- Native-`Int` indexing: `at`, including negative indices.

Use explicit defaults or inhabitance assumptions for the agreed total Lean
counterparts, retaining separate classification of the original partial types.
The 350-signature inhabitation results do not establish this behavioral matrix.

The remaining sections preserve historical results and diagnoses at their
recorded revisions. They do not override this execution order.

## What changed in this re-triage

The [focused introduction change](2026-09-08-focused-forall-search.md) follows the actual unfinished
`maybeEither` branch to the queue boundary. It remains at position 1,002 of
8,192 after the original prefix; it was delayed, not capacity-pruned.
Completing determined goal groups before interleaving siblings, together with
retaining the forall estimate until arrow parameters enter scope, finds a
matching program in the original 256-candidate window. Ordering alone failed;
the combined generic change passes the full extended corpus and regressions.
This supersedes the earlier Exference search-miss diagnosis below. The
parenthesized-forall worker correction remains a separate accepted repair.
Leant still pins `4a4ed0fc`; advancing that dependency requires native validation.

The [accepted integration follow-up](https://github.com/VladimirReshetnikov/Leant/blob/main/docs/reports/2026-09-08-global-contextual-providers.md) supersedes the earlier
working-prototype and incomplete-regression status below. The following
diagnoses and counts remain historical evidence at their recorded revisions.

The [constraint-only inference report](2026-09-08-constraint-only-provider-inference.md)
supersedes the engine failure diagnosis below with implementation and tests.
It retains the remaining frontend failures rather than counting behavioral
preflight as synthesis acceptance. The earlier diagnostic evidence below
continues to belong to its recorded sources and settings.

The [diagnostic receipt](../../test-church/receipts/synthesis-retriage-diagnostics.json) embeds selected process captures, the failed
method fixture, tested heuristic diffs and source hashes. It is a diagnostic
extraction from completed local runs, not a new acceptance matrix. Full raw
prefix artifacts remain in the hash-addressed local runs identified there.

**The class-method boundary is now specific.** In the forced fixture, `α`
occurs only in the provider's class constraint, so the `Nat` result cannot
determine it. Djinn reaches exact lexical-Given lookup with a fresh unresolved
type variable. Exference returns no checked group; the same inference gap is
a source-supported hypothesis, but its terminal path has not yet been isolated.
Adding a term argument of type `α` would avoid the failing case and would not
meet this delivery gate.

Start with a unique coherent substitution obtained from in-scope Givens. Match
only fresh provider variables, check all constraints together, and retain the
ordinary scope/occurs checks and graph witnesses. Do not mutate a goal's rigid
type variables or recover dictionary authority from a printed class name.
Multiple possible instantiations need bounded alternatives with their own
evidence; equal predicates need the selected dictionary occurrence. Those are
distinct extensions, so an ambiguity refusal may remain in the first milestone.

**The `maybeEither` prefix is now matched to the real frontend.** An empty user
workspace still loads intrinsic list, unit and tuple constructors. A manually
empty environment produced a different order and failed the first diagnostic's
correspondence gate. The corrected loader-based capture reproduces the original
CLI samples at positions 63, 117 and 119. It observes 256 graph-backed, typed
candidates in 3,346 batches, with 31,165 queue prunings. Of those candidates,
199 have extra result-argument applications; 232 contain three forall
introductions and 24 contain four. The supplied checked reference has five.
These shapes do not prove which rule produced a candidate, that five
introductions are necessary, or that every equivalent solution is unreachable.
The public observer exposes no pending goals or actual search-rule history.

Counting forall bodies/bound variables in the complexity heuristic passed its
83 focused engine tests but still produced 256 false candidates. Combining that
change with an extra-arrow application penalty also produced 256 false
candidates. Both runs had zero behavioral errors/timeouts and passed their
separate False/oracle controls. Neither experiment was adopted; baseline source
was restored and rebuilt. A new ranking proposal now needs an observed branch
or queue diagnosis and a successful original behavioral query.

**`foldl1` has a valid reference construction, not synthesis acceptance.** Its
supplied continuation-carrier witness compiles at the original full signature
and passes all 36 original observations. That run performs no search. Inspect
the point where a flexible goal would need function structure, and establish
whether a general construction or scheduling rule is missing before changing
defaults. This witness does not prove every equivalent derivation needs that
same rule.

## Current evidence and its limits

The evidence below records accepted milestones and historical diagnostics.
The September 8 decision table above supersedes their earlier delivery order;
their recorded source revisions and acceptance limits remain unchanged.

The [forall-graph increment](2026-09-08-exference-forall-graphs.md) separates
two remaining concerns. A supplied, provider-free `maybeEither` witness now
passes exact graph construction, typed rendering and GHC replay of the
original predicate. A fresh public query at the original window still misses:
256 false, zero errors and zero timeouts, including five successful
same-candidate compilation repairs. This adds no behavioral coverage. Candidate
construction and ordering are the next diagnostic targets; a larger default
window is not the acceptance criterion.

The [published Leant integration](https://github.com/VladimirReshetnikov/Leant/blob/e1ed6f7eb308b9a431db1ec8c956f9eb7a30d05f/docs/reports/2026-09-08-nested-forall-graphs.md)
closes the matching frontend gate: all nine native cells pass, with six exact
full-type outputs, 24 finite observations, three actual False controls and 16
empty axiom inventories. Each variant retains its own engine, renderer, closed
root and both actual forall introduction sites. The strict build and complete
686-test suite pass in 358.12 seconds. This strengthens the reason to address
candidate construction and ordering next; it does not increase the accepted
Church behavioral operation count or narrow priorities 2–4.

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
premises without source packets. The published accepted provider map is empty
and rejects typed globals; the subsequent global-method acceptance above extends this older boundary.
Equal active dictionaries remain unsupported. Rank-N constrained
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
command, not a resolved kernel-executable hash. This historical event-only trace predates the request-correlated diagnosis below.

The newer [request-correlated receipt](https://github.com/VladimirReshetnikov/Leant/blob/72d967e57b7d044320b574151f912e88a6ff5a3e/test-recursive/receipts/request-correlation.json) passes the strict build,
14 focused controls and all 686 unit tests (357.63 seconds), and captures the
failed append baseline completely. Candidate type and positive/negative checks
omit `env` in the fresh user session. The installed REPL source confirms that
this repeats header/import initialization. The [matched empty-session control](https://github.com/VladimirReshetnikov/Leant/blob/72d967e57b7d044320b574151f912e88a6ff5a3e/docs/reports/2026-09-07-empty-environment-diagnosis.md)
preserves the exact candidate command and annotation: backend 1/request 11
times out after 5.008023501 seconds, while control request 15 completes in
0.0223364 seconds. Its decoded payload differs only by `env: 2`. Append then
passes in 24.85 seconds with exact independent kernel replay; its actual False
control passes in 10.19 seconds. The latter trace retains 128 requests and
explicitly omits 187, so its diagnostic capture is incomplete even though the
behavioral control passes. Physical file rereads and a transport defect are
not established by this diagnostic evidence.

The subsequent [production reuse repair](https://github.com/VladimirReshetnikov/Leant/blob/f35c243a9a2cf26d6c5ffe8c5e16ba4fba523b2e/docs/reports/2026-09-07-empty-environment-reuse.md) now passes native Djinn append
and length at their original bounds, exact kernel replay, and actual False
controls. Append's command files are byte-identical to the failed baseline.
Four lifecycle sessions cover 11 queries, four accepted replays and ten empty
axiom inventories, including actual empty-session backend retirement. The
strict build and complete 686-test suite pass in 335.32 seconds. Both native
positive traces are complete with no timeout. Each native False control passes
with 87 falsifications and zero inconclusive checks while its diagnostic trace
explicitly omits 184 records at the unchanged cap. The [acceptance receipt](https://github.com/VladimirReshetnikov/Leant/blob/f35c243a9a2cf26d6c5ffe8c5e16ba4fba523b2e/test-recursive/receipts/empty-user-environment.json)
keeps these boundaries separate; remaining engine, tree-fold and accumulator
acceptance follows this specific reliability repair.

The [P4 behavioral coverage report](2026-09-07-priority4-behavioral-coverage.md)
now records a fresh **13/13 extended Haskell Exference run**, plus historical
**12/13 Djinn extended and 2/4 Exference explicit-default** acceptance.
Djinn still lacks `maybeEither`; Exference `foldl1` and `at`
remain unaccepted at the recorded bounds. Fresh native-Int length synthesis,
exact GHC replay and False controls pass in both Haskell engines; Exference's
repeat length run adds no unique coverage. Historical harness hashes and
the two earlier Djinn length fixture failures remain separate.

Lean Exference now accepts **6/6 of the selected first-six operations**
from two runs: five exact kernel replays in the original batch and native
length in an independent corrected-fixture follow-up. Both False controls
pass. The initial length inventory failure and missing replay remain in the
historical receipt; this is a composite, not a fresh six-cell rerun.
These are recorded subsets, not a complete new matrix. All 13 extended
operations and all 19 explicit-default counterparts remain required across
both Haskell engines and all three Lean modes.

Both partial oracle preflights pass: 20 Lean files with 491 exact inventories
(487 empty and four named observer/proof allowances), and 63 Haskell controls.
All 736 observations remain represented. Earlier supplied-default `head`
acceptance has its own two-engine receipts. Preflight and the 350-signature
inhabitation corpus do not substitute for the remaining behavioral cells.

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
| Exact accepted-candidate provenance and reproducible replay | Complete 39-cell matrix and 680-test suite accepted | Retain own-Variant graph/renderer/engine observations, non-forcing diagnostics and direct kernel pinning. Retain the accepted bounded request capture and its whole-record omission accounting. Keep the accepted empty-environment repair and lifecycle controls as regressions; defer a general tracing framework. |
| Verification trace and runner consistency | Completed for the global-method increment | Retain exact ordinary/where verification correlation, portable runners, explicit capture caps and negative controls. Defer a broader tracing framework unless another concrete acceptance gap requires it. |
| Routing-test maintenance | Incremental when a boundary is touched | Replace brittle source-text counts with executable routing or boundary controls where practical, preserving coverage. Do not turn this into a broad cleanup milestone. |
| Constraint-only provider instantiation | Engine/API, bounded Leant integration and loaded Haskell explicit-forall behavioral use accepted | Retain the 2,306-test canonical and 701-test native receipts at their distinct revisions. Implicit-root scoping and ordinary presentation are delivery 2; derived method schemes and mixed Lean inventories are delivery 4. Do not reopen completed admission work. |
| Selected dictionary occurrence and complete context metadata | Required before relaxing overlap guards | Separately demonstrate differing outer/inner payload behavior under faithful scoped capture in Haskell and Lean. A unique type substitution, graph identity or successful compilation alone does not establish selected dictionary ownership. Extend conditional providers and superclass evidence individually. |
| Tree folds and accumulator programs | Independent next recursion delivery; fixture prepared, not run | Use the polymorphic tree accumulator to test order-sensitive composition, retaining exact recursor inventories, full-signature behavior and termination checking. Native verification repair is already accepted. |
| Flexible-goal construction and branch diagnostics | Apply next to remaining Djinn/default misses | Exference `maybeEither` is accepted. Use one failing original query to identify the next unavailable or delayed construction, beginning with Djinn parity and `foldl1`. Preserve every alternative and existing budget charges; independently check the emitted term. |
| Further heuristic tuning | Keep the accepted focused introduction change; defer broad tuning | The actual branch remained queued, and the combined scheduling/forall-phase estimate passes all 13 extended Exference operations at original bounds. Earlier broad estimates and ordering alone failed. Promote another change only for a measured remaining miss and a controlled accepted-corpus comparison. |
| Counterexample-guided search | Conditional follow-up | Promote if repeated expensive behavioral rejection remains material after required constructions become reachable. Cache only observations of the exact candidate under the exact environment and predicate; finite agreement must not become general equivalence or justify unsound pruning. |
| Native Windows Length acquisition | Conditional milestone for a concrete Windows Length workflow | Implement the complete bounded acquisition, solver execution, and independent replay route; configuration parsing and the 432 existing Length tests do not establish this missing native acquisition path. |
| Cross-engine progress and cancellation | Retain the accepted request-policy and raw-slot alternation fixes as regressions | Leant now passes the tuple/default cases in all three modes without increasing the recorded bounds. These fixes do not preempt a single engine step. Promote broader scheduling only for a measured remaining latency or cancellation failure. |
| Other search and checker performance | Measure rejected contextual proofs and the existing exhaustion case next; optimize only a demonstrated cost | The Exference supplied-fold `[a] -> a` rejection fixture takes about 147 seconds at 100,000 steps. Nested introduction helpers can also consume slots with unsupported proofs. Measure those costs separately from cold startup, first accepted result, rendering/checking, and memory. Preserve original charges; do not silently refill a window or raise defaults. |
| Failure diagnostics and capability receipts | Required delivery infrastructure; promote precise stage diagnostics, not another framework rewrite | Keep graph absence, unsupported proof admission, compiler rejection, behavioral falsehood, inconclusive checking, and exhausted search distinct. Retain per-operation and per-engine results, actual replay status, source revision, settings, exact emitted source, and negative controls. Freeze a small repeated validation matrix before expanding diagnostics further. |
| Semantic provider retrieval | Conditional scaling investigation | Show a useful provider excluded by inventory selection in a realistic project; measure retrieval recall as well as latency. The forced method fixture now passes; the mixed-inventory refusal requires source representation, not broader retrieval. Larger inventories alone are not an acceptance criterion. |
| Canonical duplicate keys and a shared subgoal DAG | Defer broad refactoring; allow a measured local optimization | A profile must identify duplicate comparison or repeated subgoal work as material. Preserve source identity, scope, budget charging, and replay. |
| Isolated-worker production routing | Separate integration project | Require environment snapshots, backend parity, transcript equality, bounded cancellation, and memory measurements. The existing foundation is not proof of a speedup. |
| Native Lean tactic integration or a Lean implementation of the engine | Defer broad migration; allow a small independently replayed vertical slice when a workflow requires it | An editor or proof-mode workflow must define the fixture. Keeping elaborated `Expr` values could remove translation boundaries, but a rewrite does not establish equivalent search coverage, budgets, or behavior and should not displace the current integration work. |
| Dependent/indexed refinement and residual-hole search | Defer until the ordinary-data and contextual milestones settle | Start from a concrete missing indexed program and explicit equality/transport obligations, rather than a general dependent-synthesis rewrite. |
| Arbitrary frontier widening, persistent caches, cooperative internal search, equality saturation, induction/invariant discovery | Defer | Require a missing program or measured bottleneck that the smaller accepted extensions cannot address. |

The older architectural proposals remain sources of design ideas. Their
statements about missing typed graphs and candidate evidence must be checked
against the implemented graph foundation before turning them into new work.
