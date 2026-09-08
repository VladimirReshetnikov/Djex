# Implementation of synthesis priorities 1–4

Status: **priority 1 complete; priorities 2–4 in progress**. This work
implements the first four items in the
[accepted roadmap](2026-09-06-synthesis-next-priorities.md). Priority 1 is complete
within its original supported-fragment scope. The earlier streaming acceptance
receipt belongs to its recorded source revisions.

The [current re-triage](2026-09-07-synthesis-retriage.md) schedules the remaining
deliveries. It preserves every completion requirement below. Bounded Lean
simplification and the eight-case ordinary-data matrix now have live acceptance;
the guarded nested-Given increment now also has complete affected-suite
acceptance. Next are its bounded production Lean integration, reliable
supplied-fold verification, and broader Church behavior. Same-candidate Haskell
elaboration, including failure-sample retention across retry timeouts, is
accepted. Full proof-selected dictionary transport remains required before
relaxing overlap restrictions. The fresh Lean strict build and 52 focused tests pass,
along with two Djinn identity replays through ordinary and named-`where`
commands, each retaining its own accepted-variant provenance. The corrected
[complete Leant suite](https://github.com/VladimirReshetnikov/Leant/blob/c17ce6655a64db348e92f6f7fb9a0889cbebaa25/test-context/receipts/unit-context-integration.json)
passes 678/678 in 283.51 seconds (283.64 seconds for the owned process), retaining
the earlier 675/678 result and three stale source-assertion corrections.
Production source and bounds stayed unchanged. The
[contextual matrix receipt](https://github.com/VladimirReshetnikov/Leant/blob/af6483a658cf2e49db127405d5e22410e8e9fd78/test-context/receipts/production-context-initial.json)
accepts 35/39 cells across two runs: the initial 29 comprise 14 exact outputs,
three actual False controls and 12 metadata refusals; six independent metadata
replays pass after explicitly declaring the universe fixture's class as `Type`.
Production stayed unchanged and all integrity checks pass. All nine named-`where`
positives pass. Four ordinary forwarding/local-Given cells still reach 45-second
timeouts in Exference and Both. A collection patch is applied but unvalidated;
the complete matrix remains unaccepted.
Corpus expansion remains part of each capability's acceptance;
it does not wait for all richer context and recursion extensions. The current
re-triage preserves the original priority numbers and completion requirements.

Leant's [ordinary-data receipt](https://github.com/VladimirReshetnikov/Leant/blob/main/test-recursive/receipts/all-engines.json)
records all eight cases under Djinn, Exference, and Both: 24 exact displayed
implementations pass independent Lean replay at their complete signatures,
with 48 empty implementation/proof axiom inventories and three actual false
controls. This closes the earlier cross-engine case failures described in the
historical checkpoints below. It does not establish recursive-call synthesis
or production use of the separate lexical-Given renderer.

## Completion requirements

1. **Evidence-guided Haskell elaboration — complete.** Capture a bounded sample of failed
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

**Priority 1 is complete within the accepted roadmap's supported fragment.**
The strict timeout-repair build passes, and the complete **101-test CLI suite**
passes in 124.17 seconds, including the existing first/best/all repairs, exact displayed-source GHC
execution, actual false predicates, and the new interrupted-retry regression.
The latter preserves the original compilation failure and observation 1,
its own graph-guided alternative, and final `BehavioralTimedOut`; it consumes
one of the three existing observation slots and successfully runs the next
query with a fresh worker. It adds neither a candidate slot nor a deadline.
The real timeout case passes in 33.53 seconds. The
[acceptance receipt](../../test-integration/receipts/elaboration-timeout-samples.json)
records the complete inventory, commands, source/executable identities, and
unchanged-source and helper checks.

Missing-authority, escaped-skolem, lexical-scope, and changed-source-identity
rejections remain covered by the established shared/private checker fixtures.
This closes the original requirement, not every possible compiler rejection.
Priorities 2–4 retain their separate recursive-data, contextual-evidence, and
expanded behavioral acceptance requirements.

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
Each bounded sample is reserved before the cancellable retry. Retained graph
metadata is fully evaluated within the original candidate deadline, so emitting
a timeout sample cannot resume graph rendering. An interrupted inspection is
reported explicitly and is never mislabeled as graph absence.

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
That initial positive compiler repair is a graph-renderer fixture. The live
acceptance added below exercises the complete query path separately.

The full facade suite passed 100 tests in 10.52 seconds, and the shared
synthesis suite passed 466 tests in 0.11 seconds. The strict build uses
GHC 9.12.4 with `-Werror`. All 98 CLI tests passed in 60.80 seconds after the
shared-deadline change. Logs are retained at
`dist-newstyle/priority-facade-final.log`, `priority-synthesis-tests.log`, and
`priority-cli-final.log`; the final CLI strict build log is
`dist-newstyle/priority-shared-deadline-build.log`.

## Implicit local evidence and live repair

Exference's independent checker now retains its exact implicit selections for
context-free local polymorphic values. Each occurrence starts from its own
declared scheme and independently allocated variables. The complete check's
substitutions normalize those selections before sealing. Source binder IDs
are reserved before allocation, including in constructor-derived local types.
The graph records implicit applications, preserves compatibility erasure, and
does not gain global provider-certificate authority. Contextual schemes retain
their existing constraint checking and explicit graph-absence result.

The public behavioral query

```haskell
:synth repaired :: (forall a. a -> a) -> [(forall b. b -> b)] where null (repaired id)
```

with Exference, `select all`, an eight-candidate observation window, and 1024
search steps reaches an occurrence whose compatibility expression is
`\f1 -> f1 (\f5 -> f5 []) f1`. GHC rejects that expression at the full signature.
The same occurrence now has a graph; its annotated expression compiles and
passes the predicate. The CLI regression checks both expression and definition
display, extracts the exact accepted elaboration, and independently compiles
and executes it with GHC. The query observes eight candidates and performs one
additional compiler check without adding a candidate slot. A second run with
`not (null (repaired id))` rejects the repaired expression as behaviorally false.

This establishes live positive repair under `all` selection. The additional
first/best acceptance below covers the other selection modes separately.

After the final namespace-reservation change, the GHC 9.12.4 `-Werror` build
passed. Full affected suites passed: 53 Exference engine tests (0.26 seconds),
513 Exference tests (1.50 seconds), 466 shared-synthesis tests (0.12 seconds),
100 facade tests (10.01 seconds), and 99 CLI tests (71.88 seconds), for 1,231
tests total. The contextual implicit-local control still declines a graph;
the existing scope, skolem, and certificate controls remain in these suites.
Logs are `dist-newstyle/priority-implicit-final-build.log` and
`dist-newstyle/priority-implicit-<suite>-final.log` for those five suites.

## First/best repaired-candidate selection

The public CLI regression now exercises `first` and `best`, each in expression
and definition display mode, with a real loaded abstract `Token` provider whose
continuation consumes an impredicative list. The first observed candidate fails
GHC at `(forall a. a -> a) -> Token`, retains its own checked graph, and passes
after graph-guided elaboration. The test independently compiles and executes the
exact displayed repair against the original provider module and full signature.
There is no substituted compiler, observation-dependent predicate, or supplied
target implementation.

The three-candidate window records one observation under `first` and three under
`best`, with one extra compiler check in each run. Best selection retains the
accepted repair while two later candidates fail compilation. This establishes
retention of that accepted result; this particular fixture does not compare two
distinct passing repairs against each other. The prior all-selection repair and
actually false predicate remain in the full CLI suite.

The strict GHC 9.12.4 build passed, the focused regression passed in 24.89 seconds,
and all 100 CLI tests passed in 142.44 seconds. Logs are
`dist-newstyle/priority-exference-forwarding-cost-build.log`,
`dist-newstyle/priority-first-best-cli-focused.log`, and
`dist-newstyle/priority-first-best-cli-full.log`. These binaries also contain the
concurrent, unaccepted recursive-case experiment; this receipt closes the CLI
selection fixture, not acceptance of that experiment or a new Leant integration.

## Leant integration and recursive-case work in progress

Leant `aab110e99e3c3d96549a05d3975b26bea93dc6ef` integrates Djex
`6890bb5a8a56902c2baf137581e23c25a376fad0`. Retained receipts record the strict
GHC build, 615 boundary tests, 18 existing corpus cases across three engine
modes, three false controls, and independent Lean replay with 69 empty axiom
inventories. This closes integration of that historical checkpoint. Priority
1's subsequent selection and timeout acceptance is recorded above; priority
4's expanded behavioral coverage remains open.

The [recursive-case checkpoint](2026-09-07-recursive-data-cases.md) now passes
all four original Haskell behavior targets and four additional scenarios for
aliases, tuple fields/results, and independently typed inputs. Coherent
datatype views and explicit constructor authority replace the earlier failing
experiment. The report records strict builds, 132 Djinn unit tests, 59 public
and 43 private graph tests, 101 facade tests, 99 CLI tests, and four property
groups with 200 trials each. The later `3ce26cfd` correction activates these
plans only when a recursive type occurs negatively, preserving positive-only
construction bounds.

Leant's working integration of that revision passes all eight Djinn-only live
queries and their independent Lean replay, with sixteen empty axiom inventories
and an actually falsified control. Cross-engine acceptance remains open:
Exference misses `tailOr` within 100,000 steps and times out on a unary tuple
payload; Both also times out on that payload. The failed matrix performed no
independent kernel replay. An Exference search/evidence fix and a correctly
configured full Leant boundary rerun are pending. See the current re-triage
for receipt locations and exact gates. This is not completion of the full
case milestone or the separate recursor stage.

## Subsequent case and lexical-evidence increment

The current Haskell ordinary-data matrix passes all sixteen cells across Djinn
and Exference: `null`, `headOr`, `tailOr`, shallow trees, aliases, `unconsOr`,
tuple fields, and independently typed recursive inputs. Each observed candidate
requires an exact source graph; independent GHC execution checks the accepted
implementation under the complete original signature. Exference's checker now
retains exhaustive constructor cases from one exact declaration inventory.
Its optional/eager case queues share one total step allowance and queue cap.
Both engines also pass `map`, order-sensitive `append`, generalized `length`,
and the empty-input control from one generic fold signature; see the
[supplied-recursor report](2026-09-07-supplied-recursor-composition.md).
The initial sixteen-cell pass and a subsequent unchanged matrix pass are logged
in `dist-newstyle/priority-cases-ordinary-protected-queue.log` and
`priority-recursors-regression-v5.log`. The earlier Leant failures above remain
the last complete cross-engine Lean receipt until this source is integrated.

The [contextual-evidence report](2026-09-07-contextual-evidence-design.md) records
explicit lexical Given graph nodes, independent source reconstruction in both
engines, qualified Haskell rendering, passing complete shared/private suites,
and full-signature GHC renderer replays. This closes the representation/checker
foundation. Eleven live Given acceptance tests cover five Exference roles,
three Djinn roles, and three leakage controls. Remaining Djinn constrained
search and production Lean projection are still open.
Instances and superclass derivations remain separate required increments.

The final strict build passes. The complete affected-suite run passes all ten
non-Length suites: 502 shared, 94 certificate, 10 graph-fingerprint, 133 Djinn,
59 public and 55 private Djinn graph, 513 Exference, 70 private Exference,
124 facade, and 100 CLI tests. Length passes 430/432; both existing deadline
failures pass an unchanged focused retry. The
[aggregate receipt](../../test-integration/receipts/recursive-context-checkpoint.json)
and [separate retry](../../test-integration/receipts/recursive-context-length-retry.json)
retain exact source/executable hashes and do not claim an unfiltered full-Length
pass. The build log is `dist-newstyle/priority-final-build-v9.log`.

Leant's [bounded behavioral simplification](https://github.com/VladimirReshetnikov/Leant/blob/main/docs/reports/2026-09-07-bounded-behavioral-simplification.md)
is published and passes live three-engine acceptance plus independent replay.
It preserves false and inconclusive outcomes. The report retains the incomplete
full boundary receipt separately from its passing unchanged focused retry.

## Conditional root-Given checkpoint

The [Djinn conditional-Given increment](2026-09-07-djinn-conditional-givens.md)
adds actual constrained local/global applications under exact root dictionaries,
complete correlated kind checks, checked dictionary erasure, and shared search
budgets. All 92 private tests and the expanded sixteen-test contextual target
pass. Ten production budget tests include zero/tiny allowances and a measured
duplicate cutoff. Qualified sources whose class methods were omitted no longer
receive false negative evidence; unconstrained refutations are preserved.
Strict builds pass with `-Werror -j1`. The
[aggregate receipt](../../test-integration/receipts/conditional-givens-checkpoint.json)
records 2,168 passing tests across twelve complete affected suites, including
all 432 Length tests. These are complementary complete-suite runs with
unchanged production source, not one unfiltered twelve-suite invocation.
The first run exposed stale expectations of unsound method-based refutations;
only three test files changed before complete reruns of all 133 Djinn unit,
139 facade, and 24 historical Djinn CLI tests passed. The detailed report
and receipts retain both runs and their exact source/executable identities.

The Lean fold diagnostic now accepts native map and independently checks the
exact append/length terms already present in the debug stream. Request failures
still prevent their live acceptance. A subsequent five-second direct control
reproduces the failure without synthesis, so startup/request variability must
be resolved before attributing it to search. Leant's published dependency remains
`922c5558`; integration of this contextual increment is still required.

## Guarded nested-Given checkpoint

The [guarded nested-Given increment](2026-09-07-djinn-nested-givens.md) now
accepts forced monomorphic nested local/global calls and conservatively
rejects ambiguous dictionary ownership through residual results, constructor
fields and aliases. Equal active dictionaries and nested forall/Given
interaction remain explicitly unsupported; no richer source evidence is
inferred from their compatibility expressions.

The final complete private and facade suites pass all 106 and 147 tests. The
facade suite takes 234.23 seconds and includes 24 contextual cases and independent
GHC witnesses. Together with ten complete v2 suites, the
[checkpoint receipt](../../test-integration/receipts/guarded-nested-givens-checkpoint.json)
records 1,048 Tasty cases, 700 Church cases and 100 scope queries across twelve
complementary complete suites. Production source stayed identical. Five fixture
lines were corrected to remove unsupported explicit datatype/synonym parameter
kinds; the initial setup failures and final complete reruns are retained.
That checkpoint's 100-test CLI suite includes the accepted first/best/all
elaboration repairs; the later 101-test timeout acceptance is recorded above.
Leant's working dependency contains the guard, but full integration acceptance
remains required.

## Remaining work

- Integrate priority 3's accepted guarded increment into Leant. Its prepared
  exact displayed-Variant/graph/renderer/engine observations and direct hashed
  replay kernel pass 14 pure harness controls; 12 extended-corpus and six
  partial pure controls also pass. The new strict build, 52 focused tests,
  two Djinn identity ordinary/`where` full-type replays, and corrected complete
  678-test suite pass. The contextual matrix accepts 35/39 cells across the two
  recorded runs. Resolve the four ordinary Exference/Both forwarding/local-Given
  timeouts within the existing gate; all nine named-`where` positives, three
  actual False controls and 18 metadata refusals already pass. Retain exact
  full-type/payload replay and each accepted variant's own provenance.
- Retain priority 2's accepted sixteen-cell Haskell matrix, 24-cell Lean replay
  and historical 647-test integration receipt. Diagnose and repair live Lean
  verification of already-found append/length candidates under unchanged limits,
  with exact replay, actual False controls and environment-preserving recovery.
  Broaden supplied tree folds and accumulator programs after that gate passes.
- Execute priority 4's broader behavioral corpus and all nineteen supplied-default
  cases. Supplied-default `head` now passes live synthesis, exact GHC
  execution, and actual False controls in both Haskell engines. Most expanded
  cells remain open. Both partial oracle preflights now pass: 20 Lean files with 491 axiom
  inventories (487 empty, four allowlisted observer/proofs) and 63 Haskell
  controls. All 736 observations remain; expanded live synthesis acceptance
  remains open. Keep each engine/operation result separate
  and retain bounded Lean simplification as a regression.
- After bounded Lean publication, extend priority 3 with one fully described
  global contextual provider in the existing `Type 0` fragment under the current
  non-overlap guard. Independently, carry actual proof-selected introduction/slot
  transport and require a forced equal-predicate outer/inner fixture before
  relaxing that guard. Haskell and Lean replay must observe differing payload
  behavior under faithful scoped capture; graph identity or successful GHC
  compilation alone is insufficient. Then extend nested forall/Given interaction,
  partial constrained use, exact provider/universe metadata, methods, conditional
  instances and superclasses
  from concrete missing programs.
- Run the appropriate complete affected suites and live compiler/kernel
  acceptance, update capability documentation, and integrate both repositories.

The goal remains all four priorities; this checkpoint does not replace it with
the currently implemented rendering path.
