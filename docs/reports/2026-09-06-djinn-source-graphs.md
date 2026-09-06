# Djinn source-typed evidence acceptance

This report separates the source graph implementation from compiler checks,
runtime checks, and downstream Lean replay. The [implementation guide](../source-typed-evidence-graph.md)
describes its authority, scope, resource limits, and deliberately unsupported cases.

**Canonical Djex and downstream Leant integration acceptance have passed
within the scope recorded below.** Producer, consumer, CLI, corpus and
behavioral checks are complete. Native Windows Length checks confirm the
existing explicit policy and platform refusals; they do not claim successful
solver-backed filtering through an unsupported configuration route.

## Canonical Djex checks

All builds use GHC 9.12.4 and serialized Cabal jobs on native Windows. Test
executables run serially under an owned process guard. Local raw receipts are
under `dist-newstyle/source-graph-acceptance/`; each guarded run records its
executable hash, selected source hashes, raw output hashes, exit status, and
elapsed time. These directories are ignored artifacts, not tracked fixtures.

| Gate | Result | Local receipt directory |
| --- | --- | --- |
| Shared graph foundation | 465 tests passed | `foundation-first` |
| Graph fingerprint resealing | 10 tests passed | `fingerprint-first` |
| Certificate association | 94 tests passed | `certificate-first` |
| Djinn proof/search unit suite | 122 tests passed | `unit-final` |
| Public Djinn source graph producer | 55 tests passed | `producer-final` |
| Private source checker and kind ownership | 43 tests passed | `private-final` |
| Public API and abstraction boundaries | 38 tests passed | `api-final` |
| Shared Length and live-worker lifecycle | 432 tests passed | `length-repaired` |
| CLI integration | 97 tests passed | `cli-complete` |
| Church signature corpus | 350 selected Djinn graphs; 700 combined GHC checks passed | `church-final` |
| Adversarial rank-N scopes | 100 queries passed; all 76 positive terms GHC-checked | `scope-final` |
| Named Haskell behavioral corpus | All six Djinn operations, false-oracle rejection and 23 independent execution assertions passed | `haskell-behavior-six` |

Each completed run in the table exited successfully. The current producer,
checker, API, unit, Church, scope, Length and behavioral receipts also have
their captured stdout/stderr hashes independently checked against the saved
files. The repaired Length run completed in 65.95 process seconds and the
complete CLI run in 64.13 process seconds. Earlier partial CLI runs are not
credited as substitutes for the complete run.

The shared foundation, fingerprint and certificate rows retain their earlier
successful receipts. The foundation receipt predates later private
source-checker completion changes; its `TypedGenerated.hs` hash is unchanged.
It validates the shared graph layer, not the later producer implementation.
The final producer, private checker and whole-corpus runs cover that
implementation, including the exact intrinsic List declaration adapter.

The Church run requires a graph for the **selected Djinn candidate** before
rendering, checks its exact clause erasure and full source root, then compiles
that same implementation against the expanded and original acceptance
signatures. Exference retains its existing compiler route. This is 350 new
selected-candidate graph checks, not graph coverage of every search alternative.
The corpus consists of 315 total signatures, 16 integer-provider cases and 19
explicit partial/default cases per engine. A matching type alone does not
establish the behavior of the source operation.

The separate behavioral run synthesized `not`, `swap`, `map`, `append`,
`reverse` and `filter` through the public named Djinn command. It used
`interleave`, balanced ranking, a 65,536-candidate window and a 500,000-choice
budget, with no named synthesis providers. All six exact emitted definitions
compiled in isolated candidate modules and passed their 626 finite
observations. The 23 execution assertions comprise those six candidate
checks, six reference-oracle sanity controls, ten deliberately wrong
well-typed controls and the specialized swap control. A separate false
predicate rejected its observed candidate and emitted no successful result.
This is finite behavioral evidence, not a universal correctness proof. Eleven
earlier candidate evaluations reported errors and were not accepted; the
receipt preserves those observations separately from the six successes.
The synthesis executable remained unchanged throughout the run, with SHA-256
`1682dfa30bddbd956997bf7ca4c7f9e02551cae0cb884d1b06ced557a8aff765`.

The private recursive List fixture checks its exact supplied tail/default-nil
clause, interprets the checked source graph under the declared zero/step
policy, and independently replays the identity-length counterexample `3 -> 2`.
It rejects ordinary case policy and a changed recursive-field schema. Public
Djinn recursive elimination remains unchanged; public tests separately cover
nonrecursive cases, recursive-container forwarding, exact intrinsic nil and
cons introduction, and refusal to synthesize an element from an arbitrary
list through the historical recursive-input abstraction.

## Corrections found by integration

An artificial 20-second per-test cap initially interrupted two existing slow
Djinn unit cases. The previous full-suite receipt showed those cases taking
approximately 48 and 28 seconds. The complete 121-test rerun used the previous
300-second process guard and passed in 108.05 seconds. The current final
122-test suite also passed under that guard, in 101.50 process seconds.

The first direct CLI run lacked Cabal's `djex_datadir` environment setting;
the corrected invocation points it at the source checkout. The resulting
frontend run exposed a real Length handoff defect: implicit Haskell universals
were presented as flexible source variables, so the existing root-opening
guard correctly refused assessment. It also exposed the omission of the
checked intrinsic list declaration from an otherwise empty Djinn scope.
The frontend now closes implicit source universals once for both the request
and its Length contract, preserving the exact graph and the rigid-opening
checks. List admission now retains only the checked canonical declaration and
its exact constructor shapes; nominal visibility restrictions remain intact.
The completed producer, Length and CLI suites cover these graph, consumer and
frontend boundaries. Implicit and explicit universals produce the same actual
scalar, pair and forced-nil definitions. Impossible contracts reject both
engines' candidates. The separate after-refutation control retains an actual
Cons candidate only with the precise invalid-counterexample diagnostic when
the deliberately always-SAT fake supplies a model that is not a counterexample.
That unassessed candidate is not credited as successful behavioral filtering.

The existing module-reload fixture also invoked POSIX `cp` through the Windows
command shell. `cmd /c where cp` confirmed that executable is unavailable on
this host. The fixture now uses native Windows copying on Windows, retains
the reload assertions and checks that the fixture mutation succeeded. Its
POSIX path and production reload behavior remain unchanged.

## Completed downstream integration checks

The Leant implementation is pinned to commit
`2d400ed8178119c922d644b509d504c097e944ba`, with canonical Djex checkpoint
`9ba199a0f07736693c06104859dd02a42dcccdcb` retained in `lib/Djex`.
Its serial GHC 9.12.4 builds completed successfully: the executable and test
build used `cabal build leant:exe:leant leant:test:leant-synth-tests --jobs=1`,
followed by a test-only rebuild for the final renderer-policy assertion.
Those build outcomes are recorded in the coordinating agent's tool receipts
(sessions 22958 and 28022, both exit 0); no separate build log is retained.

The independently auditable runtime receipts are under Leant's
`dist-newstyle/source-graph-acceptance/`:

| Gate | Result | Process time | Local receipt directory |
| --- | --- | --- | --- |
| Focused source ownership, rendering and Length | 32 tests passed | 5.37 seconds | `unit-focused-third` |
| Complete Leant synthesis unit suite | 608 tests passed | 566.76 seconds | `unit-full-first` |
| Djinn Lean corpus through source-typed rendering | 350 exact terms kernel-checked; all 350 axiom-free | 164.42 seconds | `corpus-first` and `lean-djinn-350` |
| Compact rank-N and provider fixtures | 81 exact terms kernel-checked; all three goldens matched | 269.81 seconds | `compact-first` and `compact` |
| Fresh named Church `not`, Djinn and Both | Two exact candidates and predicates kernel-checked; actual false-oracle rejection | 79.75 / 66.21 seconds | `followups-first/behavior-not-djinn` and `behavior-not-both`, with their `guard-not-*` receipts |
| Native Windows Length refusal controls | Four disabled-policy refusals and the expected configuration-platform refusal | 4.82 / 0.14 seconds | `followups-first/native-length-refusals` |

The focused tests are included in the full suite, not 32 additional tests.
Both processes exited 0 without a timeout. Each receipt's eight input hashes
and two stdout/stderr hashes match the retained files, including the actual
Engine, Length handoff, counterexample bank, tests and vendored source checker.
The test executable has SHA-256
`c8e34ba69d3218c98d8bba25ee21a79dbdd11cff57e15fab6d1040b064758456`;
the Leant executable has SHA-256
`bc041b71eb86e7787e5bd4461635702ed34fdf97804c7c26d5e9994d4b7e15fc`.

Leant now closes implicit variables that transport opaque Lean atoms before
constructing the Djinn request. It retains the original translation, renderer
maps and source/search provenance, and applies the same closure when matching
the request and sealing its Length contract. The source checker supplies its
own rigid openings; no existing graph is relabeled. Exference's request
boundary remains unchanged. Scalar and pair live observations are executed
and independently replayed within the same exact query continuation, so an
engine switch cannot transfer another query's observation or bank receipt.

The constructor regression checks each actual nil rendering against its own
retained source identity and ordinal. The existing exact zero/step policy
admits that ordinal and independently replays result length zero. When the
rendered group has multiple variants, the historical singleton policy must
still refuse it with `LengthHandoffRendererNotUnique`; the positive check
does not weaken that restriction or claim recursive-input elimination.

The fresh corpus used Lean 4.32.0, Djinn, a one-candidate search and
verification window, 4,096 steps and a 30-second synthesis timeout. Its saved
transcript records **350 `typed-candidate-rendered`, 350
`lean-variant-attempted` and 350 `lean-candidate-verified` observations**:
each query has exactly one of each, and none records a legacy fallback.
Because each query observed one candidate group, this establishes that every
displayed result came through Djinn's own graph, rather than merely showing
that its compatibility term type-checked in Lean.

The audit matched the manifest and canonical Church source hashes, all 350
translated cases, the exact acknowledged command sequence, the captured
candidate blocks and the complete saved kernel source. Live synthesis and
kernel replay both exited 0, all 350 exact declarations have empty axiom
inventories, and the Leant executable remained unchanged. The corpus wrapper's
eight input hashes and two output hashes also match. The result receipt is
`lean-djinn-350/results.json`, SHA-256
`db487e81534c97560a6f25a1cf13f4bb24643b204ca2a331b6a146073cf5bc29`.
Coverage retains the 315 total, 16 integer-provider and 19 explicit-default
cases; quantified Lean type universes are independently inferred. This does
not turn Lean's predicative `Type` hierarchy into Haskell impredicativity or
establish the source operations' behavior from their signatures.

The compact run covered `synth-church-rankn` (69 queries),
`synth-church-layered-providers` (six) and `synth-church-providers` (six):
27 queries each under Djinn, Exference and Both. All three live processes and
kernel replays exited 0; validation passed before golden comparison, and the
existing goldens matched without changes. The 69 rank-N terms have empty
axiom inventories. The other 12 terms depend on exactly the fixture-declared
opaque primitives: `Token` and `chosen` for each provider family, or `Seed`,
`seed`, `G2`/`G3` and the matching `maker2`/`maker3` for the layered family.
They are checked relative to those premises, not counted as axiom-free terms.

The compact audit reparsed the complete acknowledged command sequences,
matched every displayed term to its saved replay source, and verified all
34 source, capture, kernel, golden and wrapper hash comparisons. Each engine's
eight- and twelve-slot provider vectors are complete and ordered; every slot
retains its distinct quantified arrow type. The executable remained the same
`bc041b71…` build. `compact/results.json` has SHA-256
`58757d7b9f45277def804a7f0667723b54d9a624d49e5932abf2b426b4ce3046`.
These receipts establish exact replay and golden compatibility for this
81-query selection; the separate 350-case capture above supplies the explicit
typed-rendering observations.

The final behavioral follow-ups each synthesized the exact Church `not` term
`fun f _ x y => f _ y x`. Djinn used an explicit 65,536-candidate/verification
window, 500,000 choices and 120 seconds; Both used 256, 100,000 and 30 seconds.
Both selected `interleave`, balanced ranking, one shown result and 100,000
Exference steps. Each isolated replay checked the exact candidate and its
supplied predicate with two empty axiom inventories. A separate module
rechecked the existing 33 oracle-control inventories; those repeated controls
are not additional synthesized operations. These are two fresh `not` cells,
with live and kernel processes exiting 0. The false predicate actually
falsified five observed Djinn candidates and 128 Both candidates, emitted no
successes and reported no inconclusive decisions. The positive queries also
reported no inconclusive decisions.

The follow-up audit matched all 14 frozen input pins before and after the run,
reparsed the acknowledged command sequences and exact candidate blocks,
regenerated the isolated replay modules, and verified 47 report, module and
process input/output hash comparisons. `followups-first/results.json` records
all three completed jobs and unchanged inputs, with SHA-256
`88eebe9503891f86bf252d9e163703db2b4d86dcad584336fb9b8988628efafd`.

## Platform and authority boundaries

Leant must retain the graph's actual engine owner through rendering, scalar or
pair Length handoff, and counterexample-bank replay. Equal rendered terms do
not permit substituting another engine's graph. The completed unit, corpus
and focused behavioral gates exercise that authority at their recorded
boundaries and limits; earlier receipts are not substituted for these runs.

The native control reached four exact
`LengthAssessmentFilteringRequiresActivatedPolicy` refusals for scalar and
pair requests under Djinn and Both, without displaying a candidate. Its
separate configured startup exited with the expected status 1 and
`LengthRankingConfigurationFilePlatformUnsupported False`, before reaching
the REPL or reading the deliberately absent configuration file. That expected
refusal passes the control; it is not solver success. Pure checked Length
replay and native Lean kernel replay likewise do not establish successful
public solver-backed filtering through that unavailable configuration route.
The existing supported POSIX configuration/launcher boundary remains distinct.

No publication result is claimed by this report.
