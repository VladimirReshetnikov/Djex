# Djinn source-typed evidence acceptance

This report separates the source graph implementation from compiler checks,
runtime checks, and downstream Lean replay. The [implementation guide](../source-typed-evidence-graph.md)
describes its authority, scope, resource limits, and deliberately unsupported cases.

**Canonical Djex acceptance has passed.** The producer, consumer, CLI, corpus
and behavioral checks below have finished. Downstream Leant acceptance remains
pending.

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

## Downstream boundaries

Leant must retain the graph's actual engine owner through rendering, scalar or
pair Length handoff, and counterexample-bank replay. Equal rendered terms do
not permit substituting another engine's graph. Downstream compilation, unit
tests and fresh exact emitted-term Lean replay are separate acceptance gates.

Native Windows deliberately rejects Leant Length configuration-file activation
with `LengthFilePlatformUnsupported`. Pure checked Length replay and native
Lean kernel replay do not establish successful public solver-backed filtering
through that unavailable configuration route. The existing supported POSIX
configuration/launcher boundary remains distinct.

No publication result is claimed by this report.
