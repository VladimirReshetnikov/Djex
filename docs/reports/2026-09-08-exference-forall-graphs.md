# Exference forall graphs: exact source scopes and lambda projection

Accepted 2026-09-08 with a strict build, thirteen complete test suites,
Church/scope regressions and exact source-witness replay. The
[receipt](../../test-church/receipts/priority4-exference-forall-graphs.json)
keeps the live behavioral miss and earlier failures separate.

Exference now retains the original root quantifiers and context-free nested
forall introductions in its independently checked term graph. This preserves
the source evidence needed to annotate a generated polymorphic argument or
result. Previously, these checked candidates could reach the compatibility
renderer while their graph was marked unavailable, preventing a graph-guided
GHC elaboration retry.

The root graph contains the original universally closed request. Its opening
witnesses carry the original binder order and exact selected rigid identities.
Consumers must compare that source type, rather than close free variables from
an already opened result. Length accepts the request's canonical shared
closure while retaining its previous direct and rigid-opening cases. Tests
cover source traversal order differing from variable order and free variables
added to a request that already begins with a forall.

Polymorphic constructors retain a closed scheme derived from their checked
datatype result and ordered fields. The corresponding unique flat binding
must match that source. Actual selections appear on implicit type-application
edges; a root-local rigid never becomes part of a global declaration. This
does not manufacture a specified-binder sidecar or visible-application
certificate. Missing or mismatched constructor metadata still cannot grant
that authority.

Compatibility syntax has an explicit projection policy. Existing graph
sealers retain their merged-lambda default, including Djinn's grouping across
erased forall evidence. Exference selects `PreserveLambdaBoundaries`, retaining
its original singleton lambda nodes. Both choices run the same type, scope,
occurrence and syntax checks; the engine still requires exact equality with
its own compatibility expression.

The additive public entrances are `sealTermGraphWithProjection`,
`sealTermGraphWithContextAndProjection`, and `resealTermGraph`, with
`TermGraphProjectionStyle` selecting the syntax policy. The opaque graph
retains the selection. Fresh fingerprint checks, including Length's graph
admission, preserve its actual projection cost. A nested expression that costs
five nodes under Exference's grouping cannot pass a four-node quota by being
silently regrouped during resealing. Certificate-associated graphs use the
same policy and retain their independent occurrence/certificate checks.

## Checked witness and live search

The exact provider-free `maybeEither` witness retains five distinct forall
introductions, its original full type and exact compatibility expression.
Its typed Haskell output passes independent GHC 9.12.4 compilation and all
seven original finite observations within the two-second evaluation guard.
This is acceptance of the source checker, graph and renderer. The witness
was supplied to the checker, so it is not a synthesized success.

A separate fresh public query retains the original full signature, predicate,
empty provider inventory, 256-candidate window, 100000-step/choice bounds and
8192 queue bound. It still finds no matching implementation: all 256 checks
are false, with zero errors or timeouts. Five same-candidate elaboration
retries repair the earlier compiler failures without replacing their original
observation slots. The three retained samples now have graphs and evaluate
false. Both independent oracle controls and the actual False query pass.
The positive search process takes 13.85 seconds; no candidate is accepted.

The initial witness probe failed before execution because unqualified package
flags exposed two local Tasty installations. Its replacement uses the exact
package unit IDs of the current Cabal test component plus the probe's unique
support dependencies. The source, requested type and predicate are identical;
the earlier setup failure remains separate from the successful replay.

## Aggregate regression acceptance

The final strict build uses GHC 9.12.4, `-Werror` and one build job. All
**2119 tests across thirteen complete suites** pass, matching each executable's
actual unique test inventory. This includes 82 private Exference tests, all
436 Length tests, all 147 facade tests, all 101 CLI tests, all 503 shared
synthesis tests, and both Djinn source-graph suites. Source, build-plan and
helper executable hashes remain unchanged throughout the run and were
rechecked before the receipt was written.

The independent whole-corpus run synthesizes and GHC-checks all **350 Church
signatures in each engine**, followed by all **50 scope probes in each engine**.
Negative scope probes mean no candidate escaped the recorded bounded search;
they are not inhabitation decisions. Corpus/type acceptance remains distinct
from the finite behavioral matrix.

The [saved witness sources](../../test-church/receipts/priority4-exference-forall-witness/README.md)
retain the exact original type, rendered implementation, graph, compatibility
expression and predicate. They permit an independent compiler/predicate
replay without adding the witness as a synthesis provider.

The first full eleven-suite attempt is retained as failure history. It exposed
three private projection failures, two constructor-dependent Length failures,
four constructor-dependent facade failures and a projection-dependent API
fallback. The CLI provider-selection fixture also improved from one repaired
candidate to three: the two formerly unsupported nested forall candidates now
pass actual GHC checking. Its updated assertions retain exact selected-source
replay. The standalone CLI queue check validates the emitted numeric final
queue size without pinning it to a historical provider inventory.

The next thirteen-suite attempt passes Length, facade, CLI, shared graph and
both Djinn graph suites. It exposes a monomorphic-constructor corner case:
the shared closure helper adds an empty forall even when no variables need
binding. The constructor helper now removes only that newly synthesized
empty-binder, empty-context wrapper, preserving a monomorphic arrow directly.
Two direct monomorphic controls supplement the unchanged production case
test. An API fixture is updated to require the now-supported nested graph,
both introduction witnesses and the unchanged validated-candidate status.

This increment addresses a concrete graph/elaboration gap. It does not change
search budgets, prove universal behavior or establish that a known witness
appears within a particular search window. The full thirteen extended Church
operations and nineteen explicit-default counterparts remain required in both
Haskell engines and all three Lean modes.
