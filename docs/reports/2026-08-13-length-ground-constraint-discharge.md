# Length occurrence-specific ground constraint discharge

Date: 2026-08-13

## Outcome and scope

Djex Length can now use an
`AssumedConstraintConditionalProviderSummary` at one narrowly authorized
candidate occurrence. The summary still retains the exact closed source scheme
with a nonempty leading constraint context and the conditional trust classifier.
It does not become a globally usable provider law. Length authorizes only the
final visible-type-application node of the same opaque associated certificate
row after its complete protected prefix has passed a graph-wide audit and every
activated obligation has an independently checked ground-discharge receipt.

The legacy `AssumedProviderSummary` boundary is unchanged: its source scheme
must be closed and leading-context-free, its constructor behavior is preserved,
and legacy-only provider inventory, semantic inventory, session, candidate, and
concrete-encoding identities retain their exact bytes.

The standalone `evaluateLengthProviderApplication` entrance remains detached
from candidate authority. It rejects conditional trust before inspecting
argument arity, roles, or values. Whole-problem evaluation can replay a
candidate which was already authorized while its exact Length problem was
sealed, but evaluation neither performs class resolution nor recreates a
receipt.

## Conditional provider trust contract

The conditional source is admitted only when its claimed provider name and
full scheme alpha-match the exact closed scheme recovered from the retained
inventory and that source scheme has a nonempty leading constraint context. The
checked summary stores the source-derived scheme, roles, normalized transfer,
and `AssumedProviderLawConditionalOnConstraintDischarge`; it never stores the
caller's scheme as authority.

The assumed provider law is explicitly uniform over independently admitted
dictionary evidence as well as over admitted closed instantiations. Exference's
certificate retains the activated obligations but does not retain which given
or instance its checker selected. Length therefore does not claim to replay the
frontend's dictionary path. Instead it independently proves each static ground
obligation under the exact session inventory. That independent proof justifies
using the transfer only because the assumed law is dictionary-evidence-uniform.

This assumption is behavioral-use policy, not a change to the retained summary
identity. The marker
`provider-law-uniform-over-dictionary-evidence/v1` is present in conditional
session policies 8/9/10 and in candidate v3. It is deliberately absent from the
provider-summary field, provider inventory v3, and semantic inventory v2, whose
conditional retention bytes remain exact.

## Session-owned resolver authority

When at least one retained provider summary is conditional, Length attempts to
seal the package-private class-resolution environment from the session's exact
inventory and `defaultClassResolutionLimits`. Compatibility session sealing
does not turn a resolver-sealing failure into a new public session failure: it
retains the resolver as unavailable. A candidate which actually needs it then
fails closed with the sanitized
`LengthAssociatedClassResolverUnavailable` reason.

An admitted query is restricted to the resolver's static closed-world policy:

- its class must be declared by the exact retained inventory;
- every referenced type synonym is rejected;
- nested `ForallType` and free variables are rejected;
- instance matching and substitution are first-order;
- the constraint must be ground and kind-correct; and
- there are no query givens.

Proof search remains bounded by the session-fingerprinted default declaration,
table, type/kind, overlap, depth, and node limits. A successful opaque
`CheckedConstraintDischarge` remains associated with that exact retained
resolver environment and canonical goal. The public problem surface exports no
resolver environment, proof tree, raw goal, instance choice, or receipt.

Length's no-givens rule describes this independent static discharge only. It
does not make a claim about the dictionary path originally used by Exference;
that path is not retained and is irrelevant to the provider transfer precisely
under the dictionary-evidence-uniform assumed law. Z3 is never queried for and
never supplies dictionary evidence.

## Row classification, prefix audit, and discharge

The existing problem gates remain ahead of associated carrier authorization:
policy association, contract resealing, and residual rejection still precede
carrier demand. For a nonempty associated carrier, Length freshly re-seals and
fingerprints the complete graph and semantic rows before inspecting
Length-specific row authority. Graph limits, schema failures, and fingerprint
byte limits therefore keep their earlier precedence.

Length then classifies every rooted row before it performs proof search. Every
row must have an exact inventory owner and full alpha-exact source scheme, must
not be owned by a modeled zero/step constructor, and must resolve to the checked
provider summary co-sealed by the same session. A legacy provider row still
requires every activated-obligation list to be empty. A conditional row instead
requires a nonempty activated context; absence is the distinct
`LengthProblemAssociatedCertificateConditionalObligationsMissing` failure.

Before any obligation is discharged, Length audits all conditional chains
against the complete graph, including dead nodes. For each row:

- the provider base is protected as a sentinel;
- each non-final visible-application result is a protected intermediate;
- no protected node may be the graph root; and
- every protected node's complete incoming-parent set must equal its certified
  function edge.

The base sentinel can carry the row's authorization through preflight but still
fails if interpreted directly. A shared base or intermediate, an additional
incoming edge, or a protected root fails before proof search. This prevents one
receipt set from authorizing a direct, partial, or shared-prefix occurrence.

After the complete audit succeeds, obligations are discharged in canonical
rooted-row, source-step, and obligation order. Each conditional authorization
retains the exact checked provider summary and all successful opaque receipts.
Only the row's final receipt-bearing visible-application node enters the final
authorization map and may invoke the assumed transfer. Another occurrence of
the same provider name or constraint has no authority unless its own complete
row is independently discharged. A direct/base/partial
occurrence therefore continues to return
`LengthProblemConditionalProviderRequiresDischarge`.

## Sanitized public diagnostics

The public `Language.Haskell.Synthesis.Semantic.Length.Problem` module exports
three closed diagnostic types for the new associated boundary:

| Type | Public constructors |
| --- | --- |
| `LengthAssociatedConstraintDischargeReason` | resolver unavailable, constraint not ground, query rejected, evidence missing, derived constraint rejected, or proof limit exceeded |
| `LengthAssociatedProviderChainSite` | provider base or canonical intermediate source-step ordinal |
| `LengthAssociatedProviderChainReason` | protected node is root or has an unexpected incoming edge |

The matching problem failures are:

- `LengthProblemAssociatedCertificateConditionalObligationsMissing`, with the
  provider name and canonical rooted-row ordinal;
- `LengthProblemAssociatedCertificateConstraintDischargeRejected`, with the
  provider name, canonical row/step/obligation ordinals, and one sanitized
  discharge reason; and
- `LengthProblemAssociatedCertificateProtectedChainRejected`, with the provider
  name, canonical row ordinal, sanitized base/intermediate site, and closed
  chain reason.

Only these new associated discharge and protected-chain failures make the
sanitization claim. Existing `LengthProblemError` constructors retain their
established payload contracts. The new failures expose no raw constraint or
type, certificate/slot/node/occurrence coordinate, resolver diagnostic, proof,
instance, dictionary, or discharge receipt.

## Identity compatibility

The new behavioral authority is additive and conditional:

| Identity | Legacy / obligation-free path | Conditional ground-discharge path |
| --- | --- | --- |
| provider inventory | exact v2 | exact conditional-retention v3 |
| semantic inventory | exact v1 | exact conditional-retention v2 |
| session encoding policy | exact 5/6/7 for ordinary, mixed-role, and exact-case modes | 8/9/10 for the same three modes |
| concrete encoding | exact 1/2/3 | 4/5/6 |
| candidate | exact v1 plain or v2 obligation-free associated | v3 ground-discharged associated |
| carrier-aware graph | v2 | v2, unchanged |
| complete problem | schema/version v1 | schema/version v1, transitively binding the new component identities |
| SMT query | existing schema/version | unchanged schema/version, transitively binding the exact problem |

The provider inventory v3 and semantic inventory v2 fields are byte-for-byte
the preceding retention-only checkpoint. Behavioral markers begin only where
the authority can be used. Conditional session policies bind exact-inventory
resolution, alias-free first-order ground queries, default resolver limits, no
query givens or Z3 authority, the dictionary-uniform assumed law, final receipt
node authority, and prefix rejection. Candidate v3 binds the inventory-bound
receipt requirement,
`provider-law-uniform-over-dictionary-evidence/v1`, occurrence-specific final
authority, protected-prefix policy, and
`static-discharge-without-givens-or-z3/v1`.

No certificate, slot, node, occurrence, constraint, proof, instance, or raw
receipt coordinate is added to a semantic key. Complete-problem and query
schemas need no new field because they already compose the exact inventory,
session policy, concrete encoding, candidate, and problem identities.

## Behavioral and Z3 boundary

A successful ground discharge proves only that the activated static constraint
is derivable under the exact bounded resolver environment. It does not prove
candidate completeness, source expression inhabitance, purity, totality,
strictness, provider implementation identity, or the provider transfer itself.
The transfer remains an assumed law, now additionally conditional on the
dictionary-evidence-uniform contract.

The Length SMT boundary continues to encode only the already sealed behavioral
problem. Solver observations remain heuristic until exact independent Length
model replay creates problem-relative evidence. Neither `sat`, `unsat`, a model,
nor successful counterexample replay is or can be Haskell dictionary evidence.

## Public and validation surface

The curated facade exports the three sanitized reason/site types and all new
`LengthProblemError` constructors while keeping resolver environments, raw
resolver errors, proofs, receipts, and the session's private resolver projection
opaque. Facade smoke constructs every new reason, site, and error shape and the
abstraction probe pins the resolver projection outside the public surface.

Focused production tests own the deeper success, precedence, graph-audit,
receipt-association, strictness, limit, identity-compatibility, and replay
matrix. This report records the trust boundary and public contract; it does not
treat facade construction alone as evidence of the implementation properties.
