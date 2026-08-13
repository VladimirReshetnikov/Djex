# Length associated-provider certificate consumption

Date: 2026-08-13

## Outcome and scope

Djex Length now consumes the opaque certificate-association branch retained by
an engine-owned `TypedCandidate`. The checkpoint is intentionally limited to
exact, obligation-free visible applications of checked Length providers. It
does not admit associated modeled constructors, discharge contextual
obligations, or make a stamped bare graph authoritative.

The public problem-sealer signatures and ordinary interpretation behavior are
unchanged. The public error vocabulary gains sanitized failures for a missing
inventory owner, non-alpha-exact full source scheme, activated obligations,
modeled-constructor use, and missing checked provider summary.

## Atomic carrier flow and precedence

After the existing policy, contract-resealing, and residual-constraint gates,
the problem sealer eliminates the private `TypedCandidate` graph carrier:

1. an unavailable graph returns the existing typed-graph failure;
2. a plain graph uses the existing fresh reseal and v1 graph fingerprint; and
3. an associated graph is freshly resealed and fingerprinted as one opaque
   carrier under the session-selected `TypeStructure` before any Length row
   authorization.

The third order is deliberate. Exact-case sessions supply their
inventory-bound constructor schemas; ordinary sessions supply the shared
structure. Graph limits, schema validation, and the fingerprint byte limit
therefore fail before owner, scheme, obligation, or provider-summary checks.
Only after the carrier-aware fingerprint succeeds does Length traverse every
association row in rooted structural order. Only after that traversal succeeds
does Length extract a standalone graph view for interpretation. Contract and
residual errors still precede all carrier demand.

An empty association literally takes the plain authority branch after its
delegating v1 graph fingerprint. Its graph fingerprint, candidate fingerprint,
concrete encoding, and complete problem identity are exactly the plain values.

## Provider-only authorization

For each nonempty rooted row Length requires:

- an owner present in the session's exact source inventory;
- a retained full source scheme alpha-equivalent to the exact inventory term
  scheme, rather than merely instantiationally admissible;
- zero activated obligations in every source-order plan step; and
- a checked Length provider summary for that owner.

The checked session co-seals every provider summary from its normalized exact
`inventoryTermScheme`. Once the association row has matched that same source
scheme, summary-scheme equality is an opaque session invariant; only summary
presence remains a dynamic candidate check. The modeled zero and step
constructors are reserved from providers and are explicitly rejected as
certificate owners in this checkpoint, even under the exact-case policy.
Exact structural equality with inventory data does not prove the candidate's
causal source, declaration provenance, or the implementation behind a provider
name.

The obligation rule does not prove that a dictionary was solved, identify an
instance, or delegate discharge to Z3. Length simply refuses any row whose
checked source step retained a nonempty activated-obligation list. Solver
reports remain heuristic until the existing exact problem-bound replay
succeeds; neither association coordinates nor this admission create behavioral
evidence.

## Public diagnostics and authority limits

Associated-row failures expose only the owner `Name`, canonical rooted-row
ordinal, canonical source-step ordinal where applicable, and bounded
obligation count. They never expose certificate IDs, raw source slots, graph
node or occurrence IDs, constraint payloads, or type trees. Rooted order fixes
the first reported row independently of caller origin-table order.

The opaque carrier establishes structural agreement between checked plans and
graph occurrences. Length additionally establishes exact source-inventory and
provider-summary membership for this candidate. Neither layer proves source
declaration provenance, binder-kind history, instance/discharge identity,
candidate completeness, solver correctness, or source-language behavior. A
public graph projection loses the carrier and remains rejected by
`fingerprintSharedTermGraph` and by the plain Length path.

## Canonical identities

A nonempty authorized carrier uses the carrier-aware graph v2 identity. Length
then emits candidate fingerprint v2 with the explicit policy fields
`opaque-associated-certificate/v1` and
`activated-obligations-empty/v1`. Certificate, slot, node, occurrence, and
caller-row allocation never enter either identity. Coordinate-renumbered
carriers therefore share the same candidate and complete-problem keys.

Plain graphs and empty carriers retain the exact candidate v1 bytes. In
contrast, the common session encoding policy deliberately changes globally:

- legacy/all-observed case rejection advances from version 2 to 5;
- mixed-role case rejection advances from version 3 to 6; and
- exact zero/step cases advance from version 4 to 7.

The policy replaces detached-certificate rejection with separate
`reject-detached-certified-visible-application/v1` and
`admit-exact-obligation-free-associated-provider-visible-application/v1`
markers. Public interpretation signatures and legacy-vs-explicit role
equivalences remain, but historical session bytes and all containing session,
query, protocol, and problem keys must be treated as invalidated caches.

## Validation

The focused strict Length suite passes all 234 tests. The associated-candidate
matrix covers one-slot provider success, coordinate-renumbering invariance,
empty-carrier/plain parity, every sanitized authority failure, contextual
obligation rejection, modeled-constructor rejection, rooted multi-row failure
order, the exact full-scheme counterexample to the former instantiational
match, residual-before-carrier laziness, graph-byte-before-authorization
precedence, exact-case schema composition, and a real Exference exact-scheme
query carried through the private associated branch into Length.

The validation command was:

```text
cabal test synthesis-length-tests -j1 --test-show-details=direct \
  --ghc-options='-Werror -Widentities -Wincomplete-patterns -Wincomplete-record-selectors'
```

The public facade test constructs every new sanitized error shape without
importing the private carrier, fingerprint, or authorization vocabulary.
