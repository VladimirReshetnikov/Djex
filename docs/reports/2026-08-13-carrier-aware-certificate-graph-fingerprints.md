# Carrier-aware certificate graph fingerprints

Date: 2026-08-13

## Outcome

Djex now has one package-private canonical-fingerprint entrance for the opaque
`CheckedTypeApplicationCertificateGraph` atom:
`fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure`. It
accepts the caller's exact `TypeStructure`, graph limits, retained-byte limit,
and complete graph/table/occurrence association as one carrier.

No public fingerprint function or subject changed. A bare projected graph has
lost the association carrier and public `fingerprintSharedTermGraph` continues
to reject its certificate handles.

## Exact empty compatibility

An atom with no certificate association literally delegates to
`fingerprintTermGraphWithTypeStructure`. It therefore retains the exact v1
canonical bytes, fresh-reseal failures, byte-limit failures, and demand order.
There is no second empty encoding and no v2 upgrade for an ordinary graph.

## Nonempty v2 encoding

A nonempty carrier first reconstructs and freshly seals its graph under the
caller-supplied structure and graph limits. Ordinary visible witnesses still
delegate to that structure; a certificate-bearing witness is provisionally
admitted only because the opaque input already co-owns the exhaustively checked
structural plan and occurrence receipt.

One strict canonicalization state then encodes the rooted graph followed by the
rooted association rows. This preserves flexible and rigid free-variable
equality classes across both halves of the identity. The v2 builder keeps the
existing `shared-typed-term-graph` role, uses dialect
`shared-typed-term-graph/v2`, and adds normalization marker
`certificate-semantic-associations/v1`.

Within the rooted term, each stamped witness encodes only a
`certificate-semantic-reference` containing canonical row and step ordinals.
Each row encodes:

- the exact global owner `Name`;
- its normalized retained source scheme;
- source-order plan steps containing source, selected, and result types; and
- each step's ordered activated constraints and arguments.

Plan binders use distinct `source-bound` and `selection-bound` tags. Genuine
free variables share the graph encoder's flexible/rigid slot state. Bound
spelling is alpha-insensitive, while free-variable flavor and equality pattern
remain semantic.

## Coordinates and authority

Certificate IDs, raw source slots, node IDs, occurrence IDs, retained node-table
order, and caller origin-row order are used only to validate and locate the
carrier's exhaustive receipts. None is encoded. Rooted structural order and
source-order plan steps determine canonical row/step ordinals instead.

The key is still only a structural graph identity. It proves no prepared
inventory membership, declaration provenance, kind witness, constraint
resolution or discharge identity, behavioral interpretation, or candidate
completeness. A domain consumer must retain the opaque carrier, establish its
own authorities, and wrap these bytes in its own identity role where needed.

## Validation

The focused certificate suite covers:

- literal empty-carrier v1 byte, reseal-error, demand, and byte-threshold parity;
- one- and two-step v2 rows, exact field order, semantic witness references,
  and unchanged public rejection;
- a two-row rooted tuple invariant under caller row order, certificate IDs,
  node IDs, occurrence IDs, and retained node-table order;
- contextual activated constraints and returned-polymorphic untagged suffixes;
- alpha-renamed binders plus free-variable flavor and equality-pattern
  sensitivity;
- exact constructor-schema admission and wrong-schema reseal precedence before
  byte-limit construction; and
- exact nonempty retained-byte acceptance with one-byte-short rejection.

The public-facade negative probe also confirms that the new entrance remains
package-private.
