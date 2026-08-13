# Typed-candidate certificate carrier

Date: 2026-08-13

## Outcome

The package-private `TypedCandidate` representation can now lazily retain one
of three states: an explicit graph-unavailable failure, a plain sealed
`TermGraph`, or the opaque
`CheckedTypeApplicationCertificateGraph` atom. This is a representation-only
checkpoint. The public four-parameter type, nominal roles, constructor
opacity, compatibility projection, graph projection, `QueryResult` projection,
and ordinary `mkTypedCandidate` behavior remain unchanged. Exference does not
yet stamp or attach the carrier, Length does not consume it, and no fingerprint
or public API changes.

## Private construction and consumption

`mkCertificateCapableTypedCandidate` accepts one lazy nested result with the
following exact convention:

- `Left failure` retains graph unavailability;
- `Right (Left graph)` retains a legacy plain graph; and
- `Right (Right atom)` retains the complete certificate association.

The nested decision sits underneath the lazy private graph field. Constructing
the typed candidate or projecting compatibility therefore does not inspect
availability. The associated-only convenience entrance delegates through this
same constructor and also leaves its `Either` lazy.

The hidden carrier sum and all three constructors remain unexported. The sole
private eliminator folds the `TypedCandidate` itself and passes compatibility
beside the selected failure, plain graph, or opaque certificate atom. This
keeps correct engine use direct and prevents a detachable hidden-retention API.
It is not an authority theorem: package-private callers are trusted and can
retain callback observations or build another candidate. They remain
responsible for pairing the exact compatibility candidate they checked. The
association atom, independently, still keeps its graph, structural table, and
derived receipts indivisible.

## Compatibility observations and authority loss

The custom `Eq`, `Ord`, and `Show` instances reproduce the old positional
`TypedCandidate candidate (Either failure graph)` observation exactly. They
inspect compatibility first and project an associated atom to its bare graph;
the hidden authority does not participate. Thus plain and associated forms
with equal compatibility and graph values are equal, compare `EQ`, and render
identically.

This is required for source compatibility, but it is intentionally lossy.
Engines must attach association authority only after final ranking and
deduplication. An equality- or ordering-based collection must not serve as
authority storage for associated typed candidates because it may retain an
equal plain representative and discard the carrier.

`typedCandidateTermGraph` likewise returns the same public
`Either failure (TermGraph ty local)` as before. For an associated value it is
a shallow bare-graph projection: certificate handles remain non-authoritative
coordinates, and the opaque table/receipt association is lost. Future
authority consumers must use the private carrier branch rather than this
projection.

## Demand behavior

Compatibility remains the first `Eq`, `Ord`, and `NFData` demand. Unequal
compatibility values short-circuit equality and ordering without inspecting a
poisonous graph result. Lazy `Show` emits the compatibility prefix before
graph demand. Compatibility projection does not force the complete three-way
availability source, while graph projection does not force compatibility.

The associated GADT branch retains the variable-domain `NFData` dictionary at
construction. This lets the public `NFData` instance keep its exact historical
constraints while forcing the entire opaque graph/table/receipt carrier after
compatibility. Public graph projection remains shallow; focused coverage uses
a retained deep poisonous local identity which only full `NFData` reaches.

The unchanged package-private `Internal.Alpha` module was relocated from the
public-source tree to `synthesis/internal`. Its contents and module name are
identical. This makes the certificate carrier's existing alpha-normalization
dependency available to private home-module test components without adding the
entire public synthesis source tree to those components; it changes no exposed
module or behavior.

## Validation scope

The focused strict certificate suite passes 85 tests, including 17 carrier
tests covering all three private fold branches, both
construction entrances, legacy constructor behavior, cross plain/associated
equality, ordering and exact rendering, compatibility-first short-circuiting,
lazy rendering, availability and callback poisons, shallow graph projection,
deep carrier forcing, and compile-time exact constructor/fold/instance
constraint signatures. Public API negative probes cover the hidden carrier
type and constructors, legacy and new private entrances, projection helpers,
and private fold. The strict public API suite passes 34 tests, the affected
strict Length suite passes 223, and the strict Exference engine suite passes
42. A strict all-target build succeeds with `-Werror`, `-Widentities`,
`-Wincomplete-patterns`, and `-Wincomplete-record-selectors`; `cabal check` and
`git diff --check` are clean.
Recompiling the complete Length home-module closure under `-Widentities` also
removed one adjacent redundant `toInteger` around the already-`Integer`
portable executable size; metadata values and encoding are unchanged.
