# Bounded type-application certificate plans

Date: 2026-08-13

## Outcome

Djex now has a package-private, bounded structural foundation for visible
type-application certificates. A raw row contains only a `CertificateId`, an
exact source type, and the complete ordered selections for its leading forall
telescope. `sealTypeApplicationCertificateTable` validates those inputs and
retains an opaque canonical plan of source, selected, and result types plus the
ordered constraints activated by each substitution step.

This checkpoint does not change any engine, candidate, graph, Length, SMT-LIB,
or public fingerprint behavior. In particular,
`fingerprintSharedTermGraph` still returns
`TermGraphFingerprintUnsupportedCertificate` for every certificate-bearing
graph.

## Structural contract

The table has independent checked limits for entries, selections per row,
derived obligations per row, nodes per type, and widths of type-owned
collections. The default policy admits 32 rows, 64 selections per row, 256
derived obligations, 4096 nodes per type, and collection width 256. Every
failure reports a capped, finite observation.

Sealing performs these operations in order:

1. Bound the outer row spine and reject duplicate `CertificateId` coordinates
   without inspecting duplicate payloads.
2. Bound each selection spine, validate and normalize the source type, require
   a nonempty complete leading telescope, and require exact selection arity.
3. Validate and normalize selections from left to right.
4. Erase binderless context-free forall wrappers and assign source binders and
   every selection's binders to disjoint positional namespaces. Free variables
   retain their exact nominal identity.
5. Replay every source binder in order, retaining the exact zero-based slot,
   intermediate source, selected type, result type, and newly activated ordered
   constraints.
6. Bound derived results and the cumulative obligation count before inspecting
   derived obligation payloads, then bound each admitted obligation argument.
   Only after those raw expanded trees are bounded, canonicalize newly saturated
   function and tuple constructors for retained results and obligations.

The complete selection vector includes vacuous binders. Constraints attached
to a multi-binder layer cannot leave that layer before its final binder. If
another leading binder follows, those contexts remain wrapped around the
residual source; the replay emits them as obligations only when the final
binder of the complete leading chain is consumed. Leading and nested
binderless source contexts retain source order. A context introduced inside a
selected polytype remains inside that selected type and is never misclassified
as a source-syntax obligation. Disjoint namespaces make the substitution
capture-free without inventing a fresh caller-domain variable.

Successful construction follows Djex's ordinary productive checked-value
boundary: it need not force the identities carried by structurally valid
variables. The opaque table's explicit `NFData` instance reaches every retained
step and identity when a consumer requests deep evaluation.

## Authority deliberately withheld

The word “checked” here means only bounded normalization and mechanical
substitution replay. The table proves none of the following:

- source inventory, provider, or global ownership;
- source or selection closure;
- positional binder kinds;
- class arity, instance resolution, or obligation discharge;
- association with a candidate, graph, node, or source occurrence;
- fidelity of a rendered type application; or
- permission to create a graph, candidate, or behavioral fingerprint.

`CertificateId` remains a lookup coordinate and is not a semantic identity.
The table, plan, step constructors, projections, and sealer are absent from the
public Djex API. No detachable caller-supplied table can unlock graph identity.

## Validation

The focused suite covers limit-constructor precedence; exact, over-limit, and
cyclic spines and types; duplicate-before-payload behavior; exact type-error
sites; complete and vacuous telescopes; same-layer and nested context
activation; contextual selected types; free-variable retention; alpha and row
order invariance; capture avoidance; derived-obligation preflight; and the
productive construction/deep-`NFData` boundary. Newly saturated function
results and tuple-valued obligation arguments pin post-substitution canonical
storage without weakening the raw-tree bounds.

At this checkpoint:

- all 31 strict certificate-plan tests pass;
- all 8 shared graph-fingerprint tests pass, including the unchanged public
  certificate rejection;
- all 34 downstream API tests pass, including type- and value-namespace
  opacity probes; and
- all 223 strict Length tests pass, including canonical fingerprint and solver
  replay snapshots.

## Next boundary

The next implementation step belongs in Exference's independent expression
checker. It must associate the exact retained source scheme and checked
constraint-resolution evidence with the visible application that produced the
graph, rather than reconstructing provenance from search-route ordinals or a
later rendered term. Only an opaque engine-owned association can then pass a
structural plan and matching graph occurrence atomically to a private
fingerprint entrance. Certificate allocation and table order must remain
nonsemantic, while ordered selections, positional slots, and derived
obligations become explicit semantic fields. Certificate-free graph bytes must
remain unchanged.
