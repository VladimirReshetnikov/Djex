# Atomic certificate graph associations

Date: 2026-08-13

## Outcome

Djex now has a package-private atomic foundation which co-owns a sealed typed
term graph, its rebuilt checked type-application certificate table, exact
global owner schemes, and complete graph-occurrence receipts for every stamped
certificate use. The new boundary is
`Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association`.

This checkpoint deliberately does not stamp Exference output, change a public
API, authorize certificate-aware fingerprints, or alter Length behavior or
fingerprint bytes. Public `fingerprintSharedTermGraph` still rejects every
certificate-bearing graph.

## Atomic entrance

`sealCheckedTypeApplicationCertificateGraph` accepts an untrusted
`TermGraphSource`, a caller-owned trusted `TypeStructure`, explicit graph and
certificate limits, and independent checker-origin rows. It never accepts a
detachable prechecked graph, certificate table, graph node, base occurrence, or
visible occurrence chain.

Sealing proceeds in this order:

1. Build the structural certificate table from each exact origin scheme and
   the selected types in its ordered observations. Existing certificate limits
   bound rows, selections, obligations, type nodes, and type collection widths.
2. Look up each certificate through the checked table and independently match
   its zero-based slot, source, selection, result, and ordered activated
   obligations. Every repeated type is bounded and normalized again. Bound
   variables compare alpha-equivalently; genuinely free variables remain
   nominal.
3. Seal the raw graph with the caller's trusted type structure. Unstamped
   visible applications delegate to its ordinary witness predicate. A stamped
   witness is only provisionally admitted so the stronger association check can
   run before any `TermGraph` escapes.
4. Traverse the sealed graph from its root in structural child order. Raw node
   table order is diagnostic storage and has no effect. Derive every stamped
   visible node and occurrence solely from its `(CertificateId, slot)` handle.
5. Reject duplicate handles, unknown or out-of-range handles, unused origins,
   missing slots, and any origin which is not exactly one complete use.
6. For every source-order step, check the child node's source type, witness
   source, specified visible argument, witness selection, result node type, and
   witness result against the checked plan. Slot zero must point to the exact
   named global and normalized owner scheme; later slots must point to the
   preceding visible node.
7. Retain only successful associations, reordered into rooted structural
   occurrence order for the future-consumer fold. Input origin order controls
   deterministic error precedence only.

The zero-origin case is uniform: a certificate-free source still becomes the
same sealed graph, while any stamped use rejects. Tests compare ordinary and
associated graph equality, metrics, erasure, and public fingerprint bytes.

## Authority deliberately withheld

Certificate IDs, term-node IDs, occurrence IDs, and their traversal order are
candidate-local coordinates. They are not semantic identities or provenance.
The checked association proves none of the following:

- membership of the owner in a prepared declaration or inventory;
- positional binder-kind correctness;
- identity of a class instance or constraint discharge;
- behavioral interpretation or candidate completeness; or
- permission to build a public or private semantic fingerprint.

The opaque carrier exposes only its co-owned graph and a package-private fold
over verified observations. Projecting the graph produces a bare legacy graph;
the stamped handles inside it confer no authority. Authority-bearing consumers
must retain and use the opaque atom. Fold results may retain owner, plan, and
receipt observations, but cannot reconstruct, detach, or recombine the checked
table or association carrier. At this checkpoint a later Length integration
still had to re-match the exact owner and scheme against its own prepared
inventory and consume this carrier atomically before any certificate-aware
fingerprint could be designed.
That later carrier-aware fingerprint and the provider-only Length consumer now
exist; they preserve this structural layer's deliberately withheld authority.
See the
[carrier-aware fingerprint report](2026-08-13-carrier-aware-certificate-graph-fingerprints.md)
and
[Length consumption report](2026-08-13-length-associated-provider-certificates.md).

## Validation

The focused strict suite contains 68 tests. The new association matrix covers:

- exact two-slot sealing with outer-first occurrence allocation;
- invariance to raw node-table order and source-order slot receipts;
- contextual activated obligations and selected-polytype alpha renaming;
- nominal free-variable rejection;
- certificate-free graph equality, metrics, erasure, and fingerprint parity;
- untagged returned-polymorphic suffix acceptance and stamped suffix rejection;
- unused, missing, duplicate, unknown, out-of-range, and noncontiguous uses;
- exact global owner and scheme checks;
- child, witness, node, visible-argument, selected, and result mismatches;
- delegation of unstamped witness validation and wrapped graph-seal errors;
- productive outer, selection, obligation, argument-width, and type-node gates;
- duplicate and missing-plan precedence before poisonous payload demand; and
- complete deep-`NFData` retention.

The public API negative probes cover the carrier, raw origin and observation
types, all raw record selectors, sealer, graph projection, matcher, and fold.
