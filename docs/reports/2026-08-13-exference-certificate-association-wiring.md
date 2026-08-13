# Exference certificate-association wiring

Date: 2026-08-13

## Outcome

Exference now retains its narrow checker-owned specialization origins as the
opaque atomic certificate graph association carried by private typed
candidates. This connects the earlier origin, association, and carrier
foundations without widening origin eligibility or giving a detached graph
new authority.

For an eligible direct global, the checked term builder stamps each complete
source-telescope prefix node with `(certificateId originId, slot)`. The raw
outer-first graph allocation can therefore contain slot handles in reverse
order, while atomic association reconstructs the canonical source-order
receipt chain. A tagged contextual application is provisionally admitted by
graph sealing and checked against the retained source, selection, result, and
activated obligations. Untagged local, inferred, partial, compatibility
fallback, oversized, and returned-polytype-suffix applications keep their
existing witness validation and remain certificate-free.

An empty origin list follows the legacy `sealTermGraph` path. A nonempty list
has no plain-graph fallback: Exference lowers every normalized origin and step
to independent association input, passes the checked term's exact
constructor-schema `TypeStructure`, and either retains the complete opaque
association or reports graph unavailability. This also allows an exact
recursive zero/step case and an eligible specialization to coexist in one
associated graph.

## Candidate and public behavior

The final engine projection uses only
`mkCertificateCapableTypedCandidate`. Its complete unavailable/plain/associated
decision stays below the lazy graph field, so compatibility projection does
not force graph construction or certificate association. Deep `NFData` does
force the entire associated graph, table, owner scheme, and receipt chain.

The private availability type preserves its historical `Eq` and `Show`
observations by projecting an association to its bare graph. In particular,
plain and associated availability values with the same graph compare equal,
and associated values render with the old `ExferenceTermGraphAvailable`
constructor spelling and precedence. As with the shared typed-candidate
carrier, these observations are deliberately lossy and must not be used to
store authority through equality-based deduplication.

`typedCandidateTermGraph` keeps its public signature. For an associated value
it returns the stamped bare graph, not the association atom. The handles in
that projection are candidate-local coordinates only, and public
`fingerprintSharedTermGraph` rejects them. A bare graph cannot recover the
checked table, owner association, or complete occurrence receipts.

## Failure and trust boundary

Association runs only after the independent checker has completed rigid-scope,
constraint-resolution, escaping-rigid, and residual-constraint validation.
Consequently an unsatisfied contextual specialization still returns the
historical checker `ConstraintMismatch` before graph association exists.
Ordinary graph sealing errors preserve the existing
`TermGraphSealingFailure`. Other private association failures are sanitized
into the additive public enum
`ExferenceTermGraphCertificateAssociationFailure`:

- `TermGraphCertificatePlanLimitFailure`;
- `TermGraphCertificatePlanValidationFailure`; and
- `TermGraphCertificateOccurrenceAssociationFailure`.

`TermGraphCertificateAssociationFailure` nests that summary in
`ExferenceTermGraphAbsence`. This is an additive public API change; exhaustive
matches on the absence type must handle the new constructor. Raw
association-error payloads—including checker type trees, owner names,
certificate-plan errors, and graph coordinates—remain package-private.

Neither a handle, sanitized failure category, successful association, nor its
bare graph projection proves declaration or inventory membership, argument
kind correctness, instance/discharge identity, behavioral meaning, candidate
completeness, or fingerprint authority. At this checkpoint a later Length
consumer still had to retain the opaque carrier and independently match the
exact owner and scheme to its own checked inventory. That follow-up is now implemented for the narrow exact,
obligation-free provider-only case; modeled constructors and contextual
obligations remain rejected. See the
[Length consumption report](2026-08-13-length-associated-provider-certificates.md).

## Bounds and validation

Origin eligibility remains capped at the six-argument provider-instantiation
limit. A seven-binder source is silently origin-ineligible and uses the plain
graph path. Atomic association additionally applies the shared default table
limits; 33 independent one-slot origins exceed the 32-row bound and fail
closed with the public plan-limit summary rather than downgrading to a plain
graph.

The strict private engine suite passes all 45 tests. Focused regressions cover
two-slot outer-first handles and source-order receipts, contextual activated
obligations, selected-polytype suffix exclusion, partial/inferred/local and
compatibility plain paths, six-versus-seven eligibility, pre-association
constraint failure, the 33-row limit, exact-case composition, public stamped
fingerprint rejection, legacy equality/rendering, deep evaluation, lazy
constructor-tag comparison, and a real scheme-backed query whose private
`TypedCandidate` fold observes the associated branch.

The strict public API suite passes all 35 tests, the broader Exference suite
passes all 493, the downstream term-graph fingerprint suite passes all 8, and
the downstream Length suite passes all 223. The complete repository
`cabal test all` run passes. A final all-target build succeeds with `-Werror`,
`-Widentities`, `-Wincomplete-patterns`, and
`-Wincomplete-record-selectors`; `cabal check` and `git diff --check` are
clean.
