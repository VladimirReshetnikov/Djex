# Query-run decoded authority narrowing

Date: 2026-08-12

## Outcome

The package-private `LengthSMTLibQueryRun` no longer retains the complete
`LengthSMTLibProtocolDecoded` value after a successful live query. The decoded
status-indexed observation remains available locally for every operation which
depends on it:

- independent Length counterexample replay against the exact sealed query;
- classification of the absent, vacuous-zero-input, or framed-value branch;
- construction of the unchanged reversible query-run identity; and
- construction of the final strict status-indexed evidence observation.

Only after those operations and the final transport boundary succeed does the
run retain its ordinal, one strict `SolverObservation`, complete run key,
transcript SHA-256, and immutable stdout/stderr boundaries. Only the
satisfiable branch can contain optional problem-associated
`BehavioralEvidence`; the `unsat` and `unknown` branches contain unit. The
package-private `lengthSMTLibQueryRunInputValues` projection remains removed,
and the former split status/evidence projections are now one package-private
whole-observation projection.

## Authority and retention

For a satisfiable values-policy result, replay traverses the full decoded
binding spine under the admitted evaluation limits. A successful evidence
receipt retains normalized source-ordered natural inputs, the result computed
from the checked candidate, and its provider-assumption basis. Invalid,
negative, missing, duplicate, unknown, stale, or non-counterexample models
still fail before a query run can escape; valid bindings are restored to source
input order before the receipt is sealed.

The deletion is not a promise to scrub solver output. The collision-free run
fingerprint remains a private reversible complete key and continues to embed
the exact bounded causal transcript bytes, including the raw model response.
The transcript digest remains a separate convenient projection. What has been
deleted is the second parsed binding representation and its package-private
selector, leaving evidence as the sole structured successful counterexample
payload.

The successful lease commit projects the run ordinal and accounting anchors
before returning it, forcing the run's strict fields. Consequently a successful
result cannot defer the solver-observation constructor or its satisfiable
`Maybe` spine in a thunk which retains the decoded binding value beyond the
commit boundary. A failed commit exposes no run. Generic `SolverObservation`
payloads remain deliberately lazy; the opaque Length owners force only the
inner `Maybe` spine needed to preserve this former demand.

## Identity and type safety

`buildLengthSMTLibQueryRunIdentity` receives one private five-way replay outcome
after validation: status-only satisfiable, validated vacuous satisfiable,
validated framed satisfiable, unsatisfiable, or unknown. Both the decoded and
replay fields pattern-match that single owner, while the exact protocol plan,
causal transcript, deadline, ordinal, markers, and transport boundaries retain
their order. The version-1 role, field layout, canonical bytes, admission
calculation, and schema tag are unchanged.

The `epoch`, `identity`, and `local` parameters remain explicitly nominal even
though the final representation no longer structurally contains the decoded
value. Its constructor remains private, and the sole construction path is
typed by the same reserved plan and decoded outcome, so foreign epochs or
queries cannot be coerced into association.

## Verification

The pure response/protocol suite asserts the whole decoded observation across
status-only, raw-binding, and vacuous `Just []` branches. Successful live-query
cases assert exact status-indexed evidence replay, normalized input order,
recomputed result, and transport accounting without consulting a raw binding
projection. Unary, binary, zero-input, split-output, and drip-output paths all
cross this boundary, while identity regressions pin the exact version-1 branch
fields.

This checkpoint changes no public API, Cabal module list, solver command,
response parser, wire byte, failure order, fingerprint byte, schema tag,
evidence rule, or Leant behavior.
