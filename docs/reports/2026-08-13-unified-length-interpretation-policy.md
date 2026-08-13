# Unified checked Length interpretation policy

Date: 2026-08-13

## Result and scope

Djex now has one checked authority for the two interpretation choices that were
previously selected by separate sealer pairs: target-argument observation and
finite-spine case handling. The closed public request type is
`LengthInterpretationPolicySource`, with three constructors:

- `LengthLegacyCasesRejected`;
- `LengthExplicitTargetRolesCasesRejected roles`; and
- `LengthExplicitTargetRolesExactZeroStepCases roles`.

The two explicit forms always carry a complete role vector, including the
all-observed and empty-vector cases. Exact zero/step authority therefore cannot
be represented without explicit role authority.

`sealLengthSessionWithInterpretationPolicy` checks the selected source and
stores an opaque `CheckedLengthInterpretationPolicy` in the session.
`checkedLengthSessionInterpretationPolicy` exposes only that nominal token: its
constructor, exact roles, mixedness projection, and case projection remain
private. New code can then use `sealLengthContractInSession` and
`sealLengthTypedCandidateProblemInSession` without independently selecting the
same policy axes again.

This checkpoint changes no SMT-LIB lowering, solver protocol, response parser,
evidence rule, or frontend graph authority.

## One source of contract authority

`sealLengthContractInSession` selects its contract path from the checked
policy:

- legacy policy derives the historical all-observed vector from the physical
  target spine; and
- explicit policy supplies the exact retained vector to the existing
  role-aware contract sealer.

All target bounds, normalization, kind checks, role bounds and arity checks,
modeled-spine checks, formula checks, and contract fingerprint construction
remain in those existing sealers. For example, a two-role policy paired with a
unary target returns the existing
`LengthContractTargetArgumentRoleArityMismatch 1 2`; an invalid target still
fails its earlier target check before role arity or formula demand.

The unified problem sealer first preserves the legacy special rejection for a
mixed contract under implicit legacy policy. It then checks the historical
case and mixed/all-observed projections. For an explicit policy it additionally
requires the checked contract's complete role vector to equal the retained
vector, including order and arity. A mismatch returns the existing
`LengthProblemTargetArgumentPolicyMismatch` before contract resealing,
residuals, or graph demand. After a match, contract replay deliberately goes
through `sealLengthContractInSession`, so future checked policy axes have one
authoritative route.

## Compatibility wrappers

The existing public entrances remain wrappers:

- `sealLengthSession` requests legacy case rejection;
- `sealRoleAwareLengthSession roles` requests explicit roles with case
  rejection; and
- `sealExactSpineCaseLengthSession roles` requests explicit roles with exact
  zero/step cases.

The existing problem sealers retain their characterized association behavior
instead of delegating to the new strict problem entrance. In particular, the
role-aware and exact compatibility wrappers compare only the old
mixed/all-observed projection. They continue to accept a contract whose role
order or arity differs from the vector used to construct the session when the
two vectors have the same mixedness. This includes an empty-role session with a
binary all-observed contract and a one-role mixed session with a longer mixed
contract. Their legacy-mixed, case-policy, target-policy, contract-reseal, and
graph failure precedence is unchanged.

The strict and compatibility behaviors are tested side by side on the same
session, detached contract, and valid candidate: the old wrapper succeeds and
the unified entrance rejects the exact-vector drift. This makes the new API an
additive association repair rather than an implicit compatibility break.

## Identity association and later version advance

The session privately retains both `Maybe [LengthTargetArgumentRole]` and the
target-policy and case-policy projections. Only those projections, not the
complete role vector, enter the session encoding fingerprint. The vector was
not added as a parallel inventory, session, concrete-encoding, candidate, or
complete-problem field; the contract and downstream identities continue to use
the same existing role-aware contract fingerprint.

The later associated-provider certificate checkpoint deliberately advances
the common candidate-policy identity even on ordinary candidates. Current
session encoding versions are 5 for legacy/all-observed case rejection, 6 for
mixed case rejection, and 7 for exact cases, superseding versions 2/3/4.
Accordingly, the current repository does not preserve the historical session
bytes or containing downstream keys described by this checkpoint's original
snapshots. Callers must invalidate those cached identities.

The structural equivalences remain: legacy and explicit all-observed policy
share the same current session identity; same-mixedness role vectors still
share a session identity while their role-aware contract identities differ;
and unified-vs-wrapper tests continue to compare every layer for all five
configurations. The public interpretation signatures and loose-wrapper versus
strict-association behavior are unchanged. See the
[associated provider-certificate report](2026-08-13-length-associated-provider-certificates.md).

## Demand and failure boundary

Session admission keeps the established order: spine schema, provider
inventory, reserved constructor/provider conflict, explicit role bound,
inventory fingerprint, then encoding fingerprint. A preceding spine failure
does not inspect a poisoned policy vector, and role overflow observes only the
bounded spine needed to report `maximum + 1`.

Construction remains productive. An explicit vector beginning with
`LengthUnobservedTarget` can seal without forcing a poisoned later role because
the mixedness projection short-circuits. The session now retains that exact
vector, however, so its honest `NFData` instance reaches all retained roles.
Deep evaluation therefore adds demand compared with the former
mixedness-only session representation. The checked policy is not eagerly
materialized during sealing and its `NFData` instance is part of the public
opaque contract.

## Public abstraction boundary

The public facade exports the closed source vocabulary, opaque checked-policy
type, opaque session projection, and the three unified functions. It does not
export:

- the `CheckedLengthInterpretationPolicy` constructor;
- the session's exact-role projection; or
- the checked policy's exact-role, mixedness, or case projections.

API probes pin the exact new function types, all five representative source
configurations, and the checked policy's `NFData` dictionary. Negative probes
confirm that the opaque checked policy exposes no `Generic`, `Eq`, `Ord`, or
`Show` dictionary and cannot be coerced to an unrelated type.

## Validation

Focused regressions cover:

- five representative valid policy sources;
- unified output and fingerprint parity with every compatibility wrapper;
- current canonical fingerprint bytes and SHA-256 snapshots, including the
  later 5/6/7 session-policy version advance;
- spine-before-policy demand and bounded role-limit failure;
- productive sealing plus honest deep forcing of retained roles;
- in-session contract error routing and target-check precedence;
- reversed and arity-drifted role rejection before a poisoned graph and a
  foreign contract reseal;
- matching-role progress to the expected foreign-context reseal failure;
- legacy mixed-contract special rejection; and
- side-by-side loose-wrapper success and strict unified rejection for ordinary
  and exact policy.

The downstream facade and abstraction-boundary suite exercises the complete
public surface without importing internal modules.
