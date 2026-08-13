# Role-aware Length target arguments

Date: 2026-08-13

## Result

The solver-neutral Length foundation can now interpret targets which combine
observed finite-list spines with arguments that have no Length semantics. The
new closed public role vocabulary is:

- `LengthObservedSpine`; and
- `LengthUnobservedTarget`.

The feature is additive. `sealLengthContract`, `sealLengthSession`, and
`sealLengthTypedCandidateProblem` keep the all-observed behavior and exact
historical fingerprint bytes. The corresponding `sealRoleAware...` entrances
admit a bounded role vector and select new identity versions only when that
vector actually contains an unobserved role.

## One role-vector authority

`CheckedLengthContract` is the sole retained structured owner of the complete
source-ordered role vector. A role-aware session consumes the same bounded
vocabulary only to select one strict semantic-policy discriminator: legacy
all-observed or mixed opaque-target. It does not cache another role list. The
candidate boundary re-seals the contract with its retained roles and checks
that mixedness agrees with the session policy before it inspects residual
constraints or graph availability.

Only observed roles must match the configured finite-spine family. Their
`LengthInput` positions are numbered compactly in observed-role order. The
result remains a required modeled spine. Thus physical roles

```text
[unobserved-target, observed-spine]
```

authorize exactly one model input, `LengthInput 0`; the unobserved position has
no SMT symbol or replay assignment.

Role admission is productive under the existing contract-input limit. Session
failure order remains spine model, provider inventory, modeled-constructor
conflicts, target roles on the new path, inventory identity, then encoding
identity. Contract sealing keeps target bound, normalization, kind, and
constraint checks before bounded role arity and observed-spine validation,
then checks the result, precondition, postcondition, and fingerprint. The
legacy paths never receive or traverse a role list.

## Opaque interpreter token

Candidate interpretation applies every physical target argument in source
order. An observed role receives its compact symbolic spine. An unobserved
role receives `SemanticOpaqueTargetArgument` carrying only the physical
position; it is not a dummy Natural, bottom approximation, fabricated source
value, or arbitrary semantic object.

The token can travel only through non-demanding interpreter paths. In
particular, a candidate may ignore it, forward it to an assumed provider
argument sealed as `LengthUnobservedArgument`, or place it in the payload field
which the checked list-step model does not inspect. Attempting to call it,
observe it as a list spine, or destructure it as a tuple returns
`LengthProblemUnobservedTargetArgumentDemanded` with the physical position and
the exact demand site. No token can enter Length arithmetic, a contract
formula, a branch condition, equality, or an SMT term.

An unobserved role makes no claim that a concrete source value inhabits the
argument type. It also says nothing about source-language purity, totality,
strictness, effects, type reflection, or whether a real implementation would
evaluate that argument. The result remains model-relative ranking evidence
under the exact checked provider assumptions, never source-language proof.

## Concrete higher-order path

The focused end-to-end fixture uses the target shape

```text
(a -> b) -> List a -> List b
```

with target roles `[unobserved-target, observed-spine]`. Its checked `map`-like
provider has roles `[unobserved, spine]` and transfer expression
`LengthProviderArgument 1`. The interpreter forwards the opaque function token
without forcing it, observes the second argument, and derives candidate result
`LengthInput 0`. Query sealing emits only `djex_length_input_0`; input-value
decoding and independent replay likewise accept only that one compact value.

## Identity compatibility

Three identity layers change conditionally:

- mixed contracts use the existing contract role with builder version 3 and
  bind the complete ordered target-role vector plus compact observed count;
- mixed sessions use the existing solver-neutral encoding role with builder
  version 3 and bind the opaque-forward-only interpreter policy; and
- mixed concrete encodings use the existing concrete-encoding role with
  builder version 2 and bind the role-aware interpreter policy.

Explicit all-observed role-aware calls use the legacy contract version 2,
session version 2, and concrete-encoding version 1 fields byte for byte. The
tests compare both the typed fingerprint values and canonical bytes on those
paths. Mixed role order changes contract identity even when compact observed
arity is unchanged. The generic behavioral problem schema, candidate identity,
QF_LIA query schema, wire commands, response grammar, and evidence replay
schema do not change.

## Validation

Focused regressions cover:

- higher-order role admission, exact projection, arity mismatch, bounds, and
  compact out-of-range references;
- legacy and explicit-all-observed contract, session, concrete-encoding, and
  complete-problem identity equality;
- mixed role-order sensitivity and session-policy separation;
- spine/provider rejection before poisoned roles and role overflow before
  fingerprint construction;
- productive cyclic role rejection at the exact bound;
- legacy-sealer and session-policy mismatch failures;
- the higher-order map provider, compact SMT input request, decoded-model
  validation, and independent replay;
- opaque callable, spine, and tuple demand sites; and
- successful opaque list-step payload forwarding.

The public facade and negative abstraction suite additionally pin the closed
role vocabulary, additive sealer signatures, contract role projection,
representative role and demand errors, hidden constructors, nominal checked
authorities, and absence of record-field construction access.

## Exact-case foundation follow-up

The later exact zero/step case foundation composes with these explicit target
roles but does not weaken this checkpoint's ordinary-path guarantees. Its
dedicated session and problem sealers select a distinct case policy; the
legacy and role-aware entrances above continue to reject every case with their
unchanged identity versions. The case-aware session still consumes the same
bounded role vector, while the contract remains its sole complete structured
owner.

The later Exference follow-up makes only the independently checked recursive
zero/step graph shape available to typed candidates. Djinn and every other
nonempty case shape remain unavailable, while this report's higher-order map
path and ordinary frontend behavior remain unchanged. See the
[exact zero/step case foundation report](2026-08-13-exact-zero-step-length-cases.md)
and the
[Exference graph report](2026-08-13-exference-exact-zero-step-graphs.md).
