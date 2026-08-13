# Exact zero/step Length case foundation

Date: 2026-08-13

## Result and scope

Djex now has an explicitly selected solver-neutral foundation for interpreting
checked finite-spine zero/step cases. The additive entrances are:

- `sealExactSpineCaseLengthSession`; and
- `sealExactSpineCaseLengthTypedCandidateProblem`.

They do not infer case authority from a graph or contract. The checked session
retains one strict private case-policy discriminator, and only the matching
problem sealer consumes it. Ordinary `sealLength...` and role-aware
`sealRoleAwareLength...` paths continue to reject cases and constructor
patterns with their previous demand order and canonical bytes.

This checkpoint deliberately stopped at the Djex foundation when it landed.
The later Exference follow-up now retains one independently checked exact
recursive zero/step graph shape and its checker-derived constructor schema;
Djinn and every other nonempty case shape remain unavailable. No frontend may
reconstruct graph or schema authority from rendered source. See the
[Exference graph report](2026-08-13-exference-exact-zero-step-graphs.md).

## Schema-bound graph identity

The public `fingerprintSharedTermGraph` remains unchanged and fail-closed. Its
shared type structure owns no constructor-family schema, so a constructor
pattern still fails its fresh reseal with
`UnknownConstructorPatternSchema`.

A package-private encoder entrance accepts a `TypeStructure` only for the
atomic operation which reconstructs the raw graph, freshly seals it, and
structurally fingerprints the checked result. Length builds that structure
from the opaque `CheckedLengthSpineModel` retained by the exact-case session:

- the exact zero constructor has no fields;
- the exact step constructor has one payload and one recursive field;
- the declared recursive-field index fixes their order; and
- every other constructor name or non-modeled pattern type is absent.

The schema is neither returned nor stored beside the graph key. A graph first
sealed by a caller under a permissive, foreign, or reversed field schema is
therefore checked again and rejected before semantic case analysis. This fresh
schema-bound fingerprint failure intentionally precedes domain-specific case
shape diagnostics.

## Closed admitted shape

Under the exact policy, every `TypedCase` must satisfy all of these conditions:

1. its scrutinee and result are modeled finite spines;
2. it has exactly two alternatives;
3. the alternatives are direct applications of the checked zero and step
   constructors, one each, in either source order; and
4. constructor fields contain only bind or wildcard patterns.

The graph checker has already proved scrutinee, pattern, field, and branch
result types against the session-derived schema. Length preflight then stores
one strict canonical zero-before-step receipt for the evaluator. Branch order
remains part of the structural candidate key even when the two graphs normalize
to the same Length expression. Malformed alternatives retain deterministic
sorted graph-node and source-alternative diagnostic order; only valid
zero/step alternatives are reordered for semantic analysis.

Constructor patterns outside an admitted case remain rejected. Holes,
residual constraints, unsupported field patterns, unknown semantics, and
certificate-bearing type applications retain their existing fail-closed
boundaries.

## Symbolic interpretation

For a scrutinee with symbolic length `n`, the interpreter computes:

```text
if n == 0 then zeroResult else stepResult
```

The recursive step field receives `n monus 1`. The payload field receives a
distinct opaque token tied to its exact pattern occurrence. Binding, ignoring,
reconstructing a checked step, or forwarding the token through a provider role
sealed as unobserved does not inspect it. Callable, spine, and tuple use returns
`LengthProblemStepPayloadDemanded` with the payload occurrence and exact demand
site; no fabricated value can enter arithmetic or control.

Both branches are analyzed in canonical zero-then-step order under one strict
evaluation state. Provider summaries reached in either branch enter the final
canonical used-law set, even when both branches produce the same normalized
length and the `LengthIf` disappears. Existing graph, syntax, fingerprint, and
evaluation-step limits still bound this work.

These rules describe only the abstract finite-spine Length model. They do not
claim that a source payload is inhabited, that a source match is lazy or strict
in a particular way, or that a candidate is pure, total, terminating,
recursive, effect-free, or behaviorally correct.

## Identity compatibility

The feature uses conditional identity versions:

- ordinary all-observed session encoding remains version 2;
- ordinary mixed-role session encoding remains version 3;
- exact-case session encoding uses version 4;
- ordinary all-observed concrete encoding remains version 1;
- ordinary mixed-role concrete encoding remains version 2; and
- exact-case concrete encoding uses version 3.

On ordinary paths, the existing roles, field order, policy tags, graph schema
version, contract identity, generic behavioral-problem schema, SMT-LIB query
schema, commands, response grammar, and evidence replay schema are unchanged.
Exact-case policy tags bind the symbolic zero test, natural `monus 1` tail,
opaque payload, and whole-case provider union only on the new versions. An
all-observed explicit role vector remains contract-compatible with the legacy
role encoding, but an exact-case session is intentionally distinct because its
graph trust boundary has changed.

## Failure and API boundary

Case-policy association is checked before residual constraints or graph
availability, so a mismatched ordinary/exact session and sealer cannot demand
a detached graph. Spine-model and provider-inventory validation still precede
role traversal when constructing a session; role overflow still precedes
fingerprint construction. Once a matched problem reaches graph identity, the
fresh session-schema reseal precedes root opening and semantic case preflight.

The public facade adds the two sealers plus closed case- and payload-demand
failure vocabulary. It does not export the private case-policy selector or the
schema-parameterized graph fingerprint function. Existing opaque session,
candidate, problem, and graph roles remain nominal.

## Validation

Focused pure regressions cover:

- the exact tail candidate `\xs -> case xs of [] -> []; _ : tail -> tail` and
  its normalized `if input0 == 0 then 0 else input0 monus 1` result;
- independent replay of that result from one compact model input;
- both ordinary/exact policy mismatch directions before graph demand;
- public shared-fingerprint and ordinary Length rejection of the same case;
- rejection after an initial seal under reversed recursive/payload fields;
- semantic equality but structural-key inequality for reversed branch order;
- explicit failure when a branch tries to observe the opaque payload;
- provider-law union across zero and step branches even when the conditional
  normalizes away;
- exact-policy identity separation, legacy/all-observed parity, role limits,
  and spine/provider-before-role failure precedence; and
- facade signatures, representative demand errors, hidden policy/schema
  projections, nominal authority, and constructor opacity.
