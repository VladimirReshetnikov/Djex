# Bounded Length input-box validation

Date: 2026-08-14

## Result and scope

Djex now has a solver-independent positive bounded verifier for one exact
`CheckedLengthProblem`. `validateLengthProblemInputBox` exhausts an explicitly
finite Cartesian box of compact modeled-input lengths and returns either:

- the first ordinary exact-problem `ValidatedLengthCounterexample` evidence; or
- exact-problem `BehavioralEvidence` carrying an opaque
  `ValidatedLengthInputBox` receipt after every assignment completed without a
  violation.

This fills the domain-owned bounded-evidence seam without converting any raw
behavioral or solver report into evidence. In particular, the new result does
not strengthen Z3 `unsat`, `sat`, or `unknown`, and a raw
`BehaviorBoundedObservation` remains an associated heuristic observation.

## Independent resource limits

`LengthInputBoxLimitSource` has two independent fields:

- `lengthInputBoxLimitSourceMaximumInputs :: Int`; and
- `lengthInputBoxLimitSourceMaximumAssignments :: Natural`.

`mkLengthInputBoxLimits` rejects a negative input cap as
`NegativeLengthInputBoxLimit LengthInputBoxMaximumInputs`. Assignment capacity
is already naturally nonnegative. The opaque checked value exposes only
`lengthInputBoxInputLimit` and `lengthInputBoxAssignmentLimit`. Defaults are
eight compact modeled inputs and 65,536 assignments.

These traversal limits do not replace `LengthEvaluationLimits`. Each inclusive
maximum is checked under `lengthAssignmentValueBitLimit` before product
calculation, and every enumerated assignment and arithmetic intermediate still
passes the ordinary concrete evaluator's assignment and intermediate bit
bounds. Consequently a custom checked problem cannot use a product-one box with
a wide maxima vector without explicit width authority, and a small-width box
cannot retain an unbounded maximum value merely because its assignment cap will
later fail.

## Admission and failure order

The verifier owns this fixed productive order:

1. compare `checkedLengthProblemInputCount` with
   `lengthInputBoxInputLimit`, returning
   `LengthInputBoxProblemInputLimitExceeded maximumInputs problemInputCount`
   before demanding the raw maxima;
2. observe the maxima spine only through the expected arity plus one, returning
   `LengthInputBoxBoundsArityMismatch expected observed` before inspecting any
   maximum value;
3. check inclusive maxima left-to-right, wrapping the existing indexed
   `LengthProblemInputValue` bit rejection in
   `LengthInputBoxMaximumValueRejected inputIndex evaluationError`;
4. compute the product of every `maximum + 1` against the assignment cap,
   returning
   `LengthInputBoxAssignmentLimitExceeded maximumAssignments
   (maximumAssignments + 1)` as a saturated lower bound before allocating the
   initial assignment; and
5. evaluate assignments in deterministic order, reporting the first failure as
   `LengthInputBoxAssignmentEvaluationRejected ordinal evaluationError` with
   its zero-based ordinal.

A nullary problem has the exact one-element box containing `[]`. An assignment
limit of zero therefore rejects it before evaluation. Cyclic or overlong maxima
spines are rejected after only the expected prefix plus one, while an earlier
problem-width rejection does not touch even the first maxima constructor.

The private mixed-radix successor also fails closed as
`LengthInputBoxInternalEnumerationInvariant` if its already-checked maxima and
current assignment widths could somehow diverge. That impossible defensive
case cannot be mistaken for successful exhaustion.

## Deterministic enumeration and demand

Assignments are source-ordered and lexicographic, with the last input varying
fastest. For inclusive maxima `[1, 2]`, the order is:

```text
[0,0], [0,1], [0,2], [1,0], [1,1], [1,2]
```

The input-box verifier and `validateLengthProblemCounterexample` share one
private replay classifier. This preserves exact-arity and value checks,
precondition-first demand, the single shared lazy candidate-result computation,
and postcondition behavior. A false precondition neither forces the candidate
result nor increments the applicable count. A true result-independent
postcondition may also avoid candidate-result demand. The first violated
postcondition forces the independently computed result, constructs the ordinary
counterexample receipt, and stops before any later assignment.

Only exact exhaustion constructs a positive receipt. Its assignment count is
the prechecked Cartesian cardinality; its applicable count records how many
assignments satisfied the precondition. Zero applicable assignments are a
valid but visibly vacuous bounded result rather than an implicit success claim.

## Receipt and provider basis

`ValidatedLengthInputBox` has no public constructor. It privately retains and
deeply evaluates:

- the fixed
  `finite-list-spine-length/bounded-input-box-validation/v1` verifier tag;
- the exact checked source-ordered inclusive maxima;
- the complete assignment count;
- the precondition-applicable assignment count; and
- the same explicit finite-spine/provider-law basis used by independently
  replayed counterexamples.

The four semantic projections are
`validatedLengthInputBoxInclusiveMaximums`,
`validatedLengthInputBoxAssignmentCount`,
`validatedLengthInputBoxApplicableAssignmentCount`, and
`validatedLengthInputBoxBasis`. `ProviderIndependentFiniteSpineModel` success
is still relative to the versioned total finite-spine model.
`FiniteSpineModelUnderAssumedProviderLaws names` additionally depends on the
canonical named assumed laws already retained by the checked problem. A
conditional provider remains usable only because its exact candidate
occurrence passed the earlier independent ground-discharge and protected-chain
boundary; the bounded verifier neither recreates nor exposes a dictionary
receipt.

The public `LengthInputBoxValidation counterexample validated` sum is only a
classification container. `LengthInputBoxCounterexample` and
`LengthInputBoxValidated` cannot manufacture either opaque receipt or generic
`BehavioralEvidence`. The domain verifier binds both result forms to
`checkedLengthProblemBehavioralProblem`, so public evidence replay still
compares domain, inventory, encoding, candidate, and complete problem in the
established order.

## Query-owned association without solver authority

`validateLengthSMTLibQueryInputBox` accepts evaluation limits, input-box limits,
one opaque `LengthSMTLibQuery`, and only the inclusive natural maxima. It reads
the query's private checked problem, delegates all traversal to
`validateLengthProblemInputBox`, and replays either evidence form against that
same query's behavioral problem before releasing the receipt.

The wrapper emits no command, launches no process, consumes no status, model,
transcript, raw artifact, or live observation, and retains no cached verdict.
Its closed error distinguishes
`LengthSMTLibInputBoxValidationRejected LengthInputBoxValidationError` from
`LengthSMTLibInputBoxValidationAssociationRejected ReplayMismatch`. Calling it
after a Z3 result does not make that result part of the evidence chain.

Bounded positive evidence therefore means exactly: every assignment in this
explicit finite box satisfied the checked problem, modulo its recorded
finite-spine/provider basis. It is not:

- universal establishment outside the box;
- exact-pruning permission for synthesis;
- validation of a provider implementation;
- dictionary, instance, or class-resolution evidence;
- proof of source-language inhabitance, purity, strictness, totality, or lack
  of effects; or
- certification of a Z3 encoding or `unsat` result.

## Identity and cache impact

The additive verifier changes no existing semantic or solver construction.
Contract normalization, provider summaries, session and candidate
interpretation, the counterexample condition, canonical QF_LIA translation,
input-only model request, and all live protocol behavior are unchanged.

Accordingly, this checkpoint changes no existing:

- contract or provider-inventory fingerprint;
- semantic-inventory or session-policy fingerprint;
- typed-candidate, concrete-encoding, or complete-problem fingerprint;
- SMT-query schema, fingerprint, command bytes, or value-request bytes;
- response, protocol, execution, process, ready-worker, query-run, or
  live-observation identity; or
- Cabal module list or package version.

The verifier's v1 tag is additive metadata retained only inside the new opaque
bounded receipt. It is not folded into any existing problem or Z3 envelope.
Existing cache entries keyed by those identities therefore remain byte-valid;
a future durable cache of bounded receipts must additionally bind this verifier
tag and exact maxima.

## Validation contract

Focused characterization should cover:

- negative width-limit sealing and exact default/projection values;
- problem-width rejection before poisoned maxima demand;
- nullary, unary, and binary boxes, including exact lexicographic first
  violation;
- productive short, overlong, cyclic, and poisoned maxima cases;
- left-to-right maximum-value rejection and saturating assignment-product caps;
- zero assignment capacity and product-one wide-box rejection;
- precondition short-circuiting, non-vacuous and vacuous applicable counts, and
  result-independent postconditions;
- first evaluation-error ordinals and counterexample early stopping;
- provider-independent and assumed-provider-relative positive bases;
- stale-problem evidence replay rejection and query-wrapper parity/freshness;
- opaque checked constructors, nominal evidence association, `NFData`, and
  complete public-facade vocabulary; and
- preservation of all existing contract, problem, query, protocol, and
  execution characterization bytes.
