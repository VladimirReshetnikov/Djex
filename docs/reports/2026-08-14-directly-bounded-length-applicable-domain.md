# Directly bounded Length applicable-domain validation

Date: 2026-08-14

## Outcome

Djex can now attempt to validate every input on which one exact checked scalar
or binary-product Length problem may apply, without asking a solver to infer a
domain. The additive entrances are:

- `validateLengthProblemApplicableDomain` and
  `validateLengthSpinePairProblemApplicableDomain` for exact checked problems;
  and
- `validateLengthSMTLibQueryApplicableDomain` and
  `validateLengthSpinePairSMTLibQueryApplicableDomain` for query-owned replay
  and association.

The query-owned functions emit no SMT-LIB, launch no process, and consume no
raw or live observation. A checked query contributes only the exact retained
problem and behavioral-association authority.

## Version-one direct coverage rule

Contract sealing has already bounded and normalized the precondition before
this pass runs. Applicable-domain coverage nevertheless remains intentionally
syntactic. The scanner considers only the precondition itself when it is one
formula, or the immediate clauses of a normalized top-level `LengthAll`. It
recognizes exactly:

```haskell
LengthAtMost
  (LengthVariable (LengthInput inputPosition))
  (LengthLiteral inclusiveMaximum)
```

The product grammar uses the corresponding `LengthSpinePairInput` variable.
Result variables cannot occur in a checked precondition and never provide
coverage. Reversed inequalities, equality, arithmetic-derived bounds,
negation, disjunction, implication, conditionals, and nested formulas are
ignored. The pass therefore proves no general boundedness theorem and cannot
acquire authority from a solver model or status.

Every compact modeled input must have a recognized clause. Repeated clauses
for one input are combined with `min`, yielding the tightest inclusive
maximum. The resulting maxima stay in compact source order. The first missing
input of a nonnullary problem is reported by
`LengthApplicableDomainInputUpperBoundMissing` and returns
`LengthApplicableDomainInapplicable`, an ordinary conservative outcome rather
than an operational error or behavioral verdict. A nullary problem has no
input requiring a bound, derives maxima `[]`, and validates the existing
singleton box containing assignment `[]`.

Input-width admission is checked before the precondition scan. Once coverage
succeeds, all further validation is delegated to the established finite-box
implementation. Its checked `LengthInputBoxLimits` still cap input width and
Cartesian assignment count; `LengthEvaluationLimits` still bound assigned and
intermediate natural values. The derived bounds do not bypass any existing
limit or demand-order rule.

## Exact traversal and results

The derived inclusive maxima define a tight Cartesian box: every assignment
outside it necessarily fails at least one recognized direct precondition
conjunct, while every assignment on which the complete precondition can hold
is inside it. The existing verifier traverses that box lexicographically with
the last input varying fastest. It evaluates the exact checked candidate and
contract independently for each assignment.

`LengthApplicableDomainValidation` has three outcomes:

1. `LengthApplicableDomainInapplicable` when direct coverage is incomplete;
2. `LengthApplicableDomainCounterexample` when traversal finds the first
   violation; or
3. `LengthApplicableDomainEstablished` only after the entire derived box has
   completed without a violation.

The counterexample arm is the ordinary scalar or product counterexample
evidence already used by direct, model, origin, and finite-box replay. There is
no weaker applicable-domain counterexample type.

Complete scalar traversal wraps the exact completed box receipt in the opaque
`ValidatedLengthApplicableDomain`; the product sibling is
`ValidatedLengthSpinePairApplicableDomain`. Their version-one tags belong only
to these new receipts. Public projections expose the derived maxima, total
assignment count, precondition-applicable assignment count, and exact
`LengthCounterexampleBasis`. Query-owned entrances release either authoritative
arm only after replaying its behavioral evidence against the exact problem
retained by the query.

An established receipt may still be vacuous when the full precondition holds
on none of the derived assignments. Its applicable-assignment count makes that
fact explicit. Provider-backed receipts remain conditional on the exact named
assumed provider laws in their basis; complete traversal does not validate a
provider implementation.

## Authority boundary

Applicable-domain establishment is relative to the checked total finite-spine
model, normalized contract, interpreted candidate, and retained provider-law
basis. It does not establish source-language termination, strictness,
inhabitance, absence of bottoms or effects, or implementation totality. It is
not universal proof, dictionary evidence, provider-implementation validation,
or permission to prune another candidate.

No `sat`, `unsat`, or `unknown` status participates in domain derivation,
traversal, evidence creation, or query association. A consumer may use the
opaque result as an explicitly model-relative ranking signal, but solver
status remains heuristic and authority-free.

## Compatibility and identity

This checkpoint is additive. Existing explicit-box, raw-model, direct-input,
origin, and live-query APIs retain their signatures and behavior. The scalar
and binary-product domains remain nominally separate even though the coverage
algorithm is shared. No contract, provider inventory, semantic inventory,
session policy, candidate, concrete encoding, complete problem, SMT query,
response, protocol, execution, process, worker, or live-observation canonical
identity changes. Only the two new opaque establishment receipts introduce
new schema tags.
