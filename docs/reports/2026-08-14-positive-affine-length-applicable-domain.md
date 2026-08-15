# Positive-affine Length applicable-domain validation

Date: 2026-08-14

## Outcome

Djex now has an additive, solver-independent applicable-domain rule for
positive-affine scalar and binary-product Length preconditions. It extends the
set of preconditions from which Djex can derive a finite coverage box without
changing the original literal-direct rule.

The exact checked-problem entrances are:

- `validateLengthProblemPositiveAffineApplicableDomain`;
- `validateLengthSpinePairProblemPositiveAffineApplicableDomain`.

The query-owned entrances are:

- `validateLengthSMTLibQueryPositiveAffineApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryPositiveAffineApplicableDomain`.

Every entrance reuses `LengthApplicableDomainValidation`: incomplete syntactic
coverage is an ordinary `LengthApplicableDomainInapplicable`; the first
independently replayed violation is the existing scalar or product
counterexample; and complete traversal releases a new opaque establishment
receipt.

## Additive compatibility with the direct rule

`validateLengthProblemApplicableDomain` and
`validateLengthSpinePairProblemApplicableDomain` remain the literal-direct v1
functions. They still recognize only an immediate normalized clause of the
form:

```haskell
LengthAtMost
  (LengthVariable (LengthInput inputPosition))
  (LengthLiteral inclusiveMaximum)
```

The corresponding product clause uses `LengthSpinePairInput`. Equality and
arithmetic remain inapplicable through those old entrances, and their opaque
`ValidatedLengthApplicableDomain` and
`ValidatedLengthSpinePairApplicableDomain` receipts retain their original tags
and meaning.

The positive-affine functions are separate explicit selections. There is no
new Djex policy enum which could detach a policy label from its receipt type.
The existing inapplicability, classification, evaluator error, and query error
types are reused because those values carry no positive policy authority.

## Positive-affine coverage rule

Contract sealing has already bounded and normalized the precondition. For a
nonnullary problem, the new scanner examines either the precondition itself or
the immediate clauses of its flat top-level `LengthAll`. The only admitted
expression grammar is:

```text
A ::= compact-input
    | natural-literal
    | LengthSum [A]
    | LengthScale positive-literal A
```

A whole comparison is recognized only as:

```text
A <= natural-literal
A == natural-literal
natural-literal == A
```

Suppose the normalized affine expression denotes:

```text
c + a0*x0 + ... + an*xn
```

All values and coefficients are natural numbers. If the literal ceiling is
`k`, then `c <= k` and either recognized comparison imply, for each `ai > 0`:

```text
xi <= (k - c) quot ai
```

The proof uses only nonnegativity: every other term can be dropped from the
left side to obtain `c + ai*xi <= k`. Equality implies the same upper
inequality. Repeated bounds for one compact input are combined with `min`, and
the result remains in compact source order.

The implementation saturates summary constants and coefficients at `k + 1`.
That cap preserves the proof: a saturated constant detects `c > k`, while a
saturated coefficient is still greater than the largest possible numerator
`k - c` and therefore yields the same zero quotient as the exact coefficient.
It also prevents the extraction pass from retaining arithmetic larger than the
comparison can distinguish.

The rule deliberately does not partially mine a rejected subtree. A
nonliteral right side, affine-versus-affine equality, `LengthNot`, natural
monus, minimum, maximum, quotient, modulo, conditional expressions, and every
other non-affine shape grant no bound. Unsupported clauses remain part of the
actual precondition during replay; they merely provide no finite-coverage
authority.

## Example

For compact inputs `input0` and `input1`, consider:

```haskell
input0 = LengthVariable (LengthInput 0)
input1 = LengthVariable (LengthInput 1)

precondition = LengthAll
  [ LengthAtMost
      (LengthSum
        [ LengthScale 2 input0
        , LengthLiteral 3
        ])
      (LengthLiteral 9)
  , LengthEqual input1 (LengthLiteral 2)
  ]
```

The first clause is `3 + 2*input0 <= 9`, so it derives `input0 <= 3`.
The equality derives `input1 <= 2`. The source-ordered maxima are therefore
`[3, 2]`, and the existing box verifier traverses 12 assignments in
last-input-fastest lexicographic order. Exactly four assignments satisfy this
precondition: `[0,2]`, `[1,2]`, `[2,2]`, and `[3,2]`.

For a checked query whose candidate satisfies the postcondition on those four
assignments:

```haskell
validation = validateLengthSMTLibQueryPositiveAffineApplicableDomain
  defaultLengthEvaluationLimits
  defaultLengthInputBoxLimits
  checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthPositiveAffineApplicableDomainInclusiveMaximums receipt
    , validatedLengthPositiveAffineApplicableDomainAssignmentCount receipt
    , validatedLengthPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([3, 2], 12, 4)
  _ -> False
```

If an earlier assignment violates the postcondition, the result is instead the
ordinary exact-problem `ValidatedLengthCounterexample`.

## Contradiction and nullary semantics

`LengthTruth False` is an exact syntactic contradiction. Normalization reduces
an unequal constant-only equality to this constructor in either source
orientation, so `1 == 2` and `2 == 1` both establish vacuity. Equal
constant-only equality normalizes to `LengthTruth True` and grants no input
bound. A recognized affine comparison is also contradictory when its constant
`c` already exceeds its literal ceiling `k`: no natural assignment can reduce
the expression below its constant term.

For a nonnullary contradictory precondition, the scanner selects one zero
maximum for every compact input. This all-zero box is a finite coverage carrier
for an empty applicable domain. Djex still delegates to the ordinary box
verifier rather than manufacturing a zero-traversal receipt. Successful
establishment therefore records:

- inclusive maxima `[0, ..., 0]`;
- total assignment count `1`;
- applicable assignment count `0`.

A recognized contradiction takes precedence over missing bounds elsewhere in
the conjunction. The scanner may stop at the first contradiction because later
clauses cannot make the conjunction applicable.

A nullary problem requires no extracted bound and does not scan the
precondition for coverage. It derives maxima `[]` and validates the existing
singleton assignment `[]`. The applicable count is one when the precondition
holds and zero otherwise. Any bounded evaluation failure on that assignment
remains an operational failure rather than establishment.

## Admission, demand, and failure order

The scalar and product paths share this fixed structure:

1. compare the checked compact input count with the input-box width limit before
   demanding the precondition;
2. for a nonnullary problem, scan canonical top-level clauses in stored order
   and recognized affine children left to right;
3. stop on the first proved contradiction, or after the complete scan report
   the first missing compact input in source order as ordinary inapplicability;
4. pass the exact generated maxima to the established box verifier, which
   checks values left to right and admits the saturating Cartesian product
   before evaluating an assignment;
5. replay assignments lexicographically with the last input varying fastest,
   retaining the zero-based ordinal of an evaluation rejection;
6. return the first ordinary counterexample or wrap the completed box receipt;
7. for a query-owned call, replay the evidence against the exact behavioral
   problem retained by that query before releasing its opaque receipt.

Width, maximum-value, Cartesian-product, indexed evaluation, internal
enumeration, and query-association failures remain closed `Left` values under
the existing scalar or nominal product error types. Incomplete coverage alone
is the successful inapplicability classification. There is no `Right Nothing`
outcome.

## Receipt authority

Complete scalar traversal produces
`ValidatedLengthPositiveAffineApplicableDomain`. The product receipt is the
nominally separate
`ValidatedLengthSpinePairPositiveAffineApplicableDomain`. Both privately wrap
the exact completed domain-specific input-box receipt and add their own fixed
coverage-rule tag.

Public projections expose:

- source-ordered inclusive maxima;
- exact total assignment count;
- exact precondition-applicable assignment count;
- `LengthCounterexampleBasis`.

The applicable count makes contradictory and otherwise vacuous establishment
visible. The basis distinguishes the provider-independent total finite-spine
model from validation conditional on the exact retained list of named assumed
provider laws. Complete traversal does not validate those implementations.

The query-owned functions emit no SMT-LIB command, launch no process, and
consume no raw or live `sat`, `unsat`, or `unknown` observation. The query
contributes exact problem association only. A raw status cannot manufacture a
positive receipt, and an association mismatch fails closed.

Applicable-domain establishment remains relative to the checked total
finite-spine model, normalized contract, interpreted candidate, evaluation
limits, and retained provider-law basis. It does not establish source-language
inhabitance, realization, termination, strictness, absence of bottoms or
effects, provider implementation behavior, dictionary evidence, universal
correctness, or permission to prune a candidate.

## Identity and compatibility

The only new canonical byte strings are the receipt tags:

```text
finite-list-spine-length/positive-affine-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/positive-affine-precondition-domain-establishment/v1
```

These are the first versions of the new positive-affine receipt families. They
do not replace or revise the literal-direct receipt tags.

No existing contract, provider inventory, semantic inventory, interpretation
policy, session, candidate, concrete encoding, complete problem, SMT query,
response, protocol, execution, process, ready worker, query run, live
observation, replay, input-box, counterexample, or direct applicable-domain
identity or canonical bytes change. Scalar and product evidence remain
nominally disjoint.

## Characterization map

The focused characterization for this checkpoint covers:

- exact scalar and product receipt tags and nominal separation;
- preservation of literal-direct v1 equality and scale inapplicability;
- direct-bound subset parity through the new rule;
- both equality orientations, sums, positive scales, constants, repeated
  variables, and duplicate-bound minima;
- coefficient-free and unsupported expressions remaining inapplicable;
- literal-false, both false constant-equality orientations, true non-binding
  constant equality, and affine-constant contradictions;
- nullary true, false, and bounded evaluation behavior;
- width, derived-value, Cartesian-product, and indexed evaluation precedence;
- lexicographic first counterexamples and provider-basis retention;
- direct/query receipt parity, exact association failure, and unchanged query
  commands, symbols, requests, and fingerprints;
- public projections, `Eq`, `Ord`, `Show`, `NFData`, hidden constructors, and
  scalar/product coercion rejection.
