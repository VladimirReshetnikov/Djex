# Relational positive-affine Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has a solver-independent applicable-domain rule which can propagate
finite upper bounds through relations between positive-affine compact inputs.
It is an additive third entrance beside the original literal-direct and
literal-ceiling positive-affine rules.

The exact checked-problem entrances are:

- `validateLengthProblemRelationalPositiveAffineApplicableDomain`;
- `validateLengthSpinePairProblemRelationalPositiveAffineApplicableDomain`.

The query-owned entrances are:

- `validateLengthSMTLibQueryRelationalPositiveAffineApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryRelationalPositiveAffineApplicableDomain`.

Every entrance reuses `LengthApplicableDomainValidation`. Failure to derive a
bound for every nonnullary compact input is ordinary
`LengthApplicableDomainInapplicable`; the first independently replayed
violation is the existing scalar or product counterexample; and complete
traversal releases a new opaque relational establishment receipt.

## Additive compatibility

The older validators are unchanged:

- `validateLengthProblemApplicableDomain` and its product sibling remain the
  literal-direct v1 rule. Equality and arithmetic-derived bounds remain
  inapplicable there.
- `validateLengthProblemPositiveAffineApplicableDomain` and its product
  sibling remain the literal-ceiling positive-affine rule. They summarize only
  an affine bounded side compared with a natural literal; they do not propagate
  bounds through an affine expression on the other side.

Their query-owned wrappers, opaque receipts, tags, canonical bytes, failure
ordering, and authority retain their exact prior meanings. Relational
propagation is available only through the explicitly named relational
entrances and produces a nominally distinct receipt.

## Relational rule construction

Contract sealing has already bounded and normalized the precondition. For a
nonnullary problem, the scanner examines the precondition itself or the
immediate clauses of its normalized flat top-level `LengthAll`. Both sides of
a recognized relation must have this grammar:

```text
A ::= compact-input
    | natural-literal
    | LengthSum [A]
    | LengthScale positive-literal A
```

The complete top-level relation must be either:

```text
A <= A
A == A
```

Unsupported clauses and subtrees contribute no rule and no partial coverage
authority. This includes natural monus, minimum, maximum, quotient, modulo,
conditionals, negation, and non-top-level relations.

Each side is summarized exactly as a natural constant plus a finite map of
compact-input coefficients. Unlike the literal-ceiling rule, this pass does
not saturate its arithmetic: arbitrary-precision naturals preserve exact
cancellation between the two sides. Common constants and the common part of
every same-input coefficient are removed. The result is a directed residual
rule:

```text
cL + sum (ai * xi) <= cR + sum (bj * xj)
```

No compact input remains on both sides. `LengthAtMost` contributes one such
rule. `LengthEqual` contributes two rules in canonical order: normalized
left-to-right first, then right-to-left. `LengthTruth False` is an immediate
contradiction. Other formulas remain part of actual precondition replay even
when they grant no coverage rule.

## Exact synchronous, rule-once propagation

Propagation is deliberately a finite rule-consumption process, not a numeric
least-fixed-point solver.

First, every rule with no residual right-side input is partitioned into the
seed pass, which uses the empty bounds snapshot and the same rule-once merge
mechanics. For every later pass:

1. one immutable map of already established input maxima is the bounds
   snapshot for the entire pass;
2. a pending rule is eligible only when every residual right-side input has a
   maximum in that snapshot;
3. an eligible rule computes the exact right-side maximum
   `R = cR + sum (bj * maximum_j)`;
4. if `cL > R`, the conjunction is contradictory;
5. otherwise every positive residual left coefficient derives the necessary
   bound `xi <= (R - cL) quot ai`;
6. all bounds derived in the pass are combined with `min`, then merged with
   the snapshot only after every pending rule has been examined;
7. every eligible rule is permanently removed, even if its bound did not
   tighten an existing maximum, while skipped rules retry in canonical stored
   order.

If a pass fires at least one rule, the retained rules are tried against the new
snapshot. If none fires, propagation stops. Successful progress therefore
consumes at least one rule, so work is bounded by the finite rule set rather
than the magnitude of a numeric bound.

The immutable snapshot is observable and intentional. Given stored clauses:

```text
x <= y
y <= 10
y <= z
z <= 2
```

the seed pass establishes `y <= 10` and `z <= 2`. In the next pass, both
`x <= y` and `y <= z` observe that same snapshot. They derive `x <= 10` and
`y <= 2`, respectively, and are then removed. The result is the sound,
deliberately nonleast box `[10, 2, 2]`; the first rule is not fired again to
tighten `x` to two.

Multi-hop discovery still retries rules whose right-side inputs were initially
unknown. For example:

```text
x <= 2*y
y <= z + 1
z <= 2
```

derives `z <= 2`, then `y <= 3`, then `x <= 6` in successive passes. Every
derived maximum is sound: the right-side maximum is already established when
its rule fires, and all omitted left-side terms are nonnegative.

## Scalar API example

Suppose `checkedQuery` was sealed from a checked candidate which satisfies its
postcondition whenever this precondition applies:

```haskell
input0 = LengthVariable (LengthInput 0)
input1 = LengthVariable (LengthInput 1)

precondition = LengthAll
  [ LengthEqual input0 input1
  , LengthAtMost input1 (LengthLiteral 5)
  ]

validation =
  validateLengthSMTLibQueryRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([5, 5], 36, 6)
  _ -> False
```

The constant-right inequality seeds `input1 <= 5`. The equality direction
`input0 <= input1` then derives `input0 <= 5`; its reverse is also safe and
fires once. The existing finite-box verifier traverses 36 assignments, of
which the six diagonal assignments satisfy the precondition.

## Binary-product API example

The nominal product validator applies the same input-domain rule while
retaining product-specific behavioral evidence. If `checkedPairQuery` retains
a safe exact binary-spine candidate:

```haskell
input = LengthVariable (LengthSpinePairInput 0)

precondition = LengthAtMost
  (LengthScale 2 input)
  (LengthSum [input, LengthLiteral 1])

validation =
  validateLengthSpinePairSMTLibQueryRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedPairQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthSpinePairRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([1], 2, 2)
  _ -> False
```

Exact coefficient cancellation reduces `2*input <= input + 1` to
`input <= 1`. The two assignments in `[0..1]` both satisfy the precondition.
The corresponding checked-problem entrance is
`validateLengthSpinePairProblemRelationalPositiveAffineApplicableDomain`.

## Contradiction, inapplicability, and failure order

`LengthTruth False` establishes contradiction immediately. An eligible rule
also establishes contradiction when its residual left constant exceeds the
maximum attainable residual right side. For example, `x + 2 <= y` becomes
contradictory once `y <= 1` is established. A contradiction takes precedence
over otherwise missing bounds and selects one zero maximum per compact input.
The existing verifier still checks that singleton all-zero coverage carrier;
successful vacuous establishment records total assignment count one and
applicable assignment count zero.

Without contradiction, the first compact input absent from the final bound map
produces `LengthApplicableDomainInputUpperBoundMissing` inside the ordinary
inapplicability arm. The rule does not speculate through unresolved cycles or
invoke a solver to fill a gap.

A nullary problem bypasses extraction, derives maxima `[]`, and validates its
ordinary singleton assignment `[]`. Its applicable count is zero or one.

All admission and behavioral work remains in the established finite-box
verifier:

1. input width is rejected before precondition extraction;
2. derived maxima are checked left-to-right under the existing value limits;
3. the Cartesian assignment cap is admitted before assignment replay;
4. assignments retain last-input-fastest lexicographic order and indexed
   evaluation failures;
5. the first postcondition violation returns the ordinary scalar or product
   counterexample;
6. only complete traversal constructs positive evidence.

## Receipt authority

Complete scalar traversal produces the opaque
`ValidatedLengthRelationalPositiveAffineApplicableDomain`. Product traversal
produces the nominally separate
`ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain`. Public
projections expose:

- source-ordered inclusive maxima;
- exact total assignment count;
- exact precondition-applicable assignment count;
- `LengthCounterexampleBasis`.

The receipts privately wrap the corresponding complete finite-box evidence.
Scalar and product evidence remain nominally disjoint and their constructors
remain hidden.

The query-owned wrappers emit no SMT-LIB command, launch no process, and
consume no raw or live `sat`, `unsat`, or `unknown` observation. Their query
contributes exact behavioral-problem association only. Both counterexample and
establishment evidence are replayed against that exact retained problem before
their payload can be released.

Applicable-domain establishment remains relative to the checked total
finite-spine model, normalized contract, interpreted candidate, bounded
evaluation, and exact retained provider-law basis. It does not establish
source-language inhabitance, realization, termination, strictness, absence of
bottoms or effects, concrete provider behavior, dictionary evidence,
universal correctness, or permission to prune a candidate.

## Identity and compatibility

The only new canonical byte strings are the two receipt tags:

```text
finite-list-spine-length/relational-positive-affine-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/relational-positive-affine-precondition-domain-establishment/v1
```

They are exposed by
`lengthRelationalPositiveAffineApplicableDomainValidationSchemaTag` and
`lengthSpinePairRelationalPositiveAffineApplicableDomainValidationSchemaTag`.
They are first versions of new nominal receipt families, not revisions of the
direct-v1 or literal-ceiling positive-affine tags.

No existing contract, provider inventory, semantic inventory, interpretation
policy, session, candidate, concrete encoding, complete problem, SMT query,
response, protocol, execution, process, ready worker, query run, live
observation, replay, input-box, counterexample, direct applicable-domain, or
positive-affine applicable-domain identity or canonical bytes change.

## Characterization map

The focused characterization for this checkpoint covers:

- preservation of direct and literal-ceiling positive-affine behavior under a
  third nominal receipt family;
- exact scalar and product receipt tags, hidden constructors, `NFData`, and
  nominal non-coercibility;
- common constant and coefficient cancellation, including
  `2*x <= x + 1`;
- equality in both orientations and propagation from either side's seed;
- source-ordered multi-hop retry across successive snapshots;
- synchronous same-pass isolation and deliberately nonleast rule-once closure;
- immediate and propagated contradiction;
- unsupported and unresolved clauses remaining ordinary inapplicability;
- width, derived-value, Cartesian-product, and indexed-evaluation precedence;
- first scalar and product counterexamples and retained provider basis;
- direct/query evidence parity, exact association failure, and unchanged query
  commands, symbols, value requests, and fingerprints.
