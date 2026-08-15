# Strict relational positive-affine Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has an additive, solver-independent applicable-domain entrance for
strict positive-affine relations between compact Length inputs. It recognizes
the exact natural complement of one top-level at-most clause while retaining
all ordinary relations supported by the established relational validator.

The checked-problem entrances are:

- `validateLengthProblemStrictRelationalPositiveAffineApplicableDomain`;
- `validateLengthSpinePairProblemStrictRelationalPositiveAffineApplicableDomain`.

The corresponding query-owned entrances are:

- `validateLengthSMTLibQueryStrictRelationalPositiveAffineApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineApplicableDomain`.

Each entrance returns the existing three-way
`LengthApplicableDomainValidation`: incomplete finite coverage is ordinary
inapplicability, the first independently replayed violation returns the
existing exact counterexample evidence, and complete traversal creates a new
opaque strict-relational receipt.

## Additive compatibility

The strict rule is a separately named fourth applicable-domain policy. It does
not revise or silently strengthen an older entrance:

- `validateLengthProblemInputBox` and its product sibling still validate only
  the caller's explicit source-ordered maxima;
- `validateLengthProblemApplicableDomain` and its product sibling remain the
  literal-direct v1 rule;
- `validateLengthProblemPositiveAffineApplicableDomain` and its product
  sibling remain the literal-ceiling positive-affine rule;
- `validateLengthProblemRelationalPositiveAffineApplicableDomain` and its
  product sibling retain the non-strict relational rule and still give
  `LengthNot` no coverage authority;
- counterexample, raw-input, query-input, origin, simplification, and live
  replay validators retain their prior evidence and observation boundaries.

Every query-owned sibling of those functions is likewise unchanged. In
particular, an older relational function grants a retained strict clause no
bound; a problem which relies on that clause for otherwise missing coverage
remains inapplicable. There is no policy enum, implicit fallback from an older
API, or coercion between receipt families.

## Exact strict-natural proof rule

Contract sealing has already bounded and normalized the precondition. For a
nonnullary problem, the strict scanner visits either the whole precondition or
the immediate clauses of its normalized flat top-level `LengthAll`. It
delegates ordinary `LengthAtMost`, `LengthEqual`, and `LengthTruth False`
clauses to the unchanged relational scanner. The only additional recognized
formula is:

```text
LengthNot (LengthAtMost L R)
```

Over natural-valued Length expressions, its exact complement is:

```text
not (L <= R)  <=>  R + 1 <= L
```

Both sides must be summarized by the existing positive-affine grammar:

```text
A ::= compact-input
    | natural-literal
    | LengthSum [A]
    | LengthScale positive-literal A
```

The implementation increments the arbitrary-precision constant in the exact
summary of `R`, then passes the resulting directed inequality through the
ordinary relational constant and per-input coefficient cancellation. The
successor is proof-only: it does not build a new checked expression, consume a
syntax node, revalidate a public literal, or alter normalized contract syntax.
Consequently `R` may already contain the largest admitted checked literal;
the internal exact summary can safely hold its successor before any derived
maximum is admitted by the established evaluation limits.

No integer or SMT reasoning is being substituted here. The equivalence relies
specifically on both sides denoting naturals. The generated rule enters the
same finite, synchronous, rule-once closure as every ordinary relational rule.

## Strict chaining

Consider the two scalar clauses:

```text
not (5 <= x)
not (x <= y)
```

The first rewrite is:

```text
x + 1 <= 5
x <= 4
```

That constant-right rule seeds `x <= 4`. The second rewrite is:

```text
y + 1 <= x
```

It becomes eligible once the maximum for `x` exists and derives `y <= 3`.
Thus a chain consisting entirely of strict clauses yields source-ordered
maxima `[4, 3]`.

The closure semantics have not changed. Seed rules fire against the empty
bounds map. Every subsequent pass examines all pending rules against one
immutable snapshot, merges all newly derived maxima with `min` only after the
pass, and permanently removes every rule that fired. Ineligible rules retry in
canonical order. The algorithm stops when a pass fires nothing; strict rules
do not turn it into a numeric least-fixed-point computation.

## Exact successor and coefficient cancellation

The product-side clause:

```text
not (x + 3 <= 2*x)
```

first becomes:

```text
2*x + 1 <= x + 3
```

Exact common-coefficient cancellation then leaves:

```text
x + 1 <= 3
x <= 2
```

Successor insertion occurs before cancellation, so it cannot be lost when a
variable occurs on both sides. Constants and coefficients use
arbitrary-precision naturals during summarization and cancellation; the
literal-ceiling validator's saturating summary is not reused.

## Scalar query-owned example

Suppose `checkedQuery` retains an exact scalar candidate which satisfies its
postcondition whenever the following precondition applies:

```haskell
input0 = LengthVariable (LengthInput 0)
input1 = LengthVariable (LengthInput 1)

precondition = LengthAll
  [ LengthNot
      (LengthAtMost (LengthLiteral 5) input0)
  , LengthNot
      (LengthAtMost input0 input1)
  ]

validation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthStrictRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([4, 3], 20, 10)
  Right (LengthApplicableDomainCounterexample _) -> False
  Right (LengthApplicableDomainInapplicable _) -> False
  Left _ -> False
```

The derived box is `[0..4] x [0..3]`, so the finite-box verifier visits 20
assignments. The normalized precondition is exactly `input1 < input0 < 5`;
ten assignments are applicable. A candidate violation would return the
ordinary `ValidatedLengthCounterexample` instead of the strict establishment
receipt.

## Binary-product query-owned example

The nominal product entrance applies the same input-domain proof without
sharing scalar evidence. If `checkedPairQuery` retains a safe exact
binary-spine candidate:

```haskell
input = LengthVariable (LengthSpinePairInput 0)

precondition = LengthNot $ LengthAtMost
  (LengthSum [input, LengthLiteral 3])
  (LengthScale 2 input)

validation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedPairQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([2], 3, 3)
  Right (LengthApplicableDomainCounterexample _) -> False
  Right (LengthApplicableDomainInapplicable _) -> False
  Left _ -> False
```

The rewrite and cancellation above derive maximum `[2]`. All three assignments
zero, one, and two satisfy the strict precondition. The corresponding
checked-problem entrance is
`validateLengthSpinePairProblemStrictRelationalPositiveAffineApplicableDomain`.

## Deliberate exclusions

This checkpoint is not a general negation-normalization or complement engine.
The new rule excludes:

- `LengthNot (LengthEqual left right)`, which is disequality rather than one
  directed strict order;
- a `LengthNot` around truth, conjunction, another retained logical shape, or
  anything other than one immediate `LengthAtMost`;
- strict comparisons nested below a non-top-level formula;
- natural monus, minimum, maximum, quotient, modulo, and conditional
  expressions on either side;
- a retained zero-factor `LengthScale` or any other shape outside the admitted
  positive-affine summary (ordinary checked normalization reduces admitted
  zero scales to a literal before this scanner runs);
- result variables, which checked preconditions reject independently, and any
  compact input position outside the exact problem width;
- solver models, solver statuses, candidate implementations, and postcondition
  expressions as sources of an input maximum.

If either side of a candidate strict clause is unsupported, the entire clause
contributes no relational rule and no partial bound. It is not an operational
failure. The original clause remains part of actual precondition evaluation
when another supported clause establishes a complete finite box.

## Contradiction, inapplicability, and precedence

The strict scalar and product entrances retain this order:

1. The checked compact-input count is compared with the input-box width limit
   before the precondition is demanded.
2. A nullary problem bypasses rule extraction, derives maxima `[]`, and sends
   the ordinary singleton assignment `[]` to the box verifier.
3. A nonnullary problem scans the normalized top-level clauses. Unsupported
   clauses are ignored for coverage; `LengthTruth False` establishes immediate
   contradiction.
4. Collected ordinary and strict rules enter the existing closure. A fired
   rule whose residual left constant exceeds the maximum attainable right side
   establishes propagated contradiction.
5. Either form of contradiction takes precedence over otherwise missing input
   bounds and selects one zero maximum per compact input. The verifier still
   evaluates the singleton all-zero carrier; successful vacuous evidence has
   total count one and applicable count zero.
6. Without contradiction, the first compact input absent from the closed bound
   map produces `LengthApplicableDomainInputUpperBoundMissing` in the ordinary
   `LengthApplicableDomainInapplicable` arm.
7. Derived maxima are admitted left to right under the existing evaluation
   value limits. The saturating Cartesian assignment count is admitted before
   assignment replay.
8. Replay remains last-input-fastest lexicographic. The first indexed
   evaluation failure is a closed `Left`; the first postcondition violation is
   the existing exact scalar or product counterexample.
9. Only complete traversal constructs a strict-relational establishment
   receipt. A query-owned entrance then replays either authoritative evidence
   arm against the exact behavioral problem retained by its query before
   releasing the payload.

An inapplicability result contains no evidence to associate, so it returns
without a query-association replay. Width, derived-value, Cartesian-product,
evaluation, enumeration, and association failures retain the existing scalar
or nominal product error vocabulary.

## Receipt and query authority

Complete scalar traversal produces opaque
`ValidatedLengthStrictRelationalPositiveAffineApplicableDomain`. Complete
product traversal produces the nominally distinct
`ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain`.
Their public projections expose:

- source-ordered inclusive maxima;
- exact total assignment count;
- exact precondition-applicable assignment count;
- the exact `LengthCounterexampleBasis`.

The constructors remain private. Each receipt wraps the corresponding complete
finite-box receipt, so scalar and binary-product behavioral evidence cannot be
coerced into each other. Provider-backed establishment remains conditional on
the exact named assumed provider laws retained by that basis.

The query-owned wrappers emit no SMT-LIB command, launch no process, and
consume no raw or live `sat`, `unsat`, or `unknown` observation. A query
contributes only its exact retained behavioral problem and the authority to
check association. Solver status cannot create, upgrade, or substitute for a
strict-relational receipt.

Establishment remains relative to the checked total finite-spine model,
normalized contract, interpreted candidate, bounded evaluation, and retained
provider-law basis. It does not establish source-language inhabitance,
realization, termination, strictness, absence of bottoms or effects, concrete
provider behavior, dictionary evidence, universal correctness, or permission
to prune a candidate.

## Identity boundaries

The only new canonical byte strings are the two receipt schema tags:

```text
finite-list-spine-length/strict-relational-positive-affine-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-precondition-domain-establishment/v1
```

They are exposed by
`lengthStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag` and
`lengthSpinePairStrictRelationalPositiveAffineApplicableDomainValidationSchemaTag`.
These are first versions of new nominal receipt families, not revisions of an
older applicable-domain tag.

The internal successor never enters normalized contract syntax or an SMT
query. No existing contract, provider inventory, semantic inventory,
interpretation policy, session, candidate, concrete encoding, complete
problem, SMT query, response, protocol, execution, process, ready worker,
query run, live observation, counterexample, input-box, direct applicable-
domain, positive-affine, or relational positive-affine identity or canonical
bytes change.

## Characterization boundary

The focused behavior for this checkpoint is defined by:

- old direct, literal-ceiling, and relational functions retaining strict-only
  inapplicability and their exact receipt tags;
- exact scalar and product strict-relational tags, opaque constructors,
  `NFData`, projections, and nominal non-coercibility;
- the natural complement `not (L <= R)` becoming exactly `R + 1 <= L`;
- scalar strict-only bound seeding and multi-hop propagation;
- successor-before-cancellation behavior on both constants and coefficients;
- preservation of synchronous snapshot and rule-once closure semantics;
- unsupported negations and unsupported affine subtrees granting no partial
  coverage;
- syntactic and propagated contradiction winning over missing coverage;
- nullary, width, derived-value, Cartesian-product, indexed-evaluation, first-
  counterexample, and query-association precedence;
- scalar/product direct-to-query evidence parity and retained provider basis;
- unchanged query commands, input symbols, value requests, fingerprints, and
  all older receipt identities.
