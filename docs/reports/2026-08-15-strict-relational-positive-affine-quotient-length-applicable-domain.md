# Strict relational positive-affine quotient Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has an additive, solver-independent applicable-domain entrance for
exact natural-number consequences of one positive-literal quotient at a
top-level relation operand's root. It retains every clause recognized by the
strict relational positive-affine validator and adds a deliberately narrow
quotient rule before the established affine closure and finite-box replay.

The checked-problem entrances are:

- `validateLengthProblemStrictRelationalPositiveAffineQuotientApplicableDomain`;
- `validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientApplicableDomain`.

The query-owned entrances are:

- `validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientApplicableDomain`.

Each returns the existing three-way `LengthApplicableDomainValidation`.
Incomplete coverage is ordinary inapplicability, the first independently
replayed violation returns the existing exact counterexample evidence, and
complete traversal creates a new opaque quotient-domain receipt.

## Additive compatibility

This is a separately named successor policy. It does not revise or silently
strengthen any older entrance:

- explicit input-box validation still uses only caller-supplied maxima;
- the direct v1 validator still recognizes only its literal-direct clauses;
- the positive-affine validator still derives only literal-ceiling bounds;
- the relational validator still excludes strict complements and quotient
  expressions;
- the strict relational validator still adds only
  `not (left <= right) <=> right + 1 <= left` for quotient-free affine sides;
- counterexample, raw-input, origin, simplification, query, observation, and
  live replay boundaries retain their prior evidence and authority.

In particular, a quotient-only ceiling remains
`LengthApplicableDomainInapplicable` through the predecessor strict entrance.
There is no policy enum, implicit fallback, receipt coercion, or behavioral
change selected merely because a checked problem contains `LengthQuotient`.

## Exact natural quotient rules

Let `q_d(A)` denote natural quotient `A quot d`, where the checked divisor
`d` is positive. Let `A` and `B` be expressions admitted by the exact
relational positive-affine summarizer. The new scanner uses these four
equivalences:

| Retained directed clause | Exact proof-only affine clause |
| --- | --- |
| `q_d(A) <= B` | `A <= d*B + (d - 1)` |
| `A <= q_d(B)` | `d*A <= B` |
| `not (q_d(A) <= B)` | `d*(B + 1) <= A` |
| `not (A <= q_d(B))` | `B + 1 <= d*A` |

All four are exact specifically over naturals. For example,
`q_d(A) <= B` means `A < d*(B + 1)`, which is the same natural interval as
`A <= d*B + d - 1`. Likewise `A <= q_d(B)` is exactly `d*A <= B` for a
positive natural divisor.

The implementation scales and increments arbitrary-precision proof summaries.
It does not construct a larger checked expression, spend a syntax node, or
revalidate the multiplied constants against the public literal limit. Checked
syntax has already proved `d > 0`; the private extractor nevertheless rejects
a zero divisor defensively. Quotient by one and a quotient of a literal are
normally removed during checked normalization before this scanner runs.

## Admitted affine operands and equality

The root quotient's dividend and its opposite relation operand must each fit
the existing exact positive-affine grammar:

```text
A ::= compact-input
    | natural-literal
    | LengthSum [A]
    | LengthScale positive-literal A
```

Ordinary constant and per-input coefficient cancellation runs only after the
quotient equivalence has been applied. Generated rules then enter the
unchanged synchronous, rule-once relational closure.

An admitted equality containing exactly one root quotient contributes both
directed inequalities in a fixed order:

```text
L = R  ==>  L <= R, then R <= L
```

The orientation-specific quotient law is applied separately to each
direction, so equality emits at most two affine rules. The operation is
all-or-nothing: if either direction cannot be summarized, the entire equality
contributes no rule. A negated equality remains excluded because it is a
disequality, not a conjunction of two directed orders.

## Worked scalar consequences

For one input `x`, the clause

```text
q_3(2*x + 1) <= 2
```

becomes:

```text
2*x + 1 <= 3*2 + 2
x <= 3
```

The derived maximum is `[3]`. The verifier visits four assignments, all four
of which satisfy the precondition.

Equality uses both directions. The clause

```text
q_3(x) = 4
```

becomes the exact interval:

```text
x <= 14
12 <= x
```

The derived box is `[0..14]`, so total count is 15 and applicable count is
three: `x` is 12, 13, or 14.

The strict right-root orientation also yields a direct ceiling:

```text
not (4 <= q_3(x))
```

becomes `x + 1 <= 12`, hence `x <= 11`. Its maximum is `[11]`, and all 12
visited assignments are applicable.

## Relational propagation and counts

Consider two inputs `x` and `y` with:

```text
x <= q_3(y)
y <= 8
```

The quotient rule yields `3*x <= y`. The ordinary ceiling first bounds `y`
at eight; the established closure then derives `x <= 2`. The source-ordered
maxima are `[2, 8]`, the rectangular traversal contains 27 assignments, and
18 satisfy `3*x <= y`.

The two strict orientations are equally available:

```text
not (q_3(y) <= x), y <= 8  ==>  3*(x + 1) <= y
not (y <= q_3(x)), y <= 2  ==>  x + 1 <= 3*y
```

They derive maxima `[1, 8]` and `[5, 2]`, respectively. Each box contains 18
assignments and nine applicable assignments.

A quotient-derived lower bound can also prove contradiction. The conjunction

```text
not (q_3(x) <= 4)
x <= 14
```

rewrites its first clause to `15 <= x`. Closure therefore establishes that
the precondition is contradictory. The validator selects the ordinary
all-zero carrier with maximum `[0]`, total count one, and applicable count
zero; it does not manufacture an empty Cartesian product or skip replay.

## Binary-product query-owned example

The nominal product entrance applies the same input-domain proof without
sharing scalar evidence. If `checkedPairQuery` retains a safe exact
binary-spine candidate under:

```haskell
input = LengthVariable (LengthSpinePairInput 0)

precondition = LengthAtMost
  (LengthQuotient 3 input)
  (LengthLiteral 2)

validation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedPairQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount
        receipt
    , validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([8], 9, 9)
  _ -> False
```

The first law gives `input <= 3*2 + 2`, hence maximum `[8]`. All nine values
in the derived box satisfy the precondition. The checked-problem sibling is
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientApplicableDomain`.

## Deliberate exclusions

This checkpoint is not a recursive quotient eliminator or general Presburger
normalizer. A quotient clause is admitted only when exactly one relation
operand is a `LengthQuotient` at its root. The scanner deliberately excludes:

- a quotient nested inside another quotient;
- a quotient embedded below `LengthSum`, `LengthScale`, or any other operand
  constructor rather than occurring at the relation root;
- root quotients on both relation operands;
- a root quotient whose dividend or opposite operand contains quotient,
  modulo, natural monus, minimum, maximum, a conditional, or any other
  non-affine subtree;
- negated equality, nested Boolean structure, and quotient relations below a
  non-top-level formula;
- a retained zero-factor scale or other expression outside the positive-affine
  summary (ordinary checked normalization reduces admitted zero scales first);
- result variables, out-of-width compact inputs, solver models, solver
  statuses, candidates, and postconditions as sources of a maximum.

Any unsupported part makes the whole candidate quotient clause contribute no
rule. There is no partial consequence from the supported side and no
operational error. The original normalized clause remains part of concrete
precondition evaluation if other supported clauses establish a complete box.

Quotient-free clauses delegate to the strict predecessor scanner unchanged.
Thus all previously admitted `LengthAtMost`, `LengthEqual`, immediate strict
at-most complements, and `LengthTruth False` behavior is retained under the
new receipt, while every older entrance remains literal.

## Closure and failure precedence

The scalar and product entrances preserve this order:

1. The checked compact-input count is compared with the input-box width limit
   before precondition extraction or evaluation-limit demand.
2. A nullary problem bypasses rule extraction, derives maxima `[]`, and sends
   the singleton assignment `[]` to the existing box verifier.
3. A nonnullary problem scans the whole precondition or the immediate clauses
   of its normalized flat top-level `LengthAll`. Unsupported clauses are
   ignored for coverage; `LengthTruth False` establishes contradiction.
4. Ordinary strict-relational and admitted quotient rules enter the existing
   closure in canonical clause order. Equality contributes left-to-right and
   then right-to-left rules. Rules fire against immutable pass snapshots,
   merge new maxima with `min` after the pass, and fire at most once.
5. Syntactic or propagated contradiction wins over otherwise missing bounds
   and selects one zero maximum per compact input.
6. Without contradiction, the first source-ordered input absent from the
   closed bound map returns `LengthApplicableDomainInputUpperBoundMissing` in
   the ordinary `LengthApplicableDomainInapplicable` arm.
7. Derived maxima are admitted left to right under the existing evaluation
   value limit. Saturating Cartesian cardinality is admitted before replay.
8. Replay remains last-input-fastest lexicographic. The first indexed
   evaluation failure is a closed `Left`; the first postcondition violation
   returns the existing exact scalar or product counterexample evidence.
9. Only complete traversal creates the new receipt. A query-owned wrapper then
   replays either evidence arm against the exact behavioral problem retained
   by its query before releasing the payload.

Inapplicability contains no evidence and therefore returns without query
association replay. Width, value, product, evaluation, enumeration, and
association failures retain the existing scalar or nominal product error
vocabulary; the quotient successor adds no new failure constructor.

## Receipt and query authority

Complete scalar traversal creates opaque
`ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain`.
Complete product traversal creates nominally distinct
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain`.
Their public projections expose:

- source-ordered inclusive maxima through
  `validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums`
  or
  `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainInclusiveMaximums`;
- exact total assignment count through
  `validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount`
  or
  `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainAssignmentCount`;
- exact precondition-applicable assignment count through
  `validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount`
  or
  `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainApplicableAssignmentCount`;
- the exact `LengthCounterexampleBasis` through
  `validatedLengthStrictRelationalPositiveAffineQuotientApplicableDomainBasis`
  or
  `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainBasis`.

Both constructors remain private; both types have structural `Eq`, `Ord`, and
`Show` and strict `NFData`. Each wraps its corresponding established
finite-box receipt, so scalar and product evidence cannot be interchanged.
Provider-backed evidence remains conditional on the exact assumed provider
laws retained by its basis.

The query-owned wrappers issue no SMT-LIB command, launch no process, and
consume no raw or live `sat`, `unsat`, or `unknown`. A sealed query contributes
only its retained behavioral problem and exact association authority. Direct
and query-owned validation therefore use the same extraction and replay; a
solver observation cannot create, upgrade, or substitute for the receipt.

Establishment remains relative to the checked total finite-spine model,
normalized contract, interpreted candidate, bounded evaluation, and retained
provider-law basis. It does not establish source-language inhabitance,
realization, termination, strictness, absence of bottoms or effects, provider
implementation behavior, dictionary evidence, universal correctness, or
permission to prune a candidate.

## Identity boundary

The only new canonical byte strings are the two receipt schema tags:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-precondition-domain-establishment/v1
```

They are exposed by
`lengthStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag`
and
`lengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomainValidationSchemaTag`.
These are first versions of new nominal receipt families, not revisions of
the predecessor strict-relational tags.

The quotient rewrites exist only as private proof summaries. They do not enter
normalized contract syntax or the SMT query plan. Existing query check bytes,
input symbols, input-value request bytes, and fingerprints remain exact.
Existing contract, provider inventory, semantic inventory, session,
candidate, encoding, complete problem, response, protocol, execution,
process, worker, run, observation, counterexample, explicit-box, direct,
positive-affine, relational, and strict-relational identities and canonical
bytes are unchanged.

## Characterization boundary

The focused behavior for this checkpoint is defined by:

- all four oriented natural quotient laws and their exact affine summaries;
- equality expansion in both orientations, left-to-right before reverse;
- scalar and product examples with exact maxima and total/applicable counts;
- quotient-free parity with the strict predecessor;
- whole-clause exclusion of nested, embedded, both-root, and unsupported
  quotient shapes;
- contradiction before missing coverage, then value, Cartesian-product,
  indexed-evaluation, counterexample, and query-association precedence;
- exact scalar/product schema tags, opaque constructors, `NFData`, four
  projections, and nominal non-coercibility;
- direct/query evidence parity and unchanged sealed query bytes and identity;
- every older entrance retaining its exact surface, behavior, authority,
  receipt family, and bytes.

## Remaining gaps and follow-up

Root extrema and natural-monus consequence rules, bounded Boolean finite
unions, and further launch hardening remain separate design gaps. The
constraint alternatives need their own soundness audits, explicit work caps,
public receipts, schema tags, and compatibility boundaries; launch hardening
needs its own execution-identity and failure-contract audit. This checkpoint
does not rank them or reserve an API version. A later audit should compare
their authority value and implementation cost before freezing the next
surface.

Any future Boolean-union design must validate explicitly admitted branch
boxes and replay their canonical finite union. It must not widen them into one
componentwise-maximum rectangle: that rectangle can introduce assignments
outside every alternative and needlessly multiply evaluation work. Root
minimum and maximum have both conjunctive and disjunctive orientations, while
natural monus has orientation-specific boundary cases, so none is silently
folded into this quotient receipt.
