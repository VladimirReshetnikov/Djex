# Root-extrema Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has an additive, solver-independent applicable-domain entrance for
four exact conjunctive consequences of one immediate normalized binary
minimum or maximum at a relation operand's root. It is the cumulative
successor to strict relational positive-affine quotient coverage: every clause
without an immediate root extremum delegates to that predecessor unchanged.

The scalar entrances are:

- `validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`;
- `validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`.

The nominal binary-product entrances are:

- `validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`.

Each returns the existing three-way `LengthApplicableDomainValidation`.
Incomplete coverage is ordinary inapplicability. A first independently
replayed violation returns the existing scalar or product counterexample
evidence. Complete traversal creates a new opaque, nominal root-extrema
receipt.

## Additive compatibility

This is a separately named successor, not a change to any earlier validator.
The direct, positive-affine, relational, strict-relational, and root-quotient
entrances retain their exact syntax, behavior, failure order, receipt types,
and tags. In particular:

- the root-quotient predecessor still excludes every minimum and maximum from
  its affine summaries;
- a problem requiring an extrema consequence remains inapplicable through
  that predecessor;
- no formula constructor implicitly selects the successor;
- no policy enum, fallback, or receipt coercion was introduced;
- counterexample, raw-input, origin, simplification, explicit-box, query,
  observation, and live replay keep their existing authority.

Every supported quotient-free or root-quotient clause is routed through the
same predecessor clause scanner and closure semantics. Selecting the successor
can create only its new nominal establishment receipt; it cannot relabel a
predecessor receipt.

## The four exact natural laws

Let `A`, `B`, and `C` be exact positive-affine natural expressions. The new
scanner recognizes exactly four directed formula shapes:

| Normalized formula | Two proof-only affine consequences |
| --- | --- |
| `max(A, B) <= C` | `A <= C`; `B <= C` |
| `C <= min(A, B)` | `C <= A`; `C <= B` |
| `not (min(A, B) <= C)` | `C + 1 <= A`; `C + 1 <= B` |
| `not (C <= max(A, B))` | `A + 1 <= C`; `B + 1 <= C` |

Each row is an equivalence over naturals. The first two are the defining
conjunctive order properties of maximum and minimum. The strict rows use the
exact natural equivalence `not (L <= R) <=> R + 1 <= L` before decomposing the
extremum.

The successors and extrema are proof-only. The implementation changes exact
arbitrary-precision affine summaries and then invokes the established
constant and per-input coefficient cancellation. It does not construct a new
checked `LengthExpression`, spend a syntax node, revalidate a derived public
literal, or change the normalized contract.

## All-or-nothing three-affine admission

Every one of the three operands must fit the predecessor's exact grammar:

```text
A ::= compact-input
    | natural-literal
    | LengthSum [A]
    | LengthScale positive-literal A
```

The scanner summarizes the normalized first extremum child, second child, and
opposite relation operand before constructing the two rules. If any summary
fails, the entire candidate clause contributes no coverage rule. There is no
partial rule from whichever child happened to be easy.

This is important for mixed syntax. For example:

```text
q_2(x) <= min(x, 4)
```

has an immediate root minimum, so the successor owns its admission decision.
The quotient opposite is not positive-affine, and the whole clause is ignored;
it does not emit the `x`-child rule or fall through to the root-quotient
scanner. The original checked clause remains in concrete precondition replay
if other clauses establish a complete box.

## Normalization and canonical order

Admission runs after checked contract normalization. Raw source spelling does
not define the scanner's shape or order:

- a same-kind minimum or maximum is flattened;
- all literal children are combined into one literal;
- duplicate children are removed and the retained children are sorted;
- the canonical extrema children are left-associated;
- equality operands are sorted unless the equality folds to truth;
- top-level conjunctions are flattened, truths removed, duplicates removed,
  and remaining clauses sorted.

Consequently an all-literal extremum can fold to one literal and delegate to
the predecessor. A retained extremum of three or more canonical terms is a
left-associated n-ary shape whose nested extremum child cannot be summarized;
the clause is ignored atomically. Merely writing a binary outer constructor in
raw syntax does not bypass this boundary.

The successor traverses normalized conjuncts in their canonical order. Within
one admitted binary extremum, it emits the normalized first-child consequence
and then the normalized second-child consequence. The collector preserves
that clause/component order when it hands rules to closure. These rules alter
no canonical contract or query bytes, but the fixed ordering keeps failure and
closure behavior deterministic.

## Equality: one necessary direction, either root position

An equality with exactly one root extremum yields the same two necessary
consequences regardless of which normalized equality side contains it:

```text
max(A, B) = C  ==>  A <= C and B <= C
C = max(A, B)  ==>  A <= C and B <= C

min(A, B) = C  ==>  C <= A and C <= B
C = min(A, B)  ==>  C <= A and C <= B
```

Only that conjunctive half is admitted. The other half of a maximum equality
would be `C <= max(A,B)`, which is `C <= A or C <= B`; the other half of a
minimum equality would be `min(A,B) <= C`, which is likewise disjunctive. The
current rectangle authority has no branch-union receipt, so neither half is
approximated as a conjunction.

For example:

```text
2*x + 1 = min(x + 5, 7)
```

derives `2*x + 1 <= x + 5` and `2*x + 1 <= 7`, hence maximum `[3]`.
The finite verifier visits four assignments and records only `x = 3` as
applicable. Reversing the raw equality spelling yields the same receipt
projections after normalization.

The one-way boundary is observable. `min(x,100) = 5` gives necessary lower
bounds but no upper bound, so the result is
`LengthApplicableDomainInputUpperBoundMissing 0`. The tautology
`max(x,x+1) = x+1` also remains missing rather than becoming a false
contradiction through an unsafe disjunctive decomposition.

An equality with root extrema on both sides is ignored as a whole. Negated
equality remains unsupported.

## Worked direct-law examples

The maximum-at-most law gives:

```text
max(x, 2*x + 1) <= 5
```

The child rules are `x <= 5` and `2*x + 1 <= 5`; exact cancellation derives
maximum `[2]`. All three assignments in `[0..2]` satisfy the precondition.

The at-most-minimum law gives:

```text
2*x + 1 <= min(x + 5, 7)
```

Its rules derive `x <= 4` and `x <= 3`, so the tight maximum is `[3]`.
All four assignments are applicable.

The strict-minimum law gives:

```text
not (min(x + 4, 9) <= 2*x)
```

It becomes `2*x + 1 <= x + 4` and `2*x + 1 <= 9`. The first is tighter and
derives `[3]`; all four assignments are applicable.

The strict-maximum law gives:

```text
not (5 <= max(2*x, x + 1))
```

It becomes `2*x + 1 <= 5` and `x + 2 <= 5`, so the maximum is `[2]` and all
three assignments are applicable.

## Propagation and rule-once closure

Extrema consequences enter the existing relational closure. Consider:

```text
max(x, y) <= z
z <= 4
```

The first clause contributes `x <= z` and `y <= z`. The constant-right second
clause seeds `z <= 4`; on the next pass both component rules become eligible
and derive `x <= 4` and `y <= 4`. The source-ordered maxima are `[4,4,4]`.
The rectangular traversal contains 125 assignments, and 55 satisfy the
original precondition.

Closure remains synchronous and deliberately nonleast:

1. constant-right rules are partitioned as seeds while preserving order;
2. each pass examines pending rules in canonical order against one immutable
   bounds snapshot;
3. every eligible rule fires once and is permanently removed;
4. all results from the pass merge afterward with `min`;
5. ineligible rules retry until one pass fires nothing.

A numeric tightening cycle therefore cannot repeatedly fire toward a least
fixed point. Every productive step consumes a still-pending rule. This is the
same sound, bounded consequence closure used by the relational, strict, and
root-quotient predecessors. Accordingly, the delegated predecessor chain
`x <= y, y <= 10, y <= z, z <= 2` still yields `[10,2,2]`, not the numeric
least box `[2,2,2]`.

## Public receipt surface

Scalar establishment returns the opaque type:

```text
ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
```

Its public tag and projections are:

- `lengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis`.

Product establishment returns the nominally separate opaque type:

```text
ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
```

Its public tag and projections are:

- `lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainValidationSchemaTag`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis`.

Both receipts have public `Eq`, `Ord`, `Show`, and `NFData` behavior but hidden
constructors. Scalar and product receipts cannot be coerced into one another.

The exact receipt tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1
```

## Query-owned example

Once `checkedQuery` has been sealed from an exact scalar candidate with the
maximum-at-most precondition above, offline query association is:

```haskell
validation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainAssignmentCount
        receipt
    , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainApplicableAssignmentCount
        receipt
    , validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomainBasis
        receipt
    )
  Right (LengthApplicableDomainCounterexample evidence) ->
    -- Existing exact counterexample evidence, independently replayed.
    error (show evidence)
  Right (LengthApplicableDomainInapplicable reason) ->
    error (show reason)
  Left failure -> error (show failure)
```

The product call and projection family use the
`LengthSpinePair`-prefixed names listed above. Query-owned validation emits no
SMT-LIB command, launches no process, and consumes no solver observation. It
uses the opaque query only to associate freshly produced evidence with the
query's exact behavioral problem.

## Deliberate exclusions

This checkpoint is not general minimum/maximum elimination and not a Boolean
finite-union analysis. The scanner ignores these shapes as whole clauses:

- the disjunctive orientations `C <= max(A,B)`, `min(A,B) <= C`,
  `not (max(A,B) <= C)`, and `not (C <= min(A,B))`;
- a minimum or maximum nested below a sum, scale, quotient, monus, conditional,
  or another expression instead of occurring at the relation root;
- root extrema on both relation operands;
- a retained canonical extremum with more than two terms;
- a root extremum whose child or opposite operand contains minimum, maximum,
  quotient, modulo, natural monus, a conditional, a result reference, a
  zero-factor retained scale, or any other non-affine subtree;
- a mixed root-extrema/root-quotient candidate clause;
- negated equality, nested Boolean structure, and extrema relations below a
  non-top-level formula.

Unsupported syntax is not an operational failure. It grants no partial
coverage rule, but the checked formula is still authoritative during finite
replay if some other clause supplies complete bounds.

## Failure precedence and exhaustive replay

The successor preserves the established ordering:

1. the compact input-count limit is checked before precondition demand;
2. a nullary problem bypasses extraction and replays the singleton `[]`;
3. normalized clauses are scanned in canonical order and rules are closed;
4. a syntactic or fired-rule contradiction selects the ordinary all-zero
   carrier before any missing-bound lookup;
5. absent contradiction, the first source-ordered input without a bound yields
   ordinary `LengthApplicableDomainInapplicable`;
6. derived maximum values are checked left to right;
7. the Cartesian assignment cap is admitted before traversal;
8. assignments replay in the existing last-input-fastest lexicographic order;
9. the first indexed evaluation rejection or postcondition violation wins;
10. complete traversal constructs the nominal establishment receipt;
11. a query wrapper finally replays the behavioral evidence association
    against the query's exact problem.

Contradiction never manufactures an empty Cartesian product. It selects the
all-zero box, which is replayed normally and records total count one and
applicable count zero when the contradictory precondition rejects that
assignment.

The replay stage evaluates the original normalized precondition, candidate
result, and postcondition. Extrema-derived bounds are admission evidence for
the rectangle, not a replacement formula. A violating candidate therefore
returns the same provider-basis-bound `ValidatedLengthCounterexample` or
`ValidatedLengthSpinePairCounterexample` used by every predecessor.

For example, `max(x,2*x) <= 4` derives maximum `[2]`. If the checked candidate
returns zero while its postcondition requires `result = x`, assignment zero
succeeds and the next lexicographic assignment is the first violation. The
validator returns the ordinary scalar counterexample with inputs `[1]`, result
zero, and the exact checked problem's provider/model basis.

## Authority and identity

A positive root-extrema receipt establishes only that Djex exhaustively
replayed the exact checked problem over one derived finite rectangle under the
recorded total finite-spine model and any named assumed provider laws. It does
not establish:

- source-language inhabitance, termination, strictness, or totality;
- that a retained provider implementation satisfies its assumed law;
- behavior outside the finite rectangle;
- a universal theorem or permission to prune candidates;
- any meaning for `sat`, `unsat`, `unknown`, or another solver status;
- a general extrema, disjunctive, Presburger, or optimization authority.

Only the two new receipt tags introduce canonical bytes. The selected offline
validator is not part of a sealed query's identity. Existing scalar and
product query commands, input symbols, value requests, behavioral problem,
and fingerprints remain byte-for-byte unchanged, as do contract, inventory,
session, candidate, encoding, response, protocol, execution, process, ready
worker, run, and observation identities. The query wrappers merely associate
the new nominal receipt or existing counterexample evidence with those exact
unchanged problems.

## Characterization

The bounded test matrix pins:

- each of the four laws and multi-clause propagation;
- equality in both raw orientations and necessary-only behavior;
- successor-before-cancellation and contradiction-before-missing precedence;
- exact projection parity for predecessor literal and root-quotient clauses;
- normalized n-ary, both-root, embedded, mixed quotient, unsupported-child,
  and disjunctive exclusions;
- value, Cartesian-product, and replay failure order;
- the lexicographically first extrema-derived counterexample;
- scalar/product nominal tags, query association, and unchanged predecessor
  query bytes and fingerprints;
- opaque constructors, public projections and `NFData`, absence of public
  `Generic`, and scalar/product non-`Coercible` roles.
