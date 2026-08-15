# Root-monus Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has an additive, solver-independent applicable-domain entrance for
immediate normalized natural monus consequences. It is the cumulative
successor to strict relational positive-affine quotient and root-extrema
coverage: every clause without an immediate root `LengthMonus` delegates to
the root-extrema predecessor unchanged.

The scalar entrances are:

- `validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`;
- `validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`.

The nominal binary-product entrances are:

- `validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`.

Each returns the existing three-way `LengthApplicableDomainValidation`.
Incomplete coverage is ordinary inapplicability. A first independently
replayed postcondition violation returns the existing scalar or product
counterexample evidence. Complete traversal creates a new opaque, nominal
root-monus establishment receipt.

## Additive compatibility

This is a separately named successor, not a change to an older validator. The
direct, positive-affine, relational, strict-relational, root-quotient, and
root-extrema entrances retain their exact syntax, scanner bodies, failure
order, receipt types, and tags. In particular:

- the root-extrema predecessor still excludes `LengthMonus` from every affine
  child or opposite-operand summary;
- a problem which needs a monus consequence remains inapplicable through that
  predecessor;
- no formula constructor, query, or live policy implicitly selects the new
  validator;
- no fallback, receipt coercion, or policy mutation was introduced;
- counterexample, raw-input, origin, simplification, explicit-box, query,
  observation, and live replay retain their existing authority.

Every supported monus-free clause is routed through the exact predecessor
clause scanner and the same consequence closure. Selecting the new entrance
can create only its new nominal positive receipt; it cannot relabel a
predecessor receipt.

## Natural monus and the five admitted cases

Let

```text
M = A monus B = max(A - B, 0)
```

for natural-valued expressions `A` and `B`. Let the exact positive-affine
summary of the opposite relation operand be

```text
C = c + sum(k_i * x_i)
```

where `c` and every `k_i` are naturals. The scanner atomically summarizes
`A`, `B`, and `C`, then admits these five cases:

| Normalized formula | Admission | Proof-only affine consequences |
| --- | --- | --- |
| `M <= C` | always | `A <= B + C` |
| `C <= M` | `c > 0`; identically-zero `C` contributes no rule | `B + C <= A` when `c > 0` |
| `not (M <= C)` | always | `B + C + 1 <= A` |
| `not (C <= M)` | always | `1 <= C`; `A + 1 <= B + C` |
| `M = C` or `C = M` | always | `A <= B + C`; append `B + C <= A` when `c > 0` |

The first four admitted rows are exact equivalences under their stated
condition. Equality deliberately retains a necessary supported half when
`C` may be zero. The derived additions and successors modify only exact
arbitrary-precision summaries. They do not construct a new checked
`LengthExpression`, spend a syntax node, revalidate a generated literal, or
change the normalized contract.

### `M <= C`

For naturals:

```text
A monus B <= C  <=>  A <= B + C
```

If `A <= B`, the monus is zero and both sides hold. If `A > B`, ordinary
cancellation gives the same inequality. This row always emits exactly one
rule after all three summaries succeed.

### The zero boundary in `C <= M`

The unconditional law is disjunctive:

```text
C <= A monus B  <=>  C = 0 or B + C <= A
```

The affine constant `c` is also the minimum value of `C` over natural inputs.
The scanner therefore distinguishes three cases:

1. If `c > 0`, `C` is uniformly positive. The zero branch is impossible and
   the scanner emits the exact single rule `B+C <= A`.
2. If `c = 0` and the coefficient map is empty, `C` is identically zero.
   `0 <= M` is tautological and the clause contributes no coverage rule.
3. If `c = 0` with at least one coefficient, `C` may be zero. The clause is
   ignored whole because its exact domain is a union.

Positivity is read only from the clause-local exact affine summary. The
scanner does not borrow `1 <= C` from another clause: the existing closure
stores upper bounds, not reusable lower-bound proofs. It also does not emit
the universally necessary but weaker `C <= A`; doing so would change this
checkpoint from exact oriented rewrites to an open-ended relaxation policy.

### Strict polarities

The natural complement of at-most gives:

```text
not (A monus B <= C)  <=>  B + C + 1 <= A
```

The reverse strict orientation crosses the truncation boundary and therefore
has two conjuncts:

```text
not (C <= A monus B)
  <=>  1 <= C and A + 1 <= B + C
```

The scanner emits `1 <= C` first and `A+1 <= B+C` second. Both rules are
constructed only after `A`, `B`, and `C` have summarized. Neither survives if
any operand is unsupported.

### Equality is necessary-one-way when `C` may be zero

The complete equality law is:

```text
A monus B = C
  <=>  (C = 0 and A <= B)
    or (1 <= C and A = B + C)
```

This is a union when an affine `C` may be zero. Equality nevertheless always
implies `M <= C`, whose exact affine rewrite is `A <= B+C`; that rule is
therefore safe and useful for either root position.

- If `c > 0`, equality also emits `B+C <= A` second. The two rules are exact.
- If `C` is identically zero, the first rule is `A <= B`, the exact monus-zero
  condition.
- If `c = 0` with coefficients, equality emits only `A <= B+C`. It does not
  claim that this necessary rule is sufficient.

Canonical equality sorting cannot change the result: a single root monus is
recognized on either equality side and the consequence order is always the
same. Negated equality remains unsupported.

## All-or-nothing three-affine admission

Every `A`, `B`, and `C` must fit the predecessor grammar:

```text
A ::= compact-input
    | natural-literal
    | LengthSum [A]
    | LengthScale positive-literal A
```

The scanner summarizes the monus minuend, subtrahend, and opposite operand
before retaining any candidate rule. This matters especially for the
two-rule strict reverse and equality cases: an easy boundary or at-most rule
cannot survive an unsupported sibling.

Exactly one relation operand may be an immediate normalized root
`LengthMonus`. These shapes are excluded as whole clauses:

- root monus on both relation operands;
- monus nested below a sum, scale, quotient, modulo, extremum, conditional, or
  another expression;
- a root monus whose minuend, subtrahend, or opposite operand contains monus,
  quotient, modulo, minimum, maximum, a conditional, a result reference, or
  any other unsupported node;
- mixed root-monus/root-quotient or root-monus/root-extrema clauses;
- negated equality and relations below nested Boolean structure.

Unsupported syntax is not an operational error. It grants no coverage rule,
but the original checked clause remains authoritative during concrete replay
if other clauses establish a complete rectangle.

## Normalization and canonical rule order

Admission sees the checked normalized tree, not raw source spelling. Monus
normalization is deliberately small and order-preserving:

- literal/literal monus folds to the saturated natural difference;
- `A monus 0` becomes `A`;
- `A monus A` becomes literal zero;
- every other monus retains its ordered binary children.

Expression children are normalized before these reductions, so a folded
monus cannot conceal an invalid reference or over-budget subtree. Equality
operands are sorted unless equality folds to truth. Top-level conjunctions
are flattened, truths removed, duplicates removed, and remaining clauses
sorted.

A raw monus which folds away therefore delegates to the root-extrema
predecessor. A retained immediate monus is owned by the new scanner. The
collector traverses normalized clauses in canonical order. Within a clause:

- the strict reverse emits `1<=C`, then `A+1<=B+C`;
- equality emits `A<=B+C`, then its uniformly-positive reverse when present;
- every one-rule orientation retains its displayed order.

No ordering affects normalized contract or query bytes, but the fixed order
keeps closure and any contradiction path deterministic.

## Worked direct-law examples

The universal at-most law gives:

```text
(x monus 3) <= 5
```

It derives `x <= 8`, so the maximum is `[8]`. All nine assignments are
applicable.

The uniformly-positive reverse law gives:

```text
1 <= (5 monus x)
```

It derives `x+1 <= 5`, hence `[4]`, with counts 5/5.

The strict forward law gives:

```text
not ((5 monus x) <= 2)
```

It derives `x+3 <= 5`, hence `[2]`, with counts 3/3.

The strict reverse law gives:

```text
not (3 <= (x monus 2))
```

It derives the true boundary `1<=3` and `x+1<=5`, hence `[4]`, with counts
5/5.

An equality with a positive opposite gives:

```text
(x monus 3) = 5
```

It derives `x<=8` and `8<=x`. The rectangle maximum is `[8]`; exhaustive
replay visits nine assignments and records only `x=8` as applicable.

An affine positive equality demonstrates both variable bounds:

```text
(5 monus x) = y + 1
```

The rules are `5<=x+y+1` and `x+y+1<=5`. Closure derives `[4,4]`; five of the
25 assignments satisfy the original equality. With an identically-zero
opposite, `(x monus 3)=0` instead derives `[3]` and has counts 4/4.

## Propagation and bounded work

Monus rules enter the unchanged relational closure. For example:

```text
(x monus y) <= z
y <= 2
z <= 3
```

The monus rule is `x<=y+z`. The two constant-right rules seed `y` and `z`;
the next immutable-snapshot pass derives `x<=5`. Source-ordered maxima are
`[5,2,3]`. The rectangle has 72 assignments, and 42 satisfy the original
precondition.

Closure remains synchronous and deliberately rule-once:

1. constant-right rules are partitioned as seeds while retaining order;
2. each pass examines pending rules against one immutable bounds snapshot;
3. every eligible rule fires once and is permanently removed;
4. pass results merge afterward with `min`;
5. processing stops when a pass fires nothing.

A monus clause emits at most two rules, matching quotient equality and
root-extrema. If a checked problem was sealed with formula-clause limit `F`,
the closure receives at most `2*F` rules from the complete cumulative scanner;
the default `F=32` therefore gives at most 64. Every productive pass consumes
at least one pending rule, so the established bounded quadratic scan remains
unchanged. Existing syntax-node, collection-width, literal, compact-input,
assigned-value, and Cartesian-assignment limits bound the remaining work. No
monus-specific work cap or lower-bound fact store was added.

## Adversarial boundaries

The exclusions are semantic, not cosmetic. These natural assignments refute
tempting alternatives:

| Unsafe claim | Counterexample | Why it fails |
| --- | --- | --- |
| `C<=A monus B` always gives `B+C<=A` | `A=0,B=1,C=0` | source is `0<=0`; consequence is `1<=0` |
| `A monus B=C` always gives `A=B+C` | `A=0,B=1,C=0` | source equality holds; affine equality does not |
| `A+1<=B+C` alone characterizes `A monus B<C` | `A=0,B=1,C=0` | proposed rule holds; strict source is false |
| `C<=A` is equivalent to `C<=A monus B` | `A=B=C=1` | weak rule holds; source is `1<=0` |
| equality's `A<=B+C` is sufficient | `A=B=C=1` | rule holds; `1 monus 1` is not `1` |

Both-root cross-addition is also unsafe. At `A=0,B=0,C=0,D=1`,
`A monus B <= C monus D` is true, while the tempting
`A+D <= B+C` is false. Both-root clauses therefore receive no partial rule.

The zero boundary is observable as coverage. Under

```text
0 <= (0 monus x)
```

the original precondition admits every natural `x`. The identically-zero
opposite contributes no rule, so the validator correctly reports input zero
missing rather than falsely establishing `[0]`. Under

```text
not (0 <= (x monus y))
```

the first strict-reverse rule is the contradiction `1<=0`; contradiction wins
over both missing inputs, selects the all-zero carrier `[0,0]`, and replay
records total/applicable counts 1/0.

## Failure precedence and exhaustive replay

The successor retains the established order:

1. reject compact input width before demanding the precondition;
2. let a nullary problem bypass extraction and replay singleton `[]`;
3. scan normalized conjuncts in canonical order and close the retained rules;
4. let a syntactic or fired-rule contradiction select the all-zero carrier
   before any missing-bound lookup;
5. otherwise return the first source-ordered missing input as ordinary
   `LengthApplicableDomainInapplicable`;
6. validate derived maxima from left to right;
7. admit the Cartesian assignment count before traversal;
8. replay assignments in last-input-fastest lexicographic order;
9. return the first indexed evaluation rejection or exact postcondition
   counterexample;
10. after complete traversal, construct the nominal establishment receipt;
11. in a query wrapper, replay the evidence association against the query's
    exact behavioral problem last.

Contradiction does not manufacture an empty product. It uses the all-zero
rectangle and replays the original formula normally. Extracted monus rules
are only sound bounding consequences; they never replace the checked
precondition. Unsupported and necessary-one-way equality clauses therefore
remain fully effective during replay.

## Public receipt surface

Scalar establishment returns the opaque type:

```text
ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
```

Its public tag and projections are:

- `lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis`.

Product establishment returns the nominally separate opaque type:

```text
ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
```

Its public tag and projections are:

- `lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainValidationSchemaTag`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainInclusiveMaximums`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainApplicableAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomainBasis`.

Both constructors are private, both types have `NFData`, and there is no
scalar/product or predecessor/new-receipt coercion. Their exact tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1
```

The product direct-law example `(x monus 3)<=5` yields the nominal product
receipt with maximum `[8]` and counts 9/9; it cannot be presented as scalar
evidence.

## Query association, identity, and authority

The query wrappers perform no solver operation. They invoke the corresponding
problem validator and then replay the returned behavioral evidence against
the sealed query's exact problem. A mismatch remains the existing query
association error.

The monus proof summaries do not enter the SMT-LIB plan. Existing check bytes,
input symbols, input-value requests, query fingerprints, status decoders,
protocols, processes, executions, workers, runs, observations, and live
strengths remain byte-for-byte unchanged. Contract, provider inventory,
semantic inventory, session, candidate, encoding, and behavioral-problem
identities are also unchanged. Only the two new positive receipt tags add
identity bytes.

Establishment means that every assignment in one admitted finite rectangle
was replayed under the exact checked finite-spine model, and that the receipt
records how many assignments met the precondition. It does not establish:

- behavior outside that rectangle;
- source-language termination, totality, or realization;
- correctness of an assumed provider implementation;
- a universal theorem or trustworthy solver `unsat` result;
- permission to prune or suppress other candidates.

## Characterization boundary

Focused characterization for this checkpoint should pin:

- all four relational polarities and both equality root positions;
- constant-positive, identically-zero, and may-zero opposite summaries;
- exact rule order and atomic failure when any of three summaries is
  unsupported;
- normalization folds and canonical equality/conjunction order;
- scalar and product maxima, total/applicable counts, bases, tags, opacity,
  `NFData`, and nominal non-coercibility;
- both-root, nested, embedded, mixed-root, unsupported-child, negated-equality,
  and nested-Boolean exclusions;
- contradiction before missing, then value, Cartesian, indexed-evaluation,
  first-counterexample, receipt, and association precedence;
- synchronous immutable-snapshot eligible-rule-once propagation;
- exhaustive original-formula replay and query/problem association;
- literal parity of every predecessor API, receipt, tag, query byte sequence,
  and identity.

## Remaining Boolean finite-union gap

The may-zero forms expose the next separate design problem. Exact coverage for

```text
C = 0 or B + C <= A
```

cannot be represented by the current single-rectangle receipt. The same is
true of the disjunctive root-extrema orientations and general Boolean
structure.

A future bounded finite-union validator needs a new nominal receipt carrying
canonical explicit branch boxes. It also needs independent caps for branch
generation, rules and closure per branch, retained boxes, total union replay,
and overlap/deduplication work, plus fixed branch, box, assignment, failure,
and counterexample order. A contradictory branch may be dropped, but every
live branch must be completely bounded before the union can claim complete
coverage.

It must not replace the union with one componentwise-maximum rectangle. Such
widening introduces cross-branch assignments which belong to no admitted
alternative and can multiply replay work. Boolean finite-union authority is
therefore a later receipt and work-cap checkpoint, not an implicit extension
of this root-monus receipt.
