# Boolean finite-union Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has an additive, solver-independent applicable-domain entrance for a
bounded canonical Boolean DNF and its exact finite union of zero-origin input
boxes. It is the cumulative successor to strict relational positive-affine,
root quotient, root extrema, and root monus consequence extraction. Every
signed relational leaf delegates to the existing root-monus clause scanner;
this checkpoint adds formula-level Boolean branching, bounded branch-local
closure, explicit union receipts, and one global exhaustive replay.

The scalar entrances are:

- `validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`;
- `validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`.

The nominal binary-product entrances are:

- `validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`.

Their arguments are, in order, the existing `LengthEvaluationLimits`, the
existing `LengthInputBoxLimits`, the new opaque
`LengthBooleanFiniteUnionLimits`, and the checked problem or sealed query.
Each returns the established three-way `LengthApplicableDomainValidation`:
ordinary inapplicability, an independently replayed counterexample, or a new
opaque nominal finite-union establishment receipt.

The exact leaf laws inherited by this cumulative entrance are recorded in the
[root-monus applicable-domain report](2026-08-15-root-monus-length-applicable-domain.md).

## Exact signed DNF grammar

Expansion reads the already checked and normalized `LengthFormula`. It carries
one of two proof polarities, written `+F` and `-F`, and has the following exact
meaning:

| Signed normalized formula | Raw DNF result |
| --- | --- |
| `+true` or `-false` | one empty conjunction |
| `+false` or `-true` | the empty union |
| `+not F` | expansion of `-F` |
| `-not F` | expansion of `+F` |
| `+all [F_i]` | Cartesian conjunction of every positive child DNF |
| `-all [F_i]` | union of every negative child DNF |
| `+(A <= B)` | one positive at-most leaf |
| `-(A <= B)` | one `not (A <= B)` leaf |
| `+(A = B)` | one equality leaf |
| `-(A = B)` | `not (A <= B)` or `not (B <= A)` |

The negated-equality split is exact over naturals:

```text
A /= B  <=>  not (A <= B) or not (B <= A)
```

There is no separate object-language `or` constructor. Normalized negation
and conjunction are sufficient to express the Boolean alternatives, and the
polarity traversal applies De Morgan's law without manufacturing checked
syntax or changing the sealed formula.

This DNF grammar opens only `LengthFormula` structure. A formula used as the
condition of expression-level `LengthIf` remains inside that unsupported
expression leaf: the traversal never descends through an expression to find
Boolean alternatives. Likewise, this version does not split a disjunction
hidden in the arithmetic semantics of one atomic relation.

## Raw branch admission and canonical branch antichain

The generated-branch cap counts complete raw DNF conjunctions before any
logical cleanup. A formula that generates `limit+1` branches is rejected even
if duplicate removal, complement removal, or subsumption would later make the
canonical DNF small. The observation is productive and retains a separate
exceeded arm even when the configured `Int` limit is `maxBound`.

After raw admission, each branch becomes a sorted set of signed normalized
literals. Canonicalization then performs, in this order:

1. remove duplicate literals within each branch;
2. drop a branch containing an exact literal and its `LengthNot` complement;
3. deduplicate equal branch sets;
4. drop every branch that is a strict literal-set superset of another branch;
5. retain the remaining branches in their `Set` ordering.

The fourth step is exact Boolean absorption. If branch `P` is already present,
`P and Q` adds no assignment to the union. It is therefore the stronger branch
that is dropped. This is a syntactic clause-set antichain, not a semantic
implication search.

Branch indices in public errors refer to this canonical post-complement,
post-deduplication, post-subsumption order. The raw branch count intentionally
does not use those smaller indices.

## Branch-local predecessor consequences and bounded closure

Every literal in a canonical branch is scanned in normalized literal order by
the exact existing
`strictRelationalPositiveAffineQuotientRootExtremaMonusClauseCoverage` path.
The finite-union validator does not duplicate or loosen any predecessor law.
Consequently one leaf may contribute:

- the predecessor relational, strict, quotient, root-extrema, or root-monus
  affine rules in its established within-clause order;
- a contradiction;
- no coverage rule for unsupported syntax.

The rule cap is per canonical branch. All admitted literal rules are appended
in canonical literal order, and the entire branch is rejected before closure
attempt `limit+1` if its rule count is too large.

Closure is also per branch. It preserves the predecessor algorithm:

1. constant-right rules are partitioned as seeds while retaining order;
2. every pass reads one immutable bounds snapshot;
3. pending rules are attempted in canonical order;
4. all rules eligible in that snapshot fire once and are removed;
5. pass results merge afterward with `min`;
6. closure stops when a pass fires nothing.

The new inspection cap counts every attempted rule across the seed pass and
all later passes. It does not turn closure into a numeric least-fixed-point
solver. A direct seed needs one inspection; a canonical chain such as
`x<=y, y<=1` needs two. A cap error is operational failure, not ordinary
coverage inapplicability.

A closure contradiction drops only that branch. All canonical branches finish
their bounded rule collection and closure, in order, before missing coverage
is inspected. If every branch is contradictory, the exact Boolean union is
empty. If any live branch remains, every compact source input must have an
upper bound in every live branch. The first source-ordered input missing from
at least one live branch makes the whole attempt ordinarily inapplicable:
dropping an unbounded live alternative would omit part of the applicable
domain.

## Canonical box antichain and the no-hull boundary

Each completely bounded live branch produces one source-ordered inclusive
maximum vector. Every vector denotes a zero-origin Cartesian box. The
validator then:

1. deduplicates equal vectors;
2. drops a box componentwise contained in another retained box;
3. retains the componentwise-maximal vectors in lexicographic order.

Containment removal is exact because a smaller zero-origin box contributes no
assignment beyond its containing box. Incomparable boxes remain separate.
They are never replaced by their componentwise maximum.

For example, the alternatives

```text
(x <= 1 and y <= 3) or (x <= 3 and y <= 1)
```

produce the incomparable maxima `[1,3]` and `[3,1]`. Their raw traversal has
`8+8=16` visits. Their overlap is the four assignments in `[1,1]`, so the
deduplicated union has 12 assignments. The componentwise hull `[3,3]` would
contain 16 assignments and add the four cross-corner assignments satisfying
neither alternative. Such widening is outside this receipt's authority and
is forbidden even when replaying those extra assignments would happen to
succeed.

## Visit count, exact union, and global replay order

After box maxima pass the existing assignment-value bit limit, the validator
computes the raw assignment-visit count without constructing assignments. It
is the sum of every retained box cardinality:

```text
visits = sum(product(maximum_i + 1) for each retained box)
```

Overlaps count once per component box here because each traversal performs a
set insertion attempt. This value is checked against the new visit cap before
the union is materialized and is retained in the positive receipt.

Each box is then enumerated lexicographically with the last source input
varying fastest, matching the existing input-box convention. Assignments are
inserted into a `Set [Natural]`; the existing
`lengthInputBoxAssignmentLimit` caps newly inserted, unique assignments. The
receipt's assignment count is this exact set cardinality, not the sum of box
cardinalities.

Concrete replay consumes `Set.toAscList` once, so failures and
counterexamples follow one global lexicographic order rather than retained-box
order. For boxes `[1,1,3]` and `[1,3,1]`, a postcondition which first fails at
either `[1,0,0]` in the first box or `[0,2,0]` in the second reports
`[0,2,0]`: it is earlier in the global set order. The zero-based evaluation
ordinal refers to that deduplicated global order.

Replay always evaluates the original checked precondition and postcondition.
The DNF and affine rules establish only that the union covers every applicable
assignment; they never replace the checked formula. An ignored leaf can widen
a branch box safely, but it remains fully authoritative when replay decides
whether an assignment is applicable. The first postcondition violation returns
the ordinary scalar or product counterexample evidence. Only complete global
replay constructs the new positive receipt.

## Empty unions and nullary formulas

Empty-union authority is explicit in this checkpoint:

- positive `false`, negative `true`, exact literal/complement elimination, or
  closure contradiction of every branch yields zero retained boxes;
- the receipt records box, visit, unique-assignment, and applicable counts all
  equal to zero;
- maximum-value checks, visit enumeration, concrete evaluation, and candidate
  result-expression evaluation do not occur; the receipt still records its
  checked problem's provider/model basis.

For example, normalized `x<=0 and not(x<=0)` has one raw branch which is
removed as an exact complement. The validator establishes the empty
applicable domain without introducing the predecessor's all-zero carrier.
This behavior belongs only to the new explicit-union receipt; every older
single-box entrance retains its literal contradiction behavior.

Nullary truth is different. Positive `true` generates one empty branch, whose
maximum vector is `[]`. The canonical boxes are `[[]]`, raw visits and unique
assignments are both one, and the original problem is replayed at assignment
`[]`. The retained-box cap must first admit that one box; a zero box cap, zero
visit cap, or zero existing unique-assignment cap rejects the singleton at its
corresponding precedence point before replay. Nullary false remains the empty
union. A nonnullary true formula has one live branch with no bounds and is
ordinarily inapplicable at compact input zero.

## Limits and defaults

`LengthBooleanFiniteUnionLimitSource` exposes five signed fields:

```text
lengthBooleanFiniteUnionLimitSourceMaximumGeneratedBranches
lengthBooleanFiniteUnionLimitSourceMaximumRulesPerBranch
lengthBooleanFiniteUnionLimitSourceMaximumClosureInspectionsPerBranch
lengthBooleanFiniteUnionLimitSourceMaximumRetainedBoxes
lengthBooleanFiniteUnionLimitSourceMaximumAssignmentVisits
```

`mkLengthBooleanFiniteUnionLimits` validates them as nonnegative in that
declaration order. A malformed source returns
`NegativeLengthBooleanFiniteUnionLimit` with one of:

```text
LengthBooleanFiniteUnionMaximumGeneratedBranches
LengthBooleanFiniteUnionMaximumRulesPerBranch
LengthBooleanFiniteUnionMaximumClosureInspectionsPerBranch
LengthBooleanFiniteUnionMaximumRetainedBoxes
LengthBooleanFiniteUnionMaximumAssignmentVisits
```

The constructor of `LengthBooleanFiniteUnionLimits` is private. Its public
projections and defaults are:

| Projection | Default |
| --- | ---: |
| `lengthBooleanFiniteUnionGeneratedBranchLimit` | 256 |
| `lengthBooleanFiniteUnionRuleLimitPerBranch` | 64 |
| `lengthBooleanFiniteUnionClosureInspectionLimitPerBranch` | 4096 |
| `lengthBooleanFiniteUnionRetainedBoxLimit` | 256 |
| `lengthBooleanFiniteUnionAssignmentVisitLimit` | 262144 |

`defaultLengthBooleanFiniteUnionLimitSource` and
`defaultLengthBooleanFiniteUnionLimits` carry exactly those values. The
existing input-box defaults remain eight compact inputs and 65,536 unique
assignments. Existing evaluation limits continue to bound assigned values and
every intermediate value during original-formula replay.

The generated-branch, rule, closure-inspection, retained-box, and visit errors
report the configured limit and a bounded observed value. An overflow reports
the saturated `limit+1`; a distinct exceeded path keeps `maxBound` fail-closed.

## Exact failure precedence

The scalar validator has this fixed order:

1. compare compact input width with `lengthInputBoxInputLimit` before demanding
   the precondition;
2. generate complete raw DNF branches and enforce the generated-branch cap;
3. remove complement branches, deduplicate, and apply branch subsumption;
4. in canonical branch order, enforce the per-branch rule cap and then the
   per-branch closure-inspection cap;
5. drop exactly contradictory branches;
6. if a live branch lacks a bound, return the first source-ordered missing
   input as ordinary `LengthApplicableDomainInapplicable`;
7. derive, deduplicate, and componentwise-antichain the live boxes;
8. enforce the retained-box cap;
9. check maxima in canonical box order and source input order;
10. compute and enforce raw assignment visits;
11. materialize the union and enforce the existing unique-assignment cap;
12. replay unique assignments in global `Set` lexicographic order;
13. return the first indexed evaluation rejection or exact counterexample;
14. after complete replay, construct the nominal receipt;
15. in a query wrapper, check exact behavioral-problem association last.

Thus all branch rule/closure work completes before missing coverage, while
missing coverage necessarily precedes a retained-box cap because an incomplete
bound map cannot form a box. Empty-union completion bypasses steps 9 through
13 with all counts zero. The product validator has the identical control
order and nominal product errors.

## Public errors

Scalar direct validation returns
`LengthBooleanFiniteUnionApplicableDomainValidationError`, whose closed
constructors are:

```text
LengthBooleanFiniteUnionProblemInputLimitExceeded
LengthBooleanFiniteUnionGeneratedBranchLimitExceeded
LengthBooleanFiniteUnionRuleLimitExceeded
LengthBooleanFiniteUnionClosureInspectionLimitExceeded
LengthBooleanFiniteUnionRetainedBoxLimitExceeded
LengthBooleanFiniteUnionMaximumValueRejected
LengthBooleanFiniteUnionAssignmentVisitLimitExceeded
LengthBooleanFiniteUnionAssignmentLimitExceeded
LengthBooleanFiniteUnionAssignmentEvaluationRejected
LengthBooleanFiniteUnionInternalEnumerationInvariant
```

Rule and closure failures carry canonical branch index, limit, and observed
count. Maximum-value failure carries box index, input index, and the existing
`LengthEvaluationError`. Evaluation failure carries the global unique
assignment ordinal and that error. The assignment-limit fields are `Natural`;
the operational work limits and indices are `Int`.

Product direct validation returns the closed nominal
`LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError`, with the
same constructor shapes and `LengthSpinePairEvaluationError` payloads:

```text
LengthSpinePairBooleanFiniteUnionProblemInputLimitExceeded
LengthSpinePairBooleanFiniteUnionGeneratedBranchLimitExceeded
LengthSpinePairBooleanFiniteUnionRuleLimitExceeded
LengthSpinePairBooleanFiniteUnionClosureInspectionLimitExceeded
LengthSpinePairBooleanFiniteUnionRetainedBoxLimitExceeded
LengthSpinePairBooleanFiniteUnionMaximumValueRejected
LengthSpinePairBooleanFiniteUnionAssignmentVisitLimitExceeded
LengthSpinePairBooleanFiniteUnionAssignmentLimitExceeded
LengthSpinePairBooleanFiniteUnionAssignmentEvaluationRejected
LengthSpinePairBooleanFiniteUnionInternalEnumerationInvariant
```

Query validation has separately nominal wrappers:

```text
LengthSMTLibBooleanFiniteUnionApplicableDomainValidationError
  = LengthSMTLibBooleanFiniteUnionApplicableDomainValidationRejected ...
  | LengthSMTLibBooleanFiniteUnionApplicableDomainValidationAssociationRejected ReplayMismatch

LengthSpinePairSMTLibBooleanFiniteUnionApplicableDomainValidationError
  = LengthSpinePairSMTLibBooleanFiniteUnionApplicableDomainValidationRejected ...
  | LengthSpinePairSMTLibBooleanFiniteUnionApplicableDomainValidationAssociationRejected ReplayMismatch
```

Association is checked only for an authoritative counterexample or positive
receipt. Ordinary inapplicability carries no evidence to associate.

## Public receipt surface and tags

Scalar establishment returns the opaque type:

```text
ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
```

Its public projections are:

- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis`.

The box projection has type `[[Natural]]`; box, visit, unique-assignment, and
applicable-assignment counts are `Natural`.

Product establishment returns the nominally separate opaque type:

```text
ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
```

Its public projections are:

- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBoxCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentVisitCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainApplicableAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainBasis`.

Both types have `NFData`, their constructors are private, and there is no
scalar/product or predecessor/successor coercion. Their public tag projections
are
`lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag`
and
`lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomainValidationSchemaTag`.

The exact scalar and product tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1
```

## Compatibility, identity, and authority

This is a separately selected cumulative entrance, not a mutation of the
root-monus validator. All earlier direct, positive-affine, relational, strict,
quotient, root-extrema, and root-monus APIs, scanners, closure bodies, receipt
types, tags, failure order, and single-box contradiction/nullary behavior
remain literal. The new work limits are call-time admission policy; they do
not enter a checked problem or query fingerprint and are not retained as
receipt fields. The receipt retains the resulting canonical union and exact
completed traversal counts instead.

DNF branches, affine rules, bounds, boxes, and replay sets are never inserted
into SMT-LIB. Query check bytes, input symbols, value requests, fingerprints,
responses, protocols, execution policies, processes, workers, observations,
and live strengths remain byte-for-byte unchanged. The query wrapper emits no
command and consumes no raw or live solver status. It adds only exact
problem/evidence association after solver-independent validation.

The new tag and nominal receipt distinguish this evidence from every
single-box predecessor and from the product sibling. Establishment means that
the canonical union soundly covers every input satisfying the original
checked precondition and that every unique assignment in that finite cover was
replayed under the exact finite-spine model. The receipt exposes how much of
that cover was actually applicable, including a vacuous zero count, and keeps
the provider-law basis explicit.

It does not establish source-language termination, totality, realization, or
effects; validate a provider implementation; make a solver status trustworthy;
prove behavior outside the checked finite-spine model; or grant pruning or
candidate-suppression authority.

## Adversarial characterization boundary

Focused characterization should pin:

- every truth, negation, conjunction, at-most, equality, and negated-equality
  polarity;
- a Cartesian DNF product whose raw count exceeds the cap before later
  absorption;
- exact literal/complement elimination, branch deduplication, and strict-
  superset subsumption;
- predecessor rule order and parity for relational, quotient, extrema, and
  monus leaves;
- per-branch rule and closure inspection `limit`/`limit+1` boundaries;
- branch-local contradiction drop, all-contradictory empty union, and branch
  cap/closure failure before missing coverage;
- first source input missing from any live branch;
- equal-box deduplication, componentwise containment removal, incomparable-box
  retention, and retained-box cap;
- the `[1,3]`/`[3,1]` no-hull example with visits 16 and unique count 12;
- maximum-value checks before visit count, visit count before unique count,
  and global set replay rather than box-by-box replay;
- empty union versus nullary true, including zero visit/assignment caps;
- first global evaluation failure, first counterexample, applicable count,
  provider basis, receipt opacity, `NFData`, scalar/product nominality, tags,
  and exact query association;
- literal preservation of every predecessor API, receipt, tag, problem/query
  byte sequence, and runtime identity.

## Separately versioned atomic branch-producing follow-up

This v1 expands formula-level Boolean structure only. Several atomic natural
laws still contain their own disjunctions:

- `C <= max(A,B)` and `min(A,B) <= C`;
- the opposite strict extrema polarities;
- may-zero `C <= A monus B`;
- the complete may-zero root-monus equality law.

The predecessor leaf scanner deliberately continues to ignore those shapes or
to retain only its already documented necessary equality consequence. This
finite-union validator does not reinterpret one ignored atom as multiple
branches.

A follow-up may add an all-or-nothing, branch-producing atomic-rule result,
but it must be a separately named and separately tagged cumulative validator.
It needs exact per-atom branch laws, atomic operand summarization, deterministic
within-atom branch order, and accounting of those generated branches under a
frozen work-cap policy. It must feed the same explicit branch and box
antichains and the same global union replay. It must not silently change this
v1 tag, borrow positivity across clauses, retain one convenient half after a
failed summary, or replace any resulting union with a componentwise hull.
