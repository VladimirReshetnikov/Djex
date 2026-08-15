# Atomic-branching Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has a separately named, separately tagged cumulative applicable-domain
validator which combines the established normalized Boolean DNF with exact
branch-producing laws for one immediate root extremum or may-zero root monus.
It retains the predecessor's explicit finite union of zero-origin boxes and
global exhaustive replay; it does not approximate an arithmetic disjunction
with a componentwise hull.

The scalar entrances are:

- `validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`;
- `validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`.

The nominal binary-product entrances are:

- `validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`.

Their arguments remain, in order, `LengthEvaluationLimits`,
`LengthInputBoxLimits`, `LengthBooleanFiniteUnionLimits`, and the checked
problem or sealed query. They return the existing three-way
`LengthApplicableDomainValidation`: ordinary inapplicability, an exact
counterexample from bounded replay, or a fresh opaque positive receipt.

The formula-level DNF, box antichain, visit accounting, unique assignment set,
and replay semantics are the literal predecessor documented in the
[Boolean finite-union report](2026-08-15-boolean-finite-union-length-applicable-domain.md).
This checkpoint changes only the branch alternatives available for one signed
atomic leaf and the raw cap accounting needed to admit their Cartesian
product.

## Operand grammar and all-or-nothing admission

For every new law, `A` and `B` denote the two normalized children of an
immediate binary root, and `C` denotes the opposite relation operand. Each is
independently summarized in the established positive-affine grammar:

```text
c + sum(k_i*x_i)
```

where constants and coefficients are naturals, variables are compact checked
inputs, and checked expressions use only `LengthVariable`, `LengthLiteral`,
`LengthSum`, and a positive-literal `LengthScale`. Summaries use exact
arbitrary-precision naturals. Proof-only addition and successor operations do
not construct checked syntax and consume no syntax-node or public-literal
budget.

The scanner summarizes all three operands before emitting any alternative.
If any summary fails, the whole atom falls back to its predecessor coverage,
which is ignored for the newly admitted shapes. It never keeps a convenient
child, borrows positivity from another literal, or derives a partial branch.

Exactly one relation operand may contain the admitted immediate binary root.
The following remain excluded atomically:

- roots on both relation operands;
- a nested or embedded extremum or monus;
- a normalized effectively n-ary extremum whose immediate child is itself an
  extremum;
- a mixed root-extremum/root-monus or root-quotient atom;
- an unsupported affine child or opposite operand;
- an expression-level conditional, result reference, modulo, quotient, monus,
  extremum, or other non-affine node inside a required summary;
- unsupported Boolean structure hidden inside an expression.

Normalization remains authoritative. Same-kind extrema are flattened,
deduplicated, sorted, and left-associated before scanning. A retained
three-or-more-term extremum therefore presents a nested extremum child and is
ignored. An all-literal root may instead fold away and reach the predecessor.
Equality operands and Boolean formula children retain their existing canonical
order.

## Exact extremum alternatives

The new relation and strict-polarity laws are exact over naturals:

```text
C <= max(A,B)          <=>  C <= A or C <= B
min(A,B) <= C          <=>  A <= C or B <= C
not(max(A,B) <= C)     <=>  C+1 <= A or C+1 <= B
not(C <= min(A,B))     <=>  A+1 <= C or B+1 <= C
```

Their emitted proof alternatives are, literally:

```text
C <= max(A,B)          -> [C<=A] | [C<=B]
min(A,B) <= C          -> [A<=C] | [B<=C]
not(max(A,B)<=C)       -> [C+1<=A] | [C+1<=B]
not(C<=min(A,B))       -> [A+1<=C] | [B+1<=C]
```

Every list before or after `|` is one branch-local ordered rule sequence.
Alternatives always follow the normalized first child and then the normalized
second child.

Equality needs both the predecessor's necessary conjunctive half and one
choice from the opposite disjunctive half:

```text
max(A,B) = C
  <=> (A<=C and B<=C and C<=A)
   or (A<=C and B<=C and C<=B)

min(A,B) = C
  <=> (C<=A and C<=B and A<=C)
   or (C<=A and C<=B and B<=C)
```

The exact emitted order is:

```text
max(A,B)=C -> [A<=C,B<=C,C<=A] | [A<=C,B<=C,C<=B]
min(A,B)=C -> [C<=A,C<=B,A<=C] | [C<=A,C<=B,B<=C]
```

The immediate root may occur on either equality side; canonical equality
orientation cannot change these consequences. Each equality alternative has
three rules. There is no rule-set deduplication even if normalization or
coincident operands make two rules extensionally equal.

The already exact predecessor extremum orientations remain singleton
alternatives with their literal rule order:

```text
max(A,B) <= C
C <= min(A,B)
not(min(A,B) <= C)
not(C <= max(A,B))
```

This additive entrance therefore completes the immediate binary extremum
orientations without changing the root-extrema predecessor receipt or scanner.

## Exact may-zero monus alternatives

Let:

```text
M = A monus B = max(A-B,0)
```

An affine opposite `C` is classified as may-zero exactly when its constant is
zero and its coefficient map is nonempty. It can be zero at the all-zero
assignment and positive elsewhere. For this class:

```text
C <= M  <=>  C <= 0 or B+C <= A
```

The new alternatives are zero first, bound second:

```text
C <= M -> [C<=0] | [B+C<=A]
```

For equality, the predecessor's necessary `A<=B+C` consequence is common to
both exact cases:

```text
M = C or C = M
  <=> (A<=B+C and C<=0)
   or (A<=B+C and B+C<=A)
```

The exact emitted rule sequences are:

```text
M = C -> [A<=B+C,C<=0] | [A<=B+C,B+C<=A]
C = M -> [A<=B+C,C<=0] | [A<=B+C,B+C<=A]
```

The common predecessor rule remains first in both alternatives. In
particular, the zero branch is not simplified to `A<=B`; doing so would alter
the frozen proof-rule stream and its rule-cap accounting. Original-formula
replay, not an algebraically substituted formula, remains final authority.

A constant-positive `C` and an identically zero `C` retain the predecessor's
singleton behavior. The established exact root-monus relation, strict, and
equality cases also remain singleton alternatives. This entrance adds no
lower-bound store and no general monus simplifier.

## Formula-by-atomic raw branch admission

Formula expansion first produces the same complete signed raw DNF as the
Boolean finite-union predecessor. For each raw formula conjunction, the new
scanner then forms the lazy Cartesian product of every literal's atomic
alternatives. A predecessor rule result, ignored result, or contradictory
result contributes one alternative. Each newly supported extremum or may-zero
monus atom contributes two.

The existing generated-branch cap observes complete witnesses in this
combined stream:

```text
raw count = sum over raw formula branches
              (product of per-literal atomic alternative counts)
```

Admission stops productively at `limit+1`. It occurs before any complement
removal, duplicate removal, Boolean absorption, proof-rule collection, or box
canonicalization. Consequently:

- two binary atoms in one conjunction have four raw witnesses;
- a negated equality whose two formula alternatives contribute two atomic
  choices and one predecessor choice has three raw witnesses;
- a duplicate or contradictory raw formula branch still consumes admission
  work even when later canonical cleanup removes it;
- a configured cap one below the complete product reports the existing
  generated-branch failure with bounded observation `limit+1`.

The witness stream contains only unit values. It carries neither replacement
formula syntax nor a proof payload, and it is not retained in positive
evidence.

## Original-literal canonicalization and ordered coverage expansion

Only after the raw product cap succeeds does canonicalization run. It is the
unchanged predecessor canonicalizer over the original checked formula
literals:

1. turn each raw formula branch into `Set (LengthFormula variable)`;
2. remove duplicate literals;
3. drop an exact literal/complement branch;
4. deduplicate equal literal sets;
5. remove a strict literal-set superset by Boolean absorption;
6. retain surviving sets in `Set` order.

The implementation deliberately does not canonicalize proof rules. Each
surviving original-literal set is traversed in `Set` order and each literal is
re-expanded into explicit `RelationalPositiveAffineClauseCoverage`
alternatives. `RelationalPositiveAffineClauseIgnored` and
`RelationalPositiveAffineClauseContradiction` remain explicit values alongside
ordered rule lists.

The resulting Cartesian stream is the canonical expanded branch stream used
for rule collection. There is:

- no manufactured `LengthFormula` representing an atomic alternative;
- no `Set` of `RelationalPositiveAffineRule`;
- no deduplication or absorption of rule alternatives;
- no added `Eq` or `Ord` instance requirement for the proof-rule type.

Rule-cap and closure-inspection errors use zero-based indices into this
expanded stream. The indices are therefore after original-literal complement,
deduplication, and subsumption, but after atomic re-expansion as well. Two
distinct expanded branches may close to equal rule sequences or equal boxes;
they retain distinct branch-local work and indices. Equal boxes are removed
only at the later existing box-antichain stage.

## Branch-local rules, closure, and the 64-rule boundary

Each expanded branch is collected in coverage order. Ignored coverage adds no
rule. Contradictory coverage drops the whole branch. Rule coverage appends its
rules in the displayed within-atom order. The existing per-branch rule cap is
checked before closure.

No new ceiling exists. The default remains:

```text
lengthBooleanFiniteUnionRuleLimitPerBranch = 64
```

An extremum equality contributes three rules to each alternative; a may-zero
monus equality contributes two. Under a configured limit of two, the former
reports observed three. Under a configured limit of one, the latter reports
observed two. Under the default ceiling, a 65th collected rule reports the
existing bounded observation 65. The validator does not discard a duplicate
rule to evade the cap.

Admitted rules enter the unchanged synchronous closure:

1. constant-right rules are partitioned as ordered seeds;
2. each pass reads one immutable bounds snapshot;
3. pending rules are inspected in order;
4. rules eligible in that snapshot fire once and are removed;
5. derived maxima merge with `min` only after the pass;
6. a pass with no firing ends closure.

Every expanded branch completes its bounded rule collection and closure
before missing coverage is considered. A closure contradiction drops that
branch. If a live branch lacks a maximum, the first source-ordered missing
compact input returns ordinary `LengthApplicableDomainInapplicable`; an
unbounded alternative cannot be discarded without omitting part of the
applicable domain.

## Box antichain, no hull, and exact replay

Every completely bounded live branch yields one inclusive maximum vector.
The inherited box canonicalizer deduplicates equal vectors, removes a vector
componentwise contained in another, and orders the remaining maximal antichain
lexicographically. Incomparable boxes remain separate.

For example:

```text
min(x,y) <= 1 and x <= 3 and y <= 3
```

uses the two alternatives `x<=1` and `y<=1`, producing:

```text
boxes        = [[1,3],[3,1]]
visits       = 8 + 8 = 16
assignments  = 12
applicable   = 12
```

The componentwise hull `[3,3]` would contain 16 unique assignments and add
four cross-corner inputs satisfying neither alternative. It is never
manufactured.

The may-zero relation gives a second exact union:

```text
x <= (3 monus y) and y <= 4
```

Its zero and bound alternatives yield:

```text
boxes        = [[0,4],[3,3]]
visits       = 5 + 16 = 21
assignments  = 17
applicable   = 11
```

Replacing the relation with `x = (3 monus y)` retains the same cover and
counts five applicable assignments. These receipt counts describe the cover
and original-formula replay separately; proof alternatives never replace the
checked precondition.

Raw visits still sum each retained box cardinality, so overlap is counted per
box. Last-input-fastest enumeration inserts assignments into `Set [Natural]`
under the existing unique-assignment limit. `Set.toAscList` then provides one
global lexicographic replay order. The first indexed evaluation rejection or
counterexample is global-set ordered rather than expanded-branch or box
ordered. Ignored atoms remain in the original precondition and are evaluated
there.

Empty union, nullary truth, provider evaluation, candidate result evaluation,
and the first counterexample retain the Boolean finite-union behavior
unchanged. No proof alternative grants permission to skip original-formula
evaluation.

## Reused limits and errors

This checkpoint adds no configuration type, limit field, default, or error
constructor. It reuses `LengthBooleanFiniteUnionLimits` and these defaults:

| Projection | Default |
| --- | ---: |
| `lengthBooleanFiniteUnionGeneratedBranchLimit` | 256 |
| `lengthBooleanFiniteUnionRuleLimitPerBranch` | 64 |
| `lengthBooleanFiniteUnionClosureInspectionLimitPerBranch` | 4096 |
| `lengthBooleanFiniteUnionRetainedBoxLimit` | 256 |
| `lengthBooleanFiniteUnionAssignmentVisitLimit` | 262144 |

The existing input-box defaults remain eight compact inputs and 65,536 unique
assignments. Evaluation limits continue to bound assigned and intermediate
values during original-problem replay.

Scalar direct validation returns the unchanged
`LengthBooleanFiniteUnionApplicableDomainValidationError`. Product direct
validation returns the unchanged nominal
`LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError`. Query
validation continues to use
`LengthSMTLibBooleanFiniteUnionApplicableDomainValidationError` and
`LengthSpinePairSMTLibBooleanFiniteUnionApplicableDomainValidationError`.
Their generated-branch, rule, closure, retained-box, value, visit, unique
assignment, evaluation, and invariant constructors and payload shapes remain
literal.

## Exact precedence

The scalar and product validators retain this order:

1. reject excess compact input width before demanding the precondition;
2. lazily count complete formula-DNF by atomic-alternative witnesses and
   enforce the existing generated-branch cap;
3. canonicalize original formula literal sets by complement removal,
   deduplication, and strict-superset absorption;
4. re-expand each canonical set in set/literal and atomic-alternative order;
5. enforce each expanded branch's rule cap and closure-inspection cap;
6. drop contradictory branches;
7. return the first source input unbounded in any live branch;
8. deduplicate and componentwise-antichain live boxes;
9. enforce the retained-box cap;
10. check maximum values in box and source-input order;
11. compute and enforce raw assignment visits;
12. materialize the union under the existing unique-assignment cap;
13. replay the original checked problem in global set order;
14. return the first indexed evaluation rejection or exact counterexample;
15. construct the fresh nominal receipt after complete replay;
16. in a query wrapper, check exact behavioral-problem association last.

Thus raw product accounting cannot be reduced by later Boolean cleanup, and
rule/closure work cannot be bypassed by an eventual missing-bound result.
Empty-union completion retains the predecessor's documented shortcuts after
all branch-local work establishes that the live union is empty.

## Public receipt surface and exact tags

Scalar establishment returns the opaque type:

```text
ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
```

Its projections are:

- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis`.

Product establishment returns the nominally separate opaque type:

```text
ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
```

Its projections are:

- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBoxCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentVisitCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainApplicableAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainBasis`.

Each receipt is a fresh strict six-field value containing its schema tag,
inclusive maximum boxes, visits, unique assignment count, applicable count,
and exact finite-spine/provider-law basis. Box count is derived from the boxes
projection. Both receipts have `NFData`; their constructors are private, and
neither is a wrapper or coercion around a predecessor or scalar/product
sibling.

The public tag projections are:

- `lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag`;
- `lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomainValidationSchemaTag`.

Their exact bytes are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1
```

## Compatibility and identity boundary

This validator is additive. The complete Boolean finite-union predecessor and
every earlier direct, positive-affine, relational, strict, quotient,
root-extrema, and root-monus entrance remain separately selectable. Their API
names, receipt representations, tags, scanners, errors, limits, precedence,
and contradiction/nullary behavior remain byte-for-byte literal.

In particular, selecting this new validator does not alter:

- the checked contract, candidate, complete problem, or behavioral-problem
  identity;
- normalized formula bytes, input symbols, SMT-LIB check commands, value
  requests, or query fingerprint;
- decoded responses, protocol plans, execution policies, process or worker
  identities, observations, or live strengths;
- the nominal scalar/product domain boundary;
- any predecessor receipt or tag.

Formula branches, atomic alternatives, affine summaries, proof rules, bounds,
boxes, replay sets, and call-time limits are solver-independent validation
state. They are never inserted into SMT-LIB or retained in query identity. A
query wrapper emits no command, launches no worker, consumes no raw or live
solver status, and adds only exact problem/evidence association after direct
validation.

The new receipt establishes that its canonical union covers every input which
satisfies the original checked precondition and that every unique assignment
in that cover was replayed under the exact checked finite-spine model and
retained provider-law basis. It does not establish source-language
termination, totality, realization, or effects; validate a provider
implementation; make solver status trustworthy; prove behavior outside the
checked model; or grant pruning or candidate-suppression authority.

## Characterization boundary

Focused characterization should pin:

- all four new extremum relation/strict laws and first-child/second-child
  alternative order;
- maximum and minimum equality in both canonical orientations, including the
  three-rule within-branch order;
- may-zero monus relation and equality in both orientations, zero-first order,
  and the unsimplified common equality predecessor rule;
- constant-positive and identically zero monus predecessor parity;
- formula-DNF by atomic-alternative Cartesian products, including complete
  count before cleanup and exact `limit`/`limit+1` admission;
- the three-branch negated-equality boundary;
- original-literal complement, deduplication, and subsumption before ordered
  coverage expansion;
- explicit ignored and contradictory coverage and the absence of proof-rule
  deduplication;
- extremum equality rule cap two/three, monus equality one/two, and the
  default 64/65 boundary;
- both-root, nested, embedded, mixed, effectively n-ary, conditional, and
  unsupported all-or-nothing exclusion;
- `[1,3]`/`[3,1]` with 16 visits and 12 unique assignments;
- `[0,4]`/`[3,3]` with 21 visits, 17 unique assignments, 11 applicable relation
  assignments, and five applicable equality assignments;
- overlap removal, componentwise antichain order, global replay order, first
  failure/counterexample precedence, and exact query association;
- scalar/product receipt opacity, `NFData`, projections, exact tags, and
  nominal separation;
- literal behavior and identity parity for every predecessor entrance.
