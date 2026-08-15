# Recursive piecewise-affine Length applicable-domain validation

Date: 2026-08-15

## Outcome

Djex now has a separately named and tagged cumulative applicable-domain
validator which extends exact Boolean finite unions from one immediate
root-extremum or may-zero-monus split to recursively nested piecewise-affine
extrema and natural monus. It retains the existing explicit box antichain,
bounded branch-local closure, and exhaustive global replay. It never replaces
a piecewise domain with a componentwise hull.

The scalar entrances are:

- `validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`;
- `validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`.

The nominal binary-product entrances are:

- `validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`.

Their arguments remain, in order, `LengthEvaluationLimits`,
`LengthInputBoxLimits`, `LengthBooleanFiniteUnionLimits`, and the checked
problem or sealed query. Their result remains the established three-way
`LengthApplicableDomainValidation`: ordinary inapplicability, the first exact
counterexample found by bounded replay, or fresh opaque positive evidence.

In this current tree, the complete atomic predecessor remains separately
selectable and is the regression-parity reference. Its exact alternatives,
ordering, caps, errors, tags, and identity boundary are documented in the
[atomic-branching report](2026-08-15-atomic-branching-length-applicable-domain.md).

This report is an experimental current-tree snapshot, not a stability or
backward-compatibility promise. Public API names, tags, errors, and exact bytes
may be revised before a stable release. Every predecessor comparison below is
a current regression characterization only.

## Atomic-first fallback boundary

Every signed formula leaf first enters the complete atomic-branching scanner.
Recursive interpretation is attempted only when both conditions hold:

1. the predecessor result is exactly the singleton
   `RelationalPositiveAffineClauseIgnored`; and
2. the relational atom retains at least one `LengthMinimum`, `LengthMaximum`,
   or `LengthMonus` somewhere inside either operand.

An already supported predecessor leaf is never reinterpreted. This includes
its exact singleton rules, two-way atomic alternatives, explicit
contradiction, and ordinary ignored results for atoms without a recursive
piecewise-affine operation. Immediate root extrema and may-zero monus
therefore retain their frozen predecessor rule streams rather than being
rederived through the more general case splitter.

The fallback accepts only the relational leaf shapes which the signed Boolean
DNF supplies:

```text
L <= R
not (L <= R)
L = R
```

Negative equality has already become its established pair of strict signed
formula alternatives. Other Boolean structure is handled by the existing DNF,
not by the expression case splitter.

## Recursive expression grammar

One admitted expression expands to a finite ordered list of guarded affine
values. A branch privately stores:

```text
ordered positive-sided selector guards
signed affine value c + sum(k_i*x_i)
```

The constant and coefficients of the value are arbitrary-precision integers.
They may be negative only because the selected positive case of natural monus
uses subtraction. Guards are immediately expressible as the existing
positive-sided affine rules.

The exact base grammar is:

- a compact checked input in range;
- a natural literal;
- `LengthSum`, expanded term by term from left to right;
- a retained positive-literal `LengthScale`;
- binary `LengthMinimum`;
- binary `LengthMaximum`;
- binary `LengthMonus`.

The case splitter descends recursively through sum, scale, minimum, maximum,
and monus children. A sum starts with affine zero and takes the Cartesian
product with each term's cases in normalized term order. A scale multiplies
the signed value but preserves its selector guards.

A retained zero scale is rejected by the fallback rather than treated as a
new recursive authority. Ordinary normalization can already fold a source
zero scale before this stage. An out-of-range variable rejects the complete
recursive atom.

The recursive grammar deliberately does not descend through:

- `LengthQuotient`;
- `LengthModulo`;
- `LengthIf`;
- a result reference or any other non-input variable;
- another unsupported or nonlinear expression node.

If any descendant is unsupported, no recursive alternative survives. The
complete atom falls back to the predecessor's ignored coverage. The successor
therefore adds no recursive quotient, modulo, expression-conditional,
result-reference, or general nonlinear claim. Predecessor-supported quotient
and other earlier leaves retain their existing authority because the
atomic-first gate does not reinterpret them.

## Exact selector cases and order

Let `L` and `R` denote the signed affine values selected by one left-child and
one right-child case. After retaining the left descendant guards and then the
right descendant guards, the three piecewise operators append these exact
choices:

```text
min(L,R)
  -> [L <= R; value L]
   | [R + 1 <= L; value R]

max(L,R)
  -> [R <= L; value L]
   | [L + 1 <= R; value R]

L monus R
  -> [L <= R; value 0]
   | [R + 1 <= L; value L - R]
```

The first alternative owns equality. The second uses an exact natural strict
inequality, so the cases are disjoint and complete:

- minimum selects its left value on `L = R`;
- maximum selects its left value on `L = R`;
- monus selects zero on `L = R`.

For every binary node, ordering is:

1. the first left-child case;
2. within it, each right-child case;
3. within that pair, the first selector choice and then the second;
4. repeat for later right- and left-child cases.

Descendant guards precede the current selector guard. Nested expressions
therefore retain a deterministic depth-first, left-before-right proof order
without sorting or deduplicating proof rules.

Once both relation operands have expanded, their cases also form a left-first
Cartesian product. The relation appends its rules last:

```text
L <= R        -> [L <= R]
not (L <= R)  -> [R + 1 <= L]
L = R         -> [L <= R, R <= L]
```

Equality order is at-most then reverse at-most. Selector and relation rules
are retained even if two happen to be extensionally equal. This ordering is
observable at the existing per-branch rule and closure caps and through
zero-based public branch error indices.

## Signed affine transfer

Signed values never enter the existing closure directly. For a generated
inequality

```text
a_0 + sum(a_i*x_i) <= b_0 + sum(b_i*x_i)
```

the implementation transfers every negative term to the opposite side. The
left positive-sided summary contains the positive terms of `a` and the
negated negative terms of `b`; the right summary contains the positive terms
of `b` and the negated negative terms of `a`. The result is one ordinary
`RelationalPositiveAffineRule` with natural constants and coefficients.

This transfer is exact integer algebra. It does not construct a checked
`LengthFormula`, consume a syntax-node or public-literal budget, or create a
signed closure. After transfer, the unchanged positive-sided synchronous
closure is the sole authority for contradiction and upper bounds:

1. constant-right rules seed bounds in rule order;
2. each pass reads one immutable bounds snapshot;
3. eligible pending rules fire once in order;
4. newly derived maxima merge with `min` after the pass;
5. a pass with no firing ends closure.

This remains bounded rule-once consequence closure, not a general linear
program, lower-bound database, or numeric least-fixed-point solver.

## Newly admitted shapes and exclusions

Because every admitted child recurses, the fallback covers the exact
piecewise-affine cases of shapes the atomic predecessor intentionally ignored:

- extrema or monus nested beneath another extremum or monus;
- extrema or monus embedded in a sum or positive scale;
- piecewise operations on both relation operands;
- mixed extrema/monus expressions;
- normalized effectively n-ary extrema represented by nested binary nodes.

This is not indiscriminate recursion. The following still leave the complete
fallback atom ignored:

- any quotient, modulo, or conditional anywhere in a required recursive
  operand;
- an unknown or out-of-range variable;
- a retained zero scale;
- any other unsupported expression child;
- a non-relational formula leaf.

Unsupported descendants are not erased, approximated, or replaced with a
convenient child. Positivity is not borrowed from another formula literal.
An ignored atom remains in the original checked precondition and still
controls applicability during final replay if other clauses establish a
complete finite cover.

## Raw branch admission before cleanup

The complete signed formula DNF remains the outer branch source. For each raw
formula conjunction, every literal contributes its atomic-first alternative
stream. A recursively admitted leaf contributes the full Cartesian product of
all child and selector cases. The generated-branch witness count remains:

```text
raw count = sum over raw formula branches
              (product of each literal's complete alternative count)
```

The existing generated-branch cap observes this lazy stream productively and
stops at `limit+1`. It runs before:

- exact complement removal;
- literal or formula-branch deduplication;
- Boolean strict-superset absorption;
- selector-guard contradiction;
- proof-rule collection or closure;
- box equality, containment, or antichain cleanup.

Consequently, an algebraically impossible recursive case still consumes raw
admission work. Cleanup cannot retroactively make an over-limit expression
admissible.

Only after raw admission succeeds are the original checked formula branches
canonicalized exactly as in the atomic predecessor:

1. each conjunction becomes `Set (LengthFormula variable)`;
2. duplicate literals disappear;
3. an exact literal/complement branch drops;
4. equal sets deduplicate;
5. a strict literal-set superset is removed by absorption;
6. surviving sets remain in `Set` order.

Each surviving original-literal set is then traversed in set order and
re-expanded into atomic-first recursive coverage choices. Zero-based
rule/closure branch indices name this expanded canonical stream. They do not
name the earlier raw witness stream.

As before, the implementation manufactures no alternative `LengthFormula`,
places no `RelationalPositiveAffineRule` in a `Set`, adds no proof-rule
deduplication or absorption, and needs no `Eq` or `Ord` instance for rules.

## Branch-local closure and finite union

Every expanded branch collects coverage in order. Ignored coverage appends no
rule. Contradictory coverage drops that branch. Rule coverage appends every
rule literally, and the existing per-branch rule cap is enforced before
closure. A deep recursive atom can multiply alternatives faster than it adds
rules, so the frozen default 64/65 discriminator stays within the 32-clause
syntax ceiling: one embedded recursive maximum equality contributes three
rules, two immediate atomic maximum equalities contribute three each, and 28
root-maximum upper relations contribute two each. Every expanded branch
therefore has `3 + 2*3 + 28*2 = 65` rules. The three binary equalities produce
only `2*2*2 = 8` raw alternatives, safely below the default generated-branch
limit. The rule cap reports branch zero, limit 64, observed 65.

All expanded branches complete their bounded rule collection and closure
before missing coverage is inspected. A closure contradiction drops a branch.
If any live branch lacks a maximum for a compact source input, the first such
input makes the whole validator ordinarily inapplicable. An unbounded
alternative cannot be discarded merely because another case is bounded.

Every completely bounded branch supplies one inclusive maximum vector. The
unchanged canonicalizer:

- deduplicates equal vectors;
- removes a vector componentwise contained in another;
- retains incomparable vectors;
- orders the maximal antichain lexicographically.

Raw visits sum every retained box cardinality, including overlap. The union is
then materialized as `Set [Natural]` under the existing unique-assignment cap,
and `Set.toAscList` gives one global lexicographic replay order. The original
checked precondition and postcondition remain final authority. Neither
selector guards nor proof rules replace replay.

## Canonical scalar discriminator

For compact inputs `x` and `y`, use the normalized precondition:

```text
max(x,y) <= 3 monus min(x,y)
x <= 3
y <= 3
```

The left maximum contributes two cases; the right minimum and enclosing monus
contribute four. Thus the complete atom has eight raw alternatives, and a
generated-branch cap of seven observes eight. Closure and antichain cleanup
retain:

```text
boxes        = [[2,3],[3,2]]
box count    = 2
visits       = 24
assignments  = 15
applicable   = 10
basis        = ProviderIndependentFiniteSpineModel
```

The two boxes overlap in nine assignments. Raw visits count the overlap twice;
the global assignment set counts it once. A componentwise hull `[3,3]` would
contain one extra unique assignment beyond the exact recursive union and is
not manufactured.

The atomic predecessor ignores the recursive atom. The ordinary clauses alone
therefore give:

```text
boxes        = [[3,3]]
box count    = 1
visits       = 16
assignments  = 16
applicable   = 10
basis        = ProviderIndependentFiniteSpineModel
```

The equal applicable count does not make the predecessor cover exact:
original-formula replay merely filters six hull assignments after visiting
them. The recursive successor establishes the smaller exact antichain before
replay.

## Canonical product and raw-cap discriminator

For the product fixture, define these recursive expressions over compact
inputs `x` and `y`:

```text
u = min(x,y) + (x monus y)
v = min(x,y) + (y monus x)
```

Use the precondition:

```text
max(u,v) <= 2
x <= 3
y <= 3
```

Each inner minimum/monus combination gives four expression cases for `u` and
four for `v`. The outer maximum contributes two selector choices:

```text
4 * 4 * 2 = 32 raw alternatives
```

A generated-branch limit of 31 stops with observed 32. A limit of 32 admits
the atom. Cases which later prove contradictory remain part of both counts.

After closure and antichain cleanup, recursive validation retains:

```text
boxes        = [[2,2]]
box count    = 1
visits       = 9
assignments  = 9
applicable   = 9
basis        = ProviderIndependentFiniteSpineModel
```

The atomic predecessor again ignores the recursive atom and retains:

```text
boxes        = [[3,3]]
box count    = 1
visits       = 16
assignments  = 16
applicable   = 9
basis        = ProviderIndependentFiniteSpineModel
```

This fixture pins both recursive case order and raw accounting before guard
contradiction cleanup.

## Reused limits and errors

This checkpoint adds no configuration type, field, default, or error
constructor. It reuses `LengthBooleanFiniteUnionLimits`:

| Projection | Default |
| --- | ---: |
| `lengthBooleanFiniteUnionGeneratedBranchLimit` | 256 |
| `lengthBooleanFiniteUnionRuleLimitPerBranch` | 64 |
| `lengthBooleanFiniteUnionClosureInspectionLimitPerBranch` | 4096 |
| `lengthBooleanFiniteUnionRetainedBoxLimit` | 256 |
| `lengthBooleanFiniteUnionAssignmentVisitLimit` | 262144 |

The existing input-box defaults remain eight compact inputs and 65,536 unique
assignments. Evaluation limits continue to own assigned and intermediate
values during original-problem replay.

Scalar direct validation returns
`LengthBooleanFiniteUnionApplicableDomainValidationError`. Product direct
validation returns the nominally distinct
`LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError`. Query
wrappers keep `LengthSMTLibBooleanFiniteUnionApplicableDomainValidationError`
and
`LengthSpinePairSMTLibBooleanFiniteUnionApplicableDomainValidationError`.
Every bounded cap failure observes at most `limit+1`, including generated
branches, branch rules, closure inspections, boxes, raw visits, and unique
assignments through their existing owners.

## Exact precedence

Scalar and product validation retain this operational order:

1. reject excess compact input width before demanding the precondition;
2. lazily count complete formula-DNF by atomic-first recursive alternatives;
3. enforce the existing generated-branch cap;
4. canonicalize original formula-literal sets;
5. re-expand each surviving set in set and recursive alternative order;
6. enforce each expanded branch's rule cap;
7. enforce each expanded branch's closure-inspection cap;
8. drop contradictory branches;
9. return the first source input unbounded in any live branch;
10. deduplicate and componentwise-antichain the live boxes;
11. enforce the retained-box cap;
12. check maximum values in box and input order;
13. compute and enforce raw assignment visits;
14. materialize the union under the unique-assignment cap;
15. replay the original checked problem in global lexicographic order;
16. return the first indexed evaluation rejection or counterexample;
17. construct the fresh nominal receipt after complete replay;
18. in a query wrapper, check exact behavioral-problem association last.

Raw accounting cannot be reduced by later formula or guard cleanup. Rule and
closure work cannot be bypassed by an eventual missing-bound result. Box work
cannot be bypassed by a later value, visit, unique-assignment, evaluation, or
query-association failure.

## Public receipt surface and exact tags

Scalar establishment returns the opaque type:

```text
ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain
```

Its projections are:

- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainBoxCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainAssignmentVisitCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainApplicableAssignmentCount`;
- `validatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainBasis`.

Product establishment returns the nominally separate opaque type:

```text
ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain
```

Its projections are:

- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainBoxCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainAssignmentVisitCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainApplicableAssignmentCount`;
- `validatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainBasis`.

Each is a strict six-field receipt containing its embedded schema tag,
inclusive maximum boxes, visits, unique assignment count, applicable count,
and exact finite-spine/provider-law basis. Box count is derived from the boxes
projection. Constructors remain private, both types have `NFData`, and neither
is a wrapper or coercion around predecessor or scalar/product sibling
evidence.

The public schema-tag projections are:

- `lengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainValidationSchemaTag`;
- `lengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomainValidationSchemaTag`.

Their exact bytes are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-recursive-extrema-monus-piecewise-affine-branching-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-recursive-extrema-monus-piecewise-affine-branching-precondition-domain-establishment/v1
```

## Current-tree identity and authority boundary

This validator is additive in the current tree. The atomic-branching entrance
and every earlier Boolean finite-union, root-monus, root-extrema, quotient,
strict, relational, positive-affine, and directly bounded entrance are
separately selectable regression references. Their currently observed API
names, receipt representations, tags, scanners, limits, errors, precedence,
nullary behavior, and normalized identities are pinned for this checkpoint,
not promised as a future compatibility surface.

Selecting recursive piecewise-affine validation does not alter:

- checked contract, candidate, complete problem, or behavioral-problem
  identity;
- normalized formula bytes, input symbols, SMT-LIB commands, value requests,
  or query fingerprint;
- response decoding, protocol plans, execution policy, process/worker
  identity, observations, or live strengths;
- nominal separation between scalar and product evidence;
- any predecessor receipt or tag.

Formula branches, recursive cases, selector guards, signed summaries,
positive-sided rules, bounds, boxes, assignment sets, and call-time limits are
solver-independent validation state. They are never encoded into SMT-LIB or
retained in a query identity. A query wrapper emits no command, launches no
worker, and consumes no raw or live solver status. It adds only exact
problem/evidence association after direct validation.

The new receipt establishes that its canonical finite union covers every
input satisfying the original checked precondition and that every unique
assignment in the union was replayed under the exact checked finite-spine
model and retained provider-law basis. It does not establish source-language
termination, totality, realization, or effect behavior; validate a provider
implementation; make solver status trustworthy; prove behavior outside the
checked model; or authorize candidate pruning or suppression.

## Characterization boundary

Focused characterization should pin:

- atomic-first behavior and literal parity for every predecessor result;
- exact minimum, maximum, and monus selector guards, tie ownership, selected
  values, and first/second choice order;
- nested left/right descendant guard order and final relation-rule order;
- at-most, strict, and equality relation rules;
- signed constants and coefficients from positive monus, and exact transfer
  to positive-sided rules;
- nested, embedded, both-root, mixed extrema/monus, and effectively n-ary
  admission;
- all-or-nothing rejection for quotient, modulo, conditional, out-of-range
  variable, retained zero-scale, and other unsupported descendants;
- complete formula-DNF by recursive-case raw counting before every cleanup;
- contradictory recursive cases consuming generated-branch work;
- canonical original-literal cleanup before recursive re-expansion and public
  expanded-stream branch indices;
- no manufactured formula, rule set, rule deduplication, or new rule
  `Eq`/`Ord` requirement;
- generated caps 7/8 on the scalar fixture and 31/32 on the product fixture;
- the 31-clause, eight-raw-alternative rule-cap witness at 64/65;
- width, generated, rule, closure, missing, box, value, visit, unique, replay,
  receipt, and query-association precedence;
- scalar `[[2,3],[3,2]]` with 2/24/15/10 and predecessor
  `[[3,3]]` with 1/16/16/10;
- product `[[2,2]]` with 1/9/9/9, predecessor `[[3,3]]` with
  1/16/16/9, and 32 raw alternatives;
- scalar/product receipt opacity, `NFData`, projections, exact tags, and
  nominal separation;
- current-tree normalized problem/query byte parity and solver-independent
  authority.
