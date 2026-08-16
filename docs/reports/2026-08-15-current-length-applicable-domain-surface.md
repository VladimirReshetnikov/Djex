# Current Length applicable-domain surface reset

Date: 2026-08-15

## Outcome

Djex now exposes one current applicable-domain validator for scalar finite
list-spine Length problems and one nominally separate binary-product sibling.
Both run the complete bounded recursive piecewise-affine finite-union analysis
and replay the original checked problem. The short names now denote that
complete current behavior.

The public predecessor ladder was deleted. There are no compatibility aliases,
deprecation shims, version-selection arguments, or migration path. Djex is
experimental and under active development; it promises neither stability nor
backward compatibility, and it has no userbase whose integrations need to be
preserved. Keeping ten selectable public snapshots would have made historical
implementation stages look like supported policy choices.

The lower-level analyses remain private. They are still valuable as an
ordered semantic fallback pipeline and regression structure, but callers
cannot select or persist them as distinct contracts.

## Current public surface

The direct entrances are:

- `validateLengthProblemApplicableDomain` for a checked scalar problem;
- `validateLengthSpinePairProblemApplicableDomain` for a checked product
  problem.

Both accept, in order:

1. `LengthEvaluationLimits`;
2. `LengthInputBoxLimits`;
3. `LengthBooleanFiniteUnionLimits`; and
4. the exact checked problem.

`LengthBooleanFiniteUnionLimits` survives as the current resource-cap bundle
for the private branch pipeline. Its historical name is not a public algorithm
selector.

The direct operational failure types are:

- `LengthApplicableDomainValidationError`;
- `LengthSpinePairApplicableDomainValidationError`.

The shared result vocabulary remains
`LengthApplicableDomainValidation counterexample established`, with three
outcomes:

- `LengthApplicableDomainInapplicable` when the conservative current analysis
  cannot bound every compact input in every live branch;
- `LengthApplicableDomainCounterexample` for the first original-problem
  counterexample found by bounded replay;
- `LengthApplicableDomainEstablished` after complete replay of the canonical
  assignment union.

The query-owned entrances are:

- `validateLengthSMTLibQueryApplicableDomain`;
- `validateLengthSpinePairSMTLibQueryApplicableDomain`.

Their nominal failure types are:

- `LengthSMTLibApplicableDomainValidationError`;
- `LengthSpinePairSMTLibApplicableDomainValidationError`.

A query wrapper calls the same direct validator and then checks exact
behavioral-evidence association with the query-owned problem. It emits no
SMT-LIB command, starts no worker, consumes no solver observation, and treats
query association as the final possible failure.

## Current receipt surface

Scalar establishment returns the opaque
`ValidatedLengthApplicableDomain`. Its six projections are:

- `validatedLengthApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthApplicableDomainBoxCount`;
- `validatedLengthApplicableDomainAssignmentVisitCount`;
- `validatedLengthApplicableDomainAssignmentCount`;
- `validatedLengthApplicableDomainApplicableAssignmentCount`;
- `validatedLengthApplicableDomainBasis`.

Product establishment returns the nominally separate opaque
`ValidatedLengthSpinePairApplicableDomain`. Its six projections are:

- `validatedLengthSpinePairApplicableDomainInclusiveMaximumBoxes`;
- `validatedLengthSpinePairApplicableDomainBoxCount`;
- `validatedLengthSpinePairApplicableDomainAssignmentVisitCount`;
- `validatedLengthSpinePairApplicableDomainAssignmentCount`;
- `validatedLengthSpinePairApplicableDomainApplicableAssignmentCount`;
- `validatedLengthSpinePairApplicableDomainBasis`.

Each receipt privately stores an algorithm schema identity, canonical
inclusive-maximum boxes, assignment visits, unique assignments, applicable
assignments, and the exact finite-spine/provider-law basis. Box count is
derived from the boxes. Constructors are private, and scalar evidence cannot
be used as product evidence or vice versa.

Receipt schema-tag projections and their bytes are not public API. Callers
must not persist, compare, or dispatch on the private tag. The private bytes
continue to identify the current algorithm internally, but their historical
descriptive spelling is not a compatibility promise.

## Deleted public contracts

Before this reset, each incremental analysis checkpoint was separately
selectable through scalar and product problem functions, scalar and product
SMT-LIB query functions, nominal receipt families and projections, direct and
query error families, and public receipt-schema-tag projections. The deleted
families represented these stages:

1. directly bounded literal coverage under the former short names;
2. positive-affine coverage;
3. relational positive-affine coverage;
4. strict relational coverage;
5. positive-literal quotient coverage;
6. root-extrema coverage;
7. root-monus coverage;
8. Boolean-DNF finite-union coverage;
9. atomic root branching; and
10. recursive piecewise-affine atomic branching under the former long names.

The short functions, errors, and receipts were repurposed for stage 10. Every
other public stage-specific function, receipt, projection, error, and tag
projection was removed. In particular, there is no public way to request the
former direct-only meaning of `validateLengthProblemApplicableDomain`.

This is an intentional contract reset, not a versioned migration. The dated
feature reports through the recursive piecewise-affine checkpoint retain their
historical names and contemporary claims so that design reasoning remains
auditable. They do not define current imports.

## Private analysis order

The complete validator retains one ordered private progression:

```text
direct literal
  -> positive affine
  -> relational positive affine
  -> strict relational
  -> positive-literal quotient
  -> root extrema
  -> root monus
  -> Boolean DNF finite union
  -> atomic root branching
  -> recursive piecewise-affine fallback
```

The order is semantically significant. Every formula leaf first receives all
earlier exact handling. Recursive interpretation is attempted only when:

1. the complete atomic scanner returns exactly its singleton ignored
   alternative; and
2. the relational atom still contains at least one minimum, maximum, or
   natural monus in either operand.

An earlier exact rule, alternative stream, contradiction, or supported
quotient/extremum/monus leaf is retained literally and never reinterpreted.
The private chain therefore preserves predecessor behavior without exposing
predecessor contracts.

The recursive fallback accepts only these signed relational leaf shapes:

```text
L <= R
not (L <= R)
L = R
```

Negative equality has already expanded into two strict alternatives. The
bounded Boolean DNF layer, not the expression splitter, owns outer Boolean
structure.

## Recursive grammar and exact cases

An admitted expression expands to a finite ordered stream of guarded
signed-affine values. The recursive grammar accepts:

- compact in-range input variables;
- natural literals;
- normalized sums, traversed left to right;
- retained positive-literal scales;
- binary minimum, maximum, and monus.

The recursive grammar does not descend through quotient, modulo, `LengthIf`,
a result reference, an out-of-range variable, a retained zero scale, or any
other unsupported child. Atomic-first handling can still accept a complete
leaf involving an earlier supported operation. If one descendant needed by
the recursive fallback is unsupported, the entire fallback atom remains
ignored; it is not approximated.

Piecewise nodes use these exact selector cases:

```text
min(L,R)
  -> [L <= R;     value L]
   | [R + 1 <= L; value R]

max(L,R)
  -> [R <= L;     value L]
   | [L + 1 <= R; value R]

L monus R
  -> [L <= R;     value 0]
   | [R + 1 <= L; value L - R]
```

The first case owns equality. For each binary node, left-child cases are
outermost, right-child cases are next, and the first selector precedes the
second. Descendant guards precede the current selector guard. Relation
operands also form a left-first Cartesian product, after which relation rules
are appended:

```text
L <= R        -> [L <= R]
not (L <= R)  -> [R + 1 <= L]
L = R         -> [L <= R, R <= L]
```

Rules are neither sorted nor deduplicated. Their literal order is observable
through per-branch rule and closure limits.

## Signed transfer and closure

The positive monus case may create negative constants or coefficients in a
private affine value. For every generated inequality, negative terms move
exactly to the opposite side. The transferred result is an ordinary
natural-coefficient relational rule; no signed bound store or new checked
formula is created.

The existing synchronous rule-once closure remains sole bound authority:

1. constant-right rules seed maxima in rule order;
2. every pass reads one immutable bounds snapshot;
3. eligible pending rules fire once, in order;
4. maxima newly derived during the pass merge with `min` after the pass;
5. a pass with no firing terminates closure.

This is bounded consequence closure, not general linear programming,
lower-bound reasoning, or a numeric least-fixed-point solver.

## Raw admission and canonical branches

The complete formula DNF is the outer raw stream. Within each conjunction,
every leaf contributes its full atomic-first alternative stream. Recursive
piecewise expressions contribute the Cartesian product of all child and
selector cases.

The generated-branch cap observes that raw stream before:

- complement removal;
- literal or branch deduplication;
- strict-superset absorption;
- selector-guard contradiction;
- proof-rule collection and closure;
- box equality, containment, or antichain cleanup.

An impossible recursive case therefore still consumes raw admission work.
Every capped count stops at no more than `limit + 1`.

After raw admission succeeds, Djex canonicalizes the original formula-literal
sets: duplicate literals disappear, an exact literal/complement branch drops,
equal sets deduplicate, strict supersets are absorbed, and surviving sets
remain in `Set` order. Those canonical sets are then re-expanded in set and
recursive-alternative order. Public rule and closure branch indices refer to
this expanded canonical stream, not the earlier raw witness stream.

## Branches, boxes, and original replay

Every expanded branch processes coverage in literal order. Ignored coverage
adds no rule, a coverage contradiction drops the branch, and rule coverage
appends every rule literally. A surviving branch enforces the rule cap and
completes bounded closure. Contradictory recursive cases have already consumed
raw admission work even if their expanded branch drops before closure. All
live branches complete rule collection and closure before missing coverage is
checked. If any live branch lacks a maximum for one compact source input, the
first such input makes the complete validator ordinarily inapplicable;
another bounded branch cannot hide it.

Every bounded live branch contributes one inclusive-maximum vector. The
canonicalizer:

- deduplicates equal vectors;
- removes a vector componentwise contained in another;
- retains incomparable vectors;
- orders the maximal antichain lexicographically.

It never creates a componentwise hull. Raw assignment visits sum every box
cardinality and count overlap repeatedly. A bounded `Set [Natural]` union
deduplicates assignments, and `Set.toAscList` supplies one global
lexicographic replay order.

The original checked precondition and postcondition are final authority over
every replayed assignment. Selector guards, derived rules, boxes, and solver
status cannot replace that replay. The first evaluation rejection or exact
postcondition counterexample stops traversal; only complete success creates a
receipt.

## Limits and exact precedence

The current algorithm uses these defaults:

| Projection | Default |
| --- | ---: |
| `lengthBooleanFiniteUnionGeneratedBranchLimit` | 256 |
| `lengthBooleanFiniteUnionRuleLimitPerBranch` | 64 |
| `lengthBooleanFiniteUnionClosureInspectionLimitPerBranch` | 4096 |
| `lengthBooleanFiniteUnionRetainedBoxLimit` | 256 |
| `lengthBooleanFiniteUnionAssignmentVisitLimit` | 262144 |
| `lengthInputBoxInputLimit` | 8 |
| `lengthInputBoxAssignmentLimit` | 65536 |

`LengthEvaluationLimits` separately governs assigned and intermediate values
during maximum admission and replay.

Scalar and product validation use this exact operational precedence:

1. reject excess compact input width;
2. count lazy raw formula-DNF by atomic-first recursive alternatives;
3. enforce the generated-branch cap;
4. canonicalize original formula-literal sets;
5. re-expand surviving sets in set and recursive-alternative order;
6. enforce the rule cap for each expanded branch;
7. enforce the closure-inspection cap for each expanded branch;
8. drop contradictory branches;
9. return the first source input unbounded in any live branch;
10. construct the deduplicated componentwise-maximal box antichain;
11. enforce the retained-box cap;
12. check maximum values in box and input order;
13. compute and enforce raw assignment visits;
14. materialize the union under the unique-assignment cap;
15. replay the original checked problem globally in lexicographic order;
16. return the first indexed evaluation rejection or counterexample;
17. construct the nominal receipt after complete replay;
18. for a query wrapper, check exact problem association last.

Later cleanup or failure cannot bypass earlier bounded work.

## Canonical discriminators

For scalar inputs `x` and `y`, the precondition

```text
max(x,y) <= 3 monus min(x,y)
x <= 3
y <= 3
```

has eight raw recursive alternatives. A cap of seven observes eight. The
canonical retained domain is:

```text
boxes        = [[2,3],[3,2]]
box count    = 2
visits       = 24
assignments  = 15
applicable   = 10
basis        = ProviderIndependentFiniteSpineModel
```

The boxes overlap in nine assignments. Visits count the overlap twice; the
union counts it once. No `[3,3]` hull is created.

For a product fixture, define:

```text
u = min(x,y) + (x monus y)
v = min(x,y) + (y monus x)
max(u,v) <= 2
x <= 3
y <= 3
```

Four cases for `u`, four for `v`, and two for the outer maximum produce 32
raw alternatives. A cap of 31 observes 32. The retained result is:

```text
boxes        = [[2,2]]
box count    = 1
visits       = 9
assignments  = 9
applicable   = 9
basis        = ProviderIndependentFiniteSpineModel
```

These fixtures distinguish precise antichains and raw cap accounting from an
over-approximating hull.

## Error and authority boundary

The scalar direct error vocabulary covers input width, generated branches,
per-branch rules, per-branch closure inspections, retained boxes, maximum
values, assignment visits, unique assignments, indexed replay evaluation, and
an internal enumeration invariant. Product errors are nominal counterparts.
The SMT-LIB error families wrap the corresponding direct rejection or an
exact evidence/problem association mismatch.

Analysis cases, selector guards, proof rules, bounds, boxes, assignment sets,
and call-time limits enter neither checked problem identity nor SMT-LIB query
fingerprints. Resetting the public function names does not change contract,
candidate, problem, query, response, protocol, execution, worker, or
observation identity.

Established evidence says only that the canonical finite union covers every
input satisfying the original checked precondition and that every unique
assignment in the union was replayed under the exact checked finite-spine
model and retained provider-law basis. It does not:

- establish source-language realization, termination, totality, or effects;
- validate a concrete provider implementation;
- make `sat`, `unsat`, or `unknown` authoritative;
- prove behavior outside the checked model;
- authorize candidate pruning or suppression.

## Validation evidence

The production surface reset landed at
`a9ab5c52a937df8613ab940de7b94fa92ef9b6ce`; the frozen code-and-test snapshot
is `e8f2cbab8318f2e3a547cea38ec609b3f052c1a7`. Validation completed with:

- strict `-Werror` builds of `test:djex-api-tests` and
  `test:synthesis-length-tests`;
- 9/9 focused current recursive applicable-domain cases;
- 37/37 Djex API cases;
- 366/366 Length cases;
- all 16 repository suites green, totaling 1,803 tests;
- clean `cabal check`, stale-surface scan, and `git diff --check`.

The rewritten `synthesis/test-length/Spec.hs` is 14,784 lines with 300 literal
`testCase` tokens, down from 20,308 lines and 371 tokens. The frozen four-file
test snapshot has combined SHA-256
`4c5e087569383daf880ca7bc257e7884485e0cf2b87caa57c28ddac56ee5f41f`.
It is the hash of this newline-terminated relative-path manifest, in the
explicit order shown:

```bash
{
  sha256sum synthesis/test-length/Spec.hs
  sha256sum test-api/FacadeSpec.hs
  sha256sum test-api/AbstractionBoundary.hs
  sha256sum test-api/Spec.hs
} | sha256sum
```

Current public export lists, tests, and active documentation contain no
predecessor public validator/receipt/tag surface. The two retained private
internal tags and dated historical reports are excluded from that stale scan
by design.

## Documentation boundary

The active API is described by the
[library guide](../library-api.md#validate-the-current-applicable-domain) and
[semantic foundations](../semantic-foundations.md#current-recursive-piecewise-affine-applicable-domain-validation).
The historical
[recursive piecewise-affine report](2026-08-15-recursive-piecewise-affine-length-applicable-domain.md)
and its predecessors remain useful algorithm-development records, but this
report supersedes every public-surface statement in that ladder.
