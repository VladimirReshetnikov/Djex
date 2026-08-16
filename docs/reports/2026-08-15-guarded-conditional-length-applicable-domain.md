# Guarded conditional Length applicable-domain validation

Date: 2026-08-15

## Outcome

The single current scalar and binary-product applicable-domain validators now
cover fully supported expression-level `LengthIf` nodes inside the existing
bounded recursive piecewise-affine finite-union analysis. The implementation
landed in `eb7ec48aac85dd257d9046a847e47dbce91c264d`.

This is a revision of the current experimental algorithm, not a new selectable
version or a compatibility layer. The short validator functions, public limit
bundle, nominal error types, receipt types, and six receipt projections remain
the only public surface.

## All-or-nothing admission

For `LengthIf condition whenTrue whenFalse`, the recursive fallback first
checks support structurally. Every leaf of both `condition` and its complement
must have exact atomic-first coverage, and both selected expressions must be
admitted recursively. The check includes an arm which a constant condition
makes unreachable. Quotient, modulo, result references, out-of-range
variables, retained zero scales, and any other unsupported required descendant
therefore reject the whole enclosing fallback atom.

This is deliberately conservative. Djex does not drop an unsupported arm,
assume a guard truth value, or approximate a child. The structural support
preflight also avoids forcing the potentially exponential guard DNF before the
existing generated-branch cap can bound it.

## Exact guarded expansion

An admitted conditional contributes this ordered alternative stream:

```text
positive DNF(condition) x recursive cases(whenTrue)
negative DNF(condition) x recursive cases(whenFalse)
```

The true arm precedes the false arm. Within either arm, guard-DNF alternatives
are outermost and selected-expression alternatives are innermost. Guard
coverage precedes selected-arm guards; those guards precede every enclosing
minimum/maximum/monus selector; final relation rules come last. Negative
equality has its exact two strict alternatives.

Coverage fragments remain separate until the enclosing relation is formed. A
contradictory guard therefore keeps its selected value, participates in every
surrounding Cartesian selector product, and consumes raw generated-branch work
before it collapses the expanded branch to a contradiction. Rules are neither
sorted nor deduplicated, so this order remains observable at rule and closure
limits.

## Caps, boxes, and replay

The existing operational precedence is unchanged:

1. compact input width;
2. raw outer DNF and complete conditional/recursive alternative counting;
3. generated-branch cap;
4. original formula-literal-set canonicalization;
5. canonical-set re-expansion;
6. per-expanded-branch rule cap;
7. per-expanded-branch closure-inspection cap;
8. contradictory-branch removal;
9. first input missing a maximum in any live branch;
10. componentwise-maximal box-antichain construction;
11. retained-box cap;
12. maximum-value checks;
13. assignment-visit cap;
14. unique-assignment cap;
15. original-problem replay in global lexicographic order;
16. first indexed evaluation rejection or counterexample;
17. nominal receipt construction;
18. exact query association, when using a query wrapper.

Incomparable boxes remain separate; the analysis never manufactures their
componentwise hull. Overlap counts in raw box visits and is deduplicated only
in the bounded assignment union. Every unique assignment is replayed against
the original checked precondition and postcondition. Conditional guards,
selector rules, inferred maxima, and SMT status are coverage machinery, not
semantic authority.

## Canonical discriminators

The scalar condition

```text
(if x <= 2 then x else 5) <= 3
```

has two raw alternatives. A branch cap of one observes two. The false branch
later becomes contradictory, leaving `[[2]]` with three visits, three unique
assignments, and three applicable assignments.

For

```text
(if x = 0 then 1 else x) <= 2
```

the complemented equality contributes two strict alternatives, so a branch
cap of two observes three. The retained receipt is again `[[2]]` with 3/3/3
visit, unique, and applicable counts.

The two-input fixture

```text
y <= 2
(if x <= 1 then max(x,y) else x monus y) <= 2
```

has four raw alternatives. A cap of three observes four; its first expanded
branch contains four rules, and a closure cap of three observes four
inspections. The exact result is one box `[[4,2]]`, 15 visits, 15 unique
assignments, and 12 applicable assignments. An independent enumeration beyond
the inferred maxima confirmed that every satisfying assignment in the
characterized range is covered. Replacing the false arm with `x mod 2` leaves
the whole conditional atom conservatively unsupported.

Scalar and product validation produce the same metrics under their nominally
separate receipts, and their query wrappers preserve the original query
fingerprints and bytes while checking association last.

## Private receipt identity

Because the algorithm's coverage semantics changed, its package-private
receipt discriminators were reset to:

```text
finite-list-spine-length/guarded-recursive-piecewise-affine-finite-union-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/guarded-recursive-piecewise-affine-finite-union-precondition-domain-establishment/v1
```

The old private descriptive bytes were removed. There is no public schema-tag
projection, persistence format, migration promise, or backward-compatibility
claim. Guard cases, rules, boxes, limits, and replay assignments still enter
neither the checked problem nor the SMT-LIB query fingerprint.

## Validation evidence

Validation completed with strict warning-as-error library and Length-test
builds, 11/11 focused guarded applicable-domain cases, and 370/370 Length
cases. The frozen repository run passed all 16 suites and 1,807 tests,
including 37/37 API cases. `cabal check` and `git diff --check` were clean.

## Documentation boundary

The current contract is defined by the
[library guide](../library-api.md#validate-the-current-applicable-domain) and
[semantic foundations](../semantic-foundations.md#current-guarded-recursive-piecewise-affine-applicable-domain-validation).
The [current-surface reset report](2026-08-15-current-length-applicable-domain-surface.md)
records the short public API, but its exclusion of `LengthIf` describes the
immediately preceding checkpoint. The recursive and earlier
applicable-domain reports remain historical development records rather than
current API or grammar specifications.
