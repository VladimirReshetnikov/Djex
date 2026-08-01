# Bounded recursive datatype elimination — 2026-07-31

## Scope and outcome

Exference can now inspect one constructor layer of a recursive datatype. This
closes the gap between retaining recursive constructors for introduction and
previously discarding the corresponding eliminator from every checked session.
It supports finite terms such as a one-step `Natural` dispatcher or a list
head/default case without introducing a recursive binding.

Djinn's recursive-datatype policy is unchanged. Its complete LJT decision
procedure still treats recursive source datatypes as outside the structurally
expanded fragment.

**Successor note — 2026-08-01:** that sentence records Djinn's policy when this
Exference change landed. Djinn now retains visible recursive datatypes for one
positive constructor layer while keeping recursive fields, negative positions,
and the exact-opaque view atomic. The bounded projection cannot establish
negative evidence when it finds no term. See the
[bounded Djinn recursive-introduction report](2026-08-01-bounded-djinn-recursive-introduction.md).

## Finite search rule

When a scoped value has a recursive datatype and pattern matching is otherwise
enabled, Exference emits the same let/case layer used for a nonrecursive
datatype. Constructor fields are installed as ordinary providers in the
alternative's lexical scope. Unlike the nonrecursive path, those new fields are
not passed back to eager pattern decomposition.

This boundary matters operationally. Exference performs datatype decomposition
while building one search transition, before the resulting node returns to the
step, queue, and depth scheduler. Feeding a self-recursive field directly back
into that operation would diverge inside the transition, where the ordinary
search bounds cannot intervene. Stopping after the first layer makes the
transition finite for direct and mutual recursion alike.

Each constructor alternative receives its own child scope, so fields cannot
leak between siblings. The independent expression checker already validates
the resulting finite case tree against the complete recursive deconstructor
record; no special recursive typing assumption is trusted from search.

## Deliberate limits

The rule does not add:

- recursive calls or target self-reference;
- induction hypotheses or an induction principle;
- automatic matching of a recursive field at a second layer;
- GADT/index refinement or equality substitution; or
- unconditional multi-constructor matching.

The existing `exferenceMultiConstructorPatterns` option still controls case
analysis for datatypes with several constructors, including lists. Recursive
single-constructor products use the ordinary single-constructor path.

`RecursiveDataEliminationUnsupported` remains in the public omission reason
type for source compatibility, but current sessions no longer produce it.

## Regression boundary

The focused coverage establishes that:

- a recursive `Natural` dispatcher is found under strict unused-variable
  checking and independently type-checks;
- the successor field is consumed as an ordinary branch provider without a
  nested eager case;
- constructing that first candidate terminates under a wall-clock guard;
- neutral and Haskell-source sessions retain recursive eliminators without an
  omission diagnostic;
- the stable facade returns the expected checked generated case; and
- the Exference benchmark corpus measures first-candidate latency for the new
  recursive pattern route.
