# Query-correlated guarded-impredicative instantiation

Date: 2026-08-09

> **Successor.** The
> [six-binder widening](2026-08-10-six-binder-instantiation.md) applies this
> same query-correlated family to six leading binders without changing its
> attempt or retained-axiom caps. The five-binder statements below are
> historical facts for this revision.

## Outcome

Djinn can now recover a multi-binder guarded-impredicative instance when the
complete specialized scheme body already occurs in the checked request. For
example,

```haskell
(forall a b. f a b)
  -> f (forall x. x -> x) (forall y. y -> y -> y)
```

can return `\x -> x`. Both quantified arguments are supplied by the request,
and substituting them together into `f a b` produces exactly its requested
result.

The historical one- through three-binder family deliberately retains its
lexically sorted Cartesian order. Under the fixed sixteen-axiom allowance, the
two different guarded quantified candidates above can occur after that prefix.
Changing the historical enumeration in place would perturb established
candidate order. The new rule is therefore a separate positive-only tail.

This is a Djinn completeness extension. Exference already finds the same
identity through its independent quantified-provider rules; the public
integration regression now pins that parity and compiles both generated
candidates with GHC.

## Scheme and candidate boundary

The new family revisits only hypothesis-side schemes embedded in the elaborated
query. Loaded value schemes retain their separate inventory and proof-symbol
family. An eligible scheme is context-free and has a complete leading chain of
at most five binders.

The candidate vocabulary is not enlarged. It contains:

- query variables, skolems from opened positive foralls, and premise-scope
  spellings, retained in source occurrence order for fair scheduling; and
- alpha-deduplicated guarded quantified subtrees already represented in the
  goal or prepared premise formulas, including structural wrappers around a
  quantified atom when they are independent of an enclosing binder.

Thus the tail does not invent a polytype or import a closed monotype from the
separate query-closed family. Its correlation authority is the canonical
elaborated query itself: candidate tuples may be considered from the existing
formula vocabulary, but their fully specialized result must occur as an
alpha-equivalent subtree of that exact query.

## Correlated tuple filtering and bounds

For each eligible scheme, Djinn fairly enumerates candidate vectors through
source windows, repeated arguments, monotone selections from both ends, and a
Cartesian tail. It inspects at most the first 512 raw tuples from that fair
schedule. A tuple enters the family builder only when both conditions hold:

1. at least one argument is a guarded quantified candidate paired with a
   leading binder which occurs free in the scheme body; and
2. substituting the complete vector produces a body whose alpha-normal key is
   present among the canonical elaborated query's subtree keys.

Tuples which put quantified choices only in vacuous binders do not activate the
tail. The raw per-scheme producer bound is also distinct from the builder's
family-wide attempt budget: tuples rejected by either prefilter never spend a
builder attempt.

## Historical exclusion and nested bridges

The structural and nominal builders are each seeded with the exact axiom set
retained by the corresponding active bounded historical run. A correlated
candidate is excluded if its logical formula is already present in that seed or
was retained earlier by the same correlated builder. Formula equality is the
authority: different visible application evidence does not make the logical
axiom new.

Before tuple enumeration, the builder scans every seeded historical formula for
shallower hypothesis-side schemes and schedules those descendants without
spending an eligible attempt. An active historical formula can therefore act as
a bridge to correlated work inside its result without being emitted a second
time. If a later correlated candidate repeats an already-seen formula, the same
discovery runs before that candidate is dropped; this later duplicate spends
one eligible attempt but neither the current scheme's retained-axiom allowance
nor the family's retained-axiom allowance.

The independent instantiation limits are therefore:

- no more than five leading binders per scheme;
- no more than sixteen distinct axioms per scheme;
- no more than 64 axioms per structural or nominal family;
- no more than 512 raw fair tuples prefiltered for each scheme; and
- no more than 512 eligible attempts charged by the builder across the family.

Every complete specialized body still has to pass prepared kind checking and
formula translation. A rejected optional vector drops only that possible
axiom.

## Plan ordering and composition

All established plan and candidate prefixes retain their order. The historical
structural and nominal work, caller-supplied provider plans, and loaded-scheme
structural and optional nominal plans run first. The established query-closed
structural and optional nominal plans retain their former final position after
that prefix and remain unchanged. Pure query-correlated structural and optional
nominal plans are appended after them.

A pure correlated plan carries the established historical axioms, loaded
premises and axioms, and provider specializations, but no query-closed axioms.
Conversely, the unchanged query-closed plans contain no correlated axioms. Only
when both families contribute does a final combined structural and optional
nominal superset carry both axiom sets, allowing one proof to compose the two
query-local capabilities without moving or duplicating either independent plan.
No plan resets the query-wide candidate cutoff or choice-point fuel.

## Soundness and evidence

The exact-result filter is an additional completeness heuristic, not a source
typing assumption: every retained axiom still connects an opaque source scheme
to the formula compiled from its completely substituted body. The prepared
kind environment validates the body, the independent proof checker validates
every returned proof, and evidence conversion erases inferable choices or
retains the shortest checked visible application required by a vacuous binder.

Both structural and nominal query-correlated plans are positive-only. They can
add a checked candidate, but exhausting their finite vocabulary never supports
a proof of non-inhabitation. Historical negative evidence and every established
candidate prefix are unchanged.

## Validation

Validation for this slice covers:

- the focused stable Djinn identity regression for the two distinct quantified
  arguments;
- regressions proving that a quantified choice in only a vacuous binder does
  not activate the tail and that a historical logical formula is not emitted a
  second time;
- correlated instantiation of a nested scheme exposed only through a retained
  historical bridge;
- preservation of the established query-closed first-result prefix and a
  separate combined correlated/query-closed proof;
- the complete `djinn-tests` suite, with all 83 tests passing;
- the public facade regression, which obtains candidates with no residual
  constraints from both Djinn and Exference and compiles both in one module
  using GHC with `Haskell2010`, `RankNTypes`, and `ImpredicativeTypes`; and
- the complete `djex-tests` facade/integration suite, with all 86 tests passing.

No Exference implementation changed in this slice; its candidate is checked as
an independent cross-engine reference for the newly recovered Djinn result.

## Deliberate limits

This is not general impredicative inference, higher-rank subsumption, or
polymorphic-let generalization. It applies only to context-free query-local
schemes with no more than five leading binders, uses only the finite checked
variable and guarded-quantified vocabulary, requires a quantified argument at a
body-relevant binder and an exact query-subtree result, and excludes logical
formulas already retained by the active historical run. Useful vectors beyond
either attempt boundary or the axiom bounds can still be missed. Such a bounded
miss remains inconclusive and never becomes negative evidence.
