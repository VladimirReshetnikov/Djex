# Supplied generic folds: Haskell composition and Lean integration gate

The Haskell acceptance fixture gives search exactly the intrinsic list
declaration and one generic value signature:

```haskell
foldList :: forall a r. (a -> r -> r) -> r -> [a] -> r
```

Search receives neither its implementation nor implementations of `map`,
`append`, or `length`. Independent GHC execution supplies the ordinary fold
definition and checks generated expressions at the complete target signatures:

```haskell
mapTarget    :: forall a b. (a -> b) -> [a] -> [b]
appendTarget :: forall a. [a] -> [a] -> [a]
lengthTarget :: forall a r. r -> (r -> r) -> [a] -> r
```

The last target is a generalized iteration count: its caller supplies the zero
and successor operation. Tests use both integer and Boolean accumulators and a
nonlinear successor. It therefore does not assume a numeric class or donate a
target-specific length provider. A separate `[a] -> a` query must produce no
candidate, including for the empty list. Contradictory behavioral observations
cannot select any generated implementation.

## Search changes

Djinn now translates generic provider instances in a coherent opaque datatype
vocabulary. Correlated type vectors retain the relationship between the fold's
element and accumulator. Exact demanded result types and ordinary recursive
inputs guide the order of bounded singleton specializations. Constructor-only
composition plans precede their case-analysis alternatives, with historical
families retained as fallbacks.

For these constructor-composition plans under explicit `Interleave` with
alternatives, Djinn introduces the query's outer arrow arguments before
searching its result. This gives the existing normal-form argument search
access to every actual input. In particular, the two list arguments can be
selected in either order: `foldList (:)` alone concatenates them in the opposite
order from `appendTarget`, while the appropriate lambda-wrapped application
passes the original observations.

Every introduced argument consumes a choice from the same global allowance.
Every observed raw proof, including duplicates and rejected proofs, consumes
its original candidate slot. The proof is checked in the opened context and
again after restoring the original arrow binders. Fresh binders avoid source,
provider, diagnostic, and internal names. These incomplete constructor plans
cannot produce a non-inhabitation claim. Both batch and streaming use this
entrance; it changes the finite eta-short prefix only in the selected plan
family. Default, first-only, and depth-first goal-introduction behavior stays
as before.

## Acceptance and limits

`test-integration/RecursorSpec.hs` requires a source graph for every observed
candidate, exact graph/clause association, and an inventory restricted to the
fold and list constructors. Independent GHC replay checks actual generated
expressions under the original signatures against empty, singleton, longer,
heterogeneous, and order-sensitive observations.

Djinn passes `map`, `append`, generalized `length`, and the empty-input control
at **1,024 raw candidates and 100,000 choices**, with the original bounds
unchanged. The subsequent sixteen-cell ordinary-data matrix also passes.
The strict GHC 9.12.4 build uses `-Werror`; logs are
`dist-newstyle/priority-context-recursors-build-v7.log` and
`priority-recursors-djinn-v7.log`.

Exference also passes the same three targets and empty-input control in the
current full 124-test facade suite, with **1,024 candidates, 100,000 steps, and
queue size 1,024**. All 133 Djinn tests pass, including the new tiny-budget
batch/streaming matrix. The complete affected-suite receipt is
[`recursive-context-checkpoint.json`](../../test-integration/receipts/recursive-context-checkpoint.json).
It records 2,090 of 2,092 passing tests across eleven unfiltered suites. The two
failures are existing short-deadline Length process fixtures; an unchanged
focused retry passes both and is recorded
[separately](../../test-integration/receipts/recursive-context-length-retry.json).
This does not claim an unfiltered full-Length pass. Source and executable hashes
remain unchanged during each recorded run.

This establishes a bounded Haskell fold-composition capability. It does not
establish all useful accumulator types, arbitrary structural recursion, tree
fold composition, or the corresponding live Lean queries. The prepared Leant
runner adds supplied list/tree folds and a providers-off rank-N fold-argument
family. Its generic fold definitions must pass Lean termination checking, and
every displayed candidate must replay at its full type with an empty axiom
inventory. Those are still integration gates.

See the [current re-triage](2026-09-07-synthesis-retriage.md) for the remaining
case, contextual-provider, and broader Church acceptance requirements.
