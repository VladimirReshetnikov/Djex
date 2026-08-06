# Quartic rank-N frontiers

Date: 2026-08-06

## Outcome

Djinn now explores the balanced four-site choices left outside its cubic
rank-N approximation. After the fully opened, exact opaque, singleton,
pairwise, and triple plans, search appends deterministic quadruple-opaque and
quadruple-open frontiers. The bounded structural family is therefore
exhaustive for every open/opaque choice across nine independent positive
`forall` sites without becoming an exponential power-set search.

The former eight-site gap—four exact five-binder transports beside four
structurally introduced identities—now synthesizes directly. Prepared global
functions retain the same quartic views, so a reusable consumer can require
that exact mixture without being retranslated for each query.

Exference commit `f3dd2495` separately closes the corresponding live Leant
search gap. Leant's actual neutral serialization lowers each of the four
vacuous five-binder schemes to five ordinary arrows from the `Type` atom; only
the four identity leaves remain explicitly quantified. Exference reaches that
exact arrow-heavy product at the existing 4,096-step, 1,024-entry queue bounds
by combining guarded whole-arrow forwarding with deep structural construction
of the known boxed-tuple tree. Follow-up commit `c0c1a461` bounds duplicate
recursive uses of that tuple shortcut without changing the live result or its
ordering.

## Djinn design

For the positive sites discovered by the polarized traversal, Djinn enumerates
unordered quadruples in stable source order.

- A quadruple-opaque plan starts from the fully opened view and retains the
  selected four sites as exact opaque atoms. It starts at eight sites, where it
  supplies every four-open/four-opaque choice.
- A quadruple-open plan starts from the exact opaque view and opens the selected
  four sites. It starts at nine sites; at eight sites its complements would
  duplicate the quadruple-opaque layer.
- Selecting nested targets opens the union of every ancestor chain needed to
  reach those targets. Unrelated sites remain opaque. One shared ancestry
  helper now implements this rule for singleton, pair, triple, and quadruple
  open selections.

All earlier result-order prefixes remain unchanged. Quartic plans run after
triple plans and before separately bounded hypothesis-instantiation families,
under the same query-wide candidate cutoff and proof-search fuel. Goal and
nominal scheduling, reusable prepared-premise caches, and the deduplicated
skolem inventory all receive the same two new plan categories.

For independent sites, the two extremes and the singleton through quadruple
layers cover all subsets through nine sites. Ten sites expose the next central
boundary: a flat proof needing exactly five sites open and five opaque remains
outside the quartic family. An empty result at that incomplete boundary remains
`NoEvidence`, never a proof of uninhabitability.

## Exference search follow-up

Writing `E x` for five ordinary `Type` arguments and `P` for a right-nested
boxed product, the serialized goal is schematically:

```text
E x = Type -> Type -> Type -> Type -> Type -> x

forall q r z m.
  E q -> E r -> E z -> E m ->
  P [E q, E r, E z, E m,
     forall a. a -> a, forall b. b -> b,
     forall c. c -> c, forall d. d -> d]
```

Scoped Exference bindings store an arrow value as its final result plus a
parameter spine. Ordinary provider use intentionally consumes that spine and
schedules application arguments. For an arrow-shaped goal, however, this hid
the option to reuse the complete scoped function and drove the search through
many equivalent eta-long applications. Commit `f3dd2495` adds a guarded lane
before eta expansion which reconstructs the complete arrow scheme when the
stored spine is nonempty. It uses the existing same-namespace unifier,
provider score, substitution path, binding-use accounting, and independent
candidate checker. It does not alter the scalar-provider path or remove eta
expansion when exact forwarding fails.

A separate sibling branch introduced in `f3dd2495` handles a concrete nonempty
boxed-tuple tree. It materializes every already-known tuple node in one
structural step and creates goals only for non-tuple leaves. The earlier
shallow tuple branch remains in the search and can continue recursively: it is
still needed when an inner product should be supplied by a scoped or
environment value rather than rebuilt structurally.

Offering both choices independently at every nested product also admitted many
duplicate structural histories. On a pure balanced tree of height `h`, the
eager choice at a node competed with every pair of histories chosen by its two
shallow children, giving the recurrence `F(h) = 1 + F(h - 1)^2`. Commit
`c0c1a461` carries a small `TupleGoalMode` provenance marker with each goal.
An independent goal may choose either the eager whole-tree lane or the shallow
lane, but fields emitted by the shallow lane remain recursively shallow through
arrows, nested forall opening, partial applications, and pattern-match
continuations. Independent provider-dependency goals remain eligible to start
the eager lane.

This provenance bounds a pure structural tree to at most two histories—one
eager and one recursively shallow—without forbidding value reuse. A shallow
product goal still tries scoped and environment providers before constructing
its next layer, so a product available only at a deeply nested field remains
usable.

The two branches are complementary on the exact live Leant shape. Deep tuple
construction exposes the eight leaves without repeatedly scheduling the
right-nested product spine; whole-arrow forwarding then reuses each of the four
arrow-valued inputs without eta-expanding it. Focused ablations under the same
4,096-step limit and 1,024-entry queue bound measured:

| Exference configuration | First checker-admitted candidate | Queue entries pruned |
| --- | --- | ---: |
| Baseline | None | 35,693 |
| Whole-arrow forwarding only | None | 35,693 |
| Deep tuple-tree construction only | None | 35,495 |
| Both branches | Step 30 | 36,491 over the full window |

The `c0c1a461` provenance hardening leaves this live golden stable: the first
checker-admitted candidate remains at step 30, and the complete bounded window
still reports 36,491 pruned queue entries.

These numbers characterize this regression and heuristic configuration, not a
general complexity bound. In particular, the larger full-window prune count
for the successful combined search does not weaken the earlier result: the
checked witness is already available at step 30.

## Soundness boundary

The Djinn change adds coherent formula views; it does not add subsumption, invent a
polytype, or trust an unchecked proof term. Every realized clause is converted
from the formula plan which admitted it and passes Djinn's independent proof
checker before reaching the stable candidate API.

The Exference follow-ups change bounded branch scheduling, not its type
relation. Whole-arrow forwarding is exact, deep tuple construction follows the
goal's fixed boxed-product structure, and `TupleGoalMode` records only how a
goal reached that structure. The generated term must still pass the independent
checker before publication.

The additional family grows as `O(n^4)`. At eight independent sites it adds 70
quadruple-opaque plans. At nine it adds 126 opaque selections and 126 dual open
selections. Formula and skolem deduplication still remove structurally
equivalent nested selections before downstream instantiation work, and the
existing query-wide cutoff and fuel bound proof enumeration. The planner does
not claim a complexity or completeness result beyond this deliberate quartic
frontier.

## Regression coverage

The focused Djinn tests exercise:

- the former eight-site four-open/four-opaque miss;
- the dual nine-site case with exactly four open sites;
- four nested selections whose shared enclosing forall must also open;
- a prepared global consumer available only through its cached quartic view;
- an independent ten-site five-open/five-opaque query that remains
  inconclusive; and
- the public Djinn facade, candidate renderer, and GHC under `RankNTypes` and
  `ImpredicativeTypes`.

The complete `djinn-tests` and `djex-tests` suites pass with the new frontier.

Focused Exference coverage fixes the regression's parser-neutral type to the
real Leant serialization, requires its direct candidate within 32 steps, and
separately exercises exact whole-arrow reuse and nested tuple-tree
construction. The combined implementation returns the checked live-shaped
candidate at step 30 under the unchanged 4,096-step/1,024-queue bounds.

The `c0c1a461` hardening adds two further checks. A balanced 32-leaf tree is
exhausted under explicit 256-step and 256-entry queue bounds with zero step or
queue pruning; it produces at most the eager and recursively shallow structural
histories and only one distinct expression. A separate checker-backed case
requires reuse of a scoped product available only at a deeply nested field.
After that follow-up, all 489 Exference unit tests, all 27 private engine tests,
and all 82 Djex facade tests pass. This extends the `f3dd2495` introduction; it
does not replace or reinterpret its stable live quartic measurements.

## Deliberate limits

The quartic family remains an approximation. It does not add general
higher-rank subsumption, polymorphic-let generalization, contextual
hypothesis-side elimination, instantiation chains beyond their separate
four-binder cap, or an arbitrary power set of occurrence choices. These limits
reduce completeness only; every returned term remains checked, and a miss at
an incomplete boundary remains operationally inconclusive.

Likewise, the Exference result does not make its heuristic search complete or
establish general rank-N or impredicative inference. It recognizes an exact
whole scoped arrow and a goal-directed concrete product tree; other equivalent
proofs may still fall outside a configured step or queue bound, and bounded
exhaustion remains inconclusive.
