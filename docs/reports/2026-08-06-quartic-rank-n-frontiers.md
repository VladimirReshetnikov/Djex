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

## Design

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

## Soundness boundary

This change adds coherent formula views; it does not add subsumption, invent a
polytype, or trust an unchecked proof term. Every realized clause is converted
from the formula plan which admitted it and passes Djinn's independent proof
checker before reaching the stable candidate API.

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

## Deliberate limits

The quartic family remains an approximation. It does not add general
higher-rank subsumption, polymorphic-let generalization, contextual
hypothesis-side elimination, instantiation chains beyond their separate
four-binder cap, or an arbitrary power set of occurrence choices. These limits
reduce completeness only; every returned term remains checked, and a miss at
an incomplete boundary remains operationally inconclusive.
