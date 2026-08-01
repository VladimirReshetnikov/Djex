# Triple rank-N frontiers

Date: 2026-08-01

## Outcome

Djinn now explores the balanced three-site choices left outside its pairwise
rank-N approximation. After the historical fully-open, exact-opaque,
singleton, and pairwise plans, search appends deterministic triple-opaque and
triple-open frontiers. The bounded family is therefore exhaustive for every
open/opaque choice across seven independent positive `forall` sites without
becoming an exponential power-set search.

The first former gap, three exact transports beside three structurally opened
identities, now synthesizes directly. Prepared global functions retain the
same triple views, so a loaded consumer can require that mixture without being
retranslated per query.

## Design

For the positive sites discovered by the polarized traversal, Djinn enumerates
unordered triples in stable source order.

- A triple-opaque plan starts from the fully-open view and retains the selected
  three sites as exact opaque atoms. It starts at six sites, where it supplies
  the formerly missing three-open/three-opaque layer.
- A triple-open plan starts from the exact-opaque view and opens the selected
  three sites. It starts at seven sites; at six sites its complements would
  duplicate the triple-opaque layer.
- Selecting nested sites opens the union of every ancestor chain needed to
  reach the three targets. Unrelated sites remain opaque.

The historical and pairwise prefix keeps its result order. Triple plans run
before the separately bounded hypothesis-instantiation plans, under the same
global search cutoff and fuel. Formula and skolem inventories are deduplicated
before instantiation analysis so structurally equivalent selections do not
multiply later work.

This change also replaces the earlier cons-pattern thresholds with explicit
site-count guards. Patterns such as `_ : _ : _ : _` match three or more list
elements, not four; downstream formula deduplication had hidden the resulting
early pair-plan duplicates. The exact thresholds are now singleton-open at
three sites, pair-opaque at four, pair-open at five, triple-opaque at six, and
triple-open at seven.

For independent sites, the two extremes and the singleton, pair, and triple
layers cover all subsets through seven sites. Eight sites expose the next
deliberate central boundary: a flat proof needing exactly four sites open and
four opaque remains outside the cubic family. An empty result at that
incomplete boundary remains `NoEvidence`, never a proof of uninhabitability.

## Regression coverage

The focused Djinn tests exercise:

- the former six-site three-open/three-opaque miss;
- the dual seven-site case with exactly three open sites;
- three nested selections whose shared ancestor chain must also open;
- a prepared global consumer available only through its cached triple view;
- an independent eight-site four-open/four-opaque query that remains
  inconclusive.

All realized clauses cross the checked request boundary and Djinn's independent
proof checker. The complete Djinn unit suite also verifies that historical
queries, result order, prepared environments, and proof identities remain
stable.

## Deliberate limits

The cubic family is still an approximation. It does not add general
higher-rank subsumption, polymorphic-let generalization, or an arbitrary
powerset of occurrence choices. Constrained hypothesis-side schemes,
instantiation chains beyond their separate binder cap, and independent
eight-site central subsets remain bounded gaps. These limits reduce
completeness only; every returned term is checked against the exact coherent
formula plan that produced it.
