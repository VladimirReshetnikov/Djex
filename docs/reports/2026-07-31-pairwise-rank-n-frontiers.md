# Pairwise rank-N frontiers

Date: 2026-07-31

## Outcome

Djinn now explores the balanced rank-N choices that its historical singleton
frontiers could not represent. After the existing fully-open, exact-opaque,
single-opaque, and single-open plans, search appends deterministic
pair-opaque and pair-open frontiers. This makes the bounded family exhaustive
for every open/opaque choice across five independent positive `forall` sites
without introducing general rank-N subsumption or an exponential power-set
search.

The same plans are compiled for checked goals and prepared global premises.
A reusable loaded function can therefore be consumed at a pairwise view while
other occurrences of that function retain different sound views.

## Design

For the positive sites discovered by the polarized traversal, Djinn enumerates
unordered pairs in stable source order.

- A pair-opaque plan starts from the fully-open view and retains the selected
  two sites as exact opaque atoms. It starts at four sites, where it supplies
  the formerly missing two-open/two-opaque layer.
- A pair-open plan starts from the exact-opaque view and opens the selected two
  sites. It starts at five sites; at four sites its complements would duplicate
  the pair-opaque layer.
- Selecting nested sites opens the union of the ancestor chains needed to reach
  both targets. Unrelated sites remain opaque.

The historical plan prefix and its result order are unchanged. The new tail
contains at most two plans per unordered pair, so plan growth is quadratic.
Duplicate formulas caused by structural relationships are still removed by the
existing cross-plan distinction pass.

For independent sites, the two extremes, two singleton layers, and two
pairwise layers cover every subset through five sites. Six sites expose the
next deliberate boundary: a proof needing exactly three sites open and three
opaque is still outside the base family. An empty result at that incomplete
boundary remains inconclusive rather than becoming negative evidence.

Bounded context-free hypothesis instantiation remains a separate, later plan
family. It can cover some omitted subsets when the required schemes and
candidate types meet its limits, but it does not change the pairwise frontier's
completeness claim.

## Regression coverage

The focused Djinn tests exercise:

- a four-site inhabitant requiring exactly two opaque transports and two
  structurally opened identities;
- the dual five-site case requiring exactly two open sites;
- two nested selections whose distinct ancestor chains must both open;
- a prepared global consumer available only through its pairwise cached view;
- a six-site three-open/three-opaque query that must remain inconclusive.

Verification uses the checked library boundary and stable proof checker rather
than accepting rendered text alone. The implementation is also covered by the
Djinn property, frontend API, shared API, integration, synthesis, and CLI test
suites.

## Deliberate limits

This change does not add general higher-rank subsumption, polymorphic-let
generalization, or a powerset of occurrence choices. Constrained
hypothesis-side schemes, instantiation chains beyond the existing binder cap,
and six-site central subsets remain bounded gaps. These limits reduce
completeness only; proof terms continue to be checked against the exact formula
for the plan that produced them.
