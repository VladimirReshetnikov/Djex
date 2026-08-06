# Bounded quintic rank-N frontiers

Date: 2026-08-06

## Outcome

Djinn now explores the five-site choices left outside its quartic positive
`forall` approximation. After the complete historical prefix through
quadruples, search appends quintuple-opaque plans from ten sites and their dual
quintuple-open plans from eleven. The former ten-site five-open/five-opaque gap
now synthesizes directly, and the combined structural family is exhaustive for
every open/opaque choice through eleven independent sites.

This is not a general power-set search. Each quintic orientation retains at
most 512 stable, edge-balanced selections. All `C(10, 5) = 252` ten-site
selections and all `C(11, 5) = 462` eleven-site selections fit beneath that
cap, while larger inputs can add no more than 1,024 quintic formula views in
total.

## Design

The primary polarized traversal records every positive quantified site in
stable source order. The new plans reuse the existing coherent lowering:

- A quintuple-opaque plan starts from the fully opened view and retains five
  selected sites as exact alpha-aware opaque atoms. These plans begin at ten
  sites, supplying the complete balanced 5/5 layer there.
- A quintuple-open plan starts from the exact opaque view and opens five
  selected targets. It begins at eleven sites; at ten sites its complements
  would duplicate the quintuple-opaque layer.
- Opening a nested target also opens every enclosing `forall` required to reach
  it. Unrelated sites remain opaque, using the same shared ancestry rule as the
  singleton through quadruple open frontiers.

Unordered quintuples are enumerated once from the source-order edge and once
from the reverse-source edge. The planner alternates the two streams,
deduplicates selections by their five-site set, and retains the first 512. The
order is deterministic, includes both edges before a large middle can dominate
the cap, and preserves every combination at ten and eleven sites. Beyond
eleven sites this is intentionally an incomplete bounded sample of the two
fifth-order orientations.

The complete scheduling order is unchanged before the new suffix:

```text
fully opened
exact opaque
single opaque, single open
pair opaque, pair open
triple opaque, triple open
quadruple opaque, quadruple open
quintuple opaque, quintuple open
bounded instantiation families
```

Both structural and nominal goal families receive the new suffix. Prepared
global functions cache the same quintic views under distinct proof identities,
so one reusable consumer can demand the five exact transports/five structural
introductions mixture without query-time retranslating. The deduplicated skolem
inventory also includes both categories before bounded instantiation candidates
are prepared.

## Bounds and completeness

For ten independent sites, the quartic family supplied 772 plans. The 252
quintuple-opaque selections complete all `2^10 = 1,024` open/opaque choices.
For eleven sites, the earlier layers supply 1,124 plans and the two quintic
orientations add `462 + 462`, completing all `2^11 = 2,048` choices.

From twelve sites onward, each new orientation stops after 512 distinct
selections even though `C(n, 5)` continues to grow. The new layer therefore
adds at most 1,024 views; the uncapped historical part remains quartic. Formula
deduplication can remove additional structurally equivalent selections caused
by nested ancestry.

Twelve independent sites expose the next central logical boundary. A proof
requiring exactly six sites open and six opaque is outside every quintic plan,
irrespective of the 512-selection cap. Exhausting that incomplete family
returns `NoEvidence`, never a false proof of uninhabitability.

## Soundness boundary

The change adds formula views, not a new typing or subsumption rule. Every
selection is lowered through the existing occurrence-identified polarized
compiler. Exact sites remain the same opaque atoms; opened sites receive the
same occurrence-scoped skolems and ancestor treatment as earlier frontiers.

Alternative formula plans cannot contribute negative evidence. Every produced
proof is checked against the exact formula that admitted it before conversion,
and the public-facade regression additionally compiles the rendered non-prefix
witness with GHC under `RankNTypes` and `ImpredicativeTypes`. Thus the cap and
edge balancing affect completeness and scheduling only, not candidate
soundness.

## Regression coverage

Focused Djinn coverage exercises:

- a non-prefix ten-site 5/5 goal whose fifth transported scheme is separated
  from the other four;
- the independently necessary eleven-site quintuple-open orientation;
- a twelve-site 6/6 query that remains candidate-free with `NoEvidence`;
- a prepared global consumer available only through its cached quintic view;
  and
- the shared Djinn facade, candidate renderer, and GHC source checker under
  `RankNTypes` and `ImpredicativeTypes`.

On the final documented tree, the complete 83-test Djinn unit suite and the
complete 83-test Djex facade suite pass. Those sweeps include the focused
rank-N regressions, the prepared-function witness under first-candidate query
options, and the public facade/GHC check.

## Deliberate limits

The bounded quintic family does not add general higher-rank subsumption,
polymorphic-let generalization, contextual hypothesis-side elimination, or an
arbitrary power set of occurrence choices. It does not lift the separate
four-binder hypothesis-instantiation cap. At more than eleven sites, some
five-site choices can fall beyond either 512-entry orientation, and six-site
central choices remain outside the structural family. These are completeness
limits only: returned candidates remain checked, while a bounded miss remains
operationally inconclusive.
