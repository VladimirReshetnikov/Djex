# Repeated-domain evidence fairness in LJT — 2026-08-05

## Scope

The preceding
[oldest-first evidence change](2026-08-05-oldest-first-evidence.md) made a
caller's arguments visible before newly derived atom proofs.  It did not,
however, make sibling choices visible inside the subtree of a curried premise.
For the raw formula

```text
A -> A -> (A -> A -> A) -> (A -> A) -> (A -> A) -> A
```

depth-first multi-proof enumeration exhausted the first argument's descendant
subtree before trying the second argument at the adjacent `A` domain.  The
direct mixed application of the binary premise was proof 112, outside Leant's
60-candidate engine batch.  Its concrete consumer symptom was
`List.append x x` appearing for `List a -> List a -> List a` while
`List.append x y` did not.

This is an enumeration problem, not a missing proof rule.  Downstream scoring
cannot promote a term that the bounded producer batch never contains.

## The bounded scheduler

`Djinn.Internal.LJT` now recognizes one exact suffix while retaining multiple
solutions:

```text
A -> A -> A
```

where every `A` is the same atomic proposition.  At the first argument choice
it takes at most the three oldest proofs of `A`, reifies their computations
from the same search state, and rotates the streams round-robin.  At the next
same-domain choice it orders unused members of that cohort before members
already used in the chain.  Any fourth or later proof remains on the
historical depth-first tail.

The implementation is deliberately local:

- it is active only when alternative proofs are retained;
- the trigger is the exact atomic binary endomorphism suffix, not every pair
  of equal adjacent domains;
- the fair cohort contains only the oldest three proofs;
- unused-before-reused ordering applies only inside that cohort; and
- the rest of the evidence and every unrelated branch keep their established
  traversal.

The true n-way queue matters.  A right-nested sequence of pairwise fair merges
would still bias the final sibling.  `interleaveChoices` instead advances one
node from every live reified stream per round, using a front/rear queue so
rotation is amortized constant time.

## Choice accounting and proof-space invariants

`Step` nodes are observable search choice points: finite budgets consume them,
and the global `Interleave` strategy alternates at their boundaries.  The local
scheduler preserves the historical total.  For `n` available proofs, a cohort
of `k` proofs, and a tail of `r = n - k` proofs:

- reifying the cohort contributes `k - 1` initial sibling steps;
- when `r > 0`, entering the tail contributes one boundary step; and
- choosing among that tail contributes `r - 1` steps.

With no tail, the cohort already contributes `n - 1`; with a tail, the sum is
`(k - 1) + 1 + (r - 1) = n - 1`.  Both exactly match the accounting of
`choose available`.
Reification also starts every sibling from the same immutable proof-search
state, so fresh-name allocation remains branch-local.  No proof constructor,
normalizer, or independent proof-checking rule changed.

Consequently the proof set and the single-result decision procedure are
unchanged.  Multi-proof order changes locally, and—as before—an unbounded
request to enumerate every proof need not itself be a finite operation even
though inhabitation remains decidable.

## Measurements

These are observations from the development machine, not stable performance
guarantees:

| Observation | Result |
| --- | ---: |
| mixed `combine x y` in the two-distractor raw fixture, before | proof 112 |
| mixed `combine x y`, final bounded scheduler | proof 12 |
| repeated `combine x x`, final bounded scheduler | proof 15 |
| all direct pairs from the oldest three proofs | within 60 proofs |
| bounded three-proof fixture | all nine pairs by budget 120; truncated with zero remaining fuel |
| four-site Leant rank-N stress case before local fairness | 0.42 s |
| unrestricted repeated-domain fairness experiment | stopped after 180 s |
| four-site case with the final oldest-three guard | 0.52 s |
| established six-site rank-N stress case, final | 1.41 s |

The unrestricted experiment exposed a Cartesian frontier across independent
rank-N plans.  Restricting promotion to an exact binary endomorphism and three
oldest proofs retained the useful mixed arguments without imposing that fanout
on wider shapes.

## Boundaries pinned by tests

The Djinn unit suite now covers both sides of the rule:

- all nine direct applications of the oldest three `A` proofs occur in the
  early `A -> A -> A` window, while a fourth proof stays outside the cohort;
- `A -> A -> B` with four `A` proofs retains its historical duplicated
  row-major depth-first sequence; and
- global `Interleave` reaches an application using the fourth proof within a
  finite 60-proof prefix, demonstrating that the preserved tail is live.

Leant pins `fun x y => Demo.combine x y` inside its twelve-group verification
frontier.  A real Lean transcript for `List a -> List a -> List a` now verifies
and displays both:

```text
fun x y => List.append x y
fun x y => List.append y x
```

The final integration was checked with:

```console
cabal test all -j1 --test-show-details=direct
LEANT_BACKEND=/path/to/lean/repl bash test/run-tests.sh library
```

The first command was run at the Djex tip and again through Leant's pinned
submodule graph; the second exercises Lean elaboration rather than only the
pure engine boundary.

## Deliberate limits

This does not implement general breadth-first proof enumeration or general
rank-N subsumption.  Non-endomorphic results, longer repeated-domain chains,
and fourth-or-later evidence receive no local promotion.  Those limits are
part of the performance contract: future widening should begin with a measured
consumer failure and preserve honest `Step` accounting, the historical tail,
and the rank-N stress fixtures.
