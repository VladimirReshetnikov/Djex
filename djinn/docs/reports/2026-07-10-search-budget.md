# Djinn proof-search modes, budget, and benchmarks (R-07)

- Date: 2026-07-10
- Resolves: finding R-07 of
  [2026-07-10-code-review.md](2026-07-10-code-review.md)
  ("list nondeterminism is left-biased and unbudgeted")
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0, Windows 11 (numbers below are from
  one desktop machine and should be read comparatively, not absolutely)

## What R-07 asked for

The original finding: proof alternatives were explored by bare list
concatenation, so an expensive earlier branch starved later ones; the user
`cutoff` truncated the lazy result stream instead of bounding search work;
and the antecedent indexes are linear lists. The prescribed remedy was to
*introduce a named search mode and explicit fuel/result budget, then
benchmark fair interleaving and indexed structures against a representative
formula corpus before changing proof order*.

All three parts are now done. The headline decisions:

1. **A search budget exists and is honest.** `:set budget=N` bounds the
   number of explored choice points; an expired budget reports
   "inhabitation is undecided" instead of "cannot be realized". The default
   remains unlimited, so the decision-procedure guarantee is unchanged.
2. **Depth-first stays the default order.** Interleaving is implemented,
   measured, and available programmatically (`SearchMode`), but benchmarks
   did not justify changing the CLI's proof order (details below).
3. **The linear indexes stay.** A `Data.Map`-backed `AtomImps` was measured
   against the corpus (details below).

## The engine: a stream with explicit choice points

The proof monad `P` was `PS -> [(PS, a)]` — state plus a lazy result list.
It is now

```haskell
data Steps a = Done | Yield a (Steps a) | Step (Steps a)
newtype P a  = P { unP :: Strategy -> PS -> Steps (PS, a) }
```

a reader (strategy) + state monad producing a lazy stream in which `Step`
marks one explored choice point. `mplus` and `choose` emit a `Step` per
alternative; with the steps ignored the stream is exactly the old list, and
the default `DepthFirst` strategy combines branches by appending, so proof
order is bit-for-bit identical to the previous engine — every output-exact
regression test passes unchanged.

The two new capabilities fall out of the representation:

- **Budget.** `runBounded` consumes the stream and decrements a counter at
  each `Step`; hitting zero stops the search and reports exhaustion. The
  budget's unit is "choice points explored", which is stable across
  machines, unlike wall-clock timeouts.
- **Fairness.** `Interleave` swaps branches at every `Step`, so results
  from the second branch surface while the first is still searching.

The public API:

```haskell
data Strategy = DepthFirst | Interleave
data SearchMode = SearchMode {
    searchAlternatives :: Bool,      -- keep alternatives at local cuts
    searchStrategy     :: Strategy,
    searchBudget       :: Maybe Integer }
proveWithMode :: SearchMode -> [(Symbol, Formula)] -> Formula -> SearchOutcome
data SearchOutcome = SearchOutcome {
    searchProofs :: [Proof],
    searchExhausted :: Bool,
    remainingSearchBudget :: Maybe Integer }
```

`prove`/`provable` are thin wrappers over the default mode and behave as
before. In the CLI, `:set budget=N` (0 = unlimited, the default) feeds
`searchBudget`; when a budgeted query produces no proof *and* the budget
expired, the answer is

```text
-- f: no proof found within budget 2; inhabitation is undecided.
```

so "cannot be realized" continues to mean *proved uninhabited*.

`remainingSearchBudget` also keeps auxiliary proof searches honest.  After a
safe search has completely refuted a query, Djinn may reintroduce an excluded
same-named assumption to determine whether the sharper self-reference
diagnostic is justified.  That diagnostic pass receives only the first pass's
remainder and stops at its first proof.  If it exhausts the remainder, the
already-decided safe result stays `Unrealizable`; diagnostic work can neither
double the configured budget nor turn a decision back into `Undecided`.

## The corpus

[`bench/Corpus.hs`](../../bench/Corpus.hs) defines size-parameterized
families, each tagged provable/unprovable:

| Family | Shape | Stresses |
| --- | --- | --- |
| `projection/16` | 16-tuple to component | conjunction splitting |
| `implChain/12,48` | `(a1->a2)->...->a1->an` | `AtomImps` indexing |
| `pickOne/8` | `a->a->...->a` | multi-solution enumeration |
| `wideDisj/6` | n-ary disjunction to itself | case/injection fan-out |
| `contBind`, `callCC`, `stateBind` | monadic plumbing | realistic queries |
| `dnLEM`, `tripleNeg` | negation towers | empty-goal machinery |
| `starvation/3` | hard dead end before an easy disjunct | branch starvation |
| `peirce`, `peirceTower/3` | non-theorems | exhaustive refutation |
| `implChainBack/10` | unreachable chain goal | index-heavy refutation |

The harness ([`bench/Bench.hs`](../../bench/Bench.hs), `tasty-bench`)
measures three costs per entry: `first` (latency to the first proof or to
refutation — what an interactive query pays), `multi200` (enumerating up to
200 alternatives — the default `+sorted` collection cost), and `decide`
(the full `provable` answer).

## Measurements

Three engines were measured over the same corpus with identical settings
(`--stdev 5 --timeout 60`, one quiet desktop):

1. **list** — the original `PS -> [(PS, a)]` monad (commit `b3a50c1`);
2. **naive Steps** — a first cut that returned the `Steps` stream directly
   from `Strategy -> PS -> Steps (PS, a)`;
3. **CPS** — the shipped two-continuation (LogicT-style) encoding.

Selected results (mean of the reported distribution; `μs` unless noted):

| benchmark | list | naive Steps | CPS | CPS vs list |
| --- | ---: | ---: | ---: | ---: |
| first `callCC` | 26.7 | 7.1 | 5.6 | −79% |
| first `dnLEM` | 21.7 | 24.0 | 4.9 | −77% |
| first `implChain/48` | 50.0 | 275.2 | 50.0 | ±0% |
| first `starvation/3` | 15.3 | 13.8 | 3.5 | −77% |
| multi200 `callCC` | 30.7 ms | 40.1 ms | 8.6 ms | −72% |
| multi200 `contBind` | 1.40 ms | 1.55 ms | 0.27 ms | −81% |
| multi200 `implChain/48` | 369.3 | 96.3 | 76.3 | −79% |
| decide `stateBind` | 1.85 | 9.25 | 1.86 | ±0% |
| decide `tripleNeg` | 2.19 | 11.4 | 2.16 | −1% |
| decide `implChainBack/10` | 3.49 | 18.3 | 4.02 | +15% |
| decide `peirceTower/3` | 3.96 | 17.8 | 4.18 | +6% |

Full CSVs for all 39 corpus measurements per engine were compared; the
aggregate picture:

- **naive Steps** was rejected: refutation-heavy searches (`decide`) ran
  3.5–5.7× slower because failed branches leave `Step`-node chains that
  nested appends re-traverse once per enclosing bind. (Its `first`-group
  numbers also carry some noise from a concurrent compile; the `decide`
  regressions are from a quiet phase and are decisive on their own.)
- **CPS** holds `decide` — the pure `provable` decision path — at parity
  with the original list engine (−14%…+17%, within this machine's noise),
  and is a *large improvement* everywhere else: 70–80% faster on most
  `first` measurements and 70–84% faster on every `multi200` measurement.
  The two-continuation encoding avoids the list monad's per-result tuple
  and cons-cell traffic, which dominates when many alternatives flow
  through deep binds — precisely Djinn's default `+sorted` mode.

### Fair interleaving

First-result latency, `DepthFirst` vs `Interleave` on the CPS engine:
interleaving never won on any of the 14 corpus entries — parity on most
(`starvation/3`: 3.46 vs 3.41 μs) and up to +51% slower on the index-heavy
`implChain/48` (43.2 vs 65.1 μs). Even the starvation family, constructed
specifically to favor fairness, shows no benefit: the expensive dead end
sits behind an antecedent-classification choice rather than a goal-level
`mplus`, and LJT's termination guarantee already bounds how long any dead
end can run. Fairness pays reification costs at every choice point and
buys nothing measurable here.

### Budget overhead

Running with a budget that never expires (`Just 10^9`) versus unlimited:
at most +9% (`implChain/48`), within noise elsewhere. Counting choice
points is effectively free, so there is no fast path to protect.

### Indexed `AtomImps`

`AtomImps` was rewritten from a sorted association list to a strict
`Data.Map Symbol [Antecedent]` (the change is order-transparent: per-key
consequence order and all observable results are identical, and the full
test matrix passes). Corpus results are in the decision below.

## Decisions

1. **Ship the CPS engine.** Parity on decisions, several-fold faster on
   enumeration, and it is the representation that makes budgets and
   fairness expressible at all.
2. **Keep `DepthFirst` as the only CLI strategy.** `Interleave` remains
   available through `proveWithMode` for programmatic experiments, but the
   corpus shows costs without benefits, and changing the default order
   would invalidate every output-exact expectation users have.
3. **Expose the budget, default unlimited.** `:set budget=N` with the
   undecided-result diagnostic; the decision-procedure contract is intact
   by default and the counting overhead is noise.
4. **Adopt the `Data.Map`-backed `AtomImps`.** Measured against the CPS
   engine with the list index (selected `decide`/`first` means):

   | benchmark | assoc list | `Data.Map` |
   | --- | ---: | ---: |
   | first `implChain/48` | 50.0 μs | 33.6 μs |
   | decide `implChain/48` | 45.2 μs | 33.0 μs |
   | first `contBind` | 6.5 μs | 5.0 μs |
   | decide `contBind` | 5.0 μs | 8.1 μs |
   | decide `pickOne/8` | 1.4 μs | 2.3 μs |
   | multi200 (all 11 entries) | — | −10%…+6% |

   The picture: a solid 25–35% win as soon as the index holds dozens of
   atoms (each declared axiom and context method feeds this index, so it
   grows with real environments), parity on enumeration, and scattered
   ±1–3 μs small-case deltas whose signs *contradict each other* between
   the nearly identical `first` and `decide` measurements — i.e., noise at
   that scale on this machine. Since the Map code is also shorter than the
   hand-rolled sorted association list and removes an O(n²) growth class
   outright, it stays. (`NestImps` and `AtomFs` remain lists: they are
   consulted by `select`, whose order is semantically significant, and the
   corpus gave no evidence they matter.)

## Validation

- All three test suites pass; the unit suite gains budget/strategy
  regressions (finished-search-not-exhausted, expired-budget reporting,
  bounded-prefix behavior, strategy-independent provability over a finite
  space) and the CLI suite gains an expired-budget scenario.
- All output-exact rendering regressions pass unchanged, confirming the
  default proof order is preserved.
- `cabal check` reports no issues; builds are warning-clean under
  `-Wall -Wcompat`.
