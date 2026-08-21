# Timed parallel backend deadline

Date: 2026-08-20

## Outcome

The shared Djex REPL now overlaps eligible Djinn and Exference searches even
when a positive query timeout is active. Both checked requests are admitted
before one fixed monotonic cutoff is captured. The two workers then prepare
strict, selection-bounded presentation plans concurrently, and the REPL owner
replays the classified Djinn outcome before the Exference outcome.

This is the timed successor to the
[initial deterministic parallel backend search](2026-08-20-deterministic-parallel-backend-search.md).
It changes no command syntax, setting default, checked adapter, public session,
query or result type, or `ReplOptions` field. `jobs = 1` remains the exact
historical serial and lower-peak-memory route.

The production checkpoint is commit
`112389d1a50b01990e5a36ba78e94bd14d8154ca`, built on the private
absolute-deadline kernel at
`e44923564f5285ac9476eb3b24cdd59ea8884013`.

## Eligibility and fallback

The timed pair route requires all of the following:

- a shared-parser query rather than the legacy-parser fallback;
- an unconstrained query resolved to `BothBackends`, including `:compare`, bare
  input or `:synth` under `:backend both`, and repetition of such a query;
- `jobs >= 2`;
- a positive `timeout`; and
- `select = first` or `select = best`, not `select = all`.

The existing untimed pair route still owns the same shape with `timeout = 0`.
Single-backend queries, `jobs = 1`, `select = all`, legacy-parser fallback,
and behavioral `--where`/Length/Z3 execution remain serial. In particular,
this checkpoint does not add a second solver session or overlap behavioral
assessment.

The serial fallback is semantically meaningful, not merely an RTS control. It
does not construct paired search actions, preserves one fresh whole-command
timeout per backend, retains partial streaming output under `select = all`,
and has the lower peak-memory shape. Reducing capabilities with `+RTS -N1`
does not by itself select that path; `:set jobs 1` does.

## Admission and one cutoff

The timed REPL entrance has one explicit order:

1. parse and kind-check the shared source;
2. evaluate the checked Djinn request result to its `Either` constructor;
3. evaluate the checked Exference request result to its `Either` constructor;
4. capture one monotonic elapsed-time cutoff;
5. start both strict lane actions; and
6. observe and replay the Djinn outcome, then observe and replay Exference,
   whose already-forked arbiter can continue independently during Djinn replay.

An ordinary checked `Left` is a lane value, so it neither skips the other
request admission nor cancels the other search. Parsing and request admission
are outside the timeout, matching the established shared-parser serial route.
The legacy fallback remains separately serial and keeps its parser inside its
historical whole-command timer. Search, selection, rendering, and
diagnostic-plan construction are inside the shared cutoff. Parent-owned
stdout/stderr replay is outside it: a plan completed in time remains completed
even if the other lane or the terminal takes longer.

Each worker forces only the finite `CommandOutput` demanded by the current
selection policy. It does not force an unused Exference result tail. This
preserves `SelectFirst` latency and bounded `SelectBest` demand while keeping
all process-handle effects in the parent.

## Lane-local deadline race

Both workers share one deadline watcher, while one arbiter per lane races that
worker against the watcher. A fully forced plan is stamped under masking and
published to a private terminal cell before the worker `Async` can report
completion. This closes the scheduler interval in which a genuinely
pre-cutoff value or synchronous failure could otherwise be discarded.

A completion is successful only when its stamp is strictly less than the
cutoff. Equality belongs to timeout. On expiry, the arbiter sends a private
`ParallelLaneDeadlineExpired` exception to only its unfinished lane and waits
for that worker to stop. Only that exact private exception becomes
`ParallelLaneTimedOut`; unrelated asynchronous exceptions, watcher failures,
and cleanup-handler exceptions propagate normally. A terminal cell already
containing a value or synchronous failure wins over the cancellation race.

One lane can therefore complete while its sibling times out. Each timed-out
lane receives the same `DJEX_SEARCH_TIMEOUT` diagnostic text as the serial
timer. Fixed parent observation preserves Djinn-before-Exference transcript
order and left-first exceptional precedence regardless of completion order.

## Monotonic clock range

GHC exposes the production monotonic nanosecond sample as a `Word64`, whose
counter cycles after roughly 584 years. The REPL historically accepts every
positive whole-second timeout whose microsecond form fits `Int`, a larger
domain on a 64-bit host. Merely converting one bounded sample to `Integer`
would therefore create an unreachable cutoff for sufficiently large accepted
budgets.

The deadline constructor instead captures a starting sample and accumulates
successive modular `Word64` deltas into an unbounded `Integer` elapsed epoch.
Its watcher waits in chunks of at most 1,800,000,000 microseconds (thirty
minutes), recomputing only the time remaining to the original cutoff after
every wake. Early wakes, late wakes, and ordinary counter wrap cannot refresh
the budget, and the full validated timeout range stays representable.

As with every modular clock extension, a single observation gap spanning a
complete approximately 584-year counter cycle would be indistinguishable from
a shorter gap. The thirty-minute requested sampling cadence makes that an
operational rather than a release concern; `threadDelay` does not formally
promise a maximum wake latency.

## Deterministic characterization

The private scheduler suite has 27 deterministic cases. Its concurrency cases
use barriers rather than timing thresholds, and the timed cases pin:

- one common watcher and simultaneous lane start;
- pre-cutoff publication surviving deadline cancellation;
- published synchronous failures surviving the same race;
- exact-cutoff timeout classification;
- success/failure values beside a sibling timeout;
- stable replay when Exference finishes first;
- left-first exception and timeout observation;
- caller cancellation and watcher-failure cleanup;
- strict presentation forcing before parent consumption;
- preservation of a worker that replaces deadline cancellation with another
  asynchronous exception;
- early wake without cutoff refresh;
- a start-near-wrap, late-wake clock crossing;
- the maximum accepted budget and bounded observation chunk; and
- Djinn check, Exference check, cutoff capture, then both lane starts even
  when the first check returns an ordinary `Left`.

The CLI suite has 93 cases. Its paired regression compares complete
exit/stdout/stderr triples for `jobs = 1` and `jobs = 2` across untimed,
streaming, and timed queries. A separate source-ownership assertion matches
the contiguous live `REPL.hs` block from the timed `BothBackends` guard through
checked admissions, cutoff capture, lane preparation, and fixed replay. The
test therefore fails if timed dispatch is deleted or redirected to the serial
path without relying on wall-clock thresholds.

Final validation passed:

```console
cabal test djex-parallel-tests -j1 --test-show-details=direct --ghc-options=-Werror
cabal test djex-cli-tests -j1 --test-show-details=direct --ghc-options=-Werror
cabal test all -j1 --test-show-details=direct --ghc-options=-Werror
cabal build all --enable-tests --enable-benchmarks -j1 --ghc-options=-Werror
cabal check
cabal sdist
```

The complete run passed all seventeen suites, including 27/27 scheduler,
93/93 CLI, and 456/456 Length tests. The scheduler suite also passed 100
consecutive runs under each of `+RTS -N1` and `+RTS -N2`. The independently
built source archive had SHA-256
`ffcceb5d0fc2b158f57de26091c965f4d59a124a6a5337b7d922ffe44ffa17ef`.

Frozen source/test SHA-256 values were:

| Path | SHA-256 |
| --- | --- |
| `src/Language/Haskell/Djex/Command.hs` | `ed3c31b33f5d1b6484d52039c09271f197a4e286e16492ce4a10cbe5c48da53b` |
| `src/Language/Haskell/Djex/REPL.hs` | `179fc34d3e7660c1d74557887a01214176be8d2b8a6fe79e041372e45eae3f7a` |
| `src/Language/Haskell/Djex/REPL/Parallel.hs` | `09ed938126523e932c347089ac1564dff87fb65d6a526c55a0738e2ce3996d26` |
| `test-cli/Spec.hs` | `814380b6309e2d898aa5d550e9227d13689285a13e2225a50ffb303260266809` |
| `test-parallel/Spec.hs` | `5f10f643f2e92f377a73dd0a8f737ac4428a0f7dddaa6ba5b206683784ffd180` |

## Performance and remaining seams

This checkpoint adds no new performance claim. The existing O2
Peirce-tower benchmark still records a modest 1.090x median end-to-end gain for
the untimed two-capability pair and exact transcript equality. It does not
measure timeout expiry, and this report does not reinterpret it as evidence
for the timed route.

Still deferred:

- concurrent or bounded-materialized `select = all` output;
- parallel Djinn proof branches or Exference frontier traversal;
- parallel behavioral assessment or multiple isolated Z3 workers;
- a public checked-library scheduler or jobs field; and
- a larger timed and untimed workload corpus with memory admission evidence.

The current user-facing boundary is in
[Paired-backend concurrency](../repl.md#paired-backend-concurrency), the
private ownership model is in the
[architecture guide](../architecture.md#deterministic-backend-pair-scheduler),
and embedding responsibility remains in the
[library guide](../library-api.md#concurrency-boundary).
