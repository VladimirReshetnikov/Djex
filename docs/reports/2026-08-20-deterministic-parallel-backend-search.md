# Deterministic parallel backend search

Date: 2026-08-20

## Outcome

The shared Djex REPL can now overlap its two independent backend searches for
the common unconstrained `both`/`:compare` case without changing terminal
order. The positive-integer `jobs` setting defaults to `2`; `:set jobs 1`
selects the exact preceding serial path, and `:unset jobs` restores `2`.

This is a deliberately coarse first checkpoint. It schedules at most one
Djinn lane and one Exference lane. It does not parallelize either engine's
internal search, change a checked adapter, or add a public jobs field to a
query, result, session, `ReplOptions`, or facade type.

## Eligibility boundary

The paired scheduler is used only when all of these conditions hold:

- the active query selection is `BothBackends`, whether through `:compare` or
  a bare query under `:backend both`;
- the shared parsed request/session route is available rather than the legacy
  parser fallback;
- the query is unconstrained rather than a behavioral `--where`/Z3 query;
- `jobs >= 2`;
- `timeout = 0`; and
- `select` is `first` or `best`, not `all`.

Everything else retains its existing path. `jobs = 1` is an exact serial
escape hatch. A nonzero timeout retains the established serial
whole-presentation timer; `select = all` retains one-pass serial streaming;
and behavioral assessment retains its checked single-session Length/Z3
lifecycle. The compatibility parser fallback also stays serial. Values above
two are valid but cannot add a third lane to a two-backend comparison.

## Strict plans and ordered replay

The frontend does not let worker threads write to process handles. Each worker
runs its checked request and fully evaluates the finite presentation demanded
by the selection policy into a private strict output plan. A plan records
ordered stdout lines, stderr lines, flush events, and the command exit status.
Forcing that plan evaluates the selected search prefix, rendering, and
diagnostic text in the worker; it does not demand an unused tail of
Exference's lazy result stream.

The parent observes and replays the Djinn plan first and the Exference plan
second, independently of completion order. The labelled transcript therefore
remains byte-for-byte deterministic. An ordinary checked failure is a plan,
not an asynchronous exception, so one backend's diagnostic does not cancel the
other backend.

The two workers live under nested structured-concurrency scopes. Worker
creation is masked, and an unexpected worker or consumer exception, or caller
interruption, cancels and waits for unfinished siblings before the scope
returns. When both workers raise, left-to-right observation preserves Djinn's
exception precedence.

## Runtime and resource ownership

The packaged `djex` executable is built with the threaded RTS and a default of
two capabilities (`-N2`). A caller may override that default, including
`+RTS -N1 -RTS`. The historical `djinn` and `exference` compatibility
executables remain serial.

Library behavior is otherwise unchanged. The checked adapter calls remain
synchronous, and an application embedding `runRepl` or `runArguments` owns its
own threaded-runtime build and capability settings; the library does not call
`setNumCapabilities` or impose an RTS default on its host.

The pair scheduler trades latency for resource overlap. Two live backend heaps
can make peak memory approach the sum of their individual footprints instead
of approximately the larger one. `jobs = 1` is therefore both the semantic
serial fallback and the documented lower-peak-memory setting.

## Deterministic validation

The scheduler tests use synchronization barriers rather than wall-clock
thresholds. They pin:

- both workers starting before either is released;
- Djinn-then-Exference consumption when Exference finishes first;
- ordinary failure-value isolation;
- cancellation and joining after a left exception;
- left-first observation when the right worker raises first;
- cleanup after caller cancellation; and
- complete plan forcing before parent consumption.

The CLI regression runs the same bounded multi-query session with `jobs = 1`
and `jobs = 2`, compares complete exit/stdout/stderr triples, repeats the
parallel case, and covers an eligible ordinary comparison plus the
`select = all` and timed serial fallbacks.

## End-to-end benchmark

`djex-parallel-bench` uses a fixed Peirce-tower-10 comparison with Exference
limited to 64 steps. It runs against a fresh empty source environment, so the
measurement includes executable and REPL startup as well as search,
selection, rendering, and shutdown. Before measuring, and again for every
alternating serial/parallel sample pair, it requires exact equality of the
complete exit/stdout/stderr transcript and `ExitSuccess`. The default is five
sample pairs. The release measurements built the benchmark and its local
dependency graph with optimization level 2:

```console
cabal bench djex-parallel-bench --enable-optimization=2
DJEX_PARALLEL_BENCH_CAPABILITIES=1 cabal bench djex-parallel-bench --enable-optimization=2
```

Each run performed one unmeasured equality precheck and five measured,
alternating serial/parallel pairs. The observed end-to-end timings were:

| RTS capabilities | `jobs = 1` median / p95 | `jobs = 2` median / p95 | Serial / parallel median |
| ---: | ---: | ---: | ---: |
| 2 | 4.186 s / 4.280 s | 3.839 s / 3.947 s | 1.090x |
| 1 | 4.424 s / 4.492 s | 4.546 s / 4.576 s | 0.973x |

One separate `/usr/bin/time` O2/N2 run provided resource context: `jobs = 1`
used 4.26 s wall, 4.42 s user, 0.23 s system, 109% CPU, and 315,356 KiB
maximum RSS; `jobs = 2` used 4.04 s wall, 4.63 s user, 0.32 s system, 122%
CPU, and 322,000 KiB maximum RSS. The higher CPU/wall ratio corroborates
overlap for that run. Its roughly 2% RSS increase is a single observation, not
a memory ceiling; other balanced searches can retain both heaps and approach
their summed footprint.

The one-capability run is a control rather than evidence for parallel search:
repeated observations varied around parity and showed no stable benefit from
overlapping the jobs on one capability. The two-capability result is a modest
workload-specific reduction rather than the larger ideal implied by adding the
individual backend times. Process startup dilutes search-only overlap,
workload balance varies, and these five samples do not support a broad speedup
claim or predict another query, runtime, or machine.

## Deferred parallel seams

This checkpoint intentionally leaves these as later work:

- a shared absolute deadline for concurrent timed comparisons;
- deterministic concurrent streaming or bounded materialization for
  `select = all`;
- concurrency inside the Exference priority frontier or Djinn proof search;
- parallel behavioral assessment or multiple isolated Z3 workers;
- a public scheduler for checked-library callers; and
- a longer corpus that separates process startup, frontend preparation, and
  engine-only throughput.

Those seams require their own ownership, determinism, cancellation, memory,
and benchmark evidence. They are not implied by the two-lane REPL scheduler.

## Documentation boundary

The current interactive syntax and fallbacks are in the
[shared REPL guide](../repl.md#paired-backend-concurrency). Scheduler and
effect ownership are in the
[architecture guide](../architecture.md#deterministic-backend-pair-scheduler),
while the unchanged embedding boundary is in the
[library guide](../library-api.md#concurrency-boundary). This dated report
records the first checkpoint and its measurements; the current guides remain
authoritative if a later scheduler supersedes it.
