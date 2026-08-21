# Exference serial ordered StepActions

Date: 2026-08-20

## Outcome

Exference production search remains serial, but the expansion of one popped
search node now has an explicit finite sibling boundary. The planner exposes an
ordered list of private `StepAction`s covering applicable scoped providers,
structural rules, and global bindings. Production interprets that list with a
serial `concatMap`, running every action from the identical popped-node
snapshot.

Alternatives inside one provider or global binding remain atomic within their
action. The historical monolithic dispatcher remains only as an internal
differential oracle. No runtime worker, jobs control, or public API was added.

The code checkpoint is commit
`28dde8f942cdd9dfac1e4e21bd0c8b1506eaaba5`.

## Exact serial boundary

The private branching carrier is unchanged:

```haskell
newtype SearchBranches a = SearchBranches (ExceptT BranchTruncation [] a)
type StepAction = StateT SearchNode SearchBranches ()
```

For one already-popped node, the applicable action order is:

| Goal shape | Ordered actions |
| --- | --- |
| Arrow | each whole scoped-provider lane, then lambda introduction |
| Leading or continuing `forall` | the corresponding structural introduction lane |
| Optional `forall` introduction | each scoped-provider lane, each global-binding lane, then introduction |
| Nonempty boxed tuple | each scoped-provider lane, the applicable tuple-tree and/or shallow-tuple lanes, then each global-binding lane |
| Other goal | each scoped-provider lane, then each global-binding lane |

Each action is run as `runSearchBranches (execStateT action initialNode)`, and
the resulting lists are concatenated in plan order. Thus siblings do not
inherit another sibling's unification, allocation, substitution, constraint,
or expression state. Inner alternatives deliberately stay inside the captured
provider or global-binding action, retaining their common allocation snapshot,
truncation cardinality, and contiguous result order.

Only after the complete ordered serial step has been interpreted does the
existing owner collect branch truncations, apply depth limits, classify
solutions and futures, rate retained futures, and commit the resulting work to
the priority queue. The extraction therefore does not introduce a second queue
owner or change the established lazy search trace.

## Differential oracle and parity

The former monolithic `<|>` dispatcher is independently spelled and reachable
only through `Language.Haskell.Exference.Core.Internal.Testing`. Production
always selects the ordered-action route. The test seam can observe both full
compatibility traces and the private typed candidate/graph associations without
exposing either route through the library API.

Differential cases cover:

- identity and equal-cost immediate solutions;
- equal-priority futures with an unbounded queue and a queue of one;
- a zero-capacity queue and depth pruning;
- multi-constructor pattern and nested-tuple structural branches;
- a retained residual constraint;
- exhaustion of each of the term, flexible-type, rigid-type, and scope
  identifier allocators;
- stable, distinct candidate-to-graph identities; and
- first-batch observability beside a poisoned next step, followed by failure
  only when the second batch is demanded.

Validation at the published checkpoint passed 493 Exference unit cases and all
49 private engine cases. It also passed all 17 strict package suites, the
all-target test/benchmark build, `cabal check`, and an independently built
source distribution. These gates used `-Werror`; the whole-tree suite ran with
`-j1` to preserve the repository's subprocess-executable ownership rule.

## Discarded performance experiments

This checkpoint makes no speed-up claim. Two experimental paths were measured
and then removed before the serial boundary was committed:

- forcing work only after a result had already been produced was slower; and
- early lane forcing was effectively flat on the retained wide diagnostic.
  With two capabilities, its serial wall result was
  `2.656 +/- 0.252 s` versus `2.546 +/- 0.123 s` for sibling forcing, only
  about 1.04x. With one capability, sibling forcing regressed from
  `2.510 +/- 0.012 s` to `2.695 +/- 0.218 s`, about 7.4%, while the two-capability
  run allocated about 2.8% more.

An additional ad hoc deep-unification calibration also failed the predeclared
1.25x gate, but its removed fixture and raw transcript were not retained, so
this report does not use it as release evidence. The retained result did not
justify a worker executor, and no executor was shipped.

Multicore measurements of the tasty-bench corpus must use
`-j1 --time-mode wall`. The first option prevents the harness from adding
cross-benchmark concurrency, and wall time measures the latency that internal
parallelism is intended to reduce. CPU-time mode can make concurrent work look
slower even when wall latency falls.

## Admission gates for a future executor

An internal parallel route remains research work. It must preserve exact full
trace and next-step laziness parity with the serial interpreter and, against
the same optimized serial baseline, demonstrate all of:

- at least 1.25x speed-up on two substantive workloads;
- a positive geometric-mean speed-up across the benchmark suite;
- no more than 5% regression with `+RTS -N1`;
- no more than 10% additional allocation; and
- no more than 25% additional maximum resident set size.

The current production boundary is described in the
[architecture guide](../architecture.md#exference-serial-step-action-boundary)
and the [Exference technical README](../../exference/README.md). The separate
Djinn/Exference REPL overlap remains the coarse-grained scheduler documented in
the [timed paired-backend report](2026-08-20-timed-parallel-backend-deadline.md);
it does not make this internal frontier concurrent.
