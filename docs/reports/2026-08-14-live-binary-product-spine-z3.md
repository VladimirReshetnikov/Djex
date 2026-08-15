# Live binary-product spine Length/Z3

Date: 2026-08-14

## Outcome and boundary

Djex can now execute a sealed `LengthSpinePairSMTLibQuery` through the scoped
live Z3 owner. The public path is:

```haskell
withLengthSMTLibLiveSession
runLengthSpinePairSMTLibLiveQuery
replayLengthSpinePairSMTLibLiveQueryObservation
```

This extends the existing pure product query and replay checkpoint without
collapsing it into the scalar Length domain. Product response decoding,
protocol planning, live query-run identity, public observations, sanitized
failures, and retained evidence are nominal product siblings. The scalar and
product paths share one already probed process only where their transport
requirements are exactly common.

Solver output remains heuristic. A live `sat`, `unsat`, or `unknown` status is
not proof, a pruning certificate, or behavioral evidence. A satisfiable values
run may carry optional evidence only after Djex has independently replayed the
input assignment against the exact checked product problem and recomputed both
result lengths. The public observation still requires an exact-query replay
gate before that receipt can be revealed.

This checkpoint exposes enough Djex surface for an engine integration to rank
product-result candidates, but it does not add that ranking to Leant. Leant
product observation consumption and ranking remain the next checkpoint.

## Common readiness is transport readiness only

`withLengthSMTLibLiveSession` still opens one rank-N-scoped worker and performs
the existing four-write readiness probe before lending it. That transcript
checks the exact common profile used here:

- startup print suppression and positional echo framing;
- reset plus a canonical QF_LIA `sat` check;
- exact input-only `get-value` decoding;
- reset plus a canonical contradictory QF_LIA `unsat` check; and
- bounded FIFO framing, boundary whitespace, stdout, stderr, deadline, and
  cleanup behavior.

The product run embeds the ready-worker identity under an explicit
common-QF_LIA readiness field. That field means only that the observed worker
can carry the required reset/check/input-value transaction shape. It does not
import the scalar checked problem, scalar query identity, scalar observation,
or `FiniteListSpineLengthV1` evidence into the product domain. Readiness itself
still creates no solver observation and no behavioral receipt.

## One exact mixed-domain budget

The live facade's fixed default remains:

```haskell
defaultLengthSMTLibLiveSessionMaximumQueries == 64
```

That is one total budget for the scope. Scalar and binary-product calls reserve
from the same zero-based ordinal counter and the same bounded spent-marker set.
An arbitrary interleaving therefore admits at most 64 transaction ordinals in
total, not 64 scalar plus another 64 product ordinals. Maximum-plus-one is
rejected before any query bytes are written.

The count is an admission ceiling, not a promise that 64 maximum-sized queries
will run. Lease-wide stdout capacity, response and protocol limits, run-identity
admission, process state, and per-query deadlines remain independently bounded.
A failure after reservation burns its ordinal and spends the worker under the
existing fail-closed lifecycle policy.

## Product response and protocol identity

`parseLengthSpinePairSMTLibInputValueResponse` reuses the existing bounded
SMT-LIB lexer, S-expression parser, integer policy, response limits, and
authority-free `LengthSMTLibIntegerBinding` carrier. It derives the expected
symbol set and order from the exact `LengthSpinePairSMTLibQuery`; it cannot use
a scalar query as decoding authority. Parsing still proves syntax only.

The package-private product transaction is identified separately by:

```text
djex-length-spine-pair-z3-smtlib2-protocol-plan/v1
djex-length-spine-pair-z3-smtlib2-protocol-phase-machine/v1
finite-binary-product-spine-lengths/z3-smtlib2-protocol-plan
```

It owns product-specific plan, barrier, required-frame, phase, write-kind,
decoded-outcome, and failure types. As with the scalar protocol, it first writes
the reset plus exact product check program, awaits status and a positional
barrier, and writes `get-value` only when the configured artifact policy and
query arity require it. A valuation already buffered before that write cannot
answer the later command. The shared causal driver supplies the algorithm and
segmented transcript mechanics, not product semantic authority.

The product plan fingerprint binds the exact product query fingerprint and
schema, common response policy, artifact policy, stream policy, barriers,
canonical writes, phase policy, and QF_LIA logic. A scalar and product plan
remain different even when their SMT-LIB command bytes happen to coincide.

## Nominal product run and mandatory replay

The package-private run uses the schema and fingerprint role:

```text
djex-length-spine-pair-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/v1
finite-binary-product-spine-lengths/z3-live-query-run
```

Its reversible identity binds the common ready-worker observation, exact
product protocol plan, shared ordinal and spent barriers, absolute query
deadline, causal transcript, decoded product branch, explicit evaluation
limits, independent product replay outcome, and committed stdout/stderr
accounting. The retained successful run is narrower: it keeps the ordinal,
strict status-indexed product observation, identity, transcript digest, and
accounting boundaries, but no detached parsed valuation.

For a values-policy satisfiable result, the live owner performs all of the
following before returning success:

1. decode exactly the product query's compact input symbol set;
2. reject missing, duplicate, unknown, or negative bindings;
3. restore compact inputs in source order;
4. call `validateLengthSpinePairSMTLibCounterexample` under the supplied
   `LengthEvaluationLimits`;
5. independently interpret the retained product candidate and recompute both
   ordered result lengths; and
6. retain optional `FiniteBinaryProductSpineLengthsV1` evidence only if the
   checked relational bad state is actually true.

A syntactic `sat` model that is not a product counterexample is rejected for
the values policy rather than being promoted as evidence. Status-only `sat`
retains no receipt. `unsat` and `unknown` also retain no receipt. None of those
statuses has more than heuristic ranking use.

## Public product observation

`runLengthSpinePairSMTLibLiveQuery` returns the opaque nominal
`LengthSpinePairSMTLibLiveQueryObservation`. Its constructor, exact query
fingerprint, whole status-indexed observation, and optional evidence are not
projectable. Public callers can inspect only:

```haskell
lengthSpinePairSMTLibLiveQueryObservationSolverStatus
lengthSpinePairSMTLibLiveQueryObservationResultStrength
lengthSpinePairSMTLibLiveQueryObservationUse
```

The strength is derived from status, and use is always
`HeuristicRankingOnly`. The product observation and its closed byte-free
failure/replay vocabularies are distinct from their scalar counterparts.

`replayLengthSpinePairSMTLibLiveQueryObservation` is the mandatory semantic
extraction edge. It first compares the exact collision-free product query
fingerprint. Only after that match can it inspect optional evidence and replay
the evidence against the `BehavioralProblem` retained by that same product
query. Its outcomes mean:

- `Right (Just receipt)`: the exact live query is associated and its already
  independently validated product counterexample receipt was replayed again;
- `Right Nothing`: the status is exactly associated but carries no evidence;
  or
- `Left`: query identity or product problem/evidence association failed.

The gate exposes no transcript, model bindings, process observation, run
ordinal, or solver claim. Direct natural-input replay remains a separate fresh
evaluation path and extracts nothing from a live observation.

## Scalar compatibility

The feature is additive. Historical scalar Length query construction,
canonical check and value-request bytes, response behavior, protocol tags and
phase bytes, query-run schema and fingerprint fields, public observation and
failure types, replay gate, and evidence association are unchanged. The
existing execution-policy, raw-process, capability, session, ready-worker,
workspace, causal-driver, barrier, and scalar live tags remain unchanged as
well.

Sharing the response lexer, QF_LIA transport profile, process, causal driver,
ordinal counter, and marker set is intentional implementation and resource
reuse. It does not make scalar and product queries, protocols, runs,
observations, or evidence representationally interchangeable.

## Regression boundary

The Length suite pins mixed scalar/product ordinal exhaustion at exactly 64,
product status-only and values branches, nullary and framed input-value paths,
split/drip framing, stale pre-write rejection, product deadline failure,
independent two-result replay, nominal plan/run identity, public query-first
replay precedence, sanitized failure mapping, shared capacity admission, and
scalar compatibility. Public API tests pin the additive facade exports without
exposing private product protocol or run constructors.
