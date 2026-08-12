# Sanitized scoped Length/Z3 facade

Date: 2026-08-11

## Scope

This checkpoint turns the package-private ordinal-bound worker into a narrow
public IO boundary. It does not expose the live Session, Process, Capability,
Protocol, or causal Driver modules. Instead,
`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live` owns their validated
defaults and projects only the information a typed behavioral-ranking layer can
use honestly.

The public scope accepts an already sealed `LengthSMTLibExecutionConfig`. That
configuration retains the caller's absolute executable path, optional SHA-256
pin, solver timeout/resource limit, host operation deadline, response limits,
and artifact policy. Constructing it still performs no IO. Opening the live
scope observes and capability-probes the configured worker under the private
ownership policy described by the worker-lease and query-run reports.

## Scoped authority

`withLengthSMTLibLiveSession` lends an opaque session through a rank-N callback.
Its epoch parameter is nominal, and the constructor is hidden. A caller can
submit a nominally associated `LengthSMTLibQuery` under explicit independent
`LengthEvaluationLimits`, but cannot inspect or control the underlying process.
Runtime lease state also rejects an existential wrapper used after callback
return.

The facade fixes the current private session maximum at 64 serial queries and
exports that number so an integration can prove its own batch fits. The limit
does not promise transport capacity for 64 maximum-sized answers: stdout is
also bounded cumulatively for the process lifetime, and every query performs
its own pre-write capacity admission.

The scope deliberately projects none of the following:

- process handles, cancellation tokens, pipe state, or worker receivers;
- executable or workspace paths, file digests, snapshot identities, or launch
  arguments;
- the secret barrier seed, readiness or query barriers, or query ordinal;
- decoded SMT symbols or integer bindings;
- capability/query transcripts, stream counters, or transcript digests; and
- private reversible worker/run fingerprints.

Those values remain useful for package-private association and auditing, but
their canonical fingerprints contain spent barriers and exact child traffic.
Publishing them would turn an intentionally small observation API into a
diagnostic and capability-disclosure surface.

## Observation and evidence authority

A successful public query observation freshly copies only:

- the exact public query fingerprint;
- `SolverSatisfiable`, `SolverUnsatisfiable`, or `SolverUnknown`;
- the corresponding raw strength (`RawSolverModelHint`,
  `RawSolverUnsatRelativeToEncoding`, or `RawSolverUnknown`);
- the constant use `HeuristicRankingOnly`; and
- optional `BehavioralEvidence FiniteListSpineLengthV1
  ValidatedLengthCounterexample`.

The observation has nominal epoch, identity, and local roles and supports
`NFData` only. Public selectors expose status, strength, and heuristic use. The
retained query fingerprint and optional evidence have no public projection and
can be consumed only together through the replay gate. The observation has no
equality, ordering, rendering, generic representation, or raw payload
projection.

Every solver status remains forgeable heuristic input. In particular, `unsat`
is relative to this bounded encoding, not a solver certificate or proof, and it
grants no pruning authority. A satisfiable values-policy run succeeds only
after the private owner decodes the exact symbol set and independently replays
the resulting natural-number assignment against the checked Length problem.
Only that replay can construct the optional evidence. Public consumers call
`replayLengthSMTLibLiveQueryObservation`, which checks the complete query
fingerprint before inspecting the optional evidence and then replays any
evidence once more against the exact behavioral problem retained by that
query. A successful `Nothing` confirms exact query association but grants no
evidence or stronger status authority.

## Sanitized failures

Package-private failures intentionally retain enough bounded structure to
diagnose framing, protocol, symbol, valuation, and lifecycle errors. Some of
that structure is child-controlled and can repeat a spent marker or arbitrary
SMT token. The public facade therefore maps rather than wraps those values.

Session failures retain one stable class such as deadline, workspace,
executable, launch, capability, resource-limit, transport, cleanup, or internal
failure. Query failures similarly retain session-unavailable, a safe
maximum/observed count pair, deadline, configuration, resource-limit,
transport, protocol, counterexample, or internal failure. Each opaque error
record adds only a Boolean reporting incomplete owned cleanup. Paths, commands,
digests, output bytes, symbols, integer values, protocol offsets, exit codes,
escalation details, and exception text are discarded.

The primary class is chosen before cleanup classification. A query which has
already spent its worker reports its query failure and the process-cleanup bit
to its callback. Final scope cleanup remains authoritative: if release of the
whole scope is incomplete, the outer scoped operation reports a sanitized scope
cleanup failure rather than returning the callback value.

## Deadlines and exceptions

The live facade currently uses distinct bounded operations:

- the scope opener/capability probe uses the private opener deadline;
- each query creates one absolute monotonic deadline from the execution host
  deadline and carries it through gate admission, IO, replay, and identity;
  and
- final readiness/cleanup uses the private lifecycle policy.

The facade does not call those independent budgets one batch-wide hard
deadline. A higher integration can apply cancellation to a lexical scope, but
durable cleanup may continue after the operation deadline and therefore cannot
honestly promise immediate return. Synchronous and asynchronous callback
exceptions are rethrown after the private owner has started durable cleanup;
they are never converted into `unknown` or a sanitized solver failure.

## Leant insertion seam

The intended Leant consumer remains after Lean callback acceptance and before
candidate text is projected into bindings. Leant can retain each checked
handoff, seal its pure query, run eligible candidates serially in one lexical
live scope, and use the public query-first replay gate before making a
candidate-specific ranking decision. After query sealing, the query itself
retains the exact behavioral problem; the live ranking state need not retain a
second copy of the heavier handoff solely for evidence replay.

The safe initial ranking policy is stable demotion, never pruning:

1. candidates without a replayed finite-spine counterexample retain their
   structural order;
2. candidates with such a counterexample follow, also in structural order;
3. `unsat`, `unknown`, and status-only `sat` remain neutral; and
4. an operational failure discards partial live reordering and restores the
   original batch order.

The worker belongs to that one ranking batch, not to the REPL state, provider
cache, or generated `it` bindings. The facade also does not infer a behavioral
contract, executable path, or digest pin; Leant must receive those explicitly
before live wiring is enabled.

## Verification

The 33-test downstream API suite pins the rank-N signatures, safe projections,
fixed query maximum, error instances, hidden constructors, nominal roles, and
absence of raw/internal projections. The 167-test Length suite exercises the
same public surface against the compiled closed-mode fake worker, including
healthy sequential association and evidence, neutral status branches,
spent-worker faults, deadline cleanup, callback exceptions, and
error-rendering sanitation.
