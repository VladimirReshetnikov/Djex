# Query-owned Length raw-input replay

Date: 2026-08-14

## Result and scope

Djex now exposes
`replayLengthSMTLibCounterexampleInputs` beside the existing decoded-model
validator. Its input is deliberately only a source-ordered `[Natural]` under
caller-supplied `LengthEvaluationLimits` and one sealed `LengthSMTLibQuery`.
The caller does not reconstruct generated SMT symbols, integer bindings, the
checked problem, or its behavioral association.

This is a pure replay entrance. It starts no solver, consumes no solver report,
and turns neither `sat` nor `unsat` into authority. It exists so an integration
can retry previously observed concrete inputs without retaining or rebuilding
solver-facing structure beside the opaque query.

## One owner for semantic association

The sealed query already retains the exact `CheckedLengthProblem` from which
its canonical commands, input symbols, query fingerprint, and behavioral
problem were derived. Raw-input replay reads that private checked problem
directly. It therefore cannot pair a caller-owned input vector with one query's
symbols and another problem's evaluator or evidence association.

Every call independently runs `validateLengthProblemCounterexample` against
that retained problem under the supplied evaluation limits. A violating input
vector first creates new `BehavioralEvidence`; the entrance then immediately
replays that evidence against the exact `BehavioralProblem` projected by the
same query. Only a successful same-problem association releases the fresh
`ValidatedLengthCounterexample` receipt. No evidence, receipt, or verdict is
cached in the query.

The result distinguishes three ordinary outcomes:

- `Right (Just receipt)` means this call freshly validated and associated a
  concrete finite-spine counterexample;
- `Right Nothing` means this call completed but the inputs did not satisfy the
  checked bad-state semantics; and
- `Left` reports either bounded evaluation rejection or the sanitized generic
  association mismatch through `LengthSMTLibInputReplayError`.

The two closed error constructors are
`LengthSMTLibInputReplayEvaluationRejected` and
`LengthSMTLibInputReplayAssociationRejected`. They expose no generated symbol,
SMT payload, transcript, process fact, or cached solver conclusion.

## Demand and authority boundary

The existing Length evaluator owns input demand and failure precedence. It
observes the compact modeled-input arity productively before inspecting an
input value, so a cyclic or overlong vector is rejected after the sealed
maximum-plus-one prefix. Candidate/provider evaluation still runs under the
explicit limits and may return the existing bounded `LengthEvaluationError`.
An honestly completed non-counterexample remains `Nothing`, not an error and
not evidence.

A returned receipt is model-relative finite-spine evidence with the same
provider-law qualifications as ordinary Length replay. It is not universal
behavioral proof, pruning permission, dictionary evidence, or an `unsat`
certificate.

## Relationship to decoded models and live observations

`validateLengthSMTLibCounterexample` remains the raw-model boundary. It accepts
untrusted symbol/integer bindings, verifies the exact generated symbol set,
rejects negative values, restores source input order, and then performs
independent replay. The new entrance does not weaken or bypass those checks for
solver output; it serves callers that already possess natural inputs rather
than decoded model bindings.

`replayLengthSMTLibLiveQueryObservation` remains the only public semantic
extraction edge from a completed live observation's hidden query association
and optional evidence. Raw-input replay is separate: it extracts no live field
and instead constructs new evidence by evaluating caller-supplied inputs. A
saved vector therefore carries no retained live ordinal, transcript, query-run
identity, solver status, or receipt authority.

## Identity compatibility

The entrance adds no field to `LengthSMTLibQuery` and changes no translation,
canonical command, symbol, typed-plan, checked-problem, behavioral-problem,
query, execution-policy, response, protocol-plan, ready-worker, or query-run
identity. All existing schema tags and version numbers remain unchanged.

## Validation boundary

Regression coverage pins the public signature and error vocabulary, parity
with exact decoded-model replay, fresh result/provider-basis association across
distinct sealed queries, productive arity rejection before value demand,
evaluation-limit rejection, the ordinary `Nothing` path, and the public facade
surface.
