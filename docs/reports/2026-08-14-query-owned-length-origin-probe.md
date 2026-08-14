# Query-owned Length origin probe

Date: 2026-08-14

## Outcome

Djex now exposes one canonical, solver-independent cold-start probe for a
sealed Length query:

```haskell
probeLengthSMTLibCounterexampleAtOrigin
  :: LengthEvaluationLimits
  -> LengthSMTLibQuery identity local
  -> Either LengthSMTLibInputReplayError
      (Maybe ValidatedLengthCounterexample)
```

The caller supplies no input count, symbols, contract syntax, candidate result,
or assignment. The query privately owns its exact checked problem. The probe
derives that problem's compact modeled-input count, constructs exactly one zero
per input in source order, and delegates to
`replayLengthSMTLibCounterexampleInputs`.

This is intentionally one assignment rather than a second finite-box
enumerator. It complements the four-entry orchestration bank in Leant and
Djex's complete bounded input-box validator without caching or duplicating
either policy.

## Authority boundary

The origin vector is merely a deterministic input choice. It gains authority
only through the existing checked-problem evaluator and exact behavioral
association replay:

- arity comes from the sealed problem rather than caller reconstruction;
- the configured `LengthEvaluationLimits` still bound concrete and
  intermediate values;
- the candidate result is recomputed from the checked candidate;
- precondition and postcondition demand order is unchanged;
- provider-backed violations retain the ordinary explicit assumed-law basis;
  and
- an association mismatch remains the existing fail-closed
  `LengthSMTLibInputReplayAssociationRejected` result.

A successful hit is the existing opaque `ValidatedLengthCounterexample`.
`Nothing` establishes only that the all-zero assignment was not a
counterexample; it is not positive bounded evidence, universal evidence, or
permission to prune. An evaluation rejection remains the existing input-replay
error. The probe consumes no solver observation, and no `sat`, `unsat`, or
`unknown` status schedules or strengthens it inside Djex.

## Demand and operational scope

The checked query already has a finite, sealed compact input count. Constructing
the origin therefore needs no new caller-controlled list traversal or
assignment-count policy. A nullary query probes `[]`; unary and wider queries
probe the exact corresponding zero vector. The implementation reuses the
ordinary replay function rather than adding another evaluator or association
path.

Djex does not choose when an application should run this probe. In particular,
this checkpoint changes no live-session behavior by itself. A later caller may
place it before a live query under a separate explicit orchestration policy;
probe misses and evaluation rejections must not be presented as solver or
behavioral verdicts.

## Identity compatibility

The probe emits no SMT-LIB command, opens no process, and creates no protocol
frame, live observation, query ordinal, or execution artifact. It introduces
no receipt or verifier tag: a hit uses the existing counterexample receipt and
its existing behavioral association.

No contract, provider-inventory, semantic-inventory, session-policy,
candidate, concrete-encoding, complete-problem, SMT-query, response, protocol,
execution, process, worker, query-run, or live-observation identity or schema
changes. Canonical query bytes and fingerprints are unchanged.

## Validation coverage

Focused Length tests cover:

- exact nullary, unary, binary, and widened origin derivation;
- equality with ordinary caller-supplied input replay;
- provider-independent and assumed-provider-backed receipts;
- ordinary non-counterexample and false-precondition misses;
- unchanged evaluation-limit rejection; and
- facade availability without exposing a new constructor, arity projection,
  or forgeable evidence path.
