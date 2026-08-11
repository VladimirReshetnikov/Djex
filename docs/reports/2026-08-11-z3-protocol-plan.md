# Pure Length/Z3 protocol plan

Date: 2026-08-11

## Scope

This checkpoint closes the pure boundary between canonical Length SMT-LIB
queries, bounded incremental response framing, and the future process-owning
Z3 adapter. It does not start a process and does not create an executed,
attested, or semantically authoritative observation.

The package-private owner is
`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol`.
Keeping it private avoids freezing nonce injection, continuation handling, or
raw transcript access as a downstream API before the live session has an
attested worker abstraction.

## Threat addressed

An incremental transport may receive more than one response in a single read.
Recursively feeding every returned tail is correct while all of those responses
answer commands which were already written. It is unsafe across a new write.
For example, this buffered sequence must not be accepted:

```text
sat
<check marker>
<valid-looking stale valuation>
```

The valuation arrived before `get-value` was written. Treating it as the answer
after merely returning a value-write action would erase the causal boundary and
could associate stale output with the current query.

## Sealed plan

`LengthSMTLibProtocolPlan` retains and privately fingerprints:

- the complete pure execution-policy key;
- the exact canonical query key;
- the stream-framing schema and total, frame, and nesting limits;
- a cumulative admitted-stdout limit;
- the phase-machine and post-barrier-whitespace schemas;
- the exact reset/check/status-marker write and expected marker response; and
- when applicable, the exact input-value/value-marker write and expected
  marker response.

The plan fingerprint is a reversible complete canonical key, not a digest, so
neither it nor its canonical bytes are public. The fingerprint byte limit is
admission-only and does not enter plan identity.

The check marker always requires exactly 32 caller-supplied nonce bytes. A value
marker is required exactly when the execution policy requests satisfiable input
values and the query has at least one input. The two markers must differ. These
checks prove only shape and within-plan distinction; the live session must
generate fresh, session-wide distinct marker material and bind its generation
and query ordinal into live identity.

## State machine

The initial action writes one ordered group:

```text
(reset)
(set-option :print-success false)
<canonical query through check-sat>
(echo "<check marker>")
```

The receiver then permits only these transitions:

1. frame and decode one exact `sat`, `unsat`, or `unknown` response;
2. frame and compare the exact check marker positionally;
3. complete for `unsat`, `unknown`, status-only policy, or a zero-input query
   (with vacuous `Just []` for satisfiable input-value policy);
4. otherwise return a separate input-value write action;
5. only after that write, frame and decode the exact query-specific valuation;
6. frame and compare the exact value marker; and
7. expose the decoded syntactic outcome atomically.

Unexpected reset output occupies the first status slot and fails. Markers are
never decoded as ordinary responses, and the machine never scans past a wrong
frame looking for a later match. A required malformed or missing valuation is
not downgraded to a status-only result.

## Byte accounting and write boundaries

The shared stream framer now reports the exact number of bytes charged to a
completed frame. This includes discarded leading trivia and retained frame
bytes, while excluding its untouched lexical lookahead tail.

Within one write group, the protocol starts the next framer on that tail:

- status to check marker; and
- valuation to value marker.

The next framer therefore charges the preceding delimiter exactly once. At the
check-marker-to-value-write boundary and at terminal completion, the current
tail is not interpreted as a response. It is scanned productively under the
cumulative stdout budget and may contain only the four SMT-LIB whitespace
bytes. Comments and all non-whitespace output fail. Only then may the value
write or completion action be returned.

Each new frame receives the smaller of its configured total-byte bound and the
transaction's remaining cumulative budget. When the cumulative budget is the
active bound, its error is reported at maximum plus one. This prevents repeated
per-frame budgets or a large post-marker tail from evading the transaction-wide
limit. If the two bounds are exactly equal, the frame-total error wins the
documented tie; cumulative failure is selected only for a strictly smaller
remaining transaction budget.

EOF never completes a reusable-worker transaction. Lexically malformed EOF has
priority; every otherwise clean or string-completing EOF is reported as missing
the response for the current phase. A live owner must destroy the worker after
every protocol failure because no continuation is returned.

## Authority boundary

`LengthSMTLibProtocolDecoded` means only that caller-fed bytes passed framing,
shape decoding, and positional marker checks for this pure plan. It deliberately
retains raw status/value frames privately and is not an execution observation.
Even an `unsat` outcome remains heuristic. Decoded satisfiable values must still
pass `validateLengthSMTLibCounterexample`, whose independent Length evaluator is
the only route to model-relative behavioral evidence.

## Next live layers

A process-owning session still needs to bind and enforce:

1. race-resistant executable resolution, hashing, and spawn;
2. the observed executable image and any configured SHA-256 pin comparison;
3. an exact Z3 capability handshake for quoted `echo`, print suppression,
   reset behavior, logic/options, and supported commands;
4. a worker/session epoch, query ordinal, and session-wide marker allocation;
5. complete writes before the corresponding receiver can consume bytes;
6. monotonic deadlines, cancellation, bounded stderr, process exit, and cleanup;
7. poisoning and teardown after every framing, decoder, marker, timeout, IO, or
   capability failure; and
8. an opaque executed-observation identity over the attested worker, pure plan,
   actual branch, writes, frames, termination outcome, and bounded artifacts.

Only that later layer may claim that a protocol outcome came from a particular
Z3 process. It still must not turn solver output into proof or pruning authority.
