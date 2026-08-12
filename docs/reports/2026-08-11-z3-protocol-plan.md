# Pure Length/Z3 protocol plan

Date: 2026-08-11

> The [scoped worker lease](2026-08-11-z3-worker-lease.md) and its
> [ordinal-bound query-run successor](2026-08-11-z3-query-runs.md) now
> implement the live process, causal execution, and observation association
> described here while retaining this pure plan as a caller-feedable boundary.

## Scope

This checkpoint closes the pure boundary between canonical Length SMT-LIB
queries, bounded incremental response framing, and the future process-owning
Z3 adapter. It does not start a process and does not create an executed,
attested, or semantically authoritative observation.

The package-private owner is
`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol`.
Keeping it private avoids freezing nonce injection, continuation handling, or
raw transcript access as a downstream API before the live session has an
capability-probed worker abstraction.

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

`LengthSMTLibProtocolPlan` retains the owners needed to rederive, and privately
fingerprints:

- the complete pure execution-policy key;
- the exact canonical query key;
- the stream-framing schema and total, frame, and nesting limits;
- a cumulative admitted-stdout limit;
- the phase-machine and post-barrier-whitespace schemas;
- the positional check marker and, when applicable, the positional input-value
  marker; and
- the exact reset/check/status-marker and optional input-value/value-marker
  writes together with their expected marker responses.

The concatenated write fragments are transient fingerprint inputs rather than
parallel retained fields. The plan keeps the sealed query and positional
markers which uniquely render them, and derives each exact write on demand
through the selectors used at its causal action edge. Presence-only inspection
of the optional write does not render request bytes. Sealing still renders and
admits the identical bytes into the unchanged complete key before the smaller
plan can escape.

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

The package-private generic Standard response layer owns the canonical
`sat`/`unsat`/`unknown` spellings, bounded check-status classification, and
standard `unsupported`/solver-error shapes. The Length decoder maps that closed
failure vocabulary into its unchanged compatibility errors and separately owns
query-specific valuation shape. The readiness capability reuses only the
canonical `sat` and `unsat` bytes: it still performs exact frame equality and
collapses every mismatch to its phase-only failure.

Unexpected reset output occupies the first status slot and fails. Markers are
never decoded as ordinary responses, and the machine never scans past a wrong
frame looking for a later match. A required malformed or missing valuation is
not downgraded to a status-only result.

## Byte accounting and write boundaries

The base stream framer reports the exact number of bytes charged to a completed
frame. This includes discarded leading trivia and retained frame bytes, while
excluding its untouched lexical lookahead tail. The domain-neutral
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream` layer now owns the
transaction-wide policy and absolute cursor used here and by the readiness
capability machine.

The opaque initial cursor can begin only at absolute offset zero. A completed
frame retains its policy, absolute end, and untouched tail without exposing
them. Its same-write continuation starts the next frame at that exact end and
feeds that exact tail. Its separate boundary operation first validates and
charges all remaining whitespace, then returns an opaque boundary which alone
can start the first frame after a new write. Callers therefore cannot detach a
tail from its admitted offset, cumulative maximum, or frame policy.

Within one write group, the protocol starts the next framer on that tail:

- status to check marker; and
- valuation to value marker.

The next framer therefore charges the preceding delimiter exactly once. At the
check-marker-to-value-write boundary and at terminal completion, the current
tail is not interpreted as a response. It is scanned productively under the
cumulative stdout budget and may contain only the four SMT-LIB whitespace
bytes. Comments and all non-whitespace output fail. Only then may the value
write or completion action be returned.

Each cursor frame receives the smaller of its configured total-byte bound and
the transaction's remaining cumulative budget. When the cumulative budget is
the active bound, its error is reported at maximum plus one. This prevents
repeated per-frame budgets or a large post-marker tail from evading the
transaction-wide limit. If the two bounds are exactly equal, the frame-total
error wins the documented tie; cumulative failure is selected only for a
strictly smaller remaining transaction budget. At a write boundary, exhaustion
is checked before the next tail byte is inspected, so maximum-plus-one also
remains productive for finite, poisoned, or cyclic whitespace.

The ordered whitespace vocabulary remains the exact bytes 9, 10, 13, and 32.
Its schema-free package-private owner is `Internal.SMTLib.Lexical`; the bounded
response parser, single-frame lexer, cumulative cursor, causal transport
attribution, process boundary drain, and unchanged protocol and capability
fingerprints all consume that same definition.

EOF never completes a reusable-worker transaction. Lexically malformed EOF has
priority; every otherwise clean or string-completing EOF is reported as missing
the response for the current phase. A live owner must destroy the worker after
every protocol failure because no continuation is returned.

## Authority boundary

`LengthSMTLibProtocolDecoded` means only that caller-fed bytes passed framing,
shape decoding, and positional marker checks for this pure plan. It deliberately
retains only the closed status and optional decoded integer bindings. The
caller retains the exact plan through driving and, in a live session, through
run-identity construction; the terminal branch does not copy either that plan
or its complete key. Raw status-frame and input-value-frame bytes likewise
remain in the process-owning causal transcript rather than being copied into
the decoded branch. A pure caller-fed decode needs no diagnostic copy. This
value is not an execution observation, and even an `unsat` outcome remains
heuristic. Decoded satisfiable values must still pass
`validateLengthSMTLibCounterexample`, whose independent Length evaluator is the
only route to model-relative behavioral evidence.

## Successor live layers

A process-owning session still needs to bind and enforce:

1. bounded executable resolution, hashing, pin comparison, and direct spawn,
   with an explicit stable-namespace limitation;
2. an honestly named pre-spawn executable-file snapshot rather than an
   executed-image attestation;
3. an exact Z3 capability handshake for quoted `echo`, print suppression,
   reset behavior, logic/options, and supported commands;
4. a worker/session epoch, query ordinal, and session-wide marker allocation;
5. complete writes before the corresponding receiver can consume bytes;
6. monotonic deadlines, cancellation, bounded stderr, process exit, and cleanup;
7. poisoning and teardown after every framing, decoder, marker, timeout, IO, or
   capability failure; and
8. an opaque query-run identity over the capability-probed worker, pure plan,
   actual branch, writes, frames, termination outcome, and bounded artifacts.

The worker lease and query-run successors now implement all eight ownership
items under the explicit portable limitations above. The live run remains a
syntactic observation; only an independently replayed satisfiable model can
produce model-relative counterexample evidence. Solver output is still not
proof or pruning authority.
