# Ordinal-bound Length/Z3 query runs

Date: 2026-08-11

## Scope

This checkpoint turns the capability-probed Length/Z3 worker lease into a
bounded serial query owner. It connects the existing pure query and protocol
plans to the live process without granting solver output more authority than it
has earned.

The implementation remains package-private in
`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session`. The
central operation accepts explicit independent evaluation limits, a scoped
ready worker, and one sealed Length query. It returns either an ownership
failure or an opaque nominal query run. No process handle, secret barrier seed,
receiver, or raw transcript accessor is exposed.

## Shared causal driver

Readiness and ordinary queries now share
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver`. The generic driver
owns exact writes, stdout reads, incremental machine feeds, EOF precedence,
boundary delimiters, and the final transcript-limit assertion for either pure
action machine.
The private Length `...SMTLib.Session.Transport` adapter keeps the exact
process, cancellation token, and deadline together behind that neutral
transport seam.

The two Length action machines separately share
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream`. That pure layer
owns their common frame policy, absolute cumulative cursor, same-write hidden
tail continuation, and validated cross-write boundary. The transport driver
does not own a protocol plan or parse a response: Session carries the exact
plan through driving and run-identity construction, while each pure receiver
maps the shared cursor's closed failures into its own phase vocabulary. Domain
schema tags and canonical plan fingerprint fields remain byte-for-byte
unchanged.

The driver's important ordering rule is:

1. collect only bounded SMT-LIB whitespace left by the preceding write;
2. perform and flush the new exact write;
3. feed that saved whitespace to the newly activated receiver; and
4. await bytes which may causally answer the write.

Feeding before the write would violate the receiver obligation even when the
saved bytes were only whitespace. Feeding arbitrary retained tail after the
write would be worse: a valid-looking valuation emitted before `get-value`
could be misassociated with the new request.

Whitespace which arrives late is attributed canonically. Leading whitespace
caused by a predecessor but first observed by its successor is retained in a
separate inherited segment, fed exactly once for protocol charging, and counted
exactly once in the process stdout delta. This makes whole, split, singleton,
and delayed output agree independently of pipe chunking.

## Lease, ordinals, and barriers

One masked `TMVar` gate serializes queries. Waiting observes the gate under the
same cancellation, poison, lifecycle, and absolute monotonic deadline ordering
as process IO; ownership is claimed only afterward by a masked nonblocking
take. This avoids losing a destructive STM acquisition to the process
controller's final precedence check.

The lease state retains:

- `Accepting`, `Closing`, or `Spent` mode;
- the next zero-based ordinal;
- all four readiness markers and both query-role markers for every reserved
  ordinal;
- the optional in-flight ordinal; and
- the last committed stdout and stderr counts.

Each query role is derived as HMAC-SHA256 with the unexposed 32-byte session
seed as key and this fixed message:

```text
query-barrier-schema || role-byte || ordinal-u64be
```

Both check and input-value roles are derived, checked against the bounded
session-wide marker set, and reserved even when the particular branch will not
request values. Maximum query count is validated against the chosen `Word64`
wire representation. The prepared transaction retains only the authoritative
`Natural` lease ordinal; its `Word64` form is transiently derived at the HMAC
and run-identity encoding edges rather than cached beside it. Exhaustion is
derived from the next ordinal and that fixed maximum instead of being stored as
a parallel lease mode. Plan
construction, remaining transport capacity, and run-identity capacity are
checked before reservation. Once reserved, an ordinal and both markers are
burned on every outcome.

Callback return changes a healthy lease to `Closing` before it waits for the
gate. A transaction which already owns the gate may finish, but its commit
cannot reopen the lease. A spent worker skips the old unconditional final
readiness check, so an expected query failure returned by the callback is not
overwritten by the cancellation which that failure intentionally caused.

## Live transaction and poisoning

The initial query write is the exact reset/check/check-marker group sealed by
the pure protocol plan. Only satisfiable input-value policy with a nonempty
request can cause the second exact get-value/value-marker write. Satisfiable
zero-input policy completes with vacuous `Just []` and no fabricated command.

The driver rejects stale output at the check-marker-to-value-write boundary,
then the owner requires:

- exact protocol completion;
- empty process queues at point-in-time boundary observations;
- transcript byte count equal to the process stdout delta;
- unchanged stderr count; and
- a final equivalent boundary observation after replay and identity work.

A pure plan rejection, query-count exhaustion, gate-wait deadline, remaining
stdout-capacity rejection, or run-identity admission rejection performs no
write and consumes no ordinal. Marker collision and every failure after
reservation atomically mark the lease `Spent`, cancel it, and start idempotent
process cleanup before returning. A process poison discovered while waiting
for the gate also spends the lease. A later query sees only the spent lease.
Package-private diagnostic errors may retain bounded child response bytes,
generated symbols, or integer values; a future public facade must map these to
byte-free classes.

## Replay and authority

`sat` under status-only policy remains a syntactic heuristic and carries no
model evidence. `unsat` and `unknown` also remain heuristic observations.

For satisfiable input-value policy, the exact decoded bindings—including the
vacuous zero-input assignment—are independently passed through
`validateLengthSMTLibCounterexample` under caller-supplied evaluation limits.
A structural/model error or a replay result of `Nothing` spends the worker.
Only `Just BehavioralEvidence` is retained by a successful query run. This is
model-relative counterexample evidence, not solver-soundness evidence and not
permission to treat an unsatisfiable result as proof.

## Reversible run identity

The opaque nominal run identity is a collision-free canonical fingerprint, not
a digest. A separate two-megabyte default admission cap is used because the
ready-worker cap alone is smaller than the protocol's maximum exact transcript.
An arithmetic encoder-size preflight mirrors the fingerprint wire format and
proves the worst permitted run can fit without allocating a dummy maximum-size
transcript.

The identity binds, in order:

- the complete ready-worker identity and authority tags;
- zero-based ordinal, HMAC schema, seed commitment, and both spent markers;
- the complete pure protocol-plan key;
- the absolute monotonic deadline;
- inherited-whitespace length and ordered write-kind/epoch lengths;
- the exact bounded transcript bytes once;
- decoded status and absent, vacuous, or framed-value branch;
- independent replay policy, limits, and successful outcome; and
- start/end stdout and stderr counts plus the final readiness policy.

The retained transcript SHA-256 and the byte count derived from the already
validated stdout boundaries are convenient projections only; they never
replace the exact bytes in identity. Because the canonical fingerprint is
reversible, package consumers can recover the spent markers and bounded
transcript. The secret seed remains unprojected.

## Verification

The compiled fake worker now supports healthy ordered input models, `unsat`,
`unknown`, stale pre-value output, and bounded status/value hangs while keeping
the four-stage capability probe exact. Query events record zero-based ordinals
and whether `get-value` was actually written.

The Length suite has 173 passing tests. Eleven live-query cases cover:

- exact admission of the `Word64` ordinal boundary and rejection at maximum
  plus one;
- two sequential satisfiable model runs with distinct ordinals and identities;
- status-only `sat`, values-policy `unsat` and `unknown`;
- vacuous zero-input and ordered binary assignments;
- split and delayed output with exact transcript accounting;
- stale pre-write model rejection with no get-value command and no reuse;
- absolute-deadline teardown of a hung status; and
- exact maximum-query admission followed by maximum-plus-one rejection.

## Next checkpoint

The next layer can associate these scoped query runs with typed candidates and
use independently replayed counterexamples as behavioral ranking evidence. It
must preserve the current authority split: raw statuses remain heuristic,
query-run identities remain internal, failures do not resume a spent worker,
and no solver result may bypass the checked Length problem and evidence replay
path.
