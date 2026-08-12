# Typed SMT-LIB boundary-whitespace receipt

Date: 2026-08-12

## Scope

The domain-neutral causal driver previously trusted a package-private prose
law: a successful transport boundary drain returned a raw strict `ByteString`
containing only SMT-LIB whitespace. The Length Process implemented that law
correctly, but the generic operation type could also represent a successful
non-whitespace drain. In the initial-adoption path the driver performs the next
exact write before feeding those raw bytes, so a future faulty adapter could
cross stale output over the very causal boundary the driver is meant to own.

This checkpoint introduces
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.BoundaryWhitespace` and
changes only the successful drain result to its opaque
`SMTLibCausalBoundaryWhitespace` receipt.

## Content proof, not provenance proof

The hidden constructor retains one strict `ByteString` lazily. The sole smart
admission scans it with `Internal.SMTLib.Lexical.isSMTLibWhitespaceByte`; safe
concatenation joins only already-admitted receipts. The byte projection is
package-private, and `NFData` still reaches the complete retained payload.

The receipt proves exactly one fact: every retained byte is one of horizontal
tab, line feed, carriage return, or space. It does not prove:

- which process produced the bytes;
- FIFO position or predecessor attribution;
- finiteness or configured byte bounds beyond strict `ByteString` storage;
- nonblocking transport behavior;
- cancellation or deadline association; or
- restoration after every possible control race.

Those remain laws of the concrete operations-and-handle pair. The canonical
empty receipt may be constructed after an empty-boundary readiness check; every
nonempty Process drain is admitted at the source.

## Atomic Process admission

`drainLengthSMTLibProcessBoundaryWhitespace` still removes the currently
queued stdout events in one STM transaction. It now admits each original
`StdoutChunk` before advancing through the snapshot and retains each original
opaque stdout-chunk receipt beside the resulting whitespace receipt in reverse
traversal order. On the first non-whitespace chunk it restores those original
stdout receipts followed by the offending event and untouched suffix in exact
FIFO order, then returns the same ready-phase `UnexpectedPendingStdout`
failure. A queued terminal retains its existing positional precedence and
consumption behavior.

On success the Process concatenates only the admitted chunk receipts. There is
no validation scan after dequeue, when restoration would be impossible. As
before, a final cancellation, deadline, or poison check outside that STM
transaction may discard a successful drain and poison the Process; this
checkpoint does not claim rollback for that pre-existing lifecycle race.

## Driver demand order

`SMTLibCausalTransportOps` can now return a successful drain only through the
opaque receipt. For an adopted predecessor boundary, Driver retains the receipt
without opening it, completes and flushes the exact new write, and only then
projects and feeds the admitted bytes. A failed write therefore wins without
demanding the receipt or receiver. Later completed-epoch drains project their
receipt once and append those bytes to the preceding transcript epoch exactly
as before.

The receipt does not change whole/split/singleton chunk normalization,
inherited-byte attribution, EOF precedence, unexpected-byte offsets, or the
maximum-plus-one cumulative assertion.

## Compatibility

This is package-private type hardening. It changes no public facade, wire byte,
process failure constructor, transcript byte, plan field, fingerprint field,
schema tag, ready-worker identity, query-run identity, Main behavior, or Leant
configuration surface.

## Validation

The foundation suite now exhausts all 256 singleton bytes, pins the canonical
four-byte admission set, rejects a mixed snapshot, checks empty and FIFO chunk
composition, and deep-evaluates a receipt. Scripted Driver tests prove:

- an invalid initial action still precedes transport demand;
- a failed exact write precedes even a bottom-valued receipt projection;
- adopted whitespace appears exactly once in the inherited transcript;
- exact segmented transcript bytes remain chunk-independent;
- stale boundary offsets, EOF precedence, and cumulative maximum-plus-one
  classification remain unchanged.

The full Length suite continues to exercise the real Process adapter through
capability probing, split/drip output, live queries, stale output, accounting,
poisoning, and cleanup.

The later nonempty stdout-receipt checkpoint strengthens the queue itself. It
does not change this whitespace proof: Process projects an already-admitted
nonempty stdout receipt only for lexical admission, and keeps the original
receipt intact for exact rollback.
