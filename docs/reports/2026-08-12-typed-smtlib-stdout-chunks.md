# Typed nonempty SMT-LIB stdout chunks

Date: 2026-08-12

## Scope

The generic causal driver previously stated that every successful
`nextStdoutChunk` result was nonempty, but its operation returned a raw strict
`ByteString`. A future adapter could therefore represent `Right empty`, make no
receiver or transcript progress, and keep the driver reading forever. In the
post-epoch boundary path, the same empty success could also be mistaken for a
required delimiter before a later write.

This checkpoint adds the package-private, schema-free
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.StdoutChunk` leaf. The
transport operation now returns its opaque `SMTLibCausalStdoutChunk` receipt.

## Exact proof boundary

The hidden strict one-field `data` representation preserves the former queue
demand of `StdoutChunk !ByteString`. Its sole smart admission rejects exactly
the empty strict byte string and accepts every nonempty byte vocabulary. The
package-private projection and `NFData` instance expose no constructor or
additional authority.

The receipt proves only nonemptiness of finite strict bytes. It does not prove:

- FIFO position or source process;
- the configured per-read or cumulative byte bound;
- cancellation or deadline association;
- response framing, lexical validity, or boundary whitespace; or
- any protocol, fingerprint, or schema identity.

Those remain responsibilities of the concrete operations-and-handle pair,
the cumulative cursor, and the Length plans.

## Process admission and terminal order

The Length stdout reader admits the result of every nonempty OS read before it
can enter the FIFO. An empty successful read still creates the existing stdout
EOF terminal. A read crossing the configured stdout maximum still charges
maximum plus one and, in one STM transaction, enqueues any nonempty permitted
prefix before its terminal failure. When no permitted byte remains, no empty
chunk can be enqueued.

As before, the controller's final cancellation, deadline, or poison check may
discard an already-dequeued successful receipt and poison the Process. This
type hardening does not add rollback for that pre-existing control race.

`nextLengthSMTLibProcessStdoutChunk` and the Length transport preserve the
opaque receipt. Driver projects it only at the two former raw-byte consumption
points: normal receiver feed and post-epoch boundary collection. Thus EOF
finish precedence, first-unexpected-byte offsets, split-chunk normalization,
delayed-whitespace attribution, transcript accounting, and cumulative
maximum-plus-one behavior are unchanged.

Boundary draining still applies its stronger lexical admission. It now keeps
each original stdout receipt beside the whitespace receipt, so a
non-whitespace rejection restores the exact original receipt segmentation and
FIFO order rather than reconstructing a chunk from raw bytes.

## Compatibility

This is package-private type hardening. It changes no accepted solver byte,
wire command, transcript byte, process failure, public facade, fingerprint
field, schema tag, ready-worker identity, query-run identity, Main behavior, or
Leant configuration.

## Validation

The foundation suite rejects the empty byte string, admits every one of the
256 singleton byte values independently of lexical meaning, pins a mixed
multi-byte payload, and deep-evaluates the receipt. Scripted causal-driver
tests can construct successful reads only through admission. Existing raw
Process tests pin both a nonempty prefix before the stdout-limit terminal and
the exact-limit case where no empty prefix event is emitted. A focused Driver
trace makes a separately read all-whitespace receipt extend its predecessor
epoch and verifies that those bytes reach the successor receiver only after
the next exact write. The full live suite continues to cover whole, split,
singleton, and drip reads, EOF and fault precedence, accounting, poisoning,
and cleanup.
