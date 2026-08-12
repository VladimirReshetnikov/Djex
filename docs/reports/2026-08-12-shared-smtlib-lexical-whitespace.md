# Shared SMT-LIB lexical whitespace

Date: 2026-08-12

## Result

The package-private module
`Language.Haskell.Synthesis.Internal.SMTLib.Lexical` now owns the one exact
SMT-LIB whitespace predicate and the canonical ordered byte list
`[9, 10, 13, 32]`: horizontal tab, line feed, carriage return, and space.

This is a vocabulary extraction, not a new parser or protocol layer. The leaf
imports only `Word8`, owns no data constructors or schema tag, and grants no
framing, parsing, transport, process, or semantic authority.

## Consumers

The following package-private layers consume the lexical owner directly or
through its opaque causal boundary-whitespace receipt:

- bounded response parsing;
- incremental single-frame parsing;
- cumulative post-barrier accounting;
- causal delayed-boundary attribution;
- opaque receipt admission used by the live Process's all-or-nothing
  queued-whitespace drain; and
- the protocol and readiness-capability fingerprint builders.

Direct imports avoid making the framer a vocabulary re-export hub and prevent
the transport and parser paths from drifting apart. The standalone fake Z3
remains an independent test executable rather than deriving its oracle from the
library under test.

## Compatibility

The predicate remains four direct byte comparisons, and the list remains in
the same order. Response retention and token-limit precedence, framing
lookahead and byte charging, cumulative maximum-before-head inspection, FIFO
transport attribution, and process drain behavior are unchanged. Protocol and
capability fingerprint fields therefore remain byte-for-byte identical; no
schema or version changes in this checkpoint.

The later `Internal.SMTLib.Causal.BoundaryWhitespace` leaf consumes this sole
predicate to mint an opaque receipt. That receipt proves content only. FIFO
origin, boundedness, restoration, cancellation, deadline, and process
association remain responsibilities of the concrete transport and Process.

The lexical leaf deliberately has no identity of its own. Any future change to
the admitted bytes or their canonical order must instead revise every affected
response, framing, protocol, and capability schema identity.

## Validation

The focused lexical test enumerates all 256 `Word8` values and independently
asserts that exactly 9, 10, 13, and 32 satisfy the predicate, while a separate
literal assertion pins their canonical order. Existing response, stream,
cumulative-boundary, causal-driver, process, protocol, capability, and
fingerprint tests remain the behavioral and identity regression surface.
