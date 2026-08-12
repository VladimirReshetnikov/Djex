# Shared cumulative SMT-LIB stream ownership

Date: 2026-08-12

## Scope

This checkpoint extracts the cumulative response-stream mechanics shared by
the pure Length query protocol and the four-write worker capability probe into
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream`.

It changes no public module, wire command, accepted response, schema tag,
fingerprint field, live identity, or failure payload. It creates no process,
solver observation, or semantic evidence. The existing domain machines still
own their phase order, exact response decoding, positional barriers, and
domain-specific failure vocabulary.

## Duplicate authority removed

Both machines previously retained and implemented the same facts beside their
domain phase state:

- configured single-frame limits and one cumulative transaction maximum;
- an absolute frame-start offset;
- an effective total bound equal to the smaller of configured frame total and
  remaining cumulative budget;
- the rule that configured frame-total failure wins an exact tie;
- recursive feeding of an untouched tail within one completed write;
- post-barrier whitespace validation before a new write;
- absolute unexpected-byte offsets; and
- productive cumulative failure reported at maximum plus one.

Keeping two copies made future drift possible precisely at the causal boundary
where stale output must fail closed. The shared layer now owns that policy and
transition vocabulary once.

## Opaque type-state boundary

`SMTLibCausalStreamPolicy` strictly retains one `SMTLibStreamLimits` value and
one cumulative byte maximum. A public initial cursor can start only at absolute
offset zero. The arbitrary-offset constructor is private.

Feeding may return an opaque completed frame. That value deliberately retains
its frame bytes, exact policy, absolute end, and untouched lexical tail without
exporting the latter three components. It offers two distinct transitions:

1. same-write continuation starts the next frame at the retained end and feeds
   the retained tail directly; or
2. boundary consumption validates and charges the complete retained tail, then
   returns an opaque boundary which alone may start the next write's cursor.

The API exposes no nonzero cursor built from an unvalidated tail. The migrated
Protocol and Capability machines therefore cannot combine a tail with a
different limit, reset its offset, or return their next write before the
present tail is sealed. Terminal branches validate and discard a boundary
without constructing an unused next framer.

Completed-frame fields remain lazy so domain response decoding and marker
comparison still precede tail traversal. Policy, cursor, and validated boundary
fields remain strict, preserving the old receiver and `NFData` demand.

## Failure and productivity rules

Each cursor seals an effective single-frame total of:

```text
min(configured frame total, cumulative maximum - absolute frame start)
```

where subtraction saturates at zero. A raw total-limit failure becomes the
cumulative maximum-plus-one failure only when that effective total is strictly
smaller than the configured total. Equality continues to report the raw
configured frame-total error.

Boundary traversal checks cumulative exhaustion before inspecting the next
byte. Empty tail at the exact maximum succeeds; a next finite byte,
bottom-valued byte payload, or cyclic whitespace stream fails at maximum plus
one without demanding data beyond that boundary. The accepted bytes remain
exactly horizontal tab, line feed, carriage return, and space.

The base `Internal.SMTLib.Stream` module now owns that ordered whitespace
vocabulary. The single-frame lexer, cumulative cursor, causal transport driver,
and both unchanged domain fingerprints consume the same definition.

## Domain ownership retained

Protocol and Capability each store the exact shared policy in their validated
limits and carry that same value into the sealed plan. Their nominal receivers
retain only plan, phase, and opaque cursor. They continue to:

- parse or compare the current frame before selecting a continuation;
- label same-write continuation failures with the next expected phase;
- compare a positional barrier before inspecting its tail;
- return a write only after a successful validated boundary; and
- report lexical EOF before their phase-specific missing-response error.

Plan fingerprint construction still projects the same stream totals, frame
limit, depth, cumulative maximum, and ordered whitespace bytes in the same
fields. No generic fingerprint or schema tag was introduced.

## Validation

The new foundation tests cover policy construction, projection, and `NFData`,
configured versus cumulative tie precedence at zero and nonzero offsets,
opaque same-write tail continuation, absolute boundary offsets,
validated-boundary restart, exact whitespace vocabulary, finite and cyclic
maximum-plus-one behavior, exhausted boundary head non-demand, lazy
completed-frame tails, and lexical EOF mapping.

The existing Length suites continue to cover exact query and capability writes,
whole/split/singleton/drip chunking, stale pre-write output, every status and
value branch, frame/cumulative ties, cyclic boundary whitespace, EOF, live
transcript accounting, run identities, cleanup, and public abstraction. New
domain regressions additionally pin that response rejection and barrier
mismatch do not force a completed frame's poisoned tail.
