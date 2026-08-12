# Shared raw Z3 process runtime

Date: 2026-08-12

## Outcome

Djex now has one package-private, domain-neutral owner for the raw Z3 runtime:

`Language.Haskell.Synthesis.Internal.SMTLib.Z3.Process`

The existing
`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process`
module remains as a compatibility and identity facade. Existing Session,
Transport, Live, and test consumers therefore keep their exact Length
constructors, selectors, `Eq`/`Ord`/`Show` behavior, and sanitized public-error
mapping while future behavioral domains can reuse the lower runtime without
importing Length semantics.

## Runtime ownership

The shared owner opens from the admitted generic Z3 launch profile and retains
one opaque subprocess together with:

- launch facts derived from that profile, including bounded pre-spawn
  executable observation and optional pin comparison;
- the exact configured argv, empty environment, caller-supplied empty cwd, and
  three binary pipes;
- FIFO nonempty stdout receipts and terminal events;
- first-byte stderr poison with bounded retained accounting and continued
  draining;
- isolated exact writes, cancellation, and one absolute monotonic deadline;
- typed all-whitespace queued-boundary drains; and
- idempotent leader-owned graceful/TERM/KILL cleanup with bounded reader and
  handle shutdown.

The IO implementation was moved mechanically. Its CPP split, POSIX metadata
and process-group behavior, Windows fallbacks, masked allocation and rollback,
reader ordering, poison precedence, maximum-plus-one reporting, and cleanup
state machine are unchanged.

## Identity-neutral boundary

The shared runtime owns no semantic-domain schema tag, fingerprint root, or
fingerprint budget. It retains one opaque associated observation whose lazy
ordered field slice contains exactly the prior snapshot, canonical cwd and
emptiness observation, PID observation, launch/runtime method tags, and
absolute deadline. It also retains the exact admitted process limits and
exposes their ordered inner fingerprint fields without assigning a domain tag.

The Length facade is the only owner of:

- `djex-length-z3-raw-process/v2`;
- the `length-z3-launched-transport` fingerprint root; and
- the `length-z3-process-limits/v1` nested wrapper.

It seals the old root as the unchanged v2 schema field, followed by the exact
associated generic observation, followed by the exact associated limit
wrapper. No caller can supply a detached observation or limits value when the
facade constructs a live process identity. Canonical raw-process,
ready-worker, and query-run bytes therefore remain unchanged by this
extraction, and no schema version advances.

## Compatibility and demand order

The facade keeps closed Length phase, failure, cleanup, and error datatypes and
maps every generic constructor explicitly. Each facade delegation restores the
underlying operation's original masking state, then completes compatibility
mapping under the facade mask. A successfully acquired process or destructively
dequeued receipt therefore cannot be separated from its Length result by a new
mapping-time asynchronous-exception window. The facade does not interleave a
second control check or cleanup action. Optional cleanup status is rebuilt to
the same strict outer level, and dynamic exception strings, paths, digests,
commands, and output bytes remain absent from errors.

Wrapper projections are passed lazily into the shared opener. Consequently:

1. initial cancellation and deadline rejection still precede process limits,
   launch profile, and working-directory demand;
2. absolute/empty working-directory admission still precedes process limits
   and launch-profile demand;
3. executable observation, hashing, pin comparison, pre-spawn control, spawn,
   and handle configuration retain their prior order; and
4. the associated observation field list remains lazy behind a strict opaque
   observation constructor, matching the former strict fingerprint-root / lazy
   field-list boundary.

## Verification

Focused regressions now pin:

- cancelled open with bottom limits, profile, and cwd;
- relative-cwd rejection with bottom limits and profile;
- the exact Length fingerprint root, v2 schema prefix, snapshot position,
  field count, and final v1 process-limit wrapper;
- absence of the complete Length execution key from the raw field and exactly
  one occurrence at the ready-worker boundary;
- configured argv, empty environment, cwd, pin match and pre-spawn mismatch,
  executable and stdout maximum-plus-one behavior, FIFO prefix-before-terminal
  ordering, stderr poison, deadlines, and cleanup; and
- representative mapped Length failures and the existing public live-behavior
  suite through the unchanged facade.

The new module is an internal Cabal home module only. This checkpoint changes
no exposed API, SMT-LIB wire byte, transcript, identity byte, file format,
evidence rule, pruning authority, or Main behavior.
