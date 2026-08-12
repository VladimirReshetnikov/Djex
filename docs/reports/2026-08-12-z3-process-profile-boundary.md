# Profile-only raw Z3 process ownership

Date: 2026-08-12

## Outcome

The raw Length/Z3 process owner now consumes only the package-private admitted
`Z3SMTLibExecutionProfile`. It no longer accepts the complete
`LengthSMTLibExecutionConfig`, and its executable snapshot no longer stores a
second canonical copy of the complete Length policy key.

The scoped Session retains the complete live policy. Its ready-worker identity
contains exactly one occurrence of that key beside the raw process observation,
rather than another copy inside that observation.

## Authority split

The raw Process retains only facts which it enforces or observes:

- requested and canonical executable paths;
- bounded executable metadata, SHA-256 digest, byte count, and exact pin result;
- the configured argv derived from admitted timeout and resource controls;
- empty child environment and requested/canonical caller-supplied cwd observed
  empty;
- spawn-isolation flags, absolute deadline, and bounded process limits; and
- the executable-snapshot strength statement.

It does not own the Length artifact policy, response grammar or limits,
protocol schema, complete execution fingerprint, capability plan, or semantic
query policy. Those facts first meet the process observation in the
ready-worker identity owned by `...SMTLib.Session`.

## Failure and demand order

Narrowing the argument does not move a launch-profile projection ahead of an
earlier check. The Process still observes:

1. initial cancellation and absolute deadline;
2. absolute, present, empty working-directory admission;
3. executable pathname canonicalization and first metadata;
4. bounded hashing;
5. repeated metadata and canonical-path consistency;
6. optional digest-pin equality;
7. the pre-spawn cancellation/deadline check; and
8. direct spawn and handle configuration.

Focused poisoned-profile tests pin that initial cancellation and a relative
working-directory refusal return before the profile is demanded.

## Identity migration

The raw process tag advances from
`djex-length-z3-raw-process/v1` to `/v2`. The ready-worker tag advances from
`djex-length-z3-capability-probed-ready-worker/v3` to `/v4`, making the change
from two complete-policy occurrences to one within ready-worker identity
explicit. Query-run schema does not change: it embeds that already versioned
ready-worker key through the same field.

Canonical ready-worker and query-run keys intentionally change and shrink.
Consequently, a custom identity-byte budget at the former exact boundary can
newly admit the same worker or run. No previously admitted value becomes
oversized. Padding or a second admission pass would preserve the very duplicate
authority this checkpoint removes.

## Verification

The focused tests pin:

- raw-process v2 and ready-worker v4 literals;
- zero exact complete-policy byte fields anywhere in the raw process field;
- exactly one complete-policy canonical byte sequence in ready-worker identity;
- cancellation and relative-cwd precedence without launch-profile demand;
- deterministic maximum-plus-one ready-worker identity rejection under the
  shorter v4 key, alongside ordinary default admission;
- configured argv, empty environment, fresh cwd, pin match/mismatch, executable
  byte caps, FIFO stdout limits, and complete cleanup; and
- independently fresh worker identities and unchanged public live behavior.

This is a private authority and identity-schema migration. It changes no public
API, SMT-LIB bytes, file format, evidence rule, pruning authority, or Main
behavior.
