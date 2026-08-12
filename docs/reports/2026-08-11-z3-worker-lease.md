# Scoped Length/Z3 worker lease

Date: 2026-08-11

> The [ordinal-bound query-run successor](2026-08-11-z3-query-runs.md) now
> allocates live query ordinals and barriers, drives the pure protocol through
> this lease, and independently replays satisfiable models. This report keeps
> the worker-readiness checkpoint and its threat model explicit.

## Scope

This checkpoint adds the first process-owning layer beneath the pure Length
SMT-LIB query, response, and transaction machinery. It can open one configured
worker, perform an exact readiness probe, lend the ready worker only through a
rank-N callback, and tear it down when that callback ends.

The implementation remains package-private:

- `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session` owns the
  complete scope;
- `...Session.Capability` owns the pure readiness phase machine; and
- `...Session.Process` owns one direct child and its pipes.

This readiness checkpoint by itself creates neither a solver observation nor
behavioral evidence, and it grants no pruning authority to `sat`, `unsat`, or
`unknown`. Its package-private successor now adds the separate live query layer
without changing that authority boundary.

## Honest executable observation

The process owner validates and canonicalizes the requested executable path,
reads its bytes under a configured maximum, computes SHA-256, and compares an
optional exact pin before direct spawn. The child receives exactly the sealed
argument vector, an empty environment, the fresh workspace as cwd, and three
new pipes.

The resulting value is deliberately called a pre-spawn executable-file
snapshot, not an attested executable image. The portable `process` API cannot
execute the descriptor which was hashed, so a hostile same-UID namespace
mutation can replace the pathname between observation and spawn. The digest
also excludes the dynamic loader and shared libraries. The snapshot identity
therefore binds an explicit strength tag:

```text
path-snapshot-then-direct-spawn/stable-namespace-assumption/v1
```

A future stronger backend must use a different tag and identity schema rather
than aliasing this observation.

## Workspace and barrier separation

Each allocation attempt requests 64 bytes from the OS entropy source and splits
them into independent 32-byte roles:

- a secret barrier seed retained only inside the scoped worker; and
- a public label used in the workspace pathname.

The old tempting design of placing the barrier epoch in the cwd would let the
child derive future markers before their writes. The split prevents that: the
child may observe its cwd without learning the barrier seed. Ready identity
contains a domain-separated SHA-256 commitment to the seed, never the seed
itself as a standalone field.

On POSIX the owner:

1. creates the directory exclusively;
2. opens it through a no-follow, close-on-exec directory descriptor;
3. fixes and verifies owner-only mode `0700`;
4. matches descriptor and pathname device, inode, owner, type, and mode;
5. verifies the canonical pathname again immediately before spawn; and
6. retains the descriptor until cleanup.

Cleanup never traverses or deletes child-created entries. After the direct
child is confirmed closed, it rechecks the retained object/path identity and
attempts only `rmdir`; a nonempty, replaced, broadened, or otherwise suspicious
root is retained. If direct-child cleanup is incomplete, the descriptor is
closed but the pathname is deliberately left in place. The Windows fallback explicitly claims only exclusive
creation plus repeated canonical pathname observations because the portable
dependency surface exposes neither a stable directory file ID nor a private
ACL proof.

This is still a pre-spawn path observation, not descriptor-bound cwd launch.
The same stable-namespace limitation therefore remains between the final check
and `createProcess`.

## Four-write readiness capability

The live owner derives four pairwise-distinct 32-byte echo barriers from the
secret seed and seals a pure capability plan. A receiver is exposed only after
the corresponding write obligation has been performed.

The exact phases are:

1. write startup print-success suppression plus a startup echo, then accept
   only that exact quoted echo;
2. write reset, the canonical QF_LIA preamble, one integer declaration,
   `input = 0`, `check-sat`, and a check echo; accept exactly `sat` and the echo;
3. write the exact input `get-value` request and a value echo; accept exactly
   `((djex_capability_input 0))` and the echo; and
4. write another reset and preamble, both `input = 0` and `input = 1`,
   `check-sat`, and a ready echo; accept exactly `unsat` and the echo.

Unexpected `success`, status, valuation, marker, error, or EOF output fails at
its positional phase. A malformed probe cannot be downgraded, scanned past, or
resumed. Completion proves only that this worker produced the exact bounded
syntactic transcript; it does not prove a Z3 version, executable provenance,
or semantic soundness.

## Causal output and exact transcript identity

The probe integrates the shared cumulative SMT-LIB stream cursor rather than
treating pipe reads as lines. Its opaque policy combines the bounded
single-frame lexer with the complete readiness-output maximum. Status-to-marker
and value-to-marker tails may advance only through the completed frame's hidden
same-write continuation because both responses were already caused by that
write. Across a write boundary, the hidden tail must first become a validated
input containing only the four canonical SMT-LIB whitespace bytes; consuming
and charging it produces the opaque boundary which alone can start the next
cursor under the same policy and absolute offset.

Every final quoted echo must be followed by at least one such delimiter. If a
pipe chunk ends exactly at the closing quote, the owner waits for the delimiter
before issuing the next write or committing readiness. This makes a separately
delivered newline part of the same protocol epoch rather than a scheduling
race.

Leading boundary whitespace observed after the following write is still
canonically attributed to the preceding write until the first non-whitespace
response byte. The bytes are nevertheless fed through the new receiver so the
capability-wide budget charges them exactly once. At readiness commit, exact
segmented transcript length must equal the process owner's observed stdout
count; a mismatch fails closed. The complete bytes, segmentation policy,
capability plan, and cumulative limit enter private identity.

The pure cursor owns framing and cumulative classification; the separate
causal transport driver owns write-before-feed ordering, pipe reads, delayed
predecessor attribution, and the final transcript assertion. This split changes
neither capability-plan fingerprint fields nor ready-worker identity bytes.

## Bounded process ownership

Stdout and stderr remain separate. Stdout chunks and terminal conditions share
one FIFO so EOF or a limit failure cannot overtake already admitted bytes. At a
stdout maximum, the permitted prefix is queued before the maximum-plus-one
terminal error regardless of read chunking.

The first stderr byte poisons the process. The stderr reader retains only a
count capped at maximum plus one, but continues strictly draining and
discarding later finite chunks until teardown so a pipe-sized flood cannot
hold the child alive. Because the pipes are independent, the ready point means
only that no stderr byte had been observed at that point; it cannot prove that
the child generated no stderr for a particular command.

Cancellation and absolute monotonic deadlines gate snapshotting, spawn,
writes, reads, boundary checks, and readiness. Cleanup is idempotent and owns
the direct child: it closes stdin, waits briefly for graceful exit, then applies
TERM and KILL stages when required, stops managed readers, and bounds handle
closure. Direct-child exit is polled with nonblocking `getProcessExitCode`
instead of a helper `waitForProcess`; the latter issued blocking `wait4` and
froze a non-threaded runtime during adversarial tests.

Cleanup of detached descendants is best effort. Once the direct child has
exited, the owner deliberately does not signal a possibly reused numeric
process-group identifier. Joined rollback and handle cleanup bounds also depend
on the underlying operation being interruptible; incomplete cleanup is
reported rather than falsely claimed complete.

## Ready-worker identity and authority

The private nominal ready-worker identity binds:

- the pure execution-policy fingerprint;
- the process limits, exact configured argv and cwd observation, snapshot
  digest/count/strength, and direct-process policy tags;
- the capability-plan fingerprint and exact segmented transcript;
- the barrier-seed commitment and workspace policy/path;
- the future per-query protocol framing and cumulative limits;
- the session opener deadline and query-count limit; and
- stdout/stderr counts at the point-in-time ready commit.

The callback receives no process handle or secret-seed accessor, and its
phantom epoch cannot escape the rank-N scope directly. The package-private,
reversible identity intentionally contains the already-spent readiness
barriers and exact transcript; a package consumer can recover those bytes.
They do not reveal the 256-bit seed from which later, domain-separated query
roles will be derived. The value is a worker lease, not a receipt. In
particular, the identity is not an image attestation, and capability success is
not a solver answer.

## Verification

The compiled `djex-fake-z3` fixture chooses from a closed behavior set using
only its executable basename. It records argv, environment, cwd, initial cwd
listing, commands, and output in a test-only sidecar outside the cwd, so the
production empty-environment and empty-workspace policies remain observable.

The current focused Length suite has 173 passing cases, including the query-run
successor. Worker-lease cases cover:

- healthy whole, split, singleton, empty-interleaved, and delayed-byte output;
- exact argv, empty environment, initially empty cwd, matching executable
  bytes, digest pin match, mismatch-before-spawn, and executable byte limits;
- wrong echo, wrong status, wrong valuation, and unsolicited reset success;
- one stderr byte and a 256 KiB finite stderr flood;
- immediate EOF, nonzero exit, closed-stdout hang, opener deadline, and a child
  stubborn after stdin EOF;
- exact transcript/output accounting and independently fresh worker identity;
  and
- stdout maximum behavior invariant under whole versus singleton reads.

The fake worker compiles with `-Wall -Werror`. The complete suite returns in a
few seconds and leaves no test workers behind.

## Successor status

The ordinal-bound query-run layer now allocates session-wide marker roles,
seals and drives `LengthSMTLibProtocolPlan`, binds the exact branch and
transcript to the ready-worker and plan identities, and poisons the lease after
every live failure. Only a satisfiable model which passes independent Length
replay creates model-relative counterexample evidence; `unsat` and `unknown`
remain heuristic observations. See the
[query-run report](2026-08-11-z3-query-runs.md) for the new invariants and next
integration boundary.
