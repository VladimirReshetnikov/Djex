# Descriptor-bound Z3 main-image launch

Date: 2026-08-15

## Outcome

Djex now has an additive Linux executable-launch policy which binds the bytes
hashed for an optional SHA-256 expectation to the sealed main image passed to
the kernel. The public pure selector is
`mkLengthSMTLibDescriptorBoundExecutionConfig`; the established
`mkLengthSMTLibExecutionConfig` remains the exact pathname-snapshot policy.
Callers can distinguish the sealed choices with
`lengthSMTLibExecutionExecutableLaunchStrategy` without recovering a path,
digest, descriptor, or runtime observation.

The new boundary is intentionally narrower than whole-process attestation. It
does not measure or bind an ELF interpreter, dynamic loader, shared object,
locale/database input, source privilege metadata, Z3 implementation semantics,
or any status emitted by the child. It establishes only that the staged,
sealed main-image bytes are the bytes selected by descriptor execution.

## The gap in the established launcher

The portable process owner historically performed these separate operations:

1. resolve and inspect the configured pathname;
2. read it under an executable-byte maximum;
3. compute SHA-256 and compare an optional expectation;
4. inspect the pathname again; and
5. ask `process` to spawn that pathname.

Its identity accurately called this
`path-snapshot-then-direct-spawn/stable-namespace-assumption/v1`. A namespace
replacement between steps 4 and 5 could make file A satisfy the pin and file B
be executed. Merely retaining A's opened descriptor would close that rename or
symlink race, but it would not freeze A against an in-place writer after the
hash. Calling such a design “hashed executed bytes” would be too strong.

The new launcher therefore executes an immutable staging descriptor rather
than the mutable source descriptor.

## Pure policy

The additive public classifier is closed:

```haskell
data LengthSMTLibExecutableLaunchStrategy
  = LengthSMTLibPathSnapshotThenDirectSpawn
  | LengthSMTLibDescriptorBoundExecutableLaunch
```

The descriptor constructor accepts the same
`LengthSMTLibExecutionConfigSource` and `LengthSMTLibExecutionLimits` as the
legacy constructor. Both constructors perform the same path, integer, digest,
response, and fingerprint admission before producing an opaque
`LengthSMTLibExecutionConfig`. Neither constructor performs IO.

Descriptor selection lives inside that opaque execution config. It is not a
parallel session flag which could disagree with the policy fingerprint. The
post-launch authority retains the selected classifier beside the exact policy
key. The legacy constructor calls its original fingerprint builder unchanged;
the descriptor constructor uses an additive role, schema, and launch field.

## Linux staged-image algorithm

The live descriptor branch owns the following sequence under the existing
cancellation and absolute opener deadline:

1. Open the final executable component read-only with close-on-exec and
   no-follow flags.
2. Inspect the descriptor, require a regular source with executable mode
   authority, and retain its bounded metadata.
3. Create a private anonymous file with `MFD_CLOEXEC` and
   `MFD_ALLOW_SEALING`.
4. Read each source chunk once. Charge the existing executable-byte maximum,
   update SHA-256 with that exact strict chunk, and write the same chunk to the
   anonymous image before advancing.
5. Reinspect the source descriptor. A changed source observation fails closed;
   the staged digest nevertheless still names exactly the copied image.
6. Compare an optional expected SHA-256 digest. A mismatch closes both
   descriptors and creates no child.
7. Give the staged image an unprivileged executable mode, verify its exact
   size, and apply write, grow, shrink, and further-seal prohibitions.
8. Read back and verify the required seal set and size, then rewind the staged
   descriptor.
9. Allocate isolated stdin, stdout, stderr, and exec-status pipes; fork a new
   process-group leader; select the already-owned fresh working directory by
   descriptor; install only the three standard streams; close every unrelated
   inherited descriptor; and invoke `execveat` with an empty pathname and
   `AT_EMPTY_PATH` on the sealed image.
10. Admit the process only after the close-on-exec status pipe reports exec
    success. A fixed child failure payload is mapped to the existing sanitized
    process/session failure boundary and never exposes errno text or paths.

There is no `/proc/self/fd` pathname, inherited ambient environment, shell, or
fallback to direct path spawn. Failure of `memfd_create`, source admission,
copying, hashing, pin comparison, `fchmod`, sealing, descriptor preparation,
`fork`, or `execveat` is terminal for this opening attempt.

## Why the copied image is the authority

For every admitted chunk `b`, the same strict `ByteString` value is supplied
to both the SHA-256 update and the anonymous-image write before the next read.
The final digest therefore names the exact concatenation written to the staged
image. The write/grow/shrink seals prevent later content or length changes,
and the child calls `execveat` on that sealed descriptor rather than resolving
the configured path again.

An attacker may rename or replace the original pathname after opening. An
attacker with a previously opened writable source handle may even modify the
source inode after staging. Neither action changes the already sealed image.
Without a configured pin, the runtime observation still records the digest of
the staged bytes; with a pin, the child is not created unless those bytes match
the caller's expectation.

## Process ownership and rollback

Descriptor launch plugs into the existing raw process owner rather than
creating a second child lifecycle. The native fork PID is wrapped as the same
`ProcessHandle` abstraction used by readiness, queries, finalization, and
cleanup. The child is a process-group leader, so the established graceful
close, direct termination, group kill, bounded reap, reader shutdown, cleanup
status, and idempotence paths remain authoritative.

Acquisition is masked until every returned descriptor and PID has a Haskell
owner. The exec-status handshake runs under the same opener deadline. A
cancellation, deadline, exception, malformed native result, missing pipe, or
post-fork failure closes parent descriptors and enters the established child
cleanup path. A deterministic package-private pre-exec hook exists only for
characterization; it runs after sealing and pin admission but before child
allocation, under the same owner, and exposes no descriptor.

## Identity and compatibility

The established branch remains byte-exact:

- the legacy execution-policy role, schema, ordered fields, canonical byte
  count, and SHA-256 characterization do not change;
- its pathname snapshot strength tag remains literal;
- its process observation, ready-worker, scalar-run, product-run, shared-v1,
  and scoped-v2 identity selections remain literal; and
- query bytes, protocol plans, barrier rules, transaction ordinals, replay,
  evidence, and status authority do not change.

The descriptor branch has new domain-separated policy, process observation,
ready-worker, scalar-run, and product-run selections. Shared-v1 and scoped-v2
deadline envelopes retain their established deadline semantics while embedding
the descriptor-specific applicable identity. Mutable descriptor numbers,
thread identifiers, errno values, and the configured path after opening do not
become evidence.

Only the new versioned identities distinguish this operational authority.
There is no behavioral-problem, checked-contract, SMT-LIB query, response,
counterexample, or applicable-domain receipt schema change.

## Failure and platform boundary

The public facade retains its byte-free closed failure vocabulary. Source open,
regularity, executable admission, byte limit, metadata, and digest failures map
to the corresponding executable rejection. Missing Linux primitives, native
staging failure, and descriptor exec failure map through the existing resource,
unavailable, or launch classes. A live-session failure still reports cleanup
incompleteness separately and grants no candidate evidence.

Pure descriptor policy construction is platform-independent so configuration
can be decoded without IO. A platform without the native sealed-image backend
fails closed only if a live session actually tries to open it. It never
silently chooses the legacy path launcher.

## Characterization

The focused matrix pins:

- legacy pure-policy canonical bytes and SHA-256 unchanged;
- distinct public classifier values and descriptor policy identity;
- pure construction performs no filesystem IO;
- digest mismatch, no-follow symlink, non-regular and non-executable sources
  start no child;
- exact maximum and maximum-plus-one source sizes;
- a post-seal hook which replaces the pathname and overwrites the source while
  the original staged fake Z3 still supplies the capability transcript;
- blocked post-seal-hook cancellation and deadline expiry before child
  allocation, plus staged-image exec failure and direct-child reaping;
- no unrelated inherited descriptor in the child;
- unchanged stdin/stdout/stderr separation, empty environment, fresh working
  directory, process group, response caps, readiness, deadlines, and cleanup;
- scalar/product and unbudgeted/shared/scoped identity separation; and
- public facade signatures, NFData, hidden constructors, and the absence of a
  raw descriptor projection.

Full-suite characterization remains serial where executable fixtures are
replaced. The deterministic hook avoids scheduler races and exists only on the
package-private process seam.

## Remaining authority limits

The staged image is not a universal executable attestation:

- SHA-256 remains an external digest format with its ordinary assumptions;
- an ELF interpreter and dynamic loader are selected by the kernel and are not
  staged or measured;
- shared libraries, locale files, solver configuration outside the sealed
  policy, kernel behavior, and hardware are not bound;
- script-interpreter behavior is not promised;
- source set-id bits, file capabilities, extended attributes, and ACLs are not
  copied to the unprivileged image;
- descendant cleanup remains best effort after the direct child exits; and
- a successful capability probe says only that the worker speaks the required
  bounded protocol. `sat`, `unsat`, and `unknown` remain heuristic, and only
  independent Length replay can release model-relative evidence.

The next semantic checkpoint—exact root-quotient consequences for bounded
behavioral constraints—is deliberately separate from this operational launch
authority.
