# Effective-ID descriptor-bound Z3 launch

Date: 2026-08-15

## Outcome

Djex now has a third, additive executable-launch policy which combines the
sealed main-image byte authority of the descriptor launcher with two
point-in-time Linux VFS execute-access checks on the opened source. The public
pure selector is
`mkLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessExecutionConfig`,
and the closed classifier adds
`LengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunch`.
`lengthSMTLibExecutionExecutableLaunchStrategy` distinguishes it from the
literal established
`LengthSMTLibPathSnapshotThenDirectSpawn` and
`LengthSMTLibDescriptorBoundExecutableLaunch` policies without exposing a
path, digest, descriptor, access result, or process observation.

The new authority is deliberately exact and narrow. It says that the opened
source descriptor passed Linux
`faccessat2(fd, "", X_OK, AT_EMPTY_PATH | AT_EACCESS)` twice under the
caller's then-current effective filesystem credentials, and that the bytes
hashed and optionally pinned were copied into the sealed main-image descriptor
used by `execveat`. It does not say that the source remained executable
between or after those observations, nor that a complete kernel execution
security decision for the source was made.

## Why an execute mode bit was insufficient

The first descriptor-bound policy requires a regular source with at least one
execute mode bit. That is a useful shape prefilter: it prevents the launcher
from silently converting an arbitrary non-executable data file into code. It
is not caller authorization. Testing `(st_mode & 0111) != 0` does not select
the applicable owner, group, or other class, evaluate a POSIX ACL, account for
a `noexec` source mount, or run the filesystem's permission hooks.

The effective-ID sibling retains that prefilter for compatibility and early
classification, but does not call it authority. It then invokes the raw Linux
`faccessat2` system call on the already opened source descriptor. `X_OK` asks
for execute access, `AT_EMPTY_PATH` makes the descriptor itself the subject,
and `AT_EACCESS` selects effective rather than real IDs. Linux file access is
evaluated with the process's filesystem credentials; the VFS path includes
ordinary discretionary access control, POSIX ACL handling where provided by
the filesystem, source-mount `noexec`, and inode permission/security hooks.
See the primary [`access(2)` documentation](https://man7.org/linux/man-pages/man2/access.2.html),
Linux's [`do_faccessat` implementation](https://github.com/torvalds/linux/blob/v6.17/fs/open.c#L391-L547),
the VFS [`inode_permission` path](https://github.com/torvalds/linux/blob/v6.17/fs/namei.c#L317-L341),
and [`credentials(7)`](https://man7.org/linux/man-pages/man7/credentials.7.html).

## Pure public policy

The complete public classifier is closed:

```haskell
data LengthSMTLibExecutableLaunchStrategy
  = LengthSMTLibPathSnapshotThenDirectSpawn
  | LengthSMTLibDescriptorBoundExecutableLaunch
  | LengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunch
```

The new maker accepts the same `LengthSMTLibExecutionLimits` and
`LengthSMTLibExecutionConfigSource` as both established makers:

```haskell
let source =
      defaultLengthSMTLibExecutionConfigSource
        "/usr/bin/z3"
        (Just expectedZ3SHA256)
in case
    mkLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessExecutionConfig
      defaultLengthSMTLibExecutionLimits source of
  Left rejected -> Left rejected
  Right execution ->
    Right
      ( lengthSMTLibExecutionExecutableLaunchStrategy execution
      , execution
      )
```

Construction remains pure and platform-independent. It performs the same
path, digest, integer, response, and fingerprint admission in the same order;
no filesystem, credential, clock, or process observation occurs until a later
live session. The two older makers and every byte of their policy identities
remain literal.

The new policy has its own role and schema:

```text
length-z3-descriptor-bound-effective-id-executable-access-execution-policy
djex-length-z3-smtlib2-execution-policy/descriptor-bound-effective-id-executable-access/v1
```

Its exact launch-strength value is:

```text
opened-source-two-point-faccessat2-x-ok-at-empty-path-at-eaccess-hash-copy-sealed-memfd-execveat/point-in-time-effective-id-source-vfs-executable-access-and-main-image-bytes/v1
```

That versioned identity prevents a future, stronger or weaker access policy
from being substituted under the same canonical bytes.

## Exact Linux lifecycle

The live owner performs the following sequence under the existing absolute
opener deadline and cancellation authority:

1. Validate and retain the already opened working-directory authority.
2. Open the final source component with
   `O_RDONLY | O_CLOEXEC | O_NOCTTY | O_NOFOLLOW | O_NONBLOCK`. Nonblocking
   open prevents a FIFO or device from defeating the deadline before it can be
   classified; a regular file ignores that flag.
3. `fstat` the source, require a regular file, and require at least one execute
   mode bit as a shape prefilter rather than an authorization result.
4. Run the first raw descriptor-based `faccessat2` check with exactly `X_OK`
   and `AT_EMPTY_PATH | AT_EACCESS`.
5. Create a private `MFD_CLOEXEC | MFD_ALLOW_SEALING` memfd. Read every
   bounded source chunk once, supplying that same strict chunk to SHA-256 and
   to the staged image before advancing. Reinspect the source metadata and
   reject a change; then compare the optional digest expectation.
6. Set the staged descriptor to the fixed mode `0500`, apply and verify
   `F_SEAL_WRITE | F_SEAL_GROW | F_SEAL_SHRINK | F_SEAL_SEAL`, verify regular
   type, exact byte count, and exact `0500` mode, and rewind it. The mode is
   launcher-owned transport metadata, not copied source authority.
7. Run the deterministic package-private hook after pin and sealed-image
   admission. The associated checker seam receives the source as a borrowed
   numeric descriptor; it is called exactly twice on a successful prefix and
   must never retain or close that descriptor.
8. Run the same `faccessat2` check again on the same opened source descriptor,
   immediately before any fork, child, or pipe allocation.
9. Enter the established masked native launch owner. The child becomes its own
   process-group leader, selects the working directory by retained descriptor,
   installs the exact configured `argv` and empty environment, closes every
   unrelated descriptor with fail-closed `close_range`, and invokes
   `execveat` with `AT_EMPTY_PATH` on the sealed staged image. The configured
   pathname remains `argv[0]`; it is not the exec target.
10. Admit the wrapped process only after the close-on-exec status pipe proves
    that `execveat` installed the image. Existing readiness, query,
    cancellation, staged cleanup, direct-child reap, and best-effort
    process-group cleanup then apply unchanged.

There is no `/proc/self/fd` indirection, pathname retry, direct-spawn retry, or
fallback to either older launch strategy. The staged-image mechanics are the
same Linux `memfd_create` and `execveat(AT_EMPTY_PATH)` primitives documented
by [`memfd_create(2)`](https://man7.org/linux/man-pages/man2/memfd_create.2.html)
and [`execveat(2)`](https://man7.org/linux/man-pages/man2/execveat.2.html).

## Closed access failures

The native access checker returns one of four closed values and never exports
an errno:

- success becomes effective-ID executable access admitted;
- `EACCES` becomes effective-ID executable access denied;
- a build without `SYS_faccessat2`, runtime `ENOSYS`, or rejection of the
  fixed flag set with `EINVAL` becomes access check unavailable; and
- every other failure, including `EPERM`, becomes access check failed.

The call is not retried on `EINTR`: a variable retry policy would be a
different observation contract. Either check can also end through the
established cancellation or deadline class. Denial maps at the public live
facade to `LengthSMTLibLiveSessionExecutableRejected`; unavailable and failed
checks map to `LengthSMTLibLiveSessionLaunchFailed`. Cleanup incompleteness is
still reported separately. Paths, credentials, descriptors, source metadata,
raw errno values, staged bytes, and child output do not cross that boundary.

A non-Linux build can admit the pure policy but fails closed with access-check
unavailable when live work first demands it. A Linux kernel or seccomp policy
without the exact `faccessat2` operation also fails closed. No mode-bit,
pathname-based `access`, libc emulation, or descriptor-launch fallback weakens
the advertised identity.

## Point-in-time authority and exclusions

Each successful check describes the retained source descriptor and the
caller's effective filesystem credentials at one instant. It is not a lease or
reservation. Mode bits, ACLs, credentials, mount policy, security policy, or
the source inode may change between the two checks or after the second check.
The two checks narrow that window and ensure that copying begins and child
allocation is approached only after fresh admissions; they do not eliminate
concurrent policy change.

Nor is `faccessat2(X_OK)` the full execution path. Linux later applies binary
format handling and execution-time `bprm` security hooks; the actual exec in
this design is against a different memfd inode with fixed `0500` mode. The
source's owner, group, ACLs, set-id bits, file capabilities, extended
attributes, security labels, IMA state, mount identity, and other inode
metadata are not copied. An ELF interpreter, dynamic loader, shared library,
script interpreter, locale/database input, solver implementation, kernel, and
reported solver status remain unbound. Source read permission is also required
because the launcher must copy the bytes. Scripts may fail closed through the
documented `execveat` close-on-exec interpreter interaction rather than being
retried by pathname.

Linux 6.14 added the distinct `AT_EXECVE_CHECK` operation for a fuller
execution-context check. The kernel documentation explicitly describes it as
a separate exec-check interface; see
[`check_exec`](https://docs.kernel.org/userspace-api/check_exec.html) and the
ordinary [`exec` permission path](https://github.com/torvalds/linux/blob/v6.17/fs/exec.c#L764-L800).
Djex does not opportunistically select it under this v1 identity: supported
deployments include older kernels, and a source `AT_EXECVE_CHECK` followed by
execution of a staged memfd would still require a carefully specified
authority boundary. Any such policy must receive its own public strategy,
schema, strength tag, platform rules, and tests.

## Identity and compatibility

The new strategy has domain-separated execution-policy, raw process,
ready-worker, scalar-run, and binary-product-run identities. Fresh per-query,
shared-v1, and scoped-v2 deadline selections each have distinct scalar and
pair run branches. Their ordered authority fields bind the two-point access
policy, source observation, staged digest and size, optional pin result, fixed
`0500` staging policy, descriptor execution, argv/environment, workspace, and
process ownership. Mutable descriptor numbers, thread identifiers, errno
values, and later pathname state are not identity material.

The legacy and first descriptor strategies retain their exact prior policy,
process, readiness, and fresh/shared/scoped scalar/pair identities. Query,
protocol, behavioral-problem, checked-contract, counterexample, and
applicable-domain receipt schemas do not change. Access admission is an
operational launch observation, never behavioral evidence and never solver
soundness authority.

## Characterization

The focused matrix pins:

- all three closed public classifiers and makers, plus hidden policy state;
- unchanged canonical bytes for both older policies and distinct new role,
  schema, strength, process, ready-worker, and scalar/pair run selections;
- pure construction and deferred all-pure use performing zero filesystem,
  access-check, and process IO;
- exactly two checker calls on one borrowed descriptor, with the second after
  sealing and the test hook but before child allocation;
- effective ownership-class denial plus deterministic checker-seam coverage;
  POSIX ACL and source-mount `noexec` semantics remain documented kernel/VFS
  scope rather than privileged, environment-dependent portable fixtures;
- first-check and second-check denial, unavailable, checker failure,
  cancellation, and deadline mapping with no child and complete descriptor
  rollback;
- fixed staged mode `0500`, required seals, exact size, and same-byte
  hash/copy/exec authority;
- pin mismatch before child allocation, pathname replacement after staging,
  successful scalar and pair transactions, compact ordinals, and atomic
  session reset after failure;
- no fallback on an unsupported backend, checker unavailability, staging
  failure, or descriptor-exec failure; and
- unchanged empty environment, exact configured `argv[0]`, working-directory
  descriptor, unrelated-fd closure, process-group ownership, deadlines,
  cancellation, and cleanup.

Descendant containment remains a separate limitation. The launcher owns and
reaps the direct child and uses best-effort process-group signalling while the
leader is known, but it cannot prove that detached or surviving descendants
were killed after leader exit. Strengthening descendant ownership would need
its own lifecycle and identity checkpoint; it does not alter the correctness
of the two point-in-time source-access observations or sealed main-image byte
binding described here.
