# Descriptor spawn resource ownership

Date: 2026-08-15

## Outcome

Djex closes the asynchronous-exception gaps between a resource-producing Z3
Process action, publication to its deadline controller, and acceptance of a
native descriptor child by the opaque process owner. The implementation landed
in `fbdc63a3e1af9fe242c0721c518c73e6bb6e994b`.

This is package-private lifecycle hardening. It changes no public launch
selector, execution policy, process observation, protocol, query, behavioral
receipt, fingerprint, or evidence authority.

## One terminal worker completion

The deadline controller now observes one terminal completion cell. Its worker
runs the attempted action, returns to a masked state, and publishes the
resulting `Either` as its final effect. There is no separately published
outcome followed by a second completion signal.

The ordinary controller restores non-resource work. Its resource-producing
sibling keeps acquisition masked through result publication; the action may
restore only interruptible regions for which it has already installed cleanup.
This distinction covers the portable `createProcess` result as well as source-
descriptor, staged-image-descriptor, and native descriptor-child acquisition.

If cancellation, deadline expiry, or a controller exception wins, the
controller kills the worker, reads that same terminal completion to join it,
and rolls back any published successful value. If the final control check
fails after normal publication, it performs the same rollback. Callback
exceptions are sanitized under the existing Process error boundary and do not
become retained authority.

The resulting ownership invariant is:

```text
acquire under mask
  -> publish one terminal result under mask
  -> controller either rolls back or begins handoff
```

No acquired value can become visible while a second worker-completion event is
still pending, and no asynchronous exception can discard a value between its
return from the resource action and publication.

## Native child handoff

The three native descriptor strategies now share one path from
`DescriptorCreated` to `Z3SMTLibProcess`:

- descriptor-bound sealed main-image launch;
- descriptor-bound effective-ID executable-access launch;
- descriptor-bound execve-check executable-access launch.

The shared handoff acquires while masked. After a successful acquisition, it
opens one restored post-acquisition checkpoint protected by the raw
`DescriptorCreated` rollback. Production uses a no-op checkpoint, so the
restoration is still a real asynchronous delivery boundary. If interruption
lands there, raw cleanup owns the child and all three standard-stream handles,
and the process consumer is never entered.

If the checkpoint completes, the consumer is entered while masked. It first
allocates the opaque process and its reader/cleanup state. Allocation failure
cleans the raw descriptor child. Once allocation succeeds, ownership has moved
exactly once: restored initialization runs under an exception handler which
closes the `Z3SMTLibProcess`, and raw cleanup no longer owns those resources.

```text
raw DescriptorCreated owner
  -> restored checkpoint, raw rollback installed
  -> masked process allocation
     -> allocation failure: raw cleanup
     -> allocation success: Z3SMTLibProcess becomes sole owner
        -> restored initialization exception: close process
```

Deadline/cancellation failures during native spawn retain the existing cleanup
status attachment. The refactoring removes three duplicated launch bodies but
does not merge their launch-strategy identities or weaken their distinct
admission checks.

## Characterization

The focused regression uses a real temporary `Handle` and pathname as an
injected acquired resource. It establishes all of the boundary conditions:

1. acquisition observes `MaskedInterruptible`;
2. the worker reaches the restored post-acquisition checkpoint;
3. an injected asynchronous exception propagates exactly;
4. the acquired handle is closed and the pathname is removed;
5. the consumer never accepts ownership.

The focused case passed 1/1 and an independent 100-repeat stress run. Strict
warning-as-error library/test builds passed. The frozen repository validation
also passed all 16 suites and 1,807 tests, including 370/370 Length cases and
37/37 API cases; `cabal check` and `git diff --check` were clean.

## Documentation boundary

The current normative lifecycle is in
[semantic foundations](../semantic-foundations.md#resource-acquisition-and-descriptor-child-handoff).
Earlier launch reports remain historical records of their respective image,
access-check, and identity checkpoints; their descriptions of an older
handoff implementation do not override the current Process ownership rule.
