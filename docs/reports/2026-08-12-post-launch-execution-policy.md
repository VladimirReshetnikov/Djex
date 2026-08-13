# Post-launch Length execution authority

Date: 2026-08-12

## Outcome

The Length ready worker and protocol sealer no longer retain or accept the
complete structured `LengthSMTLibExecutionConfig` after ready-worker identity
admission. A new package-private opaque
`LengthSMTLibPostLaunchExecutionPolicy` is the single strict worker
post-launch owner, before protocol-plan sealing, of exactly:

- the validated host-deadline milliseconds used to create each query deadline;
- the exact artifact policy used to choose the satisfiable value branch;
- the exact response limits used for protocol admission and decoding; and
- the original complete Length execution-policy fingerprint object.

The post-launch policy has no Z3 execution profile. In particular, it cannot
separately project the executable path or digest expectation, solver timeout or
resource limit, argv, environment policy, or working-directory policy.

This is structured-authority removal, not byte scrubbing. The complete policy
fingerprint is a reversible canonical key, and the original object is retained
unchanged so ready-worker, protocol-plan, and query-run identities continue to
contain the exact policy bytes they contained before this checkpoint.

## Boundary and order

Pre-readiness Session code still owns the complete execution configuration. It
uses that authority for the shared Z3 profile and process launch, retains it
through the capability probe, and consumes its complete key in the unchanged
v4 ready-worker identity. Only after ready-identity fingerprint admission
succeeds does Session narrow the configuration and construct the strict
post-launch policy for the lent worker.

Each query then observes the same sequence:

1. project the retained host deadline and create the absolute query deadline;
2. perform lease admission and controlled process-boundary observation;
3. derive positional barriers and inspect the artifact branch;
4. seal a protocol plan from the associated artifact/response/key policy;
5. perform capacity and query-run identity admission;
6. reserve, drive, replay, and construct the unchanged run identity.

Protocol framing validation reads the response limits from this associated
post-launch owner. Protocol fingerprinting projects the original complete key
and emits its canonical bytes at the same field position. The artifact branch
is still inspected before framing validation. No error constructor or failure
precedence changes.

## Strictness and association

The post-launch policy is a four-field strict data type with an explicit
`NFData` instance. Its constructor is hidden, and the narrowing function copies
only already validated strict fields plus the existing fingerprint object; it
does not revalidate or rebuild identity. `ReadyWorkerQueryPolicy` retains this
opaque value as one strict field beside the existing query-count, identity, and
protocol bounds.

Once a protocol plan has been sealed, it remains the sole runtime owner of its
artifact and response projections. Replay still obtains its exact query and
artifact policy from that plan. The post-launch worker policy cannot be paired
with a completed plan as an alternate replay authority.

## Compatibility and verification

No public module exports the new type. No Cabal module list, schema tag,
fingerprint version or role, canonical field, SMT-LIB write, parser behavior,
process lifecycle, cleanup path, evidence rule, or Leant-facing behavior
changes.

Focused tests assert parity of all four post-launch projections with the
admitted complete configuration. Existing regressions continue to pin:

- the complete execution-policy canonical length and SHA-256 snapshot;
- protocol-plan fingerprint sensitivity to launch-only policy changes;
- artifact-policy status/value branching;
- nondefault response-limit enforcement after plan sealing;
- per-query host-deadline behavior;
- exact protocol-plan fingerprint bytes and writes; and
- live query replay, accounting, cleanup, and worker-spending behavior.
