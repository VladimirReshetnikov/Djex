# Ready-worker post-probe query authority

Date: 2026-08-12

## Outcome

`LengthSMTLibReadyWorker` no longer retains the complete five-part
`LengthSMTLibSessionConfig` after the capability probe. The opaque nominal
worker instead owns one private strict post-probe query policy containing only:

- the maximum query count;
- the query-run identity fingerprint budget;
- the Length protocol limits used to seal future plans; and
- the complete Length execution policy used to derive each query deadline and
  required by those plans.

This is a storage and association change only. The v4 ready-worker identity,
v1 query-run identity, SMT-LIB writes, public facade, cleanup behavior, and
failure vocabulary are unchanged.

## Completed and retained authority

Workspace-allocation attempts, capability limits, and ready-worker identity
admission have completed before the rank-N callback receives a worker. The
opener deadline and Session workspace-cleanup policy remain in the enclosing
Session scope for the final readiness check and teardown; they are not copied
into the worker. Process shutdown limits remain process-owned.

The exact process limits already belong to the opaque retained process. A new
package-private Length projection exposes those associated limits without
reconstructing them from Session configuration or the cached raw-process
fingerprint. Remaining stdout capacity and worst-case query-run identity sizing
therefore consume the same process-owned limits. The private identity-admission
helper no longer accepts a separately pairable limits argument.

## Plan-owned replay

A sealed `LengthSMTLibProtocolPlan` already retains its exact nominal query and
complete execution policy. Replay now projects both the query and artifact
policy from that plan. The worker-wide execution policy remains necessary for
deriving the next query deadline and sealing its plan; it is not a second
replay authority for a plan which already exists.

This preserves the existing order:

1. lease and query-count admission;
2. controlled process-boundary observation;
3. exact protocol-plan sealing;
4. remaining process-capacity and query-run-identity admission;
5. atomic ordinal/barrier reservation;
6. causal driving, accounting, replay, and identity construction.

Pre-reservation capacity or identity rejection still performs no write,
consumes no ordinal, and leaves the worker reusable.

## Verification

Focused regressions pin:

- process-owned limit projection against the exact limits used at open;
- protocol-plan artifact-policy projection alongside its exact query;
- a one-byte query-run identity cap rejecting twice before any query write;
- an exactly exhausted nondefault process stdout cap rejecting a query before
  any write; and
- the existing nondefault two-query maximum and artifact-policy branches.

The complete Djex build and test suite remain the compatibility boundary. No
schema tag or fingerprint field changes in this checkpoint.
