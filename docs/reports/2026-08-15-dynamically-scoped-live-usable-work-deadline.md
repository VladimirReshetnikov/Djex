# Dynamically scoped live usable-work deadline

Date: 2026-08-15

## Outcome

Djex now exposes an additive v2 usable-work deadline whose public authority is
valid only on its creating thread and during the dynamic extent of its owner
callback. The owner closes runtime admission on both normal and exceptional
exit. A closure can still retain the opaque Haskell value, but a checkpoint or
session-opening attempt through that value after exit is rejected before clock,
configuration, workspace, or process demand. A forked token operation is
likewise rejected while the owner callback is still open.

The v2 deadline retains the original shared-budget objective: application
preparation, session opening, and a mixed scalar/product query batch can share
one absolute monotonic deadline instead of receiving a new full window for
every query. It remains a cooperative usable-work boundary rather than an
asynchronous watchdog.

The runtime-unscoped v1 API is retained for source and exact identity
compatibility. The legacy fresh-per-query entrance, v1 budgeted identities,
canonical SMT-LIB, query and protocol identities, observation and replay
surfaces, and behavioral authority remain unchanged. V2 uses new additive
ready-worker and scalar/product run identities.

## Why rank-N was not a dynamic boundary

The v1 owner supplies:

```haskell
LengthSMTLibLiveUsableWorkDeadline budget
```

through a callback quantified over `budget`. This generativity prevents
ordinary type-level mixing of tokens captured by different owners. It does not
prove that the value is used only during the callback. A result can hide the
token inside an `IO` action closure whose public type does not mention
`budget`, and a callback can give the token to a forked thread. V1 retains no
owner thread or open/closed state, so either action can later attempt to open a
session under the captured absolute deadline.

Consequently, the earlier claim that the rank-N phantom itself prevented
escape was too strong. V1 is safe only under a caller discipline which neither
retains nor shares its token. It remains present because changing or silently
reinterpreting its established identities would conflate two different
runtime policies.

V2 keeps the nominal phantom and adds the runtime facts needed to enforce the
intended use boundary. The public token remains opaque:

```haskell
LengthSMTLibLiveScopedUsableWorkDeadline budget
```

Its private representation owns the validated duration, captured absolute
deadline, creating `ThreadId`, and mutable open/closed state. The thread and
state are admission mechanisms, not semantic or fingerprint authority.

## Public v2 surface

The additive public operations are:

```haskell
withLengthSMTLibLiveScopedUsableWorkDeadline
  :: LengthSMTLibLiveUsableWorkBudget
  -> (forall budget.
        LengthSMTLibLiveScopedUsableWorkDeadline budget -> IO result)
  -> IO (Either LengthSMTLibLiveSessionError result)

checkLengthSMTLibLiveScopedUsableWorkDeadline
  :: LengthSMTLibLiveScopedUsableWorkDeadline budget
  -> IO (Either LengthSMTLibLiveSessionError ())

withLengthSMTLibLiveSessionUnderScopedDeadline
  :: LengthSMTLibLiveScopedUsableWorkDeadline budget
  -> LengthSMTLibExecutionConfig
  -> (forall epoch. LengthSMTLibLiveSession epoch -> IO result)
  -> IO (Either LengthSMTLibLiveSessionError result)

withLengthSMTLibLiveSessionWithScopedUsableWorkBudget
  :: LengthSMTLibLiveUsableWorkBudget
  -> LengthSMTLibExecutionConfig
  -> (forall epoch. LengthSMTLibLiveSession epoch -> IO result)
  -> IO (Either LengthSMTLibLiveSessionError result)
```

The existing pure `mkLengthSMTLibLiveUsableWorkBudget` validation is shared by
v1 and v2. It still rejects nonpositive milliseconds and host-microsecond or
monotonic-nanosecond conversion overflow without reading a clock.

## Owner, checkpoint, and session example

This batch forces deferred application work after capture, explicitly observes
the same absolute deadline, then opens a session and performs nominally
separate scalar and product queries:

```haskell
import Control.DeepSeq (force)
import Control.Exception (evaluate)

budget <- either (fail . show) pure $
  mkLengthSMTLibLiveUsableWorkBudget
    LengthSMTLibLiveUsableWorkBudgetSource
      { lengthSMTLibLiveUsableWorkBudgetSourceMilliseconds = 30000 }

owned <-
  withLengthSMTLibLiveScopedUsableWorkDeadline budget $ \deadline -> do
    (scalarQuery, pairQuery) <- evaluate $ force
      (deferredScalarQuery, deferredPairQuery)
    checkpoint <- checkLengthSMTLibLiveScopedUsableWorkDeadline deadline
    case checkpoint of
      Left failure -> pure (Left failure)
      Right () ->
        withLengthSMTLibLiveSessionUnderScopedDeadline
          deadline executionConfig $ \session -> do
            scalarResult <- runLengthSMTLibLiveQuery
              defaultLengthEvaluationLimits session scalarQuery
            pairResult <- runLengthSpinePairSMTLibLiveQuery
              defaultLengthEvaluationLimits session pairQuery
            pure (scalarResult, pairResult)

case owned of
  Left ownerFailure -> handleSessionFailure ownerFailure
  Right (Left checkpointOrSessionFailure) ->
    handleSessionFailure checkpointOrSessionFailure
  Right (Right (scalarResult, pairResult)) ->
    consumeNominalResults scalarResult pairResult
```

When there is no preparation between capture and session opening, the shorter
entrance is:

```haskell
withLengthSMTLibLiveSessionWithScopedUsableWorkBudget
  budget executionConfig $ \session -> do
    scalarResult <- runLengthSMTLibLiveQuery
      defaultLengthEvaluationLimits session scalarQuery
    pairResult <- runLengthSpinePairSMTLibLiveQuery
      defaultLengthEvaluationLimits session pairQuery
    pure (scalarResult, pairResult)
```

## Dynamic admission and precedence

The v2 owner captures the monotonic deadline, records its own thread, and
creates an open lease before restoring ordinary exception delivery around the
user callback. On a normal return it closes the lease before performing the
owner's final deadline observation. On an exceptional return it closes the
lease under masking and rethrows the exception. There is therefore no public
exit path which intentionally leaves admission open.

Both token-consuming public operations first compare the current thread with
the recorded owner and then inspect the open/closed state. A wrong-thread or
closed use maps to the new byte-free public class:

```haskell
LengthSMTLibLiveSessionUsableWorkScopeUnavailable
```

Only an admitted operation reads the monotonic clock. Scope unavailability
therefore wins if the token is both stale and expired. The public scoped
session entrance performs this admission before evaluating the execution
configuration. The package-private opener repeats it at the production
boundary before selecting an effective deadline or acquiring a workspace or
process resource.

This is runtime rejection, not a claim that Haskell makes capture impossible.
For example, returning a hidden action is well typed, but invoking it after the
owner exits returns scope unavailable:

```haskell
escaped <-
  withLengthSMTLibLiveScopedUsableWorkDeadline budget $ \deadline ->
    pure $ checkLengthSMTLibLiveScopedUsableWorkDeadline deadline

case escaped of
  Left ownerFailure -> handleSessionFailure ownerFailure
  Right useLater -> do
    stale <- useLater
    -- stale is Left ...UsableWorkScopeUnavailable
    consumeExpectedStaleResult stale
```

Similarly, a child thread which checkpoints or opens a session with the token
gets scope unavailable even before the owner callback returns. The guarantee
applies to operations on the v2 deadline token. After an owner-thread session
opening has been admitted, the lent `LengthSMTLibLiveSession epoch` continues
to use its existing rank-N callback lifecycle, serial query gate, and closing
state; v2 does not redefine the worker as a general thread-affinity primitive.

## Cooperative checkpoint, not watchdog

`checkLengthSMTLibLiveScopedUsableWorkDeadline` observes the existing absolute
deadline. It does not create or extend a window. A successful checkpoint:

- consumes no scalar/product query ordinal;
- writes no SMT-LIB and waits for no child response;
- creates no solver observation or behavioral evidence;
- changes no ready-worker or query-run identity; and
- installs no timer or asynchronous exception.

A computation which blocks forever, performs unbounded caller IO, or never
reaches a checkpoint or live operation is not interrupted. The next explicit
checkpoint or controlled live boundary can observe expiry, and normal callback
return is checked as described below. “Usable-work deadline” is intentionally
narrower than “hard wall-clock deadline.”

## Session coverage and minimum selection

Once scoped admission succeeds, v2 covers the same Djex-owned operations as
v1: workspace allocation and inspection, executable snapshot and launch,
capability probing, waiting for the shared serial query gate, scalar or product
transaction transport, bounded response handling, independent model replay,
and query-run identity admission and sealing.

Opening and every query use the earlier of the captured shared absolute
deadline and the applicable fresh local deadline. The shared deadline wins an
exact tie. When the configured local duration is shorter, the runtime still
compares absolute deadlines because time may have elapsed since shared capture.
The existing maximum of 64 committed-or-spent transactions remains one shared
scalar/product ordinal budget and is independent of elapsed time.

Scoped-token admission occurs at checkpoint and session-opening boundaries;
query execution beneath an already admitted worker is bounded by the retained
absolute deadline and existing worker lifecycle. This avoids fingerprinting
or treating mutable scope state as part of a query's semantic association.

## Two-step and convenience finalizer behavior

The general two-step owner,
`withLengthSMTLibLiveScopedUsableWorkDeadline`, closes its lease and checks the
shared deadline when its callback returns normally. If that callback waits for
a nested `withLengthSMTLibLiveSessionUnderScopedDeadline`, it cannot return
until the nested session's final readiness observation and durable process and
workspace cleanup have completed. Those finalizer stages use fresh established
private windows rather than the shared usable-work deadline, but their elapsed
time can make the later outer-owner check report
`LengthSMTLibLiveSessionDeadlineExceeded`.

The convenience
`withLengthSMTLibLiveSessionWithScopedUsableWorkBudget` has a deliberately
different outer boundary. The nested session checks expiry immediately after
its user callback and before fresh final readiness and cleanup. When the nested
session later returns, the outer owner closes the v2 lease but performs no
second deadline check. Finalizer time alone therefore cannot replace otherwise
successful usable work with a shared-deadline failure in the convenience form.

This difference matches the established v1 two-step/convenience accounting;
v2 adds lifecycle enforcement without silently changing which finalizer stages
the convenience entrance excludes. Callback exceptions remain authoritative.
They are not replaced by a deadline or scope-unavailable `Either`, and durable
owned cleanup retains its existing bounded behavior and separately reported
incomplete bit.

## Additive identities

Every historical and v1 identity remains byte-exact. Scoped v2 uses a separate
ready-worker fingerprint role:

```text
finite-list-spine-length/z3-capability-probed-ready-worker/scoped-shared-usable-work-deadline/v2
```

and envelope tag:

```text
djex-length-z3-scoped-shared-usable-work-deadline/v2
```

The exact legacy ready-worker identity is embedded once. The additive fields
bind the duration and captured deadline, minimum/shared-on-tie policy, owned
operation coverage, owner-thread/open-only admission, close-on-normal-or-
exceptional-exit lifecycle, scope-unavailable precedence, cooperative
checkpoint semantics, and fresh final-readiness/cleanup policy.

Scalar v2 runs use:

```text
djex-length-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/scoped-shared-usable-work-deadline/v2
finite-list-spine-length/z3-live-query-run/scoped-shared-usable-work-deadline/v2
```

Product v2 runs use the nominally separate sibling:

```text
djex-length-spine-pair-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/scoped-shared-usable-work-deadline/v2
finite-binary-product-spine-lengths/z3-live-query-run/scoped-shared-usable-work-deadline/v2
```

Each run binds its effective deadline and cause in addition to the complete
applicable legacy association and scoped policy. The owner `ThreadId`, mutable
lease cell, and sequence or outcome of explicit checkpoint calls are omitted:
they are runtime admission details, unstable across executions, and not
semantic evidence. V2 ready-worker, scalar-run, and product-run identities are
therefore distinct from each other and from v1 without pretending that a
checkpoint transcript exists.

## Behavioral authority limit

Dynamic scope improves process/session causality only. It does not attest the
executable image, loader, libraries, or solver; validate Z3 soundness; turn
`unsat` into proof; grant pruning authority; or transfer authority between the
scalar and binary-product domains.

Every public solver status remains `HeuristicRankingOnly`. Optional
counterexample evidence still requires bounded input decoding, independent
replay against the exact nominal behavioral problem, and complete query
association at the matching public replay gate. Checkpoint success means only
that this owner-thread operation observed an open lease before its captured
absolute deadline. It is not a behavioral receipt.

For the original shared-deadline coverage, minimum-selection, and v1 identity
details, see the
[shared live usable-work budget report](2026-08-15-shared-live-usable-work-budget.md).
