# Shared live usable-work budget for Length/Z3

Date: 2026-08-15

## Outcome

Djex now has an additive live-session entrance which can charge opening and a
mixed scalar/product query batch to one shared absolute monotonic deadline.
This closes the operational gap in which the historical entrance derived a
fresh host window for every query, allowing a 64-query lease to consume nearly
64 independent query windows.

The new policy is deliberately named a *usable-work budget*. It bounds the
live operations which Djex owns and observes; it is not an asynchronous
watchdog around arbitrary caller code. The existing 64-entry mixed-domain
ordinal budget remains a separate admission limit.

The legacy entrance, canonical SMT-LIB bytes, execution policy, query and
protocol identities, observations, replay behavior, and evidence authority are
unchanged. Callers opt into the shared elapsed-time policy through explicitly
new public values and entrances.

## Public validation and rank-N token

The pure source is:

```haskell
LengthSMTLibLiveUsableWorkBudgetSource
  { lengthSMTLibLiveUsableWorkBudgetSourceMilliseconds :: Int }
```

`mkLengthSMTLibLiveUsableWorkBudget` accepts only a positive duration for which
both conversions used by the runtime are representable: milliseconds to the
host's `Int` microsecond wait and milliseconds to the `Word64` monotonic
nanosecond delta. It returns one of two exact pure failures:

```haskell
LengthSMTLibLiveUsableWorkBudgetNonPositive
LengthSMTLibLiveUsableWorkBudgetMicrosecondsOverflow
```

Validation reads no clock and performs no IO. The resulting
`LengthSMTLibLiveUsableWorkBudget` exposes neither its constructor nor a raw
duration projection.

`withLengthSMTLibLiveUsableWorkDeadline` samples the monotonic clock once and
lends an opaque `LengthSMTLibLiveUsableWorkDeadline budget` through a rank-N
callback. The nominal `budget` parameter is generative, so an absolute deadline
cannot escape as forgeable or reusable session authority. The owner checks the
deadline again when its callback returns normally.

## Coverage and minimum selection

`withLengthSMTLibLiveSessionUnderDeadline` consumes that token. Its effective
opening deadline is the earlier of:

- the captured shared absolute deadline; and
- a fresh local opener deadline derived from the established session policy.

Every scalar and product query likewise selects the earlier of the same shared
absolute deadline and its fresh configured per-query deadline. The shared
deadline wins an exact tie. When the configured local duration is shorter, the
runtime still compares the resulting absolute deadlines because time may have
elapsed since the shared capture.

The shared operational deadline covers:

- workspace allocation, retained-identity inspection, executable snapshot and
  hashing, direct launch, and the complete readiness capability probe;
- the wait for the one serial scalar/product query gate;
- query planning and identity admission around the controlled transaction;
- every causal write, bounded stdout/stderr wait, framing and response decode;
- independent scalar or product model replay; and
- final query-run identity sealing and commit.

A shared deadline is rechecked before the worker is lent and immediately after
the user session callback returns. The next live operation can therefore
observe an overrun before callback return, while a batch which performs only
caller work is still rejected at the normal-return boundary.

Final readiness, when the worker remains usable, receives a fresh established
opener-sized window. Durable process and workspace cleanup retains its existing
fresh bounded windows. Neither operation runs under the shared operational
deadline. `withLengthSMTLibLiveSessionWithUsableWorkBudget` deliberately omits
a second owner check after those stages, so their elapsed time alone cannot
turn its successful usable work into a shared-budget failure.

The general two-step owner necessarily has a broader normal-return boundary.
If its callback waits for a nested `withLengthSMTLibLiveSessionUnderDeadline`
call to finish, the outer owner check occurs after that nested call's fresh
final readiness and cleanup. It can therefore observe that the shared deadline
elapsed during those stages even though the stages themselves used only their
fresh private operational windows. This distinction is explicit rather than a
hard-deadline claim.

## Deferred-work and mixed-domain example

The two-step API exists for application work which must begin consuming the
window before a live session is opened. Here two deferred sealed queries are
forced after capture, then one scalar and one product transaction share the
same process, ordinal space, and absolute deadline:

```haskell
import Control.DeepSeq (force)
import Control.Exception (evaluate)

budget <- either (fail . show) pure $
  mkLengthSMTLibLiveUsableWorkBudget
    LengthSMTLibLiveUsableWorkBudgetSource
      { lengthSMTLibLiveUsableWorkBudgetSourceMilliseconds = 30000 }

owned <- withLengthSMTLibLiveUsableWorkDeadline budget $ \deadline -> do
  (scalarQuery, pairQuery) <- evaluate $ force
    (deferredScalarQuery, deferredPairQuery)
  withLengthSMTLibLiveSessionUnderDeadline
    deadline executionConfig $ \session -> do
      scalarResult <- runLengthSMTLibLiveQuery
        defaultLengthEvaluationLimits session scalarQuery
      pairResult <- runLengthSpinePairSMTLibLiveQuery
        defaultLengthEvaluationLimits session pairQuery
      pure (scalarResult, pairResult)

case owned of
  Left ownerFailure -> handleSessionFailure ownerFailure
  Right (Left sessionFailure) -> handleSessionFailure sessionFailure
  Right (Right (scalarResult, pairResult)) ->
    consumeNominalResults scalarResult pairResult
```

The force is application work, not a new Djex sealing entrance; it illustrates
that deferred pure preparation can be placed inside the generative owner. The
owner cannot preempt a nonterminating force, but it rejects normal return after
expiry. When there is no such pre-session work, the convenience entrance is:

```haskell
withLengthSMTLibLiveSessionWithUsableWorkBudget
  budget executionConfig $ \session -> do
    scalarResult <- runLengthSMTLibLiveQuery
      defaultLengthEvaluationLimits session scalarQuery
    pairResult <- runLengthSpinePairSMTLibLiveQuery
      defaultLengthEvaluationLimits session pairQuery
    pure (scalarResult, pairResult)
```

The session still admits at most 64 total transactions across both calls and
all later interleavings. A time budget neither raises that ceiling nor promises
that all 64 transactions fit.

## Callback, failure, and cleanup truth

No asynchronous timer thread throws into an arbitrary callback. Blocking
caller IO and nonterminating caller computation may continue beyond the
deadline until they return or perform another controlled live operation.
Synchronous and asynchronous callback exceptions remain authoritative: the
owner starts durable cleanup and rethrows the exception rather than replacing
it with an `Either` deadline value.

An expiry observed while constructing or closing the scope maps to the existing
sanitized `LengthSMTLibLiveSessionDeadlineExceeded`. An expiry returned directly
by `runLengthSMTLibLiveQuery` or
`runLengthSpinePairSMTLibLiveQuery` maps to that domain's existing byte-free
query deadline failure. If the shared deadline is then also expired when the
session callback returns, the enclosing session boundary can truthfully return
the session deadline failure. No child output, command, path, clock value, or
deadline cause crosses the public error boundary.

The cleanup-incomplete bit keeps its existing meaning and is independent of the
primary deadline class. A failure from the general deadline owner itself has no
owned process cleanup to report. The convenience session entrance retains the
actual nested session cleanup status.

## Additive identity distinction

Legacy sessions continue to use the exact v4 ready-worker identity and exact
scalar/product query-run schemas and roles. No shared-deadline field is appended
to the historical path.

A budgeted ready worker instead uses this outer role and schema field:

```text
finite-list-spine-length/z3-budgeted-capability-probed-ready-worker
djex-length-z3-shared-usable-work-deadline/v1
```

The envelope embeds the complete legacy ready-worker identity once, then binds
the validated duration, captured absolute deadline, workspace/launch/
capability/query coverage, minimum-selection and shared-on-tie rule, lack of
arbitrary callback interruption, callback-return check, and fresh final-
readiness/cleanup policy.

Budgeted scalar runs use:

```text
djex-length-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/shared-usable-work-deadline/v1
finite-list-spine-length/z3-live-query-run/shared-usable-work-deadline
```

Budgeted product runs use the nominally separate sibling:

```text
djex-length-spine-pair-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/shared-usable-work-deadline/v1
finite-binary-product-spine-lengths/z3-live-query-run/shared-usable-work-deadline
```

Each budgeted run retains its exact domain protocol and legacy authority fields,
then additionally binds the effective absolute query deadline, requested shared
duration, captured shared deadline, minimum-selection policy, and whether the
effective cause was the shared or fresh per-query deadline. A shared exact tie
is recorded as the shared cause. Scalar and product identities remain distinct
even when their commands or effective deadlines coincide.

## Authority limit

The budget proves only that Djex associated owned live operations with the
recorded monotonic deadline policy. It does not prove that the path observed
before spawn was the image executed, measure the dynamic loader or libraries,
attest Z3, validate solver soundness, or upgrade a solver status.

Every public status remains `HeuristicRankingOnly`. `unsat` is still relative
to the checked encoding and supplies no proof or pruning authority. A
satisfiable model can produce optional counterexample evidence only after exact
input decoding and independent replay against the exact scalar or product
behavioral problem. The public replay gate must still associate the opaque live
observation with that exact nominal query before releasing the receipt.
