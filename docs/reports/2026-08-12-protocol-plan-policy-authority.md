# Protocol-plan policy authority

Date: 2026-08-12

## Outcome

The package-private `LengthSMTLibProtocolPlan` no longer retains the complete
`LengthSMTLibExecutionConfig`. Sealing still consumes that complete policy in
the same order, renders the same writes, and constructs the same reversible
plan fingerprint. After the fingerprint succeeds, the plan retains only the
execution projections with later protocol consumers:

- the exact artifact policy which determines whether a satisfiable query has
  a value phase and whether a zero-input result is vacuous;
- the exact bounded response limits used by status and valuation decoding;
- the nominal checked query;
- the cumulative stream policy and positional barriers; and
- the complete plan fingerprint.

The Z3 launch profile, executable path and digest expectation, solver controls,
host deadline, environment and working-directory policy have no post-seal
operational consumer in the protocol machine. They are therefore no longer
retained as separate structured fields in every prepared, reserved, and
executing query plan. Their canonical identity remains nested in the complete
plan fingerprint.

## Identity and execution

`buildPlanFingerprint` still receives the complete execution configuration and
emits the identical execution-policy field in the identical position. The
protocol schema, fingerprint version and role, exact writes, response parsing,
phase transitions, and failure precedence are unchanged. Query-run identity
continues to bind the unchanged complete plan key.

The worker remains the owner of the complete execution policy because it must
derive each per-query host deadline and seal later plans. Once a plan exists,
response decoding and replay consume only values retained by that same plan;
neither operation pairs the plan with a detached worker-wide policy projection.

The new two-field policy retention preserves the former weak-head boundary.
The old execution configuration was a strict field whose artifact and response
members were themselves strict; the replacement fields are strict and are
projected only after successful policy validation and plan fingerprinting.

## Verification

Focused protocol regressions pin:

- the existing exact plan-fingerprint SHA-256 snapshot;
- fingerprint sensitivity to a launch-only execution field even though that
  field is no longer retained as a structured runtime-plan field;
- exact initial and optional value writes;
- artifact-policy projection from the sealed plan;
- nondefault response-token enforcement after sealing; and
- the existing status, valuation, barrier, cumulative-budget, EOF, and failure
  precedence matrix.

This checkpoint changes no public API, Cabal module list, SMT-LIB byte,
fingerprint byte, schema tag, error vocabulary, process behavior, evidence
rule, or Leant behavior.
