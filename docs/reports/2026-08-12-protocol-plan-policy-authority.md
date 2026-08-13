# Protocol-plan policy authority

Date: 2026-08-12

## Outcome

The package-private `LengthSMTLibProtocolPlan` no longer retains the complete
`LengthSMTLibExecutionConfig`, and its sealer no longer accepts that structured
pre-launch authority. Sealing receives the narrower strict post-launch policy,
renders the same writes, and constructs the same reversible plan fingerprint
from its original complete execution key. After the fingerprint succeeds, the
plan retains only the execution projections with later protocol consumers:

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

`buildPlanFingerprint` now receives the opaque post-launch policy and projects
the original complete execution fingerprint object. It emits the identical
execution-policy field in the identical position. The protocol schema,
fingerprint version and role, exact writes, response parsing, phase transitions,
and failure precedence are unchanged. Query-run identity continues to bind the
unchanged complete plan key.

The worker retains only the host deadline, artifact policy, response limits,
and original complete key needed to derive each per-query deadline and seal
later plans. It does not retain the structured Z3 launch profile. Once a plan
exists, response decoding and replay consume only values retained by that same
plan; neither operation pairs the plan with a detached worker-wide policy
projection.

The post-launch policy is a strict four-field data owner. It is constructed only
after ready-worker identity admission from the already validated deadline,
artifact policy, response limits, and original strict fingerprint. Protocol
sealing still observes artifact policy before framing validation and performs
fingerprint admission at the same point, so failure precedence is unchanged.

## Verification

Focused protocol regressions pin:

- the existing exact plan-fingerprint SHA-256 snapshot;
- parity of all four post-launch projections with their admitted source;
- fingerprint sensitivity to a launch-only execution field even though that
  field is no longer retained as structured post-ready authority;
- exact initial and optional value writes;
- artifact-policy projection from the sealed plan;
- nondefault response-token enforcement after sealing; and
- the existing status, valuation, barrier, cumulative-budget, EOF, and failure
  precedence matrix.

The complete key remains a reversible canonical value and still contains its
original policy bytes; this is structured-authority removal, not byte
scrubbing. This checkpoint changes no public API, Cabal module list, SMT-LIB
byte, fingerprint byte, schema tag, error vocabulary, process behavior,
evidence rule, or Leant behavior.
