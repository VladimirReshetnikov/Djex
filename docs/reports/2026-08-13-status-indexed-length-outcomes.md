# Status-indexed Length SMT-LIB outcomes

Date: 2026-08-13

## Outcome

The pure Length protocol, completed live query run, and public live observation
now each own one strict status-indexed `SolverObservation` instead of retaining
a `SolverStatus` beside an optional values or evidence payload. The closed
branches make the valid combinations explicit:

- status-only satisfiable is `SatisfiableObservation Nothing`;
- satisfiable zero-input value policy is
  `SatisfiableObservation (Just [])` at the protocol edge;
- a framed satisfiable valuation is `SatisfiableObservation (Just bindings)`;
- a successfully replayed satisfiable run carries
  `SatisfiableObservation (Just evidence)`; and
- unsatisfiable and unknown observations contain only unit.

Consequently an unsatisfiable or unknown result cannot be paired with decoded
bindings or behavioral evidence inside any completed opaque Length owner. The
generic public `SolverObservation` constructors remain unchanged and their
payloads remain deliberately lazy.

## Protocol and replay authority

`LengthSMTLibProtocolDecoded` retains one strict
`SolverObservation (Maybe [LengthSMTLibIntegerBinding]) () ()`. The
input-value phase states no longer carry a redundant satisfiable status: those
states are reachable only after the exact `sat` check branch. The protocol
still forces the satisfiable `Maybe` spine before exposing a decoded result,
matching the former pair of strict fields without forcing the binding list.

The live Session consumes that whole decoded observation and produces a
private five-way replay outcome: status-only satisfiable, validated vacuous
satisfiable, validated framed satisfiable, unsatisfiable, or unknown. This one
transient owner feeds both the completed run observation and the existing
identity fields. Counterexample validation still completes before vacuous or
framed success is classified, so replay failures retain their previous demand
and failure precedence.

`LengthSMTLibQueryRun` retains one strict
`SolverObservation (Maybe (BehavioralEvidence FiniteListSpineLengthV1
ValidatedLengthCounterexample)) () ()`. Its constructor forces the
satisfiable `Maybe` spine, preserving the former optional-evidence demand, but
does not force the evidence payload beyond the established strict fields and
`NFData` behavior.

## Public boundary

`LengthSMTLibLiveQueryObservation` copies the complete private run observation
once rather than projecting and re-pairing status and evidence. The whole
observation remains unexported. Public callers still see only status, derived
heuristic strength, and heuristic use. The replay gate still compares the
exact query fingerprint before inspecting the hidden observation; only a
matching satisfiable observation with evidence can reach behavioral-evidence
replay. Status-only satisfiable, unsatisfiable, and unknown all return
`Right Nothing` after association succeeds.

The public generic observation API, Live type roles, error vocabulary, and
constructor opacity are unchanged. Negative API probes now also reject a whole
Live solver-observation projection, preventing the consolidated payload from
becoming a detached evidence edge.

## Stable identity and behavior

The query-run fingerprint remains version 1. The private five-way replay owner
emits the same ordered status/value tags and replay tags as before:

| branch | status tag | value tag | replay tag |
| --- | --- | --- | --- |
| status-only satisfiable | `satisfiable` | `absent` | `not-requested-policy` |
| vacuous satisfiable | `satisfiable` | `vacuous-zero-input` | `validated-counterexample` |
| framed satisfiable | `satisfiable` | `framed-input-values` | `validated-counterexample` |
| unsatisfiable | `unsatisfiable` | `absent` | `not-applicable-status` |
| unknown | `unknown` | `absent` | `not-applicable-status` |

Branch regressions independently encode those two fingerprint fields and
assert that each exact field occurs once inside the completed reversible run
key. The schema tag, field order, canonical bytes, admission arithmetic,
solver commands, framed wire bytes, transcript ownership, and public failure
order therefore remain unchanged.

This checkpoint changes no Cabal module list and does not scrub solver output.
Validated evidence still retains normalized source-ordered inputs, while the
private reversible run identity still embeds the exact bounded causal
transcript bytes.

## Verification

The Length suite exercises all five successful identity branches, including
status-only satisfiable, vacuous zero-input, framed unary and binary values,
unsatisfiable, unknown, split output, and drip output. Protocol assertions now
compare whole observations rather than detached fields. The API suite retains
the public generic-constructor/laziness contract and adds the negative whole
Live-observation projection probe.
