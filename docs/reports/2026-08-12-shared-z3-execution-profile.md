# Shared pure Z3 execution profile

Date: 2026-08-12

## Outcome

Djex now has one package-private, domain-neutral owner for the pure facts that
describe a direct Z3 SMT-LIB launch. The new module is:

`Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution`

It admits and retains:

- a bounded absolute executable path;
- an optional exact 32-byte SHA-256 executable-file expectation;
- the finite Z3 timeout and resource limit;
- the host-side absolute-deadline duration and required cleanup margin;
- the exact fixed argument prefix and the controls that determine the
  configured argument vector;
- the startup print-suppression command and per-query reset prefix;
- the empty child environment and fresh-working-directory policy; and
- the flat canonical fingerprint fields for those facts.

The profile is pure. It performs no path resolution, file observation,
hashing, spawn, capability probe, stream framing, or solver query. Its optional
digest remains only an expectation for the later live opener.

## Authority split

The shared profile does not own a behavioral protocol schema, response
grammar, artifact policy, fingerprint, or fingerprint budget. In particular,
both existing Length schema tags remain Length-owned. The complete Length key
is still built with version 1 and role `length-z3-execution-policy` as:

1. the Length execution-policy schema;
2. the Length session-protocol schema;
3. the eleven shared Z3 launch fields, spliced flat in their established
   order;
4. the Length artifact policy; and
5. the Length response schema and five response bounds.

No generic tag or nested sequence was inserted. There is still one Length
fingerprint admission pass and one complete reversible Length identity.

`LengthSMTLibExecutionConfig` now retains the admitted Z3 profile, Length
artifact policy, Length response policy, and complete Length fingerprint.
Its public source, errors, constants, selectors, equality, and abstraction
boundary are unchanged. Package-private process code may inspect the profile;
downstream callers cannot.

## Validation and demand order

The shared constructor preserves the previous fixed order:

1. solver timeout;
2. solver resource limit;
3. host deadline, including microsecond conversion and timeout margin;
4. bounded path characters, followed by finite empty/absolute checks; and
5. exact digest length.

Only after those checks does Length traverse the complete canonical key. Path
and digest payloads remain lazy fields inside the admitted profile; the three
validated numeric controls remain strict. The Length wrapper retains that
profile strictly, followed by its artifact policy, response limits, and full
fingerprint, preserving the old successful-construction and `NFData` demand.

The existing maximum-plus-one behavior is unchanged. A path fails at its first
cell beyond the configured bound without inspecting that character. A digest
fails at its 33rd cons without inspecting the excess byte. Public errors map
exhaustively from the closed generic error vocabulary and retain no path or
digest bytes.

## Live use

The shared raw Z3 process owner now accepts only the shared profile. It binds
requested and canonical paths, metadata, observed digest and byte count, the
exact pin result, configured argv, empty environment, requested and canonical
cwd, process flags, the absolute deadline, and process limits to one opaque
runtime. Its schema-free observation slice excludes the strict limits retained
alongside it. It owns no domain identity schema. The Length Process facade
seals both associated field groups under the raw-process v2 root without
accepting or embedding the complete Length execution policy.

The live Session retains the complete Length wrapper. Its ready-worker v4
identity binds the complete policy key once, immediately beside the raw
process field. Artifact and response policy therefore first enter live
identity at the Session owner which actually needs the complete domain policy,
not at the lower launch transport.

The raw-process and ready-worker versions distinguish this representation.
Query-run schema remains unchanged because it embeds the already-versioned
ready-worker key. The removed duplicate shortens both ready-worker and
transitive query-run canonical keys. A custom ready-worker or query-run
identity-byte budget near the former size can newly admit the same policy;
no previously admitted value becomes oversized. This is intentional and is
not hidden with padding or a redundant preflight.

The pure Length protocol and readiness capability use the generic reset and
startup command bytes. They retain their Length-specific plans, schemas,
queries, markers, phase machines, fingerprints, and causal write ownership.
Wire bytes and plan identities are unchanged.

## Compatibility checks

The focused suite now pins:

- all shared constants, environment, limits, projections, configured argv,
  and the exact eleven-field slice width;
- the old public Length validation errors and maximum-plus-one behavior;
- poisoned later source fields at every validation boundary;
- the complete 687-byte pinned Length execution key and its SHA-256 snapshot;
- every configurable Length field's identity sensitivity;
- the unchanged protocol-plan fingerprint snapshot;
- the live five-argument argv, empty environment, fresh cwd, digest pin, and
  pre-spawn rejection behavior; and
- cancellation and invalid-cwd precedence before launch-profile demand, the
  raw-process v2 and ready-worker v4 tags, zero complete-policy copies in the
  raw process field, and exactly one in ready-worker identity; and
- the downstream absence of path, digest, generic-profile, and reversible-key
  projections.

This successor changes only private raw-process and ready-worker fingerprint
schemas and their transitive private keys. It introduces no public API, Cabal
exposed module, Main behavior, SMT-LIB wire schema, file format, solver
evidence, or pruning authority.
