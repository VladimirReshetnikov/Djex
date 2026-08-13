# Derived Length raw-process identity

Date: 2026-08-12

## Outcome

The package-private Length process facade now retains one strict field: its
exact opaque `Z3SMTLibProcess`. It no longer caches a second strict
`FingerprintField` beside that process.

`lengthSMTLibProcessFingerprintField` derives the unchanged private identity
from the retained process's associated schema-free launch observation and its
process-owned admitted limits. The Length facade remains the sole owner of the
`djex-length-z3-raw-process/v2` schema, the
`length-z3-launched-transport` root, and the
`length-z3-process-limits/v1` wrapper. The generic runtime still owns no
domain schema.

## Association and exact bytes

The derived field has exactly the existing order:

1. the raw-process v2 schema bytes;
2. the generic process's associated snapshot, canonical working-directory,
   emptiness, PID, launch/runtime-method, and absolute-deadline observations;
3. the wrapper around that same process's associated limits.

Neither observation fields nor limits can be supplied independently to the
selector. Repeated projections from one live process are structurally equal.
The root, version, complete ordered field layout, platform-specific emptiness
tag, and all canonical payloads are unchanged. Ready-worker v4 and query-run
v1 identities therefore embed byte-for-byte identical raw-process fields; no
schema or fingerprint budget changes.

This removes cached structured authority, not canonical identity bytes. The
ready-worker and query-run reversible keys still retain this exact field.

## Demand and asynchronous exceptions

The old two-field constructor was strict in both the generic process and the
cached outer root. On a successful open, forcing the wrapper first demanded
the process and then the outer `FingerprintTag`; the ordered observation list
inside that tag remained lazy.

The one-field wrapper preserves that boundary explicitly. After the generic
opener returns successfully, while the Length facade's existing mask is back
in force, it:

1. constructs and forces the strict one-field Length wrapper;
2. transiently constructs and forces the derived root to weak head normal
   form; and
3. returns the wrapper while discarding that transient root.

There is still no asynchronous-exception window between successful generic
acquisition and the Length result. Initial cancellation, deadline,
working-directory, executable observation, hashing, pin, spawn, readiness,
and failure-mapping precedence are untouched. Projection later performs only
the same bounded outer construction and exposes the same lazy retained field
list; it performs no IO, control check, hashing, or process observation.

## Verification

The focused raw-process regression pins:

- repeated derived-projection equality;
- the exact Length root and v2 schema bytes;
- all 17 ordered fields, including the snapshot, canonical cwd,
  platform-specific emptiness method, admitted PID shape, every runtime-method
  tag, absolute deadline, and terminal process-limit wrapper;
- zero copies of the complete Length execution-policy key in the raw field;
  and
- unchanged cancellation and relative-working-directory non-demand
  precedence.

Validation completed successfully:

- `cabal test synthesis-length-tests --test-show-details=direct`: 183 of 183;
- `cabal test djex-api-tests --test-show-details=direct`: 34 of 34;
- `cabal build all --enable-tests`;
- `cabal check`; and
- `git diff --check`.

An independent read-only audit found no blocker in association, strictness,
masking, identity layout, tests, or documentation. This checkpoint changes no
public export, SMT-LIB byte, transcript, file format, failure vocabulary,
evidence rule, pruning authority, or Main behavior.
