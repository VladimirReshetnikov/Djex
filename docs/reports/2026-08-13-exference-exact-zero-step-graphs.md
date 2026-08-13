# Checker-owned exact zero/step Exference graphs

Date: 2026-08-13

## Result

Exference now retains a typed graph for one closed nonempty constructor-case
shape needed by the finite-list-spine Length foundation. The independent
expression checker, not rendered source or search metadata, must prove all of
the following:

- the deconstructor is recursive and has exactly two constructors;
- one constructor has no fields;
- the other has exactly two fields, exactly one of which is the scrutinized
  recursive spine;
- the generated case has exactly those two direct alternatives; and
- scrutinee and result are the same checked spine type.

Every other nonempty case retains its prior `ExferenceTermGraphUnavailable`
result. In particular this change does not admit nominal one-constructor
matches, nonrecursive sums, differently shaped recursive data, incomplete
alternatives, nested field patterns, or a general `TypedCase` authority.

## Atomic schema authority

The checker stores an opaque draft containing the checked constructor names,
pattern type, ordered field annotations, and checked branch bodies. Graph
construction turns unused field binders into wildcards by inspecting the
checked branch, matching the established generated-expression cleanup.

The same draft supplies a private `TypeStructure` only to the atomic graph
seal. Repeated equal constructor observations are deduplicated; conflicting
field vectors resolve to no schema and fail closed. The sealed graph must erase
to the independently cleaned accepted expression before Exference returns it.
No constructor schema is exported or retained beside the opaque graph.

The shared public graph sealer and fingerprint remain unchanged. Length later
performs its own fresh seal against the checked session spine model, so the
frontend checker and behavioral domain independently establish the same
constructor association.

## Compatibility and bounds

Search scheduling, the multiple-constructor option, candidate ordering,
compatibility expressions, public types, and graph limits are unchanged.
Ordinary candidates keep their previous shared-structure seal. The additive
draft uses the existing node, edge, pattern, occurrence, and collection limits;
it introduces no new schema or fingerprint version because the structural
graph encoding already covered cases and constructor patterns.

## Validation

Focused engine tests prove both a manually checked case and a strict
no-unused-variable production-search candidate reach
`ExferenceTermGraphAvailable`, preserve exact
erasure, retain the recursive binder, and wildcard the unused payload. A
matching but nonrecursive deconstructor retains the historical
nominal-constructor absence.

The downstream Length and Leant checkpoints remain responsible for explicitly
selecting exact-case policy, resolving the concrete List family, verifying the
accepted renderer variant, and replaying any solver evidence. This checkpoint
alone does not grant behavioral meaning to a case graph. A Djex integration
regression now composes the public production surfaces end to end: it searches
a declared unary spine with Exference, structurally selects the retained
rebuild case, freshly seals exact Length authority from
`exferenceSessionInventory`, emits the QF_LIA query, validates model input 3,
and independently replays candidate result 3.
