# Shared typed QF_LIA foundation

Date: 2026-08-13

## Outcome

Djex now has one package-private, domain-neutral typed foundation for the
quantifier-free linear integer arithmetic fragment used by semantic SMT-LIB
translators:

```text
Language.Haskell.Synthesis.Internal.SMTLib.QFLIA
```

It owns:

- typed integer expressions;
- typed Boolean expressions;
- typed commands;
- the exact `QF_LIA` logic bytes;
- canonical SMT-LIB rendering; and
- structural `FingerprintField` projections for the same syntax.

The first consumer is the finite-list-spine Length translator. The extraction
is package-internal and does not add or change a public API.

## Boundary

The shared syntax covers natural numerals embedded in integer expressions,
symbols, sums, literal scaling, differences, conditionals, equality,
less-than-or-equal, negation, conjunction, declarations, assertions, fixed
binary integer definitions, satisfiability checks, and value requests.

A named binary application retains its exact function-symbol bytes. This lets
a domain own helper selection and definitions without teaching the shared AST
about Length's monus, minimum, or maximum policy. The historical structural
field tag for that node remains `helper`; changing it to a cosmetically newer
name would change every affected query identity.

The shared module does not know about:

- Length expressions, formulas, contracts, or checked problems;
- helper selection or generated symbol names;
- quotient/modulo Euclidean projections or witness ordering;
- query command, numeral, or fingerprint limits;
- solver processes, response decoding, or execution policy;
- behavioral-problem fingerprints; or
- concrete model validation and independent replay.

Those responsibilities remain in the Length and solver-execution layers. This
keeps the reusable module a typed serialization foundation rather than a
second semantic authority.

## Canonical rendering and identity

Rendering and structural fingerprinting are independent projections of one
typed value. Rendering retains the established special cases:

- an empty integer sum renders as `0`;
- a singleton integer sum renders as its child;
- an empty conjunction renders as `true`;
- a singleton conjunction renders as its child; and
- every command has exactly one trailing line feed.

Structural fingerprinting deliberately retains the empty or singleton
`sum`/`all` node even when its rendered bytes collapse. Thus rendered text
does not become the semantic source of truth. All historical field tags and
nesting remain unchanged, including `define-int2`, `declare-int`, `at-most`,
`get-value`, and `helper`.

`lengthSMTLibQueryLogic` remains the same public Length constant, now defined
from the shared exact `qfliaLogicBytes`. No query, protocol, execution, or
response schema version changes: relocation alone changes no serialized
meaning.

## Compatibility evidence

Length still constructs its preamble and commands in the same order. It still
allocates quotient and modulo witness ordinals before operand translation,
uses the same operation-specific names and lowering tags, checks limits in the
same order, requests only original input symbols, and replays models against
the exact retained problem.

Existing regressions continue to pin:

- the complete constant-zero script and SHA-256 snapshot of the
  collision-free canonical query-key bytes;
- nested modulo witness bytes and the corresponding SHA-256 snapshot;
- mixed quotient/modulo witness names, equations, and order;
- the SHA-256 snapshot of the complete collision-free canonical protocol-plan
  key bytes; and
- public API availability and independent counterexample replay.

The extraction therefore preserves exact scripts and fingerprint bytes while
removing duplicate infrastructure from the Length-specific module.

## Focused verification

New package-private tests exercise every integer, Boolean, and command
constructor through both canonical rendering and structural fields. They also
prove that render-equivalent empty/singleton sums and conjunctions remain
structurally distinct.

The full Length suite runs these neutral tests beside all existing script,
fingerprint, protocol, live fake-Z3, and replay regressions. Strict builds keep
the shared module subject to the same warning policy as the rest of Djex.
