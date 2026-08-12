# Shared SMT-LIB standard response boundary

Date: 2026-08-12

## Scope

Djex now has one package-private owner for the standard response facts shared
by solver-facing layers and future semantic domains:

- canonical exact bytes for `sat`, `unsat`, and `unknown`;
- bounded decoding of one standard `check-sat` response;
- the command-independent `unsupported` response; and
- the standard `(error "...")` response shape.

`Language.Haskell.Synthesis.Internal.SMTLib.Response.Standard` sits above the
existing transport-neutral bounded S-expression parser and below Length. It
introduces no schema tag, fingerprint, byte budget, process handle, query
association, or evidence authority.

That lack of a generic schema is deliberate rather than permission to drift.
Any future change to accepted bytes or classification must revise every
consuming domain response/plan schema identity. Length owns the response schema
today; the capability plan already binds its exact status bytes directly.

## Preserved precedence

The shared check decoder preserves the established ordering:

1. retain the entire response under the total byte bound;
2. parse exactly one bounded S-expression;
3. recognize exact simple-symbol `sat`, `unsat`, and `unknown`;
4. reject exact simple-symbol `success` as the wrong response kind;
5. classify `unsupported` or a two-field `(error <string>)`; and
6. reject every other valid shape as unexpected.

Thus `sat unsat` remains a trailing-expression syntax error, while `satjunk`,
`|sat|`, and `(sat)` remain unexpected check responses. Cyclic input still
stops at the configured maximum plus one. Solver-error message bytes remain
lazy after classification and were already admitted by response and token
bounds.

## Domain adapters

The Length response module exhaustively maps the closed shared failures into
its unchanged public compatibility constructors. It continues to own signed
limit validation, the Length response schema, query-specific valuation shape,
symbol restoration, integer decoding, and model replay boundary. In
particular, the unsolicited-value check still rejects before touching response
bytes.

The readiness capability does not call the shared parser. It imports only the
canonical `sat` and `unsat` bytes and continues to compare complete framed
responses exactly, reporting only its phase-specific sanitized mismatch. The
capability plan schema, fingerprint fields, and causal failure behavior are
unchanged.

## Validation

The new foundation tests cover all three canonical statuses with trivia,
standard unsupported and error responses (including doubled quotes,
backslashes, non-ASCII bytes, and a non-forced payload), `success`, unexpected
status shapes, trailing syntax, zero-byte admission, cyclic maximum-plus-one
productivity, and deep evaluation of every closed failure constructor.

The existing Length suite continues to pin its exact error vocabulary,
valuation precedence, response limits, protocol behavior, live status
branches, readiness mismatches, and canonical plan fingerprint bytes. No
public facade, wire command, identity schema, or trust claim changed.
