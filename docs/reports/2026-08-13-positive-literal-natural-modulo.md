# Positive-literal natural modulo foundation

Date: 2026-08-13

## Outcome

The finite-list-spine Length dialect now has one genuinely new bounded
arithmetic operation:

```haskell
LengthModulo divisor expression
```

The divisor is a literal `Natural`, not an arbitrary expression. This admits
parity and fixed-congruence contracts while keeping the solver boundary in
linear integer arithmetic. It does not admit variable multiplication,
division, sequence contents, or nonlinear reasoning, and Leant does not yet
decode this constructor from either of its version-1 JSON formats.

## Semantic admission and normalization

The public source constructor keeps both fields lazy. The existing checked
Length boundaries are still the authority which makes it usable:

1. a zero divisor is rejected as `LengthModuloDivisorZero` before the operand
   is inspected;
2. the positive divisor passes the existing literal-bit bound before the
   operand is inspected;
3. the operand is normalized under the shared syntax and variable bounds; and
4. only then does normalization fold a literal operand or reduce every
   expression modulo one to zero.

This ordering prevents a simplification from hiding a malformed or unbounded
operand. Provider-to-candidate substitution and result substitution preserve
the constructor, after which the same joint-budget normalizer checks the
result. Solver-neutral evaluation uses exact `Natural` `mod` and retains the
existing intermediate-value bound. A defensive internal evaluation error and
a defensive SMT translation error close impossible raw-zero states without
changing any valid checked path.

The structural node is fingerprinted as
`positive-literal-natural-modulo/v1`, with the divisor before the normalized
operand. Literal-folded and modulo-one expressions therefore retain the exact
identity of their already-supported canonical result; remaining modulo nodes
cannot collide with an older expression form.

## QF_LIA witness lowering

The SMT-LIB QF_LIA logic does not include the `div` or `mod` operators. Djex
does not rely on Z3 accepting a larger language under `(set-logic QF_LIA)`.
For every remaining normalized occurrence `e mod k`, where `k > 0`, the typed
plan allocates private integer symbols `q` and `r` and asserts:

```text
q >= 0
r >= 0
r <= k - 1
e = k*q + r
```

Only multiplication by the concrete numeral `k` is emitted, so the result is
still QF_LIA. The positive divisor and remainder interval make `r` the unique
natural remainder. The SMT-LIB language boundary follows the official
[QF_LIA logic declaration](https://smt-lib.org/Logics/QF_LIA.smt2); the
corresponding integer division/remainder convention is documented by the
[SMT-LIB Ints theory](https://smt-lib.org/theories-Ints.shtml).

Witness ordinals are reserved in normalized-expression preorder, before an
occurrence's operand is translated. This makes nested allocation deterministic:
an outer occurrence receives its number before an inner occurrence, even
though the outer equation can reference the inner remainder. All witness
symbols are declared before any witness assertion, so this dependency is
well-formed and assertion order does not change its meaning.

The check script orders:

1. the unchanged fixed options, logic, and helper definitions;
2. original input declarations;
3. all quotient/remainder declarations;
4. original input nonnegativity assertions;
5. the four assertions for each witness in ordinal order; and
6. the original combined condition and `check-sat`.

The optional `get-value` request remains derived solely from the checked input
arity. Quotient and remainder witnesses are never requested, decoded, or
retained as evidence. Independent replay still consumes only normalized
source-ordered natural inputs and recomputes modulo through the solver-neutral
evaluator.

## Identity and compatibility

The fixed logic remains `QF_LIA`; the query, protocol, execution, and response
schema tags are unchanged. Rendered declarations and assertions already enter
both the exact check-command field and the structural typed plan. A modulo
plan additionally carries the conditional lowering tag
`djex-length-z3-qf-lia-positive-literal-modulo-witness/v1`.

The tag is absent when no witness survives normalization. Consequently every
pre-existing no-modulo command and reversible canonical query key stays
byte-for-byte identical, including the existing SHA-256 snapshot. Modulo
queries bind both the versioned semantic node and the versioned lowering
policy; no global schema bump needlessly invalidates older keys.

The command-byte, fingerprint-byte, numeral-bit, syntax-node, collection, and
evaluation bounds all remain active. Witness count is bounded by the admitted
normalized syntax tree. A divisor that exceeds the SMT numeral limit is
reported at its dedicated `LengthSMTLibModuloDivisorNumeral` site before the
operand is translated.

## Verification

Focused regressions cover:

- zero-divisor and divisor-bit failure precedence without operand demand;
- traversal of a cyclic operand before the modulo-one reduction;
- literal and modulo-one normalization plus canonical fingerprint parity;
- provider-transfer and result substitution;
- exact concrete replay across small natural values;
- nested preorder witness names, declarations, constraint order, and equations;
- the absence of an SMT-LIB `(mod ...)` term;
- input-only `get-value` despite private witnesses;
- conditional semantic/lowering tags and unchanged no-modulo query bytes; and
- the dedicated SMT divisor numeral bound.

This checkpoint changes the public Length expression vocabulary and its closed
error vocabularies. It changes no existing valid contract result, no-modulo
fingerprint byte, solver status/evidence rule, causal protocol, process policy,
or live failure precedence.
