# Positive-literal natural quotient foundation

Date: 2026-08-13

## Outcome

The public finite-list-spine Length vocabulary now includes natural floor
quotient by a positive literal:

```haskell
LengthQuotient divisor expression
```

The divisor comes first, matching `LengthModulo`. It is a literal `Natural`,
not an expression. This supports bounded chunk counts, fixed-rate thinning,
floor ratios, and quotient/remainder laws without admitting variable
multiplication or nonlinear arithmetic.

## Semantic admission and normalization

The checked Length boundary applies the same productive ordering as modulo:

1. zero is rejected as `LengthQuotientDivisorZero` before the operand is
   inspected;
2. the divisor passes the shared literal-bit bound before operand traversal;
3. the operand is normalized under the shared syntax and variable bounds; and
4. only then is a literal operand folded with `Natural` `quot`, or quotient by
   one replaced with its already-validated operand.

Quotient by one therefore cannot conceal an invalid reference, cyclic term, or
over-budget child. Provider-to-candidate and result substitutions traverse the
new node. Solver-neutral replay uses exact `Natural` `quot`, retains the
intermediate-value check, and has the distinct defensive error
`LengthEvaluationInternalQuotientDivisorZero` for an impossible malformed
checked value.

The structural fingerprint tag is
`positive-literal-natural-quotient/v1`, with the divisor before the normalized
operand. Literal-folded and quotient-by-one contracts intentionally share the
canonical fingerprint of their existing normalized result. A surviving node
cannot collide with any earlier expression form.

## Shared Euclidean QF_LIA lowering

QF_LIA has neither the integer `div` nor `mod` operator. Quotient and modulo
now share a private Euclidean witness implementation. For every surviving
occurrence with operand `e` and positive literal divisor `k`, the typed plan
allocates integers `q` and `r` and asserts:

```text
q >= 0
r >= 0
r <= k - 1
e = k*q + r
```

Only multiplication by the literal `k` is emitted. The constraints select the
unique natural Euclidean pair; a quotient node projects `q`, while a modulo
node projects `r`. The translator never emits `(div ...)` or `(mod ...)`.

An occurrence reserves its global ordinal before translating its operand.
Nested and mixed quotient/modulo trees therefore use deterministic normalized
expression preorder even when an outer equation references an inner projected
symbol. All witness declarations precede input and witness assertions, and
witness constraints follow global ordinal order.

Quotient occurrences use private names
`djex_length_quotient_quotient_N` and
`djex_length_quotient_remainder_N`. They are never requested, decoded, or
treated as evidence. The optional `get-value` command still contains only the
original problem inputs, and independent replay recomputes the result from
those inputs.

## Errors, identity, and compatibility

The SMT admission surface distinguishes
`LengthSMTLibQuotientDivisorZero` and the dedicated numeral site
`LengthSMTLibQuotientDivisorNumeral`. A query containing quotient witnesses
adds the lowering-policy tag
`djex-length-z3-qf-lia-positive-literal-natural-quotient-witness/v1`.
Mixed queries carry both quotient and modulo lowering tags in a fixed order.

The shared implementation is private and additive. In particular, modulo-only
translation preserves all of the following byte-for-byte:

- `djex_length_modulo_quotient_N` and
  `djex_length_modulo_remainder_N` names;
- normalized-expression preorder and declaration/assertion order;
- `djex-length-z3-qf-lia-positive-literal-modulo-witness/v1`;
- canonical SMT-LIB scripts; and
- reversible structural query fingerprints.

The existing constant-zero and nested-modulo SHA-256 snapshots remain
unchanged. Query, protocol, execution, and response schema tags also remain
unchanged because the structural typed plan and exact command bytes already
bind the additive policy.

## Verification

Focused regressions cover:

- zero-divisor and divisor-bit error precedence without operand demand;
- quotient-by-one child traversal before reduction;
- literal and quotient-by-one normalization with fingerprint parity;
- provider transfer and result substitution;
- differential replay against direct `Natural` `quot` on small values;
- nested mixed witness names, global ordering, bounds, and equations;
- separate `q` and `r` projection in the final bad-state formula;
- absence of both SMT-LIB `div` and `mod` operators;
- input-only model requests and independent counterexample replay;
- conditional semantic and lowering-policy tags;
- the dedicated quotient divisor numeral limit; and
- the unchanged modulo-only script and canonical fingerprint snapshots.

This checkpoint changes the public Length expression and closed error
vocabularies. It does not change any pre-existing valid contract result,
modulo-only byte identity, solver-status authority, replay rule, causal
protocol, process policy, or live failure precedence.
