# Finite binary product spine SMT and replay

Date: 2026-08-14

## Outcome and boundary

Djex now has the pure offline QF_LIA query and replay stage for the checked
`FiniteBinaryProductSpineLengthsV1` behavioral domain. One opaque
`CheckedLengthSpinePairProblem` can be sealed as one opaque
`LengthSpinePairSMTLibQuery`; the query exposes canonical check bytes, exact
input-symbol order, an optional canonical input-only `get-value` request, its
complete product-query fingerprint, and its retained product behavioral
problem.

This is the stage immediately after the solver-independent product foundation.
It deliberately does not add a product response parser, execution plan,
protocol plan, process profile, worker, ready-worker lease, query run, or live
Z3 facade. The canonical bytes can be consumed by an external offline driver,
but a status returned by that driver has no behavioral authority. Raw `sat`,
`unsat`, and `unknown` remain observations rather than evidence.

The only ways exposed here to obtain a product receipt are:

- independently decode and replay the exact query's returned input bindings;
- freshly replay source-ordered natural inputs through the query-owned checked
  problem;
- freshly probe the query-owned all-zero assignment; or
- exhaust a separately bounded finite input box through the domain-owned
  evaluator.

No solver status participates in any of those paths.

## Public offline query surface

`Language.Haskell.Synthesis.Semantic.Length.SMTLib` exports the following
additive product-query identity and construction surface:

```haskell
LengthSpinePairSMTLibQueryFingerprintSubject
lengthSpinePairSMTLibQuerySchemaTag
lengthSpinePairSMTLibQueryLogic
LengthSpinePairSMTLibQueryError (..)
LengthSpinePairSMTLibQuery
sealLengthSpinePairSMTLibQuery
lengthSpinePairSMTLibQueryInputSymbols
lengthSpinePairSMTLibQueryCheckBytes
lengthSpinePairSMTLibQueryInputValueRequestBytes
lengthSpinePairSMTLibQueryFingerprint
lengthSpinePairSMTLibQueryBehavioralProblem
```

The construction entrance has the exact shape:

```haskell
sealLengthSpinePairSMTLibQuery
  :: LengthSMTLibLimits
  -> CheckedLengthSpinePairProblem identity local
  -> Either
      LengthSpinePairSMTLibQueryError
      (LengthSpinePairSMTLibQuery identity local)
```

The query constructor is private and both phantom roles are nominal. Callers
cannot splice canonical bytes onto a different checked problem, substitute a
scalar problem, or manufacture a product query by coercing a scalar one.
`NFData` forces the retained checked problem, bounded check bytes, and complete
query fingerprint.

The product translator reuses the existing checked `LengthSMTLibLimits`.
Command-byte, structural-fingerprint-byte, and numeral-bit bounds therefore
retain their established semantics and validation order. Construction failures
are translated into the closed nominal product vocabulary:

```haskell
LengthSpinePairSMTLibUnexpectedResultVariable
LengthSpinePairSMTLibInputVariableOutOfRange
LengthSpinePairSMTLibQuotientDivisorZero
LengthSpinePairSMTLibModuloDivisorZero
LengthSpinePairSMTLibNumeralBitLimitExceeded
LengthSpinePairSMTLibCommandByteLimitExceeded
LengthSpinePairSMTLibFingerprintByteLimitExceeded
```

Sharing the resource-policy carrier and enums does not widen the scalar
`LengthSMTLibQueryError` or let a scalar construction failure stand for
product authority.

## Input-only QF_LIA lowering

The checked product problem already retains the normalized bad-state formula
formed by substituting the interpreted first and second candidate-result
expressions into the relational contract. Consequently product query sealing
translates a formula over compact input variables only. It does not declare,
request, or decode either modeled result component.

The product logic projection is exactly:

```text
QF_LIA
```

Its exact schema tag is:

```text
djex-length-spine-pair-z3-qf-lia-smtlib2/v1
```

The shared typed lowering kernel emits the same fixed model-producing option,
random seed, integer input declarations, input nonnegativity assertions,
normalized counterexample assertion, and `check-sat` command order used by the
scalar Length translator. Positive-literal natural quotient and modulo retain
the shared deterministic Euclidean witness lowering and never introduce
SMT-LIB `div` or `mod`.

Generated compact input symbols remain source ordered:

```text
djex_length_input_0
djex_length_input_1
...
```

`lengthSpinePairSMTLibQueryInputValueRequestBytes` requests exactly those
symbols and no private Euclidean witness or modeled result. A nullary problem
has no input symbols and returns `Nothing` for the value request. The symbol
list and optional request bytes are canonically rederived from the checked
problem's sealed arity after sealing has already bounded and structurally
fingerprinted the corresponding typed plan and rendering.

This design permits a product query and scalar query to have byte-identical
check programs when their substituted input-only bad states coincide. Byte
equality is an encoding fact, not semantic or evidence identity.

## Nominal product query identity

Product queries use the distinct structural fingerprint role:

```text
finite-binary-product-spine-lengths/z3-qf-lia-query
```

The version-one query fingerprint binds:

- the product query schema and QF_LIA logic;
- the fixed model and random-seed options;
- the exact product behavioral domain;
- the structurally wrapped product inventory fingerprint;
- the checked product encoding, candidate, and complete-problem fingerprints;
- the complete typed QF_LIA plan, including ordered input symbols, condition,
  commands, optional request, and any Euclidean lowering policy tags;
- the retained canonical check bytes; and
- the absent or present canonical input-value request bytes.

The fingerprint subject, role, schema, embedded behavioral problem, and
complete problem are all product specific. A scalar and product query therefore
cannot share a query key or evidence association even when their check and
value-request bytes match exactly. This separation continues the stage-one
rule that the product inventory structurally wraps scalar-session authority
rather than representationally coercing scalar evidence.

## Decoded input-model replay

The decoded-model surface is:

```haskell
LengthSpinePairSMTLibModelError (..)
validateLengthSpinePairSMTLibCounterexample
```

The validator deliberately reuses public
`LengthSMTLibIntegerBinding` as an untrusted parser-decoded symbol/integer
carrier. That carrier has no query, domain, result, solver, or evidence
authority. The product validator performs these checks before evidence can be
constructed:

1. observe exact binding-list arity productively;
2. retain every symbol only within the generated-symbol byte bound;
3. reject an unknown input symbol;
4. reject a duplicate input symbol;
5. reject a negative integer before converting it to a natural;
6. require every expected input symbol;
7. restore the exact source input order; and
8. independently replay the ordered inputs through the checked product problem.

The closed rejection vocabulary distinguishes arity, symbol bounds, unknown,
duplicate, negative, and missing bindings from
`LengthSpinePairSMTLibCounterexampleReplayRejected`, which carries the exact
product evaluation failure. The caller never supplies either result length.
Replay recomputes candidate semantics and, only for an actual violation,
materializes both ordered result lengths in a fresh
`ValidatedLengthSpinePairCounterexample` receipt.

The successful return remains wrapped as exact product-domain
`BehavioralEvidence` so its domain, inventory, encoding, candidate, and
complete-problem association can be replayed or rejected in the established
fixed order. A syntactically valid model that is not a semantic
counterexample returns `Nothing`; `sat` alone cannot change that result.

## Query-owned direct replay and origin probe

Callers that already hold source-ordered natural inputs need not reconstruct
SMT symbols or integer bindings. The direct entrance is:

```haskell
replayLengthSpinePairSMTLibCounterexampleInputs
  :: LengthEvaluationLimits
  -> LengthSpinePairSMTLibQuery identity local
  -> [Natural]
  -> Either LengthSpinePairSMTLibInputReplayError
      (Maybe ValidatedLengthSpinePairCounterexample)
```

The query privately owns the exact checked product problem and behavioral
association. Every call evaluates afresh, constructs new evidence only for a
violation, and immediately replays it against that same product problem before
releasing the opaque receipt. It retains no cached verdict and accepts no
solver observation. The closed error separates a product evaluation rejection
from a sanitized generic association mismatch.

`probeLengthSpinePairSMTLibCounterexampleAtOrigin` is the canonical all-zero
specialization. It derives the compact arity from the problem retained by the
query and supplies exactly one zero per input. A nullary query probes `[]`.
`Just receipt` is an ordinary freshly validated counterexample; `Nothing`
means only that this assignment was not a violation. A miss is not bounded
positive evidence and says nothing about any other assignment.

## Query-owned finite boxes

`validateLengthSpinePairSMTLibQueryInputBox` accepts ordinary
`LengthEvaluationLimits`, independent `LengthInputBoxLimits`, one product
query, and one inclusive natural maximum per compact input. It delegates all
enumeration to `validateLengthSpinePairProblemInputBox` and uses the query only
as exact association authority.

The result remains the generic `LengthInputBoxValidation` classification with
product receipts:

- `LengthInputBoxCounterexample` releases the first independently replayed
  `ValidatedLengthSpinePairCounterexample`; or
- `LengthInputBoxValidated` releases a
  `ValidatedLengthSpinePairInputBox` only after complete traversal.

Admission, productive arity checking, maximum-value checking, saturating
Cartesian cardinality, lexicographic enumeration, first-error precedence, and
applicable-assignment counting are unchanged from the solver-independent
product verifier. Both evidence forms are replayed against the product
behavioral problem projected from the exact query before their receipts are
released.

The wrapper emits no command and consumes no raw model, status, transcript, or
live observation. In particular, calling it after an external `unsat` report
does not certify that report. Positive success establishes only the explicitly
enumerated finite box under the receipt's versioned total-spine model and
explicit provider-law basis. It is neither universal proof nor exact pruning
authority.

## No live product worker yet

The scalar Length stack already has bounded response parsing, protocol and
execution identities, process management, ready-worker authority, query-run
association, live observations, and sanitized replay. This checkpoint does not
generalize any of those layers to product queries.

In particular, there is no public product counterpart yet for the scalar live
query facade. An application must not pass product bytes through a scalar
worker and treat the scalar response, query-run identity, or live observation
as product authority. A future live product stage must add nominal response,
execution, protocol, worker, query-run, and observation identities, preserve
framing and resource bounds, and retain the rule that only independent input
replay can release counterexample evidence.

## Scalar compatibility and cache impact

This stage is additive. It does not change any historical scalar:

- public constructor, signature, closed error vocabulary, or nominal role;
- contract, provider-inventory, semantic-inventory, session-policy, candidate,
  concrete-encoding, or complete-problem identity;
- SMT query schema, structural fingerprint fields, canonical check bytes,
  input symbols, or value-request bytes;
- response, protocol, execution, process, worker, query-run, or live-observation
  identity; or
- decoded-model, direct-input, origin-probe, input-box, or live replay
  semantics.

The shared QF_LIA kernel and `LengthSMTLibIntegerBinding` representation are
reused without widening scalar authority. Product additions have new names and
nominal subjects. Existing scalar cache keys and canonical artifacts therefore
remain byte-valid; a product query cache must use the new product query
fingerprint and must not key solely on rendered SMT-LIB bytes.

## Characterization boundary

Focused coverage characterizes:

- the exact product schema, QF_LIA logic, ordered input symbols, canonical
  check bytes, and input-only value request;
- byte equality with an equivalent scalar script together with strict query
  fingerprint inequality;
- lowering of relations involving both ordered result components without a
  result symbol escaping into SMT-LIB;
- decoded binding validation and independent recomputation of both results;
- agreement among decoded-model replay, direct natural-input replay, and the
  all-zero probe;
- rejection of stale product problems and nominally forged scalar evidence;
- query-associated positive and counterexample finite boxes;
- nullary provider-backed replay with the unioned provider-law basis;
- productive malformed-model and over-arity rejection;
- command, numeral, and fingerprint resource precedence through the closed
  product query errors;
- opacity, nominal roles, deep evaluation, and curated public exports; and
- exact preservation of the established scalar query golden bytes, API, and
  replay behavior.

The earlier
[finite binary product spine-length foundation report](2026-08-14-finite-binary-product-spine-length-foundation.md)
records the solver-independent stage and its deliberately deferred query seam.
This report records the additive offline query/replay stage that fills that
seam while leaving live product execution for a later checkpoint.
