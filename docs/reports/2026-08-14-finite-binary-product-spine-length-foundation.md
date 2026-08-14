# Finite binary product spine-length foundation

Date: 2026-08-14

## Outcome and boundary

Djex now has a solver-independent stage-one behavioral domain for a function
whose result is exactly a boxed binary product of two modeled finite spines:

```haskell
FiniteBinaryProductSpineLengthsV1
```

This is a nominal sibling of `FiniteListSpineLengthV1`, not a widening of that
domain. Its exact runtime tag is
`finite-binary-product-spine-lengths/v1`. It enables one checked postcondition
to relate the two source-ordered result lengths to each other and to the compact
modeled input lengths. Typical laws include conservation across a split,
duplication into both outputs, and swapping two input lengths.

The boundary is intentionally finite and explicit:

- the result must be `TupleType Boxed` with exactly two fields;
- each field must be the session's exact checked spine type;
- nested products, unboxed tuples, nonbinary tuples, and nonspine fields are
  rejected;
- input roles and compact input numbering retain scalar Length semantics;
- scalar spine providers and scalar exact zero/step cases may be used inside
  either result field when the session already authorizes them;
- no provider summary may return a product, and no case rule may have a
  product-valued scrutinee or result; and
- there is no product SMT-LIB query, model decoder, execution plan, protocol,
  worker, or live-Z3 orchestration in this checkpoint.

The stage nevertheless reaches authoritative behavioral replay: it includes
opaque product problems, independently replayed product counterexamples, and
positive bounded product receipts. No solver observation participates in
constructing any of that evidence.

## Contract language

`Language.Haskell.Synthesis.Semantic.Length` exposes the shared ordered carrier
and the product-specific contract vocabulary:

```haskell
data LengthSpinePair value = LengthSpinePair
  { lengthSpinePairFirst :: value
  , lengthSpinePairSecond :: value
  }

data LengthSpinePairComponent
  = LengthSpinePairFirst
  | LengthSpinePairSecond

data LengthSpinePairContractVariable
  = LengthSpinePairInput Natural
  | LengthSpinePairResult LengthSpinePairComponent

data LengthSpinePairContractSource = LengthSpinePairContractSource
  { lengthSpinePairContractPrecondition
      :: LengthFormula LengthSpinePairContractVariable
  , lengthSpinePairContractPostcondition
      :: LengthFormula LengthSpinePairContractVariable
  }
```

`LengthSpinePair` carries ordering but no checked or evidence authority.
`LengthSpinePairInput` uses the existing compact observed-role order.
`LengthSpinePairResult LengthSpinePairFirst` and
`LengthSpinePairResult LengthSpinePairSecond` are independently addressable in
the postcondition. Either result component in the precondition is rejected as
`LengthResultNotAvailableInPrecondition`; an out-of-range compact input retains
the existing `LengthInputReferenceOutOfRange` syntax rejection.

The four additive contract entrances are
`sealLengthSpinePairContract`, `sealLengthSpinePairContractInContext`,
`sealRoleAwareLengthSpinePairContract`, and
`sealRoleAwareLengthSpinePairContractInContext`. Their opaque result is
`CheckedLengthSpinePairContract`, with projections for the target, complete
source-ordered target-role vector, compact input count, normalized
precondition, normalized postcondition, and the nominal
`LengthSpinePairContractFingerprintSubject` fingerprint.

Contract sealing owns this fixed high-level order:

1. productively bound and normalize the target type;
2. check its kind under the exact inventory and reject a constrained target;
3. bound the physical argument list and validate the selected role vector;
4. require every observed argument to expose the checked spine model;
5. require a boxed tuple result of arity two;
6. require the first and then second result fields to expose that same spine;
7. normalize the precondition and postcondition under one shared syntax-usage
   budget; and
8. build the bounded product-contract fingerprint.

`LengthSpinePairContractError` is a closed sibling error surface. Adding this
domain therefore does not widen exhaustive matches over `LengthContractError`.

## Shared authority without nominal coercion

`sealLengthSpinePairContractInSession` derives the product contract's roles and
spine authority from an existing `CheckedLengthSession`. Product candidate
sealers likewise consume that same opaque session, so they reuse its exact:

- annotation-erased source inventory and inferred kind facts;
- checked built-in or declared spine schema;
- complete target-role and case policy;
- normalized context-free and constraint-conditional provider summaries;
- restricted ground class-resolution environment;
- final-node conditional-provider and protected-prefix authority; and
- construction and interpretation limits.

Reuse does not equate the evidence domains. The private product inventory
fingerprint is built as a new version-one structural envelope with role
`finite-binary-product-spine-lengths/semantic-inventory`. It contains the
product dialect tag, the canonical bytes of the exact scalar session semantic
inventory, and the explicit
`structural-wrapper-not-representational-coercion/v1` and
`same-checked-spine-model-and-provider-laws/v1` derivation policies.

Consequently two product problems can share authority only when their wrapped
scalar session inventory is exact. Evidence parameterized by
`FiniteListSpineLengthV1` can never replay as evidence parameterized by
`FiniteBinaryProductSpineLengthsV1`.

## Typed-candidate sealing

`Language.Haskell.Synthesis.Semantic.Length.Problem` exposes the opaque
`CheckedLengthSpinePairCandidate` and `CheckedLengthSpinePairProblem` siblings.
Their four construction entrances mirror the established interpretation
policies:

- `sealLengthSpinePairTypedCandidateProblem`;
- `sealRoleAwareLengthSpinePairTypedCandidateProblem`;
- `sealExactSpineCaseLengthSpinePairTypedCandidateProblem`; and
- `sealLengthSpinePairTypedCandidateProblemInSession`.

Compatibility, unified-session, residual-constraint, graph retention,
certificate association, root opening, kind, provider, conditional discharge,
and protected-prefix checks reuse the existing fail-closed scalar machinery.
They are translated into the closed `LengthSpinePairProblemError` vocabulary
before crossing the public product boundary.

After applying every physical target argument under the selected role policy,
the product-specific interpretation edge requires the final semantic value to
be one exact tuple. It then:

1. checks tuple arity without forcing a field;
2. forces the first field and requires a modeled spine;
3. forces the second field and requires a modeled spine;
4. normalizes the first symbolic length expression;
5. continues the same syntax-usage budget while normalizing the second; and
6. substitutes both normalized expressions into the product postcondition to
   produce one scalar-variable bad-state formula.

This order is semantically visible in failure precedence and in provider-law
collection. The final used-provider set is the canonical union across both
fields. A provider remains a scalar spine transfer, including an independently
ground-discharged conditional transfer at its authorized final occurrence. An
exact zero/step case remains a scalar-spine case and can occur as either tuple
field. The interpreter does not infer a product transfer or a product case from
the surrounding tuple.

The complete product problem has distinct fingerprints for:

- the structurally wrapped inventory;
- the exact boxed-pair contract and interpreted pair result;
- the candidate graph and its plain, obligation-free associated, or
  ground-discharged authority class; and
- the complete problem tuple.

The concrete encoding records both result expressions in first/second order,
the normalized counterexample condition, the exact used provider laws, and the
shared scalar session policy. Its interpretation policy explicitly records
boxed pair extraction, left-before-right extraction, scalar provider-law union
across fields, and rejection of product-valued case results.

## Solver-independent concrete replay

`Language.Haskell.Synthesis.Semantic.Length.Evaluate` exposes a closed product
replay surface. Detached contract classification accepts
`LengthSpinePairContractAssignment`, whose result is a caller-supplied
`LengthSpinePair Natural`, and returns the existing three-way
`LengthContractEvaluation`. It checks exact arity, input values left-to-right,
then the first and second result values before evaluating the checked
precondition and postcondition.

Whole-problem replay is authoritative and input-only:

```haskell
validateLengthSpinePairProblemCounterexample
  :: LengthEvaluationLimits
  -> CheckedLengthSpinePairProblem identity local
  -> LengthProblemAssignment
  -> Either LengthSpinePairEvaluationError
      (Maybe
        (BehavioralEvidence
          FiniteBinaryProductSpineLengthsV1
          ValidatedLengthSpinePairCounterexample))
```

The caller supplies no pair result. Replay checks the compact natural inputs,
evaluates the precondition before candidate expressions, recomputes whichever
result components the postcondition demands, and on a violation materializes
both results into the opaque receipt. `Nothing` means only that the assignment
did not violate the contract. The receipt projects exact inputs, the computed
ordered pair, and `LengthCounterexampleBasis`.

Provider-free replay records `ProviderIndependentFiniteSpineModel`.
Provider-backed replay records
`FiniteSpineModelUnderAssumedProviderLaws names`, where names are the canonical
used-law set collected across both fields. This remains a claim about the
versioned total finite-spine model under those explicit assumptions, not about
source-language bottoms, effects, inhabitance, strictness, or totality.

`LengthSpinePairEvaluationError` and
`LengthSpinePairEvaluationValueSite` are nominally separate closed
vocabularies. Product support therefore does not add constructors to
`LengthEvaluationError` or change scalar replay failure matching.

## Product-domain finite boxes

`validateLengthSpinePairProblemInputBox` reuses the opaque
`LengthInputBoxLimits`, the compact source-ordered inclusive maxima, and the
same lexicographic enumeration order with the last input varying fastest. It
returns the existing generic `LengthInputBoxValidation` classification, but
both payloads are product-domain evidence:

- `LengthInputBoxCounterexample` carries
  `BehavioralEvidence FiniteBinaryProductSpineLengthsV1
  ValidatedLengthSpinePairCounterexample`; and
- `LengthInputBoxValidated` carries
  `BehavioralEvidence FiniteBinaryProductSpineLengthsV1
  ValidatedLengthSpinePairInputBox`.

The product verifier retains scalar traversal precedence: problem width before
raw maxima demand, exact maxima arity before values, maxima left-to-right,
saturating Cartesian cardinality before the first assignment, then the first
evaluation rejection or violation by zero-based ordinal. A nullary box still
contains exactly `[]`. False-precondition assignments do not contribute to the
applicable count, and complete traversal is required before positive evidence
is constructed.

The positive receipt retains the exact maxima, assignment count, applicable
count, and provider/model basis under the product-specific verifier tag:

```text
finite-binary-product-spine-lengths/bounded-input-box-validation/v1
```

`LengthSpinePairInputBoxValidationError` is distinct because its indexed
value and evaluation failures carry `LengthSpinePairEvaluationError` rather
than the scalar error type.

Neither single-assignment replay nor finite-box traversal reads a raw status,
model, query, transcript, or live observation. Running this verifier after a
solver report would not make that report part of the evidence chain. Positive
box evidence is exact only for the enumerated box and its recorded provider
basis; it is not universal establishment or exact-pruning authority.

## Scalar compatibility and cache impact

This foundation is additive. Existing scalar APIs keep their exact signatures
and closed error vocabularies, including:

- `CheckedLengthContract`, `CheckedLengthCandidate`, and
  `CheckedLengthProblem`;
- `ValidatedLengthCounterexample` and `ValidatedLengthInputBox`;
- `LengthEvaluationError` and `LengthInputBoxValidationError`; and
- every scalar SMT-LIB, response, protocol, execution, worker, and live-Z3
  entrance.

The scalar domain tag, contract fingerprint, provider inventory, semantic
inventory, session encoding policy, candidate, concrete encoding, complete
problem, query, response, protocol, execution, and observation identities are
not versioned by this work. Product fingerprints use new nominal subjects,
roles, tags, and envelopes. Existing scalar canonical bytes and cache entries
therefore remain valid, while any future product cache must key on the new
product identities and verifier tag.

## Deliberately deferred work

This checkpoint prepares a normalized input-only bad-state formula but does not
translate or send it. A later additive stage may introduce a product-specific
QF_LIA query, response association, replay wrapper, finite-box query wrapper,
process execution, and live orchestration. That stage must continue to keep
raw `sat`, `unsat`, and `unknown` heuristic-only and must replay any decoded
inputs against the exact product problem before exposing evidence.

General products are also deferred. Supporting nested products, other tuple
arities, product-valued provider summaries, or product-valued case analysis
would require new checked shape descriptions, interpretation rules, identity
versions, replay carriers, and evidence domains rather than silently broadening
`FiniteBinaryProductSpineLengthsV1`.

## Characterization contract

Focused product coverage, together with the unchanged scalar-kernel regression
suite, exercises:

- boxed binary acceptance and unboxed, scalar, wrong-arity, nested, and
  nonspine-field rejection in fixed order;
- precondition result rejection, independent first/second postcondition
  references, relational formulas, normalization, and fingerprint bounds;
- exact session role reuse and case-policy/sealer rejection before graph demand;
- nominal separation of scalar and product domain, inventory, contract,
  candidate, encoding, complete-problem, counterexample, and bounded evidence;
- first-before-second tuple forcing, the closed result-shape error vocabulary,
  and joint syntax budgets;
- provider-free fields and shared scalar provider laws across fields; the
  unchanged shared-kernel suite continues to characterize exact scalar cases,
  conditional-provider authority, and rejection of product-valued cases;
- detached assignment replay, input-only whole-problem replay, precondition
  short-circuiting, first/second result limits, provider bases, and stale
  evidence association rejection;
- nullary and nonnullary product boxes, productive maxima admission,
  saturating cardinality, deterministic first violations, applicable counts,
  and product-specific verifier metadata;
- opaque constructors, nominal roles, deep `NFData`, and curated facade
  availability; and
- exact preservation of all established scalar APIs, errors, tags,
  fingerprints, canonical bytes, and Z3 behavior.
