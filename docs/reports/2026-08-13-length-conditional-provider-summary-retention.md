# Length conditional provider-summary retention

Date: 2026-08-13

## Outcome and scope

Djex Length can now retain an exact constrained provider scheme without
granting authority to use its assumed transfer. The additive public source
constructor is `AssumedConstraintConditionalProviderSummary`; its checked form
records `AssumedProviderLawConditionalOnConstraintDischarge` alongside the
source-derived scheme, argument roles, and normalized transfer.

This is a retention-only checkpoint. No candidate path or standalone evaluator
can apply a conditional law. Length does not call the package-private checked
class resolver, bind one of its discharge receipts to a provider occurrence,
or treat a Z3 result as dictionary evidence. Existing associated-certificate
obligation rejection also remains in force.

## Exact source-bound admission

Both provider-summary constructors pass through the same inventory-bound
sealing boundary. The claimed provider name must resolve in the retained exact
source inventory, and the claimed full scheme must be alpha-equivalent to the
closed scheme recovered from that declaration. The checked summary stores that
source-derived scheme rather than the caller's copy. Existing type, kind,
argument-role, transfer-syntax, and resource checks apply unchanged.

The constructors then select disjoint context policies:

- `AssumedProviderSummary` continues to require an empty leading constraint
  context and records `AssumedProviderLaw`;
- `AssumedConstraintConditionalProviderSummary` requires a nonempty leading
  constraint context and records conditional trust; and
- using the conditional constructor for a context-free scheme fails with
  `LengthProviderConditionalSchemeHasNoConstraints`.

Consequently, the new form cannot silently reclassify an ordinary provider.
Nor can the legacy constructor admit a constrained scheme: its existing
`LengthProviderConstrainedScheme` failure remains unchanged. In both cases the
entire provider scheme is closed, so the conditional context is retained as
part of the exact source authority rather than as a detached caller payload.

## Retention is not behavioral use

The trust classifier is checked at both currently reachable use boundaries.
`evaluateLengthProviderApplication` accepts only `AssumedProviderLaw`. For a
conditional summary it returns
`LengthEvaluationConditionalProviderRequiresDischarge` before inspecting
assignment arity, argument roles, or values. Thus a caller cannot use the
standalone evaluator to bypass candidate-local discharge.

Candidate interpretation likewise checks trust on the exact checked summary
carried from the session. A conditional occurrence returns
`LengthProblemConditionalProviderRequiresDischarge` with only its graph-local
term node and provider name; the transfer is not evaluated. This
applies to plain graphs and to a direct conditional occurrence in the graph
eventually projected from an opaque carrier associated for another provider.

Associated rows retain their stricter, earlier structural rule. A complete
certificate plan selects every leading source binder, so specializing a
constrained source activates its nonempty context. That row still fails with
`LengthProblemAssociatedCertificateActivatedObligations` during rooted row
authorization. A separate obligation-free association does not establish that
some other direct conditional occurrence was solved: the later trust check
still refuses that occurrence. Empty obligations are not a dictionary, an
instance choice, or a class-resolution receipt.

## Identity compatibility

The conditional trust distinction is encoded only in identities which describe
the provider assumptions being retained:

| Identity | Legacy-only inventory | Inventory containing a conditional summary |
| --- | --- | --- |
| provider inventory | exact version 2 bytes | version 3, including the conditional trust fields |
| semantic inventory | exact version 1 bytes | version 2, including the retention-only provider-constraint policy |

The legacy provider trust fields and normalized summary field order are
unchanged. Because the provider-inventory version is selected from the checked
summaries, adding the new constructor does not perturb an inventory containing
only `AssumedProviderSummary` values. The semantic-inventory version is selected
the same way, so all-legacy sessions also retain their exact existing bytes.

No encoding-policy, concrete-encoding, candidate, complete-problem, SMT query,
protocol, or execution identity schema or version changes in this checkpoint.
Those identities continue to compose exactly as before. In particular, an
enclosing problem or query still binds the semantic inventory through its
existing field, so its complete value naturally distinguishes a session which
actually retains conditional authority without introducing a new downstream
policy marker or schema version.

## Resolver and Z3 boundary

The package-private checked class-resolution foundation remains standalone.
Length neither supplies it the provider context nor retains its environment,
goal, proof, or discharge receipt. There is therefore no candidate-local
association between a resolved ground constraint and the provider occurrence
whose conditional transfer would depend on it.

The solver boundary remains orthogonal. A raw `sat`, `unsat`, or `unknown`
status is heuristic; decoded values can become model-relative Length evidence
only after independent replay against an already sealed problem. Neither path
constructs a Haskell dictionary or changes the conditional trust classifier.

## Public and test surface

The curated Djex facade exposes the additive source and trust constructors and
the sanitized sealing, standalone-evaluation, and candidate-use failures. The
checked summary itself remains opaque and exposes trust and the exact retained
scheme only through projections.

The facade smoke coverage constructs a real zero-parameter class and an exact
closed provider scheme with a nonempty context, seals the conditional summary,
checks its retained scheme and trust classifier, and confirms standalone
evaluation refusal. It also constructs the new candidate diagnostic, checks the
conditional-without-context sealer failure, and preserves the existing
obligation-rejection vocabulary. Focused Length tests own the deeper
fingerprint-compatibility, failure-precedence, candidate-path, and laziness
matrix.
