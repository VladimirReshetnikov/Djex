# Djex checker-boundary convergence follow-up

Date: 2026-07-17

## Scope and method

This follow-up reviewed `djinn/` and `exference/` after the whole-tree
convergence pass, concentrating on places where an exposed compatibility API
can bypass a checked session. The main target was Exference's independent
generated-expression checker: unlike normal search, it accepts raw function,
deconstructor, class, goal, constraint, and expression values directly.

The review traced each invariant from live search sealing to independent
checking:

- identity uniqueness for functions, constructors, and datatype eliminators;
- lexical validity of global expression and constructor-pattern names;
- every explicit environment constraint, including class superclasses and
  instance heads and prerequisites;
- rank restrictions in used and unused capabilities;
- deconstructor nominal shape and field-variable scope; and
- local scope and syntax in the final shared generated-expression tree.

The Djinn and Exference checked adapters were compared again at their session,
request, result, and rendering boundaries. Partial-looking calls, outstanding
research notes, and the bounded lazy-hint trust boundary were also inspected
to distinguish actionable input bugs from deliberate invariant sentinels or
backend policy.

## Correctness findings resolved

1. **Raw checker lookup was list-order dependent.** Duplicate functions,
   constructors, or datatype eliminators could reach inference, where the first
   matching list element silently won. Search already rejected those inputs.
   `EnvDictionary` now owns one complete identity validator used by both
   consumers, with deterministic error ordering and sorted diagnostics.
2. **Rank validation covered only values reached by inference.** An unused
   rank-N function or datatype field could remain in the raw checker
   environment. The same gap existed in static class superclasses, explicit
   instance heads/prerequisites, expected constraints, query assumptions, and
   generated local annotations. The complete environment-constraint projection
   now lives beside `EnvDictionary`, and checker preflight scans all of these
   sites before inference.
3. **Generated syntax and scope were not a checker invariant.** Malformed
   unused global names could pass checking even though search refused to seal
   them. Repeated pattern-local identities were more serious: successive
   `IntMap.insert` calls silently replaced an earlier field binding, allowing a
   non-renderable pattern to be certified. Environment syntax validation is
   now shared with search, while the checker validates the final shared
   `Generated.Expression` for scope and syntax before reconstructing types.

Eight focused test cases cover the new behavior, bringing the Exference unit
suite from 361 to 369 tests. The cases exercise all three duplicate identity
classes, unused binding and datatype rank-N capabilities, class/instance
constraints, generated annotations, malformed unused names, and duplicate
pattern binders.

## Consolidation and simplification

The fixes remove validation ownership from the large search implementation.
`Language.Haskell.Exference.Core.FunctionBinding`, already the home of
`EnvDictionary` and deconstructor-shape validation, now provides the reusable
projections and typed checks:

- `validateEnvironmentBindingIdentities`;
- `validateEnvironmentBindingSyntax`;
- `environmentConstraints`; and
- `validateDeconstructorBinding`.

Live search translates those typed failures into its historical
`ExferenceInputError` vocabulary. The independent checker reports the same
underlying failures through `ExpressionCheckError`. Neither implementation now
reconstructs its own incomplete view of the raw environment.

The broader adapter comparison found the intended uniform architecture intact:
both backends have private session and request invariant owners, consume the
shared declaration/type/query/generated-term vocabulary, and publish the same
result envelope. A further mechanical merger would add abstraction without
removing policy because the remaining core differences are semantic.

## Deliberate differences retained

- Djinn's terminating LJT proof search and Exference's bounded heuristic search
  have different evidence and completion meanings.
- Exference still omits recursive datatype elimination with a structured
  reason. Supporting it requires a termination policy, not reuse of Djinn's
  proof rule.
- Djinn's stored canonical binder spellings and Exference's numeric local
  identities remain observable compatibility contracts.
- The narrow bounded evaluation of caller-owned lazy Exference name hints
  remains the documented `unsafePerformIO` boundary. Its tests cover cyclic,
  exceptional, and overlong inputs while preserving asynchronous exceptions.
- Remaining explicit `error` calls in production search code are strictness
  probes or sealed-invariant sentinels; replacing them with input recovery
  would hide an internal contract violation rather than make the public API
  safer.

## Validation

The final tree passed the complete release handoff on GHC 9.12.4:

```console
cabal build all
cabal test all -j1 --test-show-details=direct
cabal build all --ghc-options='-Werror -Widentities -Wincomplete-patterns -Wincomplete-record-selectors'
cabal check
cabal haddock lib:djex --haddock-hyperlink-source
cabal sdist
```

All eleven test suites passed. `cabal check` reported no package errors or
warnings, Haddock produced the library documentation, and `cabal sdist`
produced `djex-2026.7.17.tar.gz`.
