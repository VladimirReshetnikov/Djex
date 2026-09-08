# Constraint-only provider inference from lexical assumptions

**Later frontend follow-up:** [loaded ordinary provider schemes](2026-09-08-loaded-contextual-providers.md)
now pass explicit-forall behavioral synthesis and independent displayed-source
replay in both engines. The original engine/API results and then-open frontend
diagnostics below remain historical evidence; the new report retains the
remaining implicit-root, ordinary-presentation and derived-method boundaries.

Djinn and Exference can now infer fresh provider type parameters from a unique,
coherent match against lexical class assumptions. For example, given only
`method :: forall b. C b => Token`, they can synthesize the goal
`forall a. C a => Token` without an ordinary argument or result occurrence of
`a`. The generated graph retains the complete provider scheme, explicit type
selection, and exact introduction/slot of the dictionary used.

This is an engine and Haskell rendering increment within priority 3. It does
not complete contextual synthesis or establish loaded-provider support in the
Haskell REPL or native global-method acceptance in Leant. The
[re-triage](2026-09-07-synthesis-retriage.md) retains those separate gates.

## Inference and evidence

Djinn's source checker first checks the expected result and value arguments,
then jointly matches all qualified layers of that provider application. Only
its fresh, unsolved instantiation variables may change. Candidate matches
preserve lexical levels, occurs checks, and alpha equivalence under quantified
types. Matching consumes the existing checker fuel. Two distinct assignments
produce an explicit ambiguity refusal. Exact dictionary lookup still runs
after type inference.

Exference uses the same bounded matching operation in search and independent
expression checking. Search includes direct root assumptions and the current
goal's lexical assumptions, without superclass expansion. The checker retains
each provider use's lexical snapshot and delays this inference until ordinary
expression checking has supplied its equations. A solved group may inform
another group; existing scope validation checks every assigned type. The
matching operation permits at most 4,096 candidate attempts per invocation.
Exhaustion, no solution, or multiple distinct assignments leave the ordinary
unresolved-constraint path in place. Finding one assignment before exhaustion
does not certify uniqueness.

Neither implementation assigns a goal's rigid variables or treats a class
name as dictionary authority. A unique type assignment also does not identify
one of two equal-predicate dictionaries. Existing overlap restrictions remain.
Instance search and superclass derivation are not new evidence sources here.

## Haskell rendering and behavioral preflight

`renderHaskellTermGraphAtSignature` renders a right-hand side under an exact,
explicitly quantified source signature with `ScopedTypeVariables`. Before it
uses source binder spellings, it checks the complete closed signature against
the graph root and validates those spellings. For a constraint-only root this
preserves the selected outer type instead of introducing a polymorphic helper
whose final implicit use would lose that selection.

For an ambiguous constrained provider, the renderer uses its checked source
declaration and an explicit type application such as `method @a`. A redundant
polymorphic annotation on the provider or lambda pattern can itself trigger
ambiguous subsumption, so those annotations are omitted at this boundary.
Other roots retain the existing rendering route. The signature-free API
explicitly refuses roots requiring lexical signature scope.

The behavioral worker enables `AllowAmbiguousTypes`. Its fresh alias also
forwards the original signature's explicitly scoped type parameters. This
repairs preflight of a named `where` predicate without changing its original
signature or evaluating the preflight placeholder. It retains the fresh outer
binding that prevents a candidate from recursively capturing a same-named
provider.

## Validation

The focused private suites passed: 114 Djinn source-graph tests and 92
Exference engine tests. New cases include global/local bare methods, jointly
constrained parameters, impredicative selections, ambiguity and work-guard
refusals, quantified-variable capture, and sibling-scope leakage.

Four public API synthesis tests cover global and local providers in both
engines. Search sees only a generic signature and an empty class declaration;
GHC replay supplies actual dictionaries with distinct payloads, 37 for `Int`
and 91 for `Bool`. Each exact generated implementation passes both positive
observations and rejects both swapped-payload observations. The local case
retains the polymorphic parameter in the original full signature. Its replay
supplier uses a type abstraction; that fixture syntax is not a new generated
term or search provider. These four tests passed in 4.42 seconds in the
focused run.

The shared renderer adds five checks covering required lexical scope, correct
source binder use, mismatched full signatures, invalid names, and free source
variables. The CLI adds a preflight regression.

The fresh [regression receipt](../../test-church/receipts/constraint-only-provider-inference.json)
records **2,304 passing tests in 15 complete suites**, taking 409.68 seconds
in the test-suite summaries. This includes all 102 Djex CLI tests, all 151
Djex integration tests, both engines' unit and CLI suites, and shared
rendering, source-graph, Length, fingerprint and certificate tests. Each
suite's passing count matches its complete unique inventory. Sources,
executables, and helper identities remained unchanged. The strict serial
build passed with `--ghc-options=-Werror`.

The receipt includes complete test captures, source/helper hashes, the local
driver and strict build log, and the separate CLI diagnostic fixtures and
results. Reproduce the suites using `cabal test` with the 15 component names
in `regression.selected_suites`, `-j1`, and
`--test-options="-j1 --color=never --timeout=180s"`; build with
`--ghc-options=-Werror`. The local receipt driver additionally checks inventory
and artifact identity. The earlier Church and Lean matrices were not rerun.

## Remaining frontend gates

Direct CLI diagnostics exposed two further boundaries:

* An explicit `forall` query now passes behavioral preflight. The loaded
  Exference contextual provider reaches the legacy provider path without a
  usable closed source scheme and reports `UnsupportedContextEvidence`.
  Early Djinn diagnostics used default value-axiom exclusion. Revision 5
  explicitly enables value axioms and still reports no declaration for
  `method`: the frontend's `admitDeclarations` separately omits values with a
  leading class context. This is an admission failure, not a search miss with
  the intended provider. Supplying the source scheme through the public
  API passes the four tests above. Preserve that distinction when extending
  source loading; do not grant authority to the legacy binding by its name.
* An implicitly quantified constraint-only query still fails behavioral alias
  preflight. Supporting it requires scoped binding generation that preserves
  the original binder order and type-application semantics. This increment's
  new rendering API explicitly requires a scoped, explicit source signature.

The first direct fixture used an explicitly kinded class parameter, which the
source loader rejects. Corrected fixtures used `NoPolyKinds` and retained the
same constraint-only provider shape; removing the class method did not resolve
the missing-source-scheme failure. Those diagnostics are not positive synthesis
acceptance. Nested constraint-only forall construction beyond the tested root
and local-provider forms also remains open.

Leant's working global-provider prototype must still be rebuilt against this
increment and pass its unchanged six positive and seven negative unit cases,
ordinary/where native matrix in all three engine modes, actual False controls,
discovery/cache isolation, the existing local-context matrix, and the complete
unit suite. No new Lean Church or tree-fold coverage is claimed here.
