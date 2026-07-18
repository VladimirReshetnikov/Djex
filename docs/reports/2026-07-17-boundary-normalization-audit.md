# Djex boundary and normalization audit

Date: 2026-07-17

## Scope

This pass reviewed the merged `djinn/` and `exference/` trees after the main
convergence work, concentrating on public compatibility records and adapters
that can be constructed without first sealing a session. The review traced
source order, canonical type identity, lazy list bounds, declaration caches,
request plans, rendering, and option validation from each public entrance to
the shared synthesis foundation.

The goal was one authority per invariant, not identical backend algorithms.
Djinn remains a proof search with historical raw research APIs; Exference
remains a heuristic search with an open-world class policy. Their stable
session, request, result, diagnostic, normalization, and rendering boundaries
now follow the same ownership model wherever their semantics permit it.

## Correctness findings resolved

1. **Canonical instance identity was incomplete.** Shared duplicate-instance
   keys alpha-normalized variables but did not canonicalize saturated function
   and tuple constructors. Canonical-equivalent heads could therefore coexist
   in one environment. The private key now canonicalizes each argument first,
   while indexes and diagnostics retain the caller's original head.
2. **Raw synonym binders could be laundered by substitution.** A reached raw
   alias with repeated parameters was converted to a `Map`, silently letting
   the last argument win. Expansion now reports the first repeated parameter
   before arity checking or substitution. Unreachable malformed raw aliases
   remain uninspected, preserving the loader's phase contract.
3. **Djinn abstract definitions had conflicting authorities.** The historical
   tuple stored an outer name and cached kind beside `HTAbstract`'s embedded
   name and declared kind. One normalizer now serves shared conversion, raw
   environment checking, and standalone HCheck operations. Names must agree,
   abstract parameters are rejected, and the embedded kind refreshes the
   compatibility cache.
4. **Exference compared noncanonical constructor signatures.** Reconciliation
   between datatype constructors and their duplicated search bindings compared
   raw type trees. Structural arrows and tuples now compare equal to their
   saturated constructor applications without weakening real shape checks.
5. **Legacy deconstructor records could bypass complete type validation.**
   Datatype-head analysis observed tuple arity and forall metadata before the
   public compatibility type crossed the checked boundary. It now validates
   and canonicalizes the complete result first, bounding poisoned/cyclic tuple
   spines and rejecting duplicate forall binders through shared diagnostics.
6. **Class diagnostics could be alphabetized or phase-reordered.** Exference
   built a diagnostic class map before local validation, and the frontend later
   passed `Map.elems` to the core. A nondiagnostic first-occurrence lookup table
   still permits forward superclass references, while one source-order walk
   owns declaration errors and duplicates.
7. **Djinn renderers trusted caller-forged residual obligations.** Genuine
   Djinn candidates are closed, but the public shared `Candidate` constructor
   can attach residual constraints. Both stable renderers now preserve their
   existing generated-syntax error precedence and then reject any nonempty
   residual list with `UnexpectedResidualConstraints` without forcing its
   tail.
8. **Stable request constructors could diverge on cyclic class arguments.**
   Both adapters traversed programmatic context spines before a session could
   supply the class arity. Request plans now retain a canonical goal and defer
   exact context arguments. Execution checks each known arity through at most
   one extra cell before entering the spine. Exference separately detaches and
   validates source spellings at sealing, then binds them to the contextual
   goal after session-bounded normalization.
9. **Stable Exference validated options twice.** The facade needed an early
   check so option errors precede elaboration, but core preparation repeated
   the same validation. A private checked-options witness now replaces the
   unchecked query field at the internal search entrance. A poisoned-field
   regression proves preparation does not evaluate the options again.
10. **Lossy compatibility search looked checked.** Three Core list projections
    erased every `ExferenceInputError`, making invalid input indistinguishable
    from a valid empty search. They are now deprecated, and
    `findExpressionsChunkedEither` completes the flat/grouped/status-bearing
    lossless family.

## Consolidation and simplification

- Exference source extraction records compact declaration slots before HSE
  annotations are erased. One stable merge now preserves successes and errors
  across signatures, datatype batches, and nested class methods without
  replaying syntax or guessing multi-name signature cardinality.
- Unconstrained `FunctionBinding`s project as shared monotypes instead of
  manufacturing `TypeForall [] []`; binderless foralls remain only when they
  own a nonempty context. Reverse conversion still accepts the historical
  empty wrapper.
- Historical and merged CLI defaults are derived from their public backend
  default records rather than repeated literals.
- The trusted unit used to bootstrap Djinn now enters through the same shared
  `prepareEnvironment` authority as operational sessions.
- Qualification of generated terms, types, constraints, and residual output
  uses the shared policy. The curated `Language.Haskell.Djex` facade now also
  re-exports the complete shared `Qualification` module.

## Deliberate compatibility boundary retained

`Djinn.Internal.Environment.validateEnvironment` was not rewritten as a thin
projection of shared preparation. Its public raw contract intentionally checks
value namespaces, raw type inference, axioms, and classes in a different order
and preserves category-specific string diagnostics. It also rejects raw
recursive dependency graphs before alias expansion, whereas operational shared
preparation classifies recursion after expansion.

All production session construction, editing, querying, and now standard-unit
bootstrapping use the shared prepared environment. The legacy validator is
therefore quarantined as a research compatibility surface rather than left as
a competing operational authority.

Unknown external Exference classes likewise retain their open-world policy.
Only a session-known class supplies a structural argument bound; finite unknown
constraints remain supported, while callers must provide an ordinary finite
list for an unknown class.

## Validation contract

Focused regressions cover canonical-equivalent instance and constructor forms,
duplicate raw synonym binders, every Djinn abstract-definition entrance,
source-order class failures, poisoned and cyclic tuple heads, forged open
Djinn candidates, once-only option validation, all lossless Core search shapes,
and timeout-bounded cyclic request spines for both backends.

The release handoff runs the complete serial suite and package checks:

```console
cabal build all
cabal test all -j1 --test-show-details=direct
cabal build all --ghc-options='-Werror -Widentities -Wincomplete-patterns -Wincomplete-record-selectors'
cabal check
cabal haddock lib:djex --haddock-hyperlink-source
cabal sdist
```

Serial tests avoid a known Cabal executable-replacement race seen when two
test-suite links target the same build tree concurrently.
