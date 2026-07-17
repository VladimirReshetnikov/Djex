# Djex post-fold review sweep

Date: 2026-07-16

## Scope

A whole-tree review of `djinn/` and `exference/` after the single-library
fold, looking for what remained: reachable defects, duplicated logic,
dead compatibility surface, and gratuitous shape differences between the
two backends. Findings were verified against the current tree before any
change; every commit in this sweep kept all eleven test suites green and
the library warning-clean under the release `ghc-options`.

## Defects fixed

1. **Partial-application let annotations.** The search engine's `noUnify`
   branch annotated a one-argument partial-application let with the
   applier's final result type instead of the remaining arrow type, while
   recording the correct arrow type in the same step's scope entry. The
   independent checker unifies let annotations with inferred binding
   types, so every solution that retained such a let — any solution
   reusing a partially applied binding, such as
   `let v = f a in h (v b) (v b)` — failed the check and was silently
   discarded by `checkedSimplification` on both output paths. A
   regression fixture pins that an arrow-typed local now survives to the
   output.
2. **Constrained goals in the public checker.** `checkExpression` opened
   a goal's prenex chain with the shared rigid-instantiation plan but
   discarded each layer's constraints, while live search adds them
   (rigid-instantiated) to its query assumptions. External validation of
   a constrained query's own search result therefore failed with
   `ConstraintMismatch`. The checker now augments its class environment
   with the opened constraints exactly as `forallStep` does. The same
   boundary also compared caller-supplied residual constraints without
   canonicalization against a canonicalized inferred side; expected
   constraints are now canonicalized once before comparison.
3. **Kinded binders on class, synonym, and instance heads.** The
   unsupported-vocabulary scan rejected kinded datatype parameters with a
   located, coded diagnostic but never inspected class heads, synonym
   heads, or an instance rule's explicit binders, all of which parse
   under the loader's mode. They previously failed deep inside an
   extractor with the bare string "kinded type variable". All four head
   forms now cross the same boundary with exact source spans.
4. **Djinn `:load` rollback after output.** `loadFile` wrapped a lazy
   `readFile` and the whole command loop in one `tryIOError`, so a decode
   or device error surfacing mid-file aborted the loop after earlier
   commands had printed results, silently restoring the pre-load session.
   The file is now forced before any command runs, keeping printed output
   and surviving state in agreement with the suite's pinned incremental
   batch semantics.

## Consolidation and simplification

- Proof engine: one shared `freshenTermBinders` traversal now backs both
  the prover's substitution copying and generated-output freshening;
  `freeVars` dropped its quadratic list operations; `TypeFormula`'s
  definition-expansion step exists once; `checkProof` lost an unreachable
  ambiguity check and its supporting machinery.
- Djinn compatibility layer: raw and native class-context instantiation
  share `instantiateContextMethods`; the primed fresh-spelling policy,
  role-aware value-name projection, canonical unit literal, and
  legacy edit-failure wording each have a single owner. The stable
  adapter's edit diagnostics now render through the same authority as
  the raw string API.
- Djinn frontend: the REPL consumes the adapter's exported
  `validateDjinnTarget` and `validateDjinnQueryType` instead of
  duplicating pinned diagnostics (its parsed-type rejection had already
  drifted to a different code); the command grammar dropped a
  placeholder kind and a constant `Bool` parameter.
- Exference search core: `limitQueue` no longer rebuilds the entire
  priority queue on every step when a maximum size is configured; the
  unifier's three projection closures share one implementation; the two
  same-named `environmentTypes` helpers with different coverage are now
  `environmentBindingMonotypes` and `completeEnvironmentTypes`.
- Exference types/adapter: `instanceConstraintVariables` and the
  exported `defaultVariableName` own the implicit-binder and fallback
  spelling policies; request-sealing diagnostics carry the shared
  goal-versus-context site role; the option-versus-query failure split
  is a complete, wildcard-free classification beside the error type;
  the checker's flexible supply comes straight from its identifier set
  through a shape-preserving `NonEmpty` freshening.
- HSE frontend: closed-world class-application conversion exists once
  (`resolveKnownClass` and `checkedClassApplication`) with instance
  heads keeping their diagnostic precedence over prerequisites; the
  loader entry points share one `runLoader` projection; `getDataConss`
  wraps the shared `tyVarTransform`.
- Packaging and tests: the merged `djex` command is now the exposed
  library module `Language.Haskell.Djex.CLI`, making all three
  executables identical thin launchers; the three CLI suites share one
  `CLIAssertions` module (the djinn copy of `countOccurrences` looped
  forever on an empty needle).
- Dead surface removed after repository-wide caller checks:
  `getBinderVars`/`getBinderVarsHE` and their `HTypes` re-export, the
  deprecated `largestSubstsId` with `maximumSubstitutionFlexibleId`,
  `constraintMapTypes`, `allocateExpressionNames`, the legacy
  `convertConstraint` wrapper, and several exports of module-internal
  helpers. `(<->)` and `pHTAtom` stay: unused in-repo but coherent
  members of exposed historical DSLs with no drift risk.

## Deliberately deferred

- **Djinn display naming.** `niceNames` bakes `a`..`z` spellings into
  the tree while Exference defers to the shared render-time allocator.
  Unifying them risks the exact historical output pinned across the
  Djinn suites and deserves its own carefully-tested pass.
- **Exference error-taxonomy split.** `SynthesisDeclarationError`
  remains one broad vocabulary where Djinn separates declaration from
  environment errors; splitting it changes a public type.
- **Extraction-phase error channel.** The HSE extraction phases still
  report span-free `Either String` values pinned into
  `EnvironmentLoadError`'s `NonEmpty String` constructors. The concrete
  boundary hole (kinded binders) is fixed; migrating the whole channel
  to located diagnostics is a larger contract change.
- **Ground edit transaction.** `declareDjinnDeclaration` weakens the
  sealed ground environment to `Int` kinds to reuse the raw editor,
  which re-grounds it. A ground-typed editor entrance would mirror
  `prepareGroundSynthesisEnvironment` but touches the transactional
  plumbing.
- **Session bridge overlap.** `loadExferenceSessionWithPolicy` repeats
  four lines of `Language.Haskell.Exference.Session`; this is the
  recorded dependencies-point-inward decision, not an oversight.
