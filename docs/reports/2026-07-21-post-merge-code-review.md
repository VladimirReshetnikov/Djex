# Djex post-merge code review

Date: 2026-07-21

## Scope and method

This pass reviewed `djinn/`, `exference/`, and the shared `synthesis/` layer as
one library. It compared the checked request/session/result paths, inspected
the historical compatibility frontends, searched production code for partial
operations and deferred work, audited every left fold that constructs an AST
or paired index, and rebuilt the complete package with the release warning
set.

The review began from a green run of all eleven test suites. Focused changes
were then validated against the shared-foundation suite and every affected
backend suite before being published. The objective remained architectural
uniformity rather than merging Djinn's proof calculus with Exference's ranked
search: they now share language-facing invariants and lifecycle, while their
different search guarantees remain explicit.

## Correctness and robustness findings resolved

1. **Exference converted HSE source coordinates twice.** Extraction errors and
   frontend utilities maintained separate implementations for turning
   `SrcSpanInfo` into the checked shared `SourceLocation`. The copies already
   differed in fallback structure and could drift on invalid constructed HSE
   spans. `Language.Haskell.Exference.HaskellSrcUtils` now owns the point,
   span, and span-start conversions; extraction delegates to that single
   policy while retaining its own diagnostic context.
2. **Preferred selection was not genuinely strict for wide batches.** The
   shared preferred-tier selector used `foldl'` over a pair, which evaluates
   only the pair constructor. One tier could retain a deferred chain of rank
   comparisons until the batch ended. Both tier summaries are now forced at
   each step, and ordinary ranked insertion is shared by the preferred and
   non-preferred paths. A 200,000-candidate regression exercises the exact
   mixed-tier case used by Exference's default preference policy.
3. **Both engines built application spines with lazy left folds.** Shared type
   applications, Djinn formula/proof/generated-term lowering, Exference search
   expressions, and Exference's HSE adapter all accumulated left-associated
   constructors with `foldl`. These functions must consume their finite input
   list before their final tree is useful, so the laziness retained a chain of
   pending constructor applications without preserving streaming behavior.
   Every layer now uses the same strict-spine rule. A 200,000-argument shared
   type regression protects the common boundary.
4. **Two paired index builders had the same hidden lazy accumulator.** The
   shared preliminary class-arity pass and Exference residual-variable naming
   used `foldl'` over `(Map, Set)`-style state. Both indexes are now brought to
   weak head normal form at each step, preventing a wide inventory or
   caller-built residual from deferring its insertion chain. Class-arity
   *values* remain deliberately lazy, so the bounded preflight still does not
   inspect a class parameter spine merely to collect nominal headers.

## Consolidation and simplification

- HSE coordinate interpretation has one frontend owner and three small named
  operations instead of two parallel conversion trees.
- Ranked selection has one `addRanked` operation for comparison and tie
  handling. Preferred selection adds only tier classification.
- Left-associated type, proof, expression, and HSE application construction
  follows one evaluation contract across the shared IR and both engines.
- Comments now identify the otherwise non-obvious distinction between a
  strict accumulator pair and lazy values stored inside its indexes.

The request/session comparison found no further responsible merger. Both
adapters already use the shared `CachedQuery` provenance envelope, bounded
context traversal, neutral `Environment`/`Inventory` authority, common
`QueryResult` and `SearchBatch` protocol, and shared generated-code renderer.
Djinn's session must retain proof premises and a formula compiler; Exference's
session must retain a policy-filtered search dictionary and omission report.
Hiding those differences behind another common record would move code without
removing an invariant or policy.

## Deliberate behavior retained

- Search output remains lazy. `SelectFirst`, `SelectAll`, checked result
  construction, and result projection do not inspect unused batches or
  candidate tails. Strictness was added only to finite work a caller has
  already requested: one batch summary, one declaration inventory, or one AST
  spine.
- Djinn may produce proof-backed non-inhabitation; Exference cannot. Evidence
  and operational completion remain separate dimensions.
- Djinn's canonical stored binder names and Exference's numeric identities
  plus checked render-time hints remain observable compatibility contracts.
- Exference still omits recursive datatype elimination with a structured
  reason. Supporting it needs a termination argument, not a mechanical reuse
  of Djinn's elimination rules.
- Exference's bounded pure rendering defense around caller-owned lazy strings
  remains the documented narrow `unsafePerformIO` boundary. It preserves
  asynchronous exceptions and prevents cyclic or exceptional hints from
  making a compatibility renderer partial.
- Production `error` sites reached by the risk scan are sealed-invariant
  sentinels; test-only sites are strictness probes. Converting those sentinels
  into ordinary input errors would obscure an internal invariant failure.

## Validation

The focused slices passed:

- 254 shared-foundation tests;
- 68 Djinn unit tests; and
- 402 Exference unit tests.

The complete library and all three executables also build cleanly with:

```console
cabal build all --ghc-options='-Werror -Widentities -Wincomplete-patterns -Wincomplete-record-selectors'
cabal check
```

The final handoff repeats all eleven suites serially and runs Haddock and source
distribution generation so documentation and packaging are checked alongside
the code.
