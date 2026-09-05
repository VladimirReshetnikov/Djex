# Church signature synthesis acceptance

The [candidate-quality guide](../docs/candidate-quality.md) and
[focused quality probes](quality.md) cover the separate comparison of
`legacy`, `balanced`, `compact`, and `diverse`. The reproduction commands
below run the current checkout and its default policy. The receipts below
distinguish current Haskell and E0 Lean corpus acceptance, quality comparisons,
and compact-fixture validation from historical receipts. The fresh full E0
Lean Church replay passed all 700 queries; remaining ordinary compatibility
checks are pending.

This suite synthesizes implementations from **every type signature** in
`docs/examples/Church.hs`: 346 top-level signatures and four explicitly
annotated local bindings. The generated implementations need only inhabit the
types; their behavior is not required to match the source functions.

No Church implementation is imported into either synthesis environment. The
source bodies are read only by GHC to determine the types of 51 partial source
signatures, such as `uncurry :: _ -> Pair a b -> c`. GHC's result supplies the
expected type, never a candidate or a named implementation provider.

## Reproduce

From the Djex repository root, using the supported GHC 9.12.4 toolchain:

```powershell
python test-church/extract_corpus.py --check
cabal test djex-church-tests --test-show-details=direct --test-options="--backend both --timeout 2 --steps 10000"
```

During development, the already built library can also run the harness directly:

```powershell
cabal exec -- runghc -package=djex test-church/Main.hs --backend both --timeout 2 --steps 10000
```

To regenerate the inventory after deliberately changing the source corpus:

```powershell
python test-church/extract_corpus.py
```

`--backend djinn` and `--backend exference` select one backend. `--names
pair,either,sequence` selects source names; selecting `go` includes both local
bindings bearing that name. `--report-dir` changes the output directory. The
per-query timeout and step/choice budget are explicit operational limits, so a
miss under these limits is a test failure, not a non-inhabitation theorem.

The test runner first re-extracts the source and fails if any checked-in
inventory artifact is stale. It then passes each expanded type through the
public Djex session/request/search/rendering API. Each successful candidate is
checked by GHC within one generated module:

1. At the fully expanded, capture-avoiding query type used for synthesis.
2. Through a forwarding binding at the original, alias-preserving resolved
   signature, with the explicit default argument described below when needed.
3. For each of the 19 partial Haskell cases, through a wrapper at the exact
   original signature that supplies `undefined` to that one explicit default
   argument. The synthesis engine never receives this value.

The second check independently tests synonym expansion against GHC. All ten
original type aliases are included in the compiler fixture. Generated modules
use `NoImplicitPrelude`; the search environments contain only the declared
abstract `Int` type and, for the classified cases below, `churchZero :: Int`.
No `undefined`, `error`, recursion helper, or source function is admitted to
search. The compiler fixture imports `undefined` solely for the final partial
wrappers.

## Explicit assumptions

The inventory separates three categories:

| Category | Count | Search context |
| --- | ---: | --- |
| `total` | 315 | No named value providers |
| `needs_int_provider` | 16 | One concrete constant, `churchZero :: Int` |
| `requires_partiality` | 19 | One explicit argument of the returned element type |
| **Total** | **350** | |

The first count includes the four local signatures. Those local declarations
are tested as standalone generalized types; their enclosing definition and
lexical origin remain recorded in the manifest. No captured implementation is
introduced into the search environment.

The 19 partial cases are `head`, `last`, `at`, `foldl1`, `foldr1`, `fromJust`,
`fromLeft`, `fromRight`, `maximumBy`, `maximumOn`, `minimumBy`, `minimumOn`,
`minMaxBy`, `minmaxElement`, `atKey`, `reduce`, `maximum`, `minimum`, and `minMax`.
Their original signatures have no total closed inhabitants: instantiate the
returned element type with an empty type and supply empty Church containers.
The remaining inputs, including encoded equality/order functions, can still
be supplied. A returned element would then inhabit the empty type.

Following the explicit partiality policy, the suite supplies **only that
element's default** as an argument inside its universal quantifier. For
example:

```haskell
-- Original partial operation:
head :: forall a. List a -> a

-- Total contextual query used by this suite:
headWithDefault :: forall a. a -> List a -> a
```

This tests a total implementation under a stated assumption. It does not claim
to have produced a total implementation of the original closed `head` type.
For Haskell, the compiler fixture additionally verifies an explicitly partial
wrapper at that exact original signature:

```haskell
headPartial :: forall a. List a -> a
headPartial = headWithDefault undefined
```

The wrapper inserts bottom only at the documented default argument after
synthesis, preserving the original partial operation's type. It does not
expose a bottom provider to any search or count a handwritten wrapper as a
new synthesized candidate.
The Lean corpus uses the same explicit default assumption, without an axiom,
`sorry`, or an unrestricted inhabitant of every type. The manifest preserves
both the original type and the augmented query type.

The 16 concrete-provider cases are `length`, `countDistinctBy`, `countIf`,
`count`, `partitionPoint`, `isSortedUntil`, `lowerBound`, `upperBound`,
`equalRange`, `compareBy`, `sizeDict`, `countKey`, `sum`, `product`,
`countDistinct`, and `compareThreeWay`. Their result requires an `Int` value
without a directly supplied `Int` argument. The constant is allowed only in
these query environments; a function with an existing `Int` argument can
instead return that argument.

## Artifacts and evidence

`manifest.json` records the canonical source SHA-256, GHC version, source line,
original signature, hole presence, resolved signature, expanded type AST,
classification, scope, and accepted assumptions for every case. Its AST uses
`name`, `app`, `arrow`, and `forall` nodes and is also consumed by the Lean
acceptance generator. Bound variables are renamed before alias substitution
so nested Church result binders cannot capture their element types.
The source hash uses UTF-8 without a BOM and normalizes CRLF/CR line endings to
LF, so Git checkouts with different line-ending policies share the same
inventory identity.

`cases.tsv` is the runner's compact projection: generated identifier, source
name, category, expanded contextual query, and alias-preserving acceptance
type, followed by the unmodified resolved source type. `aliases.hs.inc`
contains only the ten source type aliases.

Each run writes, separately for each engine, the generated Haskell module, a
per-case TSV result, and the full GHC diagnostic output. These reproducible
run artifacts are ignored by Git. A successful exit requires every selected
query to produce a candidate and the entire generated module to pass GHC.

### Current corpus and quality acceptance

Djex `2954b6d2` passed all **19 test components** in
`results/quality/build-all-05.log`. That aggregate reran this full Church
inventory: **350/350 candidates for Djinn and 350/350 for Exference**, both
with `failed=0` and successful independent GHC checks of their generated
modules. It also passed all 100 independent scope queries, with 76 positive
implementations checked by GHC and 24 bounded negative cases.

Fresh Lean Church runs on the unchanged E0 executable produced **350/350
Djinn and 350/350 Exference candidates**. All **700 exact displayed terms
passed independent kernel replay**, with exactly 700 empty axiom inventories.
Each engine covers 315 total cases, 16 integer-provider cases, and 19
explicit-default cases. Leant's
`test-church/quality-results/church-djinn-final/results.json` and
`church-exference-final/results.json` record successful live and kernel exits,
the exact source/manifest hashes, and stable executable identity. Independent
artifact review reconstructed every replay declaration from its exact
displayed term; neither engine changed a candidate's text from the earlier
`dab110...` corpus run. This is fresh preservation evidence, separate from
quality improvement measurements.

These corpus runs used one-candidate observation, verification, and display
windows, a 30-second synthesis timeout, and 4,096 Exference steps. Djinn
retained `synth-budget off`, subject to the shared deadline and intrinsic
planning caps. The synthesis timeout excludes startup, goal serialization,
and separate kernel replay. Defaults remain ordinary Lean arguments only in
the 19 partial cases; they do not introduce unchecked axioms.

The [focused quality guide](quality.md) records the separate current policy
comparisons: **56 Haskell queries and 104 GHC-checked terms**, plus **14 exact
CLI outputs and ten invalid-option rejections**. Leant `5629936`, with
unchanged Djex synthesis code `2954b6d2`, passed **84 quality queries and 136
exact kernel-accepted terms**: 112 had empty axiom inventories and 24 used
only declared provider premises. Its unchanged E0 executable SHA-256 is
`e0b9c87cae0bc34d59c8d5a34a58fdd5005676913969a80d5503d7281081d025`.
Leant's `test-church/quality-results/matrix-final/results.json` records this
quality comparison, separately from the 350-signature corpus.

The same executable passed **90 compact rank-N/provider queries**, including
all eight/twelve-argument providers and layered global factories. Every exact
term passed independent kernel replay: 78 empty axiom inventories and 12
exact declared-premise inventories. All four live processes and pre-golden
checks passed. After three reviewed golden changes, all four goldens match
the preserved captures by offline comparison; this is not a second synthesis
or kernel run. Leant's `test-church/quality-results/fixtures-repair/results.json`
and `compact-comparison/results.json` retain those distinct outcomes.
The full Leant unit rerun passed **all 569 tests in 389.71 seconds**, with
process exit zero and E0 unchanged (`test-church/quality-results/build-leant-06.log`).
Only the **remaining ordinary compatibility checks are pending**.

The earlier Leant `fb84b96` executable
`dab110ad2a7903ac4ef4883898d48532c00cc8c3b1b8d8748aac7744eedffb61`
passed 565 tests, an 84-query/139-term quality matrix, and the full Church
corpus: 350/350 candidates per engine, all 700 exact terms independently
kernel-accepted with empty axiom inventories. Its receipts remain under
`test-church/quality-results/matrix-accepted/results.json`,
`church-djinn/results.json`, and `church-exference/results.json` in Leant;
they are historical evidence for that executable, not fresh E0 results.
Those corpus runs used a one-candidate window, a 30-second synthesis timeout,
and 4,096 Exference steps. Djinn retained `synth-budget off`, subject to the
shared deadline and intrinsic planning caps; the timeout excludes startup,
serialization, and separate kernel replay.

### Historical rank-N and ordinary compatibility acceptance

The earlier Djex `e2eb71e` validation on 2026-09-04 with GHC 9.12.4,
a two-second per-query timeout, and a
10,000-step/choice budget produced **350/350 candidates for Djinn and 350/350
for Exference**. Both complete generated modules, including all original-type
forwarding checks and all 38 exact-original-signature partial wrappers, passed
GHC. These are compiler-checked type-inhabitation
results, not behavioral equivalence tests for the reference functions.

Leant independently completed the same 350-case inventory through each engine
in live runs: **350/350 Djinn and 350/350 Exference**, with all **700 exact
displayed terms accepted by Lean 4.32.0 and all 700 axiom inventories empty**.
Each engine covers 315 pure total cases, 16 integer-provider cases, and 19
explicit-default cases. The runs used a one-candidate window and a configured
thirty-second synthesis timeout. Exference used 4,096 search steps; Djinn
retained its default unbounded choice-point budget (`synth-budget off`), subject
to the shared wall-clock search deadline and intrinsic finite planning caps.
The Lean runner's `--steps` option sets the Exference-only `synth-steps` budget,
so `--steps 4096` does not impose a Djinn choice-point limit. The synthesis
timeout does not cover process startup, goal serialization, or the separate
standalone kernel replay. Both runs used one unchanged executable:
`addfac35b9d82955fc871c177b582a8c043475c0171c22cb17977e0e9f5b9869` (SHA-256),
built from Leant `4757569` with Djex `e2eb71e`.

The reports in Leant are
`test-church/generated-djinn-acceptance/results.json` and
`test-church/generated-exference-acceptance/results.json`; the independent
90-query compact fixtures also pass on that executable.

The full ordinary Leant compatibility run also completed on the same unchanged
executable, using a configured 600-second synthesis timeout. It covered 30
files and 265 synthesis commands (263 explicit-type queries plus two proof-mode
commands), taking 5,515.86 seconds. The original runner returned exit code 1:
17 fixture outputs matched immediately and 13 differed from their baselines.
Those changes were reviewed query by query: 114 actual displayed terms across
34 changed queries passed independent Lean 4.32.0 replay, with 31 empty axiom
inventories and 83 containing exactly their fixture-declared premises. No
previously successful query lost its result. Leant's
`test-church/generated-final-drift-index/results.json` records that review,
excluding four earlier baseline updates and their separate 44 term replays.
Only reviewed term and diagnostic changes were applied to the golden files.

All **30 final golden files match the retained full live-run outputs** after
the production Bash runner's normalization and command-substitution semantics.
This final check is an offline comparison of the completed live outputs, not
a second live synthesis run. Leant's
`test-church/generated-goldens-final/results.json` preserves the original
exit code, timing, and executable identity; `final-comparison.json` records
the successful final comparisons, with `runner.log` and `transcripts/`
retaining the original output. The
[comprehensive account](../docs/rank-n-impredicative-synthesis.tex) records
those separate acceptance boundaries and Lean's universe/default policy.

## Independent scope and reconstruction probe

```powershell
cabal exec -- runghc -package=djex test-church/probe_scopes.hs
```

This separate probe exercises 38 inhabitable signatures and 12 deliberately
incompatible scope or correlation patterns through each public backend. It
covers alpha-renaming, shadowed binders, nested polymorphic results, ambient
type variables, repeated correlations, higher-kinded applications, and
instantiation choices determined only when an argument is supplied. It also
constructs one or two distinct polymorphic arguments, composes providers inside
a new polymorphic argument, preserves dependent nested scopes, and selects
polymorphic instances across successive ordinary-argument/forall layers. Every
query receives only the abstract type constructors `F`, `G`, `G3`, `H`, `Seed`,
and `Token`, plus any provider signatures explicitly listed for that case. Five global
provider cases exercise delayed instantiation or successive quantified result
layers of a named polymorphic function;
one additional case supplies only a unit value. The remaining cases use only
values supplied as query arguments.

Provider bodies are generated solely in a separate compiler support module.
They are total, their constructors are hidden, and their source never enters
the synthesis API. Each candidate is compiled in its own module importing
exactly its case's allowed provider names. This prevents one case's compiler
context from accidentally supplying an implementation to another case.

Every returned candidate is sent to GHC at the unchanged query signature,
including any unexpected candidate for a negative case. Missing positive
candidates, unexpected negative candidates, errors, timeouts, or compiler
failures make the probe fail. A negative result means that no candidate was
returned under the stated search budget; it is not a non-inhabitation theorem.
Positive queries use 10,000 steps or choices; every negative query uses the
same smaller 1,000-step or choice budget. All queries retain a three-second
wall-clock safety timeout, and reaching that timeout is a failure. This
distinction matters for intentionally empty polymorphic factory goals whose
bounded search can otherwise spend substantial time exploring alternatives.
Generated modules and per-case results are written to the ignored
`results/scopes/` directory. `NoPolyKinds` in the compiler fixture makes the
support module's datatype declarations agree with the explicitly supplied kinds
of the abstract constructors in the synthesis API.

The nested-result cases also require reconstruction evidence that GHC cannot
always infer from an unannotated occurrence. The implementation report in
[`docs/rank-n-impredicative-synthesis.tex`](../docs/rank-n-impredicative-synthesis.tex)
explains these cases and compiler-checked explicit type applications.

The pre-quality reconstruction pass was validated in September 2026: all 100 probe
queries met their expectations across both engines, all 76 positive
implementations passed GHC, and all 24 negative queries returned no candidate
without errors or timeouts. The current `2954b6d2` aggregate in
`results/quality/build-all-05.log` reran the same 100-query inventory with all
expectations satisfied and both GHC checks successful. These counts are
separate from the Church corpus.
