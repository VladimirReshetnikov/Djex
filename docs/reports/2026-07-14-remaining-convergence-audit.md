# Djex remaining-convergence audit

Date: 2026-07-14

## Scope

This audit began by recording what remained before Djex could honestly be
described as one parser-free library rather than one package containing two
engines and a shared foundation. It now also records the completed inventory,
query-type authority, and Cabal-component convergence on the
`codex/unify-djinn-and-exference` branch.

The existing work is substantial, but the original merger objective is not
complete. Search algorithms may remain backend-specific; duplicate language
IRs, competing result/environment authorities, and the three parser-free
component identities targeted here have now been removed. The two compatibility
frontends remain separate to isolate parser and interactive dependencies.

## Evidence from the current tree

At audit time `djex.cabal` defined six library components:

1. the unnamed `djex` facade;
2. `synthesis`;
3. `djinn-core`;
4. `djinn-frontend`;
5. `exference-core`; and
6. `exference-frontend`.

That component DAG enforced a useful direction:

```text
synthesis <- backend cores <- compatibility frontends
          \- checked adapters -/        |
                    \- djex facade -----/
```

The default library and parser-independent cores consequently did not acquire
`haskell-src-exts`, `directory`, `filepath`, or `haskeline`. The core and
facade API suites exercise those package boundaries rather than merely checking
that a module happens to import.

**Completed on 2026-07-14:** `djex.cabal` now defines three library components:
the unnamed parser-free `djex` library, `djinn-frontend`, and
`exference-frontend`. The first compiles every uniquely named module under
`src`, `synthesis/src`, `djinn/src-core`, and `exference/src-core` into one
acyclic unit. Its external dependency union is only `base`, `containers`,
`deepseq`, `pqueue`, `pretty`, and `transformers`. `directory`, `filepath`,
`haskeline`, and `haskell-src-exts` remain confined to the two compatibility
frontends. All modules formerly exposed by `synthesis`, `djinn-core`, and
`exference-core` remain exposed by `djex`; only their obsolete Cabal unit
identities and internal self-dependencies disappeared.

The resulting component graph is:

```text
                         djinn-frontend (Haskeline)
                        /
parser-free djex library
                        \
                         exference-frontend (HSE/filesystem)
```

`djex-parser-free-api-tests` depends only on `djex` and imports the shared
vocabulary plus both engines. The two frontend API suites depend only on their
respective frontend and verify that compatibility reexports still cross this
boundary.

## Priority 1: make the shared vocabulary native to Exference

**Completed on 2026-07-14:** `QualifiedName`, `HsConstraint`, and `HsType` now
use the shared `Name`, `Constraint`, and `Type (Variable Int)` values directly.
Historical constructor spellings survive as explicit compatibility patterns;
the structural conversion functions are identity shims, and checked adapters
only validate and canonicalize. This removed all three duplicate source-type
representations without collapsing the useful component dependency checks at
that stage; the later fold retains only frontend dependency-isolation checks.

At audit time `Language.Haskell.Exference.Core.Types` defined three pieces of a
second source-type model beside `Language.Haskell.Synthesis`: a `QualifiedName`
wrapper, the recursive `HsType`, and a duplicate `HsConstraint`. The first
stage removed the name and constraint representations, including every
production `toSynthesisName` / `fromSynthesisName` call. The completed type
stage then replaced the recursive `HsType` tree with the shared type directly.

The migration reached the search core, stable request/session storage,
generated candidates, residual constraints, rendering hints, class
environments, and the HSE compatibility frontend. Production code no longer
uses the old total structural-conversion shims; only named validation and
canonicalization boundaries remain.

The migration proceeded in five compile-checked stages:

1. **Completed:** moved shared `Name` and `Constraint` values into the search
   core, preserving historical constructor spellings as explicit compatibility
   patterns rather than as engine-owned representations.
2. **Completed:** moved free-variable collection, substitution, rendering hints,
   and constraint traversal to `Type (Variable Int)`. Exference-specific
   unification remains backend code but now operates on the shared tree.
3. **Completed:** taught the unifier, rigid-instantiation, expression checker,
   constraint solver, and search-node builders to consume the shared type
   directly.
4. **Completed:** deleted the now-identity type/constraint conversion passes in
   the checked adapter and candidate projection. Attaching a checked target to
   an output clause remains a lazy `fmap`, so later search batches are not
   forced.
5. **Completed:** made the HSE frontend construct shared types once. The old
   `HsType` surface remains as a compatibility alias and patterns over the
   native value, not as the representation stored in a sealed session.

Tuple representation received explicit treatment. The shared IR's structural
`TupleType` is the canonical checked form; the search algorithms handle it
directly and normalize saturated constructor applications to it. The
documented `COMPLETE` set includes the structural tuple case and the total
`TypeForallNative` view, so compatibility patterns do not conceal an unhandled
native constructor.

Regression evidence now covers flexible-versus-rigid identity,
capture-avoiding forall substitution, tuple canonicalization, residual
constraint order, unknown-class policy, type-variable rendering hints, and
lazy batch tails. It additionally pins malformed native-value rejection,
higher-kinded tuple/function unification, exact historical tuple ranking, and
the supported GHC 9.12.4 compiler graph. Priority 2 subsequently removed the
remaining result authority beneath the shared envelope.

## Priority 2: remove duplicate result envelopes

**Completed on 2026-07-14.** Both cores now construct their stable
`QueryResult` payloads directly; neither checked adapter owns a second result
authority.

The Djinn proof core constructs
`QueryResult DjinnQueryMetadata DjinnCandidate` directly, retaining the exact
checked `DefinitionName` in every candidate. The checked adapter returns that
same value without a metadata, completion, evidence, or candidate rebuild.
`GeneratedQueryReport`, `QueryOutcome`, and `QueryReport` remain outer
compatibility projections only; they no longer own the canonical search
result. Structured invariant failures also remain distinct from ordinary
query rejection.

The Exference core's `findQueryResultsInEnvironmentEither` excludes the exact
checked target, validates and prepares the query once, derives rendering hints
from the retained plan, and lazily attaches that target to every generated
`FunctionClause`. `runExferenceQuery` returns those results directly. Candidate
statistics, candidate details, and batch metadata each retain one core-owned
record; the stable polished names are zero-cost type aliases and bidirectional
record-pattern views, not records copied field by field.

`SearchCompletion`, `SearchStatus`, `ExferenceChunkElement`, and the checked
status-to-progress conversions remain only at Exference's historical API edge.
The canonical path projects private engine chunks straight into the common
search and query envelopes, so retaining those source-compatible types does
not retain a competing modern result model. Explicit imports of the stable
record views migrate from `T(..)` to `T`, `pattern T`, and the used field
selectors under `PatternSynonyms`; their pattern fields cannot be used by GHC
record-update syntax, so updates match and reconstruct instead. This
experimental representation migration also adopts the core records' derived
`Show`, `Typeable`, generic, and ABI identities; dependants must recompile and
must not treat the former derived `Show` text as a stable serialization.

Regression coverage pins direct core/adapter equality, exact operator targets,
target exclusion without excluding qualified homonyms, candidate-derived
logical evidence, simultaneous operational truncation reasons, exact
`Natural` accounting, and lazy batch and candidate tails. Priority 3 and the
parser-free packaging fold subsequently removed the remaining authority and
component splits beneath those envelopes; the backend-specific search
algorithms remain intentionally distinct.

## Priority 3: make shared inventories authoritative

**Exference source authority completed:** source checking now retains one
opaque annotated prepared-inventory witness instead of a `SourceEnvironment`,
an annotated Inventory, and a neutral Inventory/backend projection. The
witness owns the sealed alias-bearing Inventory, its synonym table, and the
exact ordered/rated backend. Alias-aware recursive-datatype metadata is
attached through an annotation-only Inventory adjustment, so sealing and kind
inference run once. The compatibility `SourceEnvironment` projection is
derived on demand; method ownership comes from nested shared class methods,
while order, ratings, classes, constructors, and recursion come from the
prepared backend. Only historical HSE synonym spellings remain separately as
presentation data. Stable sessions erase annotations without re-preparing the
synonym table or backend, and HSE query parsing derives its minimal known-type
and class-arity resolver from that same session inventory. The final session
keeps only the shared inventory/synonym witness and policy-filtered search
environment; the source wrapper's complete unfiltered backend is consumed and
its omission summary is forced before the wrapper leaves scope.

**Djinn prepared authority completed:** a session now retains only its exact
prepared projection; declaration replacement/removal derives the editable
shared environment losslessly from the grounded Inventory, then publishes the
replacement only after complete resealing. The REPL stores no raw
`Environment`; display reconstructs historical tables from the Inventory on
demand, while instance lookup consumes a nominal class index. Raw preparation
also reprojects embedded kinds from the inferred inventory, fixing a split-brain
bug in which a forged raw class parameter kind could override the inventory
during context checking. `PreparedEnvironment` now contains the shared opaque
Inventory/synonym witness and only its justified private class, ordered
global-premise, kind, and formula caches. Global functions are translated once
while sealing rather than once per query; instantiated class methods remain
query-dependent. Transactional edits rebuild all caches before publishing the
replacement session.

**Cross-backend prepared authority completed:** the neutral synthesis layer now
owns one opaque `PreparedInventory` abstraction. Both backends use it instead
of separately pairing an Inventory with a synonym table; annotation erasure
and datatype-metadata adjustment preserve the exact normalized table without
re-running its allocator-sensitive preparation.

These changes must preserve source diagnostic precedence, declaration
replacement/removal order, alias saturation and recursion checks, inferred
kinds, class-method instantiation, policy exclusions/ratings, and transactional
rollback.

## Packaging end state

**Completed on 2026-07-14:** with duplicated environment authorities removed,
the named parser-free core and foundation sublibraries were folded into the
unnamed `djex` library. `djex:synthesis`, `djex:djinn-core`, and
`djex:exference-core` migrate to plain `djex`; Haskell module names remain
available, but their unit identities deliberately change and downstream
clients must recompile.

The compatibility frontends remain named libraries with no independent
semantic environment or search representation. This is a dependency boundary,
not a competing core architecture: `djinn-frontend` owns Haskeline and the
historical REPL, while `exference-frontend` owns HSE, filesystem loading, and
source-aware compatibility APIs.

This ordering made the Cabal change evidence of a source merger rather than a
relabeling. Deletion is measurable: three public library stanzas and all
internal dependencies on them disappeared after their implementations already
shared one native vocabulary, result boundary, and inventory authority.

## Post-fold command-policy review

**Completed on 2026-07-14:** the first review of the merged command found that
`djex exference` loaded an unrestricted programmatic session while the
historical `exference` command excluded three known recursion helpers. The
merged command could therefore synthesize `Data.Function.fix` silently for a
type that has no terminating inhabitant under the configured environment.

The Exference HSE frontend now owns one checked command-session policy. Both
commands exclude the exact structural names `Data.Function.fix`,
`Control.Monad.forever`, and `Control.Monad.Loops.iterateM_` by default and
accept `--fix` as the same explicit opt-in. The neutral library default remains
unrestricted for programmatic callers. Subprocess regressions exercise both
command paths, including a fix-only source environment; the merged command also
rejects repeated `--fix` before loading that environment.

## Post-fold request-provenance review

**Completed on 2026-07-14:** both opaque request types previously stored an
identical lazy `Maybe (FilePath, SourceSpan)` inside their backend cache. This
duplicated the same authority, allowed a reusable request to retain its complete
source buffer until a deferred diagnostic demanded the span, and left
source-aware constructor failures unlocated.

The shared query layer now owns a strict `RequestProvenance` in every
`CachedQuery`, independently of the adapter's lazy derived cache. A strict
opaque `SourceLocation` materializes complete text spans during sealing;
programmatic requests remain explicitly source-free, and equality/display
still observe only the neutral `QueryRequest`. At that stage, Djinn's cache
contained only its raw proof-core projection, while Exference's contained only
one opaque checked source-hint value.

The review also fixed two attribution bugs. Exference now retains the exact
normalized HSE parse filename for deferred failures, including preserving
angle-bracket virtual-buffer names. Djinn now distinguishes typed invalid
options, ordinary query rejection, internal proof/projection failures, and
shared result-invariant failures. Type-source provenance applies only to input
rejection; separately supplied options and internal invariants are source-less
in both adapters. Historical Djinn string entry points retain their established
option messages.

## Post-fold source-hint trust-boundary review

**Completed on 2026-07-14:** Exference's source frontend previously passed an
unchecked `Map String Int` through its public SPI, opaque request cache, and
canonical core result API. Although those spellings could not change search
or evidence, malformed names and control characters could reach rendered
residual constraints and terminal/source output. Out-of-scope IDs could also
pollute public candidate details or collide with a later search allocation;
validation and forcing cost remained deferred until a candidate demanded the
map.

Canonical search now accepts an opaque, fully forced
`ExferenceSourceTypeVariableHints`. Its checked constructor validates every
alias against the supported extension-aware type-variable grammar, rejects
wildcard, constructor, `forall`, and type-family keyword spellings,
requires IDs in the contextual goal's flexible namespace, stores one
lexicographically least spelling per ID, and retains the exact canonical goal
as a scope witness. Successful values and rejected spelling details are both
fully detached. The source-aware request SPI retains
its raw-map signature as a compatibility entrance but seals it immediately,
reporting `DJEX_EXF_SOURCE_HINT` with the exact request location. Programmatic
requests store a goal-bound empty value, the obsolete one-field request cache
wrapper is gone, and canonical core search rejects a witness paired with a
different same-numbered query. The invariant witness deliberately has no
`Generic` instance: `Generic.to` would otherwise reconstruct its hidden fields
despite the abstract export.

The first independent review found that integer intersection alone was not an
origin proof: an erased phantom argument and an unrelated `forall` binder from
a synonym body could share an ID. The repair belongs to the common synthesis
layer. Alias expansion now alpha-freshens every introduced binder away from
the complete original source namespace, recursively through direct, nested,
and zero-argument aliases, while ordinary substitution retains its narrower
capture-avoidance semantics. The stable adapter then retargets the paired
hints to the elaborated goal before rigid-name propagation.

Historical caller-constructed candidate/batch hint maps no longer feed the
modern checked result path. They remain observable compatibility data, but the
stable residual renderer treats them only as untrusted preferences: it copies
a bounded finite prefix under synchronous-exception containment, validates
the complete identifier, rejects wildcard/malformed/partial/infinite values,
deduplicates accepted names, and freshens deterministic fallbacks. Stable term
rendering applies the same bounded detachment to caller-built local hints while
preserving finite lexical errors, and both paths rethrow asynchronous
exceptions. Regression
coverage includes malformed/control spellings, failure-path strictness, scope
and tag errors, full-`Int` IDs, alias selection, cross-query witness rejection,
source-located SPI errors, flexible/rigid residual rendering, malicious raw
candidate details, and direct plus nested phantom/binder collisions.

## Seal representation-derived invariant witnesses

**Completed on 2026-07-14:** a cross-package abstraction audit found two GHC
capabilities that constructor hiding alone does not prevent. A derived
`Generic` instance makes `Generic.to` a public representation constructor, and
record update needs an exported field label but does not need the data
constructor. Either route could recombine separately validated pieces of the
shared `Environment`, `Inventory`, `TypeSynonyms`, or `QueryResult` values.

The shared witnesses no longer derive `Generic`; invariant-bearing record
labels on `Inventory` and `QueryResult` are now same-named ordinary projections.
The same repair covers Exference's rigid-instantiation context and plan, static
and per-query class indexes, and scope forest, plus Djinn's derived proof
environment. Explicit `NFData` instances retain the previous forcing contract.
Ordinary public payloads whose constructors are already exported keep their
useful generic instances.

`djex-parser-free-api-tests` now compiles deliberately forbidden `Generic` and
`HasField` constraints with deferred type errors, then requires each constraint
to fail when forced. Because that suite is a downstream Cabal component, the
regression observes the real public API under the sole supported GHC 9.12.4
rather than privileged home-module constructor scope.

## Post-fold Djinn query-type authority review

**Completed on 2026-07-15:** the stable Djinn adapter previously accepted the
common `Type String`, converted it to the historical recursive `HType` while
sealing a request, converted it back for shared kind checking and synonym
elaboration, and finally reconstructed `HType` for the formula compiler. This
was lossless for the supported subset but retained a redundant private query
representation and made the stable path look less native than Exference's.

`mkDjinnRequest` now retains two deliberately different shared values: the exact
neutral `QueryRequest` returned by `djinnRequestQuery`, including any
noncanonical but valid constructor spelling, and a private canonical shared
plan. Reusing that plan against another session still resolves aliases and
classes against the session being run; request sealing does not capture an
environment's synonym meanings.

The common type tree now survives environment-dependent kind checking, synonym
elaboration, and capture-safe class-method instantiation. At this stage only,
the alias-free goal and instantiated methods were still projected to `HType`
immediately beside the historical formula translator; the follow-on formula
authority pass below removes that last stable-path projection. The public raw
`HType` query APIs remain
compatibility boundaries: they perform their established diagnostic preflight,
project checked ordinary types into the shared IR, and delegate to the same
native worker. Regression coverage compares raw and native prepared-core
results, preserves the exact neutral request view, and reuses one alias-bearing
request against sessions with different alias definitions.

## Post-fold synonym-saturation authority review

**Completed on 2026-07-15:** the first native Djinn query pass retained a
second recursive shared-type saturation walker beside the opaque shared
`TypeSynonyms` table. It also looked up aliases linearly through a separately
projected list of string names and arities. The two implementations shared
diagnostic text by convention rather than a semantic authority.

The synthesis layer now exposes a non-expanding minimum-saturation preflight
on `TypeSynonyms` itself. It checks application heads before arguments and
forall constraints before bodies, rejects partial aliases, and deliberately
leaves overapplication to kind inference. Djinn's native path consumes that
operation plus the exact Inventory kind assumptions; its duplicate shared-tree
walker is gone. The raw `HType` path keeps one compatibility traversal because
malformed names and declaration-only nodes must not overtake its historical
saturation diagnostic.

Elaboration now also lets `KindInference.checkTypesKinds` perform the sole
structural validation in each pre/post-expansion phase, remapping its explicit
invalid-type result back to the established phase-tagged error. Raw and native
labeled kind batches share one reporter. Regressions pin bare, nested,
saturated, overapplied, head-first, forall-context, phase-classification, and
raw declaration-node precedence behavior.

## Post-fold formula compilation authority review

**Completed on 2026-07-15:** Djinn's last stable-path representation detour was
the alias-free `Type String -> HType -> Formula` conversion. The formula engine
also occupied nearly six hundred lines of the exposed `HTypes` compatibility
module even though its expansion provenance, cycle validation, and logical
lowering are independent of either source-type tree.

`Djinn.Internal.TypeFormula` is now a package-private compiler driven by a
one-layer type view. Raw `HType` and native shared types enter the same opaque
prepared definition table, so stable goals, instantiated class methods, and
ordered global assumptions remain in `Type String` through formula
compilation. The compiler is built from the exact prepared Inventory stream:
synonym definitions retain their checked bodies, while all other declarations
reuse the alias-expanded forms already computed for datatype-recursion
classification. Historical raw formula APIs remain checked compatibility
wrappers and preserve their first-binding, malformed-prefix-arrow, expansion
cycle, and diagnostic-order contracts.

The shared zero-field boxed tuple is deliberately viewed as the nominal `()`
constructor rather than structural truth. Regressions pin its translated
formula `(|true)`, first proof `Inj0 Tuple0`, and rendered clause, plus explicit
raw/native formula equality for aliases, lists, tuples, abstract and empty
types, datatype constructor order, and unary tuple fields. Global premise tests
also cover source order that differs from map order and bare operator symbols;
the integration operator-method fixture now has an inhabited domain and a
no-context negative control, making that method logically necessary.

## Post-fold compiler-support alignment

**Completed on 2026-07-15:** Djex's package metadata named GHC 9.12.4 as its
sole tested compiler while retaining `base >= 4.19 && < 5`, an intentionally
broad range that still admitted several unsupported compiler release series.
That portability claim no longer matched the workspace policy or the two
sibling packages.

All three workspace packages now use `base == 4.21.2.*`, the API line bundled
with the installed GHC 9.12.4 toolchain. `Tested-With: GHC == 9.12.4` continues
to carry the exact compiler identity because a `base` version is not a unique
compiler identifier. Other dependency bounds remain ranges so
`--prefer-oldest` continues to validate the actual declared compatibility
surface on GHC 9.12.4.

## Post-fold request-sealing authority follow-up

**Completed on 2026-07-15:** after provenance storage moved into the shared
`CachedQuery`, the Djinn and Exference constructors still independently
implemented the same non-obvious sealing protocol: force a source location
before inspecting validation, attach it to every input failure, publish the
adapter's chosen neutral request, and leave its private cache lazy. The two
copies were behaviorally tested but could still drift.

`sealCachedQueryWithProvenance` now owns that protocol in the synthesis query
layer. Djinn supplies its exact caller-visible request plus canonical shared
plan; Exference supplies its canonical request plus checked source-name hints.
Foundation regressions independently pin successful projections, sourced and
programmatic failure attribution, eager source-span materialization, and lazy
backend caches, while the existing adapter integration tests continue to pin
their diagnostic phase policies.

## Validation gates for each stage

Every migration milestone should retain the current release-style gates:

- focused unit/property tests for touched core operations;
- all Djex test suites and both benchmarks compiled with `-Werror`;
- the supported GHC 9.12.4 build and test graph;
- `--prefer-oldest` resolution for the declared bounds;
- `cabal check`, source-distribution inspection, and Haddock generation; and
- a clean tracked tree with the milestone commit pushed before the next
  representation is removed.

The next architecture decision is whether compatibility merits keeping the two
frontend libraries as explicit dependency-isolation boundaries. Folding either
one into `djex` would broaden every client with Haskeline or HSE/filesystem
dependencies; deleting one instead would retire a historical public API. Until
that policy is chosen, further convergence should target source duplication and
shared behavior rather than erasing a boundary that still enforces a real
dependency invariant.
