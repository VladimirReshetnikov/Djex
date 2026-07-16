# Djex

Djex is the codename for the synthesis tool being formed by merging Djinn and
Exference. The name contracts **Dj**inn and Exference's **ex**. It is one Cabal
package with one library: both independently testable engines, compatibility
frontends, and their neutral foundation share a version, dependency contract,
source distribution, data-file layout, and build graph while duplicated
validation, environment, search-envelope, and generated-output infrastructure
is progressively consolidated.

## Components

- The unnamed `djex` library is the complete product. Its `src/`,
  `synthesis/src/`, both `src-core/` roots, and both `src-frontend/` roots
  compile into one unit with one dependency closure.
  `Language.Haskell.Djex` is the curated neutral entry point;
  `Language.Haskell.Djex.Djinn` and
  `Language.Haskell.Djex.Exference` run both engines through the shared
  query/evidence/search envelope. All modules formerly exposed by the three
  parser-free sublibraries remain exposed by `djex` for import compatibility.
- `synthesis/` is the neutral foundation source area: validated names, types,
  kinds, declarations, environments, diagnostics, collision-free allocation,
  generated output, and operational search status.
- `djinn/` contributes the LJT proof engine, checked adapter, historical
  `Djinn` API, and Haskeline REPL to that library.
- `exference/` contributes the heuristic search engine, checked adapter,
  Haskell-source/environment loader, and historical CLI API. The executable
  component remains only its six-line launcher.

The `djex` executable is the merged one-shot frontend and selects either checked
backend explicitly. The historical `djinn` and `exference` executable names
remain available for their REPL and compatibility contracts. The package also
retains facade coverage inside its downstream API suite, plus the integration,
backend, property, CLI, and benchmark suites; this preserves differential
testing while the two engines continue converging.

The package is now genuinely one library rather than a facade over five
separately compiled internal units. The final frontend fold deliberately trades
Haskeline/HSE dependency isolation for the requested single dependency and
version contract; parser-independent module boundaries remain visible in the
source graph. The current duplication audit and the ordered path to this state
are recorded
in [the 2026-07-14 remaining-convergence audit](docs/reports/2026-07-14-remaining-convergence-audit.md).
That report captures the starting point for the current work; its Priority 1
native-vocabulary and Priority 2 result-envelope migrations are now complete.
Exference's `HsType` is an alias for the shared `Type (Variable Int)`, with
compatibility patterns over the native tree and one canonical structural
representation for saturated functions and tuples. Djinn's `HKind` is a
private-representation compatibility newtype over shared `Kind Int`; bundled
patterns preserve `HKind(..)` imports and the historical `*`/`kN` rendering,
while all kind bridging and grounding operate on the single shared tree.
Prepared Djinn kind-check caches and their trusted Inventory bridge are private
implementation details rather than authority exposed by the raw compatibility
module. Djinn's LJT lowering likewise constructs and simplifies the shared
generated `Expression`/`Pattern` tree directly; `HExpr` and `HPat` remain only
as projections for historical low-level callers. Djinn's raw and native class
contexts also share one sealed class lookup and capture-avoiding method
instantiation; the raw API projects only its final methods back to `HType`,
and the obsolete internal raw substitution and class-index projections have
been removed. Raw `Djinn.Core` declaration edits likewise convert once to the
shared environment, use the same checked transaction as stable sessions, and
project the fully sealed result back only after success; the former parallel
association-list mutation engine is gone.
Both engines now construct
their stable `QueryResult` payloads in the core: Djinn preserves its richer
  logical evidence, while Exference derives evidence from each lazy candidate
  batch after one checked query preparation. Exference source checking and both
  stable sessions now make their shared inventories authoritative. Exference's
  stable and compatibility environment projections also share one declaration
  traversal, with an explicit policy deciding whether constructor and class-
  method bindings are derived or retained only from the compatibility input.
  Djinn also
  seals its ordered global proof premises and class lookup from that Inventory
  without retaining raw backend tables. The component folds then deleted the
  obsolete `synthesis`, `djinn-core`, `exference-core`, `djinn-frontend`, and
  `exference-frontend` library identities without changing their Haskell
  module names.

## Dependency migration

The single-package layout intentionally replaces the three former package
identities. Existing Cabal dependencies migrate as follows:

| Former dependency | Djex dependency |
| --- | --- |
| `haskell-synthesis` | `djex` |
| `djex:synthesis` | `djex` |
| `djinn:djinn-core` or `djex:djinn-core` | `djex` |
| unnamed `djinn` library or `djex:djinn-frontend` | `djex` |
| `exference:exference-core` or `djex:exference-core` | `djex` |
| unnamed `exference` library or `djex:exference-frontend` | `djex` |

All library clients use one unnamed `djex` dependency for the curated facade,
shared synthesis vocabulary, checked adapters, lower-level engines, source
loading, and the historical REPL API. Build-tool dependencies for the commands
remain `djex:djinn` and `djex:exference`; their executable names are unchanged.
The one library consequently has the union of core and frontend dependencies:
`haskell-src-exts`, `directory`, `filepath`, and `haskeline` now share the same
versioned component contract as the engines that consume their output.

The filesystem and Cabal-project migration is equally deliberate:

- the former top-level `synthesis/`, `djinn/`, and `exference/` trees now live
  at `djex/synthesis/`, `djex/djinn/`, and `djex/exference/`;
- both backend trees follow the same live layout: `src-core/`,
  `src-frontend/`, `app/`, and one explicit directory per test suite. The
  package root similarly uses `src/`, `app/`, `test-integration/`,
  `test-api/` (including the curated-facade import guard), and
  `test-cli/`;
- their separate package descriptions and project files have been replaced by
  `djex/djex.cabal`; the repository-root `cabal.project` is the single solver
  root, and Cabal discovers it by walking to the parent when invoked here;
- package-generated code must import `Paths_djex` instead of `Paths_djinn` or
  `Paths_exference`; version discovery and installed-data lookup now belong to
  Djex as a whole; and
- Exference's installed environment is a Djex data directory. Use
  `getDataFileName "exference/environment"` from `Paths_djex`, rather than
  assuming either a checkout-relative path or the old package data root.

The root Cabal project enables tests and benchmarks, so `cabal build all` and
`cabal test all` exercise the same component graph whether invoked at the
repository root or in `djex/`, with one solver plan and build cache.

## Building

The repository root contains the canonical Cabal project; Cabal discovers that
parent project when commands start in this directory. From either location,
build and test the complete graph:

```console
cabal build all
cabal test all --test-show-details=direct
```

The complete package graph is tested warning-clean on, and currently supports,
GHC 9.12.4. It is the active toolchain because it has full Haskell Language
Server support. `Tested-With: GHC == 9.12.4` states the exact compiler contract;
the matching `base == 4.21.2.*` bound pins the library API line bundled with
that toolchain. A `base` version is not itself a unique compiler identity, so
both declarations are intentional.

The project pins the Hackage index snapshot used by the solver, while
`djex.cabal` retains explicit dependency ranges for library consumers and
solver checks. Update that snapshot only as part of a dependency review;
this keeps otherwise identical checkouts on one package universe without
turning the package metadata into an application-style version lock.

The complete component graph and test matrix are also expected to work with
the oldest dependency versions permitted by those ranges on the supported GHC:

```console
cabal build all --prefer-oldest --builddir=dist-newstyle-oldest
cabal test all --prefer-oldest --builddir=dist-newstyle-oldest --test-show-details=direct
```

Use an isolated build directory as above so lower-bound validation cannot
replace the ordinary latest-compatible build plan.

Useful component and compatibility-executable targets include:

```console
cabal build djex:lib:djex
cabal run exe:djex -- djinn --render expression "a -> a"
cabal run exe:djex -- exference --select first "a -> a"
cabal run djinn
cabal run exference -- --first "a -> a"
cabal bench djinn-bench
cabal bench exference-bench
```

All three commands are serial and accept explicit caller-supplied `+RTS`
resource tuning, for example `+RTS -K64m -RTS`. None starts one capability per
core or inherits a fixed multi-gigabyte heap hint.

The Exference benchmark uses parser-free, explicitly step- and queue-bounded
core fixtures. For an optimization-sensitive comparison of the search engine,
build its whole local dependency graph consistently with
`cabal bench exference-bench --enable-optimization=2`.

The backend subdirectories are source roots, not independent Cabal projects;
run package commands from the repository root or from `djex/`.

## Unified command

Djex never guesses which engine's semantics were intended:

```console
djex djinn [OPTION...] TYPE
djex exference [OPTION...] TYPE
```

Both subcommands share `--target`, `--select first|best|all`,
`--render definition|expression`, and
`--qualification none|identifiers|full`. Djinn additionally exposes its
candidate limit and choice-point budget; Exference exposes its checked source
environment, constraint/pattern policy, and step, queue, and depth bounds.
Run `djex <backend> --help` for the exact backend-specific table.

Generated Haskell alone is written to stdout. Loader messages, logical negative
answers, undecided/truncated status, and structured diagnostics go to stderr.
Help, version, successful synthesis, proof-backed uninhabitability, and bounded
no-result searches exit 0; load/parse/search/render failures exit 1; malformed
command lines exit 2. This keeps a valid negative answer distinct from a broken
invocation and never mislabels an exhausted Exference heuristic search as a
proof of uninhabitability.

## Query boundary

Exference core names now are the shared synthesis `Name` itself; `QualifiedName`
is a compatibility alias rather than another nominal wrapper. Compatibility views
remain separately exported patterns (`QualifiedName`, `ListCon`, `TupleCon`,
`UnboxedTupleCon`, and `Cons`), while ordinary names and boxed tuples are still
constructed with `mkQualifiedName` and `mkBoxedTupleName` so malformed source
spelling, qualification, or tuple arity receives a structured error. Exference
constraints likewise are `Constraint HsType`, with `HsConstraint` retained as
a compatibility pattern. `HsType` itself is now exactly the shared
`Type (Variable Int)`. Its historical constructors survive as separately
exported patterns; `TypeForall` preserves the old flexible-binder-only view,
while the exhaustive `TypeForallNative` exposes every shared binder. Checked
Exference boundaries canonicalize saturated function and tuple applications
to structural `FunctionType` and `TupleType` values and reject rigid forall
binders, which the search engine cannot instantiate as source quantifiers.
The old structural conversion functions remain identity compatibility shims;
the checked conversion names now serve only as validation and canonicalization
boundaries.

`Language.Haskell.Synthesis.Query` shares the target, goal, contexts, logical
evidence, and search-batch shape without pretending that both engines accept
the same types or options. Every `QueryRequest` carries an opaque
`DefinitionName`, constructed once from a structural `Name`; it guarantees the
target is an unqualified value identifier or operator other than the wildcard,
so neither backend can defer or disagree about that shared output invariant.
Raw-name parser helpers retain their compatibility signatures and perform this
check before parsing, preserving usage-error precedence. Its `QueryResult`
constructor is opaque:
`mkQueryResult` checks that `ValidatedCandidates` accompanies exactly the
nonempty batches, while `queryResultFromCandidates` derives that evidence for
ordinary heuristic-search batches. Both checks inspect only the list spine's
first constructor, so they do not sacrifice lazy candidate tails.
Opaque invariant witnesses do not derive representation-producing classes such
as `Generic`. Their exported observations are ordinary functions rather than
record fields whenever record update could split checked state: this applies to
the shared environment, inventory, synonym table, and result envelope, as well
as the corresponding rigid-instantiation, class-index, scope, and proof-state
witnesses in the backends. Hiding a constructor alone is insufficient because
`Generic.to` can rebuild its representation and GHC record update needs an
exported field label, not the constructor. The downstream API suite keeps both
reconstruction routes closed.
`CachedQuery` separately owns a strict `RequestProvenance` and the adapter's
lazy derived cache. Parsed requests materialize their complete neutral
`SourceLocation` while sealing, avoiding retention of the input buffer;
programmatic requests carry an explicit source-free provenance. Equality and
display continue to observe only the neutral request. Both adapters enter
`sealCachedQueryWithProvenance`, so validation failures receive the same
source attribution and strictness contract without duplicating that subtle
protocol in their private request constructors; backend caches remain lazy.
`mkDjinnSession` lowers and seals the kind-ground neutral shared
`DjinnEnvironment = Environment DjinnTypeVariable Void ()` through one
authoritative closed Inventory with Haskell 98 class-kind defaulting. Djinn's
historical `Kind Int` remains only in its raw compatibility API; a surviving
kind variable was never valid session state, so the stable path no longer
weakens and re-grounds an already ground environment. An opaque shared
`PreparedInventory` keeps that
Inventory and its exact normalized synonym table inseparable; the mutable raw
`Djinn.Core.Environment` no longer crosses the
curated facade or survives inside `PreparedEnvironment`. Synonyms are expanded
for saturation and recursive datatype validation before ordered global
assumptions are translated once into proof premises. Class lookup, kinds,
synonyms, formula definitions, and those premises are private indexes of the
same Inventory; historical raw declaration tables are derived only when a
compatibility caller asks to display them. At query time Djinn elaborates the
goal and all class arguments as one shared kind scope, instantiates class
methods in that same shared type tree, and compiles the alias-free goal and
methods directly into formulas through one representation-neutral prepared
definition cache; opaque
requests still retain their exact session-independent source view. Invalid
search controls now have a typed core failure and stable
`DJEX_DJINN_OPTIONS` diagnostic; query-type provenance is attached only to
source-derived input rejection, never to separately supplied options or an
internal proof/result invariant.
`standardDjinnSession` converts the historical built-in spelling once and then
uses the same neutral `mkDjinnSession` path as every caller-supplied environment.
`parseDjinnRequest` shares the compatibility frontend's optional class-context
grammar through the full-consumption `Djinn.Core.parseContextualHType` entry
point rather than importing an internal parser. Both `DjinnRequest` and
`DjinnCandidate` expose `DjinnType = Type DjinnTypeVariable`; that shared type
is checked and canonicalized once by `mkDjinnRequest`, which seals an opaque
shared execution plan while retaining the caller's exact neutral query.
Parsed `HType` values already store ordinary source types in that shared IR;
the checked boundary validates and canonicalizes their native tree while
retaining declaration-only and constructor-sensitive caller-built forms for
compatibility diagnostics. `djinnRequestQuery` recovers the exact stable
source view. The
plan remains shared through environment-dependent kind checking, synonym
elaboration, class-method instantiation, and formula compilation. The query
returns shared candidates containing
structured generated
clauses, empty residual constraints, and Djinn's unused-binder ranking details
in one terminal batch. A proof beyond `optionCutoff` produces
`Truncated CandidateLimitReached` without forcing the proof-stream suffix.
Proof-backed `ProvedUninhabitable`, target-reference evidence, and
budget-limited `NoEvidence` remain distinct from the batch's operational
`Finished` or `Truncated` completion.

The one-import `Language.Haskell.Djex` surface reexports the complete neutral
declaration, environment, inventory, kind-inference, synonym-elaboration, and
type-rendering vocabulary. `DjinnEnvironment`, `DjinnInventory`,
`DjinnTypeVariable`, `DjinnLocal`, and `DjinnType` make every Djinn adapter
signature nameable without depending on a hidden backend alias. Both stable
environment aliases now use `Void` for explicit kind variables, making their
common ground-kind contract visible in types. The historical
REPL now derives the editable shared environment from the grounded Inventory,
edits it transactionally, and publishes only the completely resealed opaque
session; raw declarations are one-way parser inputs and private derived
display/search views, not retained editable state.
Exference now has the same stable construction boundary:
`ExferenceEnvironment = Environment ExferenceTypeVariable Void ()`, and
`mkExferenceSession` kind-checks and lowers that parser-independent environment
directly. Its source loader is an additional frontend, not the definition of
an Exference session.

`Language.Haskell.Djex.Exference.HaskellSrc.loadExferenceSession` and its
policy-aware counterpart compute Exference's
backend-supported projection once and turn a directory into the same opaque
session, while translating every fatal loader phase into source-preserving
`EXF_*` diagnostics. Stable callers therefore never handle a parser-specific
checked environment. The explicitly named
`Language.Haskell.Exference.Session` module retains the raw
`CheckedSourceEnvironment` bridge for the historical CLI and clients that opt
into the compatibility frontend. The source boundary tags class methods with
their qualified owner, nests them under the common class declaration for
validation, and lowers each rated selector exactly once into Exference's flat
search inventory without changing source order. HSE aliases remain unexpanded
through common Inventory kind checking; the same neutral lowering used by
programmatic sessions then expands them capture-safely, normalizes classes and
instances, and derives cross-module recursion before source ratings/order are
reapplied. Source checking returns one opaque annotated witness which owns the
checked Inventory, synonym table, and backend together; the frontend can
reorder the exact checked names and attach finite ratings, but cannot combine
an inventory with an independently prepared search dictionary. Alias-aware
recursion metadata is attached to that Inventory without resealing it or
repeating kind inference. The historical flat `SourceEnvironment` projection
is derived on demand from the witness; only legacy synonym spellings remain as
frontend presentation data. Erasing annotations for a stable session shares
the same prepared synonym table and backend during sealing. That wrapper
exposes one foundation witness and one backend projection: the synthesis
foundation remains the sole owner of the Inventory and normalized-synonym
projections, rather than Exference mirroring them through neutral aliases. The
sealed session then retains only the shared inventory/synonym witness, its
policy-adjusted checked search environment, and a fully materialized structured
omission summary; the complete unfiltered backend-bearing wrapper is released.
HSE query parsing derives known types and class arities
from the witness's shared inventory rather than retaining parallel type/class
caches; neither an HSE source environment nor its legacy synonym map survives
sealing.

`ExferenceSessionPolicy` applies exact structural-name exclusions and finite,
signed rating overrides while the private search projection is sealed.
Overrides neither reorder declarations nor leak into the annotation-erased
public inventory. `exferenceSessionEnvironment` and
`exferenceSessionInventory` expose the unchanged authoritative views in
parallel with Djinn's stable session API. Unknown names and non-finite ratings
are fatal structured diagnostics; exclusion wins when both policies mention
the same binding.
Unsupported rank-N introduction/elimination and recursive-data elimination
capabilities remain visible as structured omissions and warning diagnostics
instead of disappearing per query. Omission order follows introduction order
and then elimination order.

`ExferenceRequest` is opaque in the same operational sense as `DjinnRequest`:
the stable adapter exposes only `mkExferenceRequest` and
`exferenceRequestQuery`. Source locations and parsed variable spellings are
private presentation data. The common `CachedQuery` owns the strict location
separately from an opaque backend-specific variable-to-spelling value. The
source SPI validates every raw alias as a non-wildcard variable identifier,
bounds its ID to the complete contextual goal, collapses aliases
deterministically, retains the exact canonical goal as a scope witness, and
detaches the map before returning. Shared synonym elaboration alpha-freshens
every alias-introduced binder away from the complete original source
namespace, including through nested and zero-argument aliases. The adapter can
therefore retarget surviving hints to the elaborated goal without confusing an
erased phantom argument with an unrelated same-numbered binder, and core search
rejects a hint value paired with any other query.
Neither provenance nor hints affects equality/display or admits an unchecked
construction path through the curated facade. The library's hidden
`Language.Haskell.Djex.Exference.Internal.Request`
representation owns that metadata. Source-adapter authors opt into the explicit parser-neutral
`Language.Haskell.Djex.Exference.FrontendSupport` service-provider interface.
Its checked wrappers also expose only the session vocabulary, prepared-session
sealing, source-aware request construction, target preflight, and
collision-safe variable allocation needed at that boundary. It is exposed
directly by `djex`, but deliberately not re-exported by
`Language.Haskell.Djex`.
HSE's normalized parse filename is also the filename retained for deferred
diagnostics, so extensionless labels no longer change identity between parse
and search phases; angle-bracket virtual-buffer names remain verbatim.

The Haskell-source loader is likewise fail-closed at its vocabulary boundary:
after parsing, but before constructing any partial inventory, it reports
source-ordered `UnsupportedVocabularyOccurrence` values for type/data families,
GADTs, datatype contexts, explicitly kinded parameters, existential or
constrained constructors, derived or overlapping instances, functional
dependencies, associated families and defaults, declaration splices, role
annotations, and XML hybrid modules. Each occurrence carries the stable
`EXF_UNSUPPORTED_VOCABULARY` diagnostic code and its exact source span.
Ordinary positional, infix, record, strict, and unpacked datatype fields are
lowered explicitly; record selectors become rated value bindings exactly once.
Imports, fixities, ordinary value and method bodies, pattern vocabulary,
default declarations, and operational pragmas remain accepted because they do
not change the nominal type/class inventory. These forms are explicit current
limitations rather than syntax that can silently disappear during loading.
`Language.Haskell.Djex.Exference.HaskellSrc.parseExferenceRequest` resolves
Haskell syntax against the session's retained
type names, classes, and kind assumptions. It deliberately does not use the
legacy frontend synonym map. `runExferenceQuery` passes both parsed and
programmatically constructed goals through the shared capture-avoiding
`TypeSynonym.elaborateType` operation, including its pre- and post-expansion
kind checks, before lowering to the core search type. Thus the two request
paths agree on aliases, cycles, saturation, and kinds. Query execution then
validates only the varying search policy and returns a lazy sequence of shared
result batches. `ExferenceEnvironment`, `ExferenceType`,
`ExferenceTypeVariable`, `ExferenceLocal`, and `ExferenceInventory` make that
complete surface nameable in the neutral IR. Session construction maps backend
ratings out of the already-checked inventory without rebuilding its indexes or
kind assumptions. Stable candidate details and batch metadata are zero-copy
public views of the exact core-owned values, so the curated facade exposes
shared names, types, metrics, and rendering hints without maintaining a second
set of records.

Programmatic clients need only the neutral adapter:

```haskell
import Language.Haskell.Djex.Exference
```

Clients that load directories or parse Haskell type text import the explicit
source boundary from the same dependency:

```haskell
import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.HaskellSrc
```
Those batches preserve queue/depth pruning, nominal binding usage, residual
constraints, statistics, and rendering hints without forcing the remaining
trace. Each generated expression is wrapped in a target-bearing shared
`FunctionClause` whose opaque `DefinitionName` preserves the checked request
target through result projection. The shared candidate expression/definition
renderers own the common clause projection and return `RenderError` directly;
each backend adapter contributes only its local-name hints and qualification
options. Exference's live search tree is now the same shared `Generated.Expression`
shape as those candidates. Its checker-specific type annotations inhabit a
private local payload, while the historical `ExpVar`/`ExpLambda`/`ExpLet`/case
constructors are bundled bidirectional compatibility patterns over that tree.
Erasing annotations is therefore a functor projection rather than a recursive
backend-to-shared conversion. Incremental hole filling likewise lives in the
shared generated-syntax module and is regression-tested independently of
Exference's search policy. Capture-safe let cleanup and eta reduction operate
on that same shared shape, parameterized only by Exference's projection from
an annotated local to its stable numeric identity; the historical
`ExpressionSimplify` module is now a compatibility re-export rather than a
second traversal authority. The opaque wrapper also exposes one structural
annotation observation derived from the shared tree's `Foldable` order; name
hints, raw-input validation, and flexible-identifier reservation therefore no
longer maintain separate copies of Exference's historical expression grammar.
Candidate selection and rendering remain presentation policies outside both
session operations. The shared `Selection` module now
provides first, global-best, streaming-all, batch-lookahead, and preferred-tier
lookahead policies over either backend's result envelope. `TypeRender` prints
shared types and constraints from tagged variable-name hints without collapsing
flexible and rigid identities.
`TypeSynonym` prepares aliases from the retained neutral inventory and owns
the table-backed minimum-saturation preflight, capture-avoiding expansion, and
pre/post kind checks that both backend adapters can share. Kind inference is
the single structural validator for each elaboration phase. Its batch
operation preserves source order while assigning one kind to each free
variable shared by a goal and its separate context arguments. The underlying
shared type module now owns the
scope-aware simultaneous substitution primitive used by synonym expansion and
Exference's compatibility substitution API. Its batch form lets Exference
substitute constraint arguments under one fresh namespace without encoding
the list as a temporary unboxed tuple. Exference's backend-specific
unifiers operate directly on the same native tree, canonicalize their inputs
and projected substitutions, preserve flexible/rigid and left/right identity,
and consume the foundation's one constructor-application view for structural
functions and tuples. Unary unboxed tuples remain structural because Haskell
has no corresponding unary tuple constructor.

The `exference` compatibility executable is a six-line launcher for
`Language.Haskell.Exference.CLI` in the library. That module is the
compatibility orchestrator at this boundary: it loads and seals one session,
parses every requested type through `parseExferenceRequest`, selects shared
candidates, and renders their generated expression bodies. Exact nominal
session policy replaces its former occurrence-text filtering of recursion
helpers. The compatibility command and `djex exference` now obtain that policy
from the same frontend operation: both exclude `Data.Function.fix`,
`Control.Monad.forever`, and `Control.Monad.Loops.iterateM_` by default, and
both accept `--fix` as an explicit opt-in. The unrestricted programmatic
session default is unchanged. Parse, kind, option, and search
failures are structured diagnostics on stderr with failure exit status;
repeated inputs are all processed and conflicting presentation modes are
rejected. Its historical ranking vector remains an explicit compatibility
profile. `--short` now adds backend-neutral structural expression size to the
candidate cost instead of being a dead option.

The `djinn` compatibility frontend likewise retains its declaration REPL while
storing only the exact sealed `DjinnSession`. Successful mutations edit its
authoritative shared environment and publish a replacement session only after
complete structural, kind, alias, recursion, and backend validation. Historical
`:environment` ordering is derived on demand from the retained Inventory;
instance-method lookup uses its prepared nominal class index, and type queries
reuse the ordered global premise cache. None requires a separately retained or
repeatedly sealed raw environment. Queries and instance methods consume shared
evidence, progress, metadata, and `FunctionClause` output through
`runDjinnQuery`.
Startup-file mode now carries
aggregate failure status across later commands and `:clear`, accepts settings
on either side of filenames, and rejects unknown or ambiguous option prefixes;
interactive recovery remains unchanged.
