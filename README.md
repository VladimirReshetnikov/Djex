# Djex

Djex is the codename for the synthesis tool being formed by merging Djinn and
Exference. The name contracts **Dj**inn and Exference's **ex**. It is one Cabal
package: both independently testable engines and their neutral foundation now
share a version, source distribution, data-file layout, and build graph while
their duplicated frontend, validation, environment, search-envelope, and
generated-output infrastructure is progressively consolidated.

## Components

- The unnamed `djex` library is the complete parser-independent product. Its
  `src/`, `synthesis/src/`, `djinn/src-core/`, and `exference/src-core/` source
  roots compile into one unit with one dependency closure.
  `Language.Haskell.Djex` is the curated neutral entry point;
  `Language.Haskell.Djex.Djinn` and
  `Language.Haskell.Djex.Exference` run both engines through the shared
  query/evidence/search envelope. All modules formerly exposed by the three
  parser-free sublibraries remain exposed by `djex` for import compatibility.
- `synthesis/` is the neutral foundation source area: validated names, types,
  kinds, declarations, environments, diagnostics, generated output, and
  operational search status.
- `djinn/` contributes the LJT proof engine and checked adapter to `djex`, plus
  the named `djinn-frontend` compatibility/REPL library. The frontend
  re-exports both `Djinn.Core` and `Language.Haskell.Djex.Djinn`.
- `exference/` contributes the heuristic search engine and checked adapter to
  `djex`, plus the named `exference-frontend` Haskell-source/environment-loader
  library. The frontend also owns `Language.Haskell.Exference.CLI`; the
  executable component is only its six-line launcher.

The `djex` executable is the merged one-shot frontend and selects either checked
backend explicitly. The historical `djinn` and `exference` executable names
remain available for their REPL and compatibility contracts. The package also
retains the facade, integration, backend, property, CLI, and benchmark suites;
this preserves differential testing while the two engines continue converging.

The parser-free package is now genuinely one library rather than a facade over
three separately compiled internal units. Only the two compatibility
frontends remain named libraries, preserving useful Haskeline and HSE
dependency isolation. The current duplication audit and the ordered path to
this state are recorded
in [the 2026-07-14 remaining-convergence audit](docs/reports/2026-07-14-remaining-convergence-audit.md).
That report captures the starting point for the current work; its Priority 1
native-vocabulary and Priority 2 result-envelope migrations are now complete.
Exference's `HsType` is an alias for the shared `Type (Variable Int)`, with
compatibility patterns over the native tree and one canonical structural
representation for saturated functions and tuples. Both engines now construct
their stable `QueryResult` payloads in the core: Djinn preserves its richer
  logical evidence, while Exference derives evidence from each lazy candidate
  batch after one checked query preparation. Exference source checking and both
  stable sessions now make their shared inventories authoritative; Djinn also
  seals its ordered global proof premises and class lookup from that Inventory
  without retaining raw backend tables. The parser-free fold then deleted the
  obsolete `synthesis`, `djinn-core`, and `exference-core` component identities
  without changing their Haskell module names.

## Dependency migration

The single-package layout intentionally replaces the three former package
identities. Existing Cabal dependencies migrate as follows:

| Former dependency | Djex dependency |
| --- | --- |
| `haskell-synthesis` | `djex` |
| `djex:synthesis` | `djex` |
| `djinn:djinn-core` or `djex:djinn-core` | `djex` |
| unnamed `djinn` library | `djex:djinn-frontend` |
| `exference:exference-core` or `djex:exference-core` | `djex` |
| unnamed `exference` library | `djex:exference-frontend` |

New parser-free clients use one unnamed `djex` dependency for the curated
facade, shared synthesis vocabulary, checked adapters, and lower-level engine
modules. Source-loading or REPL clients add `djex:exference-frontend` or
`djex:djinn-frontend` explicitly. Build-tool dependencies for the historical
commands remain
`djex:djinn` and `djex:exference`; their executable names are unchanged.
The parser-free library has no `haskell-src-exts`, `directory`, `filepath`, or
`haskeline` dependency.

Both compatibility components re-export their stable checked adapter:
`djinn-frontend` provides `Language.Haskell.Djex.Djinn`, and
`exference-frontend` provides `Language.Haskell.Djex.Exference` alongside its
source-loading extension. Clients therefore do not need a redundant direct
dependency on `djex` merely to name the adapter through a frontend dependency.

The filesystem and Cabal-project migration is equally deliberate:

- the former top-level `synthesis/`, `djinn/`, and `exference/` trees now live
  at `djex/synthesis/`, `djex/djinn/`, and `djex/exference/`;
- both backend trees follow the same live layout: `src-core/`,
  `src-frontend/`, `app/`, and one explicit directory per test suite. The
  package root similarly uses `src/`, `app/`, `test-integration/`,
  `test-facade/`, `test-parser-free-api/`, and `test-cli/`;
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

The complete package graph is tested warning-clean on GHC 9.8.4 and GHC
9.12.4. The declared `base >= 4.19` floor is therefore exercised by GHC 9.8.4,
not merely inferred from dependency metadata. GHC 9.12.4 remains the preferred
local toolchain because it is the newest installed compiler with full Haskell
Language Server support. To reproduce the lower-compiler check without
changing the selected compiler, use an isolated build directory:

```console
cabal build all -w ghc-9.8.4 --builddir=dist-newstyle-ghc-9.8.4 --enable-tests --enable-benchmarks --ghc-options=-Werror
cabal test all -w ghc-9.8.4 --builddir=dist-newstyle-ghc-9.8.4 --enable-tests --ghc-options=-Werror --test-show-details=direct
```

The project pins the Hackage index snapshot used by the solver, while
`djex.cabal` retains explicit dependency ranges for library consumers and
lower-bound testing. Update that snapshot only as part of a dependency review;
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
cabal build djex:lib:djex djex:lib:djinn-frontend djex:lib:exference-frontend
cabal run exe:djex -- djinn --render expression "a -> a"
cabal run exe:djex -- exference --select first "a -> a"
cabal run djinn
cabal run exference -- --first "a -> a"
cabal bench djinn-bench
cabal bench exference-bench
```

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
`CachedQuery` separately owns a strict `RequestProvenance` and the adapter's
lazy derived cache. Parsed requests materialize their complete neutral
`SourceLocation` while sealing, avoiding retention of the input buffer;
programmatic requests carry an explicit source-free provenance. Equality and
display continue to observe only the neutral request.
`mkDjinnSession` lowers and seals the neutral shared
`DjinnEnvironment` through one authoritative closed Inventory with Haskell 98
class-kind defaulting. An opaque shared `PreparedInventory` keeps that
Inventory and its exact normalized synonym table inseparable; the mutable raw
`Djinn.Core.Environment` no longer crosses the
curated facade or survives inside `PreparedEnvironment`. Synonyms are expanded
for saturation and recursive datatype validation before ordered global
assumptions are translated once into proof premises. Class lookup, kinds,
synonyms, formula definitions, and those premises are private indexes of the
same Inventory; historical raw declaration tables are derived only when a
compatibility caller asks to display them. At query time Djinn elaborates the
goal and all class arguments as one shared kind scope, translates only the goal
and instantiated class methods, then sends the alias-free projection to proof
search; opaque requests still retain their exact session-independent source
view. Invalid search controls now have a typed core failure and stable
`DJEX_DJINN_OPTIONS` diagnostic; query-type provenance is attached only to
source-derived input rejection, never to separately supplied options or an
internal proof/result invariant.
`standardDjinnSession` converts the historical built-in spelling once and then
uses the same neutral `mkDjinnSession` path as every caller-supplied environment.
`parseDjinnRequest` shares the compatibility frontend's optional class-context
grammar through the full-consumption `Djinn.Core.parseContextualHType` entry
point rather than importing an internal parser. Both `DjinnRequest` and
`DjinnCandidate` expose `DjinnType = Type DjinnTypeVariable`; that shared type
is checked and lowered once by `mkDjinnRequest`, which seals the neutral query
and its raw projection behind an opaque request exactly as Exference does.
Parsed raw types travel in the opposite direction and are checked into the
shared IR; `djinnRequestQuery` recovers the stable source view. The query
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
signature nameable without depending on a hidden backend alias. The historical
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
the same prepared synonym table and backend during sealing. The sealed session
then retains only the shared inventory/synonym witness, its policy-adjusted
checked search environment, and a fully materialized structured omission
summary; the complete unfiltered backend-bearing wrapper is released. HSE
query parsing derives known types and class arities
from the witness's shared inventory rather than retaining parallel type/class
caches; neither an HSE source environment nor its legacy synonym map survives
sealing.

`ExferenceSessionPolicy` applies exact structural-name exclusions and finite,
signed rating overrides while the private search projection is sealed.
Overrides neither reorder declarations nor leak into the annotation-erased
public inventory. Unknown names and non-finite ratings are fatal structured
diagnostics; exclusion wins when both policies mention the same binding.
Unsupported rank-N introduction/elimination and recursive-data elimination
capabilities remain visible as structured omissions and warning diagnostics
instead of disappearing per query. Omission order follows introduction order
and then elimination order.

`ExferenceRequest` is opaque in the same operational sense as `DjinnRequest`:
the stable adapter exposes only `mkExferenceRequest` and
`exferenceRequestQuery`. Source locations and parsed variable spellings are
private presentation data, while the common `CachedQuery` owns the strict
location separately from the backend-specific spelling cache. Neither affects
equality/display nor admits an unchecked construction path through the curated facade. The parser-free
library's hidden `Language.Haskell.Djex.Exference.Internal.Request`
representation owns that metadata. Source-adapter authors opt into the explicit parser-neutral
`Language.Haskell.Djex.Exference.FrontendSupport` service-provider interface.
Its checked wrappers also expose only the session vocabulary, prepared-session
sealing, source-aware request construction, target preflight, and
collision-safe variable allocation needed at that boundary. It is exposed
directly by `djex`, but deliberately not re-exported by
`Language.Haskell.Djex` or the frontend library.
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

Clients that load directories or parse Haskell type text add the frontend
component and its explicit source boundary:

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
options.
Candidate selection and rendering remain presentation policies outside both
session operations. The shared `Selection` module now
provides first, global-best, streaming-all, batch-lookahead, and preferred-tier
lookahead policies over either backend's result envelope. `TypeRender` prints
shared types and constraints from tagged variable-name hints without collapsing
flexible and rigid identities.
`TypeSynonym` prepares aliases from the retained neutral inventory and owns
capture-avoiding, saturation-checked expansion plus the pre/post kind checks
that both backend adapters can now share. Its batch operation preserves source
order while assigning one kind to each free variable shared by a goal and its
separate context arguments. The underlying shared type module now owns the
scope-aware simultaneous substitution primitive used by synonym expansion and
Exference's compatibility substitution API. Exference's backend-specific
unifiers operate directly on the same native tree, canonicalize their inputs
and projected substitutions, preserve flexible/rigid and left/right identity,
and treat structural functions and tuples through the same applicative kernel.

The `exference` compatibility executable is a six-line launcher for
`Language.Haskell.Exference.CLI` in the frontend component. That module is the
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
