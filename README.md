# Djex

Djex is a Haskell expression synthesizer formed by merging
[Djinn](https://github.com/augustss/djinn) and
[Exference](https://github.com/lspitzner/exference). Given a type, it
generates a Haskell expression of that type. Djinn contributes a complete
intuitionistic prover built on Dyckhoff's LJT calculus, so it terminates and
can prove a type uninhabited; Exference contributes a ranked heuristic search
engine with type-class participation and explicit resource controls. Its
nominal instance resolution terminates for accepted rules, but its broader
expression search is not an inhabitation decision procedure. Both
engines, their compatibility frontends, and a shared parser-independent
synthesis foundation compile into one Cabal package with a single library,
version, and dependency contract.

## Start here

- To try the commands, continue with [Building](#building) and the
  [unified-command guide](#unified-command).
- To embed Djex, use the runnable
  [library quick start and API guide](docs/library-api.md).
- To understand ownership, dependency direction, and which modules are stable
  checked surfaces versus compatibility or implementation APIs, read the
  [architecture guide](docs/architecture.md).
- For a concise map of the neutral modules, see the
  [shared synthesis foundation](synthesis/README.md).

New library code should start with `Language.Haskell.Djex`, a narrower checked
backend adapter, or a focused `Language.Haskell.Synthesis.*` import. The package
also exposes historical Djinn and Exference research modules for source
compatibility; an exposed `.Internal.` module belongs to that compatibility
tier, not to the curated stability boundary. Cabal `Other-Modules` remain
private. Djex is currently marked experimental; the architecture guide records
these tiers explicitly.

## Components

- The unnamed `djex` library is the complete product, compiled from `src/`,
  `synthesis/src/`, both `src-core/` roots, and both `src-frontend/` roots.
  `Language.Haskell.Djex` is the curated neutral entry point;
  `Language.Haskell.Djex.Djinn` and `Language.Haskell.Djex.Exference` run
  both engines through the shared query/evidence/search envelope. Their
  sessions are immutable neutral-environment projections; historical Djinn
  declaration edits and instance-method projections stay in its compatibility
  frontend. All
  modules formerly exposed by the three parser-free sublibraries remain
  exposed for import compatibility.
- `synthesis/` is the neutral foundation: validated names, types, kinds,
  declarations, environments, diagnostics, collision-free allocation,
  generated output, and operational search status.
- `djinn/` contributes the LJT proof engine, checked adapter, historical
  `Djinn` API, and Haskeline REPL.
- `exference/` contributes the heuristic search engine, checked adapter,
  Haskell-source/environment loader, and historical CLI API.

The `djex` executable is the merged one-shot frontend and selects either
checked backend explicitly. The historical `djinn` and `exference`
executable names remain available for their REPL and compatibility
contracts. The single library deliberately trades Haskeline/HSE dependency
isolation for one dependency and version contract; parser-independent module
boundaries remain visible in the source graph. Integration, backend,
property, CLI, API, and benchmark suites preserve differential testing while
the two engines continue converging. The latest whole-tree findings,
architectural decisions, and retained semantic differences are recorded in
[the final convergence review](docs/reports/2026-07-17-final-convergence-review.md);
the subsequent raw-checker audit is recorded in the
[checker-boundary follow-up](docs/reports/2026-07-17-checker-boundary-follow-up.md).

## Building

Build and test the complete graph from the repository root:

```console
cabal build all
cabal test all --test-show-details=direct
```

Install the merged command and both historical command names into Cabal's
executable directory with:

```console
cabal install exe:djex exe:djinn exe:exference
```

The package is tested warning-clean on GHC 9.12.4, the active toolchain
because it has full Haskell Language Server support.
`Tested-With: GHC == 9.12.4` states the exact compiler contract, and the
matching `base == 4.21.2.*` bound pins the library API line bundled with
that toolchain; a `base` version is not itself a unique compiler identity,
so both declarations are intentional.

The complete component graph and test matrix are also expected to work with
the oldest dependency versions permitted by the declared ranges. Use an
isolated build directory so lower-bound validation cannot replace the
ordinary latest-compatible build plan:

```console
cabal build all --prefer-oldest --builddir=dist-newstyle-oldest
cabal test all --prefer-oldest --builddir=dist-newstyle-oldest --test-show-details=direct
```

Useful component and compatibility-executable targets:

```console
cabal build djex:lib:djex
cabal run exe:djex -- djinn --render expression "a -> a"
cabal run exe:djex -- exference --select first "a -> a"
cabal run djinn
cabal run exference -- --first "a -> a"
cabal bench djinn-bench
cabal bench exference-bench
```

All three executables are serial and accept explicit caller-supplied `+RTS`
resource tuning, for example `+RTS -K64m -RTS`; none starts one capability
per core or inherits a fixed multi-gigabyte heap hint.

The Exference benchmark uses parser-free, explicitly step- and queue-bounded
core fixtures. For an optimization-sensitive comparison of the search
engine, build its whole local dependency graph consistently with
`cabal bench exference-bench --enable-optimization=2`.

The backend subdirectories are source roots, not independent Cabal packages;
run package commands from the repository root.

## Unified command

Djex never guesses which engine's semantics were intended:

```console
djex djinn [OPTION...] TYPE
djex exference [OPTION...] TYPE
```

Both subcommands share `--target`, `--select first|best|all`,
`--render definition|expression`, and
`--qualification none|identifiers|full`. Djinn additionally exposes its
candidate limit and choice-point budget; Exference exposes its checked
source environment, constraint/pattern policy, and step, queue, and depth
bounds. Run `djex <backend> --help` for the exact backend-specific table.

Generated Haskell alone is written to stdout. Loader messages, logical
negative answers, undecided/truncated status, and structured diagnostics go
to stderr. Help, version, successful synthesis, proof-backed
uninhabitability, and bounded no-result searches exit 0;
load/parse/search/render failures exit 1; malformed command lines exit 2. A
valid negative answer therefore stays distinct from a broken invocation, and
an exhausted Exference heuristic search is never mislabeled as a proof of
uninhabitability.

## Query boundary

### Shared query surface

`Language.Haskell.Synthesis.Query` shares the target, goal, contexts,
logical evidence, and search-batch shape without pretending that both
engines accept the same types or options. Every `QueryRequest` carries an
opaque `DefinitionName`, constructed once from a structural `Name`; it
guarantees the target is an unqualified value identifier or operator other
than the wildcard, so neither backend can defer or disagree about that
shared output invariant. Raw-name parser helpers perform this check before
parsing, preserving usage-error precedence. `QueryResult` is likewise
opaque: `mkQueryResult` checks that `ValidatedCandidates` accompanies
exactly the nonempty batches, `queryResultFromCandidates` derives that
evidence for ordinary heuristic-search batches, and both checks inspect only
the list spine's first constructor, preserving lazy candidate tails.

Opaque invariant witnesses do not derive representation-producing classes
such as `Generic`, and their exported observations are ordinary functions
rather than record fields: `Generic.to` can rebuild a hidden representation
and GHC record update needs only an exported field label, so either route
would let checked state be reconstructed unchecked. This applies to the
shared environment, inventory, synonym table, and result envelope, as well
as the corresponding rigid-instantiation, class-index, scope, and
proof-state witnesses in the backends.

`CachedQuery` separately owns a strict `RequestProvenance` and the adapter's
lazy derived cache. Parsed requests materialize their complete neutral
`SourceLocation` while sealing, avoiding retention of the input buffer;
programmatic requests carry an explicit source-free provenance. Equality and
display observe only the neutral request. Both adapters enter
`sealCachedQueryWithProvenance`, so validation failures receive the same
source attribution and strictness contract; backend caches remain lazy.

The one-import `Language.Haskell.Djex` surface reexports the complete
neutral declaration, environment, inventory, kind-inference,
synonym-elaboration, and type-rendering vocabulary. `DjinnEnvironment`,
`DjinnInventory`, `DjinnTypeVariable`, `DjinnLocal`, and `DjinnType` make
every Djinn adapter signature nameable without depending on a hidden backend
alias, and both stable environment aliases use `Void` for explicit kind
variables, making their common ground-kind contract visible in types.

Checked boundaries preflight widths before any structural traversal that
assumes a finite list spine. Known class applications observe at most the
declared arity plus one cell, and tuple validation observes at most the shared
maximum tuple arity plus one cell. Environment declarations, nested type
constraints, kind inference, backend requests, and Exference's nominal class
environment therefore reject cyclic or overlong argument and tuple spines
with a structured arity/type failure instead of diverging while counting or
canonicalizing them. Full name, binder, kind, and type validation follows this
bounded preflight; the preflight is a denial-of-service boundary, not a
substitute for those semantic checks.

### Djinn sessions and requests

`mkDjinnSession` lowers and seals the kind-ground neutral shared
`DjinnEnvironment = Environment DjinnTypeVariable Void ()` through one
authoritative closed Inventory with Haskell 98 class-kind defaulting.
Djinn's historical `Kind Int` survives only in its raw compatibility API,
and its `HKind` is a private-representation compatibility newtype over the
shared `Kind Int`: bundled patterns preserve `HKind(..)` imports and the
historical `*`/`kN` rendering while all kind bridging and grounding operate
on the single shared tree.

The foundation first builds an opaque transient
`PreparedInventoryExpansion`: one operation prepares the exact synonym
table, expands operational declarations in source order, and classifies
recursion from that same operationally alias-free stream. An opaque shared
`PreparedInventory` then keeps the Inventory and its exact normalized
synonym table inseparable after the transient stream has supplied Djinn's
formula, premise, and no-recursion checks; the mutable raw
`Djinn.Core.Environment` never crosses the curated facade. Synonyms are
expanded for saturation and recursive datatype validation before ordered
global assumptions are translated once into proof premises. The sealed
environment retains exactly the prepared Inventory/synonym witness, the
foundation's annotation-free prepared class index, formula compiler, and
those ordered premises; historical raw declaration tables are derived only
when a compatibility caller asks to display them.

At query time Djinn elaborates the goal and all class arguments as one
shared kind scope, instantiates class methods in that same shared type tree,
and compiles the alias-free goal and methods directly into formulas through
one representation-neutral prepared definition cache; opaque requests retain
their exact session-independent source view. Invalid search controls have a
typed core failure and stable `DJEX_DJINN_OPTIONS` diagnostic; query-type
provenance is attached only to source-derived input rejection, never to
separately supplied options or an internal proof/result invariant.
`standardDjinnSession` converts the historical built-in spelling once and
then uses the same neutral `mkDjinnSession` path as every caller-supplied
environment, and `parseDjinnRequest` shares the compatibility frontend's
optional class-context grammar through the full-consumption
`Djinn.Core.parseContextualHType` entry point.

Like the Exference adapter, the curated Djinn adapter publishes no mutable or
raw-typed session operations. Callers replace declarations by constructing and
sealing a complete neutral `Environment`; only the historical REPL imports the
private raw declaration snapshot, edit, and instance-method helpers.

Both `DjinnRequest` and `DjinnCandidate` expose
`DjinnType = Type DjinnTypeVariable`; that shared type is checked and
canonicalized once by `mkDjinnRequest`, which seals an opaque shared
execution plan while retaining the caller's exact neutral query, and
`djinnRequestQuery` recovers the exact stable source view. The plan stays
shared through environment-dependent kind checking, synonym elaboration,
class-method instantiation, and formula compilation. Queries return shared
candidates containing structured generated clauses, empty residual
constraints, and Djinn's unused-binder ranking details in one terminal
batch. A proof beyond `optionCutoff` produces
`Truncated CandidateLimitReached` without forcing the proof-stream suffix,
and proof-backed `ProvedUninhabitable`, target-reference evidence, and
budget-limited `NoEvidence` remain distinct from the batch's operational
`Finished` or `Truncated` completion.

### Exference sessions and requests

Exference has the same stable construction boundary:
`ExferenceEnvironment = Environment ExferenceTypeVariable Void ()`, and
`mkExferenceSession` kind-checks and lowers that parser-independent
environment directly; the source loader is an additional frontend, not the
definition of an Exference session.

Exference core names are the shared synthesis `Name` itself; `QualifiedName`
is a compatibility alias, and the `QualifiedName`, `ListCon`, `TupleCon`,
`UnboxedTupleCon`, and `Cons` views are separately exported patterns.
Ordinary names and boxed tuples are constructed with `mkQualifiedName` and
`mkBoxedTupleName`, so malformed source spelling, qualification, or tuple
arity receives a structured error. Exference constraints are
`Constraint HsType`, with `HsConstraint` retained as a compatibility
pattern, and `HsType` is exactly the shared `Type (Variable Int)`: its
historical constructors survive as separately exported patterns, where
`TypeForall` preserves the flexible-binder-only view and
`TypeForallNative` exposes every shared binder. Checked Exference
boundaries canonicalize saturated function and tuple applications to
structural `FunctionType` and `TupleType` values and reject rigid forall
binders, which the search engine cannot instantiate as source quantifiers.
The shared `normalizeType` operation owns canonicalization and structural
validation for both backends and Exference unification; adapters add only
their genuine variable/binder and source-vocabulary restrictions.

`ExferenceRequest` is opaque in the same operational sense as
`DjinnRequest`: the stable adapter exposes only `mkExferenceRequest` and
`exferenceRequestQuery`, and source locations and parsed variable spellings
are private presentation data. Execution uses only the canonical contextual
goal retained in the private plan, while projection, equality, and display
publish the caller's exact neutral request. The common `CachedQuery` owns
the strict location separately from an opaque backend-specific
variable-to-spelling value; that checked hint witness also owns the
canonical contextual goal, so query execution does not rebuild it. The
source SPI validates every raw alias as a non-wildcard variable identifier,
bounds its ID to the complete contextual goal, collapses aliases
deterministically, retains the exact canonical goal as a scope witness, and
detaches the map before returning. Shared synonym elaboration
alpha-freshens every alias-introduced binder away from the complete
original source namespace, including through nested and zero-argument
aliases, so the adapter can retarget surviving hints to the elaborated goal
without confusing an erased phantom argument with an unrelated
same-numbered binder — and core search rejects a hint value paired with any
other query. The hidden
`Language.Haskell.Djex.Exference.Internal.Request` representation owns that
metadata; external clients use either the neutral stable adapter or
`Language.Haskell.Djex.Exference.HaskellSrc`.

One dependency-leaf `ExferenceOptions` definition is re-exported by both
the core and checked facades; the exact value in the neutral request is
retained as `ExferenceQuery.querySearchOptions`, and only the historical
flat `ExferenceInput` compatibility record needs a one-way projection with
its established validation order unchanged. `ExferenceEnvironment`,
`ExferenceType`, `ExferenceTypeVariable`, `ExferenceLocal`, and
`ExferenceInventory` make that complete surface nameable in the neutral IR.
Session construction maps backend ratings out of the already-checked
inventory without rebuilding its indexes or kind assumptions, and stable
candidate details and batch metadata are zero-copy public views of the
exact core-owned values.

The nominal class environment also enforces a termination condition before
publishing its instance index. For every prerequisite whose variables can be
grounded by matching the instance head, the prerequisite may not contain more
type nodes than the head and may not use any head variable more often. The
check also covers superclass-inflated rules. A rule such as
`C [a] => C a` is rejected as `ExpandingInstancePrerequisite`, because it
would turn `C Int` into an unbounded chain of larger goals. Shrinking rules are
accepted; exact and size-preserving cycles are safe because resolution tracks
the constraints on its current path. Prerequisites with variables absent from
the head cannot be grounded by the match and remain unresolved rather than
being recursively expanded. Consequently nominal resolution terminates for
finite ground constraints accepted by this boundary. This guarantee is local
to class resolution and does not turn Exference's expression enumeration into
a proof of inhabitation or non-inhabitation.

### Loading Haskell source environments

`Language.Haskell.Djex.Exference.HaskellSrc.loadExferenceSession` and its
policy-aware counterpart compute Exference's backend-supported projection
once and turn a directory into the same opaque session. Failures in the
source-loader phases are reported with stable `EXF_*` codes; diagnostics that
originate at a source-aware read, parse, vocabulary, or extraction boundary
retain their exact spans. After the neutral inventory has been built, shared
sealing and session-policy failures instead retain their `DJEX_EXF_*` codes
and may be source-free, because they describe the complete prepared inventory
rather than one source token. Stable callers never handle a parser-specific
checked environment. The explicitly named
`Language.Haskell.Exference.Session` module retains the raw
`CheckedSourceEnvironment` bridge for the historical CLI and clients that
opt into the compatibility frontend.

The source boundary tags class methods with their qualified owner, nests
them under the common class declaration for validation, and lowers each
rated selector exactly once into Exference's flat search inventory without
changing source order. Ordinary signatures, datatype batches, and nested
class-method signatures retain compact module-local source slots while they
are lowered; the loader stable-merges those batches, so successes and
extraction failures remain interleaved exactly as written without replaying
the HSE syntax or guessing multi-name signature cardinality. HSE aliases
remain unexpanded through common Inventory kind checking; the same transient
prepared-expansion witness used
by Djinn then expands them capture-safely and derives cross-module
recursion before Exference normalizes classes and instances and reapplies
source ratings and order. Source checking returns one opaque annotated
witness owning the checked Inventory, synonym table, and backend together;
the frontend can reorder the exact checked names and attach finite ratings,
but cannot combine an inventory with an independently prepared search
dictionary. The synthesis foundation remains the sole owner of the
Inventory and normalized-synonym projections, the historical flat
`SourceEnvironment` projection is derived on demand from the witness, and
the sealed session retains only the shared inventory/synonym witness, its
policy-adjusted checked search environment, and a fully materialized
structured omission summary. HSE query parsing derives known types and
class arities from the witness's shared inventory; neither an HSE source
environment nor its legacy synonym map survives sealing. HSE's normalized
parse filename is also the filename retained for deferred diagnostics, so
extensionless labels do not change identity between parse and search
phases; angle-bracket virtual-buffer names remain verbatim.

The loader is fail-closed at its vocabulary boundary: after parsing, but
before constructing any partial inventory, it reports source-ordered
`UnsupportedVocabularyOccurrence` values for type/data families, GADTs,
explicit module export lists, datatype contexts, explicitly kinded parameters,
existential or constrained constructors, derived or overlapping instances,
functional dependencies, associated families and defaults, pattern-synonym
signatures, declaration splices, role annotations, and XML page or hybrid
modules, each with the stable `EXF_UNSUPPORTED_VOCABULARY` diagnostic code and
its exact source span. The exported `UnsupportedVocabularyForm` constructors
are the authoritative exhaustive vocabulary for this rejection phase.
Ordinary positional, infix, record, strict, and unpacked datatype fields are
lowered explicitly; record selectors become rated value bindings exactly
once. Imports, fixities, ordinary value and method bodies, ordinary term
patterns and pattern-value bodies, default declarations, and operational
pragmas remain accepted because they do not change the nominal type/class
inventory. These forms are explicit current limitations rather than syntax
that can silently disappear during loading.

`ExferenceSessionPolicy` applies exact structural-name exclusions and
finite, signed rating overrides while the private search projection is
sealed. Overrides neither reorder declarations nor leak into the
annotation-erased public inventory. `exferenceSessionEnvironment` and
`exferenceSessionInventory` expose the unchanged authoritative views in
parallel with Djinn's stable session API. An exclusion is a subtractive
capability request, so an unknown excluded name is an intentional no-op; this
also lets command defaults name optional recursion helpers without requiring
every environment to define them. A rating override claims to change search,
so a non-finite rating or a name unavailable after exclusion and capability
filtering is a fatal structured diagnostic. Unsupported rank-N
introduction/elimination and recursive-data elimination capabilities remain
visible as structured omissions and warning diagnostics instead of
disappearing per query; omission order follows introduction order and then
elimination order.

`Language.Haskell.Djex.Exference.HaskellSrc.parseExferenceRequest` resolves
Haskell syntax against the session's retained type names, classes, and kind
assumptions. `runExferenceQuery` passes both parsed and programmatically
constructed goals through the shared capture-avoiding
`TypeSynonym.elaboratePreparedType` operation on the session's exact opaque
witness, including its pre- and post-expansion kind checks, before lowering
to the core search type, so the two request paths agree on aliases, cycles,
saturation, and kinds. Query execution then validates only the varying
search policy and returns a lazy sequence of shared result batches.

Programmatic clients need only the neutral adapter:

```haskell
import Language.Haskell.Djex.Exference
```

Clients that load directories or parse Haskell type text import the
explicit source boundary from the same dependency:

```haskell
import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.HaskellSrc
```

### Generated output and rendering

Result batches preserve queue/depth pruning, nominal binding usage,
residual constraints, statistics, and rendering hints without forcing the
remaining trace. Both engines construct their stable `QueryResult` payloads
in the core: Djinn preserves its richer logical evidence, while Exference
derives evidence from each lazy candidate batch after one checked query
preparation. Each generated expression is wrapped in a target-bearing
shared `FunctionClause` whose opaque `DefinitionName` preserves the checked
request target through result projection. The shared candidate
expression/definition renderers own the common clause projection and return
`RenderError` directly; each backend adapter contributes only its local-name
hints and qualification options. Because the public `Candidate` constructor
remains available for compatibility, both stable renderers first validate the
complete clause scope. A caller-forged free local or duplicate pattern-binder
identity is rejected rather than rendered as if it were checked backend
output.

Exference's live search tree is the same shared `Generated.Expression`
shape as those candidates: checker-specific type annotations inhabit a
private local payload, the historical `ExpVar`/`ExpLambda`/`ExpLet`/case
constructors are bundled bidirectional compatibility patterns over that
tree, and erasing annotations is a functor projection rather than a
recursive conversion. Djinn's LJT lowering constructs and simplifies that
same shared generated `Expression`/`Pattern` tree directly; `HExpr` and
`HPat` remain only as projections for historical low-level callers.
Incremental hole filling, capture-safe let cleanup, and eta reduction live
in the shared generated-syntax module, parameterized only by Exference's
projection from an annotated local to its stable numeric identity. Djinn's
pattern alias normalization, unused-binder pruning, application-spine
inspection, and case-body alpha-equivalence use that same authority, as do
leading-lambda construction and decomposition: both backends promote the
complete nonempty lambda spine through the same expression-to-clause
operation, the inverse clause operation restores one canonical group, and a
caller-built `Lambda []` is a validation error rather than being silently
erased.

Candidate selection and rendering remain presentation policies outside both
session operations. The shared `Selection` module provides first,
global-best, streaming-all, batch-lookahead, and preferred-tier lookahead
policies over either backend's result envelope. `TypeRender` prints shared
types and constraints from tagged variable-name hints without collapsing
flexible and rigid identities. Its qualification-aware entry points use the
same identifier/operator policy as generated terms, so an Exference candidate
and its residual obligations cannot disagree about module prefixes. The stable
Exference residual renderers return
`Either ExferenceResidualRenderError [String]`, validating caller-built class
identities and every shared type argument before emitting text. This is an
intentional source break from the
former pure list result: the public compatibility `Candidate` constructor
otherwise permits malformed obligations to bypass the checked query path.
`TypeSynonym` prepares aliases from the
retained neutral inventory and owns prepared-witness operations for
whole-type and individual-application minimum-saturation preflight,
capture-avoiding expansion, and the pre/post kind checks both backend
adapters share. Kind inference is the single structural validator for each
elaboration phase; its batch operation preserves source order while
assigning one kind to each free variable shared by a goal and its separate
context arguments. The shared type module owns the scope-aware simultaneous
substitution primitive used by synonym expansion and Exference's
compatibility substitution API. Exference's backend-specific unifiers
operate directly on the same native tree, canonicalize their inputs and
projected substitutions, preserve flexible/rigid and left/right identity,
and consume the foundation's one constructor-application view for
structural functions and tuples; unary unboxed tuples remain structural
because Haskell has no corresponding unary tuple constructor.

### Compatibility executables

The `exference` compatibility executable is a six-line launcher for
`Language.Haskell.Exference.CLI`, the compatibility orchestrator at this
boundary: it loads and seals one session, parses every requested type
through `parseExferenceRequest`, selects shared candidates, and renders
their generated expression bodies. The compatibility command and
`djex exference` obtain their session policy from the same frontend
operation: both exclude `Data.Function.fix`, `Control.Monad.forever`, and
`Control.Monad.Loops.iterateM_` by default, and both accept `--fix` as an
explicit opt-in, while the unrestricted programmatic session default is
unchanged. Parse, kind, option, and search failures are structured
diagnostics on stderr with failure exit status; repeated inputs are all
processed and conflicting presentation modes are rejected. The historical
ranking vector remains an explicit compatibility profile, and `--short`
adds backend-neutral structural expression size to the candidate cost.

The `djinn` compatibility frontend retains its declaration REPL while
storing only the exact sealed `DjinnSession`. Successful mutations edit its
authoritative shared environment and publish a replacement session only
after complete structural, kind, alias, recursion, and backend validation.
Historical `:environment` ordering is derived on demand from the retained
Inventory, instance-method lookup uses its prepared nominal class index,
and type queries reuse the ordered global premise cache. Queries and
instance methods consume shared evidence, progress, metadata, and
`FunctionClause` output through `runDjinnQuery`. Startup-file mode carries
aggregate failure status across later commands and `:clear`, accepts
settings on either side of filenames, and rejects unknown or ambiguous
option prefixes; interactive recovery is unchanged.

## Dependency migration

The single-package layout replaces three former package identities.
Existing Cabal dependencies migrate as follows:

| Former dependency | Djex dependency |
| --- | --- |
| `haskell-synthesis` | `djex` |
| `djex:synthesis` | `djex` |
| `djinn:djinn-core` or `djex:djinn-core` | `djex` |
| unnamed `djinn` library or `djex:djinn-frontend` | `djex` |
| `exference:exference-core` or `djex:exference-core` | `djex` |
| unnamed `exference` library or `djex:exference-frontend` | `djex` |

All library clients use one unnamed `djex` dependency for the curated
facade, shared synthesis vocabulary, checked adapters, lower-level engines,
source loading, and the historical REPL API. Build-tool dependencies for
the commands remain `djex:djinn` and `djex:exference`; the executable names
are unchanged. The one library consequently has the union of core and
frontend dependencies: `haskell-src-exts`, `directory`, `filepath`, and
`haskeline` share the same versioned component contract as the engines that
consume their output.

Both backend trees follow the same layout — `src-core/`, `src-frontend/`,
`app/`, and one explicit directory per test suite — while the package root
uses `src/`, `app/`, `test-integration/`, `test-api/`, and `test-cli/`.

Package-generated code imports `Paths_djex` instead of `Paths_djinn` or
`Paths_exference`; version discovery and installed-data lookup belong to
Djex as a whole. Exference's installed environment is a Djex data directory.
Checked library clients use `defaultExferenceEnvironmentPath`,
`loadDefaultExferenceSession`, or its policy-aware counterpart from
`Language.Haskell.Djex.Exference.HaskellSrc`; Cabal's generated `Paths_djex`
module remains a private packaging detail.

## License and credits

Djex is distributed under the BSD-3-Clause license; see
[LICENSE](LICENSE).

Djex descends from two projects:

- [Djinn](https://github.com/augustss/djinn) by Lennart Augustsson supplied
  the intuitionistic proof engine behind `djex djinn`. Its LJT prover in
  turn descends from Roy Dyckhoff's original Prolog implementation; the
  module header of `Djinn.Internal.LJT` records the lineage. Djinn's
  original license is preserved verbatim in [djinn/LICENSE](djinn/LICENSE).
- [Exference](https://github.com/lspitzner/exference) by Lennart Spitzner
  supplied the heuristic polymorphic-expression search engine behind
  `djex exference`. Its original license is preserved verbatim in
  [exference/LICENSE](exference/LICENSE).

Djex is neither affiliated with nor endorsed by Lennart Augustsson,
Lennart Spitzner, or the other upstream contributors.
