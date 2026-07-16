# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\b -> fmap (\g -> show (b g))`

[Djinn](https://hackage.haskell.org/package/djinn) is a well known tool that
does something similar; the main difference is that *Exference* supports a
larger subset of the haskell type system - most prominently type classes. This
comes at a cost, however: *Exference* makes no promise regarding termination.
Where *Djinn* tells you "there are no solutions", exference will keep trying,
sometimes stopping with "i could not find any solutions".

## References and environment

- **Documentation:** [exference.pdf](https://github.com/lspitzner/exference-paper/raw/master/exference.pdf)
  describes the original implementation and its properties.
- The executable loads Djex's [packaged local environment](environment/) of
  functions, classes, instances, and ratings by default. Installed builds find
  it through Cabal rather than depending on a source checkout.
- Historical Exference releases advertised `exferenceBot` on Freenode's
  `#exference` channel. That bot is not a maintained interface to this tree;
  use the local executable for reproducible behavior against the packaged
  environment.

## Building from source

Exference is part of the unified `djex` Cabal package. Its libraries and
deterministic test suites build with the supported GHC 9.12.4 toolchain using
Cabal 3.16.1.0. Cabal is the maintained build path; the historical Stack file
targeted LTS 5.18 and
dependencies that no longer exist in this tree, so it has been removed rather
than pretending to provide a second supported toolchain. Run these commands
from the repository root or `djex/`:

```console
cabal build djex:lib:djex djex:exe:exference
cabal test djex-api-tests exference-frontend-api-tests exference-tests exference-cli-tests --test-show-details=direct
```

The former top-level `exference/` package is now the `djex/exference/` source
tree and has no independent package or project file. Package-generated code
imports `Paths_djex` (not `Paths_exference`), and the default environment is
located with `getDataFileName "exference/environment"`. See the current
[Djex architecture and complete package migration guide](../README.md). The
[2026-07-10 code review](docs/reports/2026-07-10-code-review.md) remains useful
as a historical compatibility audit, but its proposed merge boundary predates
the unified Djex package.

`src-core/` is Exference's parser-independent contribution, compiled into the
same `djex` library as the shared foundation, Djinn, and both compatibility
frontends. Parser independence remains a Haskell module/source-layer property;
the single Cabal library deliberately owns the union of core and frontend
dependencies.
It uses `transformers` directly: expression checking has strict state, while
the branching search deliberately retains lazy `StateT`.  Its hidden search
state uses ordinary record selectors and explicit updates, keeping the engine
independent of `mtl`, generated optics, and Template Haskell.
`Language.Haskell.Exference.Core.Unify` names its two flexible-variable
contracts explicitly: `unifyDisjoint` returns separate substitutions for
independent input namespaces, while `unifyShared` applies one occurs-checked
substitution to a common namespace. The historical `unify` name remains a
compatibility alias for `unifyDisjoint`. All three operate directly on the
native shared type tree, canonicalize saturated functions and tuples before
solving, reject quantifiers at any depth through the shared type and
constraint-argument inspection authority, and canonicalize their projected
substitutions. Declaration ingress and egress likewise use one checked native
type/constraint operation; the former converted/lowered pair was only a name
distinction after `HsType` became the shared tree. The right-directed matcher
uses the same tagged solver while
keeping left-side variables rigid, so equal numeric IDs from its two inputs
cannot alias accidentally. Exference's substitution API likewise delegates to
the shared scope-aware simultaneous substitution primitive, including
capture-avoiding alpha-renaming under foralls. That representation-level API
remains total for native rigid-binder forms; checked search boundaries reject
those unsupported binders before execution.
Flexible classification, identity projection, selective renaming, and
flexible-versus-rigid collection now use the shared tagged-variable operations;
Exference retains only its finite `Int` allocation and backend-specific error
policies.

The `src-frontend/` root contains the `haskell-src-exts` frontend, environment
loader, public `Language.Haskell.Exference.CLI` compatibility entry point, and
`Language.Haskell.Djex.Exference.HaskellSrc` source adapter. These modules now
live in the same library as `Language.Haskell.Djex.Exference`; the executable's
`app/` root remains only a six-line launcher. The curated
`Language.Haskell.Djex` module still re-exports the neutral adapter rather than
the source-loading extension.
Source conversion exposes its concrete `ConversionT` stack, with errors inside
lazy state so caught failures retain earlier variable allocations. Its opaque
inventory separates source-spelling hints from the exact reserved-ID set: this
preserves aliases and hintless alpha-renamed binders while allocating safely
through sparse and `maxBound` namespaces. Together with direct reader and
strict-writer transformers, this also keeps the frontend independent of `mtl`.
That raw spelling-oriented index remains a parser/compatibility value. The
checked request boundary validates every alias as a non-wildcard identifier in
Exference's enabled Haskell type grammar (including its extension keywords),
requires its ID to occur flexibly in the complete
contextual goal, collapses aliases to the lexicographically least spelling,
retains that exact canonical goal as a scope witness, and fully detaches the
resulting opaque variable-to-spelling map while sealing. Accepted and rejected
spellings are both forced at this boundary. Malformed maps therefore fail early as source-located
`DJEX_EXF_SOURCE_HINT` diagnostics rather than reaching candidate rendering.
This mirrors Djinn's library-first organization while retaining the
parser/source boundary at the module level.

The source component crosses into core through the explicit parser-neutral
`Language.Haskell.Djex.Exference.FrontendSupport` service-provider interface.
It documents and wraps the four checked operations needed by source-adapter
authors without exposing request, session, or allocator representations.
Adapters import it directly from `djex`; the curated
`Language.Haskell.Djex` facade does not re-export it. The
request representation behind it remains a hidden module. Stable clients
therefore see an opaque `ExferenceRequest` with one neutral smart constructor;
the shared `CachedQuery` owns its strict complete source provenance, while
its backend cache is exactly one opaque checked source-hint value. Neither
provenance nor hints participate in request equality or display; sealing
materializes both the complete location and every accepted spelling, so a
reusable request retains neither its source buffer nor the raw parser map.
Its compatibility parser still accepts a raw shared `Name`, but converts it to
the request's opaque `DefinitionName` before parsing so invalid-target
diagnostics retain their historical precedence. Programmatic `QueryRequest`s
cannot carry an unchecked target at all.
The filename stored after a successful parse is the same normalized HSE
filename used by immediate parse diagnostics: ordinary extensionless labels
gain `.hs`, existing extensions remain unchanged, and angle-bracket virtual
buffer names remain verbatim.

Core names use the shared synthesis modules in `djex` and their validated
structural `Name` directly:
`QualifiedName` is now a compatibility alias, not an opaque wrapper. The
`QualifiedName`, `ListCon`, `TupleCon`, `UnboxedTupleCon`, and `Cons`
compatibility views remain separately exported patterns. The input-bearing
ordinary and boxed-tuple views are match-only; construct them with
`mkQualifiedName` and `mkBoxedTupleName` so invalid source text or tuple arity
is reported explicitly. Programmatic unboxed tuples use the shared
`tupleName Unboxed` builder; the compatibility HSE frontend still rejects
unboxed tuple syntax because the search language cannot generate its terms.
Unqualified frontend lookup rejects ambiguous imported type names instead of
silently choosing the first.

Exference's `HsType` is now a compatibility alias for the shared
`Type (Variable Int)`, not a recursively isomorphic engine-owned tree. The
historical `TypeVar`, `TypeConstant`, `TypeCons`, `TypeArrow`, `TypeApp`,
`TypeTuple`, and `TypeForall` spellings remain separately exported patterns.
`TypeForall` deliberately constructs and matches only flexible binders, as the
old representation did; `TypeForallNative` is the total view used by exhaustive
internal matches. Checked environments and requests canonicalize saturated
function and tuple constructor applications to structural `FunctionType` and
`TupleType` values. They reject a rigid variable in any forall binder because
Exference treats rigid IDs as search constants rather than source binders.

Class constraints use `Language.Haskell.Synthesis.Constraint` directly as
`Constraint HsType`; the historical `HsConstraint` constructor spelling is a
bidirectional compatibility pattern. Class declarations and instances live in
sealed strict maps built by
`mkStaticClassEnv`, which
checks names, duplicate declarations/parameters, superclass variables and
cycles, referenced classes, exact arities, and every native constraint-argument
type before superclass inflation. Query-constraint closure and derived
instance-head indexing share one immediate-superclass instantiator, including
its exact-arity guard and capture-safe parameter substitution.
Query and binding inputs likewise reject wrong arities for known classes while
retaining unknown classes as explicit external constraints.

Exact compatibility imports must now name the aliases and patterns separately
and enable `PatternSynonyms`, for example `QualifiedName, pattern QualifiedName`
and `HsConstraint, pattern HsConstraint`, or
`HsType, pattern TypeVar, pattern TypeForallNative`. Imports such as
`QualifiedName(..)`, `HsType(..)`, and `HsConstraint(HsConstraint)` cannot
describe constructors of type aliases. A complete compatibility match must use
`TypeForallNative`; the flexible-only `TypeForall` pattern is intentionally not
part of the module's `COMPLETE` set. The now-impossible
`UnsupportedSpecialName`, `UnsupportedSynthesisName`, and
`DeclarationNameConversionError` alternatives have consequently been removed.

There is no longer a structural conversion API: `QualifiedName`, `HsType`, and
`HsConstraint` already are the shared values, so the merger-only identity
projections would falsely imply a representation boundary. `toSynthesisType`
and `fromSynthesisType` both canonicalize and validate the native value, and
both reject rigid forall binders rather than weakening them during lowering.
The checked constraint operations validate the class namespace and normalize
their argument types without reconstructing the outer constraint. Because
`HsType` inherits the shared structural `Show` instance, diagnostics and other
source-like output should call `showHsType` (and `showHsConstraint`) rather than
relying on `show`.

The independent generated-expression checker validates every reachable native
type and constraint before inference. It uses the same higher-kinded view of
structural functions and tuples as search, while tuple complexity replays the
historical left-associated constructor/application accumulation exactly so
floating-point rounding and saturation cannot perturb queue order.

`Language.Haskell.Exference.Core.Declaration` converts function bindings,
classes, instances, and deconstructor/data records to the shared declaration
IR; the HSE frontend uses the same boundary for type synonyms. Function
and constructor search penalties and recursive-datatype flags survive as
explicit metadata. Class methods are nested under their shared class
declaration with their rating intact; the implicit owner constraint required
by Exference's flat search binding is derived from the class parameters,
checked and removed while nesting, and restored exactly once while lowering.
The older method-free class conversion still rejects a declaration containing
methods instead of silently dropping them.
The legacy `applyTypeDecls` entry point now adapts its finite raw definition
map to the shared capture-avoiding synonym engine as well. It retains
reachable-only cycle reporting and invalid-declaration fallback for source
compatibility, but no longer owns a second substitution implementation.

Method-free core environments round-trip through the shared sealed declaration
inventory with `fromSynthesisEnvironment`. That legacy reverse adapter rejects
a method-bearing class because `EnvDictionary` has no ownership field and
flattening a selector into `environmentFunctions` would silently change its
meaning. Ownership-aware callers instead use
`fromSynthesisEnvironmentWithClassMethods`, which returns the ordinary
`EnvDictionary` together with a `Map QualifiedName [FunctionBinding]` accepted
by `toSynthesisEnvironmentWithConstructorPenaltiesAndClassMethods`.
`StaticClassEnv` retains explicit instance declarations separately from its
superclass-inflated lookup index, so adapters serialize source facts rather
than derived cache entries. The core-only adapters still reject frontend-only
declarations such as type synonyms because the search dictionary has no
representation for them.

The HSE loader returns `IO (LoadReport CheckedSourceEnvironment)`: fatal phases
use a typed error, while nonfatal warnings and summaries are structured shared
diagnostics. Fatal module parse diagnostics retain HSE's exact source filename
and point location under the stable `EXF_MODULE_PARSE` code, with the native
parser detail preserved as context. HSE locations cross the shared checked
one-based, half-open span boundary explicitly; a malformed native location
retains its source and becomes diagnostic context rather than causing a crash
or forging an invalid span. Low-level `parseModules` keeps aliases
unexpanded in its `SourceEnvironment`; `checkSourceEnvironment` seals and
kind-checks that source graph once, then sends the checked Inventory through
the parser-independent neutral lowerer. The resulting backend projection is
reconciled by name with the original binding/deconstructor order and ratings.
One opaque annotated prepared value wraps the shared `PreparedInventory` and
its backend lowering, keeping the checked Inventory, its exact synonym table,
and that lowering inseparable; the retained frontend/core module seam
accepts only exact-name order and finite rating metadata, never a second
independently prepared dictionary. Alias-aware recursion metadata is attached
to the sealed Inventory through a checked annotation-only adjustment, without
rebuilding its indexes or repeating kind inference. `CheckedSourceEnvironment`
stores only that witness and the historical synonym spellings; its flat source
projection is derived on demand. The wrapper exposes only
`preparedSynthesisWitness` and `preparedSynthesisBackend`; Inventory and
synonym-table access goes through the foundation witness, and the
annotation-free session view is written explicitly as
`PreparedSynthesisInventory ()`. It shares the already prepared synonyms and
backend. Thus synonym expansion, forall
freshness, class/instance normalization, and whole-inventory recursion
classification have one implementation, while the checked Inventory still
retains the source aliases needed by later queries.
Its ordered binding field is now `sourceBindings :: [SourceBinding]`, where
`SourceFunction` denotes an ordinary `FunctionBinding` and
`SourceClassMethod QualifiedName` records the
exact owning class beside one. Both `SourceBinding` and `SourceEnvironment` are
monomorphic: HSE signatures cross `functionBindingFromType` once during
extraction, and rating updates change only `functionPenalty`. The flat
`sourceFunctions` accessor remains available for search and compatibility
clients, but code constructing or updating the old `sourceFunctions` record
field must migrate to `sourceBindings` and wrap its ordinary entries in
`SourceFunction`. A read-only explicit import that formerly named only
`SourceEnvironment(..)` must now also name the standalone `sourceFunctions`
accessor; broad module imports remain unaffected.
The public `sourceTypeNames` field remains as a compatibility lookup cache, not
as validation authority. `checkSourceEnvironment` rebuilds it from the
normalized ordinary datatypes, retained synonyms, and checked class table, so
a sealed projection cannot advertise stale or caller-forged names.

The low-level HSE adapters are total at their public boundaries:
`convertName`, `convertModuleName`, and `getDataTypes` return explicit
conversion failures even for malformed caller-constructed syntax trees. The
former unchecked aliases and parallel `*Checked` names have been folded into
those single ordinary APIs. Source ratings likewise have one canonical
application pass; the obsolete first-match dictionary compiler and unsealed
single-module tuple loader are no longer separate behaviors. Callers that need
one neutral module can use `environmentFromModule`, which runs the same loader
and inventory sealing as the rated and directory entry points.

Class heads, superclasses, instances, and method bodies are elaborated from one
collected class inventory. `loadClassEnvironment` returns one named
`LoadedClassEnvironment` containing the validated static graph, the explicit
source-instance count, and ownership-bearing methods grouped in input-module
order; no method-free projection traverses the class bodies again. A method's
compatibility type carries the implicit
owner constraint, while its `SourceClassMethod` tag carries only the qualified
owner name, avoiding a second copy of the class parameter IDs. Inventory
sealing checks that tag and leading constraint against the owning class, nests
the signature, and indexes the method once in the common value namespace. The
checked backend projection then lowers the method back into its original list
position, preserving rating and equal-cost search order.

Constructor signatures duplicated in Exference's search-function list are
represented only by their datatype declarations at this boundary; list, unit,
and tuple constructors
have explicit intrinsic datatype records, so `(:)` never masquerades as an
ordinary value. Constructor shape and search penalty are then lowered back
from that checked inventory rather than trusted from a parallel raw record.
The default source environment derives its flat constructor functions from
those same ordered datatype records. Shared names and types represent boxed
tuples through arity 64, while the eager Exference search inventory deliberately
materializes only arities 2 through 7: higher eager constructors would add a
partial-application branch to every non-arrow goal. This operational cap is
exported as `maximumBuiltInTupleArity` rather than repeated as a magic number.
Recursive flags are derived after alias expansion across all loaded modules
and appear in both the backend-derived projection and checked Inventory;
caller-supplied or module-local preliminary bits are never authoritative.
Class-environment construction likewise rejects repeated instance heads before
building its lookup index; each shipped primitive instance now has one owning
module instead of a second shadow declaration in `Data.hs`.
Sealing also runs the shared whole-inventory kind checker. Recursive datatypes
remain valid, recursive synonyms and ill-kinded signatures do not, and
unconstrained class parameters can generalize to support the shipped modern
poly-kinded `Typeable` vocabulary. The frontend selects the explicit open
inventory policy because loading a subset of modules deliberately retains
external type names after reporting them as warnings.
Synonym kinds are frozen after their defining declarations and before values
or instances are checked, so an operational use cannot retroactively make an
unused phantom parameter higher-kinded. Open inventories continue to let empty
datatype stubs acquire a missing kind shape from instances; the packaged
environment uses that compatibility rule for abstract base-library types.
Kind assumptions currently generalize only a wholly unconstrained class
parameter. The shared IR cannot yet retain a partially polymorphic scheme such
as `k -> Type`; an unresolved variable below a fixed outer kind shape therefore
defaults to `Type`, as it did before this class-method migration.
`toSynthesisSourceInventory` retains both the sealed environment and those
inferred assumptions; the older environment-only projection remains available
for compatibility callers. `Language.Haskell.Djex.Exference` can now seal the
same reusable session either from this checked source inventory or directly
from a parser-independent
`ExferenceEnvironment = Environment ExferenceTypeVariable Void ()`. Both paths
use the annotation-polymorphic `prepareSynthesisInventory` boundary to prepare
the shared inventory and synonym table, then seal one core search
projection, and report unsupported rank-N or recursive-data elimination
capabilities structurally; the source path preserves its historical ratings
and equal-cost order. Its explicit `prepareSourceSynthesisInventory` refinement
also copies alias-aware recursion into the retained source annotations; there
is no separate neutral Inventory type or preparation API. The CLI parses
requests through that session; a
query whose proper-type obligation conflicts with retained constructor or
class kinds cannot reach heuristic search.

The executable no longer rebuilds an `ExferenceInput`, repeats rank-N filters,
or consumes legacy tuple chunks. It maps flags to `ExferenceOptions`, calls
`runExferenceQuery`, applies the backend-neutral shared selection policies, and
renders the resulting shared candidates. The historical `exference` command
and merged `djex exference` command now consume one checked command-session
policy. Its conservative default denylist uses exact names
(`Data.Function.fix`, `Control.Monad.forever`, and
`Control.Monad.Loops.iterateM_`), so a same-spelled binding in another module
remains searchable; `--fix` explicitly opts either command into those known
nonterminating helpers. Programmatic sessions remain unrestricted unless their
caller supplies a policy. Multiple input types run in order; conflicting
selection or qualification flags fail explicitly; parse, kind, and search
failures use stderr and nonzero status. The historical heuristic vector remains
named in the CLI, and `--short` now ranks by structural generated-expression
size rather than rendered identifier length.

Exference's implicit instance variables become explicit binders at that shared
boundary. Reverse lowering accepts exactly the free flexible variables of the
instance context and head, preventing an unused or rigid shared binder from
silently changing meaning when returned to Exference's implicit form.

## Exference 1.7 migration

Version 1.7 intentionally breaks the old recursive type and constraint/class
environment representations. `HsType` is now an alias for the shared
`Type (Variable Int)`, and `HsConstraint` is an alias for the shared
`Constraint HsType`; separately imported patterns preserve the historical
construction and matching vocabulary.
`HsInstance` stores prerequisites plus an `instance_head`; class collections
are strict `Map QualifiedName HsTypeClass` values; and `mkStaticClassEnv`
returns `Either ClassEnvError StaticClassEnv`. `StaticClassEnv` and
`QueryClassEnv` expose read-only accessors rather than updateable record fields.
The generic `Data` instances that depended on the recursive representation
were removed. Imports through `Language.Haskell.Exference` and the former core
module paths remain available, but callers constructing class values must
adopt the checked API. `RigidIdentifierSupplyExhausted` is now positional, so
the partial record selectors `maximumPreexistingRigidIdentifier` and
`requestedRigidIdentifierCount` have also been removed; pattern matching still
exposes both values in the same order.

### Reusable core search inputs

New core clients should split the old all-in-one `ExferenceInput` at its actual
lifetime boundary. Construct an `EnvDictionary`, pass it once to
`mkExferenceEnvironment`, and retain the resulting abstract
`ExferenceEnvironment`. For each search, construct an `ExferenceQuery` and call
`findGeneratedSearchBatchesInEnvironmentEither` (or its `WithHints` variant).
The first operation validates environment names, generated syntax, ratings,
types, and class constraints once; the second validates only the goal,
constraints, limits, and heuristics that vary per query.

The compatibility field labels map to the reusable API as follows:

| `ExferenceInput` field | Reusable field |
| --- | --- |
| `input_envFuncs` | `EnvDictionary.environmentFunctions` |
| `input_envDeconsS` | `EnvDictionary.environmentDeconstructors` |
| `input_envClasses` | `EnvDictionary.environmentClasses` |
| `input_goalType` | `ExferenceQuery.queryGoalType` |
| no former field | `ExferenceQuery.queryExcludedBindings` |
| `input_allowUnused` | `ExferenceQuery.queryAllowUnused` |
| `input_allowConstraints` | `ExferenceQuery.queryAllowConstraints` |
| `input_allowConstraintsStopStep` | `ExferenceQuery.queryConstraintDeferralSteps` |
| `input_multiPM` | `ExferenceQuery.queryMultiConstructorPatterns` |
| `input_maxSteps` | `ExferenceQuery.queryMaximumSteps` |
| `input_maxQueueSize` | `ExferenceQuery.queryMaximumQueueSize` |
| `input_maxDepth` | `ExferenceQuery.queryMaximumDepth` |
| `input_heuristicsConfig` | `ExferenceQuery.queryHeuristics` |

`queryExcludedBindings` is a set of shared structural names, not rendered
spellings. Exclusion is exact: hiding `Data.Function.fix` does not hide an
unqualified `fix` or another module's homonym. The same reduced environment is
used by heuristic search and the independent candidate checker, and excluded
names do not leak into binding-use metadata. Legacy `ExferenceInput` entry
points remain compatibility adapters and behave as if this set were empty.

Completed candidates are simplified inside the core and the exact transformed
tree is independently type-checked before it is returned.  Simplification is
environment-free and never invents globals such as `id` or `(.)`.  The typed
candidate is then erased into `Language.Haskell.Synthesis.Generated`, the same
scope-safe output tree and renderer used by Djinn.  That shared boundary
allocates names by variable identity, avoids binder/global capture, and applies
one qualification policy.  The `haskell-src-exts` converter remains only as a
compatibility frontend and consumes the shared allocator rather than owning a
second naming implementation. Its `expressionToHaskellSrc` and
`functionToHaskellSrc` conveniences validate Exference expressions before
conversion; the lower-level `generatedExpressionToHaskellSrc` and
`generatedFunctionClauseToHaskellSrc` functions accept the shared output IR
directly. The raw-name function convenience rejects invalid definitions before
constructing a clause; shared clauses already carry an opaque checked
`DefinitionName`. Every entry point reports free locals, malformed syntax, and
globals that qualification would turn into accidental recursion instead of
exposing an unchecked rendering path.

`findQueryResultsInEnvironmentEither` is the canonical core result API.
Repeated callers seal an abstract `ExferenceEnvironment` once, then supply its
varying `ExferenceQuery`, exact checked `DefinitionName`, and source type-name
hints as an opaque `ExferenceSourceTypeVariableHints`. Core callers
construct it with `mkExferenceSourceTypeVariableHints`; the frontend SPI keeps
its raw-map signature only as a source-compatible checked entrance. The core
inserts the exact target into the excluded-binding set before checking the
query, prepares and validates the query once, and derives final flexible and
rigid rendering hints from that same retained rigid-instantiation plan. Shared
synonym expansion alpha-freshens binders introduced by every direct, nested,
or zero-argument alias away from the complete original source namespace. The
stable adapter can therefore retarget only genuinely surviving hints after
elaboration; the core additionally rejects any hint witness whose canonical
goal differs from the prepared query. The target then
becomes the name of every generated `FunctionClause`; it is never round-tripped
through source text. `runExferenceQuery` returns those core-built
`QueryResult`s directly rather than rebuilding progress, evidence, metadata,
or candidates in the adapter. Type-source provenance is applied to
elaboration, lowering, and query-input failures. Search controls are supplied
independently and therefore report `DJEX_EXF_OPTIONS` without falsely pointing
at the type text.

The older `findGeneratedSearchBatchesEither` and
`findGeneratedSearchBatchesWithHintsEither` entry points remain core
compatibility conveniences for callers that do not need the checked target or
logical-evidence envelope. Their historical raw final-hint map and
`typeVariableHints` helpers remain explicitly unchecked compatibility
surfaces; canonical result search accepts only the opaque source form, and the
stable residual renderer validates bounded copies, rejects partial/infinite or
malformed names, deduplicates preferences, and freshens fallbacks before any
such compatibility value reaches output. Expression and definition rendering
applies the same bounded-copy rule to caller-built term-local hints while
retaining structured lexical errors for finite bad names. All paths still
project the engine trace lazily:
candidate conversion never traverses the whole search. Every canonical result
contains a shared `Candidate` whose output is the exact-target
`FunctionClause`, together with fully shared residual constraints and
`ExferenceCandidateDetails`. The details retain search statistics and both
term-local and tagged flexible/rigid type-name hints, so erasing backend
annotations does not degrade later rendering. Each demanded candidate is fully
detached from its typed search tree; private engine chunks supply shared
progress directly, with compatibility chunks as a sibling projection rather
than the modern API reinterpreting legacy status.

`toGeneratedSearchBatch` remains the checked adapter for caller-constructed
status-bearing compatibility chunks. It rejects malformed generated syntax,
invalid scope, unfinished holes, malformed constraints, contradictory status,
and negative binding-use counts, while typed expressions stay available
through the historical API.
Each shared batch carries
`ExferenceBatchMetadata`: exact `Natural` binding-use counts keyed by nominal
`QualifiedName`, plus cumulative queue- and depth-pruning counts. Keeping the
counts in metadata makes partial progress observable even though shared
`Progress` records pruning reasons only when a search terminates. Historical
status-bearing chunks retain their `Int` binding counts: engine totals saturate
at that compatibility boundary, while caller-constructed negative counts are
rejected before the chunk can enter the modern batch API.

Candidate metrics, candidate details, and batch metadata now each have one
core-owned record and one storage representation. The polished stable names
`ExferenceCandidateMetrics`, `ExferenceCandidateDetails`, and
`ExferenceBatchMetadata` are type aliases with bidirectional record-pattern
views over those records; no adapter copies their fields. Local/type-variable
hints use the shared variable tags, exact `Natural` binding usage is keyed by
shared `Name`, and the retained `ExferenceInventory` has backend ratings erased
by a total functor map.

Because those three stable names are aliases rather than new data types, an
explicit compatibility import must replace `T(..)` with the type, pattern, and
any selectors it uses, and must enable `PatternSynonyms`. For example:

```haskell
{-# LANGUAGE PatternSynonyms #-}

import Language.Haskell.Djex.Exference
  ( ExferenceCandidateDetails
  , pattern ExferenceCandidateDetails
  , exferenceCandidateStatistics
  , exferenceCandidateLocalNames
  , exferenceCandidateTypeVariableNames
  )
```

Use the same form for `ExferenceCandidateMetrics`, importing
`pattern ExferenceCandidateMetrics`, `exferenceCandidateSteps`,
`exferenceCandidateComplexity`, and `exferenceCandidateFinalQueueSize`.
`ExferenceBatchMetadata` similarly requires `pattern ExferenceBatchMetadata`,
`exferenceBatchBindingUsages`, `exferenceBatchQueuePruned`, and
`exferenceBatchDepthPruned`. Broad module imports need no change. The stable
pattern fields work for selection, matching, and construction, but GHC
record-update syntax does not update through a pattern synonym; match and
reconstruct the value when a stable field must change.

This representation migration is intentionally source- and ABI-visible for
the experimental package. Derived `Show` now uses the canonical core
constructor and field spellings, and `Typeable` identity, generic
representation, and data-constructor symbols are those of the core-owned
record. Recompile dependants rather than mixing artifacts across this change;
code that needs the polished presentation vocabulary should format through the
stable selectors instead of depending on derived `Show` text.

Stable callers may construct the retained inventory through
`mkExferenceSession` from a neutral `ExferenceEnvironment`. Source clients
additionally import
`Language.Haskell.Djex.Exference.HaskellSrc` and use `loadExferenceSession` for
Haskell source directories. The raw
`CheckedSourceEnvironment -> ExferenceSession` bridge lives separately in
`Language.Haskell.Exference.Session`; no parser type is retained in a sealed
session. `ExferenceSessionPolicy` supplies exact-name exclusions and finite,
signed rating overrides while the private search projection is built. Unknown
override names and non-finite ratings fail explicitly, and overrides preserve
source/declaration order. The adapter's expression and definition conveniences
supply the retained local-name hints to the shared candidate renderer and
expose the common `RenderError` directly.

The session retains one annotation-free shared prepared-inventory witness, its
policy-filtered search environment, and a fully evaluated structured omission
summary. The source wrapper and its complete unfiltered backend are consumed
during sealing rather than retained beside that filtered view.
Parser-independent type-name and class-arity resolvers are derived from the
witness Inventory rather than stored as parallel caches. Parsed and
programmatic goals then pass through the same
capture-avoiding shared synonym elaborator and its pre/post kind checks before
core lowering. The former HSE `TypeDeclMap` is therefore a loader concern
rather than hidden session state.

Symmetric unification keeps goal and provider variables tagged until the final
projection, so substitutions returned for either side are closed even when the
two inputs reuse numeric IDs.  The independent checker consumes every prenex
`forall` layer with the same rigid-ID order as search, and type rendering uses
one source-name map for quantifiers, constraints, and body occurrences.

The status-bearing search API is `findExpressionsWithStatsEither`. It retains
structured input failures and distinguishes a genuinely exhausted search space
from a step-limited search and one made incomplete by queue/depth pruning. The
validator also rejects negative step counts for delayed constraint solving,
rather than accepting a setting whose threshold can never be reached. It checks
every goal, binding, deconstructor, and explicit constraint argument through
the shared type vocabulary, including rank-N occurrences in function contexts
that the historical filter overlooked. The
`toSearchProgress` projection maps those compatibility statuses to
`Language.Haskell.Synthesis.Search`, retaining simultaneous queue and depth
pruning reasons and rejecting malformed hand-constructed status values. It
does not turn heuristic exhaustion into a logical uninhabitability claim. The
historical list-returning entry points remain compatibility adapters (including
their “invalid input means no elements” convention): `findExpressions` exposes
the raw result stream, and `findOneExpression` is simply its first element. The
duplicated historical `SearchSelection` and rating/lookahead `find*`/`select*`
presentation family has been retired. Callers should construct a checked
session and execute requests with
`Language.Haskell.Djex.Exference.runExferenceQuery`, then use
`Language.Haskell.Synthesis.Selection.selectQueryResults` (or
`selectPreferredQueryResults` for the constraint-free preference policy).
Each returned batch uses the shared checked result boundary to derive
`ValidatedCandidates` exactly when the batch is nonempty; an empty heuristic
batch records `NoEvidence` without conflating exhaustion with a proof.
The shared `Selection` result preserves the last inspected progress alongside
the selected candidates. The executable therefore validates and runs search
once, then says when an empty result is conclusive versus when inhabitation
remains undecided.

The `exference` executable is a normal build target again. Its implementation
lives in `Language.Haskell.Exference.CLI`, making the executable component a
pure launcher. The serial search accepts caller-supplied heap and GC `+RTS`
tuning but no longer starts one capability per core or reserves a
multi-gigabyte heap for trivial invocations. Its obsolete Hood, search-tree,
parallel-mode, and embedded manual-test machinery has been removed;
deterministic regressions live in `exference-tests`, the frontend-import check,
and the separate `exference-cli-tests` subprocess suite.

```console
cabal run exference -- --first "a -> a"
```

## Usage notes

There are certain types of queries where *Exference* will not be able to find
any / the right solution. Some common current limitations are:

- By default, searches **only for solutions where all input is used up**, e.g.
  `(a, b) -> a` will not find a solution (unless given `--allowunused` flag).
  Often this is the desired behaviour, consider queries such as
  `(a->b) -> [a] -> [b]` where a trivial solution would be `\_ _ -> []`.
  This also means that certain functions are not included in the environment,
  e.g. `length` or `mapM_`, as they "lose information";
- Type synonyms are kind-checked in their unexpanded source applications and
  expanded only after inventory sealing; cycles, unsaturated uses, and
  ill-kinded phantom arguments are rejected with diagnostics;
- Source inventories and queries are kind-checked against the same retained
  assumptions; an ill-kinded application such as `Maybe Maybe` is rejected
  before search;
- The source loader represents ordinary positional, infix, and record
  datatypes (including strict/unpacked fields and rated selectors), type
  synonyms, classes, instances, and value/method signatures. Type/data
  families, GADTs, datatype contexts, kinded parameters, existential or
  constrained constructors, derived or overlapping instances, functional
  dependencies, associated families/defaults, declaration splices, and role
  annotations fail before inventory construction as source-spanned
  `EXF_UNSUPPORTED_VOCABULARY` diagnostics; they are never silently omitted;
- The environment is composed by hand currently, and does only include parts
  of base plus a few other selected modules. Its canonical inventory of 41
  classes and 432 source instances is checked at load time and expands to 535
  lookup instances after superclass inflation; all counts are pinned by tests.
  The [normalization report](docs/reports/2026-07-11-environment-normalization.md)
  documents its naming and validation rules. Additions welcome!
- Pattern matching on multiple-constructor data types is disabled by default;
  an experimental opt-in is described below;
- Recursive datatypes remain valid input, but recursive deconstructors are
  currently omitted from search with a structured
  `RecursiveDataEliminationUnsupported` warning rather than risking an
  unsound or divergent elimination projection. This includes the built-in
  list deconstructor, so HSE-loaded sessions report the limitation explicitly;
- See also the detailed feature description in the [exference.pdf](https://github.com/lspitzner/exference-paper/raw/master/exference.pdf) report.

## Experimental features

- Pattern-matching on multi-constructor data types can be enabled via
  `-c` / `--patternMatchMC`, but can reduce performance significantly for
  non-trivial queries. It is intentionally not part of the default search
  policy while that branch of the core algorithm remains expensive.
- Chains of outer (prenex) `forall`s are supported. Rank-N positions are
  rejected conservatively; the historical implementation erased some nested
  quantifiers during unification, which was not a sound implementation of
  subsumption.

## Other known (technical) issues

- **Memory consumption is large** (even more so when profiling);
- Environment loading, checked inventory sealing, search execution, and result
  selection now have reusable library boundaries. `Language.Haskell.Djex`
  identifies the two backends and re-exports their checked session/query APIs.
  The backend-selecting `djex exference` driver consumes that stable facade
  and, for source-backed sessions, the explicit
  `Language.Haskell.Djex.Exference.HaskellSrc` frontend. The native
  name/constraint/type migration has removed Exference's
  duplicate source-type IR, and both engines now construct their stable result
  envelopes directly. Exference source checking and both stable sessions now
  make their shared inventories authoritative. Djinn's prepared proof caches
  are now inventory-derived, and the shared foundation plus both parser-free
  cores compile as one `djex` library. The two backend search algorithms remain
  intentionally independent for now.
- The detailed [Djinn/Exference integration audit](docs/reports/2026-07-11-djinn-integration-audit.md)
  records concrete correctness reproducers, shared-IR boundaries, and the
  staged migration order.

## Contributing

### environment

If you want to add new elements to the environment, be careful not to add
functions that
- are just synonyms of other functions (including cases such as `mapM` vs `forM`);
- lose information, e.g. `void :: Functor f => f a -> f ()`;

and avoid adding functions that
- are polymorphic in their return type (as they increase the search space
  for any query) - if really necessary, they can be added including an
  appropriate rating entry;
- are just more specific versions of existing functions.

## Trivia

* The author did not learn about the term "entailment" until after implementing
  the respective part of the algorithm.
* *Exference* was used at least once to implement some typed hole in its own
  source code.
