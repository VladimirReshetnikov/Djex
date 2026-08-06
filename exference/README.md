# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\b -> fmap (\g -> show (b g))`

[Djinn](https://hackage.haskell.org/package/djinn) is a well known tool that
does something similar; the main difference is that *Exference* supports a
larger subset of the haskell type system - most prominently type classes. This
comes at a cost, however: Exference is a bounded heuristic search rather than
an inhabitation decision procedure. Every checked query has a positive step
limit and terminates, and accepted nominal instance rules have a separate
termination check. Exhausting those configured bounds without a candidate is
still not a proof that the requested Haskell type is uninhabited; Djinn can
make that stronger claim for the intuitionistic fragment its prover supports.

## References and environment

- **Documentation:** [exference.pdf](https://github.com/lspitzner/exference-paper/raw/master/exference.pdf)
  describes the original implementation and its properties.
- The executable loads Djex's [packaged local environment](environment/) of
  functions, classes, instances, and ratings by default. Installed builds find
  it through Cabal rather than depending on a source checkout.
- A custom `--envdir` is scanned non-recursively. Immediate `.hs` modules and
  `.ratings` files are each processed in lexical filename order. Rating files
  contain whitespace-separated `name finite-number` pairs; malformed files,
  duplicate or unknown ratings, and missing ratings are diagnosed while
  affected declarations keep their neutral rating.
- Historical Exference releases advertised `exferenceBot` on Freenode's
  `#exference` channel. That bot is not a maintained interface to this tree;
  use the local executable for reproducible behavior against the packaged
  environment.

## Building from source

Exference is part of the unified `djex` Cabal package. Its library and
deterministic test suites build with the supported GHC 9.12.4 toolchain using
Cabal 3.16.1.0. Cabal is the maintained build path; the historical Stack file
targeted LTS 5.18 and
dependencies that no longer exist in this tree, so it has been removed rather
than pretending to provide a second supported toolchain. Run these commands
from the repository root:

```console
cabal build djex:lib:djex djex:exe:exference
cabal test djex-api-tests exference-frontend-api-tests exference-tests exference-engine-tests exference-cli-tests --test-show-details=direct
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
solving, and represent every quantified subtree as an alpha-aware opaque atom.
They can compare or bind that whole atom but never decompose its quantified
body. Declaration ingress and egress likewise use one checked native
type/constraint operation; the former converted/lowered pair was only a name
distinction after `HsType` became the shared tree. The right-directed matcher
uses the same tagged solver while
keeping left-side variables rigid, so equal numeric IDs from its two inputs
cannot alias accidentally. Exference's substitution API likewise delegates to
the shared scope-aware simultaneous substitution primitive, including
capture-avoiding alpha-renaming under foralls. That representation-level API
remains total for native rigid-binder forms; checked search boundaries reject
those unsupported binders before execution.
The stable candidate, constraint, and substitution signatures call this native
tree `HsType` directly. The short-lived merger synonym `SynthesisType` has been
removed: it was exactly the same type, not a separate checked representation.
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
Exference's enabled Haskell type grammar (including its extension keywords)
and fully detaches and forces the spelling entries while sealing. Context
arguments remain untouched until a session can use the owning class's known
arity as a structural bound. After that bounded normalization, execution
checks contextual scope, collapses aliases to the lexicographically least
spelling, and binds the opaque hints to the exact canonical contextual goal.
Malformed spellings therefore fail early as source-located
`DJEX_EXF_SOURCE_HINT` diagnostics, while scope failures retain the same
provenance at the session-aware boundary rather than reaching rendering.
This mirrors Djinn's library-first organization while retaining the
parser/source boundary at the module level.

The bundled source modules and parser-neutral core now inhabit the same
library. They meet directly at the hidden request, session, and finite-variable
supply owners; the former `FrontendSupport` service-provider module was only a
bridge between the retired Cabal components and has been removed. Stable
clients use `Language.Haskell.Djex.Exference.HaskellSrc` for source loading and
parsing or the neutral `Language.Haskell.Djex.Exference` API. The request
representation remains hidden, so stable clients
therefore see an opaque `ExferenceRequest` with one neutral smart constructor;
the shared `CachedQuery` owns its strict complete source provenance, while
its backend cache is an opaque canonical-goal and detached-spelling plan. The
neutral slot preserves the caller's exact `QueryRequest`, matching Djinn.
Execution bounds known class argument spines from the selected session before
building the canonical contextual goal and its checked source-hint witness.
Neither provenance nor that private plan participates in request equality or
display; sealing
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
Checked module loading resolves bare type and class names through the defining
module's direct imports and rejects ambiguity instead of choosing by load
order.

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

Generated terms likewise use the shared expression tree. The historical
`ExpVar`, `ExpName`, `ExpLambda`, `ExpApply`, `ExpTypeApply`, `ExpHole`,
`ExpLetMatch`, `ExpLet`, and `ExpCaseMatch` views remain available, and
`ExpTuple` exposes the shared structural boxed-tuple form. A saturated boxed
tuple goal is introduced directly, like a lambda, so it does not require or
record an ordinary tuple-constructor binding. Excluding or rerating `(,)`
therefore affects its binding and partial-application paths, not structural
introduction of an already fixed pair goal.

Class constraints use `Language.Haskell.Synthesis.Constraint` directly as
`Constraint HsType`; the historical `HsConstraint` constructor spelling is a
bidirectional compatibility pattern. Class declarations and instances live in
sealed strict maps built by
`mkStaticClassEnv`, which
checks names, duplicate declarations/parameters, superclass variables and
cycles, instance-head duplicates modulo alpha-renaming, referenced classes,
exact arities, and every native constraint-argument type before superclass
inflation. Query-constraint closure and derived
instance-head indexing share one immediate-superclass instantiator, including
its exact-arity guard and capture-safe parameter substitution.
Query and binding inputs likewise reject wrong arities for known classes while
retaining unknown classes as explicit external constraints.

Stable programmatic requests validate the goal and context class headers while
sealing, but defer context arguments until execution. A session-known class
provides the exact traversal bound, so a cyclic or over-applied argument list
fails after observing at most one cell beyond the declaration width; no
arbitrary maximum class arity is imposed. Finite unknown external constraints
retain the existing open-world policy.

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
and `fromSynthesisType` both call the foundation's single checked
`normalizeType` boundary over the complete tree, including explicit foralls.
The unifier consumes that same checked normalization before changing ordinary
structure to its applicative higher-kinded view; each quantified subtree is
instead lowered to the shared opaque `TypeAtom` representation.
The checked constraint operations validate the class namespace and normalize
their argument types without reconstructing the outer constraint. Because
`HsType` inherits the shared structural `Show` instance, diagnostics and other
source-like output should call `showHsType` (and `showHsConstraint`) rather than
relying on `show`.

The independent generated-expression checker validates every reachable native
type and constraint before inference. Its raw environment must also have
unique function, constructor, and datatype-eliminator identities; the same
validator runs during live search sealing, so neither path can resolve an
ambiguous name by list order. It also validates unused function and constructor
names through the shared generated-syntax boundary, and rejects unbound or
duplicate local identities before its inference map can hide a malformed
pattern. Rank-N occurrences are validated across unused bindings, datatype
fields, the complete class/instance environment, expected constraints, and
generated annotations rather than only when inference happens to reach them.
The checker and search use both the same opaque, alpha-aware unifier and the
same scoped provider-use rules. An exact quantified occurrence remains opaque;
a bounded rule can shallowly subsume one context-free prenex scheme to another;
and a monomorphic occurrence freshly instantiates the provider's leading
foralls. A quantified expected type, including one with direct contexts, can
also open with fresh branch-local rigids so its body is checked structurally,
after the ordinary opaque provider routes have been tried. Its substituted
contexts are lexical givens for that body, not ambient evidence for sibling
work. These are explicit typing rules, not
permission for the unifier to decompose a quantified body. Search records the
requested scheme on a subsumed occurrence, and the independent checker
reclassifies or structurally checks that occurrence instead of trusting the
search result. They also share the
higher-kinded view of structural functions and tuples, while tuple complexity
replays the historical left-associated constructor/application accumulation
exactly so floating-point rounding and saturation cannot perturb queue order.

`Language.Haskell.Exference.Core.Declaration` converts function bindings,
classes, instances, and deconstructor/data records to the shared declaration
IR; the HSE frontend uses the same boundary for type synonyms. Function
and constructor search penalties and recursive-datatype flags survive as
explicit metadata. Class methods are nested under their shared class
declaration with their rating intact; the implicit owner constraint required
by Exference's flat search binding is derived from the class parameters,
checked and removed while nesting, and restored exactly once while lowering.
An unconstrained flat binding crosses this boundary as a plain monotype; the
reverse adapter still accepts the historical explicit `TypeForall [] []`
wrapper and canonicalizes it on the next shared projection. A nonempty
constraint context retains its binderless `forall`, because that node owns the
context.
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
superclass-completed resolution index, so adapters serialize source facts
rather than derived cache entries. The core-only adapters still reject frontend-only
declarations such as type synonyms because the search dictionary has no
representation for them.

The HSE loader returns `IO (LoadReport CheckedSourceEnvironment)`: fatal phases
use a typed error, while nonfatal warnings and summaries are structured shared
diagnostics. Fatal module parse diagnostics retain HSE's exact source filename
and point location under the stable `EXF_MODULE_PARSE` code, with the native
parser detail preserved as context. HSE locations cross the shared checked
one-based, half-open span boundary explicitly; a malformed native location
retains its source and becomes diagnostic context rather than causing a crash
or forging an invalid span. Duplicate explicit module headers and multiple
headerless `Main` modules fail before nominal scope maps are built; ordered
`EXF_MODULE_DUPLICATE` diagnostics identify every later source and the first
declaration it conflicts with. Binding extractors attach module-local source
slots before erasing HSE annotations. Ordinary signatures and datatype
declarations occupy top-level slots; class methods occupy nested slots, and
every multi-name signature remains one success batch or one failure. The final
stable merge
therefore preserves exact cross-category binding and diagnostic order without
reconstructing declaration cardinality. Low-level `parseModules` keeps aliases
unexpanded in its `SourceEnvironment`; `checkSourceEnvironment` seals and
kind-checks that source graph once, then sends the checked Inventory through
the parser-independent neutral lowerer. The resulting backend projection is
reconciled by name with the original binding/deconstructor order and ratings.
That exact multiset check lives only in the opaque core projection: the HSE
frontend supplies presentation order and ratings without prevalidating the same
binding names a second time. Its ordered deconstructor records contribute only
their nominal heads; every projected shape still comes from the prepared
witness. The unused intermediate entrance that accepted constructor penalties
but could not retain class-method ownership has been removed; source sealing
always uses the complete ownership-aware operation.

All public, default, directory, file, and in-memory source loaders elaborate
declarations with the same module-aware resolver. Local type and class names
take precedence; direct imports honor `qualified`, `as`, positive import lists,
and `hiding`. Export surfaces include named exports and `module M` re-exports,
and re-exported entities retain their defining canonical names. The
`module M` surface is the identity intersection of unqualified scope and scope
through the written qualifier `M`, so self locals and ordinary aliases are
included while qualified-only imports are not. A loaded `Prelude` contributes
an implicit unqualified import unless the module is `Prelude`, imports it
explicitly, or enables `NoImplicitPrelude` or `RebindableSyntax`. The resolver
covers datatype fields, type synonyms, class heads and methods, instances, and
ordinary or foreign signatures; term bodies remain outside Exference's source
semantics.

Fixity declarations are not part of that neutral source inventory. The loader
therefore rejects an unparenthesized chain of type operators with a located
`UnparenthesizedTypeOperatorChain` occurrence rather than preserving a tree
whose association depended on discarded metadata. One infix application is
supported, and either association of a longer chain is supported when written
with explicit parentheses.

Ordinary module imports must be acyclic. The loader reports
`CyclicModuleImports` at the import closing the first stable cycle before it
computes export surfaces; `{-# SOURCE #-}` imports remain interface edges that
break the dependency graph. This prevents cyclic re-exports from acquiring a
meaning based on module-count iteration parity.

Imports govern elaboration, not dependency discovery. Explicit file and source
loaders consume exactly the ordered closure supplied by their caller; the
unified REPL discovers resolvable local imports before passing that snapshot to
the same loader. Loaded modules have an exact surface, so an unimported loaded
declaration is out of scope even through its canonical qualifier. Djex does not
load external interfaces, however, and open inventories retain genuinely
unknown names as external. A positive import list provides a finite canonical
surface for an unloaded module. Unrestricted imports remain open; `hiding`
imports remain open except for their exact negative occurrences. Import routes
are resolved separately, so sharing an alias with an exact loaded import does
not close an unrelated external module.

Package-qualified imports are rejected consistently by all loaders, whether or
not a later declaration uses them. Package identity is not representable in
the shared nominal name, so treating such an import as ordinary source or as
an unknown external module would be unsound.

Export lists restrict downstream imports but never delete declarations from
the checked inventory. Private declarations remain available to whole-graph
kind, synonym, recursion, and class validation and to an explicit `*MODULE`
prompt scope. Source imports are consumed while that inventory is built;
interactive prompt imports are a later query/search projection and do not
reinterpret source declarations or discover dependencies.

Before backend lowering, the foundation's opaque transient
`PreparedInventoryExpansion` prepares the exact alias table, expands
operational declarations in source order, attributes the first failure to its
owning declaration, and derives the recursive datatype set once. Exference
then performs only its private variable-ID normalization and lowering, using
that set to disable recursive elimination. One opaque annotated prepared value
wraps the shared `PreparedInventory` and its backend lowering, keeping the
checked Inventory, its exact synonym table, and that lowering inseparable; the
expanded declaration copy is released after sealing. The retained
frontend/core module seam
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
backend. Thus synonym expansion, forall freshness, declaration attribution,
and whole-inventory recursion classification have one implementation;
class/instance normalization remains an Exference lowering detail, while the
checked Inventory still retains the source aliases needed by later queries.
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
ordinary value. Reconciliation canonicalizes structural and saturated
function/tuple spellings before comparing the compatibility records.
Constructor shape and search penalty are then lowered back from that checked
inventory rather than trusted from a parallel raw record.
The default source environment derives its flat constructor functions from
those same ordered datatype records. Shared names and types represent boxed
tuples through arity 64, while the eager Exference search inventory deliberately
materializes only arities 2 through 7: higher eager constructors would add a
partial-application branch to every non-arrow goal. This operational cap is
exported as `maximumBuiltInTupleArity` rather than repeated as a magic number.
It limits materialized constructor bindings, not saturated structural tuple
introduction, which accepts every boxed arity validated by the shared type
model. Unit remains on its existing constructor-binding path.
Recursive flags come from the shared alias-expanded inventory witness across
all loaded modules and appear in both the backend-derived projection and
checked Inventory; caller-supplied or module-local preliminary bits are never
authoritative.
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
datatype stubs acquire a missing kind shape from instances. The packaged
environment first uses that rule to infer the complete kind of each opaque
base-library stub, then replaces the stub with an abstract declaration before
building Exference's elimination environment.
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
projection. Recursive datatype eliminators use the same bounded one-layer
policy in either path; the source path preserves its historical ratings
and equal-cost order. Its explicit `prepareSourceSynthesisInventory` refinement
also copies alias-aware recursion into the retained source annotations; there
is no separate neutral Inventory type or preparation API. The CLI parses
requests through that session; a
query whose proper-type obligation conflicts with retained constructor or
class kinds cannot reach heuristic search.

The executable no longer rebuilds an `ExferenceInput`, maintains a second
rank-N filter, or consumes legacy tuple chunks. It maps flags to
`ExferenceOptions`, calls
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
size rather than rendered identifier length. Recoverable loader and session
warnings are also written to stderr at every verbosity; informational loading
summaries remain quiet by default and retain their historical stdout output
under `--verbose`. Warnings accumulated before a later fatal inventory check
remain visible instead of being discarded with the unusable environment.

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
`ExferenceEnvironment`. For each search, construct an `ExferenceQuery`, an
exact checked `DefinitionName`, and opaque source type-name hints with
`mkExferenceSourceTypeVariableHints`, then call
`findQueryResultsInEnvironmentEither`. Environment construction validates
names, generated syntax, ratings, types, and class constraints once; query
execution validates only the goal, constraints, limits, and heuristics that
vary per request, and verifies that its opaque source hints belong to that
prepared goal. The checked target is excluded from the same request before
search begins. The stable `runExferenceQuery` checks options before context
preparation and elaboration, then carries a private checked-options witness
into core query preparation so search controls have one validation authority
and one evaluation.

`Language.Haskell.Exference.Core.defaultHeuristicsConfig` owns the
parser-neutral library default used by reusable and stable queries. The
historical `Language.Haskell.Exference.SimpleDict` import path directly
re-exports that same binding. The command-line frontend deliberately retains
its distinct interactive ranking profile. The core and checked Djex facades
also re-export one canonical `ExferenceOptions` type and
`defaultExferenceOptions`; an `ExferenceQuery` stores that options value
directly beside its goal and exact exclusions, so a stable request no longer
copies eight controls into a parallel query record.

The compatibility field labels map to the reusable API as follows:

| `ExferenceInput` field | Reusable field |
| --- | --- |
| `input_envFuncs` | `EnvDictionary.environmentFunctions` |
| `input_envDeconsS` | `EnvDictionary.environmentDeconstructors` |
| `input_envClasses` | `EnvDictionary.environmentClasses` |
| `input_goalType` | `ExferenceQuery.queryGoalType` |
| no former field | `ExferenceQuery.queryExcludedBindings` |
| `input_allowUnused` | `querySearchOptions.exferenceAllowUnused` |
| `input_allowConstraints` | `querySearchOptions.exferenceAllowResidualConstraints` |
| `input_allowConstraintsStopStep` | `querySearchOptions.exferenceConstraintDeferralSteps` |
| `input_multiPM` | `querySearchOptions.exferenceMultiConstructorPatterns` |
| `input_maxSteps` | `querySearchOptions.exferenceMaximumSteps` |
| `input_maxQueueSize` | `querySearchOptions.exferenceMaximumQueueSize` |
| `input_maxDepth` | `querySearchOptions.exferenceMaximumDepth` |
| `input_heuristicsConfig` | `querySearchOptions.exferenceHeuristics` |

Code written against the short-lived flat `ExferenceQuery.query*` control
fields should move those construction and update expressions under the
`querySearchOptions` field. The historical flat `ExferenceInput` constructor
and record fields remain unchanged; its compatibility boundary performs the
only necessary projection into `ExferenceOptions`, without changing validation
or first-error precedence.

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
directly. Expression conversion decomposes the shared leading-lambda spine, and
function conversion uses the same shared expression-to-clause promotion as
Djinn rather than a frontend-private lambda walk. A malformed empty lambda is
left visible to syntax validation instead of disappearing during promotion.
The raw-name function convenience rejects invalid definitions before
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

The canonical result API is the only shared candidate envelope exposed by the
core. It accepts only the opaque, exact-goal-bound source-hint form and returns
the checked target and logical evidence together with each lazy result batch.
The stable residual renderers validate each class identity and every complete
shared type argument in original candidate order before deduplicating or
emitting source. They return `Either ExferenceResidualRenderError [String]`,
whose failures retain zero-based constraint/argument positions and the shared
structural error. This intentionally replaces the former unchecked pure-list
signature: compatibility `Candidate` values can be caller-built even though
canonical query results are checked. The renderers also validate bounded hint
copies, reject partial/infinite or malformed names, deduplicate preferences,
and freshen fallbacks. The qualification-aware entry point applies the
generated term's exact identifier/operator policy to both class names and
nested type names; the no-policy helper remains fully qualified.
Expression and definition rendering applies the same bounded-copy
rule to caller-built term-local hints while retaining structured lexical
errors for finite bad names. Candidate projection never traverses the whole
search. Every canonical result contains a shared `Candidate` whose output is
the exact-target
`FunctionClause`, together with fully shared residual constraints and
`ExferenceCandidateDetails`. The details retain search statistics and both
term-local and tagged flexible/rigid type-name hints, so erasing backend
annotations does not degrade later rendering. Each demanded candidate is fully
detached from its typed search tree, and private checked engine batches supply shared
progress directly rather than passing through a public intermediate batch.
Each canonical batch carries
`ExferenceBatchMetadata`: exact `Natural` binding-use counts keyed by nominal
`QualifiedName`, plus cumulative queue- and depth-pruning counts. Keeping the
counts in metadata makes partial progress observable even though shared
`Progress` records pruning reasons only when a search terminates. Historical
status-bearing chunks retain their `Int` binding counts and engine totals
saturate at that compatibility boundary. Those raw chunks are an outward-only
historical projection; they cannot enter or stand in for the canonical result
API.

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
Haskell source directories. That source-facing module is an outward-only
facade: after loading the checked prepared witness, it seals through the
private session owner directly rather than routing stable construction back
through a historical public adapter. `Language.Haskell.Exference.Session`
remains a sibling compatibility entrance for callers that already own an
opaque `CheckedSourceEnvironment`; both entrances consume the same
annotation-erased prepared witness and converge only at the one private sealer.
No parser type is retained in the resulting session.

Callers that already hold an exact in-memory directory snapshot can pass its
module, rating, and visibility sources together to
`environmentFromSourcesWithTypeVisibility`. The corresponding stable session
entrances are `loadExferenceSessionFromSourcesWithTypeVisibility` and
`loadExferenceSessionFromSourcesWithTypeVisibilityWithPolicy`. Their source
lists are, in order, modules, ratings, and visibility snapshots; the
policy-aware variant prepends its policy. A nonempty visibility list is one
complete manifest for the supplied module snapshot, and none of these inputs
is reopened from the filesystem. The older
`environmentFromSources`, `loadExferenceSessionFromSources`, and
`loadExferenceSessionFromSourcesWithPolicy` deliberately pass an empty
visibility list. Like the explicit file loaders, they remain manifest-blind
and preserve ordinary Haskell empty-datatype semantics.

`ExferenceSessionPolicy` supplies exact-name exclusions and finite,
signed rating overrides while the private search projection is built. Unknown
override names, overrides for omitted or excluded bindings, and non-finite
ratings fail explicitly; accepted overrides preserve source/declaration order.
The adapter's expression and definition conveniences
supply the retained local-name hints to the shared candidate renderer and
expose the common `RenderError` directly.

The session retains one annotation-free shared prepared-inventory witness, its
policy-filtered search environment, and a fully evaluated structured omission
summary. The source wrapper and its complete unfiltered backend are consumed
during sealing rather than retained beside that filtered view.
Parser-independent type-name and class-arity resolvers are derived from the
witness Inventory rather than stored as parallel caches. Parsed and
programmatic goals then pass through the same prepared-witness synonym
elaborator and its pre/post kind checks before core lowering. The session owns
that operation without exposing its private synonym table to the stable
adapter. The former HSE `TypeDeclMap` is therefore a loader concern rather than
hidden session state.

Symmetric unification keeps goal and provider variables tagged until the final
projection, so substitutions returned for either side are closed even when the
two inputs reuse numeric IDs. The root query's prenex `forall` layers use the
same rigid-ID order in search and the independent checker. Quantifiers reached
under an arrow, constructor, tuple, constraint, or spawned search goal remain
opaque to structural unification. When lambda introduction or pattern
elimination exposes one as the leading type of a scoped provider, the explicit
use-site rule described below may instantiate it. Type rendering uses one
capture-safe source-name plan for quantifiers, constraints, and body
occurrences.

The raw status-bearing API `findExpressionsWithStatsEither` retains structured
input failures and distinguishes a genuinely exhausted search space from a
step-limited search and one made incomplete by queue/depth pruning. Its
validator also rejects negative step counts for delayed constraint solving,
rather than accepting a setting whose threshold can never be reached. It checks
every goal, binding, deconstructor, and explicit constraint argument through
the shared type vocabulary. Rank-N occurrences that the historical filter
overlooked are retained as opaque atoms. This API remains for clients that need
raw expressions and compatibility status values; new core code should use
`findQueryResultsInEnvironmentEither`, whose private checked engine batches
provide exact shared progress without reinterpreting raw status. Neither API
turns heuristic exhaustion into a logical uninhabitability claim. The
historical list-returning entry points are deprecated compatibility adapters
(including their “invalid input means no elements” convention): core
`findExpressions`, `findExpressionsChunked`, and `findExpressionsWithStats`
all erase `ExferenceInputError`, while the older facade's `findOneExpression`
is simply the first element of that lossy stream. Use `findExpressionsEither`,
`findExpressionsChunkedEither`, or `findExpressionsWithStatsEither` when the
raw core API is unavoidable. The
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
once, reports resource truncation even when useful candidates were printed,
and says when an empty result is conclusive versus when inhabitation remains
undecided. A truncation warning does not change the successful process status.

The `exference` executable is a normal build target again. Its implementation
lives in `Language.Haskell.Exference.CLI`, making the executable component a
pure launcher. The serial search accepts caller-supplied heap and GC `+RTS`
tuning but no longer starts one capability per core or reserves a
multi-gigabyte heap for trivial invocations. Its obsolete Hood, search-tree,
parallel-mode, and embedded manual-test machinery has been removed;
deterministic regressions live in `exference-tests`, the frontend-import check,
and the separate `exference-cli-tests` subprocess suite. Artificial
finite-identifier, queue-capacity, saturation, and poisoned-value checks live
in `exference-engine-tests`. That component compiles the parser-neutral core
and its test seam as home modules, so
`Language.Haskell.Exference.Core.Internal.Testing` is not part of the library
API. The seam retains only its exercised raw compatibility-element and
canonical `QueryResult` paths; the unconsumed generated-batch intermediate and
private search-node export aliases remain gone.

```console
cabal run exference -- --first "a -> a"
```

## Usage notes

There are certain types of queries where *Exference* will not be able to find
any / the right solution. Some common current limitations are:

- By default, searches **only for solutions where all input is used up**, e.g.
  `(a, b) -> a` will not find a solution (unless given `--allowUnused` flag).
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
  classes and 432 source instances is checked at load time and produces 432
  resolution rules whose explicit heads retain stably deduplicated superclass
  prerequisites; all counts are pinned by tests. Given dictionaries still
  entail their transitive superclasses in the ordinary direction.
  The [normalization report](docs/reports/2026-07-11-environment-normalization.md)
  documents its naming and validation rules. Additions welcome!
- Pattern matching on multiple-constructor data types is disabled by default;
  an experimental opt-in is described below;
- Nonrecursive empty datatypes are always eligible for elimination, independent
  of the multiple-constructor flag. A value of an empty datatype can therefore
  satisfy any result type through an empty `case`; rendered source uses
  `case value of {}` and requires GHC's `EmptyCase` extension. A
  constructorless declaration is not automatically assumed empty in a curated
  directory that supplies the visibility manifest described below;
- Recursive datatypes remain valid input and may be eliminated through one
  constructor layer. Constructor fields become ordinary branch-local
  providers, but search does not eagerly decompose them again; the generated
  term is therefore a finite match, never a recursive definition or induction
  principle. Multiple-constructor recursive datatypes, including lists, still
  require the ordinary multiple-pattern opt-in. The legacy
  `RecursiveDataEliminationUnsupported` omission reason remains source
  compatible but is no longer produced by current sessions. Parameter
  specialization preserves quantified fields, so for

  ```haskell
  data Headed a = HeadedValue a (Headed a)
  ```

  the request
  `Headed (forall x. x -> x) -> (forall x. x -> x)` can project the first
  field when `input_allowUnused` is true. The recursive tail is deliberately
  unused, so the strict default rejects that candidate. Search and independent
  checking retain the complete typed constructor pattern; only the accepted
  stable generated candidate replaces the unused tail binder with `_`;
- See also the detailed feature description in the [exference.pdf](https://github.com/lspitzner/exference-paper/raw/master/exference.pdf) report.

### Constructorless declarations in curated directories

`environmentFromPath` discovers optional `*.visibility` sidecars alongside
the directory's `*.hs` and `*.ratings` files. Each non-comment line has one of
these forms:

```text
abstract Module.Type ARITY PARAMETER_KIND...
empty Module.Type ARITY PARAMETER_KIND...
```

`abstract` means that the source catalogue omitted the runtime constructors;
the type keeps the exact manifest kind checked against the complete source
inventory but has no Exference deconstructor. `empty` asserts a genuinely
uninhabited Haskell datatype and retains empty-case elimination. Each source
parameter has one ground kind token: `Type`, or a fully parenthesized arrow
such as `(Type->Type)`. Explicit kinds preserve otherwise unconstrained
higher-kinded parameters and are checked by resealing the source inventory.
If any visibility sidecar is present, every constructorless datatype must be
classified exactly once.
Duplicate or malformed entries, unknown names, entries naming an inhabited
datatype, missing entries, and arity drift fail closed with
`EXF_TYPE_VISIBILITY` diagnostics.

A comment is a line whose first non-whitespace character is `#`. Hashes later
in a line remain part of legal symbolic names such as `Module.(:#)`.

This interpretation is used by directory discovery and by the explicit
visibility-snapshot APIs described above. Legacy explicit-file and in-memory
source APIs do not infer or discover a manifest; they continue to give
`data Empty` ordinary Haskell empty-datatype semantics. The installed manifest
classifies the opaque base-library signature stubs as abstract and retains only
`Data.Void.Void` and `GHC.Generics.V1` as genuine empty datatypes.

## Checked provider-local candidates and assignments

`runExferenceQueryWithInstantiationCandidates` retains the original scalar
pool contract. Each `ProviderInstantiationCandidate` associates one already
established type with an exact retained global; the visible provider rule may
reconstruct bounded binder combinations from that provider-local pool.
`runExferenceQueryWithInstantiationAssignments` instead accepts
`ProviderInstantiationAssignment`, whose argument list is one complete ordered
leading-binder vector. Exact vectors are consumed once and are never flattened
or Cartesian-recombined. The parallel
`runExferenceQueryWithKindedInstantiationAssignments` accepts
`KindedProviderInstantiationAssignment`, pairing every argument with the
frontend-attested `GroundKind` of that provider-binder position.

All outer lists are globally bounded at 32 before any element is entered. The
assignment runners additionally bound every argument spine at four before
entering an argument. They then resolve the exact retained polymorphic provider,
require a nonempty vector whose length matches its complete leading forall
chain, and reject a contextual scheme. After those checks, the kinded runner
applies a productive node preflight to each supplied kind under the public
`maximumProviderInstantiationKindNodes = 129` bound. This happens before kind
inference, same-provider kind-vector equality, recursive conversion of that
supplied kind, or forcing the paired type. Cyclic kinds and finite trees above
129 nodes thus fail after a bounded observation, while the shared 64-tuple
constructor's right-associated all-`Type` kind remains accepted at exactly
`2 * 64 + 1` nodes. That kind capacity is separate from the four-argument
provider-vector limit.

Every candidate or argument is synonym-elaborated in the sealed session. The
legacy scalar Candidate route continues to require a closed, context-free
proper type and check it at kind `Type`; it remains proper-type-only. For a
legacy exact assignment, Exference infers every leading binder's ground kind
from the retained provider body, defaults a vacuous binder to `Type`, and
checks the argument at that inferred kind. For a kinded assignment, it checks
all observable binder uses against the supplied complete kind vector and
elaborates each paired argument at its supplied kind. A vacuous binder has no
body occurrence capable of proving its source kind; that `GroundKind` is
therefore caller-attested rather than inferred by Exference.
Repeated assignments naming one provider must agree on their complete kind
vector before their type vectors are alpha-deduplicated. Assignment arguments
in both forms must be closed, context-free, and representable as specified
visible type arguments.
The complete substituted provider body is independently checked at kind
`Type`, after which the private core rechecks provider closure, context, arity,
and the lowered visible-argument shape. The legacy route supports
higher-kinded positions determined by the provider body and retains the
vacuous-`Type` default. The kinded route additionally supports a bare or
partially applied higher-kinded constructor in a vacuous position, including a
vector mixing it with a closed impredicative `Type` argument. Either direction
of an observable kind mismatch is rejected. Scalar types and complete ordered
vectors are alpha-deduplicated per provider while retaining their first
occurrences.
Association is nominal: a different global with an alpha-equivalent scheme
receives no choice, and neither a scoped value nor a sibling global consults
the supplied map. The caller remains responsible for the source-language fact
which justifies any assertion.

All provider-evidence forms participate only in visible exact-global lookup;
the ordinary implicit use remains first. The scalar runner preserves its established visible
order: ground monomorphic instance-head choices, checked query-derived choices,
then supplied candidates. Query-derived and supplied products keep separate
32-combination caps, so a wide query pool cannot spend the scalar supplied
route's allowance. With either exact assignment runner, the visible order is
ground monomorphic instance-head choices, exact supplied vectors, then checked
query-derived choices. The exact route performs no Cartesian product and does
not require selected binders to be vacuous: because the caller supplied the
complete vector, those binders may occur in the provider body. Both routes open
at most four leading binders, and search plus the independent expression
checker consume their applications through the same checked representation.

`runExferenceQuery` follows the exact empty-evidence path. Calling any explicit
evidence runner with `[]` returns the same batches, candidate order, budget
observations, and diagnostics. Current regressions cover empty compatibility,
lazy outer and inner bounds, productive rejection of cyclic supplied kinds plus
shared finite-tree node-bound coverage, exact arity and locality, non-vacuous
and structural impredicative arguments, higher-kinded and mixed
higher-kinded/impredicative
assignments, vacuous higher-kinded evidence, same-provider kind-vector
consistency, two distinct same-provider vectors at the genuine kind
`(Type -> Type) -> Type`, both directions of kind mismatch, legacy scalar
rejection of a higher-kinded argument, and an ordered four-binder application.
Nonempty evidence remains a bounded global-only capability. It does not enable
scoped-provider donation, invent a polytype, decompose a quantified body in
ordinary unification, or provide general impredicative inference. See the
original
[provider-local candidate report](../docs/reports/2026-08-05-provider-local-instantiation-evidence.md)
and the
[exact provider-assignment report](../docs/reports/2026-08-05-exact-provider-instantiation-assignments.md).

## Experimental features

- Pattern-matching on multi-constructor data types can be enabled via
  `-c` / `--patternMatchMC`, but can reduce performance significantly for
  non-trivial queries. It is intentionally not part of the default search
  policy while that branch of the core algorithm remains expensive.
- Chains of outer (prenex) `forall`s are opened rigidly at the query root. At a
  scoped-value use site, an exposed leading `forall` is first eligible for an
  exact opaque match against a quantified goal, which preserves polymorphic
  forwarding. Empty-binder, empty-context forall wrappers are ignored for
  this classification. If that exact match fails, Exference can also apply
  shallow subsumption when both sides are context-free prenex
  schemes with no free flexible variables. Requested binders stay rigid; only
  provider binders may be solved, and each solution is either a monotype or —
  in the guarded Quick-Look sense — a quantified subtree the requested scheme
  itself already contains. No polytype is ever invented. For
  example, all of these requests can return `\f -> f`:

  ```text
  (forall a. a) -> (forall b. b -> b)
  (forall a b. a -> b -> a) -> (forall x. x -> x -> x)
  (forall a. a -> a) ->
    (forall x. (forall y. y -> y) -> (forall z. z -> z))
  ```

  The last one instantiates the provider binder impredicatively at the
  requested scheme's own quantified atom; the generated code may need
  `ImpredicativeTypes` to compile. Ambient rigid constants are allowed and
  remain nominal. Non-exact schemes
  with contexts and schemes containing free flexible variables stay outside
  this rule.
  In particular, it does not accept directionally invalid specialization
  or a request whose quantified atoms cannot share one binder image:

  ```text
  (forall ignored. Int -> Int) -> (forall x. x -> x)
  (forall a. a -> a) ->
    (forall x. (forall y. y -> y) -> (forall z. z -> x))
  ```

  At a monomorphic (non-quantified) goal, provider binders are instead
  instantiated with fresh flexible variables for that occurrence, their direct
  contexts become proof obligations, and the arrow body participates in
  ordinary search. Thus a value of type
  `forall a. C a => a -> a` can be applied at `Int` when `C Int` is available,
  and a rank-N datatype field can participate after pattern elimination.
  A separate bounded visible branch may select the complete prefix from a
  matching ground instance head. For a context-free provider with no free
  flexible variables whose leading binders are all vacuous, it may instead
  select checked query proper types, including complete closed context-free
  foralls already present below arrows or tuples. The residual provider body
  may mention ambient rigids opened from that query. This works for scoped
  values and retained globals, retains
  at most four binders and 32 query combinations, and can emit
  `provider @(forall a0_0. a0_0 -> a0_0)`. Ordinary fresh instantiation keeps
  priority, and explicit instance-head selection remains monotype-only.
  A nested quantified type exposed as a goal can now be constructed as well,
  including a contextual type whose body needs its local evidence. For example,
  the callback request

  ```text
  ((forall a. a -> a) -> result) -> result
  ```

  can synthesize the quantified identity argument rather than requiring a
  polymorphic provider already in scope. Exact forwarding and context-free
  shallow subsumption retain priority; the structural branch opens the complete
  leading chain with fresh branch-local rigid constants and then searches the
  body. Each layer substitutes the same rigids through its contexts and body.
  The contexts become lexical givens only for that descendant goal, including
  when a layer has no binders. Every deferred class obligation snapshots the
  givens active where it arose; substitutions update both the givens and the
  obligation, and instance prerequisites preserve that snapshot. Resolution
  therefore supports local methods and superclass entailment without allowing
  a nested context to solve an unrelated sibling obligation.

  At every opened layer, flexible variables that already existed are forbidden
  from acquiring its rigids, including indirectly through later flexible
  substitutions. The independent expression checker repeats the dynamic
  allocation, lexical evidence scope, and escape check instead of trusting
  search.
  Search-owned local rigid spellings are matched injectively up to
  alpha-renaming, so its breadth-first goal queue need not agree with the
  checker's tree traversal order; environment, root, and standalone caller
  rigids remain nominal. An unresolved constraint containing a nested skolem
  is rejected rather than published as a top-level obligation. Non-exact
  subsumption between contextual quantified schemes remains unsupported, while
  exhaustion of the finite identifier namespace truncates only the affected
  search branch.
  Quantifiers that have not reached one of these explicit boundaries remain
  alpha-aware `TypeAtom`s. Nested quantified subtrees may still compare exactly,
  but shallow subsumption never recurses into them. These limited rules do not
  provide deep or general higher-rank subsumption, polymorphic let
  generalization, or arbitrary caller-directed visible type application.

  The contextual evidence boundary and its regression matrix are detailed in
  the
  [2026-07-29 contextual rank-N report](../docs/reports/2026-07-29-contextual-rank-n-introduction.md).
  The original context-free rule is recorded in the
  [forall-introduction report](../docs/reports/2026-07-29-exference-forall-introduction.md).

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

## License and provenance

The imported source is Copyright 2014–2017 Lennart Spitzner and is distributed
under the BSD 3-Clause license in [`LICENSE`](LICENSE). Local changes retain
that license. The imported source was cloned from:

<https://github.com/lspitzner/exference>
