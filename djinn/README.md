# Djinn

Djinn generates a Haskell expression from its type. It reads a small Haskell-like
declaration language, translates types through the Curry–Howard correspondence,
and uses a terminating proof search for intuitionistic propositional logic to
construct a proof term. Each candidate is independently type-checked, converted
through a total error-reporting boundary, simplified, and printed as Haskell.

This directory contains the Djinn backend of Djex `2026.7.14`, based on a
reviewed local fork of
[`djinn-2025.2.21`](https://hackage.haskell.org/package/djinn-2025.2.21), based on
the [upstream `augustss/djinn` repository](https://github.com/augustss/djinn).
Djex's date version records the source-breaking checked facade and parser-free
component fold; the original one-line import note has been expanded here
because upstream ships its user guide inside the executable rather than as a
README.

`Djinn.Core.toSynthesisInventory` exposes the validated Djinn environment as
the same structural-and-kind artifact used by Exference sessions. Both now
seal that Inventory together with its exact normalized synonyms in the shared
opaque `PreparedInventory` witness.
Standalone declaration adapters still round-trip historical `KVar` syntax,
while inventory sealing rejects any unsolved kind rather than allowing it into
query elaboration. `HKind` now stores the shared `Kind Int` tree directly
behind bundled compatibility patterns, retaining `HKind(..)` imports and
Djinn's exact `*`/`kN` rendering without a second recursive representation.
This compatibility is deliberately value-level: unlike the former data
constructors, pattern synonyms cannot be promoted as `'KStar` or `'KArrow` by
`DataKinds`. The stable `Djinn.Core` surface has always exposed `HKind`
abstractly; promoted uses of the raw research AST were outside its contract.
Neutral `DjinnEnvironment` sessions preserve their original
sealed declaration order in one authoritative Inventory with Haskell 98
class-kind defaulting, derive every raw backend kind from that Inventory, and
reconstruct the editable neutral environment only for transactional REPL edits.

## Build and run

Djinn is part of the unified `djex` Cabal package and is tested with the
supported GHC 9.12.4 toolchain using Cabal 3.16.1.0. Run these commands from
the repository root
or `djex/`:

```console
cabal build djex:lib:djex djex:lib:djinn-frontend djex:exe:djinn
cabal test djinn-tests djinn-property-tests djinn-frontend-api-tests djinn-cli-tests
cabal run djinn
```

The former top-level `djinn/` package is now the `djex/djinn/` source tree and
has no independent package or project file. Library dependencies migrate to
`djex` or, for the historical REPL API, `djex:djinn-frontend`;
package-generated code imports
`Paths_djex` rather than `Paths_djinn`. The complete component and filesystem
migration is documented in the [Djex guide](../README.md).

Djinn can also execute one or more command files non-interactively:

```console
cabal run djinn -- examples.djinn
```

Each non-comment line in a command file is one REPL command. `--` starts a line
comment. Files are processed from left to right in the same evolving environment.
An invalid command or unreadable nested/startup file does not prevent later
commands from running, but startup-file mode remembers the failure and exits
nonzero. Interactive sessions retain their historical recover-and-continue
behavior and exit successfully.

## Testing

Run the complete test matrix with independently reported test names:

```console
cabal test djinn-tests djinn-property-tests djinn-frontend-api-tests djinn-cli-tests --test-show-details=direct
```

| Suite | Scope |
| --- | --- |
| `djinn-tests` | 60 focused Tasty/HUnit regressions over parsing, declaration token boundaries, kind representation and compatibility, the raw HCheck boundary, class signatures, neutral-environment sealing and cache invalidation, proof search/checking, budgets, rendering, declaration namespaces, built-ins, identifiers, and the `Djinn.Core` facade. |
| `djinn-property-tests` | Four QuickCheck properties, 200 generated cases each (a floor; raise it with `--test-options='--quickcheck-tests=N'`), covering proof production/checking/rendering, arbitrary identity, budgeted-search honesty, and `HType` display/parser round-trips. |
| `djinn-frontend-api-tests` | Three import-boundary checks proving that a dependency on `djinn-frontend` alone exposes `Djinn`, `Djinn.Core`, and `Language.Haskell.Djex.Djinn`. |
| `djinn-cli-tests` | Nineteen subprocess scenarios against the packaged executable, including EOF, explicit RTS tuning, diagnostics, parser recovery and token boundaries, missing startup files, mutation rollback, budget expiry, kind enforcement, atomic instance output, stateful query behavior, argument permutation, and aggregate batch status. |

Each suite can be selected independently, and Tasty patterns can isolate one
named test. For example:

```console
cabal test djinn-property-tests --test-show-details=direct
cabal test djinn-tests --test-options='-p /nominal empty/'
```

The proof/search engine lives in `djinn/src-core` as part of the single
parser-free `djex` library. The public `djinn-frontend` sublibrary is a
separate compatibility/CLI facade that
re-exports both `Djinn.Core` and the checked
`Language.Haskell.Djex.Djinn` adapter; the executable depends only on that
facade. The `djex` library exposes `Djinn.Core` and the formerly public
research modules for import compatibility, while its curated
`Language.Haskell.Djex` module re-exports only the checked adapter. The REPL and
Haskeline remain confined to `djinn-frontend`. HPC coverage is available for
the in-process unit and property suites:

```console
cabal test djinn-tests djinn-property-tests --enable-coverage
```

The CLI suite is intentionally run without HPC: it launches multiple copies of
the instrumented executable, whose shared `.tix` file would conflate processes.

A `tasty-bench` benchmark target measures proof-search performance over a
size-parameterized formula corpus ([`bench/Corpus.hs`](bench/Corpus.hs)) —
first-proof latency, multi-solution enumeration, and decision cost for
non-theorems:

```console
cabal bench djinn-bench --benchmark-options='--stdev 5'
```

## Quick tour

Ask for an implementation by putting `?` between a name and a type:

```text
Djinn> identity ? a -> a
identity :: a -> a
identity a = a

Djinn> swap ? (a, b) -> (b, a)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

Djinn> impossible ? a -> b
-- impossible cannot be realized.
```

The alternative query spelling `? name :: type` is equivalent. Djinn searches
for total, terminating inhabitants: `undefined`, exceptions, and general
recursion are deliberately outside its logic.

The initial type environment contains `()`, `Bool`, `Either`, `Maybe`, `Void`,
and the synonym `Not a = a -> Void`. Tuple and function types are intrinsic;
list syntax is accepted as an abstract type constructor. The initial class
environment contains small `Eq` and `Monad` declarations.

### Add declarations

Function declarations make monomorphic building blocks available to proof
search:

```text
Djinn> type Input :: *
Djinn> type Middle :: *
Djinn> type Output :: *
Djinn> first :: Input -> Middle
Djinn> second :: Middle -> Output
Djinn> pipeline ? Input -> Output
pipeline :: Input -> Output
pipeline a = second (first a)
```

An assumption may use a qualified external name, allowing emitted code to retain
an imported reference such as `Data.Function.id`. Generated query, class, and
method names are deliberately unqualified because they introduce local bindings.
Names follow Haskell lexical rules: leading underscores are accepted, reserved
words and reserved operators are rejected, and operators are written in
parentheses in prefix positions. Function assumptions and class methods share
Haskell's printed value namespace, so Djinn rejects an unqualified collision in
either declaration order. A genuinely qualified assumption such as
`External.select` remains distinct from a method named `select`.

`type T :: kind` declares an abstract type constructor. This is the right way to
introduce an opaque type such as `Int`; `data T` instead declares an empty type.
Ordinary synonyms and finite algebraic data types use familiar syntax:

```text
type Pair a = (a, a)
data Choice a b = This a | That b
data Empty
```

Recursive type declarations are rejected because the logical translation
expands data types structurally.

Unit is deliberately wired in rather than user-declared. The spelling `()` is
part of Djinn's type grammar but is not an ordinary Haskell constructor
identifier, so the public API rejects types, classes, and constructors that try
to claim it. `standardEnvironment` privately installs exactly `data () = ()`,
and that binding cannot be replaced or deleted; start from `emptyEnvironment`
when an environment without the standard unit type is wanted. The unit spelling
remains available normally inside type expressions and constructor fields.

### Type-class contexts

A context is interpreted as an extra collection of available class methods:

```text
Djinn> reflexive ? Eq a => a -> Bool
reflexive :: (Eq a) => a -> Bool
reflexive a = a == a

Djinn> ?instance Monad Maybe
instance Monad Maybe where
   ...
```

This is not Haskell instance resolution. Djinn neither imports the installed
package environment nor instantiates arbitrary polymorphic methods. Classes and
methods needed beyond the small initial environment must be declared explicitly.

At the library boundary, `Context` is the shared backend-neutral
`Constraint HType` value from the shared synthesis modules in `djex`, rather
than Djinn's former raw `(String, [HType])` pair. `mkContext` is the checked
bridge for existing string-based clients:

```haskell
a <- parseHType "a"
eqA <- mkContext "Eq" [a]
report <- inhabit defaultQueryOptions environment [eqA] "reflexive" goal
```

Class lookup, kind checking, and the interpretation of methods as search
premises remain deliberately Djinn-specific.

Class parameter kinds are inferred from the method types when a class is
declared (defaulting to `*`, so `Monad`'s parameter is `* -> *` while a
method-less class parameter is `*`), and every context or `?instance`
argument is checked against that signature:

```text
Djinn> ?instance Monad Bool
Error: argument Bool of class Monad: kind mismatch: * vs * -> *
```

Each `?instance` head and all of its prerequisite constraints are checked in
one kind-variable scope, so a shared variable cannot silently acquire
different kinds in different parts of the generated instance signature.
For kind inference and class-argument substitution, method-local variables
have per-signature scope: identical spellings in sibling methods do not share a
kind, and instantiating a class parameter alpha-renames a colliding local.
After resolution, Djinn's intentionally shallow premise model still uses the
resulting string-named type atoms without fresh polymorphic instantiation.
Djinn also searches every method before printing the header: an unrealizable
method produces diagnostics without leaving a partial, non-compiling instance
block in the output.

## Worked examples

Djinn is strongest exactly where hand-writing plumbing is most error-prone:
continuation- and state-passing code. The classic demonstration derives the
`Monad` operations, and `callCC`, for a continuation type:

```text
Djinn> data CD r a = CD ((a -> r) -> r)
Djinn> returnCD ? a -> CD r a
returnCD :: a -> CD r a
returnCD a = CD (\ b -> b a)

Djinn> bindCD ? CD r a -> (a -> CD r b) -> CD r b
bindCD :: CD r a -> (a -> CD r b) -> CD r b
bindCD a b =
      case a of
      CD c -> CD (\ d ->
                  c (\ e ->
                     case b e of
                     CD f -> f d))

Djinn> callCCD ? ((a -> CD r b) -> CD r a) -> CD r a
callCCD :: ((a -> CD r b) -> CD r a) -> CD r a
callCCD a =
       CD (\ b ->
           case a (\ c -> CD (\ _ -> b c)) of
           CD d -> d b)
```

The state monad works the same way through a synonym:

```text
Djinn> type S s a = s -> (a, s)
Djinn> bindS ? S s a -> (a -> S s b) -> S s b
bindS :: S s a -> (a -> S s b) -> S s b
bindS a b c =
     case a c of
     (d, e) -> b d e
```

Because Djinn implements intuitionistic propositional logic, it can also
answer small logical questions. The double negation of the law of excluded
middle is provable; the law itself is not:

```text
Djinn> f ? Not (Not (Either x (Not x)))
f :: Not (Not (Either x (Not x)))
f a = case a (Right (\b -> a (Left b))) of {}

Djinn> g ? Either x (Not x)
-- g cannot be realized.
```

When a type has several inhabitants, `:set +multi` prints de-duplicated
alternatives, ranked (under the default `+sorted`) so that solutions using
more of their arguments come first:

```text
Djinn> :set +multi
Djinn> k ? a -> a -> a
k :: a -> a -> a
k _ a = a
-- or
k a _ = a
```

## Command reference

Commands may be abbreviated to an unambiguous prefix.

| Command | Effect |
| --- | --- |
| `name ? [context =>] type` | Search for an inhabitant. |
| `? name :: [context =>] type` | Equivalent query syntax. |
| `name :: type` | Add a monomorphic axiom/function. |
| `type T vars = type` | Add or replace a type synonym. |
| `type T :: kind` | Add or replace an abstract type constructor. |
| `data T vars = C types | ...` | Add or replace a finite data type. |
| `data T vars` | Add an empty data type. |
| `class C vars where methods` | Add or replace a class declaration. |
| `?instance [context =>] C types` | Generate every method of an instance. |
| `:environment` | Print types, axioms, and classes in scope. |
| `:delete name` | Delete a declaration from the environment. |
| `:clear` | Restore the initial environment and settings. |
| `:load file` | Execute a command file. |
| `:set option` | Change a search/output option. |
| `:help` / `:verbose-help` | Print short or extended help. |
| `:quit` | Exit. |

Methods in a `class` declaration are separated with semicolons. Kinds use `*`
and right-associative `->`, for example `type F :: * -> *`.

Type replacement and deletion are transactional. Djinn rebuilds inferred kinds
and revalidates every remaining synonym, axiom, and class method; a mutation that
would leave a dangling or ill-kinded dependency is rejected without changing the
environment. Deleting a name that was never defined is reported as an error, and
qualified axiom names can be deleted with the same spelling used to add them.

### Settings

The Boolean settings use `+name` to enable and `-name` to disable them:

| Setting | Default | Meaning |
| --- | ---: | --- |
| `multi` | off | Print alternative, de-duplicated solutions. |
| `sorted` | on | Rank by the fraction of unused binders, then binder count. |
| `debug` | off | Print the translated formula and internal proof term. |
| `cutoff=N` | `200` | Consider at most positive `N` proof candidates. |
| `budget=N` | `0` | Explore at most `N` proof-search steps; `0` is unlimited. |

With the default unlimited budget, proof search is a decision procedure:
"cannot be realized" is a proof of uninhabitedness. A positive budget bounds
the work instead; if it expires before any proof is found, Djinn reports
`no proof found within budget N; inhabitation is undecided` rather than
claiming unprovability. Any follow-up search used solely to refine a
self-reference diagnostic receives only the first search's unspent fuel, so
the setting remains a total per-query bound rather than a per-pass allowance.

For example:

```text
:set +multi
:set cutoff=20
```

The same Boolean options may appear before or after file names, such as
`cabal run djinn -- examples.djinn +multi`. Exact option names win over unique
prefixes; unknown or ambiguous prefixes fail. A standalone `--` ends option
scanning when a file name itself begins with `+` or `-`.

## How the code is organized

| Module | Responsibility |
| --- | --- |
| `app/Main.hs` | Thin executable launcher. |
| `Djinn.Core` | The stable, validated library API (see below). |
| `Language.Haskell.Djex.Djinn` | Opaque checked session and shared query/evidence/search adapter. |
| `Djinn.Internal.Declaration` | Djinn declaration compatibility values and checked shared-IR lowering. |
| `Djinn` (`src-frontend/Djinn.hs`) | CLI frontend: settings, command parser, and printing, built on `Djinn.Core`. |
| `Djinn.Internal.REPL` | Haskeline loop and EOF handling. |
| `Djinn.Internal.HCheck` | Pre-cache five-operation raw compatibility facade over shared kind inference. |
| `Djinn.Internal.HCheck.Implementation` | Private raw-compatibility kind-check cache and trusted Inventory-assumption bridge. |
| `Djinn.Internal.Environment` | Authoritative Inventory preparation plus native shared-type checking and private class, synonym, formula, and ordered global-premise indexes. |
| `Djinn.Internal.HIdentifier` | String-compatible parser adapter over the validated shared name and operator rules in `djex`. |
| `Djinn.Internal.HTypes` | Shared-kind compatibility view, type parser, logical translation, and proof-term conversion/cleanup. |
| `Djinn.Internal.Type` | Native shared-type validation/canonicalization plus checked projection to and from Djinn's historical source types. |
| `Djinn.Internal.Generated` | Djinn's Haskell-shaped cleanup tree and its adapter to the shared generated-code AST. |
| `Language.Haskell.Synthesis.Generated` | Shared local/global output tree, scope validation, capture-safe naming, qualification, and Haskell rendering. |
| `Language.Haskell.Synthesis.KindInference` | Shared kind unification, class-parameter inference, and acyclic declaration dependency ordering. |
| `Djinn.Internal.LJTFormula` | Formula and proof-term data types. |
| `Djinn.Internal.LJT` | Dyckhoff-style contraction-free proof search and proof normalization. |
| `Djinn.Internal.ProofEnv` | Isolation of external proof identities from printable names. |
| `Djinn.Internal.ProofCheck` | Independent type checking of generated proof terms. |
| `Djinn.Internal.Help` | Extended in-program help. |

`Djinn.Core`, `Language.Haskell.Djex.Djinn`, and the proof, type, environment,
and validation modules under `src-core/` compile into the parser-free `djex`
library. The checked declaration and type adapters are exported by
`Djinn.Core`; their `Djinn.Internal.Declaration` and `Djinn.Internal.Type`
implementation modules remain private. `Djinn`,
`Djinn.Internal.Help`, and `Djinn.Internal.REPL` live under `src-frontend/` in the
named `djinn-frontend` component; Help and REPL are private implementation
modules, while the component re-exports both stable core entry modules. The
executable's `app/` source root contains only its launcher, so
neither the frontend nor executable can accidentally compile core modules as
home modules.

The exposed `Djinn.Internal.HCheck` deliberately retains only the five raw
operations from Djinn's pre-cache checker surface. Its sealed compatibility
cache pairs legacy synonym spellings/arities with kind assumptions derived
from one exact Inventory, so its constructor and operations live in the
package-private `Djinn.Internal.HCheck.Implementation` module. Native shared
types instead preflight saturation against the Inventory's opaque shared
synonym table and use its exact kind assumptions directly; neither path can
assemble unrelated halves or depend on query-time cache structure.

The REPL state stores only an opaque `DjinnSession`; every declaration or
deletion produces and validates a fully resealed replacement before installing
it. Queries and instance methods go through `runDjinnQuery`, consume
shared logical evidence and operational completion independently, and render
the `FunctionClause` output of returned shared `Candidate`s. This preserves the
declaration language while eliminating the frontend's former direct
`inhabit`/`QueryReport` path.

## Using Djinn through the `djex` library

`Djinn.Core` is the supported programmatic interface. It keeps invalid data
unrepresentable: environments are opaque and only constructible through
validating operations, every declaration is name-checked, kind-checked, and
revalidated transactionally, and its primary query API returns checked shared
candidates while keeping logical evidence separate from search completion:

```haskell
import Djinn.Core
import Language.Haskell.Synthesis.Query
    (QueryEvidence(..), resultEvidence, resultSearch)
import Language.Haskell.Synthesis.Search (batchCandidates)
import Language.Haskell.Synthesis.Generated (mkDefinitionName)
import Language.Haskell.Synthesis.Name (mkIdentifier)

demo :: Either String [DjinnCandidate]
demo = do
    first  <- parseHType "Input -> Middle"
    second <- parseHType "Middle -> Output"
    goal   <- parseHType "Input -> Output"
    env <- pure standardEnvironment
       >>= declare (AbstractType "Input" kStar)
       >>= declare (AbstractType "Middle" kStar)
       >>= declare (AbstractType "Output" kStar)
       >>= declare (Function "first" first)
       >>= declare (Function "second" second)
    targetName <- either (Left . show) Right $ mkIdentifier "pipeline"
    target <- either (Left . show) Right $ mkDefinitionName targetName
    result <- either (Left . show) Right $
        inhabitResult defaultQueryOptions env [] target goal
    case resultEvidence result of
        ValidatedCandidates -> Right
            (batchCandidates $ resultSearch result)
        ProvedUninhabitable -> Left "no total inhabitant exists"
        RequiresTargetReference -> Left "only via recursion"
        NoEvidence -> Left "the search limit established no conclusion"
```

This example needs only the unnamed `djex` library. The same dependency exposes
the shared vocabulary, checked adapter, `Djinn.Core`, and the formerly public
raw research modules; `Language.Haskell.Djex` remains the smaller curated
import surface.

The essentials: `declare`/`removeDeclaration` grow and shrink an
`Environment` (starting from `emptyEnvironment` or `standardEnvironment`);
`parseHType`/`parseContextualHType`/`parseHKind` parse with full-input
validation; the contextual entry point owns the REPL's optional constraint
grammar so checked clients need not import its internal `ReadP` parser.
Raw `HType` query entry points retain Djinn's historical class lookup, arity,
kind, and synonym-saturation diagnostic preflight, project the validated
ordinary types into the shared IR, and delegate to the native query worker.
`inhabitResult` then runs formula translation, budgeted proof search,
independent proof checking, and constructs the shared `QueryResult` directly
without choosing a renderer or passing through a backend-owned report envelope.
The historical `inhabit` entry point is its explicit rendered-string
compatibility wrapper. Both report the formula and first proof term for
debugging. Their completion uses the shared operational vocabulary:
`Finished` means the configured proof exploration completed, while
`Truncated ChoicePointLimitReached` explains `Undecided`, and
`Truncated CandidateLimitReached` says that another proof was observed beyond
`optionCutoff`. The latter inspects only that one overflow witness instead of
forcing the remaining proof stream. This status remains separate from Djinn's
proof-backed `Unrealizable` outcomes. `resolveContext`
instantiates one class context;
`resolveInstanceMethods` jointly checks an instance target and all of its
prerequisites before returning the target's instantiated methods. A query's
goal and every class argument are likewise kind-checked together, so a free
type variable has one kind throughout the complete signature. Public query
budgets must be non-negative; `Nothing` is unlimited and `Just 0` expires at
the first choice point. `toSynthesisType` and `fromSynthesisType` expose the
lossless ordinary-type subset of `HType`; they explicitly reject declaration
bodies, shared foralls, and unboxed tuples instead of conflating those layers.
`toSynthesisDeclaration` and `fromSynthesisDeclaration` likewise round-trip
Djinn's synonyms, data/abstract types, classes, and assumptions while rejecting
shared superclass and instance semantics that Djinn does not implement.
The opaque Djinn `Environment` itself now round-trips through
`Language.Haskell.Synthesis.Environment`; reverse lowering preflights Djinn's
stricter source subset, grounds and checks the neutral Inventory once, validates
synonym saturation in every type-bearing declaration position, and classifies
recursive datatypes only after aliases have been expanded. The resulting
raw compatibility checker, synonym table, nominal class index, checked formula
translator, and ordered global proof premises are projections of that same
Inventory rather than a second independently inferred environment. Native
saturation is a `TypeSynonym` table operation; raw `HType` retains one separate
source-order traversal solely to preserve its malformed-input diagnostics.
Global assumptions are translated once while sealing; only a query goal and
its instantiated class methods still vary per search. Historical raw search
tables are reconstructed from the Inventory only for compatibility inspection,
not retained in `PreparedEnvironment`. Raw
`prepareEnvironment` applies that same expand-first recursion preflight, so a
constructor-forged compatibility environment cannot bypass it while phantom
aliases retain their alias-free meaning.
Canonical `DjinnResult`s expose candidates through
`batchCandidates . resultSearch`; every Djinn candidate has empty residual
constraints and retains the unused-binder fraction and exact
arbitrary-precision binder count used by the historical ranking policy.
`GeneratedQueryReport` and `QueryReport` remain compatibility projections of
that result, with the latter retaining `reportGeneratedClauses` and legacy
rendered strings.
The default `djex` library additionally exposes
`Language.Haskell.Djex.Djinn`: `mkDjinnSession` lowers a neutral
`DjinnEnvironment` and retains the exact shared `DjinnInventory` that validated
its private proof indexes together with the exact prepared synonym table. The
session no longer retains a second editable `DjinnEnvironment`; the grounded
Inventory is weakened losslessly when an edit begins, then the replacement is
fully resealed before publication. The raw `Djinn.Core.Environment` therefore
remains confined to compatibility inputs and the REPL parser;
`standardDjinnSession` converts the checked built-in spelling once and then
uses the same neutral `mkDjinnSession` path as caller-supplied environments,
`parseDjinnRequest` shares the REPL's optional class-context grammar, and
`mkDjinnRequest` seals a `QueryRequest DjinnType QueryOptions` into the opaque
`DjinnRequest` consumed by `runDjinnQuery`. Existing programmatic clients
should replace direct construction or record updates of `DjinnRequest` with a
neutral `QueryRequest` followed by `mkDjinnRequest`, and use
`djinnRequestQuery` when they need to inspect that original value. Sealing
receives the request's already checked shared `DefinitionName`, checks Djinn's
narrower class-name namespace, retains that target in the original
`QueryRequest`, and caches only a canonical shared goal and shared context
arguments. The raw-`Name` parser helper constructs
that checked target before parsing so target diagnostics retain precedence;
search-option validation and all environment-dependent class and kind checks
still occur when the request is run. One request can therefore elaborate the
same alias spelling against different compatible sessions without capturing
the first session's meaning. The shared `CachedQuery` owns a strict
programmatic-or-source `RequestProvenance` independently of its private plan;
source-derived constructor and query failures receive its complete input span,
while options and internal proof/result invariants remain unlocated. Invalid
cutoffs and budgets are typed `DjinnQueryOptionsError` values and map to
`DJEX_DJINN_OPTIONS`; the historical `Either String` facade preserves its exact
messages. Here
`DjinnType` is the shared `Type DjinnTypeVariable` source representation;
`DjinnTypeVariable` and the generated-binder `DjinnLocal` remain distinct API
names despite both currently being represented by `String`. Parsed raw types
are checked into `DjinnType`; stable requests retain that shared representation
for the goal, constraints, and instantiated class methods throughout kind
checking and synonym elaboration. Only the resulting alias-free goal and method
types are projected to raw `HType`, immediately at the legacy formula compiler
boundary rather than as request or session state.
Its checked `QueryResult` carries the same shared `Candidate DjinnType`
structure as Exference plus Djinn's
formula/proof metadata; even the currently empty residual constraints no
longer expose a backend type. The Core constructs this `QueryResult` directly,
and its checked constructor rejects any mismatch between logical evidence and
the candidate payload before either can escape. The definition/expression
renderers consume
canonical candidates through the shared rendering pipeline and return
its `RenderError` directly, without conflating logical evidence with
operational completion.
The raw proof/search `Djinn.Internal.*` modules used by compatibility tests and
research tooling remain exposed by `djex`, but their constructors can
violate these invariants and carry no stability promise. The checked
declaration/type and prepared-kind-check implementation modules, plus the
frontend-only Help and REPL modules, are deliberately not exposed. Raw formula
translation is nevertheless
total as a checked operation over finite inputs: `prepareTypeFormulaTranslator`
checks the complete first-binding definition expansion graph once and returns a
reusable `HType -> Either String Formula` translator. The graph conservatively
counts every nominal definition reference, so even malformed inert recursive
tables whose references are under- or over-saturated are rejected. Each
translation also tracks active constructor occurrences, because a higher-order
source such as `S S` for `S f = f f` can create a cycle absent from the
definition graph. Query-tree identities survive substitution and duplication;
cached definition-body identities are instantiated beneath the concrete head
occurrence that expanded them. Distinct finite occurrences and definition
instances therefore remain distinct. Re-entering one still-active occurrence
is rejected conservatively: checked, well-kinded sessions cannot encode such
untyped self-application, while arbitrary raw `Djinn.Internal` inputs otherwise
make exact normalization undecidable. The returned closure consequently fails
deterministically on a source-created recursive synonym/type-definition
expansion; `hTypeToFormula`
is the one-shot checked wrapper. Checked Djinn sessions retain that compiled
closure and every ordered global premise in `PreparedEnvironment`, so later
queries perform only their source-local goal and instantiated-method expansion
checks rather than repeating whole-table analysis or translating unchanged
function assumptions.

The central pipelines converge before environment-dependent validation:

```text
REPL command -> HType -> checked compatibility projection --\
neutral request -> Type String -> canonical shared plan -----+-> shared kind check
    -> shared synonym elaboration + class-method instantiation
    -> alias-free HType at formula boundary -> Formula -> LJT proof search
    -> proof-term normalization -> independent proof check
    -> Haskell AST cleanup -> shared Candidate + Generated scope check/renderer
    -> printed clause
```

## How proof search works

Djinn translates the query type into a formula of intuitionistic propositional
logic: tuples become conjunctions, `data` alternatives become disjunctions
tagged with their constructors, functions become implications, empty data
types become nominally tagged falsehoods, and everything else (opaque
constructors, variables, list types) becomes a propositional atom. By the
Curry–Howard correspondence, a constructive proof of that formula *is* a
program of the original type.

The prover in `LJT.hs` is Roy Dyckhoff's contraction-free sequent calculus
LJT ("Contraction-free sequent calculi for intuitionistic logic", JSL 1992),
translated from Dyckhoff's 1991 Prolog implementation. Its key property is
termination without loop checking: the usual troublesome rule for a nested
implication `(a -> b) -> c` on the left is replaced by rules that always
reduce a well-founded measure, so the search is a decision procedure — when
Djinn says a type cannot be realized, that is a proof of uninhabitedness (in
the total, propositional model), not a timeout.

Operationally, `redant` classifies antecedents into four groups — unprocessed
formulas, implications indexed by their atomic premise (`AtomImps`), nested
implications (`NestImps`), and bare atoms — and `redsucc` then reduces the
goal. Nested implications are the branching point of the search; everything
else is deterministic reduction. The search runs in a small backtracking
monad `P` producing a lazy stream with explicit choice-point markers, which
is what the `budget` setting counts; with `+multi` the local cuts are
disabled so alternative proofs stream out lazily. The `LJT` module also
exposes `proveWithMode` with a named `SearchMode` (alternatives, depth-first
or interleaved strategy, optional budget) for programmatic use; the CLI uses
the classical depth-first order.

Each selected proof term is normalized (`nf`), checked against the requested
formula by an independent unification-based type checker (`ProofCheck`),
alpha-renamed so every binder is globally unique, converted to a small
Haskell AST, cleaned up (case collapsing, unused-binder elision, eta
reduction, nicer names), and erased into the shared generated-code tree. The
shared boundary revalidates lexical scope, distinguishes locals from structural
global names, reserves emitted globals during local-name allocation, and
pretty-prints the final clause.

Two invariants make the back half of that pipeline safe, and both are worth
knowing before editing the source:

- **Global freshness.** `Symbol` is shared by proof variables and
  propositional atoms. Before searching, the prover reserves every symbol
  occurring in the environment and the goal (`formulaSymbols`), and every
  generated name is recorded in the search state. Assumptions are additionally
  given fresh internal identities (`ProofEnv`) so a query can never capture,
  shadow, or recursively reference a caller-supplied name. Target-named
  assumptions are retained only in a non-renderable diagnostic environment:
  Djinn reports "without a recursive self-reference" only after finding and
  independently checking a proof that actually needs that enlarged environment.
- **Unique binders.** The Haskell-AST simplifiers assume that no two binders
  share a name and that binders are disjoint from free variables. The
  converter enforces this with an explicit alpha-renaming pass rather than
  trusting term producers; the shared output scope checker and allocator then
  enforce the same invariant at the renderer boundary.

## Important limitations

- Djinn implements propositional intuitionistic reasoning, not the full Haskell
  type system. It has no higher-rank types, GADTs, type families, constraints
  imported from packages, or general type-class solver.
- Added functions are used at exactly their declared type; their polymorphic
  type variables are not freshly instantiated at each use.
- Type synonyms must be fully saturated, matching Haskell. Data and abstract
  constructors may still be used partially in higher-kinded positions.
- Genuinely recursive data types cannot be declared structurally. Recursion is
  classified after synonym expansion, so a phantom alias can erase an apparent
  surface cycle; list types remain opaque rather than being expanded into `[]`
  and `(:)`.
- Empty data types are logically false but retain nominal tags. Identity is used
  only for the same empty type; conversion to another result uses explicit empty
  elimination.
- The simplifier assumes total semantics. Reordering or eliminating pattern
  matches need not preserve Haskell's behavior in the presence of bottoms or
  `seq`.
- Empty-type elimination is printed structurally as `case value of {}`. This
  avoids an implicit helper name that could capture or be captured by a user
  declaration; generated code using it requires GHC's `EmptyCase` extension.
- Proof search is a decision procedure, but the search space and the number of
  inhabitants can grow rapidly. Keep `cutoff` modest when requesting multiple
  or sorted solutions, and consider `:set budget=N` as a safety net for
  queries that may explode; an expired budget is reported as undecided.
- Proof terms are independently checked, but generated clauses are not passed
  through GHC automatically. Treat the output as a strong candidate that still
  belongs in the normal compile/test loop.

See [`docs/reports/`](docs/reports/) for the local review history:
[the correctness review](docs/reports/2026-07-10-code-review.md) covering the
import's fixed defects and remaining engineering risks,
[the simplification pass](docs/reports/2026-07-10-simplification-pass.md)
covering deduplication, readability work, and the empty-goal completeness
fix, [the search-mode and budget work](docs/reports/2026-07-10-search-budget.md)
with its benchmark-driven engine decisions,
[the class parameter kind enforcement](docs/reports/2026-07-10-class-kinds.md),
[the library API hardening](docs/reports/2026-07-10-library-api.md), and
[the shared constraint and method-scope migration](docs/reports/2026-07-11-shared-constraint-contexts.md).

## License and provenance

The imported source is Copyright 2005–2014 Lennart Augustsson and is distributed
under the BSD 3-Clause license in [`LICENSE`](LICENSE). Local changes retain that
license. The imported version was downloaded from:

<https://hackage.haskell.org/package/djinn-2025.2.21/djinn-2025.2.21.tar.gz>
