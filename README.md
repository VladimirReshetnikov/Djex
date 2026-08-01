# Djex

Djex is a Haskell expression synthesizer formed by merging
[Djinn](https://github.com/augustss/djinn) and
[Exference](https://github.com/lspitzner/exference). Given a type, it
generates a Haskell expression of that type. Djinn contributes a complete
intuitionistic prover built on Dyckhoff's LJT calculus, so it terminates and
can prove a type uninhabited; Exference contributes a ranked heuristic search
engine with type-class evidence resolution and explicit resource controls.
Both engines carry class obligations as the same shared
`Constraint (Type variable)` structure. Exference resolves givens,
superclasses, and explicit instances; Djinn validates the class, arity, and
kinds of a context but proves only inhabitants that do not need a class
method. Exference's nominal instance resolution terminates for accepted rules,
but its broader expression search is not an inhabitation decision procedure.
Both engines, their compatibility frontends, and a shared parser-independent
synthesis foundation compile into one Cabal package with a single library,
version, and dependency contract.

## Start here

- To try the commands, continue with [Building](#building) and the
  [unified-command guide](#unified-command), then see the complete
  [shared REPL guide](docs/repl.md).
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
  declaration edits and instance generation stay in its compatibility
  frontend. All
  modules formerly exposed by the three parser-free sublibraries remain
  exposed for import compatibility.
- `synthesis/` is the neutral foundation: validated names, types, kinds,
  declarations, environments, diagnostics, collision-free allocation,
  generated output, and operational search status.
- `djinn/` contributes the LJT proof engine, checked adapter, historical
  `Djinn` API, and compatibility Haskeline REPL.
- `exference/` contributes the heuristic search engine, checked adapter,
  Haskell-source/environment loader, and historical CLI API.

The `djex` executable is the merged frontend. With no arguments (or the
`repl` subcommand) it starts one persistent session that can query Djinn,
Exference, or both; the `djinn` and `exference` subcommands retain explicit
one-shot operation. The historical `djinn` and `exference` executable names
remain available for their distinct compatibility contracts. The single
library deliberately trades Haskeline/HSE dependency
isolation for one dependency and version contract; parser-independent module
boundaries remain visible in the source graph. Integration, backend,
property, CLI, API, and benchmark suites preserve differential testing while
the two engines continue converging. Exference's finite recursive-pattern rule
is recorded in the
[2026-07-31 bounded recursive elimination report](docs/reports/2026-07-31-bounded-recursive-elimination.md).
Djinn's widened bounded hypothesis-instantiation rule is recorded in the
[2026-08-01 four-binder instantiation report](docs/reports/2026-08-01-four-binder-instantiation.md).
Djinn's complementary nominal view of reachable parameterized datatypes is
recorded in the
[2026-08-01 nominal parametric-data transport report](docs/reports/2026-08-01-nominal-parametric-data-transport.md).
Djinn's cubic extension of its bounded rank-N plan family is recorded in the
[2026-08-01 triple rank-N frontiers report](docs/reports/2026-08-01-triple-rank-n-frontiers.md),
following the
[2026-07-31 pairwise rank-N frontiers report](docs/reports/2026-07-31-pairwise-rank-n-frontiers.md).
Contextual goal introduction and its
lexical evidence boundary are recorded in the
[2026-07-29 contextual rank-N report](docs/reports/2026-07-29-contextual-rank-n-introduction.md).
The bounded generated-term and Exference provider-use extension is recorded in
the
[2026-07-29 visible type application report](docs/reports/2026-07-29-visible-type-application.md).
The preceding context-free Exference rule and Djinn quantified-wrapper
follow-up are recorded in the
[2026-07-29 forall-introduction report](docs/reports/2026-07-29-exference-forall-introduction.md).
Earlier work is recorded in the
[hypothesis-instantiation report](docs/reports/2026-07-29-hypothesis-instantiation.md),
[rank-N inference review](docs/reports/2026-07-28-rank-n-inference-review.md),
[2026-07-27 source-semantics follow-up](docs/reports/2026-07-27-source-semantics-follow-up.md),
and
[unification review](docs/reports/2026-07-27-unification-review.md).
The earlier [post-merge code review](docs/reports/2026-07-21-post-merge-code-review.md),
[final convergence review](docs/reports/2026-07-17-final-convergence-review.md),
and [checker-boundary follow-up](docs/reports/2026-07-17-checker-boundary-follow-up.md)
record the larger strictness, compatibility, and raw-checker migrations that
preceded it.

## Building

Build and test the complete graph from the repository root:

```console
cabal build all
cabal test all -j1 --test-show-details=direct
```

The whole-tree test run is deliberately serial. Several CLI suites invoke the
freshly linked `djex`, `djinn`, and `exference` tools as subprocesses; serial
scheduling prevents a concurrent component rebuild from replacing an
executable while a suite is consuming it. Focused library-only suites do not
need this restriction.

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
cabal test all -j1 --prefer-oldest --builddir=dist-newstyle-oldest --test-show-details=direct
```

Useful component and compatibility-executable targets:

```console
cabal build djex:lib:djex
cabal run exe:djex
cabal run exe:djex -- repl --backend both
cabal run exe:djex -- djinn --render expression "a -> a"
cabal run exe:djex -- exference --select first "a -> a"
cabal run exe:djex -- download CABAL_TARGET
cabal run exe:djex -- install [--lib] CABAL_TARGET
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

Start the shared interactive session with either equivalent form:

```console
djex
djex repl [--backend djinn|exference|both] [--environment DIR] [--fix] [--history FILE] [--ignore-startup]
```

The default prompt is `djex[djinn]>`. Entering a bare type synthesizes with
the active backend selection; `:backend` changes that selection, while
`:djinn TYPE`, `:exference TYPE`, and `:compare TYPE` select a backend for one
query. Both-mode labels each engine's independent output and still runs the
other engine if one rejects the checked type or fails. With a loaded source
workspace, Djex parses, resolves, expands, and kind-checks the query once, then
projects that shared type structurally into both engines; backend search and
evidence semantics remain independent. This resembles GHCi's colon commands,
startup files, history, completion, and `:{`/`:}` input. Bare input is
deliberately still a requested result type, not a Haskell expression; the
explicit `:eval` command is the separate boundary that compiles and executes
an expression with real GHC.

Rank-N support now uses deliberately bounded, backend-specific rule families.
Djinn can introduce a `forall`, including one with an already validated class
context, in a positive position: arrow results, products, and datatype fields
preserve that position, while each arrow parameter reverses it. As at the query
root, the context contributes no proof premises, so the result must remain
dictionary-independent. Djinn can also eliminate a hypothesis-side
context-free `forall` of up to four leading binders by instantiating its
complete chain at sequent-supplied candidates: the goal's type variables, the
skolems of opened positive occurrences, premise-scope variables, and — as a
guarded form of impredicativity — any query-supplied subtree that is
independent of enclosing binders and contains quantification, including a
structural wrapper around a quantified atom. Exference can introduce a nested
`forall`, with or without class contexts, once ordinary search exposes it as a
goal, for example as a callback argument or an arrow result. It opens the
complete leading chain with branch-local fresh rigid constants and treats each
layer's substituted context as lexical givens for that body only. Every
deferred obligation retains the givens under which it arose, so superclass and
instance solving can use them without letting evidence leak to a sibling goal.
Exference synthesizes the body and rejects any direct or indirect substitution
that would let a rigid escape through a flexible variable from an older scope.
Generated local skolem spellings compare up to a scope-owned alpha-renaming,
but environment and root constants remain nominal; unresolved constraints
containing a nested skolem cannot escape as result obligations. Exference can
also eliminate the complete leading `forall` chain of a scoped value at a
monomorphic use site, freshly and
independently for each occurrence; direct contexts become ordinary proof
obligations.
For a constrained scoped provider, a separate evidence-directed branch can
make that instantiation visible when an explicit ground instance head fixes
its complete leading binder prefix. It emits closed applications such as
`provider @Int`; every specified argument is a variable-free, `forall`-free
monotype. The ordinary implicit branch remains available, and global bindings
retain their existing implicit behavior. The shared generated-expression tree
also represents the inferred argument `@_`, although Exference search itself
emits only specified ground arguments from this rule.
Exference can also forward a context-free quantified provider with no free
flexible variables to a less-general such goal; provider binders are solved
with monotypes or, in the guarded Quick-Look sense, with quantified subtrees
the requested scheme itself supplies. This covers, for example:

```text
:djinn c -> (forall a. a -> a)
:djinn c -> (forall a. Eq a => a -> a)
:djinn ((forall a. a -> a) -> c) -> c
:djinn (forall a. a -> a) -> b -> b
:djinn (forall a. a -> Maybe a) -> (forall b. b -> b) -> Maybe (forall b. b -> b)
:djinn (forall a. f a) -> f (Maybe (forall b. b -> b))
:exference ((forall a. a -> a) -> result) -> result
:exference (forall a. a -> a) -> Int -> Int
:exference (forall a b. a -> b -> a) -> (forall x. x -> x -> x)
```

Declared datatypes normally remain structural in Djinn: their constructor
sums support introduction and case elimination and stay first in search. A
query-directed backward slice additionally identifies reachable applications
of datatypes with at least one parameter. When the slice reaches one, Djinn can
run a complementary formula view that retains parameterized datatype
applications as alpha-aware nominal atoms. With these declarations loaded,

```haskell
data D a = EmptyD | FullD a
data R = R
data Token = Token

finish :: D (forall b. b -> b) -> R
token :: Token
poly :: Token -> (forall a. D a)
```

the complementary view covers direct transport, composition through a loaded
consumer, and a closed global provider/consumer chain:

```text
(forall a. D a) -> D (forall b. b -> b)  -- \x -> x
(forall a. D a) -> R                     -- \x -> finish x
R                                        -- finish (poly token)
```

Synonyms are expanded before the slice is computed, so an alias of `D` is
transparent. A nullary datatype keeps only its historical structural
constructor view. Unrelated parameterized declarations do not activate or
reshape a query's plan family.

The slice can look through positive function results, tuple elements, and
fields of an aggregate value that is actually available. Datatype fields are
specialized at that value's concrete owner arguments; a declaration such as
`data Box a = Box a` creates no reachability edge by itself. Function
parameters introduced while constructing the query result are treated as
query-local providers, so the same projection works for a goal such as
`Holder -> R`. A per-path datatype-head guard keeps nested and future recursive
field projection finite.

The full historical structural no-axiom prefix runs in its established order
before the focused nominal work. Each nominal formula is tried both plainly
and, when available, with its separately compiled bounded instantiation
axioms. These plans consume the same global candidate cutoff and choice-point
fuel as every structural plan. They are proof-producing, positive-only
approximations: failure of a nominal plan never establishes uninhabitability.

Instantiation evidence is erased only after independent proof checking. Every
proof that consumes it uses conservative no-eta conversion. It retains the
lambda when erasure would expose a higher-rank application boundary under
GHC's simplified subsumption rules. Consequently, the second
example remains `\x -> finish x`, rather than being eta-contracted to
`finish`. The same protection applies when presentation turns structural
record elimination into a selector projection. Such signatures and generated
terms may require both `RankNTypes` and `ImpredicativeTypes`.

Every quantified subtree outside those explicit boundaries remains an opaque
atom. Alpha-renamed binders compare by lexical scope and declaration position,
while free variables remain significant. Ordinary structure outside the atom
is retained, including impredicative applications such as lists of Church
booleans:

```text
:compare forall item. (forall result. (item -> result -> result) -> result -> result) -> (forall answer. (item -> answer -> answer) -> answer -> answer)
:compare [(forall result. result -> result -> result)] -> [(forall answer. answer -> answer -> answer)]
```

This is not general higher-rank subsumption, polymorphic-let generalization, or
general visible type application. In particular, open arguments such as `@a`
and impredicative type arguments remain unsupported. Djinn search does not gain
the Exference rule, and its historical expression projection rejects the new
generated node explicitly. Unsupported Djinn positions remain opaque and make
an otherwise empty search inconclusive rather than manufacturing a logical
refutation. Exference still does not perform non-exact subsumption between
contextual schemes; quantified types outside an exposed goal/provider boundary
remain opaque. Finite identifier or search-budget exhaustion is truncation, not
negative evidence.
Djinn searches a cubically bounded family. Its historical prefix remains
the fully opened polarized plan, the exact-opaque plan, and the two singleton
frontiers: one positive `forall` stays opaque while its siblings open, or one
occurrence opens while unrelated siblings remain opaque. A deterministic tail
then makes the same choices for each unordered pair and triple of sites.
Opening nested occurrences also opens the union of their enclosing chains.
Loaded functions expose those sound views together, so a reusable premise can
be consumed at different views in one proof. The family is exhaustive for
seven independent sites without enumerating the power set; for eight
independent sites, a proof requiring exactly four open and four opaque
occurrences may remain inconclusive. After that complete structural no-axiom
prefix, bounded instantiation plans cover many omitted middle subsets, but
chains
beyond four binders, constrained chains, and candidates outside the sequent's
own vocabulary stay out of reach, and the instantiation closure is capped per
scheme and per query. Four-binder tuple selection fairly mixes source-order,
repeated, sparse, and Cartesian shapes while one- through three-binder schemes
retain their historical order. Those caps lose completeness only, never
soundness. The nominal parametric-datatype plans obey the same caps and add no
negative evidence. An incomplete primary premise also makes negative evidence
conservative for the whole query. The examples use the same
Church Boolean and Church List shapes as the
[church-encoding reference](https://github.com/VladimirReshetnikov/Haskell/blob/main/church-encoding/src/Church.hs).

`:type EXPRESSION` (or `:t EXPRESSION`) is a separate, non-evaluating
inspection command. It infers against term signatures in the current loaded
module scope, regardless of the selected synthesis backend; the shared
`qualification` setting controls rendering. Ordinary ambiguity defaulting is
applied to eligible numeric variables, while `:type +d EXPRESSION`
additionally defaults those that occur in the reported type. This is a
documented expression subset, not a GHC evaluator; see the
[shared REPL guide](docs/repl.md#inspecting-expression-types) for supported
forms and diagnostics.

`:kind TYPE` (or `:k TYPE`) inspects type-level structure against that same
loaded module scope and neutral declaration inventory. It retains genuinely
generalized result kinds, prints class applications with a final `Constraint`,
and is likewise independent of backend selection. Attach the bang to the
command—`:kind! TYPE` or `:k! TYPE`—to add a second line with saturated type
synonyms normalized. See
[kind inspection](docs/repl.md#inspecting-type-kinds) for scope rules,
qualification behavior, and the intentionally supported kind-language subset.

`:eval EXPRESSION` compiles the loaded source workspace with real GHC and
evaluates one expression in the current prompt module/import context. This is
the only REPL command that executes Haskell code, and each invocation uses a
fresh interpreter. If the workspace or its prompt context does not compile,
evaluation falls back to Prelude scope and reports an advisory. See
[evaluating expressions](docs/repl.md#evaluating-expressions) for the scope,
fallback, isolation, and interrupt contract.

The shared REPL has a GHCi-shaped source workspace. `:load`, `:add`,
`:unadd`, and `:reload` manage module/file targets and their local source
dependencies; bare Haskell imports and `:module` manage the prompt scope;
`:show targets`, `:show modules`, and `:show imports` expose the three distinct
states. Loading and scope changes are transactional. This is source loading,
not GHC compilation for synthesis: Djex consumes declarations and signatures
without compiling or inferring function bodies. Imports written in each module
govern that module's declaration elaboration; bare interactive imports and
`:module` govern the later prompt scope and never reinterpret the inventory.
Djex projects that prompt scope into checked Exference and Djinn sessions.
Djinn's abstract type stubs reuse kinds inferred by the shared inventory, and
its presentation uses a record-selector spelling only when that selector is
visible unqualified. At startup, Djinn falls back to its standard checked
session if the initial source projection cannot be sealed. Later workspace,
scope, and projection-policy changes publish atomically across both backends;
a failed projection retains the complete previous state. The explicit `:eval`
boundary separately gives the loaded files to real GHC; its package and
compiled-module scope is not added to either synthesis inventory.
See the [shared REPL guide](docs/repl.md) for the target grammar, export
visibility, commands, settings, defaults, and failure behavior.

Djex can also delegate explicit package-manager work to Cabal:

```console
djex download CABAL_TARGET ...
djex install [--lib] CABAL_TARGET ...
```

The same operations are available inside the shared REPL as `:download`
(`:dl`) and `:install`. Downloading asks dependency-aware `cabal fetch` to
populate Cabal's configured source cache. Installing runs project-independent
`cabal install`; it installs executable components by default, while `--lib`
selects Cabal's library mode. Cabal may download dependencies, build arbitrary
package-supplied code, update its store, executable directory, or default GHC
package environment. Review target provenance before installing it.

Cabal targets are separate process arguments after `--`, never shell text or
Cabal options; Djex recognizes only the leading install-mode option `--lib`.
Targets may be repository package selectors, local package paths or archives,
or archive URLs, so Hackage verification does not cover every accepted target.
A package-manager command does not change backend selection,
the last synthesis query, or the loaded source workspace. In particular,
Cabal-installed `.hi` and object files do not become Djex declarations. Load
compatible `.hs`/`.lhs` source or signature stubs with `:load`/`:add` when a
package API should participate in synthesis. See the
[package-command contract](docs/repl.md#downloading-and-installing-packages).

For stateless invocation, select the backend explicitly:

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
load/parse/search/render failures exit 1; malformed command lines exit 2.
Package commands propagate ordinary Cabal exit codes; failure to launch Cabal
returns 1, malformed package-command input returns 2, and interrupt returns
130. A
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
shared kind scope, then compiles the alias-free goal directly into a formula
through one representation-neutral prepared definition cache. Opaque requests
retain their exact session-independent source view. Invalid search controls
have a typed core failure and stable `DJEX_DJINN_OPTIONS` diagnostic;
query-type provenance is attached only to source-derived input rejection,
never to separately supplied options or an internal proof/result invariant.
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
`DjinnType = Type DjinnTypeVariable`. `mkDjinnRequest` performs a bounded
structural preflight, validates each context class header without entering its
argument spine, and seals an opaque shared execution plan while retaining the
caller's exact neutral query; `djinnRequestQuery` recovers that stable source
view. Execution resolves each class in the selected session and checks every
finite arity, including contexts nested under a `forall`, before complete
canonicalization traverses an argument spine. It then capture-safely lowers
leading `ForallType` binders and normalizes the finite arguments, so known
cyclic spines terminate without a global class-width limit. The goal and every
constraint argument share one kind scope and synonym environment before the
operational goal enters formula compilation.

Djinn deliberately does not add context methods to the proof environment.
Treating a polymorphic method as one monomorphic premise made inhabitation
depend on incidental source-variable spelling. A constrained query therefore
succeeds only when its inhabitant is dictionary-independent: for example,
`Eq a => a -> a` can yield the identity function, while a query whose only
possible implementation calls `(==)` remains uninhabitable. Class lookup,
arity, and kind failures are still rejected rather than silently erasing an
invalid context. Queries return shared
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
are private presentation data. The private plan retains the canonical goal
and a detached, lexically checked spelling index, while projection, equality,
and display publish the caller's exact neutral request. Context argument
normalization is intentionally deferred: when a request is run, the session's
known class arity bounds each argument spine before traversal, without a
global class-arity limit. The adapter then checks context scope, builds the
canonical contextual goal, and binds the detached spellings to it. This makes
caller-built cyclic spines for known classes terminate with a structured kind
diagnostic while preserving ordinary finite external constraints. The common
`CachedQuery` owns the strict location separately from that opaque plan. The
source SPI validates every raw alias as a non-wildcard variable identifier
and detaches the map while sealing; contextual scope and deterministic alias
collapse occur once the session-bounded goal is available. Shared synonym
elaboration
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
check also covers the direct superclass prerequisites added to explicit
instance rules. A rule such as
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

All file and snapshot loaders reject duplicate logical modules before building
scope maps. This includes multiple headerless files, which all declare
`Main`; each later occurrence receives an `EXF_MODULE_DUPLICATE` diagnostic
that identifies the first source.

Directory discovery also recognizes optional `*.visibility` manifests for
hand-written signature catalogues. Lines use
`abstract|empty Module.Type ARITY PARAMETER_KIND...`; parameter kinds are
`Type` or fully parenthesized arrows such as `(Type->Type)`. Once a manifest is
present it must classify every constructorless datatype exactly once; unknown,
inhabited, duplicate, missing, kind-invalid, or arity-mismatched entries fail
with `EXF_TYPE_VISIBILITY`. Abstract entries retain the explicit checked kind
but contribute no pattern-match deconstructor, while empty entries continue to
support `case value of {}`. Directory loading, the installed default loader,
and unified-REPL directory targets apply this convention. Snapshot-owning
clients can opt in explicitly with
`environmentFromSourcesWithTypeVisibility` or
`loadExferenceSessionFromSourcesWithTypeVisibility` (and its policy-aware
counterpart). Ordinary explicit-file and source-snapshot APIs remain
manifest-blind and preserve normal Haskell semantics for user-written
`data Empty`.
The bundled catalogue marks `Data.Void.Void` and `GHC.Generics.V1` as empty and
its constructor-omitting base-library stubs as abstract.

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
datatype contexts, explicitly kinded parameters, existential or constrained
constructors, derived or overlapping instances, functional dependencies,
associated families and defaults, pattern-synonym signatures, declaration
splices, role annotations, and XML page or hybrid modules, each with the stable
`EXF_UNSUPPORTED_VOCABULARY` diagnostic code and its exact source span. The
exported `UnsupportedVocabularyForm` constructors are the authoritative
compatibility vocabulary for this rejection phase; `ExplicitExportList`
remains for source compatibility but is no longer emitted.

Every source signature and nominal declaration is elaborated in its defining
module's own scope. Local type and class names take precedence; direct imports
honor `qualified`, `as`, positive lists, and `hiding`. Loaded export surfaces
include named exports and `module M` re-exports, with re-exported entities
retaining their defining canonical names. As in Haskell, `module M` means the
identities available both unqualified and through the written qualifier `M`:
self exports include local declarations, aliases are honored, and a
qualified-only import contributes no names. A loaded `Prelude` is imported
implicitly unless the module is `Prelude`, imports it explicitly, or enables
`NoImplicitPrelude` or `RebindableSyntax`.

Interactive import and re-export routes retain Haskell's type/value namespace
selection alongside the namespace-neutral canonical `Name`. A type-only item
cannot expose a same-named constructor, `pattern` cannot expose its datatype,
and hiding either one leaves the other intact in both backend projections.

Ordinary imports must form an acyclic graph. Every source-loader entry point
rejects the first stable cycle as `CyclicModuleImports` before export surfaces
are computed; `{-# SOURCE #-}` imports are interface edges and intentionally
break that graph. The filesystem workspace and in-memory loader share one
stable dependency traversal, so cycle and ordering policy cannot drift.

Import resolution does not discover files. Directory and explicit snapshot
loaders elaborate only the supplied modules; the shared REPL first discovers
its resolvable local dependency closure and then invokes the same loader. Scope
is exact among loaded modules, so an unimported loaded name cannot bypass an
import by using its canonical qualifier. Unloaded module interfaces are not
available, and the open inventory therefore retains genuinely unknown names as
external. A positive import list provides a finite canonical external surface;
unrestricted and `hiding` imports cannot enumerate or verify the remainder.

Source loading retains private as well as exported declarations in the
authoritative inventory for whole-environment validation and later `*MODULE`
scope. Export lists govern downstream imports rather than deleting facts.
Ordinary positional, infix, record, strict, and unpacked datatype fields are
lowered explicitly; record selectors become rated Exference value bindings
exactly once. Fixities, ordinary value and method bodies, ordinary term patterns
and pattern-value bodies, default declarations, and operational pragmas remain
accepted because they do not add nominal declarations or executable semantics
to the synthesis inventory. These are explicit limitations, not syntax that is
silently reinterpreted.

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
filtering is a fatal structured diagnostic. Quantified subtrees remain
searchable through opaque atoms outside each backend's bounded rank-N rule.
Exference retains recursive datatype eliminators under a finite rule: matching
a recursive scrutinee exposes one constructor layer, and its fields become
ordinary providers in that branch without being fed back into eager pattern
decomposition. This constructs finite case expressions, not recursive calls or
induction. Multiple-constructor recursive types still require the existing
multiple-pattern opt-in, so their search-space cost remains explicit.

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

Frontends which want to parse once and choose or compare engines import
`Language.Haskell.Djex.HaskellSrc`. Its `ParsedSourceType` retains one shared
semantic type plus detached spelling and source-location metadata; the legacy
Exference parser functions delegate to this boundary.

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
output. Djinn then enforces its stronger closed-term invariant: a public
candidate with any residual obligation fails with
`UnexpectedResidualConstraints`. Clause scope and lexical failures retain
precedence when both parts of a candidate are forged.

Exference's live search tree is the same shared `Generated.Expression`
shape as those candidates: checker-specific type annotations inhabit a
private local payload, the historical `ExpVar`/`ExpLambda`/`ExpLet`/case
constructors are bundled bidirectional compatibility patterns over that
tree, and erasing annotations is a functor projection rather than a
recursive conversion. Its bounded visible-type-application node accepts `@_`
or a checked closed, variable-free, `forall`-free monotype and renders with
Haskell's required type-argument parentheses. Compiling such a candidate
requires `TypeApplications`; the surrounding provider signature will commonly
also require `RankNTypes`, and an ambiguous contextual signature may require
`AllowAmbiguousTypes`. Djinn's LJT lowering constructs and simplifies that
same shared generated `Expression`/`Pattern` tree directly; `HExpr` and
`HPat` remain only as projections for historical low-level callers.
Incremental hole filling, capture-safe let cleanup, and eta reduction live
in the shared generated-syntax module, parameterized only by Exference's
projection from an annotated local to its stable numeric identity. Djinn's
pattern alias normalization, unused-binder pruning, mixed term/type
application-spine inspection, and case-body alpha-equivalence use that same
authority. Generated-expression consumers can also use the shared pure or
effectful bottom-up rewriter instead of duplicating constructor walks.
Leading-lambda construction and decomposition are likewise shared: both
backends promote the complete nonempty lambda spine through the same
expression-to-clause operation, the inverse clause operation restores one
canonical group, and a
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
their generated expression bodies. The compatibility command,
`djex exference`, and the shared `djex` REPL obtain their session policy from
the same frontend operation. They exclude `Data.Function.fix`,
`Control.Monad.forever`, and `Control.Monad.Loops.iterateM_` by default; the
commands accept `--fix`, while the REPL also exposes `:set +fix`, as explicit
opt-ins. The unrestricted programmatic session default is unchanged. Parse,
kind, option, and search failures are structured diagnostics; one-shot
failures have failure exit status, while an interactive diagnostic leaves the
REPL available. Repeated compatibility-command inputs are all processed and
conflicting presentation modes are rejected. The historical ranking vector
remains an explicit compatibility profile, and `--short` adds backend-neutral
structural expression size to the candidate cost.

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

All library clients use one unnamed `djex` dependency for the curated facade,
shared synthesis vocabulary, checked adapters, lower-level engines, source
loading, shared REPL launcher, and historical compatibility APIs. Build-tool
dependencies for the commands remain `djex:djinn` and `djex:exference`; the
executable names are unchanged. The one library consequently has the union of
core and frontend dependencies: `haskell-src-exts`, `directory`, `filepath`,
`haskeline`, and `process` share the same versioned component contract as the
engines and frontends that consume their output.

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
