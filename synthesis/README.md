# Haskell Synthesis

The `synthesis/src` area of the `djex` library is the parser- and backend-
independent foundation for Djinn and Exference. It was formerly the
standalone `haskell-synthesis` package and then a named Djex sublibrary. Its
layers define validated Haskell names, structured diagnostics, non-recursive
class constraints parameterized
over a backend's type representation, a scope-aware generated-code tree, and
neutral operational search status. Djinn
and Exference both store query contexts through the shared `Constraint` value
and consume the validated name vocabulary. Their checked Djex session adapters
also share the diagnostic, query, candidate, inventory, and generated-output
boundaries described below. Backend class resolution and search semantics
remain deliberately independent.

`Language.Haskell.Synthesis.Name` represents built-in tuple constructors only
at GHC's supported arities (zero or 2 through 64). In particular, `(# #)` is
the zero-field unboxed `Unit#` constructor; unary unboxed tuple values remain a
valid structural `Type`, but use `MkSolo#` rather than a comma-spelled tuple
constructor. The finite constructor ceiling also keeps every public name and
error renderer bounded even when handed an adversarial `SpecialName` payload.

`Language.Haskell.Synthesis.Type` is the single source-type tree used by the
foundation and both engines. Its structural queries own leading-prenex
decomposition, binder collection, general, constraint-argument, and nested
quantifier detection, complete embedded-constraint traversal, and nominal
constructor-head discovery. Its `applyTypeArguments`/`applicationSpine` and
`functionType`/`functionSpine` pairs give construction and decomposition one
source-order associativity contract. The prenex split returns binders, direct
constraints, and the residual body in source order without forcing later
layers merely to emit an outer binder. Its free-variable traversal similarly
exposes each scoped value once in first-occurrence order, then derives the
unordered set view from that same authority. Constraint arguments precede a
forall body, matching their source position, and an available variable does
not force an unused suffix.
Constraint collection has an explicit source-order contract: every direct
constraint at a forall precedes nested constraints in its arguments, followed
by constraints in the body. Historical Exference query names are compatibility
aliases over these operations rather than independent recursive walkers.
The tagged `Variable` operations likewise own identity projection, flexible
classification, tag-selective mapping, and flexible/rigid folds; backend code
supplies only its identifier container and error policy.

`Language.Haskell.Synthesis.Diagnostic` is the parser-independent reporting
boundary used by both sessions. It carries severity, an optional stable code,
source file and half-open span, and an ordered context trail; parser adapters
decide how native locations map into that neutral representation. Source
positions and spans are opaque and checked by smart constructors, so the
documented one-based, ordered half-open range cannot be forged through the
public API. A strict opaque `SourceLocation` pairs the complete name/span case
used by checked requests; `sourceTextLocation` evaluates the span while a
request is sealed, so a reusable request does not retain its complete input
buffer through a deferred traversal. Diagnostics deliberately retain separate
file and span fields because parser and filesystem failures can know only one.
Its renderer is deterministic and compiler-shaped, but callers remain free to
present the structured value themselves.

`Language.Haskell.Synthesis.Query` keeps source provenance out of semantic
`QueryRequest` equality and display. Its opaque `CachedQuery` owns a strict
`RequestProvenance` beside the neutral request and a lazy backend cache; both
adapters therefore share one programmatic-versus-sourced lifetime and
diagnostic contract without making parser metadata part of the synthesis
request. Its site-aware request traversal visits the goal first, then each
context's arguments and complete-context hook before the next context. Djinn
and Exference therefore share normalization order without giving up
backend-specific class validation or diagnostic wording. Both adapters retain
the exact supplied neutral request in the visible slot and keep canonical
execution values only in the cache. Cache and provenance differences remain
intentionally invisible to `Eq` and `Show`.

`Language.Haskell.Synthesis.Count` keeps intrinsically non-negative totals in
`Natural`, with a strict collection count and one explicit saturating boundary
for historical APIs that still expose machine-sized `Int` values.

`Language.Haskell.Synthesis.Collection` classifies finite ordered collections
without occurrence counters. Its opaque duplicate summary exposes exact
absent, unique, and duplicate membership, the repeated-value set, and repeats
in first-repetition order, so adapters can retain their established diagnostic
ordering without bounded counts or private duplicate-detection loops. The
separate short-circuiting `firstDuplicate` query preserves second-occurrence
precedence and can return without forcing an unused suffix. Its lazy
`distinctOn` operation retains the first value for each ordered key; emitting a
fresh representative likewise does not force the remaining input. Ordered
optional observations use the same explicit semantics: `firstPresent` stops at
the first value without forcing its suffix, while `maximumPresent` strictly
folds a finite collection and ignores absent values.

`Language.Haskell.Synthesis.Fresh` owns deterministic collision-skipping for
both unbounded and exhaustible candidate generators. It returns the selected
value, the reservation store with that value published, and the generator
state immediately after the selection without forcing that continuation. Its
ordinary entry points use `Set`; container-polymorphic forms let dense backend
domains retain stores such as `IntSet` without a boxed conversion. Djinn uses
the total form for string and `Natural` namespaces. Exference's single finite
`Int` supply uses the polymorphic exhaustible form for search variables and
rigid skolems, while its tagged declaration allocator retains the established
non-negative-before-negative ordering and structured exhaustion policy.

`Language.Haskell.Synthesis.Generated` is the common checked-output boundary.
It separates backend-owned local identities from structural global `Name`s,
and its opaque `DefinitionName` checks the narrower generated top-level value
namespace exactly once at a raw request or compatibility boundary. Function
clauses retain that checked name, so neither independently constructed output
nor a backend result can reintroduce an invalid definition between query
validation and rendering. The module represents lambdas, application,
tuples, holes, lets, cases, constructor/tuple
patterns, as-patterns, and function clauses. Its independent scope checker
rejects free locals, repeated binders in one pattern, and identity reuse in an
overlapping scope. `expressionHoles` retains structural order and duplicate
identities so allocation and backend completeness checks share one traversal.
`fillExpressionHole` replaces one selected identity throughout a partial tree
without recursively consuming holes in the inserted replacement, allowing an
incremental engine to publish newly allocated work safely.
`expressionFreeLocalIdentitiesBy` computes lexical free locals through lambda,
let, and case pattern scopes, while `expressionGlobals` provides the one
structural observation of referenced names and pattern constructors.
`substituteExpressionLocalBy` is the corresponding capture-avoiding local
substitution boundary: shadowing stops replacement, and a backend receives
`Nothing` when its payload would require freshening rather than risking silent
capture.
Application- and leading-lambda-spine decomposition, canonical nonempty-lambda
construction, lexical alpha-equivalence, total-term smart case construction,
redundant as-pattern normalization, and scope-aware unused-pattern pruning also
live at this boundary. The lambda operations flatten adjacent nonempty groups
without treating caller-built `Lambda []` as an identity: an empty group is a
malformed-syntax barrier retained for validation. Alpha-equivalence keeps a
bidirectional correspondence through lambda, let, and case scopes, so a free
local cannot become equal to a same-spelled binder; holes remain exact
operational identities. Pattern pruning accepts a backend identity projection
just like the ordinary simplifier, preserving annotations while deciding
binder use.
`simplifyExpressionBy` owns scope-aware let elimination, capture-safe
single-use inlining, and eta reduction for synthesis terms. A backend supplies
only its local-identity projection, so annotations remain intact while binder
comparison, shadowing, and occurrence saturation use the stable identity.
Local allocation and scope operations consume the generated pattern and
expression types' derived `Foldable` order directly. This keeps binder,
occurrence, and hole payload ordering attached to the canonical grammar rather
than mirrored by another private recursive walk.
`functionClauseFromExpression` and `functionClauseExpression` are the paired
expression/equation boundary: the former promotes a complete leading lambda
spine to clause patterns, while the latter restores those binders and
canonicalizes adjacent groups. A patternless value body remains unchanged. The
renderer allocates stable Haskell
variable spellings against globals and caller reservations, supports the three
qualification policies needed by the existing frontends, and prints symbolic
and tuple applications in Haskell form. Djinn's proof terms retain their
private representation and erase into this tree after proof checking.
Exference uses this tree during search as well, carrying checker-only type
annotations in its private local payload and erasing them by functor mapping at
the stable candidate boundary.

`Language.Haskell.Synthesis.Search` distinguishes a finished exploration from
one truncated by step, choice-point, candidate, queue, or depth limits, and
supports continuing chunk streams. It deliberately carries no logical
inhabitation claim: each backend keeps its own evidence and search semantics.
`observeProgress` losslessly classifies the last progress value a frontend
actually inspected as absent, continuing, finished, or truncated, without
looking beyond that supplied observation or interpreting logical evidence.

`Language.Haskell.Synthesis.Candidate` is the corresponding neutral candidate
envelope. It keeps generated output together with shared residual constraints
and backend-owned details such as proof or heuristic statistics. Mapping and
traversal affect only the output, so adapters can add a target-bearing clause
without rebuilding or weakening the attached obligations and metadata.
`renderCandidateExpression` and `renderCandidateDefinition` are the common
clause-to-text pipeline: adapters provide only their local-name hints and
qualification in `RenderOptions`, and callers receive the shared `RenderError`
without a backend-specific singleton wrapper.

`Language.Haskell.Synthesis.Query` supplies the generic request/result seam.
Targets are checked `DefinitionName`s; contexts use the backend's goal type,
while goal types, search options, metadata, and candidates remain parameters.
For shared `Type` goals, explicit contexts are inserted beneath the complete
leading `forall` chain without crossing a non-leading type boundary. Its
logical evidence is deliberately independent of `Search.Completion`: a backend
may return checked candidates from a truncated search, prove non-inhabitation,
identify an excluded target self-reference, or establish no conclusion.

`Language.Haskell.Synthesis.Selection` applies backend-neutral presentation
policies to a lazy list of query-result batches. Callers provide candidate rank
and admissibility functions; the selector can stop at the first candidate,
retain every globally best candidate in one strict traversal, inspect batches
for a bounded local-best lookahead, or stream all admissible candidates. Every
selection records the progress of the last batch it actually inspected, so an
empty selection still
distinguishes exhaustion or truncation from an empty continuing prefix. The
lookahead budget is measured in whole batches after the first admissible rank;
a strictly better batch resets it, while empty, inadmissible, equal, and worse
batches consume one unit. A companion single-pass selector supports two-tier
preference: it retains globally best fallback candidates until the first
preferred candidate appears, then discards the fallback regardless of rank and
applies the same batch-counted lookahead only to the preferred tier. This lets
frontends prefer candidates without residual obligations without retaining and
rescanning a lazy search trace.

`Language.Haskell.Synthesis.Type` is the common parser-independent source-type
tree: variables (optionally flexible/rigid), structural names, application,
functions, boxed or unboxed tuples, and explicit foralls with shared
constraints. It canonicalizes saturated function and tuple constructors and
exposes the inverse constructor-application view used by higher-kinded
unification without changing that structural storage convention. It
decomposes application and right-associated function spines in source order,
quantifies a policy-selected free-variable namespace at one outer scope,
capture-safely converts a leading explicit prenex chain into implicit fresh
variables, validates lexical/arity/binder invariants, and computes free
variables for both complete types and individual constraint arguments with
nested quantifier scope. It also exposes all-depth binder order separately
from ordinary occurrences and returns the exact first quantified subtree for
diagnostics, so adapters do not need private type-grammar inspections.
Its checked binder normalizer makes every explicit forall identity globally
unique against free and caller-protected names while leaving backend binder
admissibility and fresh-identity selection as explicit policies. Capture-safe
substitution also has a batch form that reserves every sibling source before
traversal and threads one fresh namespace across grouped types.
Datatype, synonym, and opaque declaration bodies deliberately do not inhabit
this AST; those belong to the declaration layer instead of being encoded as
special type nodes.

`Language.Haskell.Synthesis.TypeSynonym` prepares aliases from the exact
checked `Inventory` and expands them with simultaneous, capture-avoiding
substitution. Its non-expanding saturation preflight consults that same opaque
table, rejects partial applications in source order, and leaves legal
overapplication to kind checking. Whole-type and individual-application
operations are both available through the opaque prepared witness, so a
compatibility adapter can preserve its own syntax traversal without extracting
or copying synonym arities. Application arities remain exact `Natural` counts;
only the established machine-sized error payload applies an explicit saturating
projection. Full elaboration rejects cycles and
kind-checks both before and after expansion so a phantom parameter cannot
erase an invalid argument; the kind checker is also the single structural
validator for each phase. Backends provide only a fresh variable allocator
for their identity domain; synonym and declaration semantics stay
parser-independent. Its opaque `PreparedInventory` pairs one Inventory with
the exact normalized synonym table derived from it, so backend sessions cannot
recombine those authorities accidentally. Annotation-only mapping and
name-aware datatype-metadata adjustment preserve that table without repeating
allocator-sensitive synonym preparation.
`prepareInventoryExpansion` adds a separate opaque, transient
`PreparedInventoryExpansion`: it prepares that exact witness, retains synonym
declarations in their checked source spelling, expands every other declaration
left-to-right, and classifies datatype recursion from the resulting operational
stream. Declaration failures carry their zero-based source index and nominal
subject; whole-table preparation failures remain distinct from operational
use-site attribution. Fresh allocation restarts at the same lexical boundaries
as ordinary type expansion,
so neither sibling fields nor later declarations can perturb one another.
Backends consume the expanded stream and recursion set while sealing, then keep
only the prepared Inventory and their derived indexes rather than a duplicate
declaration tree.

`Language.Haskell.Synthesis.TypeRender` renders that shared structure back to
compact Haskell source while leaving tagged variable spellings to the caller.
This preserves frontend distinctions such as flexible and rigid variables
that happen to reuse one numeric backend identity. Constraint rendering has
one structural owner in `Language.Haskell.Synthesis.Constraint`:
`showsConstraintWith` accepts the type layer's argument-position renderer, so
the generic `Show` instance and the shared type renderer retain their own
precedence policies without separately traversing class names and arguments.

`Language.Haskell.Synthesis.Kind` and `.Declaration` provide the next source
layer: kind variables/arrows, kinded type parameters, synonyms, data and
constructor declarations, opaque types, values, classes, and instances. The
validator enforces declaration namespaces, distinct parameters/members, and
the common type invariants. Synonym bodies, datatype fields, superclasses, and
instance constraints must be covered by their declaration binders; value
signatures and class methods retain Haskell's implicit local quantification.
The layer does not prescribe backend-specific class or instance resolution.
It maps type and kind identities independently, exposes every type-variable
occurrence in structural source order, and assigns each declaration a nominal
diagnostic subject (the head class for an instance). Backend projections can
therefore repack identities or attribute failures without rebuilding every
declaration constructor privately.
Its `recursiveDataTypeNames` query is the common whole-declaration SCC
classifier. The prepared-expansion boundary enforces its alias-free
precondition; otherwise phantom aliases could invent edges and alias-mediated
recursion could hide them. Backend adapters apply their own policy to that one
nominal result rather than repeating expansion or graph construction.

`Language.Haskell.Synthesis.KindInference` owns the common kind unifier. It
checks several types in one variable scope, gives class methods independent
local quantifiers around shared class parameters, validates constraint arity
and parameter kinds, recognizes intrinsic function/list/tuple constructors,
and infers legacy reduced acyclic type-constructor graphs in dependency order.
Its whole-inventory operations accept only an opaque, structurally validated
`Environment`: callers cannot bypass declaration and namespace validation with
a raw declaration list. Kind inference unwraps that exact sealed environment
internally, admits recursive datatype groups, rejects recursive synonym
expansion, checks values and instances, and
generalizes otherwise unconstrained class parameters for poly-kinded classes
such as `Typeable`. A Haskell 98 compatibility frontend can instead request
that declared class parameters be defaulted to `Type`; both policies freeze
residual variables beneath an already-known higher-kinded shape before values
and instances are checked. Public fixed-kind results use an uninhabited
kind-variable parameter, so an unsolved monomorphic kind cannot escape.
Closed inventories reject unknown type and class names; open inventories infer
one stable kind per external type name and require every occurrence of an
external class to agree on arity. Synonym kinds are frozen after definition
checking, before operational declarations, so a value or instance cannot make
an unused phantom parameter higher-kinded. Closed inventories freeze all
nominal type kinds at that boundary; open inventories deliberately leave
datatype kinds live so compatibility frontends can represent abstract empty
datatype stubs whose omitted shape is supplied by later instances.

`Language.Haskell.Synthesis.Environment` seals structurally valid declarations
and builds deterministic type/class, value/method, constructor, and
instance-head indexes. It rejects cross-declaration namespace collisions and
duplicate instances modulo alpha-renaming of declaration and nested `forall`
binders, while preserving the original source heads in its public index and
diagnostics and preserving qualified-name identity. It does not pretend that
an index is also a kind proof or a backend's resolution policy.
`mapEnvironmentKindVariables` changes explicit kind-variable identities across
every declaration-bearing view without rebuilding or revalidating those
indexes; in particular, it losslessly weakens a grounded `Void` environment
when a compatibility editor needs its historical kind-variable parameter.

`Language.Haskell.Synthesis.Inventory` is the frontend handoff that keeps a
sealed `Environment` together with the kind assumptions inferred from exactly
the same declarations. Query elaboration can therefore reuse checked kinds
without rerunning inference or trusting a parallel cache. Declarations may
retain a frontend's kind-variable identity while being edited or round-tripped;
sealing grounds their explicit kinds and reports the first unsolved identity.
Frontends that already own a sealed `Environment` can construct an inventory
from it directly, avoiding a redundant validation/indexing pass over the
source environment. `PreparedInventory` is the stronger boundary checked
sessions retain: engine-specific dictionaries and indexes are derived
projections of its Inventory and exact synonym table, not competing sources of
truth.

Build the library or run the focused foundation suite from the
repository root or `djex/`:

```console
cabal build djex:lib:djex
cabal test synthesis-tests --test-show-details=direct
```
