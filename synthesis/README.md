# Haskell Synthesis

The public `synthesis` sublibrary of the `djex` package is the parser- and
backend-independent foundation for Djinn and Exference. It was formerly the
standalone `haskell-synthesis` package. Its layers define validated Haskell
names, structured diagnostics, non-recursive class constraints parameterized
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

`Language.Haskell.Synthesis.Diagnostic` is the parser-independent reporting
boundary used by both sessions. It carries severity, an optional stable code,
source file and half-open span, and an ordered context trail; parser adapters
decide how native locations map into that neutral representation. Source
positions and spans are opaque and checked by smart constructors, so the
documented one-based, ordered half-open range cannot be forged through the
public API. Its renderer is deterministic and compiler-shaped, but callers
remain free to present the structured value themselves.

`Language.Haskell.Synthesis.Count` keeps intrinsically non-negative totals in
`Natural`, with a strict collection count and one explicit saturating boundary
for historical APIs that still expose machine-sized `Int` values.

`Language.Haskell.Synthesis.Collection` classifies finite ordered collections
without occurrence counters. Its opaque duplicate summary exposes exact
absent, unique, and duplicate membership, the repeated-value set, and repeats
in first-repetition order, so adapters can retain their established diagnostic
ordering without bounded counts or private duplicate-detection loops.

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
`functionClauseExpression` recovers the expression denoted
by a clause, retaining its argument patterns as a leading lambda while leaving
a patternless value body unchanged. The renderer allocates stable Haskell
variable spellings against globals and caller reservations, supports the three
qualification policies needed by the existing frontends, and prints symbolic
and tuple applications in Haskell form. Search/proof terms keep their private
types and annotations; backends erase them into this tree only after their own
checks.

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
constraints. It canonicalizes saturated function and tuple constructors,
decomposes application spines in source order, validates
lexical/arity/binder invariants, and computes free variables.
Datatype, synonym, and opaque declaration bodies deliberately do not inhabit
this AST; those belong to the declaration layer instead of being encoded as
special type nodes.

`Language.Haskell.Synthesis.TypeSynonym` prepares aliases from the exact
checked `Inventory` and expands them with simultaneous, capture-avoiding
substitution. It rejects partial applications and cycles, preserves legal
overapplication, and kind-checks both before and after expansion so a phantom
parameter cannot erase an invalid argument. Backends provide only a fresh
variable allocator for their identity domain; synonym and declaration
semantics stay parser-independent.

`Language.Haskell.Synthesis.TypeRender` renders that shared structure back to
compact Haskell source while leaving tagged variable spellings to the caller.
This preserves frontend distinctions such as flexible and rigid variables
that happen to reuse one numeric backend identity.

`Language.Haskell.Synthesis.Kind` and `.Declaration` provide the next source
layer: kind variables/arrows, kinded type parameters, synonyms, data and
constructor declarations, opaque types, values, classes, and instances. The
validator enforces declaration namespaces, distinct parameters/members, and
the common type invariants. Synonym bodies, datatype fields, superclasses, and
instance constraints must be covered by their declaration binders; value
signatures and class methods retain Haskell's implicit local quantification.
The layer does not prescribe backend-specific class or instance resolution.
Its `recursiveDataTypeNames` query is the common whole-declaration SCC
classifier. Callers invoke it only after synonym expansion; otherwise phantom
aliases can invent edges and alias-mediated recursion can hide them. Backend
adapters attach their own recursion metadata from that one nominal result.

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

`Language.Haskell.Synthesis.Inventory` is the frontend handoff that keeps a
sealed `Environment` together with the kind assumptions inferred from exactly
the same declarations. Query elaboration can therefore reuse checked kinds
without rerunning inference or trusting a parallel cache. Declarations may
retain a frontend's kind-variable identity while being edited or round-tripped;
sealing grounds their explicit kinds and reports the first unsolved identity.
Frontends that already own a sealed `Environment` can construct an inventory
from it directly, avoiding a redundant validation/indexing pass over the
source environment.
This is the boundary checked sessions retain; engine-specific dictionaries are
derived projections of it, not competing sources of truth.

Build or test just this sublibrary from the repository root or `djex/`:

```console
cabal build djex:lib:synthesis
cabal test synthesis-tests --test-show-details=direct
```
