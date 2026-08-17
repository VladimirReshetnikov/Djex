# Djex architecture

Djex is one Cabal package with one library, three installable executables
(`djex`, `djinn`, and `exference`), and two private test-fixture executables
(`djex-fake-cabal` and `djex-fake-z3`) that test suites invoke as controlled
stand-ins for the real tools. The library
contains a shared parser-independent synthesis foundation, two deliberately
different search engines, checked adapters for both engines, and the historical
Djinn and Exference compatibility frontends.

This guide describes the current architecture. The dated files under
`docs/reports/` record how the repository reached it and should be read as
historical review notes rather than as the current API guide; the
[reports index](reports/README.md) lists all of them chronologically.

## Source and component layout

| Path | Responsibility |
| --- | --- |
| `synthesis/src/` | Shared names, types, declarations, environments, kind inference, diagnostics, generated code, and query/result envelopes. |
| `synthesis/internal/` | Package-private fingerprints, protocol machinery, and shared implementation invariants. |
| `djinn/src-core/` | Djinn's checked adapter, logical translation, LJT proof search, and proof checking. |
| `djinn/src-internal/` | Package-private Djinn implementation modules shared by the library and focused tests. |
| `djinn/src-frontend/` | Historical `Djinn` API and compatibility Haskeline REPL. |
| `exference/src-core/` | Exference's checked adapter, class environment, heuristic search, unification, scoring, and independent expression checker. |
| `exference/src-frontend/` | Haskell-source extraction, environment loading, and the historical Exference command API. |
| `src/` | The `Language.Haskell.Djex` facade, shared REPL and result presentation, and merged `djex` command. |
| `app/`, `djinn/app/`, `exference/app/` | Thin executable launchers. |
| `test-support/` | Shared CLI test assertions plus the private `djex-fake-cabal` and `djex-fake-z3` fixture executables. |

All eight source roots compile into the unnamed `djex` library. The directory
split documents dependency direction and provenance; it is not a set of Cabal
sublibraries.

```text
Haskell source / Djinn syntax
            |
            v
 shared Name, Type, Constraint, Declaration
            |
            v
 Environment -> Inventory -> PreparedInventory
            |                       |
            |              synonym/kind authority
            |                       |
            +--------+--------------+
                     |
          +----------+----------+
          |                     |
          v                     v
  Djinn prepared proof     Exference prepared
      environment          search environment
          |                     |
          +----------+----------+
                     v
 QueryRequest -> checked request -> QueryResult
                     |
                     v
       SearchBatch (Candidate FunctionClause)
                     |
                     v
          shared validation and rendering
                     |
                     v
       one-shot CLI or persistent shared REPL
```

## Shared authorities

The merger is organized around a few values that own invariants once:

- `Name`, `Type`, and `Constraint` are the common source vocabulary. Backend
  compatibility aliases and patterns are views of these values, not parallel
  recursive representations.
- `Environment` owns declaration order, namespace checks, and deterministic
  indexes. Its constructor is hidden so those views cannot disagree.
- `Inventory` pairs one grounded `Environment` with the kind assumptions
  inferred from exactly that environment.
- `PreparedInventory` pairs an `Inventory` with its exact checked synonym
  table. The transient `PreparedInventoryExpansion` expands operational
  declarations and classifies datatype recursion without becoming a second
  retained declaration authority.
- `QueryRequest`, `CachedQuery`, `QueryResult`, `SearchBatch`, and `Candidate`
  provide the common request/result protocol. Backend options, metadata,
  logical evidence, and candidate details remain typed backend parameters.
- `Generated.Expression`, `Pattern`, and `FunctionClause` are the common output
  grammar. Scope checking, simplification, qualification, and Haskell rendering
  happen at this boundary. `Candidate` remains constructible for compatibility,
  so the stable candidate renderers do not treat construction as proof of
  validity: they reject a free local or duplicate pattern-binder identity before
  rendering either an expression or a definition.

Opaque values intentionally use smart constructors and ordinary projection
functions instead of exported record fields or `Generic` representations when
record update or `GHC.Generics.to` could bypass an invariant.

Both checked adapters use the same ownership split:

| Layer | Responsibility |
| --- | --- |
| Public backend facade | Parse requests, run the backend, translate failures, and render candidates. |
| Private `Internal.Request` owner | Seal the exact neutral request together with canonical execution data and diagnostic provenance. |
| Private `Internal.Session` owner | Seal the authoritative inventory together with every backend index derived from it; publish replacements only after transactional validation. |
| Backend core | Execute proof or heuristic search without owning the public integration contract. |

The private owners are Cabal `Other-Modules`. Keeping them separate from the
facades makes the two adapters uniform without conflating their search
semantics or exposing their cached plans.

## Finite structural boundaries

The public syntax trees are intentionally ordinary Haskell values, including
lazy lists. A smart constructor therefore must not call `length`, perform a
complete fold, or canonicalize a child before it has established that every
width-bearing spine is finite and within its semantic bound.

The shared preflight operations use bounded observation:

- a known class application inspects no more than its declared arity plus one
  argument cell;
- a tuple inspects no more than the supported maximum arity plus one element;
- the same operation descends through nested type and `forall` constraints only
  after validating each constraint header; and
- environment construction performs this pass using a preliminary class-arity
  table before duplicate, free-variable, kind, or canonicalization analyses.

Djinn request/context validation, Exference request and independent-checker
validation, shared kind inference, and Exference class/instance construction
apply the same rule at their exposed boundaries. Thus a cyclic argument spine
or an overlong/cyclic tuple receives the corresponding structured arity or type
error without hanging an otherwise bounded operation. The later full traversal
still owns semantic validation; the preflight establishes only the finite-width
condition needed to run it safely.

## Evaluation and streaming boundaries

Strictness follows ownership rather than a blanket eager-or-lazy rule:

- Finite declaration indexes and left-associated type, proof, expression, and
  HSE application spines are accumulated strictly. Once one of these
  operations is requested, it must consume the complete finite list before the
  resulting index or outermost left-associated tree is useful; retaining
  pending fold applications only increases residency.
- A strict fold over a product is not sufficient by itself: it evaluates the
  product constructor, not its fields. Multi-index builders explicitly force
  each map/set summary while leaving payload values lazy when their boundary
  requires it. The class-arity inventory, for example, materializes its map
  structure without forcing parameter-list lengths until an arity is queried.
- Search batches, candidate tails, adapter caches, and right-associated syntax
  remain lazy where a caller can make progress from a prefix. `SelectFirst` and
  `SelectAll` do not inspect later results, and checked query-result projection
  does not force candidate payloads merely to establish that a batch is
  nonempty.

This contract lets frontends stream heuristic results without making already
requested finite construction retain avoidable thunk chains. Wide-batch and
wide-application regressions protect both sides of the boundary.

## Shared interactive frontend

The `djex` executable starts a persistent REPL when invoked without arguments
or with the `repl` subcommand. `Language.Haskell.Djex.REPL` exposes the same
launcher in process. The interactive frontend and the one-shot subcommands use
one private result-presentation operation, so selection, validation, rendering,
residual constraints, evidence, truncation notices, and diagnostics cannot
silently acquire two frontend interpretations.

The REPL is a coordinator over one source parser and two sessions, not a third
synthesis engine and not a mutable union environment:

```text
REPL state
  |-- Djex source query parser
  |     `-- one resolved, kind-checked shared Type plus source metadata
  |-- Djinn runtime
  |     |-- immutable standard-session fallback and axiom policy
  |     `-- Maybe checked projection of the current prompt scope
  |-- Exference runtime
  |     |-- requested targets and Maybe SourceWorkspace
  |     |-- Maybe prompt scope
  |     |-- fix policy
  |     |-- Maybe immutable base and search-projected sessions
  |     `-- diagnostics from the latest load attempt
  |-- visible record-selector presentation map
  |-- active backend selection and last query
  |-- shared result, presentation, and prompt settings
  |-- backend-specific search settings
  `-- active script-inclusion stack
```

Switching the active backend changes routing only. When a source workspace is
available, every mode parses, resolves, expands synonyms, and kind-checks the
query once through `Language.Haskell.Djex.HaskellSrc`. The checked shared type
is then projected structurally into Djinn's prompt vocabulary and Exference's
tagged variable vocabulary. Both-mode labels each result and continues to the
second backend when the first backend's lowering or search reports a
diagnostic. A common parse or kind error is reported once. The historical
backend parsers remain the compatibility fallback when the shared source
runtime, or a requested Djinn scope projection, is unavailable.

The two search projections share `TypeAtom` for quantified subtrees. Its
retained source tree supports rendering and capture-avoiding substitution of
free variables; its cached lexical alpha key owns equality and ordering.
Binders use scope and declaration position, so spelling is irrelevant while
reordering binders or changing a free variable is significant. Ordinary
unification never decomposes an atom. Instead, the two engines own explicit,
bounded rules at different architectural boundaries. Djinn's polarized formula
compiler may reopen a positive forall, including a contextual one; the
validated context contributes no LJT premises, so generated inhabitants remain
dictionary-independent. Exference may open an exposed goal's complete leading
forall chain, substitute fresh rigids through each layer's contexts and body,
and make those contexts lexical givens only for that body. Deferred obligations
retain their exact given snapshot, preventing binderless or sibling evidence
leakage. Exference may also instantiate a scoped provider's leading binders
immediately before ordinary type unification. A separate visible branch keeps
explicit-instance selection monotype-only, while a fully vacuous,
context-free scoped or retained global provider with no free flexible
variables may select complete closed context-free foralls from the checked
query's proven proper-type positions. Ambient query rigids may remain in the
provider body; they are never solved by this branch. Both sources are bounded
and the independent checker replays the choice.

A richer frontend may supply provider-local facts through either of two shared
relations. `ProviderInstantiationCandidate` contributes independent scalar
types to one exact provider's bounded candidate pool. It remains the stable
compatibility path for frontends which genuinely authorize the resulting
Cartesian combinations. `ProviderInstantiationAssignment` instead contributes
one complete ordered leading-binder vector. It preserves the correlation
established by one external proof, so neither backend reconstructs cross-vector
tuples. `KindedProviderInstantiationAssignment` carries the same relation but
pairs each argument with a frontend-attested positional `GroundKind`; it exists
for source kind facts which the retained, kind-erased value scheme cannot
recover.

The checked Djinn and Exference runners first bound either outer relation at 32
before entering an element. Assignment runners also bound each argument spine
at six before entering an argument. The runners then resolve every `Name`
against the exact sealed session and require an eligible context-free retained
polymorphic scheme whose complete leading arity matches the vector.

After those checks, kinded runners productively count at most 129 constructors
in each caller-supplied kind under the public
`maximumProviderInstantiationKindNodes = 129` bound. If work remains, the
observer returns the one-over-bound sentinel without entering the pending
constructor. This per-kind preflight precedes kind inference, same-provider
vector equality, backend kind conversion, and forcing the paired type. It
rejects a cyclic kind or any finite tree above the bound without entering an
unbounded remainder, while preserving the shared 64-tuple constructor's
129-node right-associated all-`Type` kind. The latter is a capacity within one
argument's kind, independent of the six-argument vector limit.

Each scalar candidate is elaborated as a closed, context-free proper type in
the sealed session's synonym and kind scope. That compatibility relation
remains proper-type-only. For a legacy assignment, both adapters infer the
ground kind of every leading binder from the exact provider body, default an
otherwise unconstrained vacuous binder to `Type`, and elaborate each argument
at that inferred kind.

For a kinded assignment, the adapters check the retained body at `Type` in one
kind-inference scope shared with every supplied binder-kind obligation. A
non-vacuous occurrence must therefore agree with the caller's `GroundKind`.
A vacuous binder imposes no observable constraint, so its source kind remains
caller-attested rather than backend-inferred; the paired argument is still
elaborated at exactly that supplied kind. This admits bare and partially
applied higher-kinded constructors in vacuous positions. Repeated assignments
for one provider must present the same complete binder-kind vector before
their type vectors are alpha-deduplicated. Multiple distinct vectors with that
same kind vector remain available independently; cross-engine regressions pin
two choices for one vacuous provider at the genuinely higher-order kind
`(Type -> Type) -> Type`, using both a bare and a partially applied
constructor.

Shared normalization continues to represent a saturated boxed pair as
`TupleType`. An exact substitution may instead leave its higher-kinded head
beneath an opaque application or contextual polytype. At that Djinn projection
boundary, only the canonical bare boxed arity-two `TypeConstructor` lowers to
the historical `(,)` atom; its renderer recognizes that already-parenthesized
spelling rather than producing `((,))`. Wider boxed and every unboxed tuple
constructor left bare or partially applied still fail with
`PartialTupleConstructorUnsupported`. This local compatibility rule preserves
ordinary structural pair compilation without widening the supported tuple
vocabulary.

Both assignment forms require lexically closed specified visible arguments,
then substitute the complete vector and independently check the whole
specialized body at kind `Type`. An argument may be a contextual polytype; its
nested context remains part of that selected type and does not become a
provider obligation. Contextual provider schemes and kind-mismatched vectors
are rejected. Scalar types and complete vectors are alpha-deduplicated only
within one provider. All relations remain keyed by provider identity, not by
scheme shape: two globals with alpha-equivalent schemes cannot donate evidence
to one another.

Calling either historical runner is definitionally the same as calling its
`WithInstantiationCandidates` variant with `[]`; calling either the legacy
`WithInstantiationAssignments` or parallel
`WithKindedInstantiationAssignments` variant with `[]` has the same observable
result, ordering, diagnostics, and finite-budget behavior. The nonempty public
entrances remain distinct: an assignment is never flattened into the legacy
scalar pool, and kinded evidence is never silently stripped of its kinds.

Djinn compiles each retained scalar specialization or exact vector into a
direct specialized premise for that loaded polymorphic provider. The
evidence-enriched structural and nominal plans also contain the historical
query-local and loaded instantiation premises, so one checked proof may compose
old and supplied evidence. Wholly vacuous scalar tuples and exact vectors
remain in the structural projection. A non-vacuous specialization enters that
projection only when the prepared structural compiler can observe every
relevant argument through its corresponding datatype formal. That existential
observation is only a boundary gate: the instantiated constructor body is then
traversed, and every marker-bearing field must preserve the argument at each
nested datatype boundary. The complete vector is marked before
substitution, so the walk checks both structure nested inside an assigned type
and scheme-owned arguments applied to an assigned higher-kinded head. The check
is per nominal argument boundary: it keeps faithful constructor fields and
correlated faithful parameters, but rejects a wholly phantom formal or one
independently erased occurrence of a repeated assignment. Recursive, unknown,
and fully applied empty datatypes remain opaque exactly as they do during
formula compilation. Rejected structural specializations remain in the nominal
family,
so erased phantom parameters cannot justify a nominally mismatched visible
application. For a nonempty evidence call this
strict superset runs before the evidence-free loaded tails, preventing a
productive loaded proof stream from starving the supplied route at the global
candidate cutoff.
Scalar pools retain their six-binder, 512-attempt, and sixteen-per-scheme
tuple windows; exact vectors bypass those reconstruction windows while
remaining within the 32-premise family bound. The independent proof checker
sees each specialization before lowering rewrites its synthetic proof identity
into the corresponding visible application of the real provider. A
target-named specialization is available only to the self-reference diagnostic
search.

Exference instead keeps the checked relation in its query state and consults it
only from exact retained-global lookup. A productive exact assignment vector
receives one leading visible lane so exact-spelling deduplication cannot discard
its checked association. Ordinary implicit use follows as a fallback, then the
historical visible sequence of monomorphic Haskell instance heads, checked
query-derived choices, and separately capped legacy scalar choices. Without a
productive exact assignment, that ordinary-first sequence is unchanged. The
exact route consumes a vector once and may instantiate non-vacuous leading
binders, whereas the scalar pool route requires a completely vacuous prefix.
Scoped values never consult either map. These are finite evidence-directed
typing rules, not permission for ordinary unification to decompose a polytype
or a claim of general impredicative inference.

Djinn has four bounded instantiation-axiom families. The historical
query-local family instantiates up to six leading binders at variables,
opened-forall skolems, premise-scope spellings, and guarded quantified query
subtrees. A separate positive-only query-correlated family fairly schedules
that same finite vocabulary. It retains only tuples which pair a quantified
candidate with a binder occurring free in the scheme body and whose complete
specialized body is alpha-equivalent to a subtree of the canonical elaborated
query. The fair producer prefilters at most 512 raw tuples per scheme.

Each structural or nominal builder starts with the exact axioms retained by its
active bounded historical run and treats any identical logical formula as a
duplicate, even when visible evidence differs. Before enumeration it seeds its
worklist with nested schemes exposed by those historical formulas, without
spending an attempt. A later duplicate correlated candidate also exposes nested
schemes and spends one eligible attempt but no axiom allowance. The builder
separately charges at most 512 eligible attempts across the family and retains
at most sixteen axioms per scheme and 64 in total.

The established query-closed positive-only family revisits only hypothesis
schemes embedded in the elaborated goal. It adds closed, forall-free monotype
subtrees from that goal and retains only tuples containing at least one such
candidate, so it does not duplicate the historical prefix.

Environment sealing separately keeps exact context-free loaded schemes in a
private structural/nominal index. That appended family may use the historical
candidates plus closed, forall-free subtrees of the checked query and
synonym-expanded loaded signatures. All four families share visible-evidence
retention: inferable choices erase, but a vacuous binder retains the shortest
visible prefix. Every complete substituted body and specified visible argument
is kind-checked against the prepared environment. These bounded extensions are
proof-producing only; the query-correlated, query-closed, and loaded
families are positive-only, and a missed non-target loaded scheme cannot
support negative evidence. The plan schedule retains the historical prefix,
then the provider and loaded plans, and then the formerly final query-closed
structural and optional nominal plans unchanged. Pure query-correlated plans
follow and carry historical, loaded, and provider premises without importing
query-closed axioms. A final combined superset appears only when both families
contribute and permits their instances to compose. None of these tails resets
the query-wide cutoff or fuel. Quantifiers outside these explicit boundaries
remain opaque.
Structure surrounding those opaque quantifiers retains its ordinary form,
which permits impredicative values such as
`[(forall a. a -> a)]` without claiming general higher-rank subsumption.

Djinn's formula compiler applies a similarly bounded rule to recursive
datatypes classified by the prepared inventory expansion. It derives
alias-normalized recursive SCC identity from the same graph used for definition
validation. A positive logical path may expose one constructor layer from each
of at most two distinct SCCs. Its branch-local SCC trail survives restored type
arguments, so direct, mutual, alias-hidden, and parameter-mediated same-SCC
revisits remain complete atoms; a third SCC, negative occurrences, and the
exact-opaque view are atomized as well. Real constructor injections still lower
through the ordinary proof term, while the exact plan preserves recursive
identity. Every translation that encounters recursive structure is marked
incomplete, so its empty proof stream cannot become negative evidence. An
unrelated recursive SCC does not weaken a complete translation for a query
which never reaches it.

The Exference runtime is deliberately richer than a bare session. Source
targets, ratings, prompt scope, and command policy are not recoverable from its
annotation-erased neutral environment, so `:load`, `:reload`, and `:set fix`
construct a complete candidate runtime from the retained or requested
workspace. The workspace, prompt scope, policy, base session, and searchable
session are published together only on success. A failed initial load leaves
Exference unavailable while Djinn remains useful; a failed replacement retains
the preceding usable sessions and settings while recording the new diagnostics
for inspection.

Private `Language.Haskell.Djex.REPL.Command` owns one descriptor table for
parsing, unique-prefix resolution, help, and completion inventories. Private
`Language.Haskell.Djex.REPL.Driver` owns the Haskeline loop, dynamic prompt,
explicit multiline collection, optional persistent history, and Ctrl-C
rollback to the state preceding the interrupted input. Script execution feeds
the same logical-input evaluator, including nested-script cycle detection,
rather than maintaining another command grammar.

Private `Language.Haskell.Djex.Package` is the single process boundary for the
top-level and REPL package commands. It validates the target-vector invariant,
resolves Cabal from `PATH`, constructs fixed argv prefixes for `fetch` and
project-independent `install`, with an explicit library mode, streams inherited
process output, and normalizes launch, interrupt, and exit reporting. Targets
follow an explicit `--` separator and never pass through a shell. Child stdin
and unrelated file descriptors are closed. A dedicated process group plus
Windows job support bounds ordinary descendant cleanup on interruption. The
operation is intentionally outside `ReplState`: it can mutate Cabal's cache,
store, and package environment
but cannot mutate backend sessions, query history, or the source workspace.
This also keeps the semantic boundary honest—Cabal installation does not turn
compiled interfaces into neutral Djex declarations.

Private `Language.Haskell.Djex.REPL.Type` implements the read-only `:type`
boundary. It derives term schemes from the authoritative neutral declarations,
including ordinary value signatures, data constructors, and class methods,
and resolves them with the same namespace-aware module scope used by other
REPL inspection. Constraint checking uses the class environment prepared from
that complete inventory. It deliberately does not infer through Exference's
policy-filtered synthesis dictionary, so backend selection and search settings
cannot change the answer; qualification remains a presentation concern.

Private `Language.Haskell.Djex.REPL.Kind` implements the parallel read-only
`:kind`/`:kind!` boundary. Scoped source conversion admits constructors,
synonyms, and classes without prematurely requiring a `Type` result. The
foundation's `inferTypeKind` reuses the ordinary checked kind engine but keeps
canonical residual variables instead of applying Haskell-98 grounding.
Saturated synonym normalization is delegated to the exact
`PreparedInventory` retained by the Exference session; only a synonym at the
complete operational head beneath context-free prenex foralls may remain
undersaturated, and source and normalized forms are both kind-checked for
`:kind!`. Plain `:kind` uses the corresponding non-expanding saturation check.
The shared `Kind` tree remains the backend contract used by
Djinn's closed compatibility patterns, so the REPL adds `Constraint` only to a
private inspection/presentation kind. A class may be that complete head under
the same context-free forall wrapper; nested Constraint-kinded class forms are
rejected explicitly. Like `:type`, this path uses the neutral base session and
module scope rather than the selected backend or filtered search projection.

Bare input remains synthesis rather than GHCi-style expression evaluation.
`:type` parses a term expression and performs structural inference without
running it or compiling loaded function bodies; `:kind` inspects only the
structural kind subset described above. The deliberately explicit `:eval`
command is the separate execution boundary. Its private Hint adapter compiles
the complete loaded dependency closure, translates the checked prompt entries
to GHC top-level modules and exact ordinary/qualified import surfaces, and
runs one expression in a fresh interpreter. Workspace or context setup is
transactional: failure resets to installed Prelude scope and emits one
advisory, while no partial GHC context enters either synthesis session.
The inferencer covers common applications, operators, lambdas and patterns,
conditionals, cases, tuples, lists, enumerations, literals, and annotations;
forms requiring local-declaration generalization or richer GHC semantics fail
with an explicit diagnostic. Annotations are currently restricted to ground
types; polymorphic annotation checking requires skolemization and context
entailment and is rejected rather than approximated. Because neutral
declarations do not retain source fixities, unparenthesized infix chains are
also rejected rather than associated speculatively. The historical `djinn`
executable remains a separate compatibility REPL with its legacy declaration
editor, while the historical `exference` executable remains one-shot. See
[the shared REPL guide](repl.md#inspecting-expression-types) for the exact
supported forms, defaulting behavior, and failure contract.

## What remains backend-specific

Uniform architecture does not mean identical semantics:

- Djinn translates a supported type into intuitionistic logic and runs a
  terminating LJT proof search when no explicit budget truncates it. It can
  prove non-inhabitation within that logic.
- Exference performs ranked heuristic exploration with explicit step, queue,
  and depth controls. Exhausting that configured search is not a proof that no
  Haskell expression exists.
- Each backend retains its own class/instance operational policy, search state,
  scoring, and evidence. Shared class and inventory values describe source
  facts; they do not prescribe resolution semantics. Exference's nominal
  environment rejects a groundable prerequisite if it increases the instance
  head's type-node measure or any head-variable occurrence count, including
  after superclass inflation. This rules out growing chains such as
  `C [a] => C a`; exact and size-preserving cycles terminate through the
  solver's current-path constraint check, while prerequisites containing a
  variable absent from the head remain unresolved. Instance resolution for an
  accepted finite ground constraint therefore terminates even though the
  enclosing heuristic expression search is not a decision procedure.
- Djinn stores canonical historical binder spellings in lowered proof output.
  Exference stores numeric locals and applies checked spelling hints at render
  time. This difference is part of the existing observable contracts.
- Djinn's raw abstract-type tuple is normalized before either legacy kind
  checking or shared declaration conversion: its duplicated names must agree,
  it cannot carry parameters, and the embedded declared kind refreshes the
  outer compatibility cache.

Logical evidence and operational completion are therefore separate. A result
can contain validated candidates from a truncated search, and a finished
Exference search can still carry `NoEvidence`.

## Haskell-source loading and session policy

The Haskell-source frontend is an optional boundary around the same neutral
Exference session constructor. Its diagnostics retain phase ownership:

- directory, module-read, parse, unsupported-vocabulary, and extraction
  failures use stable `EXF_*` codes; source-aware failures preserve the exact
  source span supplied by that phase;
- once a neutral inventory exists, shared preparation and policy errors use
  `DJEX_EXF_*` codes and may have no single source location; and
- warnings and omissions remain separate from the fatal load result in
  `ExferenceSessionLoadReport`.

The vocabulary scan is fail-closed. In addition to unsupported type/class
features, it rejects pattern-synonym signatures and XML page or hybrid modules
rather than silently projecting only part of them. `UnsupportedVocabularyForm`
is the authoritative compatibility vocabulary; its `ExplicitExportList`
constructor remains available to source clients but is no longer emitted.
Ordinary term patterns and pattern-value bodies are accepted, as are ordinary
value/method bodies and other syntax that does not alter the nominal
inventory.

Every source declaration is elaborated in its defining module's nominal scope.
Local type and class declarations take precedence; only direct imports add
loaded names, and `qualified`, `as`, positive import lists, and `hiding` are
honored. A loaded `Prelude` contributes its implicit unqualified surface unless
the module is `Prelude`, imports it explicitly, or enables `NoImplicitPrelude`
or `RebindableSyntax`. Export surfaces include named exports and `module M`
re-exports, so a downstream import sees only that surface while a re-exported
entity retains its defining canonical name. Transitive imports do not become
scope unless they are explicitly re-exported. A `module M` item is computed as
an identity intersection: the entity must be in scope both unqualified and
through the written qualifier `M`. This includes the defining module's locals,
honors `as` aliases, and prevents qualified-only imports from leaking.

This resolution pass does not discover or load dependencies. Directory and
explicit snapshot loaders elaborate exactly the modules supplied to them; the
REPL workspace separately discovers resolvable local imports before invoking
the loader. Scope is exact for modules in that loaded set: an unimported loaded
name cannot be rescued by spelling its canonical qualifier. Interfaces for
unloaded modules are unavailable, so the open-inventory policy still retains
genuinely unknown names as external. A positive import list supplies a finite
canonical surface for such a module. An unrestricted import remains open, and
a `hiding` import remains open except for its exact excluded occurrences.
Those routes stay separate when aliases coincide, so an exact loaded import
cannot accidentally close an unrelated external one.

Package-qualified imports are rejected by every source-loader entry point,
even when unused. The neutral `Name` identity records a module and occurrence,
not a Cabal package key, so accepting `import "one" M` would either conflate it
with a supplied source module `M` or silently lose the qualifier through the
open-world fallback. Rejecting the syntax before scope projection keeps file,
directory, in-memory, compatibility, and REPL loading on one fail-closed rule.

Source loading keeps exported and private declarations together in the
authoritative inventory for whole-environment kind, synonym, recursion, and
class checks. Export lists restrict what another module may import; they do not
discard private facts. A later `*MODULE` prompt entry can therefore request a
loaded module's full top-level scope without rebuilding the inventory.

Successful extraction erases HSE annotations, so ordering metadata is captured
before that boundary. Ordinary signatures, datatype batches, and nested class
methods retain small module-local source slots; multi-name signatures stay in
one batch. The loader performs one stable merge of those tagged results. It
does not concatenate extractor categories or replay syntax to recover their
cardinality, and failures use the same ordering path as successful bindings.

Session policy is intentionally asymmetric. Excluding a name that the prepared
environment does not contain is a harmless no-op: an exclusion can only remove
a capability. A rating override promises to affect search, so non-finite
ratings and names unavailable after exclusions and capability filtering are
fatal policy errors. Neither operation mutates or reorders the authoritative
annotation-erased inventory.

## Shared REPL source workspace

The shared REPL adds an immutable workspace owner in front of the Exference
source boundary. It keeps three values separate:

| State | Owner and invariant |
| --- | --- |
| Explicit targets | Canonical module/file/directory admissions in user order; changed by `:load`, `:add`, and `:unadd`. |
| Loaded modules | Parsed local dependency closure in deterministic dependency-first order; rebuilt by every target operation and `:reload`. |
| Prompt context | Ordered imports and plain/starred source-scope entries projected over loaded modules; changed by bare `import` and `:module`. |

Module-name targets are resolved through hierarchical `.hs`/`.lhs` paths.
File targets may declare an unrelated module name. A directory target is the
legacy environment compatibility form: it recursively contributes source and
`.ratings` files while remaining one explicit target. Resolvable local imports
are discovered transitively by the workspace before the source loader
elaborates each module under those imports. The loader's import resolver itself
never expands the dependency closure. The workspace does not consult a GHC
package database or load interface/object code for synthesis. `:eval` is an
explicit parallel boundary that hands the retained source paths to real GHC;
package and compiled-module names visible there are never added to the neutral
inventory.

The package commands do not weaken this source-only invariant. `download` asks
Cabal to populate its source cache, while `install` builds executable
components or, with `--lib`, a library into Cabal's own store and environment;
neither adds a workspace target. A caller that wants package vocabulary in
synthesis must separately load compatible source or signature stubs. Cabal
targets include repository selectors, local sources, and archive URLs. Package
builds are arbitrary-code execution and remain an explicit frontend action,
never an implicit consequence of parsing an import or query.

Every filesystem target is canonicalized at admission, so a later `:cd` cannot
retarget `:reload`. The workspace parses the whole candidate graph, rejects
duplicate modules, dependency files whose declaration disagrees with the
imported name, and non-`SOURCE` cycles, orders the closure, elaborates and seals
one Exference session, computes export-aware prompt scope, and publishes the
replacement only after every phase succeeds. Source imports are fixed inputs
to declaration elaboration. Bare interactive imports and `:module` instead
change the later prompt projection; they neither reinterpret declarations nor
alter the dependency graph, and use the same transactional publication
discipline.

The raw file and in-memory source loaders repeat the non-`SOURCE` cycle
preflight because their callers supply an already discovered closure. Both
layers use the same stable dependency-order engine; only the surrounding path
discovery and diagnostics differ. This makes the export-surface fixed point an
acyclic calculation instead of returning a parity-dependent intermediate state.

The REPL then parses Exference types with `ExferenceQueryScope`: exact visible
names govern unqualified lookup, one full-top-level current module gets local
precedence, aliases map prompt qualifiers to canonical modules, and canonical
qualified lookup still consults the complete sealed inventory. The searchable
Exference environment is narrowed to the visible binding projection. The same
prompt scope retains a type/value namespace set per canonical identity and per
written qualifier. Type parsing and kind inspection receive the type surface;
expression typing and search receive the value surface; Djinn receives both as
separate visibility sets. This preserves `type`/`pattern` import items and
namespace-specific `hiding` even when a datatype and constructor share one
neutral `Name`. The scope then drives a checked Djinn declaration projection.
The prepared Exference session supplies the exact recursive datatype heads from
its alias-expanded deconstructor metadata; Djinn translates those canonical
identities through the prompt renaming rather than reclassifying shaped source
declarations. A visible recursive datatype therefore retains its constructors
for bounded positive introduction but is not considered structurally
eliminable. Constructor-hidden datatypes and declarations requiring a later
repair are instead degraded to abstract types. Every abstract projection reuses
the exact ground kind inferred by the shared inventory; arity-derived kinds are
only the fallback for genuinely absent external names. Record selectors are
axioms only when their parent cannot be eliminated structurally, including a
recursive record, and remain available under either Djinn axiom policy. For an
eliminable record, presentation may replace the structural projection with a
selector spelling only when that selector is visible unqualified, so hidden
names never leak into output. Every unsupported or hidden declaration is
recorded as an omission.

Djinn's standard checked session is the startup recovery state when no source
projection is available. After startup, workspace replacement, prompt-scope
edits, and projection-affecting settings publish only when both backend
sessions have been checked; a Djinn projection failure retains the complete
preceding state. The scope projection tracks type and value claims separately
while resolving source spellings, then keeps type-constructor and class
requirements distinct while repairing the closed Djinn inventory. A
same-named value therefore cannot masquerade as a referenced type even though
the shared structural `Name` is namespace-neutral. Real-GHC evaluation
consumes the same ordered prompt entries but has no access to either backend's
synthesis dictionary.

## API and stability tiers

The package is marked experimental. The tiers below describe intended use and
invariant strength; they are not a promise that the current date-versioned API
will never change.

### Curated checked API

New code should start with:

- `Language.Haskell.Djex` for the shared vocabulary, backend metadata, and both
  checked parser-neutral adapters;
- `Language.Haskell.Djex.REPL` when an application wants to launch the shared
  terminal session without spawning `djex`;
- `Language.Haskell.Djex.Djinn` or
  `Language.Haskell.Djex.Exference` when a narrower import is clearer;
- `Language.Haskell.Djex.Exference.HaskellSrc` when loading Haskell source or
  parsing a Haskell type against an Exference session;
- `Language.Haskell.Synthesis.*` for a focused import of one shared abstraction.

In particular, `Language.Haskell.Djex.Djinn` is the curated checked Djinn
facade. `Djinn.Core` belongs to the compatibility/research tier below; its
historical types and mutable environment operations are not the neutral stable
session boundary. Both checked backend sessions are immutable projections of a
neutral `Environment`: applications replace declarations by sealing a newly
validated environment. Only the historical `djinn` frontend imports its
private raw declaration snapshot, edit, and instance-method operations.

`Language.Haskell.Djex.CLI` is the in-process dispatcher for both the REPL and
the explicit one-shot subcommands. `Language.Haskell.Djex.REPL` exposes the
narrower interactive launcher and its startup options. Both are exposed so
applications can invoke a frontend without spawning a process, but neither is
re-exported by the main facade. They remain effectful terminal frontends: the
dispatcher returns an `ExitCode` instead of terminating its host, but may use
process streams, read trusted startup files and sources, change the working
directory, evaluate Haskell, or launch editor, shell, and Cabal children.

### Public compatibility and research API

The single library also exposes the historical `Djinn`, `Djinn.Core`, and
`Language.Haskell.Exference.*` surfaces. The specifically retained
`Language.Haskell.Exference.Core.Internal.Scope` research API predates the
merge; its `.Internal.` spelling is intentional, and new clients should not
treat its representation as the curated stability boundary. Other Exference
`Internal` modules are private implementation, not historical compatibility
promises.

Module exposure, not the spelling alone, determines whether a module is
importable. The `Exposed-Modules` section of `djex.cabal` is the exact public
compatibility inventory.

### Private implementation

Modules in Cabal's `Other-Modules` are implementation details even when their
filesystem path contains a historically public namespace. Examples include the
private checked-request/session owners for both adapters, the shared REPL's
command and Haskeline workers, and the historical Djinn formula and REPL
workers. Downstream code cannot import them through a `djex` dependency.

The converse also holds: a module named `.Internal.` may live under
`synthesis/src/` rather than `synthesis/internal/` when the shared source
tier itself consumes it — `Language.Haskell.Synthesis.Internal.InstanceHead`
sits beside its `Environment` consumer for exactly this reason. Directory
placement documents dependency direction; Cabal visibility, not the path,
decides whether a module is private.

## Test boundaries

The package has sixteen test suites:

- shared-foundation, facade integration, downstream API, and merged CLI suites;
- semantic Length, structural fingerprint, certificate, and term-graph
  fingerprint suites over the shared foundation;
- Djinn unit, property, frontend-import, and CLI suites;
- Exference unit, private-engine, frontend-import, and CLI suites.

`exference-engine-tests` compiles the parser-neutral Exference core and a
test-only seam as home modules. This preserves finite-identifier, queue
representation, saturation, and strictness regressions whose artificial
limits cannot be reached through production options, without exposing that
seam from the `djex` library.

The Djinn unit suite pins canonical bare/partial pair projection, wider-boxed
and unboxed rejection, and non-vacuous exact and contextual rank-N assignment
use through the public runner. The facade integration suite then carries the
same boxed-pair and partial-`Either` evidence through both engines, renders all
four definitions, and type-checks the combined fixture with GHC.

The downstream API suite also compiles deliberately rejected probes to protect
opaque-constructor boundaries. Benchmarks are separate Cabal components and are
not run by `cabal test all`.

Run the complete supported validation from the repository root:

```console
cabal build all
cabal test all -j1 --test-show-details=direct
cabal check
```

The serial whole-tree run is intentional: subprocess CLI suites consume the
freshly linked `djex`, `djinn`, and `exference` tools, so another component
must not replace one of those executables while a suite is using it. Focused
library-only test targets may still run concurrently.

Two practical notes about reading the result:

- `cabal test all` stops at the **first** failing suite and never reaches the
  ones after it. A single red suite therefore looks like a much larger outage
  than it is. When something fails, re-run the suites individually (or pass
  `--keep-going`) before drawing conclusions about scope.
- `synthesis-length-tests` is the only suite that is **not** expected to be
  fully green off Linux. Seven of its 366 cases exercise the descriptor-bound
  Z3 launch strategies, whose real implementation is Linux-only:

  | Case | What it wants |
  |---|---|
  | `fail closed before demanding unsupported-platform inputs` (×3, one per launch strategy) | the non-Linux stub to refuse *before* it asks for a workspace descriptor; the stubs currently ask first, so the assertion fires |
  | `bind sealed-launch scalar and pair identities under scoped authority` | a live descriptor-bound launch |
  | `bind effective-ID scalar and pair identities under scoped authority` | the same, via the effective-ID strategy |
  | `preserve scalar/pair wire bytes and statuses across sealed launches` | wire bytes from a real sealed launch |
  | `seal exact initial and conditional value writes` | the same launch path |

  On Linux the suite is 366/366. On Windows it is 359/366, and those seven are
  the baseline rather than a regression --- confirm against a pristine checkout
  before treating any of them as new. The refusals are still closed refusals;
  the disagreement is about *when* the stub gives up, not whether it does.

See [the library API guide](library-api.md) for runnable examples and
[the synthesis API map](../synthesis/README.md) for the shared modules. The
[2026-07-27 unification review](reports/2026-07-27-unification-review.md)
records the latest comparative findings and REPL audit. The earlier
[post-merge code review](reports/2026-07-21-post-merge-code-review.md),
[final convergence review](reports/2026-07-17-final-convergence-review.md) and
[checker-boundary follow-up](reports/2026-07-17-checker-boundary-follow-up.md)
record the preceding strictness, compatibility, and raw-checker changes.
