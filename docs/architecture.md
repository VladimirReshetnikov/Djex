# Djex architecture

Djex is one Cabal package with one library and three executables. The library
contains a shared parser-independent synthesis foundation, two deliberately
different search engines, checked adapters for both engines, and the historical
Djinn and Exference compatibility frontends.

This guide describes the current architecture. The dated files under
`docs/reports/` record how the repository reached it and should be read as
historical review notes rather than as the current API guide.

## Source and component layout

| Path | Responsibility |
| --- | --- |
| `synthesis/src/` | Shared names, types, declarations, environments, kind inference, diagnostics, generated code, and query/result envelopes. |
| `djinn/src-core/` | Djinn's checked adapter, logical translation, LJT proof search, and proof checking. |
| `djinn/src-frontend/` | Historical `Djinn` API and Haskeline REPL. |
| `exference/src-core/` | Exference's checked adapter, class environment, heuristic search, unification, scoring, and independent expression checker. |
| `exference/src-frontend/` | Haskell-source extraction, environment loading, and the historical Exference command API. |
| `src/` | The `Language.Haskell.Djex` facade and merged `djex` command. |
| `app/`, `djinn/app/`, `exference/app/` | Thin executable launchers. |

All six source roots compile into the unnamed `djex` library. The directory
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
features, it rejects explicit module export lists, pattern-synonym signatures,
and XML page or hybrid modules rather than silently projecting only part of
them. `UnsupportedVocabularyForm` is the authoritative list. Ordinary term
patterns and pattern-value bodies are accepted, as are ordinary value/method
bodies and other syntax that does not alter the nominal inventory.

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

## API and stability tiers

The package is marked experimental. The tiers below describe intended use and
invariant strength; they are not a promise that the current date-versioned API
will never change.

### Curated checked API

New code should start with:

- `Language.Haskell.Djex` for the shared vocabulary, backend metadata, and both
  checked parser-neutral adapters;
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

`Language.Haskell.Djex.CLI` is an explicit in-process command frontend. It is
exposed so applications can invoke the merged command without spawning a
process, but it is not re-exported by the main facade.

### Public compatibility and research API

The single library also exposes the historical `Djinn`, `Djinn.Core`, and
`Language.Haskell.Exference.*` surfaces. Some exposed compatibility modules
contain `.Internal.` in their names because earlier packages made those
research APIs importable. They remain available for migration and testing, but
the name is intentional: new clients should not treat their representation as
the curated stability boundary.

Module exposure, not the spelling alone, determines whether a module is
importable. The `Exposed-Modules` section of `djex.cabal` is the exact public
compatibility inventory.

### Private implementation

Modules in Cabal's `Other-Modules` are implementation details even when their
filesystem path contains a historically public namespace. Examples include the
private checked-request/session owners for both adapters and Djinn's formula
and REPL workers. Downstream code cannot import them through a `djex`
dependency.

## Test boundaries

The package has eleven test suites:

- shared-foundation, facade integration, downstream API, and merged CLI suites;
- Djinn unit, property, frontend-import, and CLI suites;
- Exference unit, frontend-import, and CLI suites.

The downstream API suite also compiles deliberately rejected probes to protect
opaque-constructor boundaries. Benchmarks are separate Cabal components and are
not run by `cabal test all`.

Run the complete supported validation from the repository root:

```console
cabal build all
cabal test all --test-show-details=direct
cabal check
```

See [the library API guide](library-api.md) for runnable examples and
[the synthesis API map](../synthesis/README.md) for the shared modules. The
[final convergence review](reports/2026-07-17-final-convergence-review.md) and
[checker-boundary follow-up](reports/2026-07-17-checker-boundary-follow-up.md)
record the latest comparative code-review findings and retained differences.
