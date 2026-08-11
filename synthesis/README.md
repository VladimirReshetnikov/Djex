# Shared synthesis foundation

`synthesis/src` is the parser- and backend-independent layer of the single
`djex` library. It defines the source vocabulary, checked environment and kind
authorities, query/result protocol, generated-code grammar, and presentation
policies shared by Djinn and Exference.

It is not an independent Cabal package. Historical dependencies on
`haskell-synthesis` or `djex:synthesis` migrate to the unnamed `djex` library.

## Data flow

```text
Name + Type + Constraint + Declaration
                  |
                  v
             Environment
                  |
                  v
              Inventory
                  |
                  v
          PreparedInventory
                  |
          +-------+-------+
          |               |
          v               v
      Djinn plan     Exference plan
          |               |
          +-------+-------+
                  v
     QueryResult (SearchBatch Candidate)
                  |
          +-------+-------+
          |               |
          v               v
   checked TermGraph   compatibility
     when retained      projection
          |               |
          +-------+-------+
                  v
       FunctionClause / Expression
```

Each arrow is a checked or deliberately opaque boundary. Backend caches are
derived projections of the shared authorities, not alternative source models.

## Module map

### Source vocabulary

| Module | Responsibility |
| --- | --- |
| `Language.Haskell.Synthesis.Name` | Validated qualified identifiers, operators, and structural built-ins such as functions, lists, and tuples. Tuple constructors follow GHC's supported zero and 2-through-64 arities. |
| `Language.Haskell.Synthesis.Qualification` | The shared unqualified, identifier-qualified, and fully qualified name-emission policy used by terms, types, constraints, and HSE compatibility output. |
| `Language.Haskell.Synthesis.Kind` | Proper types, kind variables, and kind arrows. |
| `Language.Haskell.Synthesis.Constraint` | Nominal class names applied to backend-neutral type arguments, with shared namespace validation and rendering. |
| `Language.Haskell.Synthesis.Type` | The common variable/constructor/application/function/tuple/forall tree; canonicalization, validation, free-variable and binder queries, spines, and capture-avoiding substitution. |
| `Language.Haskell.Synthesis.TypeAtom` | Opaque rank-N atoms with retained source structure, cached lexical alpha-normal identity, free-variable queries, and checked capture-avoiding mapping and substitution. |
| `Language.Haskell.Synthesis.TypeRender` | Haskell-like rendering of the shared type tree and constraints with caller-supplied variable spellings and optional shared qualification policy. The original entry points remain fully qualified. |
| `Language.Haskell.Synthesis.Declaration` | Synonym, datatype, abstract type, value, class, and instance declarations plus declaration-local validation and recursion analysis. |

`Type` is the native source tree used by both checked adapters. Exference's
historical `HsType` and constraint names are compatibility aliases and patterns;
Djinn converts its historical raw syntax at its compatibility boundary.
`TypeAtom` is the common operational boundary for a non-vacuous explicit
`forall` reached below the prenex query prefix. Its key records binders by
lexical scope and position, not by source spelling, while free variables remain
nominal. Consequently Church encodings such as
`forall result. (item -> result -> result) -> result -> result` compare equal
after a consistent binder rename, including when stored inside a list.
`TypeAtom` itself defines no typing rule: Djinn and Exference may reopen the
retained tree only at their documented positive-introduction and scoped-use
elimination boundaries. The original type tree is retained separately so
rendering and diagnostics do not expose the normalized key.

### Checked environments and elaboration

| Module | Responsibility |
| --- | --- |
| `Language.Haskell.Synthesis.Environment` | Seals declaration order, cross-declaration namespaces, and type/value/constructor/class/instance indexes. |
| `Language.Haskell.Synthesis.KindInference` | Kind checking and inference for types and whole environments, including canonical residual kinds for inspection, open/closed inventories, and class-kind finalization policy. |
| `Language.Haskell.Synthesis.Inventory` | Pairs one grounded `Environment` with the kind assumptions inferred from it. |
| `Language.Haskell.Synthesis.TypeSynonym` | Prepares exact synonym tables, rejects repeated raw parameters when reached, checks saturation, expands capture-safely, performs pre/post-expansion kind checks, and offers the deliberately outer-head-lenient normalization used by `:kind!`. |
| `Language.Haskell.Synthesis.Class` | Provides an opaque source-order view of declared classes, final parameter kinds, methods, and explicit instances from an `Inventory`. |

`PreparedInventory` is the long-lived session authority: it keeps an Inventory
and its exact normalized synonym table inseparable. The transient
`PreparedInventoryExpansion` additionally supplies an alias-free operational
declaration stream and datatype-recursion set while a backend is being sealed;
backends do not retain that expanded declaration copy afterward.

`inferTypeKind` shares the normal preflight, validation, assumptions, and
unifier but does not apply the Haskell-98 `Type` default to a residual kind
variable. Its `InferredKind` variables are renumbered from zero in structural
first-occurrence order, so callers never observe private inference allocation
tokens. `normalizePreparedTypeSynonyms` is intentionally separate from kind
checking: it expands saturated aliases and permits only the complete input's
operational synonym head beneath context-free prenex foralls to remain
undersaturated. Interactive callers therefore check before normalization and
may defensively check the result again.

`checkClassApplicationKinds` validates a possibly partial class application
and returns the unapplied parameter-kind suffix. All supplied arguments share
one inference scope, including generalized parameters, so repeated source
variables cannot acquire incompatible kinds merely because they occur in
different class arguments.

Shared class values describe source facts. Djinn and Exference intentionally
retain different resolution and search policies.

### Query, search, and presentation

| Module | Responsibility |
| --- | --- |
| `Language.Haskell.Synthesis.Query` | `QueryRequest`, source provenance, opaque `CachedQuery`, logical evidence, and the checked `QueryResult` envelope. |
| `Language.Haskell.Synthesis.Search` | Continuing/finished/truncated progress, exact truncation reasons, and candidate batches. |
| `Language.Haskell.Synthesis.Candidate` | Generated output with residual constraints and backend-owned details, plus common candidate renderers. |
| `Language.Haskell.Synthesis.Selection` | First, best, lookahead-best, all, and preferred-tier policies over lazy result batches. |
| `Language.Haskell.Synthesis.Diagnostic` | Severity, stable code, checked source locations/spans, ordered context, and deterministic rendering. |
| `Language.Haskell.Synthesis.Fingerprint` | Public inspection of opaque, nominal, collision-free canonical identities; construction and byte budgets remain package-private. |
| `Language.Haskell.Synthesis.Semantic.Length` | Exact inventory-bound finite-spine contexts, normalized contracts, source-bound assumed provider laws, and model-aware fingerprints. |
| `Language.Haskell.Synthesis.Semantic.Length.Evaluate` | Bounded deterministic replay of checked contracts and provider transfers for one concrete natural-number assignment; results carry no evidence authority. |
| `Language.Haskell.Synthesis.Semantic.Observation` | Raw three-valued solver and four-valued behavioral reports without candidate association or evidence claims. |
| `Language.Haskell.Synthesis.Semantic.Problem` | Bounded raw artifacts associated with exact domain, inventory, encoding, candidate, and problem identities; every raw result is restricted to heuristic ranking, while authoritative evidence has only a future private construction seam. |
| `Language.Haskell.Synthesis.Generated` | Scope-aware expressions, patterns, clauses, holes, mixed term/type application spines, bottom-up rewriting, simplification, alpha-equivalence, substitution, and Haskell rendering through the common qualification policy. |
| `Language.Haskell.Synthesis.TypedGenerated` | Bounded typed candidate graphs with stable node, source-occurrence, and certificate identities; checked application and visible-specialization witnesses; a neutral sealing pass; exact graph metrics; and one-way projection to `Generated`. |
| `Language.Haskell.Synthesis.Observability` | Opaque exact counters, stable cross-engine metric codes, deterministic aggregation, and deliberately non-strict snapshots that can be inspected independently of a lazy result. |

Logical evidence is independent of operational progress. A truncated search can
return validated candidates; a finished heuristic search can return no logical
conclusion. Candidate selection is likewise a frontend policy and does not
change backend search semantics.

Behavioral observations remain independent of logical evidence too.  A domain
module constructs all four structural fingerprints before sealing a
`BehavioralProblem`; raw artifact format and payload bytes must fit explicit
limits before a solver or behavioral report can be associated with that exact
tuple. Replay compares domain, inventory, encoding, candidate, and complete
problem in that order and rejects the first mismatch. Even an `unsat` report is
only relative to its encoding and grants `HeuristicRankingOnly`. The opaque
`BehavioralEvidence` seam has no public constructor or raw-report conversion;
a later authoritative checker must bind its own receipt to the same identities,
and a public consumer can recover that receipt only through successful replay.

`Generated` separates backend-local identity from structural global `Name`s.
Its checked `DefinitionName` narrows top-level output names once, and its scope
checker and renderer prevent free locals, binder reuse, capture, and ambiguous
qualification from becoming generated Haskell. Structural consumers can use
`expressionFullApplicationSpine` without losing visible type arguments, and
`rewriteExpressionBottomUp` or `rewriteExpressionBottomUpM` to cover every
expression constructor in one postorder pass.

`TypedGenerated` is the richer producer/consumer edge. A raw graph is not
evidence: `sealTermGraph` first bounds every node, edge, pattern, collection,
type annotation, and compatibility projection; rejects duplicate, dangling,
cyclic, unreachable, or multiply referenced nodes; checks local scope and the
neutral typing relationships; and stores the single checked `Generated`
projection beside the graph. Stable occurrence identities distinguish equal
uses and must never be recovered from names or traversal paths. The existing
candidate and query constructors remain unchanged while engines migrate, so a
backend must report typed-view absence explicitly rather than inventing an
annotation after erasure.

`Observability` is orthogonal to logical evidence and search progress. Its
`Natural` counts cannot wrap, zero-valued entries have one canonical absent
representation, mapped collisions sum exactly, and entries are inspected in
key order. Both fields of an `ObservationSnapshot` are intentionally lazy:
reading cumulative observations must not force a candidate stream, and reading
the result must not perform diagnostic accounting.

### Small shared utilities

| Module | Responsibility |
| --- | --- |
| `Language.Haskell.Synthesis.Collection` | Stable distinctness, duplicate summaries, optional observations, and finite transitive closure. |
| `Language.Haskell.Synthesis.Count` | Exact `Natural` counts and explicit saturation at historical `Int` boundaries. |
| `Language.Haskell.Synthesis.Fresh` | Deterministic collision-skipping selection and allocation for total or exhaustible generators and caller-owned reservation stores. |

The private `Language.Haskell.Synthesis.Internal.InstanceHead` module owns the
canonical, alpha-normal comparison key used to detect duplicate instance
heads. Saturated function and tuple applications share that key with their
structural forms, while public indexes and diagnostics retain the original
source head rather than exposing or substituting the private key.

## Invariant conventions

- Opaque authorities do not derive `Generic` when `GHC.Generics.to` could forge
  a checked representation.
- Invariant-bearing observations are ordinary functions rather than record
  fields when record update could bypass a smart constructor.
- Source order is explicit for diagnostics, declaration lowering, constraints,
  candidates, and duplicate reporting.
- Counts and freshness tokens are arbitrary precision until an established
  compatibility API requires a bounded projection.
- Parser locations are converted once to checked one-based, half-open shared
  spans; semantic requests do not carry parser ASTs.
- Finite indexes and left-associated application spines use strict
  accumulation; strict product accumulators force each index while preserving
  intentionally lazy payload values.
- Laziness is part of the search contract: constructors and selectors avoid
  forcing uninspected result batches, candidate payloads, or candidate tails.

## Build and test

Run foundation validation from the repository root:

```console
cabal build djex:lib:djex
cabal test synthesis-tests --test-show-details=direct
```

The complete package integration matrix is:

```console
cabal test all --test-show-details=direct
```

See [the architecture guide](../docs/architecture.md) for dependency and
stability boundaries and [the library guide](../docs/library-api.md) for
runnable checked-adapter examples.
