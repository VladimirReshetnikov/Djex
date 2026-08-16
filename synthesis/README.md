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
| `Language.Haskell.Synthesis.TypeInstantiation` | One-way, capture-safe matching from a context-free leading-forall scheme to an actual type, with opaque source-order selections and exact free-variable observations for later policy checks. |
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

`TypeInstantiation` fills the narrower shared boundary needed after a backend
has retained an implicitly instantiated typed global. It solves only the
source scheme's complete leading binder prefix and treats every actual-side
variable as a constant. A binder may select a whole closed impredicative type;
nested `forall` binders are paired with private skolems before their bodies are
compared, so an outer selection cannot capture a variable that exists only
inside the nested scope. The resulting selections are opaque: domain modules
may inspect their exact free-variable set or recognize one selected variable,
but cannot manufacture a successful match.

### Checked environments and elaboration

| Module | Responsibility |
| --- | --- |
| `Language.Haskell.Synthesis.Environment` | Seals declaration order, cross-declaration namespaces, and type/value/constructor/class/instance indexes. |
| `Language.Haskell.Synthesis.KindInference` | Kind checking and inference for types and whole environments, including canonical residual kinds for inspection, open/closed inventories, and class-kind finalization policy. |
| `Language.Haskell.Synthesis.Inventory` | Pairs one grounded `Environment` with the kind assumptions inferred from it. |
| `Language.Haskell.Synthesis.TypeSynonym` | Prepares exact synonym tables, rejects repeated raw parameters when reached, checks saturation, expands capture-safely, performs pre/post-expansion kind checks, and offers the deliberately outer-head-lenient normalization used by `:kind!`. |
| `Language.Haskell.Synthesis.Class` | Provides an opaque source-order view of declared classes, final parameter kinds, methods, and explicit instances from an `Inventory`. |
| `Language.Haskell.Synthesis.Internal.ClassResolution` | Package-private bounded declared-class resolution over one exact checked `Inventory`, with alias-free first-order ground discharge, direct-superclass completion, overlap and termination admission, and environment-plus-goal replay receipts. |

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

Shared class values describe source facts. The package-private
`Internal.ClassResolution` module now adds one executable policy without
changing that neutral representation. It reads the raw, unexpanded declaration
view from one already-checked `Inventory`, closes authority to classes declared
there, and admits only normalized, alias-free, first-order constraints. Every
referenced type synonym and every nested `ForallType` is rejected explicitly;
queries must be ground, and there is no given-constraint context.

Sealing completes each explicit instance with its owner's direct
superclasses, rejects superclass cycles and overlapping instance heads, and
checks non-expansion for every explicit or completed prerequisite that its head
can ground. Symmetric overlap checking, directional runtime matching, and the
Paterson-style size/occurrence measure share the same canonical applicative
function/tuple kernel. Resolution is source ordered, tracks current-path
cycles, and returns an opaque proof receipt only for a successful discharge.
Replay first requires a structurally equal retained checked environment,
including its limits and source-ordered facts, and then the same canonical
goal; a detached proof tree is diagnostic, not transferable authority.

The package-private heterogeneous query entrance performs that same bounded
validation before changing namespaces. It rejects every free variable and
nested `ForallType` in the caller's domain, then structurally retypes only the
variable-free constraint into the checked environment's domain. No coercion,
sentinel identity, given, or external proof participates.

Independent limits cover the recovered declarations accepted for retention,
retained class and instance tables, retained type-constructor-kind table,
declaration/type collection widths, type and kind nodes, overlap comparisons,
proof depth, and proof nodes. They bound accepted structural shapes and proof
exploration, not identifier bytes or every normalization step. Source order is
recovered in full from an `Inventory` which has already been checked before the
declaration cap is observed, so this is not a new streaming work bound for
constructing or projecting an inventory from an unbounded raw declaration
source.

Djinn and Exference do not consume this resolver directly, so their backend
resolution and search policies remain unchanged.  A Length session containing
a conditional provider law instead attempts to co-seal the resolver from its
exact inventory.  Its candidate boundary may bind successful static ground
discharge receipts only to the final occurrence in the same associated
certificate row; a graph fingerprint or certificate alone grants no discharge
authority.  A compatibility session may retain an unavailable resolver and
then fails closed with a sanitized candidate diagnostic when discharge is
needed.  Z3 is never a source of dictionary evidence.  The foundation and its
narrow Length consumer are recorded in the
[checked class-resolution report](../docs/reports/2026-08-13-checked-class-resolution-foundation.md)
and the
[ground constraint-discharge report](../docs/reports/2026-08-13-length-ground-constraint-discharge.md).

### Query, search, and presentation

| Module | Responsibility |
| --- | --- |
| `Language.Haskell.Synthesis.Query` | `QueryRequest`, source provenance, opaque `CachedQuery`, logical evidence, and the checked `QueryResult` envelope. |
| `Language.Haskell.Synthesis.Search` | Continuing/finished/truncated progress, exact truncation reasons, and candidate batches. |
| `Language.Haskell.Synthesis.Candidate` | Generated output with residual constraints and backend-owned details, plus common candidate renderers. |
| `Language.Haskell.Synthesis.Selection` | First, best, lookahead-best, all, and preferred-tier policies over lazy result batches. |
| `Language.Haskell.Synthesis.Diagnostic` | Severity, stable code, checked source locations/spans, ordered context, and deterministic rendering. |
| `Language.Haskell.Synthesis.Fingerprint` | Public inspection of opaque, nominal, collision-free canonical identities; construction and byte budgets remain package-private. |
| `Language.Haskell.Synthesis.Semantic.Length` | Exact inventory-bound finite-spine contexts, normalized scalar and exact boxed binary-product contracts, source-bound assumed provider laws, and model-aware nominal fingerprints; an additive trust class retains exact nonempty constrained schemes under a dictionary-uniform conditional-law assumption, while every scalar byte stays exact. |
| `Language.Haskell.Synthesis.Semantic.Length.Evaluate` | Bounded deterministic replay of checked scalar and binary-spine-product contracts, exact candidate problems, finite Cartesian input boxes, and the one current guarded recursive piecewise-affine finite-union applicable-domain analysis, with all-or-nothing `LengthIf` guard/arm support, ordered private relational/strict/quotient/extrema/monus/Boolean/atomic fallbacks, explicit box antichains, global original-problem replay, and nominally separate counterexample and positive receipts; detached conditional-provider evaluation still fails before argument inspection. |
| `Language.Haskell.Synthesis.Semantic.Length.Problem` | Atomic checked sessions and nominally distinct scalar/product typed-candidate behavioral problems: session-owned provider and restricted resolver authority, contract resealing, residual rejection, rigid root/provider authorization, mixed-role opaque targets, exact scalar zero/step cases inside product fields, and provider-only consumption of independently authorized certificate carriers. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib` | Bounded canonical QF_LIA translation plus exact input-symbol, natural-input, origin, finite-box, and current guarded recursive piecewise-affine applicable-domain replay for nominally distinct checked scalar and binary-product Length problems; the short applicable-domain wrappers only validate and associate evidence, without launching or trusting a solver. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution` | Pure validated Z3 launch, resource, artifact, and response-decoder policy with a package-private complete identity and a byte-free digest-expectation presence classifier; it performs no IO or attestation. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live` | Rank-N scoped capability-probed Z3 ownership for scalar and exact binary-product queries, with one shared 64-query lease budget, retained runtime-unscoped v1 usable-work tokens, recommended owner-thread-affine dynamically scoped v2 deadlines and cooperative checkpoints, nominal byte-free failures and observations, heuristic status/strength/use, and domain-specific query-first replay gates. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation` | Opaque query-specific association of bounded raw solver reports, with heuristic-only safe projections and exact problem-plus-query replay before payload access. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response` | Pure bounded SMT-LIB 2.x check-status and scalar/product query-specific input-valuation decoding; syntax remains untrusted and only independent domain replay may create evidence. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol` | Package-private incremental reset/check/value transaction planning with exact positional barriers, causal write boundaries, and bounded cumulative stdout; it performs no IO or attestation. It hosts the one phase machine shared by both domains, parameterized by a `LengthSMTLibProtocolIdentity` record of domain tags, fingerprint fields, and query projections, plus the scalar identity itself. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol.SpinePair` | Thin nominal product wrapper over the shared phase machine: the product schema tags, fingerprint role, capability-reuse fields, and query projections, with plan, receiver, and decoded outcomes kept nominally distinct through the product query and fingerprint subject. The barrier, phase, write-kind, and error vocabularies are deliberately the shared ones. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Capability` | Package-private four-write readiness probe for print suppression, reset replay, exact `sat`, input valuation, contradictory `unsat`, and positional fresh barriers. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process` | Length compatibility and identity facade which exhaustively maps the shared runtime's sanitized vocabulary and derives the unchanged raw-process v2 root from its exact associated observation and process-owned limits without caching a parallel root. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Causal` | Domain-neutral pure write/await/complete action algebra with nominal write-kind, receiver, and outcome associations. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Causal.BoundaryWhitespace` | Opaque content proof for finite strict SMT-LIB boundary whitespace, with lexical admission and safe FIFO-chunk concatenation but no transport-origin or schema authority. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Causal.StdoutChunk` | Opaque proof that one strict causal-transport stdout chunk is nonempty, without FIFO-origin, configured-bound, process-association, or schema authority. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream` | Domain-neutral cumulative framing policy and opaque zero-start cursor, completed-frame, and validated-boundary continuations; it prevents tails, offsets, and budgets from being detached across same-write frames or causal write boundaries. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver` | Domain-neutral causal transport algorithm and exact segmented transcript ownership, with write-before-feed, positional EOF precedence, and delayed-boundary-whitespace attribution. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Lexical` | Schema-free owner of the exact SMT-LIB whitespace predicate, the canonical `[HT, LF, CR, SP]` fingerprint order, and the shared bare-delimiter, printable, string-character, and quoted-symbol-character byte classes consumed by parsing, framing, causal accounting, boundary draining, and domain plans. |
| `Language.Haskell.Synthesis.Internal.SMTLib.QFLIA` | Domain-neutral typed QF_LIA integer, Boolean, and command syntax with one exact logic spelling, canonical rendering, and matching structural fingerprint-field projections; it owns no domain translation, naming, limits, solver, or replay policy. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Response` | Domain-neutral bounded SMT-LIB response lexer and S-expression parser with productive total, depth, node, token, and numeral limits. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Response.Standard` | Canonical solver-status bytes, bounded standard check-response decoding, and shared `unsupported`/solver-error classification without process or semantic authority. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution` | Domain-neutral admitted Z3 launch profile, mechanical startup/reset facts, configured argv, and flat launch fingerprint-field slice without process or domain-schema authority. |
| `Language.Haskell.Synthesis.Internal.SMTLib.Z3.Process` | Domain-neutral opaque direct-process runtime owning pre-spawn observation, masked resource-result publication, rollback-protected descriptor-child ownership handoff, pipes, FIFO events, cancellation/deadlines, and bounded cleanup while exposing only associated schema-free observation and limit fields to a domain facade. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Transport` | Length/Z3 adapter which binds one process, cancellation token, and absolute deadline behind the generic causal driver operations. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session` | Rank-N scoped ownership of one capability-probed common-QF_LIA worker and one shared 64-entry scalar/product ordinal budget, with optional v1 generative or v2 owner-thread-affine dynamically scoped shared absolute deadlines spanning opening and query work, secret/public entropy separation, a fresh fd-observed workspace, exact segmented probe transcript, nominal domain runs, and no public process handle. |
| `Language.Haskell.Synthesis.Semantic.Observation` | Raw three-valued solver and four-valued behavioral reports without candidate association or evidence claims. |
| `Language.Haskell.Synthesis.Semantic.Problem` | Bounded raw artifacts associated with exact domain, inventory, encoding, candidate, and problem identities; every raw result is restricted to heuristic ranking, while domain-owned authoritative evidence has only a private construction seam. |
| `Language.Haskell.Synthesis.Generated` | Scope-aware expressions, patterns, clauses, holes, mixed term/type application spines, bottom-up rewriting, simplification, alpha-equivalence, substitution, and Haskell rendering through the common qualification policy. |
| `Language.Haskell.Synthesis.TypedCandidate` | Opaque engine-checked compatibility/graph associations with a package-private lazy unavailable/plain/certificate-carrier representation, nominal authority domains, unchanged legacy `Eq`/`Ord`/`Show` and public projections, deep carrier `NFData`, and one evidence/progress/metadata-preserving `QueryResult` compatibility projection shared by both engines. |
| `Language.Haskell.Synthesis.TypedGenerated` | Bounded typed candidate graphs with stable node, source-occurrence, and certificate identities; checked application and visible-specialization witnesses; a neutral sealing pass; nominal type/local authority even after graph projection; exact graph metrics; and one-way projection to `Generated`. |
| `Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate` | Package-private bounded structural plans for complete leading-telescope selections: capture-free canonical substitution steps and derived ordered scheme-syntax obligations, without provenance, closure, kind, discharge, graph association, or fingerprint authority. |
| `Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association` | Package-private atomic raw-graph sealing plus exhaustive one-use association of independent checker observations, structural plans, exact global owner schemes, and derived base/visible occurrence chains; its projection and fold grant no detachable carrier, inventory, kind, discharge, behavioral, or fingerprint authority. |
| `Language.Haskell.Synthesis.TypedGenerated.Fingerprint` | Bounded, allocation- and alpha-insensitive structural identities after a fresh reseal; the public shared entrance rejects certificate- and constructor-schema-dependent graphs, while package-private entrances can consume an opaque checked schema or certificate-association carrier without exporting either as graph authority. |
| `Language.Haskell.Synthesis.Observability` | Opaque exact counters, stable cross-engine metric codes, deterministic aggregation, and deliberately non-strict snapshots that can be inspected independently of a lazy result. |

Logical evidence is independent of operational progress. A truncated search can
return validated candidates; a finished heuristic search can return no logical
conclusion. Candidate selection is likewise a frontend policy and does not
change backend search semantics.

Behavioral observations remain independent of logical evidence too.  A domain
module constructs all four structural fingerprints before sealing a
`BehavioralProblem`; raw artifact format and payload bytes must fit explicit
limits before a solver or behavioral report can be associated with that exact
tuple. Associated observations and domain-owned evidence retain that same
opaque problem directly, so there is no parallel fingerprint-tuple
representation which can drift from the problem authority. Replay compares
domain, inventory, encoding, candidate, and complete problem in that order and
rejects the first mismatch. Even an `unsat` report is
only relative to its encoding and grants `HeuristicRankingOnly`. The opaque
`BehavioralEvidence` seam has no public constructor or raw-report conversion.
Domain-owned checkers may bind independently replayed receipts to the same
identities, and a public consumer can recover a receipt only through successful
replay against the exact problem.

The complete module-by-module narrative of the Length contract, its SMT-LIB
translation, and the Z3 live stack — schema versions, sealing order, replay
gates, framing, protocol, session, and process ownership — lives in the
[semantic foundations reference](../docs/semantic-foundations.md#length-module-narrative);
this map keeps only the table above and the invariants below.

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
