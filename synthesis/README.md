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
| `Language.Haskell.Synthesis.Semantic.Length.Evaluate` | Bounded deterministic replay of checked contracts, provider transfers, and exact candidate problems; only independently validated model-relative violations receive problem-bound evidence with an explicit provider-assumption basis. |
| `Language.Haskell.Synthesis.Semantic.Length.Problem` | Atomic checked sessions and typed-candidate behavioral problems: session-owned provider authority carried intact through interpretation, exact separately supplied contract resealing, residual rejection, rigid root/provider authorization, normalized counterexample formulas, and separate inventory/encoding/candidate/problem identities; the candidate identity structurally binds the transient shared graph key without retaining a parallel graph field. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib` | Bounded canonical QF_LIA translation and exact input-symbol model replay for one checked Length problem, without launching or trusting a solver. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution` | Pure validated Z3 launch, resource, artifact, and response-decoder policy with a package-private complete identity and a byte-free digest-expectation presence classifier; it performs no IO or attestation. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live` | Rank-N scoped capability-probed Z3 ownership with byte-free failures, heuristic status/strength/use, and a query-first replay gate for independently validated counterexample evidence. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation` | Opaque query-specific association of bounded raw solver reports, with heuristic-only safe projections and exact problem-plus-query replay before payload access. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response` | Pure bounded SMT-LIB 2.x check-status and query-specific input-valuation decoding; syntax remains untrusted and only independent Length replay may create evidence. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol` | Package-private incremental reset/check/value transaction planning with exact positional barriers, causal write boundaries, and bounded cumulative stdout; it performs no IO or attestation. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Capability` | Package-private four-write readiness probe for print suppression, reset replay, exact `sat`, input valuation, contradictory `unsat`, and positional fresh barriers. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session.Process` | Bounded direct-process owner with a pre-spawn executable-file snapshot, exact isolated launch, FIFO stdout framing support, first-byte stderr poison, cancellation/deadlines, and staged cleanup. |
| `Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session` | Rank-N scoped ownership of one capability-probed worker, with secret/public entropy separation, a fresh fd-observed workspace, exact segmented probe transcript, and no public process handle. |
| `Language.Haskell.Synthesis.Semantic.Observation` | Raw three-valued solver and four-valued behavioral reports without candidate association or evidence claims. |
| `Language.Haskell.Synthesis.Semantic.Problem` | Bounded raw artifacts associated with exact domain, inventory, encoding, candidate, and problem identities; every raw result is restricted to heuristic ranking, while domain-owned authoritative evidence has only a private construction seam. |
| `Language.Haskell.Synthesis.Generated` | Scope-aware expressions, patterns, clauses, holes, mixed term/type application spines, bottom-up rewriting, simplification, alpha-equivalence, substitution, and Haskell rendering through the common qualification policy. |
| `Language.Haskell.Synthesis.TypedCandidate` | Opaque engine-checked compatibility/graph associations, nominal authority domains, lazy per-candidate projections, and one evidence/progress/metadata-preserving `QueryResult` compatibility projection shared by both engines. |
| `Language.Haskell.Synthesis.TypedGenerated` | Bounded typed candidate graphs with stable node, source-occurrence, and certificate identities; checked application and visible-specialization witnesses; a neutral sealing pass; nominal type/local authority even after graph projection; exact graph metrics; and one-way projection to `Generated`. |
| `Language.Haskell.Synthesis.TypedGenerated.Fingerprint` | Bounded, allocation- and alpha-insensitive structural identities for shared typed graphs, after resealing with the shared type checker; certificate- and constructor-schema-dependent graphs fail closed until their semantic authorities exist. |
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

`Language.Haskell.Synthesis.Semantic.Length.SMTLib` provides a pure canonical
QF_LIA boundary over an exact checked length problem. Its opaque nominal query
contains bounded check and input-only `get-value` commands; it neither starts a
solver nor associates a raw solver status. Decoded input bindings can produce a
counterexample receipt only through independent replay against the retained
problem, while raw models and even `unsat` remain heuristic observations.
The complete typed SMT plan remains transient through bounded rendering and
structural fingerprinting. The sealed query retains only the checked problem,
canonical check bytes, and complete fingerprint. Exact decoder-symbol order and
optional `get-value` bytes are canonically rederived from the checked problem's
sealed arity after query sealing has already bounded and structurally
fingerprinted them; the unchanged structural plan field in that fingerprint
keeps rendered bytes from becoming the semantic source of truth.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation` can associate
such a bounded raw report with both the retained behavioral problem and the
exact query fingerprint. Its opaque value deliberately projects no artifact or
nested generic association before exact replay. Status, conservative strength,
and use remain inspectable, and every status is restricted to
`HeuristicRankingOnly`. Even after replay the report is still raw: a model must
pass independent Length validation, while `unsat` never becomes evidence.
Query identity is only the canonical semantic translation identity, not a run
or cache identity. A later executor must additionally seal the solver build,
capability handshake, process and protocol state, parser schema, requested
artifact policy, deadlines, cancellation, and resource limits; `unknown`
should not be cached by query identity alone.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response` is the matching
pure response boundary. It retains at most 65,536 bytes before parsing, so
cyclic or infinite lazy input is rejected productively, then enforces separate
list-depth, S-expression-node, source-token-byte, and integer-width limits. The
private lexer handles exact SMT-LIB whitespace and comments, doubled-quote
strings, quoted symbols, and all standard atom categories. The public surface
accepts only `sat`, `unsat`, `unknown`, or the exact input valuation requested
by one query; valuations are symbol-checked and restored to source order.
Its versioned schema tag gives a later execution identity an exact parser and
normalization policy to bind. This decoder is not stream framing or execution
association. Standard solver errors remain failures, `unsat` remains
heuristic, and decoded values must still pass the existing exact Length model
validator before any evidence exists.

`Language.Haskell.Synthesis.Internal.SMTLib.Stream` supplies the separate,
package-private framing primitive. It incrementally finds one exact top-level
SMT-LIB 2.7 response across arbitrary byte chunks, preserving internal
whitespace and comments and returning the original post-frame tail after at
most one byte of lexical lookahead. The
explicit state machine distinguishes doubled-quote strings, multiline quoted
symbols, CR/LF comments, bare atoms, and bounded list nesting. Total-byte,
retained-frame, and depth limits make leading trivia, cyclic chunks, and
hostile nesting productive failures. Bare and quoted-symbol responses require
the standard-mandated following whitespace instead of being accepted at EOF.
A fixed 32-byte nonce becomes a package-owned lowercase-hex `echo` marker;
both its command and its expected response include the surrounding quotes
required by the standard. Recognition is exact and positional only. Completed
frames report the exact count of charged leading trivia and frame bytes while
leaving lexical lookahead in the returned tail. This lets the composing
transaction account every accepted stdout byte without charging a tail twice.
A live capability probe must establish that Z3's `echo` emits and flushes a
trailing byte (normally a newline): a top-level string's closing quote is
intentionally held for one-byte lookahead so a quote at the next chunk boundary
can still form a doubled quote.

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol` owns the
next pure boundary. It seals the execution-policy key, exact query key, stream
limits, cumulative stdout budget, phase schema, and caller-supplied check/value
markers into one private protocol plan. The initial action writes reset,
canonical check commands, and a status marker together. The machine then
accepts exactly one decoded status followed by that exact marker. Only `sat`
under an input-value policy for a query with inputs exposes the separate
`get-value`/marker write; unsatisfiable, unknown, status-only, and zero-input
paths terminate without it. Satisfiable zero-input value policy is represented
as a vacuous `Just []`, without fabricating an input-value frame. A decoded
nonempty-query valuation is not released until its own exact marker arrives.
The plan does not cache the concatenated initial and optional value writes: it
renders the bytes transiently for the complete fingerprint and derives them
again on demand from the retained sealed query and positional markers through
the selectors used at their causal write edges. Presence-only inspection of
the optional write does not render request bytes.

Tails cross status-to-marker and value-to-marker phases only because those
responses answer commands already in the same completed write. A tail crossing
the status-marker-to-`get-value` boundary must contain only bounded SMT-LIB
whitespace; a stale buffered valuation is rejected before the new write action
exists. Framing, response, marker, cumulative-budget, unexpected-byte, and EOF
failures expose no continuation. The next live query driver must therefore
destroy the worker on any failure, generate barriers uniquely across the session,
perform each write before feeding its receiver, and bind the process snapshot
and observed transcript facts into a separate run identity. Pure decoded
outcomes remain caller-feedable syntax, never execution receipts or semantic
evidence.

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session` now owns
the first live layer. It samples 64 bytes of OS entropy and separates the
secret barrier seed from the public workspace label. On POSIX it creates an
exclusive owner-only directory, retains a no-follow directory descriptor, and
checks descriptor/path device, inode, owner, mode, and canonical path before
spawn. Cleanup never traverses or deletes workspace contents: after the direct
child is closed it rechecks the retained identity and attempts only an
empty-directory removal. Windows uses the explicitly weaker portable
pathname-observation policy because the current dependencies expose no stable
directory file ID or private-ACL proof.

The process layer hashes and bounds the configured executable pathname before
direct spawn, checks an optional SHA-256 pin, supplies the exact configured
argv, empty environment, fresh cwd, and three pipes, then owns all readers and
writes. This is a capability-probed pre-spawn pathname snapshot under a stable
namespace assumption, not executed-image attestation: portable `process`
cannot execute the already-hashed descriptor, and the digest excludes the
dynamic loader and shared libraries. Stdout remains FIFO ordered; the first
stderr byte poisons the worker while the reader continues discarding a finite
flood so teardown cannot deadlock. Absolute monotonic deadlines and explicit
cancellation gate each operation. Cleanup closes stdin, polls the direct child
without blocking a non-threaded runtime, then applies bounded TERM/KILL stages
and bounded reader/handle cleanup. Descendant cleanup remains best effort.

Before lending the worker through a rank-N callback, the Session drives a
four-write capability plan: startup print suppression and echo; reset/replay
with `input = 0`, `sat`, and echo; exact input `get-value` and echo; then a
fresh reset/replay with contradictory `input = 0` and `input = 1`, `unsat`,
and a final echo. Every barrier is derived from the unexposed secret seed.
Write-boundary whitespace is charged once, canonically attributed to the
preceding write, and must include a delimiter after the final quoted echo.
The opaque ready-worker identity binds the pure execution key, process
snapshot strength and policy, capability plan, exact segmented transcript,
secret-seed commitment, workspace policy/path, and semantic limits. It is
still not a solver result, proof, pruning authority, or general Z3 feature
claim. The next layer must drive individual query plans through the scoped
worker and independently replay any model before evidence exists.

The public live facade copies only bounded association and authority fields out
of each private run. It retains status once and derives the corresponding
heuristic strength rather than storing a duplicate fact. Public selectors
expose status, derived strength, and use, but not the retained query key or
optional evidence.
`replayLengthSMTLibLiveQueryObservation` is the sole checked semantic
extraction edge for
those hidden fields: it compares the complete collision-free query key before
it inspects optional evidence, then replays any retained evidence against the
exact `BehavioralProblem` owned by that query. A mismatched query therefore
cannot make receipt replay observable, while a successful result without a
receipt remains only an exactly associated heuristic status. The gate exposes
no process, transcript, decoded valuation, or stronger use for `unsat`.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution` seals the next
pure boundary. Its v2 policy fixes the direct prefix
`-in -smt2 smtlib2_compliant=true`, derives exact launch-time `timeout` and
`rlimit` arguments, and selects a deliberately empty child environment plus a
fresh empty working-directory policy instead of inheriting ambient process
state. Compliance mode supplies standard quoted `echo` responses. Exact
startup bytes disable `:print-success`, while the per-query reset prefix
repeats that suppression before replaying every query's complete start-mode
options and logic. The QF_LIA translator now places `:produce-models` and
`:random-seed 1` before `set-logic`, keeping each reset/replay sequence
standard-conforming while avoiding seed zero's implementation-chosen value.
The legacy `lengthSMTLibExecutionArgumentVector` name projects only the fixed
prefix; a process launcher must use
`lengthSMTLibExecutionConfiguredArgumentVector` to retain both resource
arguments.

Construction productively bounds and checks an absolute Unicode-scalar
executable path, optionally records an exact 32-byte SHA-256 executable-file
pin, rejects unbounded solver time and resource settings, requires a host
deadline with an explicit response/cleanup margin, and retains the requested
status-only or satisfiable-input artifact policy. The package-private complete
canonical fingerprint binds those fields, the protocol schema, complete argv,
exact startup and reset bytes, the response schema, and every response byte,
nesting, node, token, and integer bound. Its reversible bytes are not publicly
exposed. The separate admission limits only bound sealing work and do not
alter a successfully sealed policy's identity. The public
`lengthSMTLibExecutionExecutableDigestExpectation` classifier reveals only
whether that sealed policy contains an expectation. It cannot recover the
digest or path and does not claim that a later live executable matches either.

This policy performs no IO: it neither resolves nor hashes the path, starts Z3,
probes a version or capability, frames a stream, handles cancellation, nor
constructs a solver observation. The package-private live Session separately
binds its pre-spawn pathname observation, establishes quoted-echo,
print-suppression, reset, valuation, and contradictory-check behavior, and
records the remaining stable-namespace limitation. The optional SHA-256 bytes
are a named external digest-pin expectation, not Djex's collision-free
identity for an unbounded executable file. Policy equality is therefore not
run identity, cache authority, or semantic evidence; all eventual statuses
remain heuristic until the existing independent Length replay validates a
concrete counterexample.

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

`TypedGenerated.Fingerprint` reconstructs and reseals that graph with
`sharedTypeStructure` before assigning its nominal v1 identity. Its rooted-tree
encoding ignores table order and raw allocation numbers while preserving exact
binding structure, hole equality, flexible/rigid free-variable flavor, globals,
normalized types, patterns, witnesses, visible arguments, and branch order. It
performs no beta, eta, let, or behavioral quotienting. The retained encoding has
a caller-supplied byte bound (one MiB by default), while graph limits separately
bound its preceding traversal. A certificate reference is rejected until a
checked certificate table can replace its allocation number with semantic
substitution and obligation identities. Constructor patterns likewise require
an inventory-bound family schema which the generic shared checker does not
possess. The result is only a
structural graph key: it neither resolves an inventory nor establishes candidate
completeness, behavioral interpretation, or evidence. Domain-owned sealers must
bind those authorities separately.

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
