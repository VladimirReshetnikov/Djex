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
| `Language.Haskell.Synthesis.Semantic.Length.Evaluate` | Bounded deterministic replay of checked scalar and binary-spine-product contracts, exact candidate problems, finite Cartesian input boxes, and direct, literal-ceiling positive-affine, relational positive-affine, or strict relational positive-affine applicable domains, with nominally separate counterexample and positive bounded receipts; detached conditional-provider evaluation still fails before argument inspection. |
| `Language.Haskell.Synthesis.Semantic.Length.Problem` | Atomic checked sessions and nominally distinct scalar/product typed-candidate behavioral problems: session-owned provider and restricted resolver authority, contract resealing, residual rejection, rigid root/provider authorization, mixed-role opaque targets, exact scalar zero/step cases inside product fields, and provider-only consumption of independently authorized certificate carriers. |
| `Language.Haskell.Synthesis.Semantic.Length.SMTLib` | Bounded canonical QF_LIA translation plus exact input-symbol, natural-input, origin, finite-box, and direct, literal-ceiling positive-affine, relational positive-affine, or strict relational positive-affine applicable-domain replay for nominally distinct checked scalar and binary-product Length problems, without launching or trusting a solver. |
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
| `Language.Haskell.Synthesis.Internal.SMTLib.Z3.Process` | Domain-neutral opaque direct-process runtime owning pre-spawn observation, pipes, FIFO events, cancellation/deadlines, and bounded cleanup while exposing only associated schema-free observation and limit fields to a domain facade. |
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

The Length contract has two additive construction paths. Existing sealers
continue to require every physical target argument to be a modeled list spine.
Role-aware sealers accept a bounded closed `LengthTargetArgumentRole` vector,
retained in full and in source order by the checked contract. Observed spine
roles receive compact `LengthInput` indices; unobserved target roles receive
opaque, non-inspectable interpreter tokens. Every physical argument is still
applied. A token may be ignored or forwarded through an explicitly
non-observing provider argument or list-step payload, but callable, spine, and
tuple demands fail at an explicit site before arbitrary semantic evaluation.
The role does not assert source-language inhabitance, purity, totality, or
non-strictness. All-observed role vectors reuse the exact current legacy-policy
contract, session, and concrete-encoding identities; mixed vectors alone
select the role-aware policy. The resulting SMT query and model replay expose
only the compact observed inputs. See the
[role-aware target-argument report](../docs/reports/2026-08-13-role-aware-target-arguments.md).

New callers can choose the complete interpretation boundary once through
`LengthInterpretationPolicySource` and
`sealLengthSessionWithInterpretationPolicy`. The session retains an opaque
checked policy; `sealLengthContractInSession` derives contract roles from it,
and `sealLengthTypedCandidateProblemInSession` checks the detached contract's
exact role order and arity before resealing or graph demand. Exact zero/step
policy always carries an explicit role vector. Existing session and problem
sealers remain wrappers with their historical signatures, loose
mixedness-only problem association—including accepted role order and arity
drift—and role/policy failure precedence before carrier inspection. Associated
success and sanitized authority failures are additive after those gates. The
strict entrance is additive. The session encoding still consumes
only the mixed/all-observed and case projections; contracts and downstream
identities retain their existing contract-role field, so explicit all-observed
ordinary policy remains identical to the corresponding legacy policy. The
provider-certificate trust-boundary change advanced the ordinary session-policy
versions from 2/3/4 to 5/6/7, however, so those session and containing
downstream keys are not historical-byte compatible and caches must be
invalidated. Conditional-capable sessions use the later 8/9/10 family described
below.
Productive construction is also preserved; deep `NFData` evaluation now
honestly forces the newly retained role vector. See the
[unified interpretation-policy report](../docs/reports/2026-08-13-unified-length-interpretation-policy.md).

Length also has an additive exact-case sealer pair. It accepts only complete
zero/step splits over the checked spine model; ordinary and role-aware sessions
retain their case-rejection behavior, signatures, and current-policy
equivalence. Their historical session bytes are superseded by the ordinary
5/6/7 policy advance above, or by 8/9/10 when conditional provider authority is
present. The package-private graph
identity entrance freshly reseals constructor patterns from the session-owned
schema, while the public shared fingerprint continues to reject them. Analysis
is canonical zero then step, maps the recursive field to `n monus 1`, keeps the
payload opaque, and unions provider authority reached in both branches.
Exference now retains that one checker-proved nonempty graph shape; Djinn and
all other nonempty case shapes remain unavailable at the typed-candidate edge.
The domain foundation itself remains independent of either frontend. See the
[exact zero/step case foundation report](../docs/reports/2026-08-13-exact-zero-step-length-cases.md)
and the additive
[Exference graph report](../docs/reports/2026-08-13-exference-exact-zero-step-graphs.md).

Length now also consumes the hidden associated-certificate branch of a
`TypedCandidate` without projecting it first. Contract resealing and residual
rejection remain earlier gates. For a nonempty carrier, the domain first
freshly re-seals and fingerprints the graph and semantic rows with the
session-selected shared or exact-case `TypeStructure`; graph limits, schema
errors, and the fingerprint byte limit therefore precede Length-specific row
authorization. It then visits every row in rooted structural order and admits
only exact inventory-owned provider schemes with a checked provider summary.
The summary's exact scheme is transitively co-sealed from the same inventory
source; after the row's alpha-exact source match, only summary presence remains
dynamic. Modeled zero/step constructor owners deliberately remain unsupported
in this provider-only boundary.

An empty carrier literally follows the plain graph/candidate v1 identity path.
A nonempty obligation-free legacy carrier uses carrier-aware graph v2 and
candidate v2, binding `opaque-associated-certificate/v1` and
`activated-obligations-empty/v1`. Certificate, slot, node, and occurrence IDs
remain nonsemantic coordinates and are absent from both keys. The stamped bare
graph remains rejected by the public fingerprint and by the plain Length path.
Empty obligations do not prove dictionary or instance discharge. See the
[Length associated-provider report](../docs/reports/2026-08-13-length-associated-provider-certificates.md).

`AssumedConstraintConditionalProviderSummary` must resolve to the same exact
closed source-inventory scheme as its claim and that scheme must have a
nonempty leading constraint context. The checked summary retains the full
source scheme, roles, transfer, and the distinct
`AssumedProviderLawConditionalOnConstraintDischarge` classifier. That trust
contract assumes the law is uniform over all independently admitted dictionary
evidence: Exference retained the activated obligations, but not the particular
given or instance chosen by its checker. The legacy `AssumedProviderSummary`
continues to require a leading-context-free scheme; its constructor behavior
and fingerprint bytes are unchanged. Detached provider evaluation still
rejects conditional trust before inspecting argument arity, roles, or values.

At session sealing, Length conditionally attempts to construct the restricted
class-resolution environment from that exact inventory and the default bounded
limits. A compatibility session can succeed with this authority unavailable;
a conditional candidate then fails closed with
`LengthAssociatedClassResolverUnavailable`. Every conditional associated row
must have a nonempty activated context. Length discharges its obligations in
canonical row/step/obligation order using static exact-inventory queries which
are alias-free, forall-free, first-order, and ground, with no query givens.
This independent proof search does not reproduce Exference's checker path; it
is sound for the provider law only under the retained assumption that the law
is uniform over dictionary evidence. Rejected shapes, missing evidence,
derived constraints,
and proof limits become closed sanitized reasons rather than public constraint,
type, instance, or receipt payloads. Z3 never participates in this discharge.

Proof receipts do not authorize a provider name globally. Before any discharge,
Length audits the complete graph, including dead nodes. A conditional row's
base and each intermediate visible-application prefix must not be the root and
must have exactly the certified incoming function edge. The base is retained
as a sentinel and still fails if evaluated directly. Only that row's final
receipt-bearing visible-application node can invoke the law; a direct, partial,
shared-prefix, or otherwise unassociated occurrence remains unauthorized. The
new discharge and chain failures expose only the provider owner and canonical
row/step/obligation or base/intermediate positions plus closed reasons.

The retention identities remain exact: all-legacy inventories keep provider
inventory v2 and semantic inventory v1, while conditional inventories keep the
preceding provider inventory v3 and semantic inventory v2 bytes. The
dictionary-uniform behavioral marker is deliberately absent from those
retention envelopes. Sessions without conditional summaries keep policy
versions 5/6/7; conditional-capable sessions use 8/9/10 and bind the exact
static resolver policy, default limits, no-givens/Z3 boundary,
dictionary-uniform law assumption, final occurrence, and prefix audit. Ordinary
concrete encodings remain 1/2/3, while conditional-capable encodings use 4/5/6.
Plain candidates remain v1, obligation-free associated candidates remain v2,
and ground-discharged conditional candidates use v3, where
`provider-law-uniform-over-dictionary-evidence/v1` and
`static-discharge-without-givens-or-z3/v1` first describe behavioral use. The
carrier graph remains v2. Complete-problem and SMT-query schemas and versions
are unchanged, although their existing composition transitively binds the new
session, concrete-encoding, and candidate identities. See the
[ground constraint-discharge report](../docs/reports/2026-08-13-length-ground-constraint-discharge.md).

`Language.Haskell.Synthesis.Semantic.Length.SMTLib` provides a pure canonical
QF_LIA boundary over an exact checked length problem. Its opaque nominal query
contains bounded check and input-only `get-value` commands; it neither starts a
solver nor associates a raw solver status. Decoded input bindings can produce a
counterexample receipt only through independent replay against the retained
problem, while raw models and even `unsat` remain heuristic observations.
`replayLengthSMTLibCounterexampleInputs` accepts only source-ordered
`[Natural]`; the opaque query, rather than its caller, owns the checked problem,
input-symbol association, and exact behavioral-problem identity. Each call
performs a new bounded concrete evaluation and returns either a fresh
counterexample receipt after exact same-query/problem association or `Nothing`
for a non-counterexample. It retains no cached verdict, consumes no solver
observation, and gives `unsat` no authority. The additive entrance changes no
SMT translation, checked-problem, query, execution, response, or protocol
identity/schema version. See the
[query-owned raw-input replay report](../docs/reports/2026-08-14-query-owned-length-input-replay.md).

`probeLengthSMTLibCounterexampleAtOrigin` specializes that exact boundary to
the canonical all-zero assignment. It derives the compact input count only
from the checked problem privately retained by the query, constructs no symbol
or contract projection, and then delegates to ordinary query-owned replay.
Consequently a hit is a fresh ordinary counterexample receipt, a miss has no
positive authority, and replay rejection retains the established evaluation or
association error. The pure probe issues no SMT-LIB, consumes no observation,
and changes no identity bytes or schema. See the
[query-owned origin-probe report](../docs/reports/2026-08-14-query-owned-length-origin-probe.md).

`validateLengthProblemInputBox` independently exhausts a finite Cartesian box
of compact modeled inputs. `LengthInputBoxLimitSource` seals a nonnegative
maximum input width together with a natural assignment-count cap; defaults are
eight inputs and 65,536 assignments. The checked problem's input count is
rejected against that width before the raw maxima list is demanded. The
verifier then observes exact source-ordered maxima arity productively, checks
each inclusive maximum left-to-right under the existing assignment-value bit
limit, and computes the Cartesian product with a saturating cap before
allocating the initial assignment. Existing intermediate-value limits continue
to bound each concrete replay. A nullary problem has the one assignment `[]`;
an assignment cap of zero rejects it before evaluation.

Enumeration is lexicographic with the last input varying fastest. The first
bounded evaluation error records its zero-based assignment ordinal, and the
first violation returns the ordinary exact-problem counterexample evidence.
Only complete traversal constructs positive bounded `BehavioralEvidence`. Its
opaque `ValidatedLengthInputBox` receipt privately retains the fixed
`finite-list-spine-length/bounded-input-box-validation/v1` tag, checked
inclusive maxima, exact total assignment count, count whose precondition held,
and the provider/model basis. The applicable count exposes vacuous validation;
provider-backed success remains conditional on the same canonical named
assumed laws.

`validateLengthSMTLibQueryInputBox` delegates all traversal to that
solver-independent verifier and uses the opaque query only to replay either
fresh evidence payload against the same behavioral problem. It sends no
command, consumes no raw or live observation, and cannot promote `unsat` (or
any other status). Bounded success establishes only the checked finite box in
the versioned total-spine model. It is not universal proof, exact-pruning
authority, dictionary evidence, provider-implementation validation, or a claim
about source-language inhabitance, bottoms, effects, or totality.

No existing contract, provider-inventory, semantic-inventory, session-policy,
candidate, concrete-encoding, complete-problem, SMT-query, response, protocol,
execution, process, worker, or live-observation version or canonical bytes
change. The new v1 tag belongs only to the opaque bounded receipt. See the
[bounded input-box validation report](../docs/reports/2026-08-14-bounded-length-input-box-validation.md).

`validateLengthProblemApplicableDomain` builds on that verifier with one
deliberately narrow finite-coverage rule over the checked normalized
precondition. It recognizes only direct top-level
`LengthAtMost (LengthVariable (LengthInput i)) (LengthLiteral maximum)`
clauses, retains the tightest maximum for duplicate input clauses, and requires
one for every compact modeled input. It performs no implication, equality,
arithmetic, nested-formula, or solver reasoning. Missing coverage and the
nonnullary problem's first unbounded input produce the ordinary
`LengthApplicableDomainInapplicable` result. A nullary problem instead derives
maxima `[]` and validates the single assignment `[]`.

When coverage succeeds, the derived source-ordered maxima form one tight
solver-independent Cartesian box. The established box limits and evaluation
limits still gate width, cardinality, values, and intermediate arithmetic. A
violation releases the ordinary `ValidatedLengthCounterexample`; complete
traversal releases a nominal opaque `ValidatedLengthApplicableDomain` which
retains the derived maxima, total/applicable counts, and exact model/provider
basis. `validateLengthSMTLibQueryApplicableDomain` adds only same-problem
association and never emits a command or consumes a solver status.

The product-domain siblings are
`validateLengthSpinePairProblemApplicableDomain`,
`validateLengthSpinePairSMTLibQueryApplicableDomain`, and
`ValidatedLengthSpinePairApplicableDomain`. Their result classification is
shared but their behavioral evidence remains nominally separate. Neither
receipt establishes source-language totality, validates concrete provider
implementations, authorizes pruning, or upgrades `sat`, `unsat`, or `unknown`.
See the
[directly bounded applicable-domain report](../docs/reports/2026-08-14-directly-bounded-length-applicable-domain.md).

The original applicable-domain surface above remains the exact literal-direct
v1 path. It still ignores equality and arithmetic-derived bounds. The additive
`validateLengthProblemPositiveAffineApplicableDomain` selects a separate,
strictly more capable syntactic rule; its query-owned sibling is
`validateLengthSMTLibQueryPositiveAffineApplicableDomain`.

For one recognized atom, the bounded side must be a positive-affine expression
built only from compact inputs, natural literals, `LengthSum`, and
positive-literal `LengthScale`. If its normalized value is
`c + sum (ai * xi)` and it is constrained by either `<= k` or `== k` for a
literal `k`, every positive coefficient contributes the necessary inclusive
maximum `(k - c) quot ai` when `c <= k`. The scanner takes the minimum of all
bounds for an input and requires coverage for every nonnullary compact input.
It does not mine a partial bound from an otherwise unsupported subtree.

`LengthTruth False` and a recognized atom with `c > k` are exact syntactic
contradictions. Normalization turns either orientation of a false constant-only
equality such as `1 == 2` into that false truth value; a true constant-only
equality is non-binding. For a nonnullary problem, contradiction wins over
missing-bound inapplicability and selects an all-zero coverage box. The box
still validates one assignment and therefore records total count one and
applicable count zero on success. Nullary problems bypass extraction, derive
maxima `[]`, and replay their ordinary singleton assignment; its applicable
count is zero or one.

All admission and behavioral work remains in the established box verifier.
Width is rejected before the normalized precondition is scanned. Without a
contradiction, the complete clause list is examined before the first missing
compact input is returned as ordinary `LengthApplicableDomainInapplicable`.
Derived maxima are checked left-to-right, the Cartesian cap is admitted before
replay, and assignments retain last-input-fastest lexicographic order and
indexed evaluation failures.

Complete traversal produces the opaque
`ValidatedLengthPositiveAffineApplicableDomain`; the product siblings are
`validateLengthSpinePairProblemPositiveAffineApplicableDomain`,
`validateLengthSpinePairSMTLibQueryPositiveAffineApplicableDomain`, and
`ValidatedLengthSpinePairPositiveAffineApplicableDomain`. Both receipt families
project the exact maxima, total/applicable counts, and model/provider basis.
The query wrappers issue no solver command and release either authoritative arm
only after exact behavioral-problem association.

The two new receipt tags are
`finite-list-spine-length/positive-affine-precondition-domain-establishment/v1`
and
`finite-binary-product-spine-lengths/positive-affine-precondition-domain-establishment/v1`.
They are the first versions of a new nominal receipt family, not changes to the
old direct-v1 tags. No existing semantic or runtime identity changes.
Establishment is still model-relative, conditional on any retained assumed
provider laws, and grants no source-language realization, totality,
provider-implementation, solver-status, universal-proof, or pruning authority.
See the
[positive-affine applicable-domain report](../docs/reports/2026-08-14-positive-affine-length-applicable-domain.md).

The third, separately selected coverage rule is relational positive-affine
validation. `validateLengthProblemRelationalPositiveAffineApplicableDomain`
and
`validateLengthSpinePairProblemRelationalPositiveAffineApplicableDomain`
summarize both sides of top-level `LengthAtMost` and `LengthEqual` clauses over
compact inputs, natural literals, sums, and positive scales. They cancel common
constants and coefficients exactly. An inequality contributes one directed
rule; equality contributes the normalized left-to-right rule followed by its
reverse. The query-owned entrances are:

```haskell
scalarValidation =
  validateLengthSMTLibQueryRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits pairQuery
```

A constant-right rule seeds bounds. Every subsequent pass observes one
immutable snapshot: all rules whose right-side inputs are bounded fire once,
their results are merged with `min` after the pass, and fired rules are
removed. Skipped rules retry in canonical order. The closure stops when a pass
fires nothing; it deliberately does not seek a numeric least fixed point. Thus
`x <= y`, `y <= 10`, `y <= z`, and `z <= 2` derive `[10, 2, 2]`, not
`[2, 2, 2]`. This finite box is sound because each fired rule derives necessary
bounds from already established right-side maxima.

`LengthTruth False` and a fired rule whose residual left constant exceeds its
right-side maximum establish contradiction and select the ordinary all-zero
coverage carrier. Without contradiction, the first unbounded compact input is
ordinary `LengthApplicableDomainInapplicable`. All width, value, product,
evaluation, counterexample, nullary, provider-basis, and exact query-association
behavior remains owned by the existing box verifier and evidence layer.

The opaque receipts are
`ValidatedLengthRelationalPositiveAffineApplicableDomain` and
`ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain`. Only their
new scalar and product receipt tags add canonical bytes. Query-owned validation
issues no solver command and consumes no status. The direct-v1 and
literal-ceiling positive-affine validators remain unchanged, as do all existing
contract, inventory, session, candidate, encoding, problem, query, protocol,
runtime, and live-observation identities. Establishment remains relative to
the checked total finite-spine model and retained assumed provider laws; it is
not source-language totality, provider-implementation validation, universal
proof, or pruning authority. See the
[relational positive-affine applicable-domain report](../docs/reports/2026-08-15-relational-positive-affine-length-applicable-domain.md).

The fourth explicit coverage rule adds exact strict natural inequalities without
changing the relational rule above. Its checked-problem entrances are
`validateLengthProblemStrictRelationalPositiveAffineApplicableDomain` and
`validateLengthSpinePairProblemStrictRelationalPositiveAffineApplicableDomain`;
the query-owned entrances are:

```haskell
scalarValidation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits pairQuery
```

The strict scanner delegates every ordinary `LengthAtMost`, `LengthEqual`, and
`LengthTruth False` clause to the unchanged relational scanner. In addition,
one immediate top-level normalized clause whose exact shape is
`LengthNot (LengthAtMost left right)` contributes this proof rule over
naturals:

```text
not (left <= right)  ==>  right + 1 <= left
```

Both sides must use only compact inputs, natural literals, `LengthSum`, and
positive-literal `LengthScale`. The successor increments the exact
arbitrary-precision affine summary of the original right side before the
ordinary relational cancellation pass. It creates no checked expression and
therefore spends no syntax or literal budget and changes no checked contract
or query bytes.

For a scalar chain, `not (5 <= x)` yields `x <= 4`; a second clause
`not (x <= y)` yields `y + 1 <= x`, so the first maximum propagates to
`y <= 3`. Query-owned complete traversal therefore reports maxima `[4, 3]`,
total count 20, and applicable count 10 for `y < x < 5`. For a one-input
product problem, `not (x + 3 <= 2*x)` becomes
`2*x + 1 <= x + 3`; exact common-coefficient cancellation gives `x <= 2`,
so the product box has maximum `[2]` and three applicable assignments.

The strict rule retains the relational closure's exact sequencing. Constant-
right rules seed bounds; later rules retry against immutable pass snapshots,
fire at most once, and merge after the pass. It is still not a general solver
or a numeric least-fixed-point computation. A negated equality, nested
logical formula, or negated comparison containing monus, minimum, maximum,
quotient, modulo, a conditional, a result reference, or another non-affine
subtree contributes no rule and no partial coverage. Unsupported clauses still
participate in ordinary precondition replay if other clauses bound the box.

Input-width rejection precedes precondition extraction. Nullary problems
delegate `[]` directly to the finite-box verifier. For nonnullary problems,
syntactic or closure contradiction wins over otherwise missing coverage and
selects the all-zero carrier; absent contradiction, the first source-ordered
missing bound is ordinary `LengthApplicableDomainInapplicable`. Derived values,
Cartesian cardinality, indexed evaluation, first counterexample, and exact
query-association failures retain the existing finite-box order.

The opaque receipts are
`ValidatedLengthStrictRelationalPositiveAffineApplicableDomain` and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain`.
They retain exact maxima, total/applicable counts, and model/provider basis,
but remain nominally disjoint. Query-owned validation emits no SMT-LIB and
consumes no solver observation; it only replays authoritative evidence against
the query's exact behavioral problem. Establishment is model-relative and
does not establish source-language behavior, validate providers, supply
universal proof, or authorize pruning.

Only the two new strict-relational receipt tags add bytes. All older explicit-
box, direct, positive-affine, relational, counterexample, raw-input, origin,
simplification, and live validators retain their exact surfaces, behavior,
receipt tags, authority, and identities. Every existing contract, inventory,
session, candidate, encoding, problem, query, response, protocol, runtime, and
live-observation identity is unchanged. See the
[strict relational positive-affine applicable-domain report](../docs/reports/2026-08-15-strict-relational-positive-affine-length-applicable-domain.md).

`FiniteBinaryProductSpineLengthsV1` adds a distinct checked domain for an exact
boxed binary product whose two source-ordered fields are modeled finite
spines. `LengthSpinePairContractVariable` retains the scalar domain's compact
observed inputs and makes `LengthSpinePairFirst` and
`LengthSpinePairSecond` available only as result components in the
postcondition. This supports relational output laws while rejecting result
references in preconditions, nonbinary or unboxed tuples, nested products, and
nonspine result fields.

The product boundary reuses one `CheckedLengthSession`'s checked spine model,
role vector, provider laws, conditional-discharge authority, and case policy.
Its nominal behavioral inventory is nevertheless built by structurally
wrapping the complete scalar semantic-inventory bytes and a versioned
authority-derivation policy; it is not a coercion between evidence domains.
Product contracts, candidates, encodings, complete problems,
counterexamples, and bounded receipts all have sibling opaque identities and
cannot associate with scalar evidence.

Candidate interpretation applies the existing target roles, requires the
final semantic value to be exactly a two-field tuple, forces its first field
before its second, and normalizes both modeled-spine expressions under one
joint left-to-right syntax budget. A field may use the scalar provider and
exact zero/step case mechanisms already authorized by the session. Providers
still return only scalar spines, and case analysis still has only scalar-spine
result and scrutinee authority; there is no product provider or product case
rule.

`validateLengthSpinePairProblemCounterexample` recomputes result components
from the checked candidate as the postcondition demands and materializes both
lengths for a violation, using only caller-supplied compact natural inputs.
`validateLengthSpinePairProblemInputBox` enumerates the same input space and
releases only an exact `ValidatedLengthSpinePairCounterexample` or positive
`ValidatedLengthSpinePairInputBox` evidence. It does not consume a solver
observation.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib` now provides the pure
offline product-query sibling. `sealLengthSpinePairSMTLibQuery` accepts only an
opaque `CheckedLengthSpinePairProblem` and produces an opaque
`LengthSpinePairSMTLibQuery` under the distinct
`djex-length-spine-pair-z3-qf-lia-smtlib2/v1` schema. Its canonical `QF_LIA`
check program and optional `get-value` request mention compact input symbols
only: candidate interpretation has already substituted both ordered result
expressions into the scalar-variable bad-state formula. The product query has
its own fingerprint subject, role, schema, error families, problem domain, and
evidence association even when its rendered bytes coincide with a scalar
query.

`validateLengthSpinePairSMTLibCounterexample` treats the shared
`LengthSMTLibIntegerBinding` carrier as untrusted, verifies the exact input
symbol set, restores source order, rejects negative values, and independently
recomputes both candidate results. Direct natural-input replay, the all-zero
probe, and exact finite-box association are exposed by
`replayLengthSpinePairSMTLibCounterexampleInputs`,
`probeLengthSpinePairSMTLibCounterexampleAtOrigin`, and
`validateLengthSpinePairSMTLibQueryInputBox`. Every entrance evaluates afresh;
a miss is not positive evidence, and box success remains finite/model-relative
under its explicit provider basis.

The product query now also has bounded query-specific response decoding, a
distinct package-private protocol plan and phase machine, a nominal product
query-run identity, and a public live observation/failure/replay surface. It
reuses the scoped worker only after that worker's four-write probe has
established the common QF_LIA/reset/status/input-valuation transport profile.
That readiness fact supplies no scalar problem, observation, or evidence
authority to the product path.

Scalar and product runs reserve from the same serial zero-based ordinal space.
The public default is exactly 64 transactions total across an arbitrary mixed
sequence, not 64 per domain, and maximum-plus-one fails before a write. Product
protocol, run, observation, and evidence identities remain separate even if a
product query's canonical input-only bytes happen to equal scalar bytes. A
values-policy `sat` path must decode the exact product input set and
independently recompute both result components before optional product evidence
can exist. The public consumer must still use
`replayLengthSpinePairSMTLibLiveQueryObservation` with the exact product query
to reveal that receipt. All status projections and their derived strength/use
remain heuristic; `unsat` and `unknown` have no proof or pruning authority.

Every historical scalar public signature, schema tag, canonical query and
protocol byte, run identity field, replay behavior, and observation API remains
exact. See the stage-one
[finite binary product spine-length foundation report](../docs/reports/2026-08-14-finite-binary-product-spine-length-foundation.md)
and the
[offline product SMT and replay report](../docs/reports/2026-08-14-finite-binary-product-spine-smt-replay.md),
followed by the
[live binary-product Length/Z3 report](../docs/reports/2026-08-14-live-binary-product-spine-z3.md).
Leant now consumes the product facade through its exact canonical-`Prod`
handoff, nominal pair ranking and presentation, and startup configuration
versions 4 and 6. That downstream integration does not convert pair evidence
to scalar authority.

Positive-literal natural quotient and modulo remain inside QF_LIA by using one
shared deterministic private Euclidean witness shape rather than the forbidden
SMT-LIB `div` and `mod` operators: for operand `e` and positive literal `k`,
sealing asserts `e = k*q + r`, nonnegative `q` and `r`, and `r <= k-1`, then
projects `q` for quotient or `r` for modulo. Normalized-expression preorder
fixes operation-specific witness names and constraint order. Witnesses are
declared and structurally fingerprinted but never requested from the solver;
`get-value` remains input-only.
The package-private `Internal.SMTLib.QFLIA` module owns the shared typed
integer, Boolean, and command AST, the exact `QF_LIA` logic spelling, canonical
rendering, and the matching structural fingerprint-field projection. Length
retains all domain translation, helper and generated-name choices, Euclidean
witnesses, limits, query identity, model validation, and replay. The complete
typed SMT plan remains transient through the two shared projections. The
sealed query retains only the checked problem, canonical check bytes, and
complete fingerprint. Exact decoder-symbol order and optional `get-value`
bytes are canonically rederived from the checked problem's sealed arity after
query sealing has already bounded and structurally fingerprinted them.
Operation-specific versioned plan tags record only the witness projections
that occur. The extraction deliberately preserves every existing symbol,
command, tag, script, and canonical key byte. The structural plan keeps
rendered bytes from becoming the semantic source of truth. See the
[shared typed QF_LIA foundation report](../docs/reports/2026-08-13-shared-typed-qf-lia-foundation.md),
the
[positive-literal quotient report](../docs/reports/2026-08-13-positive-literal-natural-quotient.md)
and the earlier
[positive-literal modulo report](../docs/reports/2026-08-13-positive-literal-natural-modulo.md)
for the exact admission, lowering, compatibility, and test boundary.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation` can associate
such a bounded raw report with both the retained behavioral problem and the
exact query fingerprint. Its opaque value deliberately projects no artifact or
nested generic association before exact replay. Status and conservative
strength are derived from the singly retained raw observation, while the fixed
use remains inspectable and every status is restricted to
`HeuristicRankingOnly`. Even after replay the report is still raw: a model
must pass independent Length validation, while `unsat` never becomes evidence.
Query identity is only the canonical semantic translation identity, not a run
or cache identity. A later executor must additionally seal the solver build,
capability handshake, process and protocol state, parser schema, requested
artifact policy, deadlines, cancellation, and resource limits; `unknown`
should not be cached by query identity alone.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response` is the matching
pure response boundary. It retains at most 65,536 bytes before parsing, so
cyclic or infinite lazy input is rejected productively, then enforces separate
list-depth, S-expression-node, source-token-byte, and integer-width limits. The
shared package-private response lexer handles comments, doubled-quote strings,
quoted symbols, and all standard atom categories while importing the exact
whitespace set from `Internal.SMTLib.Lexical`. The public surface
accepts only `sat`, `unsat`, `unknown`, or the exact input valuation requested
by one query; valuations are symbol-checked and restored to source order.
The package-private shared Standard layer owns canonical status bytes, bounded
check-status decoding, and standard `unsupported`/solver-error shapes. Length
exhaustively maps those closed failures into its compatibility vocabulary and
continues to own its limit wrapper and defaults, valuation shape, and response
schema. The readiness capability imports only canonical `sat`/`unsat` bytes
and retains byte-exact,
payload-free phase failures.
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

`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream` owns cumulative
framing once for both the query protocol and readiness capability machine. An
opaque policy combines the exact single-frame limits with the transaction
maximum. Its public initial cursor always starts at absolute offset zero;
nonzero restarts can be obtained only from an opaque completed frame or a
successfully validated write boundary. Same-write continuation feeds the
completed frame's hidden tail under the same policy and offset. Cross-write
continuation first admits and discards only the canonical four SMT-LIB
whitespace bytes, then carries the same policy and exact charged offset into
the next receiver. The configured frame-total error wins an equal-limit tie;
only a strictly tighter remaining cumulative budget is reclassified at maximum
plus one. The schema-free `Internal.SMTLib.Lexical` leaf supplies that ordered
whitespace vocabulary to the response parser, framer, cursor, transport
driver, process boundary drain, and domain fingerprints. The schema-free
`Internal.SMTLib.Causal.BoundaryWhitespace` leaf admits finite strict drain
bytes into an opaque content proof. Process mints nonempty receipts while its
STM inspection can still restore a rejected FIFO snapshot. For the initial
adopted predecessor boundary, Driver opens the successful receipt only after
the first exact write succeeds; later completed-epoch drains retain their
existing append-before-next-write timing. A separate opaque stdout receipt is
minted for every nonempty strict Process read before enqueue, so the generic
driver's successful read cannot express an empty, zero-progress chunk. These
receipts do not claim FIFO origin, configured bounds, process association, or
restoration: those remain concrete transport laws.

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol` owns the
next pure boundary. It seals the complete execution-policy key, exact query
key, stream limits, cumulative stdout budget, phase schema, and caller-supplied
check/value markers into one private protocol-plan identity. The runtime plan
then retains only the exact artifact policy, response limits, query, stream
policy, positional markers, and that complete key; launch profile, digest,
deadline, and other launch-only execution facts are not retained as separate
runtime protocol fields. Their canonical identity remains nested in the
complete plan key.
The initial action writes reset, canonical check commands, and a status marker
together. The machine then
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
After response frames are bounded and decoded, the protocol phase state and
terminal decoded branch retain one strict status-indexed observation. Only its
satisfiable branch can contain optional integer bindings: status-only `sat` is
`Nothing`, zero-input value policy is `Just []`, and a framed valuation is a
nonempty `Just`; the `unsat` and `unknown` branches contain only unit. The
opaque receiver separately owns the still-driving plan and
shared cumulative cursor; the live Session separately carries that plan
through execution and binds its key directly into run identity instead of
copying it into the decoded branch. Exact raw status-frame and
input-value-frame bytes have one live owner in the causal transcript. Those
transcript bytes are bound into the query-run identity rather than being copied
into the decoded value.

Tails cross status-to-marker and value-to-marker phases only because those
responses answer commands already in the same completed write. A tail crossing
the status-marker-to-`get-value` boundary must contain only bounded SMT-LIB
whitespace; a stale buffered valuation is rejected before the new write action
exists. Framing, response, marker, cumulative-budget, unexpected-byte, and EOF
failures expose no continuation. The live Session therefore destroys the
worker on any failure, generates barriers uniquely across the session,
performs each write before feeding its receiver, and binds the process snapshot
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

The shared `Internal.SMTLib.Z3.Process` layer hashes and bounds the configured
executable pathname before direct spawn, checks an optional SHA-256 pin,
supplies the exact configured argv, empty environment, fresh cwd, and three
pipes, then owns all readers and writes. It consumes only the shared admitted
Z3 launch profile and retains the exact observation and limits with that opaque
runtime. The Length `Session.Process` facade maps its closed failures and seals
those associated schema-free fields under the raw v2 identity. That identity
retains the facts the runtime enforces and observes—requested and canonical
paths, metadata, digest and pin result, argv, environment, cwd, process limits,
deadline, and launch flags—without embedding artifact policy, response policy,
or a second copy of the complete Length execution key. This is a
capability-probed pre-spawn pathname snapshot under a stable
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
Every returned queued-drain success is an opaque lexical receipt; a rejected
non-whitespace snapshot is restored before Process poison, while later
cancellation or deadline poison retains its existing destructive-drain
semantics.
The opaque v4 ready-worker identity binds the complete Length execution key
once, then the process launch snapshot and policy, capability plan, exact
segmented transcript, secret-seed commitment, workspace policy/path, and
semantic limits. It is
still not a solver result, proof, pruning authority, or general Z3 feature
claim. The same Session drives nominally separate scalar and product query
plans through the scoped worker and independently replays any model before
evidence exists. The probe establishes only their shared QF_LIA/input-value
transport requirements, not cross-domain behavioral authority.

The live worker does not retain the whole pre-readiness Session configuration.
Its private strict query policy keeps one query-count cap shared by scalar and
product runs, the query-run identity budget, protocol limits, and a post-launch
execution policy containing
only the host deadline, artifact policy, response limits, and original complete
execution-policy key needed to derive each query deadline and seal future
plans. It does not retain the structured Z3 launch profile, executable path or
pin, solver controls, argv, environment, or working-directory policy as
separately projectable fields. Exact process limits are projected from the
retained opaque runtime. Workspace-allocation, capability, and ready-identity
admission work has completed before lending; the opener deadline and Session
workspace-cleanup authority remain with the enclosing scope.
Once a plan is sealed, replay reads the exact query and artifact policy from
that domain's plan rather than pairing it with independent worker-wide fields.
Response decoding likewise reads the limits retained by that exact plan.
The terminal decoded value remains local through independent replay and the
corresponding nominal query-run identity builder. Before a successful run
escapes, commit forces a narrower owner containing its ordinal, one strict
status-indexed observation, reversible key, transcript digest, and accounting
boundaries,
but no parsed symbol/integer binding list. Only the satisfiable observation
branch can contain optional problem-bound evidence. Evidence retains the
normalized compact source-ordered observed-spine counterexample inputs, and
the private reversible key retains exact transcript bytes; the deletion
narrows structured authority rather than scrubbing child output.

The public live facade copies each domain's whole status-indexed observation
once along with its bounded query association, rather than re-pairing a status
and an optional payload. It derives the corresponding heuristic strength from
the observation's status. Scalar and product selectors expose status, derived
strength, and use, but not the retained query key, whole observation, or
optional evidence. `replayLengthSMTLibLiveQueryObservation` and
`replayLengthSpinePairSMTLibLiveQueryObservation` are the respective checked
semantic extraction edges: each compares the complete collision-free nominal
query key before it inspects optional evidence, then replays any retained
evidence against the exact `BehavioralProblem` owned by that query. A
mismatched query therefore cannot make receipt replay observable, while a
successful result without a receipt remains only an exactly associated
heuristic status. Neither gate exposes a process, transcript, decoded
valuation, or stronger use for `unsat`. Direct raw-input replay instead
evaluates caller-supplied naturals afresh and does not extract any hidden
live-observation field.

The public live facade also offers additive elapsed-time policies. A caller
first validates `LengthSMTLibLiveUsableWorkBudgetSource` with
`mkLengthSMTLibLiveUsableWorkBudget`; validation is pure and rejects a
nonpositive duration or host-microsecond/monotonic-nanosecond conversion
overflow. The original `withLengthSMTLibLiveUsableWorkDeadline` captures one
absolute monotonic deadline and supplies a generative
`LengthSMTLibLiveUsableWorkDeadline budget`. Its rank-N phantom prevents
accidental type-level mixing between captures, but it does not enforce dynamic
non-escape: a returned action closure or fork can retain and later use the v1
token. V1 remains available for source and exact identity compatibility.

New integrations should use
`withLengthSMTLibLiveScopedUsableWorkDeadline`. Its opaque
`LengthSMTLibLiveScopedUsableWorkDeadline budget` additionally records an
owner thread and open/closed runtime state. Both
`checkLengthSMTLibLiveScopedUsableWorkDeadline` and
`withLengthSMTLibLiveSessionUnderScopedDeadline` accept it only on that owner
thread while the owner callback is open. The owner closes admission on normal
or exceptional exit. A forked use, or an action closure invoked after exit on
the original thread, returns
`LengthSMTLibLiveSessionUsableWorkScopeUnavailable`; that lifecycle check
precedes the clock, configuration, and workspace, so scope unavailability wins
over simultaneous expiry. The session opener repeats admission at its private
production boundary before resource acquisition.

Application work can be forced after v2 capture, checked cooperatively, and
then followed by a deferred session:

```haskell
scoped <-
  withLengthSMTLibLiveScopedUsableWorkDeadline budget $ \deadline -> do
    preparedQuery <- evaluate $ force deferredQuery
    checkpoint <- checkLengthSMTLibLiveScopedUsableWorkDeadline deadline
    case checkpoint of
      Left failure -> pure (Left failure)
      Right () ->
        withLengthSMTLibLiveSessionUnderScopedDeadline
          deadline executionConfig $ \session ->
            runLengthSMTLibLiveQuery
              defaultLengthEvaluationLimits session preparedQuery
```

The shorter `withLengthSMTLibLiveSessionWithScopedUsableWorkBudget` captures
and consumes v2 around exactly one session. The corresponding v1 two-step and
convenience names remain available as compatibility entrances, but their
tokens are unsafe to retain or share.

The legacy `withLengthSMTLibLiveSession` path is unchanged: opening has its
established private deadline and each query derives a fresh local host
deadline. Beneath a shared token, the opener and every scalar or product query
instead select the minimum of the captured absolute deadline and their fresh
local deadline, with the shared deadline winning an exact tie. This covers
workspace allocation and inspection, launch and executable snapshot, the
capability probe, waiting for the one serial gate, transaction transport,
independent model replay, and run-identity admission/sealing. It is orthogonal
to the existing shared 64-entry scalar/product ordinal ceiling.

Neither policy installs an asynchronous watchdog around arbitrary application
callbacks. A blocked callback or nonterminating pure computation is not
interrupted. A v2 checkpoint reads the same absolute deadline without
refreshing it; it consumes no query ordinal, emits no SMT-LIB, and creates no
solver observation. Expiry is observed at a checkpoint, live operation, or
normal callback boundary. Callback exceptions remain authoritative and are
re-thrown after v2 closes admission; a nested session begins its durable owned
cleanup before an exception crosses that session boundary.

The session's post-callback final-readiness observation and durable cleanup run
under fresh established private windows, not the shared operational deadline.
The v2 convenience owner closes its lease after the nested session returns but
performs no second deadline check after those stages. The general two-step v2
owner closes and checks on normal return, so an outer callback which waits for
a nested session to finish can observe that the shared time elapsed while the
nested fresh-window finalization completed. This is an explicit accounting
choice, not a whole-callback hard deadline.

Deadline expiry maps into the existing byte-free public failure vocabularies:
owner/session-boundary expiry is
`LengthSMTLibLiveSessionDeadlineExceeded`, while wrong-thread or closed v2 use
is `LengthSMTLibLiveSessionUsableWorkScopeUnavailable` and expiry observed
within a scalar or product query is its nominal query deadline failure.
Cleanup incompleteness remains separately inspectable; neither child bytes nor
clock values cross the facade.

V1 budgeted workers and runs retain their exact additive identities. V2 adds
new ready-worker, scalar-run, and product-run roles and schema tags under
`scoped-shared-usable-work-deadline/v2`; each embeds the exact applicable
legacy identity and binds the duration, captured shared deadline, effective
minimum and cause, shared-on-tie rule, runtime lifecycle/admission policy, and
coverage policy. Thread identifiers, mutable open/closed state, and checkpoint
observations do not enter identities. Legacy, v1, scalar-v2, and product-v2
identities remain distinct, while query, protocol, observation, and replay
bytes remain exact.

This deadline is process/session causality only. It supplies no executable-
image attestation, solver-soundness claim, proof, pruning certificate, or
cross-domain authority. Scalar and product statuses remain nominally separate
`HeuristicRankingOnly` observations; optional counterexample evidence still
requires exact query association and independent domain replay. The API,
identity, coverage, exclusion, and failure details are recorded in the
[shared live usable-work budget report](../docs/reports/2026-08-15-shared-live-usable-work-budget.md).
The dynamically enforced v2 scope and the retained v1 limitation are recorded
in the
[dynamically scoped live usable-work deadline report](../docs/reports/2026-08-15-dynamically-scoped-live-usable-work-deadline.md).

`Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution` seals the shared
pure launch profile below domain protocols. It owns path/pin admission,
timeout, `rlimit`, host deadline, complete argv, exact startup/reset commands,
empty environment, fresh-cwd policy, and the flat launch field slice consumed
by a domain-owned complete fingerprint. It owns no standalone schema,
fingerprint budget, response grammar, or solver authority.
`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution` keeps its
unchanged public compatibility surface and combines that profile with the
Length policy/protocol tags, artifact policy, response limits, and complete
Length fingerprint. Its v2 policy therefore still fixes the direct prefix
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

The package-private shared Process opener consumes only this profile and owns
no domain identity schema. The Length facade retains only that exact process
and derives its v2 field from the process-associated observation and limits;
ready-worker identity binds one occurrence of the complete Length execution
key beside that raw process field rather than embedding a second occurrence
inside it. Removing the former nested complete-policy duplicate deliberately
shortens private ready-worker and query-run keys. A custom identity-byte budget
at the old boundary can therefore newly admit the same policy; no previously
admitted policy becomes oversized.

The raw runtime extraction and later facade cache deletion are recorded in the
[shared raw Z3 process report](../docs/reports/2026-08-12-shared-z3-process-runtime.md)
and [derived Length process identity report](../docs/reports/2026-08-12-length-process-derived-identity.md).

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

The package-private `Internal.TypedGenerated.Certificate` module accepts a
separately bounded table of raw certificate coordinates, exact source schemes,
and complete ordered leading-telescope selections. It validates and
canonicalizes every admitted type, assigns source and selection binders to
disjoint positional namespaces, replays each substitution without caller-owned
fresh names, and derives the ordered constraints that become unconditional.
Binderless context-free foralls are erased before scope assignment; vacuous
binders still consume semantic slots; contexts carried inside a selected type
remain part of that selected type rather than becoming source obligations.
Neither raw obligations nor intermediate results are accepted as caller claims.

This checked table is intentionally not a certificate of origin. It does not
prove that a source belongs to an inventory or provider, that source or
selection types are closed, that a selection has the positional binder kind,
that a constraint names a known class with the declared arity, is entailed, or
is discharged, or that a row belongs to one graph occurrence. Its constructor
and observations are package-private, and it has no fingerprint entrance. The
existing public fingerprint failure is unchanged. See the
[bounded certificate-plan report](../docs/reports/2026-08-13-bounded-type-application-certificate-plans.md).

`Internal.TypedGenerated.Certificate.Association` adds an opaque, atomic
graph-occurrence foundation without widening that neutral table. Its single
entrance takes an untrusted `TermGraphSource`, caller-owned trusted
`TypeStructure`, graph limits, certificate limits, and independent origin rows.
It builds and matches the table before sealing the graph. During that seal only
certificate-bearing visible witnesses are admitted provisionally; every
uncertified witness and every other type relationship still delegates to the
trusted structure. Before a graph can escape, the association checks the exact
owner scheme, base global, node/child/witness source and result types, specified
argument and selection, complete zero-based child chain, and exhaustive
one-origin-to-one-use coverage. All occurrence receipts are derived from the
rooted graph rather than supplied as caller coordinates.

The carrier co-owns its graph, structural table, normalized owner schemes, and
complete occurrence receipts. Projecting its graph produces only a bare legacy
observation and discards association authority. The narrow fold may hand
retainable verified observations to a package-private consumer, but cannot detach or
rebuild the checked table or carrier. Certificate IDs, term nodes, occurrences,
and fold order remain candidate-local coordinates; none may be treated as
provenance or enter a semantic key on that basis. No inventory or declaration
membership, binder kind, constraint discharge identity, behavioral
interpretation, or fingerprint authority is established here. Public graph
fingerprints therefore still reject stamped graphs. See the
[atomic association report](../docs/reports/2026-08-13-atomic-certificate-graph-associations.md).

Exference now consumes this private entrance for checker-owned, exact-scheme
direct-global source-telescope prefixes. Its checked annotations alone are
lowered to certificate handles and independent origin observations; a nonempty
origin table must seal as one opaque association or graph availability fails
closed. Certificate-free candidates retain the ordinary graph path. The
associated atom is stored in the hidden `TypedCandidate` carrier, while its
public graph projection is deliberately bare and remains rejected by the
public fingerprint entrance. Neither the handles nor Exference's sanitized
failure categories add inventory, kind, constraint-discharge, provenance,
behavioral, or fingerprint authority. See the
[Exference wiring report](../docs/reports/2026-08-13-exference-certificate-association-wiring.md).

`TypedGenerated.Fingerprint` reconstructs and reseals that graph with
`sharedTypeStructure` before assigning its nominal v1 identity. Its rooted-tree
encoding ignores table order and raw allocation numbers while preserving exact
binding structure, hole equality, flexible/rigid free-variable flavor, globals,
normalized types, patterns, witnesses, visible arguments, and branch order. It
performs no beta, eta, let, or behavioral quotienting. The retained encoding has
a caller-supplied byte bound (one MiB by default), while graph limits separately
bound its preceding traversal. The package-private carrier-aware entrance
literally delegates an empty association to this v1 path. For a nonempty atom,
it freshly reseals with the caller's exact `TypeStructure`, then builds v2 with
one shared variable-slot state across the rooted graph and rooted association
rows. Visible witnesses reference canonical row/step ordinals; rows encode the
exact owner, normalized scheme, source-order source/selected/result plan, and
ordered activated obligations. Certificate, node, occurrence, raw slot, and
input-row coordinates are validation-only and never enter the key. The public
entrance remains unchanged and rejects a projected stamped graph because graph
projection has discarded the opaque association authority. Constructor
patterns likewise require an inventory-bound family schema which the generic
shared checker does not possess. The result is only a structural graph key: it
establishes no inventory membership or provenance, kind correctness, constraint
discharge, candidate completeness, or behavioral meaning. Domain-owned sealers
must bind those authorities separately. Length now does so for the narrow exact
provider cases described above: either an obligation-free legacy row or one
conditional row whose own ground obligations, protected prefix, and final
occurrence have been authorized. It does not broaden the structural carrier's
own claims.

The detailed encoding and trust boundary are recorded in the
[carrier-aware certificate fingerprint report](../docs/reports/2026-08-13-carrier-aware-certificate-graph-fingerprints.md).

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
