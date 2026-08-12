# Djex

Djex is a Haskell expression synthesizer formed by merging
[Djinn](https://github.com/augustss/djinn) and
[Exference](https://github.com/lspitzner/exference) and adding support for rank-N types. Given a type, it
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
  `synthesis/src/`, `synthesis/internal/`, both `src-core/` roots,
  `djinn/src-internal/`, and both `src-frontend/` roots.
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
Its impredicative-field follow-up and checked wildcard projection are recorded
in the
[2026-08-01 impredicative recursive projection report](docs/reports/2026-08-01-impredicative-recursive-projection.md).
Djinn's complementary bounded recursive-constructor rule is recorded in the
[2026-08-01 bounded recursive introduction report](docs/reports/2026-08-01-bounded-djinn-recursive-introduction.md).
Djinn's widened bounded hypothesis-instantiation rule is recorded in the
[2026-08-01 four-binder instantiation report](docs/reports/2026-08-01-four-binder-instantiation.md).
The shared five-binder successor across Djinn, Exference, and exact provider
evidence is recorded in the
[2026-08-09 five-binder instantiation report](docs/reports/2026-08-09-five-binder-instantiation.md).
The shared six-binder successor preserves the same finite schedules and moves
the conservative boundary to seven in the
[2026-08-10 six-binder instantiation report](docs/reports/2026-08-10-six-binder-instantiation.md).
Its per-occurrence instantiation of loaded polymorphic values and closed source
monotypes is recorded in the
[2026-08-01 loaded polymorphic values report](docs/reports/2026-08-01-loaded-polymorphic-djinn-values.md).
The separate positive-only extension which instantiates query-local hypotheses
at closed monotypes already present in the requested type is recorded in the
[2026-08-09 query-local closed-monotype report](docs/reports/2026-08-09-query-local-closed-monotype-instantiation.md).
The complementary query-correlated guarded-impredicative tail, which fairly
selects a multi-binder tuple only when its complete specialized body already
occurs in the checked request, is recorded in the
[2026-08-09 query-correlated guarded-impredicative report](docs/reports/2026-08-09-query-correlated-guarded-impredicative-instantiation.md).
The checked provider-local instantiation evidence shared by the stable Djinn
and Exference adapters is recorded in the
[2026-08-05 provider-local instantiation evidence report](docs/reports/2026-08-05-provider-local-instantiation-evidence.md).
Its exact ordered-vector extension, which preserves correlations between the
leading binders selected by one external proof, and its later kind-aware form,
which retains caller-attested positional ground kinds even for vacuous binders,
are recorded in the
[2026-08-05 exact provider-instantiation assignment report](docs/reports/2026-08-05-exact-provider-instantiation-assignments.md).
Djinn's complementary nominal view of reachable parameterized datatypes is
recorded in the
[2026-08-01 nominal parametric-data transport report](docs/reports/2026-08-01-nominal-parametric-data-transport.md).
Djinn's capped quintic extension of its bounded rank-N plan family is recorded
in the
[2026-08-06 quintic rank-N frontiers report](docs/reports/2026-08-06-quintic-rank-n-frontiers.md),
following the
[2026-08-06 quartic rank-N frontiers report](docs/reports/2026-08-06-quartic-rank-n-frontiers.md),
the
[2026-08-01 triple rank-N frontiers report](docs/reports/2026-08-01-triple-rank-n-frontiers.md)
and the
[2026-07-31 pairwise rank-N frontiers report](docs/reports/2026-07-31-pairwise-rank-n-frontiers.md).
Its later proof-enumeration work is recorded in the
[oldest-first evidence report](docs/reports/2026-08-05-oldest-first-evidence.md)
and the
[repeated-domain fairness report](docs/reports/2026-08-05-repeated-domain-evidence-fairness.md).
Contextual goal introduction and its
lexical evidence boundary are recorded in the
[2026-07-29 contextual rank-N report](docs/reports/2026-07-29-contextual-rank-n-introduction.md).
The bounded generated-term and Exference provider-use extension is recorded in
the
[2026-07-29 visible type application report](docs/reports/2026-07-29-visible-type-application.md).
Its closed-polytype extension to scoped providers in both engines is recorded
in the
[2026-08-01 scoped closed-polytype application report](docs/reports/2026-08-01-scoped-closed-polytype-applications.md).
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

## Canonical typed candidate identities

Both checked engines now expose the same typed-result shape. Exference may
retain a sealed graph or an engine-specific absence reason.
`runDjinnTypedQuery` and its three provider-evidence variants retain each
post-deduplication, final-order Djinn candidate in the same opaque
`TypedCandidate` envelope, but currently report the explicit
`DjinnTermGraphSourceTypingContextUnavailable` absence. Djinn's checked LJT
sidecar predates assumption-name restoration, provider rewriting,
instantiation-evidence erasure, visible applications, and generated-term
cleanup; its formulas have also erased source nominal structure. Reconstructing
a shared graph from either rendered formulas or final generated code would
therefore invent authority.

The graph type is nevertheless fixed at the sound future boundary:
`DjinnTermGraphType = Type DjinnTermGraphTypeVariable`, where
`DjinnTermGraphTypeVariable = Variable HSymbol`, distinct from the historical
compatibility `DjinnType = Type DjinnTypeVariable`. A future lowerer must retain
whether an identity is flexible or rigid before Length or another behavioral
domain may authorize root skolems. Final candidate keys are already allocated
after cross-plan de-duplication and the configured final ordering step, so
later graph node and occurrence identities need not reuse discarded
search-plan ordinals. The four
legacy Djinn runners lift the shared `typedQueryResultCompatibility` projection
over their single-result error channel. Exference lifts the same per-result
projection over its lazy result trace. The helper maps only the opaque
candidate payload, so both paths preserve evidence, completion, metadata,
ordering, output, and diagnostics exactly without observing graph
availability.

`TermGraph` itself is nominal in both its type and local-identity parameters.
This remains necessary after `typedCandidateTermGraph` projects a graph out of
the already nominal `TypedCandidate`: representationally equal newtypes cannot
relabel the domains under which the graph was sealed. Its public `Generic`
instance has also been removed, so the private constructor cannot be recreated
through `GHC.Generics.to`; deep evaluation uses an explicit `NFData`
implementation instead. See the
[Djinn typed-result seam report](docs/reports/2026-08-11-djinn-typed-result-seam.md).

`Language.Haskell.Synthesis.TypedGenerated.Fingerprint` assigns an opaque,
nominal structural identity to a shared typed `TermGraph`. Before encoding, it
reconstructs the raw graph and reseals it with `sharedTypeStructure` under the
caller's explicit graph limits; an earlier seal performed with a different type
checker is not trusted. The canonical rooted-tree key ignores node-table order
and raw node, occurrence, local-binder, hole, and private type-variable
allocation numbers. It preserves lexical binding and hole equality classes,
flexible-versus-rigid free-variable flavor, exact global names, normalized node
and pattern types, term and shared-checkable pattern forms, application and visible-type
witnesses, inferred-versus-specified type arguments, and case-branch order.
There is deliberately no beta, eta, let, or behavioral quotienting.

Certificate allocation numbers are not identities. Until a checked certificate
table can supply the corresponding substitution and obligation fingerprints,
any certificate-bearing visible application is rejected. The shared checker
also lacks constructor-family schemas, so constructor-pattern graphs fail
closed until an inventory-bound sealer supplies that authority. Canonical
construction has an explicit retained-byte bound (one MiB by default), while
the preceding traversal is bounded separately by the supplied graph limits.

This fingerprint identifies only the checked shared graph. It does not resolve
globals against an `Inventory`, prove that holes or residual obligations are
absent, identify a complete behavioral problem, or provide behavioral evidence.
A domain-owned session or problem sealer must establish those facts and wrap the
canonical bytes in its own candidate identity.

`Language.Haskell.Synthesis.TypeInstantiation` supplies the missing checked
association for an implicitly specialized typed global. It matches one exact
context-free leading-forall source scheme against an actual type without ever
solving an actual-side variable. Selections may be impredicative and repeated
selections compare modulo lexical binder renaming. Nested binders are paired
with private skolems, which rejects an apparent match that would capture a
variable exposed only inside a nested `forall`. Length and future behavioral
domains can therefore apply their own rigid-opening or authorized-free-variable
policy to an opaque, source-ordered match rather than reusing an engine-private
unifier.

## Finite list-spine length contracts

`Language.Haskell.Synthesis.Semantic.Length` defines the first checked
behavioral-contract dialect in the engine-neutral foundation. It describes
total finite list-spine lengths over unbounded natural numbers with a small,
normalized expression and formula language. Construction is resource-bounded
before normalization or fingerprinting. A `CheckedLengthContext` retains the
exact opaque `Inventory` that supplies declaration and proper-kind authority,
together with its checked spine model. The model can be the versioned Haskell
`[]`/`(:)` structure or an exactly named unary datatype with one nullary and
one binary payload/recursive constructor. Contracts and provider inventories
sealed through that context remain opaque values available from the curated
`Language.Haskell.Djex` facade.

Provider summaries are an explicit trust boundary. A provider name must resolve
in the retained source inventory, and the caller's claimed scheme must be
alpha-equivalent to the closed scheme derived from that exact declaration. The
checked value stores the source-derived scheme, never the caller's copy. Each
summary is still only an assumed law, uniform over the instances of its closed,
context-free scheme; it is search guidance, not behavioral evidence. A spine
role says that the law may reference that argument's list length; it does not
require the normalized transfer to mention it. An unobserved role does not
assert purity, totality, strictness, absence of effects or type reflection, or
even that the provider will not evaluate the argument.

The identities deliberately remain split. Contract and provider-inventory
fingerprints include the exact checked spine model. A contract fingerprint also
identifies the normalized length relation and its ordered spine inputs, while
an inventory fingerprint identifies the exact normalized provider assumptions.
Opaque element types and caller-selected resource caps are not part of the
contract fingerprint. `Language.Haskell.Synthesis.Semantic.Length.Problem`
now seals a `CheckedLengthSession` atomically from one raw inventory, spine
model, and provider-summary source list. Its annotation-erased semantic
inventory fingerprint retains every neutral declaration, inferred kind fact,
the exact spine schema, and every normalized assumed provider law. Its separate
encoding-policy fingerprint identifies only the solver-neutral arithmetic,
normalization, and fail-closed candidate policy. It deliberately does not claim
the generic behavioral encoding role: a concrete encoding must additionally
bind a re-sealed contract and normalized interpreted candidate formula.

The smaller provider-inventory fingerprint still does not identify provider
implementations or the complete source inventory. The atomic session does, and
prevents a context checked from one inventory from being combined with provider
laws checked from another. It also reserves the modeled zero and step
constructor names from provider laws, avoiding an ambiguous semantic global.

`sealLengthTypedCandidateProblem` completes that association without accepting
a detachable raw graph. It consumes an engine-owned `TypedCandidate`, uses the
provider inventory already owned by the opaque checked session, re-seals the
separately supplied contract through the retained context, rejects the first
residual dictionary before inspecting graph availability, and freshly rechecks
and fingerprints the graph. The contract's
free flexible variables are treated as implicit source quantifiers only at the
root boundary; the engine's corresponding selections must be distinct rigid
variables. Every provider occurrence is then matched capture-safely against
its exact closed inventory scheme, and any free selected variable must be one
of those authorized root rigids. The interpreter carries that exact checked
summary through provider use and into the canonical used-law set; it does not
discard the authority to a name and recover it later. Closed impredicative
selections remain admissible. Every graph, pattern, application-witness, and
visible-application type is kind-checked again under the session's exact
inventory assumptions.
A visible selection is checked at the kind inferred for the leading binder,
so closed higher-kinded and impredicative selections remain legal while free
flexibles, non-root rigids, and types from a foreign inventory fail closed.

The first symbolic interpreter is deliberately narrow and lazy. It supports
locals, lambdas, application, certificate-free visible type application,
tuples, lets, bind/wildcard/tuple/as patterns, the checked zero and step
constructors, and checked provider transfers. Holes, cases, constructor
patterns, residual constraints, unknown globals, unmodeled inventory globals,
and certificate-bearing graphs fail closed. Explicit graph-byte and evaluation
step limits bound candidate work; the existing Length syntax budget jointly
bounds the normalized result and the counterexample condition
`precondition && not postcondition[result := interpretedResult]`.

A successful `CheckedLengthProblem` carries its source-ordered input arity,
normalized replay formulas, interpreted candidate, and one generic
`BehavioralProblem` envelope. That envelope is the sole field of the checked
problem which retains the inventory, concrete encoding, and complete problem
fingerprints; it also binds the candidate fingerprint retained by the
interpreted candidate receipt. The concrete encoding identifies the re-sealed
contract, normalized result and counterexample condition, interpreter policy,
and exactly the provider laws actually used. The candidate key wraps the fresh
shared graph identity and explicitly describes candidate-only authority. The
raw graph fingerprint is transient after those exact bytes enter that key, so
the receipt retains no parallel graph-identity field. It does not pretend to
retain batch completion status.

Contract arguments and results must expose the context's outer modeled spine;
their element types remain opaque and may themselves be impredicative. A
direct rank-N contract argument is rejected. Provider schemes are closed and
leading-context-free: spine-observed arguments and the result must use that
same modeled spine, while unobserved arguments may be non-spine or rank-N
values. `Language.Haskell.Synthesis.Semantic.Length.Evaluate` supplies bounded,
deterministic evaluation of one concrete assignment, assumed provider call, or
sealed candidate problem, including exact natural-number monus and
short-circuiting conditionals. Its three-way detached-contract result
distinguishes a failed precondition, a satisfied postcondition, and a violated
postcondition. Whole-problem replay accepts only source-ordered inputs, computes
the candidate result itself, and produces an opaque counterexample receipt
bound to the exact problem tuple only when the normalized bad-state formula is
true. Replay evaluates the retained precondition before the candidate and
postcondition, independent of canonical conjunction ordering. The receipt
explicitly distinguishes provider-independent finite-spine results from those
conditional on a listed set of fingerprinted provider assumptions; it does not
establish those implementations or realize the abstract model in a source
language with bottoms or effects. This is not universal behavioral evidence or
permission to prune other candidates. A future Z3 adapter can rank or challenge
candidates, but raw solver output is not trusted evidence without this
independent replay.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib` is a pure Z3-facing
translation boundary that seals an opaque nominal QF_LIA query from one exact
`CheckedLengthProblem`. It emits bounded canonical check commands and an
input-only `get-value` command, but launches no solver and assigns no authority
to `sat`, `unsat`, or `unknown`. `validateLengthSMTLibCounterexample` accepts
decoded integer bindings only for that query's input symbols and independently
replays them against the retained problem; raw model text and even `unsat`
remain heuristic observations, never pruning permission or proof.

The typed SMT plan remains transient through canonical rendering and
structural query fingerprinting. After that seal succeeds, the opaque query
retains only the checked problem, bounded check bytes, and complete fingerprint
needed by execution, association, and independent replay. Exact ordered input
symbols and optional canonical `get-value` bytes are rederived from the
problem's sealed arity; query sealing still constructs, bounds, and
structurally fingerprints both before discarding those parallel caches. The
structural `typed-plan` fingerprint field remains unchanged; the rendered
script is not promoted into the semantic source of truth.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Observation` closes the
remaining raw-report identity gap. Its opaque association binds bounded
status-specific artifacts to both the solver-neutral behavioral problem and
the exact canonical SMT-LIB query. The generic association retains the raw
observation once, and the outer Length association stores no additional status
or strength: both projections are derived from that single observation
constructor. Before exact replay, callers can inspect only status, query
identity, conservative result strength, and the fixed
`HeuristicRankingOnly` use; neither the raw payload nor the nested generic
association is exposed. Successful replay reveals only the still-raw report:
models still require independent validation, and `unsat` still proves nothing.
The query fingerprint is deliberately not a run or cache key. A future
executor must separately bind the exact Z3 build and capabilities, invocation,
protocol session and sentinel state, parser schema, artifact policy, deadlines,
cancellation, and resource limits. In particular, `unknown` must not be cached
solely by query identity.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response` adds the pure
response side without starting a process. It first retains one complete
response under a total byte bound, then applies an explicit-stack SMT-LIB 2.x
lexer and S-expression parser with independent nesting, node, token, and
integer-width limits. Token limits count source bytes, including both bytes of
a doubled quote. A versioned response-schema tag covers lexical,
normalization, and shape-decoding policy for a future execution key. The
public decoder admits only exact check statuses and
the input-only valuation shape requested by a particular Length query. It
normalizes quoted symbols, rejects malformed, missing, extra, duplicate,
unknown, wrong-sort, and unsolicited bindings, and restores source input
order. Parsed negative integers remain raw values for the existing natural
domain validator to reject. Parsed statuses are observations only. A private
stream and protocol layer now supplies bounded lexical framing, exact echo
markers, and fail-closed phase sequencing. The package-private live Session now
owns worker identity, deadlines, recovery, query-specific execution, and raw
observation association. Only independent Length replay can create
model-relative counterexample evidence.

`Language.Haskell.Synthesis.Internal.SMTLib.Stream` frames one exact SMT-LIB
2.7 response incrementally without line-based assumptions. It carries lexical
state across arbitrary chunks for doubled-quote strings, quoted symbols,
comments, bare responses, and nested lists; bounds all consumed trivia,
retained frame bytes, and nesting; and returns the original post-frame tail
after at most one byte of lexical lookahead.
Bare and quoted-symbol responses are not complete until the required following
whitespace arrives. Package-owned echo sentinels encode exactly 32 nonce bytes
as lowercase hex, and include the standard-required surrounding quotes in both
the command and expected response. Exact comparison never scans strings,
comments, symbols, or nested expressions for marker text. Completed frames now
report their exact charged byte count as well as the untouched tail so a
composing transaction can enforce a cumulative stdout budget. Framing alone
is still not a protocol receipt. Because a
top-level string needs one-byte lookahead to distinguish a doubled quote, the
live Z3 capability probe must also establish that `echo` emits and flushes a
trailing byte (normally its newline) after the marker's closing quote.

`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Stream` is the shared pure
transaction layer above that single-frame framer. One opaque policy owns the
configured frame limits and cumulative maximum. Its zero-start cursor keeps
the absolute charged offset and effective remaining frame budget together;
opaque completed frames carry their hidden tails into an associated same-write
frame, while validated boundaries carry the same policy and exact charged
offset into a next-write receiver. Protocol and readiness therefore cannot
restart a detached tail under a different offset or budget.
The configured frame-total failure still wins an exact tie, while a strictly
tighter cumulative budget reports its established maximum-plus-one failure.
The base framer also owns the canonical ordered SMT-LIB whitespace vocabulary
used by this cursor, causal transport attribution, and both plan fingerprints.

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol` composes
that cumulative cursor with the sealed execution policy and query. Its initial
action is one exact reset/check/status-marker write. It decodes exactly one
status and accepts the marker only in the following position; only `sat` under the
input-value artifact policy may then expose a second value-request/marker
write. Status and value tails are recursively consumed only within the write
which could have caused them. At the status-marker-to-value-write boundary,
already-buffered bytes must be finitely bounded SMT-LIB whitespace, so a stale
valid-looking valuation cannot cross that causal boundary. Every framing,
decoder, marker, cumulative-output, unexpected-byte, or EOF failure returns no
successor. The live query owner treats every such transaction as spent and
discards the worker.
The private plan key binds policy, query, framing limits, cumulative budget,
phase schema, exact writes, and marker responses. This remains a pure,
caller-feedable protocol decode—not an executed observation, attestation, or
receipt. A live session must still generate barriers uniquely across its
lifetime, enforce writes, capability-probe the process, and bind the actual
transcript into a separate run identity; the Session described below now owns
those obligations.

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session` now
provides that package-private live ownership checkpoint. It samples separate
secret barrier and public workspace material, launches the exact configured
argv with an empty environment in a fresh directory, and lends the resulting
worker only through a rank-N callback. On POSIX the workspace is observed
through a retained no-follow directory descriptor and matched by device,
inode, owner, mode, and canonical path; cleanup never traverses contents and
attempts only identity-checked empty-directory removal. The portable Windows
fallback explicitly claims only repeated pathname observations.

The process owner bounds a pre-spawn SHA-256 observation of the configured
executable file, compares an optional pin, owns separate stdout/stderr pipes,
poisons on the first stderr byte, and continues draining a finite stderr flood
so cleanup cannot deadlock. FIFO stdout, absolute monotonic deadlines,
cancellation, isolated writes, staged direct-child shutdown, and idempotent
cleanup are all retained in private policy identity. Shutdown uses bounded
nonblocking exit polling, so it also progresses in a non-threaded runtime.
This is not executed-image attestation: pathname hashing plus portable direct
spawn has a same-UID namespace race, and neither the loader nor shared
libraries are measured. Descendant cleanup is best effort after the direct
child exits.

Readiness requires four causally separated writes and fresh positional echo
barriers. The probe checks startup print suppression; reset/replay with an
`input = 0` satisfiable problem; exact input valuation; and a second reset with
contradictory zero/one assertions producing `unsat`. Delimiter and boundary
whitespace are charged once and canonically retained with the preceding write,
including a separately delivered final newline. The ready-worker identity
binds the pure execution policy, process observation method, exact segmented
capability transcript, secret-seed commitment, workspace policy, and configured
live-query limits. Readiness itself creates no solver observation or evidence.
The ownership and threat-model details are recorded in the
[2026-08-11 scoped worker lease report](docs/reports/2026-08-11-z3-worker-lease.md).

The same Session now owns ordinal-bound live queries through the generic private
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver`; its Length-specific
`...Session.Transport` adapter binds one process, cancellation token, and
absolute deadline as a single transport handle. A masked serial gate allocates
zero-based ordinals and two HMAC-SHA256 marker roles from the unexposed session
seed, checks all markers against a bounded lease-wide set, and seals the exact
pure protocol plan before reservation. The shared causal driver writes before
activating each receiver, attributes delayed predecessor whitespace exactly
once, and requires exact stdout-delta and stderr accounting. Every marker,
protocol, transport, replay, or identity failure after reservation spends the
ordinal, cancels the lease, and closes the process; plan, capacity,
identity-admission, and query-count rejections before reservation remain
non-mutating.

Successful query runs retain an opaque nominal reversible identity over the
ready worker, plan, ordinal, spent markers, absolute deadline, exact segmented
transcript, decoded branch, replay policy, and transport counters. `sat` under
the input-value policy yields counterexample evidence only after independent
Length replay under explicit evaluation limits. Status-only `sat`, `unsat`,
and `unknown` remain heuristic observations and grant no pruning authority.
The exact design and threat boundary are recorded in the
[2026-08-11 ordinal-bound query-run report](docs/reports/2026-08-11-z3-query-runs.md).

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live` is the deliberately
narrow public edge over that owner. It lends an opaque worker only through a
rank-N scope. Each successful query observation internally retains its exact
query fingerprint, three-valued status, and optional independently replayed
counterexample evidence. Its heuristic strength is derived from the retained
status rather than stored as a second fact; public selectors expose status,
that derived strength, and heuristic use. The fingerprint and evidence have
no detached projection: `replayLengthSMTLibLiveQueryObservation` is their only
public consumer. It checks the exact query fingerprint before inspecting
optional evidence, then replays that evidence against the query's retained
behavioral problem. A successful `Nothing` remains only an exactly associated
heuristic status. Process handles, cancellation, paths, executable
observations, barriers, ordinals, decoded valuations, transcripts, transport
counters, and reversible run identities remain private. Public session and
query execution failures are mapped to byte-free classes plus a
cleanup-incomplete bit; the pure replay gate returns its own closed byte-free
association error.
Child-controlled payloads and operating-system details never cross the facade.
The private session opener and configured per-query deadlines remain separate
budgets rather than a claimed hard deadline for a caller-defined batch.

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution` now seals the
pure Z3 launch and protocol policy without launching anything. V2 fixes the
direct prefix `-in -smt2 smtlib2_compliant=true`, derives exact launch-time
`timeout` and `rlimit` arguments from the validated policy, uses an empty child
environment and a fresh empty working directory, and retains a bounded
absolute executable path plus an optional exact 32-byte SHA-256 pin. Standard
compliance makes `echo` responses quoted; exact startup bytes immediately
disable `:print-success`, and the reset prefix repeats that suppression before
every self-contained query. The canonical QF_LIA query now emits start-mode
options before `set-logic` and uses fixed nonzero random seed `1`.
The legacy `lengthSMTLibExecutionArgumentVector` projection names only the
fixed prefix; launchers must use
`lengthSMTLibExecutionConfiguredArgumentVector` for the complete argv.

The package-private complete fingerprint binds the protocol schema, complete
argv, exact startup and reset bytes, every retained execution field, the
response schema, and all five response bounds; public callers see only safe
operational projections. Admission limits are not execution semantics. This
is configuration, not a run receipt: it performs no path resolution, hashing,
spawn, version or capability probe, stream framing, or process observation.
The package-private live Session separately verifies that the selected worker
exhibits the required quoted-echo, print-suppression, reset, valuation, and
contradictory-check behavior before accepting it.
SHA-256 names an external executable-file pin and is not a collision-free
identity for the executable's unbounded bytes. Parsed statuses still have no
pruning or proof authority.

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
context-free `forall` of up to six leading binders. Its historical family
instantiates the complete chain at sequent-supplied candidates: the goal's type
variables, skolems of opened positive occurrences, premise-scope variables,
and — as guarded impredicativity — query subtrees that are independent of
enclosing binders and contain quantification.

A separate positive-only query-correlated tail fairly revisits that same finite
variable and guarded-quantified vocabulary. It keeps only tuples which pair a
quantified candidate with a binder occurring free in the scheme body and
specialize that complete body to an alpha-equivalent subtree of the elaborated
query. Seeding its builder with the active historical axioms suppresses
duplicate logical formulas without blocking nested schemes discovered through
an excluded bridge. The fair producer observes at most 512 raw tuples per
scheme; the family builder separately charges at most 512 eligible attempts,
sixteen retained axioms per scheme, and 64 retained axioms in total.

The established query-closed positive-only family revisits only schemes
embedded on the hypothesis side of the requested goal. It adds closed,
forall-free monotype subtrees already present in that elaborated goal, retains
only tuples using at least one such closed candidate, and permits mixed tuples
with the historical candidates without changing the historical prefix. A
different appended family retains exact context-free schemes from loaded values
and may also use closed, forall-free subtrees of the query and synonym-expanded
loaded value signatures. Ordinary workspace values enter that loaded family
only after `:set djinn-axioms on`; value axioms are off by default. Every
substituted body is kind-checked, so a closed higher-kinded constructor is
admitted only when its complete use has kind `Type`. The query-correlated,
query-closed, and loaded extensions are bounded and positive-only; a missed
non-target retained loaded scheme makes the result inconclusive rather than
proving non-inhabitation. Exference can introduce a nested
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
For an instantiable scoped or retained global provider, a separate bounded
branch can make the choice visible. A direct constraint may select its complete
leading binder prefix from an explicit ground instance head, producing an
application such as `provider @Int`. A context-free provider with no free
flexible variables whose leading binders are all vacuous may instead select
checked proper types already supplied below arrows or tuples in the query. Its
residual body may mention ambient rigids opened from that same query. The
candidates include complete closed context-free foralls, so search can emit
`provider @(forall a0_0. a0_0 -> a0_0)`. The query route retains at most six
binders and 32 combinations. Ordinary implicit instantiation remains first,
and the instance-head route remains monotype-only.
A richer frontend may instead pass either backend one complete, correlated
provider-assignment vector. The legacy `ProviderInstantiationAssignment`
runners infer each positional ground kind from the exact provider body and
default an unconstrained vacuous binder to `Type`. The parallel
`KindedProviderInstantiationAssignment` runners,
`runDjinnQueryWithKindedInstantiationAssignments` and
`runExferenceQueryWithKindedInstantiationAssignments`, pair every argument
with a caller-attested `GroundKind`. Each adapter checks those supplied kinds
against all observable uses in the retained body, elaborates every argument at
its paired kind, requires repeated assignments for the same provider to agree
on the complete kind vector, and still checks the fully specialized body at
`Type`. For each assignment that passes provider, scheme, and exact-arity
checks, all its supplied kinds receive a productive node preflight under the
public `maximumProviderInstantiationKindNodes = 129` bound. That preflight
precedes the assignment's kind inference, same-provider kind-vector equality,
backend kind conversion, and paired-type forcing; cyclic kinds and finite trees
above the bound are therefore rejected without traversing an unbounded
remainder. The shared 64-tuple constructor's right-associated all-`Type` kind
has exactly 129 nodes and remains accepted.

A vacuous binder supplies no body constraint from which its source kind could
be inferred; its higher kind is therefore frontend evidence, not a backend
inference. This admits a bare or partially applied higher-kinded constructor at
such a position, including vectors that also contain a closed impredicative
`Type` argument. An exact argument must be lexically closed but may itself be a
contextual polytype such as `forall a. Eq a => a -> a`; that nested context is
part of the selected type, not an obligation of the provider scheme. Multiple
distinct vectors may be retained for the same provider when their complete
kind vectors agree; regressions exercise two such
choices at the genuinely higher-order kind `(Type -> Type) -> Type`. Both
assignment forms retain the 32-vector and six-argument bounds, the kinded form
adds the 129-node per-kind bound, and contextual provider schemes remain
unsupported. The legacy scalar `ProviderInstantiationCandidate` entrances
remain proper-type-only.

Djinn's checked lowering deliberately recognizes one residual tuple head in
that higher-kinded evidence: the bare boxed arity-two constructor is preserved
and rendered canonically as `(,)`. A saturated pair such as `(Natural,
Boolean)` remains the existing structural `TupleType`; the exception does not
broaden tuple support generally. Bare or partially applied wider boxed tuple
constructors, and bare or partially applied unboxed tuple constructors, remain
fail-closed with `PartialTupleConstructorUnsupported`. Focused `djinn-tests` pin
the bare and partial pair adapter, the negative tuple boundary, non-vacuous
exact provider substitution, and contextual rank-N use. A `djex-tests` facade
regression sends `(,)` and `Either Natural` through both engines, renders their
evidence, and asks GHC to check the combined generated fixture.

Djinn marks each complete legacy scalar tuple or exact vector before
substituting it into a provider and admits the resulting non-vacuous premise to
its constructor-expanding search projection only when every reached datatype
argument remains observable through its own formal. That existential
observation is a boundary gate; Djinn then traverses the instantiated
constructor body and requires every marker-bearing field to preserve the
argument at each nested datatype boundary. This checks structure inside an
assigned type as well as later arguments applied to an assigned higher-kinded
head. It preserves useful elimination through fully faithful and correlated
constructor fields while routing wholly phantom parameters and any
independently erased occurrence through the nominal projection alone.
Recursive, unknown, and parameterized empty types retain the opaque
complete-application identity used by compilation.
Consequently structural equality cannot turn a selected visible application
into a nominally different one after phantom information is erased.
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
:compare (forall a b. f a b) -> f (forall x. x -> x) (forall y. y -> y -> y)
:exference ((forall a. a -> a) -> result) -> result
:exference (forall a. a -> a) -> Int -> Int
:exference (forall a b. a -> b -> a) -> (forall x. x -> x -> x)
```

Nonrecursive declared datatypes remain fully structural in Djinn: their
constructor sums support introduction and case elimination and stay first in
search. Recursive datatypes use a narrower structural view. A positive logical
path may expose one real constructor layer from each of at most two distinct
alias-normalized recursive SCCs. Reopening the same SCC through direct, mutual,
alias-hidden, or parameter-mediated recursion, reaching a third SCC, every
negative occurrence, and the exact-opaque view all retain the complete
application as an atom. This lets an independent `Outer` and `Inner` compose
one finite layer each while preventing unbounded or exponentially duplicated
expansion. The exact view preserves forwarding such as `Rec a -> Rec a`, and
the positive view can construct `Done a :: Rec a` without admitting recursive
elimination, recursive calls, or induction. Every query translation that
touches this bounded rule is incomplete, so an empty search is inconclusive
rather than proof of non-inhabitation.

A query-directed backward slice additionally identifies reachable applications
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

These nominal candidates follow the complete structural prefix. Use
`:set select all` (or `:set select best`) to inspect them when `EmptyD` or
`R` already supplies an earlier structural inhabitant; the interactive default
`select = first` may stop before the nominal family.

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

Inferable instantiation evidence is erased only after independent proof
checking. When a selected leading binder is vacuous, conversion instead
retains the shortest visible prefix, using `@_` for earlier inferable positions
and a specified closed argument—including a quantified one—for the selected
choice. Every proof that consumes instantiation evidence uses conservative
no-eta conversion. It retains the lambda when erasure would expose a
higher-rank application boundary under GHC's simplified subsumption rules.
Consequently, the second
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
general visible type application. Explicit open arguments such as `@a` remain
unsupported. A closed quantified argument is admitted only through the bounded
query-supplied routes above; neither backend invents a polytype. Djinn can
retain the chosen application for a vacuous query-local or loaded scheme, while
its historical `HExpr` compatibility projection still rejects the shared node
explicitly. Unsupported Djinn positions remain opaque and make
an otherwise empty search inconclusive rather than manufacturing a logical
refutation. Exference still does not perform non-exact subsumption between
contextual schemes; quantified types outside an exposed goal/provider boundary
remain opaque. Finite identifier or search-budget exhaustion is truncation, not
negative evidence.
Djinn searches a quartic historical family followed by a capped quintic tail.
Its prefix remains the fully opened polarized plan, the exact-opaque plan, and
the two singleton frontiers: one positive `forall` stays opaque while its
siblings open, or one occurrence opens while unrelated siblings remain opaque.
A deterministic tail then makes the same choices for each unordered pair,
triple, quadruple, and bounded quintuple of sites. Quintuple-opaque plans begin
at ten sites and quintuple-open plans begin at eleven. Each orientation retains
at most 512 selections, alternated stably from both source-order edges, so the
new layer contributes at most 1,024 formula views on larger inputs.
Opening nested occurrences also opens the union of their enclosing chains.
Loaded functions expose those sound views together, so a reusable premise can
be consumed at different views in one proof. All 252 five-site selections at
ten sites and all 462 at eleven fit below the cap, making the family exhaustive
for eleven independent sites without a general power-set search. Twelve sites
expose the next central boundary: a proof requiring exactly six open and six
opaque occurrences may remain inconclusive. After that complete structural
no-axiom prefix, bounded instantiation plans cover many omitted middle subsets,
but chains beyond six binders, constrained chains, and candidates outside the
finite query/value-signature vocabulary stay out of reach. Each structural or
nominal instantiation family is capped per scheme and per family. Four-,
five-, and six-binder historical query-local tuple selection fairly mixes source-order,
repeated, sparse, and Cartesian shapes while one- through three-binder schemes
retain their historical order. The appended loaded-value family uses the same
tuple shapes while alternating both ends of its source-ordered candidate list.
The established query-closed structural and optional nominal plans keep their
position after those loaded and provider plans. They schedule the combined
historical and closed-query pool and retain only tuples containing a closed
query candidate. Pure query-correlated structural and optional nominal plans
follow, carrying historical, loaded, and provider premises but leaving the
query-closed family unchanged. A final combined superset is present only when
both families contribute axioms, allowing one proof to compose their instances
without duplicating either single-family plan. Every added plan contributes
candidates but no negative evidence. Those caps lose completeness only, never
soundness.
The nominal parametric-datatype plans obey the same caps and add no negative
evidence. An incomplete primary premise also makes negative evidence
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
variables, making their common ground-kind contract visible in types. A Djinn
session preserves explicit `GroundKind` annotations on shared class parameters,
including methodless higher-kinded classes, while using an unannotated copy
only for the historical `ClassDecl` lexical preflight. The standalone
shared-to-legacy declaration conversion remains strict because that legacy
syntax cannot represent the annotation.

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
formula compilers, premise caches, reachability index, and exact recursive-data
classification; the mutable raw
`Djinn.Core.Environment` never crosses the curated facade. Synonyms are
expanded for saturation and recursive datatype classification before ordered
global assumptions are translated once into proof premises. The sealed
environment retains the prepared Inventory/synonym witness, the foundation's
annotation-free class index, structural and nominal formula compilers and
premise views, and the nominal reachability index; historical raw declaration
tables are derived only when a compatibility caller asks to display them.

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
Djinn retains visible recursive constructors for bounded introduction: at most
two distinct alias-normalized recursive SCCs may expose one positive
constructor layer each on a logical path. Same-SCC revisits, a third SCC,
negative occurrences, and the exact-opaque view remain atomic. Its exact
fallback preserves recursive identity, and any search that encounters the
bounded projection withholds negative evidence when it finds no term.
Exference retains recursive datatype eliminators under a finite rule: matching
a recursive scrutinee exposes one constructor layer, and its fields become
ordinary providers in that branch without being fed back into eager pattern
decomposition. This constructs finite case expressions, not recursive calls or
induction. Multiple-constructor recursive types still require the existing
multiple-pattern opt-in, so their search-space cost remains explicit. A field
specialized to a quantified type remains available at that exact type. When a
one-layer projection deliberately ignores the recursive tail,
`exferenceAllowUnused` must be enabled; after the fully annotated term passes
independent checking, the stable generated candidate renders that unused field
as `_`.

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
or a structurally valid, lexically closed type and renders with Haskell's
required type-argument parentheses. Quantified binders are alpha-normalized;
`visibleTypeArgumentClosedType` recovers the complete type while
`visibleTypeArgumentType` remains the monotype compatibility projection.
Compiling such a candidate requires `TypeApplications`; a quantified argument
generally also requires `RankNTypes` and `ImpredicativeTypes`, and an ambiguous
contextual signature may require `AllowAmbiguousTypes`. Djinn's LJT lowering
constructs and simplifies that
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
