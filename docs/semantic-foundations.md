# Semantic foundations

*The design detail behind Djex's checked semantic stratum: typed candidate
identities, ground class resolution, and the finite list-spine Length
behavioral domain with its SMT-LIB/Z3 live stack. The [README](../README.md)
gives the one-paragraph orientation and the
[architecture guide](architecture.md) the ownership map; this document is
the specification, moved out of the README and maintained here since.*

## What this stratum is, in one screen

Beside the two search engines, Djex carries a backend-neutral layer that
never runs a search. It answers a different question: given a candidate the
engines already produced and independently type-checked, *what can be
said about its behavior*, and *what identity does that claim attach to*?

- **Typed candidate identities** make the answer attachable. Every candidate
  can carry a sealed, typed term graph with stable node, occurrence, and
  certificate identities, and a `Fingerprint` — a complete versioned
  canonical encoding, not a lossy hash — names each artifact.
- **Ground class resolution** discharges the class obligations such a
  candidate carries against an exact inventory, so a behavioral claim never
  silently assumes a dictionary.
- **Length contracts** are the first behavioral dialect: a law over the
  lengths of list-spine inputs and results, checked into a bounded canonical
  `QF_LIA` query and, optionally, put to a scoped Z3 worker. Solver output is
  never trusted directly — only independent replay against the exact checked
  problem produces evidence, and raw `sat`/`unsat`/`unknown` has no proof or
  pruning authority.

Every section below is a specification and reads as one. The dated reports
indexed at [reports/README.md](reports/README.md) record how each boundary
was reached.

---

## Contents

- [What this stratum is, in one screen](#what-this-stratum-is-in-one-screen)
- [Canonical typed candidate identities](#canonical-typed-candidate-identities)
  - [Graph fingerprints](#graph-fingerprints)
  - [Certificate tables and atomic graph associations](#certificate-tables-and-atomic-graph-associations)
  - [The typed-candidate certificate carrier and Exference wiring](#the-typed-candidate-certificate-carrier-and-exference-wiring)
  - [Carrier-aware fingerprints and their limits](#carrier-aware-fingerprints-and-their-limits)
  - [Checked type instantiation](#checked-type-instantiation)
- [Checked ground class-resolution foundation](#checked-ground-class-resolution-foundation)
- [Finite list-spine length contracts](#finite-list-spine-length-contracts)
  - [Bounded where-clause surface syntax](#bounded-where-clause-surface-syntax)
  - [Provider summaries as a trust boundary](#provider-summaries-as-a-trust-boundary)
  - [Split contract, inventory, and session identities](#split-contract-inventory-and-session-identities)
  - [Sealing typed-candidate problems and provider associations](#sealing-typed-candidate-problems-and-provider-associations)
  - [The symbolic interpreter and target-argument roles](#the-symbolic-interpreter-and-target-argument-roles)
  - [Exact-case and unified interpretation policies](#exact-case-and-unified-interpretation-policies)
  - [Case semantics and fail-closed boundaries](#case-semantics-and-fail-closed-boundaries)
  - [Checked problems and candidate keys](#checked-problems-and-candidate-keys)
  - [Candidate-independent counterexample-bank scopes and bounded stores](#candidate-independent-counterexample-bank-scopes-and-bounded-stores)
  - [Spine exposure and bounded concrete evaluation](#spine-exposure-and-bounded-concrete-evaluation)
  - [The versioned identity families at a glance](#the-versioned-identity-families-at-a-glance)
  - [Offline SMT-LIB queries, replay, and origin probes](#offline-smt-lib-queries-replay-and-origin-probes)
  - [Query-owned bounded counterexample simplification](#query-owned-bounded-counterexample-simplification)
  - [Bounded input-box validation](#bounded-input-box-validation)
  - [Current guarded recursive piecewise-affine applicable-domain validation](#current-guarded-recursive-piecewise-affine-applicable-domain-validation)
    - [Private ordered analysis and atomic-first fallback](#private-ordered-analysis-and-atomic-first-fallback)
    - [Guarded `LengthIf` expansion](#guarded-lengthif-expansion)
    - [Boolean admission, boxes, and replay](#boolean-admission-boxes-and-replay)
    - [Receipts and authority](#receipts-and-authority)
  - [Finite binary product spine lengths, offline and live SMT replay](#finite-binary-product-spine-lengths-offline-and-live-smt-replay)
    - [Offline product SMT queries and replay](#offline-product-smt-queries-and-replay)
    - [Live product queries](#live-product-queries)
    - [Shared live usable-work budget](#shared-live-usable-work-budget)
  - [Linear-arithmetic lowering and the SMT-LIB response stack](#linear-arithmetic-lowering-and-the-smt-lib-response-stack)
    - [Shared typed SMT-LIB syntax and renderer](#shared-typed-smt-lib-syntax-and-renderer)
    - [Raw-report observation association](#raw-report-observation-association)
    - [Bounded response parsing](#bounded-response-parsing)
    - [Lexical whitespace and incremental framing](#lexical-whitespace-and-incremental-framing)
    - [Causal transaction cursor](#causal-transaction-cursor)
  - [Live Z3 protocol, session, and process ownership](#live-z3-protocol-session-and-process-ownership)
    - [Scoped live session ownership](#scoped-live-session-ownership)
    - [Shared raw Z3 process owner](#shared-raw-z3-process-owner)
    - [Resource acquisition and descriptor-child handoff](#resource-acquisition-and-descriptor-child-handoff)
    - [Descriptor-bound Z3 executable launch](#descriptor-bound-z3-executable-launch)
    - [Effective-ID executable-access descriptor launch](#effective-id-executable-access-descriptor-launch)
    - [Derived Length process identity](#derived-length-process-identity)
    - [Worker readiness and identity](#worker-readiness-and-identity)
    - [Ordinal-bound live query runs](#ordinal-bound-live-query-runs)
    - [The public live facade](#the-public-live-facade)
    - [Shared execution profile and complete policy identity](#shared-execution-profile-and-complete-policy-identity)
- [Length module narrative](#length-module-narrative)
  - [Contract construction paths and target-argument roles](#contract-construction-paths-and-target-argument-roles)
  - [Unified interpretation policy](#unified-interpretation-policy)
  - [Exact zero/step case sealers](#exact-zerostep-case-sealers)
  - [Associated-certificate carriers](#associated-certificate-carriers)
  - [Conditional provider summaries and ground discharge](#conditional-provider-summaries-and-ground-discharge)
  - [Retention identities and schema versions](#retention-identities-and-schema-versions)
  - [Semantic.Length.CounterexampleBank](#semanticlengthcounterexamplebank)
  - [Semantic.Length.SMTLib](#semanticlengthsmtlib)
    - [The canonical query and raw-input replay](#the-canonical-query-and-raw-input-replay)
    - [Counterexample-bank query replay](#counterexample-bank-query-replay)
    - [The all-zero origin probe](#the-all-zero-origin-probe)
    - [Exhaustive input-box validation](#exhaustive-input-box-validation)
    - [Current guarded recursive piecewise-affine coverage policy](#current-guarded-recursive-piecewise-affine-coverage-policy)
    - [Finite binary product spine domains](#finite-binary-product-spine-domains)
    - [The offline product query](#the-offline-product-query)
    - [Live product runs and the shared ordinal space](#live-product-runs-and-the-shared-ordinal-space)
    - [Euclidean witnesses and the shared typed QF_LIA plan](#euclidean-witnesses-and-the-shared-typed-qf_lia-plan)
  - [Semantic.Length.SMTLib.Observation](#semanticlengthsmtlibobservation)
  - [Semantic.Length.SMTLib.Response](#semanticlengthsmtlibresponse)
  - [Internal.SMTLib.Stream](#internalsmtlibstream)
  - [Internal.SMTLib.Causal.Stream](#internalsmtlibcausalstream)
  - [Internal.Semantic.Length.SMTLib.Protocol](#internalsemanticlengthsmtlibprotocol)
  - [Internal.Semantic.Length.SMTLib.Session](#internalsemanticlengthsmtlibsession)
    - [The shared Z3 process layer](#the-shared-z3-process-layer)
    - [The capability probe and ready-worker identity](#the-capability-probe-and-ready-worker-identity)
    - [Retained worker policy and run commitment](#retained-worker-policy-and-run-commitment)
    - [Live facade observation replay](#live-facade-observation-replay)
    - [Usable-work budgets and scoped deadlines](#usable-work-budgets-and-scoped-deadlines)
    - [Deadline coverage, expiry, and identities](#deadline-coverage-expiry-and-identities)
  - [Internal.SMTLib.Z3.Execution and Semantic.Length.SMTLib.Execution](#internalsmtlibz3execution-and-semanticlengthsmtlibexecution)
    - [Execution-policy construction and fingerprint](#execution-policy-construction-and-fingerprint)
    - [Descriptor-bound main-image launch](#descriptor-bound-main-image-launch)
    - [Effective-ID executable-access launch](#effective-id-executable-access-launch)
    - [Execve-check executable-access launch](#execve-check-executable-access-launch)
  - [Generated and TypedGenerated](#generated-and-typedgenerated)
    - [Internal.TypedGenerated.Certificate](#internaltypedgeneratedcertificate)
    - [Internal.TypedGenerated.Certificate.Association](#internaltypedgeneratedcertificateassociation)
    - [TypedGenerated.Fingerprint](#typedgeneratedfingerprint)
  - [Observability](#observability)

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
[Djinn typed-result seam report](reports/2026-08-11-djinn-typed-result-seam.md).

### Graph fingerprints

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

### Certificate tables and atomic graph associations

Certificate allocation numbers are not identities. The package-private
`Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate` foundation
now seals a bounded table of complete leading-telescope selections and derives
their capture-free intermediate types and ordered source-syntax obligations.
It deliberately proves only structural replay: it proves no inventory or
provider provenance, closure, binder kind, constraint resolution or discharge,
or association with a particular graph occurrence.

The package-private
`Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association`
entrance provides the next atomic foundation. It consumes a raw graph source,
trusted caller-owned type structure and graph limits, and independent checker
origin observations. In one seal it rebuilds the certificate table, matches
every observed source, selected, result, and activated-obligation field, seals
the graph, and derives the unique base global plus complete visible occurrence
chain exclusively from stamped `(CertificateId, slot)` handles and child links.
Missing, duplicate, out-of-range, noncontiguous, or unused rows fail closed.
Certificate, node, and occurrence values remain candidate-local coordinates,
not provenance.

The opaque result co-owns graph, table, exact owner scheme, and occurrence
receipts. Its bare graph projection loses that association authority; its
package-private fold exposes retainable checked observations but cannot
reconstruct or recombine the carrier or structural-table authority. It still
proves no inventory membership, positional kind, discharge identity,
behavioral meaning, or fingerprint permission. The public graph fingerprint
therefore continues to reject every certificate-bearing visible application;
the generic association foundation alone does not stamp an engine graph or
alter public bytes. See the
[atomic certificate association report](reports/2026-08-13-atomic-certificate-graph-associations.md).

### The typed-candidate certificate carrier and Exference wiring

The package-private `TypedCandidate` representation can now retain that opaque
atom without changing its public four-parameter type or either public
projection. Its hidden graph state distinguishes unavailable, ordinary sealed
graph, and certificate-associated graph. One lazy three-way constructor keeps
the complete availability decision below compatibility projection, and one
private eliminator gives each branch the compatibility candidate beside its
carrier. Trusted engine adapters still own the obligation to pair the exact
checked compatibility value; `TypedCandidate` itself is not a pairing proof.

Public `Eq`, `Ord`, and `Show` intentionally retain their exact historical
observation by erasing an associated atom to its bare graph. A plain and an
associated candidate with the same compatibility value and graph therefore
compare equal and render identically. Ranking and deduplication must finish
before attaching association authority, and authority-bearing typed candidates
must not use equality- or ordering-based containers as authority storage: such
containers may keep the plain representative and discard the carrier. Deep
`NFData` evaluation does force the full retained carrier, while public graph
projection remains shallow and loses association authority. See the
[typed-candidate certificate-carrier report](reports/2026-08-13-typed-candidate-certificate-carrier.md).

Exference now joins those two private foundations for the narrow origin class
proved by its independent checker. Checker-owned direct-global source-prefix
annotations become `(CertificateId, slot)` handles; any nonempty origin set
must pass atomic table, graph, owner-scheme, and complete-occurrence-chain
association or the graph fails closed. Untagged local, inferred, partial,
fallback, oversized, and returned-polytype-suffix applications retain their
legacy graph rules. The final typed-candidate projection uses the single lazy
three-way carrier entrance, so reading compatibility does not inspect graph or
association availability.

The public bare-graph projection of a successful associated candidate now
contains those handles, but it discards the opaque table and receipt carrier;
public fingerprinting therefore rejects the stamped graph. Handles are
candidate-local diagnostic coordinates; the sanitized association-failure
summaries are diagnostics only. Neither grants inventory, kind, discharge,
provenance, behavioral, or fingerprint authority.
The additive public failure vocabulary is documented in the
[Exference certificate-wiring report](reports/2026-08-13-exference-certificate-association-wiring.md).

### Carrier-aware fingerprints and their limits

A new package-private carrier-aware fingerprint entrance can consume the
opaque association atom directly. Empty atoms literally delegate to the
existing v1 graph fingerprint, preserving its bytes, failures, and demand.
Nonempty atoms are freshly resealed under a caller-owned `TypeStructure` and
receive a v2 structural key: the rooted graph refers to canonical row/step
ordinals, then each rooted row records the exact owner, normalized scheme,
source-order substitution plan, and ordered activated constraints. One shared
variable-slot state spans root then rows. Certificate, node, occurrence, raw
slot, and caller row-order coordinates never enter the key. The public graph
fingerprint remains unchanged and still rejects the projected stamped graph.
The v2 key itself establishes no inventory membership or provenance, kind
correctness, constraint discharge, candidate completeness, or behavioral
meaning; domain-owned sealers must bind those authorities independently. The
package-private class-resolution foundation described below can provide one
such discharge receipt, but no certificate or fingerprint entrance consumes it
in this checkpoint.
See the
[carrier-aware fingerprint report](reports/2026-08-13-carrier-aware-certificate-graph-fingerprints.md).

The shared checker also lacks constructor-family schemas, so the public
fingerprint entrance always rejects constructor-pattern graphs. A
package-private domain entrance can reuse the encoder only by atomically
freshly resealing against a schema derived from its own opaque checked
inventory; callers cannot attach a detachable schema to an existing graph.
Canonical construction has an explicit retained-byte bound (one MiB by
default), while the preceding traversal is bounded separately by the supplied
graph limits. The certificate plan's exact structural and trust boundary is
recorded in the
[bounded type-application certificate-plan report](reports/2026-08-13-bounded-type-application-certificate-plans.md).

This fingerprint identifies only the checked shared graph. It does not resolve
globals against an `Inventory`, prove that holes or residual obligations are
absent, identify a complete behavioral problem, or provide behavioral evidence.
A domain-owned session or problem sealer must establish those facts and wrap the
canonical bytes in its own candidate identity.

### Checked type instantiation

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

## Checked ground class-resolution foundation

The package-private
`Language.Haskell.Synthesis.Internal.ClassResolution` module seals one exact,
already-checked `Inventory` into a bounded declared-class resolver. It reads the
raw, unexpanded inventory rather than a prepared synonym expansion: a synonym
declaration may exist, but any resolution constraint that references one is
rejected, as is every nested `ForallType`. The admitted language is normalized,
alias-free, first-order and declared-class only. Queries must be ground and no
given constraints participate.

Each explicit instance is completed with the direct superclasses of its head
class. Sealing rejects superclass cycles, duplicate or overlapping heads, and
every groundable explicit or completed prerequisite which grows relative to
its head under the Paterson-style node and binder-occurrence measure. The
symmetric overlap unifier, directional runtime matcher, and termination measure
share one canonical applicative kernel, so higher-kinded application and the
structural/function/tuple spellings cannot be classified inconsistently.
Resolution preserves instance and prerequisite source order, treats a
current-path cycle or a prerequisite-only binder as absence of evidence, and
never accepts a caller-supplied proof.

The checked limits independently bound the declarations accepted for resolver
retention, retained class and instance tables, retained type-constructor-kind
assumptions, collection widths, type nodes, kind nodes, overlap comparisons,
proof depth, and proof nodes. These are accepted structural-shape and proof
exploration bounds, not a byte bound on names or variables or a claim that
every normalization step is streaming. In particular, source order is
recovered in full from an `Inventory` that has already passed environment and
kind checking before the declaration limit is observed.

A successful opaque receipt co-owns the exact retained checked resolver
environment, canonical ground goal, and proof tree. Proof authority is exposed
only by replay: a structurally equal retained environment, including its
limits and source-ordered facts, is required before the replay goal is
inspected, then that goal must validate and canonicalize to the retained goal.
The projected goal and proof tree are diagnostics and cannot be recombined with
different retained resolver authority.

The package-private heterogeneous ground entrance applies that same bounded
preflight while a query still inhabits its caller-owned variable namespace.
Only after free variables and nested quantification have been rejected does it
structurally retype the variable-free constraint for proof search in the
checked environment. It never coerces or invents an environment identity; the
resulting receipt has the same replay association as an ordinary discharge.

Djinn and Exference do not consume this resolver directly and keep their
existing backend policies. A Length session with conditional provider laws now
attempts to co-seal it from the exact inventory, then the problem boundary
binds successful ground-discharge receipts only to the final occurrence of the
same associated certificate row. If the restricted resolver is unavailable,
the session remains constructible but candidate use fails closed with a
sanitized reason. A graph fingerprint or certificate row alone still
establishes no discharge. Z3 observations remain behavioral heuristics and
never supply dictionary evidence. See the
[checked class-resolution foundation report](reports/2026-08-13-checked-class-resolution-foundation.md).

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

### Bounded where-clause surface syntax

`Language.Haskell.Synthesis.Semantic.Length.Where` (re-exported from the
facade) is an optional bounded ASCII front door onto the same passive
contract vocabulary.  Its grammar is one relation between two arithmetic
expressions: natural literals, `len(argN)` physical target-argument
references, `len(result)` in the scalar domain or `len(result.first)` /
`len(result.second)` in the binary-product domain, `+`, truncated `-`,
`*`, `/` and `%` with a direct positive literal divisor, `min`/`max`, and
parentheses, related by `=`, `!=`, `<=`, `>=`, `<`, or `>` with chained
comparisons rejected.  Admission is fail-closed and sanitized: a hard
16,384-byte source bound and 64-deep nesting bound, whole-source ASCII
validation, then left-to-right parsing whose closed error vocabulary
carries byte offsets but never source bytes.

The module exposes three parser entrances over this one grammar core, and
they differ only in spelling.  `parseLengthWhereSource` accepts the
compact form above.  `parseHaskellLengthWhereSource` accepts the same
formulas in Haskell notation: application-style `length arg0` and
`length result` (with `length (fst result)` / `length (snd result)` as
the binary-product projections), the Haskell relations `==`, `/=`, `<=`,
`>=`, `<`, and `>`, prefix or backticked `div` and `mod` with the same
direct-positive-literal divisor rule, and application-style `min`/`max`
whose arguments are literals or parenthesized expressions.
`parseLeanLengthWhereSource` instead accepts `List.length arg0`, scalar
`List.length result`, and binary-product `List.length result.1` / `.2`,
with `=`, `!=`, `/`, `%`, and application-style `min`/`max`. Each mode
rejects the other modes' distinctive spellings, so no source is ambiguous.
The host entrances are surface parsers, not host-language evaluators: all
three entrances share the byte, nesting, and ASCII admission bounds and the
offsets-only error vocabulary, and all construct the same opaque
`LengthWhereSource`, so elaboration, normalization, fingerprints, and
replay downstream cannot observe which spelling admitted a formula.

The two stages deliberately split authority.  `parseLengthWhereSource`
returns an opaque `LengthWhereSource` that retains the admitting
`LengthLimits` (so elaboration cannot be paired with a more permissive
boundary) and no source text.  `elaborateLengthWhereSource` then requires
the caller's explicit `LengthWhereDomain` and complete source-ordered
target-argument role vector, maps only explicitly observed physical
arguments onto the compact input indices of the checked vocabulary, and
returns a `LengthWhereContractSource` wrapping the ordinary passive
`LengthContractSource` or `LengthSpinePairContractSource` beside the exact
roles supplied.  Nothing here is a checked contract, behavioral receipt,
or inference of a spine model, role, provider law, or solver policy; the
result enters the same sealing pipeline as any hand-built contract source.

### Host-language REPL surfaces

The bounded compact `len(...)` grammar remains a source-level compatibility
boundary, not the primary interactive notation. Djex now also exposes
`parseHaskellLengthWhereSource` and `parseLeanLengthWhereSource`, which
produce the same opaque source with the same normalization, fingerprints,
limits, replay rules, and authority boundaries. Djex's Haskell REPL is active;
the Leant command adapter is the remaining surface wiring:

```text
-- Djex / Haskell
:synth --where length result == length arg0 -- [a] -> [a]

-- Leant / Lean
:synth --where List.length result = List.length arg0 -- List a -> List a
```

The host adapters are nominally separate parsers. Djex uses Haskell application,
projection, equality/inequality, `div`, and `mod` notation; Leant uses
`List.length`, Lean projections, and Lean relations. Neither frontend executes
the displayed expression, accepts arbitrary host code, or defines a second
behavioral semantics. The Djex half has landed end to end:
`parseHaskellLengthWhereSource` (previous section) admits exactly the
Haskell-shaped spelling and lowers it to the same opaque source, and the
standalone REPL already parses the `--where CLAUSE -- TYPE` envelope with
Haskell-shaped help examples and a pure `:set length-z3` policy seal. Once that
policy is active, the REPL resolves one conservative built-in profile, searches
typed Exference candidates, opens one live Length session, and applies the
existing problem/query/observation/replay pipeline before selection and
rendering.

In the built-in list case, omission is intentionally useful but bounded:
`--where` explicitly selects filtering, the host's standard list model is the
default, the scalar/product domain must agree with both expression and target,
and every eligible list input is observed in physical source order rather than
being guessed from formula references. Ambiguous shapes and custom datatypes
require the existing explicit contract/model path. Execution permission is
still independent: a safe activated policy makes this a one-line query; an
unconfigured session needs one policy-activation line first. Missing policy
or ambiguity fails before solver IO.

Djex's standalone REPL and Leant's REPL are both first-class consumers. Djex's
outer structured query grammar, Haskell parser, pure policy seal, conservative
profile resolver, and Exference assessment path have landed. The stored policy
performs no filesystem or process IO at setting time and grants no solver
verdict rejection authority. Only exact independent replay of a returned model
can refute a candidate; every status-only or failed assessment retains it.
Djinn-only constrained queries remain unavailable because Djinn does not yet
retain a matching source-typed graph, and Both mode never compensates by
running it unconstrained. The current explicit Leant form and the direct
`parseLengthWhereSource`, `parseHaskellLengthWhereSource`, and
`parseLeanLengthWhereSource` APIs remain available beside the REPL surfaces.

### Provider summaries as a trust boundary

Provider summaries are an explicit trust boundary. A provider name must resolve
in the retained source inventory, and the caller's claimed scheme must be
alpha-equivalent to the closed scheme derived from that exact declaration. The
checked value stores the source-derived scheme, never the caller's copy. The
legacy `AssumedProviderSummary` remains an assumed law over a closed,
context-free scheme, with its constructor behavior and exact fingerprint bytes
unchanged. The additive
`AssumedConstraintConditionalProviderSummary` instead requires and retains a
closed scheme with a nonempty leading constraint context and records
`AssumedProviderLawConditionalOnConstraintDischarge`. This explicitly assumes
that the provider law is uniform over independently admitted dictionary
evidence as well as admitted closed instantiations. That assumption is necessary
because Exference's certificate records the activated obligations, not the
particular given or instance its checker used. Standalone provider evaluation
still rejects the conditional law. Candidate interpretation may use it only at
the final visible-application node of its own exact associated row after every
obligation has passed static ground discharge. Both forms remain assumed laws,
not behavioral evidence. A spine role says that the law may reference that
argument's list length; it does not require the normalized transfer to mention
it. An unobserved role does not assert purity, totality, strictness, absence of
effects or type reflection, or even that the provider will not evaluate the
argument.

### Split contract, inventory, and session identities

The identities deliberately remain split. Contract and provider-inventory
fingerprints include the exact checked spine model. A contract fingerprint also
identifies the normalized length relation and its ordered observed-spine
inputs (plus the full target-role vector on the mixed path), while
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

Inventories containing only legacy provider summaries retain the exact
provider-inventory v2 and semantic-inventory v1 bytes. Presence of any
constraint-conditional summary advances only those envelopes to provider
inventory v3 and semantic inventory v2; those retention identities remain
byte-for-byte identical to the preceding retention-only checkpoint. Behavioral
use is bound later: conditional sessions use encoding-policy versions 8, 9,
and 10 for legacy/all-observed, mixed-role, and exact-case interpretation,
while sessions without conditional summaries retain exact versions 5, 6, and
7. The explicit dictionary-uniform-law marker lives in those new session
policies and in ground-discharged candidate v3, not in provider inventory v3 or
semantic inventory v2.

The smaller provider-inventory fingerprint still does not identify provider
implementations or the complete source inventory. The atomic session does, and
prevents a context checked from one inventory from being combined with provider
laws checked from another. It also reserves the modeled zero and step
constructor names from provider laws, avoiding an ambiguous semantic global.

### Sealing typed-candidate problems and provider associations

`sealLengthTypedCandidateProblem` completes that association without accepting
a detachable raw graph. It consumes an engine-owned `TypedCandidate`, uses the
provider inventory already owned by the opaque checked session, re-seals the
separately supplied contract through the retained context, rejects the first
residual dictionary before inspecting graph availability, and folds the
candidate's hidden graph carrier without first projecting it. A plain graph
keeps its existing fresh-reseal and fingerprint path. For a nonempty
certificate association, Length first freshly re-seals and fingerprints the
whole carrier with the session-selected shared or exact-case `TypeStructure`,
then authorizes every rooted association row, and only then projects its graph
for interpretation. This preserves graph-limit, schema, and fingerprint-byte
failure precedence before domain authorization.

The associated path is deliberately provider-only. Each rooted row must name
an exact inventory owner, retain a full source scheme alpha-equivalent to that
inventory scheme, and resolve to a checked Length provider summary.
Provider-summary scheme equality is already an opaque session co-sealing
invariant after the exact inventory-source match; only its presence remains
dynamic here. Modeled zero/step constructors are not admitted as certificate
owners. A legacy context-free provider row still requires every activated
obligation list to be empty. A conditional row instead requires a nonempty
activated context and discharges every obligation in canonical
row/step/obligation order through the available resolver which the session
attempted to co-seal from its exact inventory. Queries are alias-free,
forall-free, first-order, ground, and have no givens; absence, rejected queries
or derived constraints, proof exhaustion, and an unavailable resolver become
sanitized public reasons without exposing the constraint or receipt. An empty
association is exactly the
plain v1 graph and candidate identity path. The contract's
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

Discharge alone does not authorize the provider name globally. Before proof
search, Length audits the complete graph, including dead nodes, so the
conditional row's base and every intermediate visible-application node have
exactly their certified incoming function edges and are not the root. The base
is retained as a sentinel which fails if evaluated directly. Only the row's
final receipt-bearing visible-application node can invoke the conditional law;
direct, partial, shared-prefix, or otherwise unassociated occurrences still fail
with discharge-required or protected-chain diagnostics. This is why the
authority is occurrence-specific rather than a dictionary table keyed only by
provider name or constraint.

### The symbolic interpreter and target-argument roles

The first symbolic interpreter is deliberately narrow and lazy. The legacy
sealers still treat every target argument as an observed list spine and retain
their public interpretation behavior and signatures. The additive role-aware
sealers instead retain one complete source-ordered
`LengthTargetArgumentRole` vector in the
checked contract. `LengthObservedSpine` positions receive compact
`LengthInput` numbers in observed-role order; `LengthUnobservedTarget`
positions receive opaque semantic tokens and no length variables. An
all-observed explicit vector canonicalizes to the corresponding ordinary or
conditional session policy. Only a mixed vector selects the role-aware
behavior. Sessions without conditional laws retain policy versions 5, 6, and
7 for legacy/all-observed, mixed roles, and exact cases. Conditional-capable
sessions use versions 8, 9, and 10 respectively and bind the exact resolver
policy, default resolver limits, dictionary-uniform provider-law assumption,
final-node authority, and protected-prefix rejection.

The interpreter still applies every physical target argument. An opaque token
may be ignored, forwarded to a provider argument sealed as unobserved, or
occupy the nonrecursive payload field of the checked list step constructor.
Trying to use it as a callable, list spine, or tuple fails at an explicit
demand site before any invented value can enter Length arithmetic or control.
An unobserved target role asserts neither that a source value inhabits the
argument type nor that evaluating a real candidate is pure, total, or
non-strict. It is only a solver-neutral non-observation policy for the checked
symbolic interpreter.

The ordinary and role-aware interpreters support locals, lambdas,
application, ordinary certificate-free visible type application, tuples,
lets, bind/wildcard/tuple/as patterns, the checked zero and step constructors,
and checked provider transfers. They additionally admit a nonempty opaque
association for an exact obligation-free legacy provider application or for an
exact conditional application whose own ground obligations and protected
prefix passed the boundary above.
The stamped bare graph remains insufficient and is still rejected by the
public graph fingerprint. Their case and constructor-pattern boundary stays
fail-closed except through the separately selected exact-case policy.

### Exact-case and unified interpretation policies

The additive `sealExactSpineCaseLengthSession` and
`sealExactSpineCaseLengthTypedCandidateProblem` entrances opt into one narrow
case policy without inference. Every admitted case inspects and returns a
modeled spine and contains exactly the checked zero and step alternatives,
either source order. The problem sealer reconstructs the raw graph and freshly
rechecks its direct constructor patterns against the session's opaque spine
descriptor before fingerprinting or interpretation. The public shared graph
fingerprint still rejects that graph.

New integrations can select those interpretation axes through one closed
boundary. `LengthInterpretationPolicySource` offers legacy case rejection,
explicit target roles with case rejection, or explicit target roles with exact
zero/step cases. `sealLengthSessionWithInterpretationPolicy` checks and retains
an opaque `CheckedLengthInterpretationPolicy` alongside the session;
`sealLengthContractInSession` takes its roles from that authority, and
`sealLengthTypedCandidateProblemInSession` requires a detached contract to
match the complete retained role vector, including order and arity, before
contract resealing or candidate-graph demand. Exact-case authority therefore
cannot exist without an explicit role vector.

The earlier session and problem sealers remain compatibility wrappers. Their
observable behavior, signatures, and role/policy failure precedence before
carrier inspection stay unchanged, including the historical problem-wrapper
acceptance of distinct role vectors when only their mixed/all-observed
projection agrees. The associated branch adds new outcomes and sanitized
errors after those gates. The new strict entrance is the opt-in association
repair. The newly session-retained role vector is not added as another
fingerprint field: session encoding still consumes only the old mixedness and
case-policy projections, while contracts and downstream identities continue
to consume the same existing contract role fingerprint.
Explicit all-observed ordinary policy remains identical to the current legacy
policy. The prior provider-certificate change advanced its three policy modes
from versions 2/3/4 to 5/6/7; this checkpoint preserves those bytes and selects
8/9/10 only when a conditional provider summary is present.
Session construction also retains productive role traversal; because the exact
vector is now stored, honest deep `NFData` evaluation additionally reaches its
later role payloads.

### Case semantics and fail-closed boundaries

For symbolic scrutinee length `n`, analysis produces
`if n == 0 then zeroResult else stepResult`; the recursive field receives
`n monus 1`. The element payload is an opaque, non-inspectable token. Both
branches are interpreted, so provider authority is their union even when
normalization erases the conditional. This finite-spine model asserts nothing
about source evaluation, strictness, inhabitance, totality, recursion, or
effects. Holes, cases outside the exact policy, unsupported patterns, residual
constraints, unknown globals, unmodeled inventory globals, and
detached certificate-bearing graphs fail closed. Opaque associated graphs are
accepted only for exact inventory-owned provider rows. Ordinary laws still
require empty activated obligations; conditional laws require nonempty ground
obligations, retained resolver receipts, and an isolated final-node occurrence.
Modeled constructor owners remain unsupported. Z3 can never supply dictionary
evidence. Explicit graph-byte and
evaluation step limits still bound candidate work.

The integration suite exercises this foundation through the public production
path: a real Exference search over a declared unary spine retains the exact
zero/step rebuild graph, Length freshly re-seals it against the session
inventory, and a canonical QF_LIA query accepts model input only after
independent replay recovers the candidate result.

### Checked problems and candidate keys

A successful `CheckedLengthProblem` carries its compact observed-spine input
arity, normalized replay formulas, interpreted candidate, one generic
`BehavioralProblem` envelope, and one candidate-independent counterexample-bank
scope. The envelope remains the sole observation/evidence association owner:
it retains the inventory, concrete encoding, candidate, and complete problem
fingerprints. The separate bank scope retains only a collision-free projection
of the complete candidate-independent session, contract, and target basis.
The concrete encoding identifies the re-sealed contract, normalized result and
counterexample condition, interpreter policy, and exactly the provider laws
actually used. Its ordinary legacy/all-observed, mixed-role, and exact-case
versions remain 1/2/3; a conditional-capable session uses 4/5/6. The candidate
key wraps the fresh
shared graph identity and explicitly describes candidate-only authority. The
raw graph fingerprint is transient after those exact bytes enter that key, so
the receipt retains no parallel graph-identity field. It does not pretend to
retain batch completion status.

For an authorized obligation-free carrier the candidate key uses v2 and binds
`opaque-associated-certificate/v1` plus
`activated-obligations-empty/v1`. A carrier which authorizes a conditional law
after ground discharge uses candidate v3 and binds the inventory-bound receipt
requirement, dictionary-uniform law assumption, occurrence-specific final node,
and protected prefix policy. Plain graphs and empty carriers retain exact v1. The
carrier-aware graph remains v2; raw coordinates do not enter these semantic
keys. This changes neither solver replay nor behavioral evidence authority: Z3
output remains heuristic until the same independently sealed problem replays
it, and even successful behavioral replay is not dictionary evidence. See the
[associated provider-certificate Length report](reports/2026-08-13-length-associated-provider-certificates.md)
and the
[ground constraint-discharge report](reports/2026-08-13-length-ground-constraint-discharge.md).

### Candidate-independent counterexample-bank scopes and bounded stores

`Language.Haskell.Synthesis.Semantic.Length.CounterexampleBank` introduces
nominally separate scalar and binary-product replay-input foundations. A scope
is sealed inside the problem constructor while the exact checked session, the
contract freshly revalidated through that session, and the normalized target
coexist. Its identity binds:

- the complete annotation-erased source inventory, checked spine model, and
  admitted provider-law/trust table;
- the solver-neutral interpretation/model policy;
- the exact contract; and
- the exact normalized target, including positional bound-variable slots and
  first-occurrence free-variable slots with flexible/rigid distinction.

The target has its own fingerprint because the existing contract fingerprint
deliberately omits it. Scalar and product scopes use distinct domain and schema
tags, fingerprints, phantom subjects, and nominal identity roles. A scalar
scope cannot be coerced into a product scope even when its concrete facts or
stored inputs happen to agree.

The scope deliberately excludes every candidate graph, interpreted result,
counterexample condition, candidate-used provider-law subset, SMT query,
solver/execution identity, preference, receipt, and verdict. Consequently two
different candidates can share a scope while retaining different candidate,
problem, and query identities. Changing the canonical complete
inventory/provider basis, retained interpretation policy, normalized contract,
or normalized target changes the scope.
`checkedLengthProblemCounterexampleBankScope` and its `SpinePair` sibling
project it from a problem; `lengthSMTLibQueryCounterexampleBankScope` and its
product sibling project the same retained value from a pure query. Query
translation, canonical bytes, query identity, and runtime behavior do not
consume the scope. Problem sealing can now report its bounded construction
through `LengthCounterexampleBankScopeFingerprint` or
`LengthSpinePairCounterexampleBankScopeFingerprint` in the existing nominal
fingerprint-limit error families.

An empty bank pairs one scope with caller-selected limits. Scalar and product
defaults are four retained entries, eight inputs per entry, 256 bits per
natural, 4,096 aggregate retained modeled bytes, and 256 replay attempts.
Custom construction checks negative entry, width, and bit limits in that order,
then rejects `maxBound` width and bit caps whose first excess could not be
observed. The natural byte and attempt caps need no negative check.

Insertion is productive and fixed-precedence. A zero entry cap rejects before
demanding the opaque origin or input vector. Otherwise the store observes at
most one list cell beyond the width cap, validates natural bit widths left to
right, computes one bounded modeled encoding, and forces every retained vector
and statistic before success. The encoding charges an origin byte, a
variable-width arity, and a nonempty magnitude representation with its own
variable-width byte count for every natural. The same byte cap bounds an
individual sample and the aggregate retained newest prefix.

Samples are newest first. Bank duplicate detection and promotion use only the
input vector, not the coarse caller-supplied origin. Reinsertion replaces the
origin and promotes that vector; a new vector is prepended. Entry or
aggregate-byte pressure retains one deterministic newest prefix and evicts the
complete oldest tail at the first excess rather than scanning past it for
smaller entries. Exact cumulative statistics distinguish retained entry/byte
counts, successful records (including duplicates), duplicate promotions, tail
evictions, and explicit replay attempts. Recording an attempt is a separate
immutable operation with its own cap; inserting a sample does not imply an
attempt.

`lengthCounterexampleBankMatchesScope` and the product sibling compare only
the complete scope fingerprint and deliberately inspect no limits, samples,
origins, or statistics. A match authorizes at most attempting fresh replay. A
bank sample contains `[Natural]` and one coarse origin—live-model,
solver-independent, or simplification replay—but no checked arity, verdict,
evidence, or receipt. Those origins are labels, not attestations. Every use
must pass the projected inputs through the current exact problem/query replay
boundary, which may reject the arity, fail under evaluation limits, return a
miss, or mint a fresh narrow receipt.

The pure SMT-LIB facade now provides two explicit association operations for
each nominal domain. One freshly reproduces an opaque counterexample through
the current query before inserting only its inputs; the other replays one
exact sample currently retained by the bank. Scope and attempt admission guard
the first path. Scope, full sample membership, and attempt admission guard the
second. Every begun replay returns the charged successor bank, and every hit
is fresh evidence minted through the current query. The exact-sample path scans
the bounded retained set for full membership, but neither path automatically
replays any other sample or runs a solver.

Inside Djex no bank is wired into live Z3 execution, candidate scheduling,
or session state; the only consumer is Leant's ranking-side command-local
bank, which threads one bank through a filter command's batches. The
foundation and the additive query association remain storage and replay
mechanics without proof or pruning authority. Their frozen characterizations are recorded in the
[nominal Length counterexample-bank report](reports/2026-08-16-nominal-length-counterexample-bank-foundation.md)
and the
[counterexample-bank query-replay report](reports/2026-08-16-length-counterexample-bank-query-replay-bridge.md).

### Spine exposure and bounded concrete evaluation

On the legacy path, every contract argument and result must expose the
context's outer modeled spine; their element types remain opaque and may
themselves be impredicative, while a direct rank-N contract argument is
rejected. On the role-aware path only observed arguments and the result must
expose that spine; unobserved target arguments may be higher-order, rank-N, or
otherwise non-spine. Legacy directly usable provider schemes are closed and
leading-context-free. Conditional summaries retain an exact nonempty leading
context; a candidate path can use one only through the occurrence-specific
ground-discharge boundary, while detached evaluation continues to reject it.
For either retained form, spine-observed provider arguments and the result must
use that same modeled spine, while unobserved provider arguments may be
non-spine or rank-N values.
`Language.Haskell.Synthesis.Semantic.Length.Evaluate` supplies bounded,
deterministic evaluation of one concrete assignment, assumed provider call, or
sealed candidate problem, including exact natural-number monus,
positive-literal natural quotient and modulo, and short-circuiting
conditionals. A raw quotient or modulo divisor is lazy, but semantic sealing
rejects zero and applies the existing literal-bit bound before traversing its
operand; normalization folds literal operands, removes quotient by one, and
reduces every expression modulo one to zero. Its three-way
detached-contract result
distinguishes a failed precondition, a satisfied postcondition, and a violated
postcondition. Standalone provider replay accepts only the context-free trust
class and rejects a conditional summary before inspecting assignment arity,
roles, or values. Whole-problem replay accepts only compact source-ordered
observed-spine inputs, computes
the candidate result itself, and produces an opaque counterexample receipt
bound to the exact problem tuple only when the normalized bad-state formula is
true. Replay evaluates the retained precondition before the candidate and
postcondition, independent of canonical conjunction ordering. The receipt
explicitly distinguishes provider-independent finite-spine results from those
conditional on a listed set of fingerprinted provider assumptions; it does not
establish those implementations or realize the abstract model in a source
language with bottoms or effects. This is not universal behavioral evidence or
permission to prune other candidates. The Z3 execution facade can rank or
challenge candidates, but raw solver output is not trusted evidence without
this independent replay.

### The versioned identity families at a glance

Every layer of the Z3 stack seals its own nominal, versioned identity, and
the prose below cites those versions where each layer is described.  This
table collects the load-bearing top-level families in one place -- the exact
role string is the identity, and a version above `v1` records a deliberate
compatibility break in that family's own history.  Component tags nested
inside these identities (entropy schemes, cleanup disciplines, deadline
coverage clauses) carry their own versions and are documented beside their
owners.

| Identity family | Current role tag | Sealed in |
| --- | --- | --- |
| Checked scalar query schema | `djex-length-z3-qf-lia-smtlib2/v2` | `Internal...Length.SMTLib` |
| Checked product query schema | `djex-length-spine-pair-z3-qf-lia-smtlib2/v1` | `Internal...Length.SMTLib` |
| Protocol plan | `djex-length-z3-smtlib2-protocol-plan/v1` (and the `spine-pair` sibling) | `Internal...SMTLib.Protocol` |
| Protocol phase machine | `djex-length-z3-smtlib2-protocol-phase-machine/v1` (and the `spine-pair` sibling) | `Internal...SMTLib.Protocol` |
| Session protocol | `djex-length-z3-smtlib2-session-protocol/v1` | `Internal...SMTLib.Execution` |
| Stream framing | `djex-smtlib2-stream-framing/v2` | `Internal.SMTLib.Stream` |
| Response decoder | `djex-length-z3-smtlib2-response/v1` | `Internal...SMTLib.Response` |
| Causal byte-stream driver | `djex-length-z3-causal-byte-stream-driver/v1` | `Internal...SMTLib.Session` |
| Raw process | `djex-length-z3-raw-process/v2` | `Internal...SMTLib.Session.Process` |
| Descriptor-bound process | `djex-length-z3-descriptor-bound-sealed-main-image-process/v1` | `Internal...SMTLib.Session.Process` |
| Execution policy | `djex-length-z3-smtlib2-execution-policy/v2`, plus one `/descriptor-bound-.../v1` schema per access-checked launch | `Internal...SMTLib.Execution` |
| Ready worker | `djex-length-z3-capability-probed-ready-worker/v4`, plus one `sealed-main-image` role per descriptor-bound launch at `v1` | `Internal...SMTLib.Session` |
| Scoped worker session | `djex-length-z3-scoped-worker-session/v3` | `Internal...SMTLib.Session` |
| Query run | `djex-length-z3-capability-probed-pre-spawn-pathname-snapshot-worker-query-run/v1` (and the `spine-pair` and `sealed-main-image` siblings) | `Internal...SMTLib.Session` |
| Shared usable-work deadline | `djex-length-z3-shared-usable-work-deadline/v1` | `Internal...SMTLib.Session` |
| Scoped usable-work deadline | `djex-length-z3-scoped-shared-usable-work-deadline/v2` | `Internal...SMTLib.Session` |
| Barrier seed commitment | `djex-length-z3-barrier-seed-commitment/v1` | `Internal...SMTLib.Session` |
| Counterexample-bank scopes | `djex-length-counterexample-bank-scope/v1` and `djex-length-spine-pair-counterexample-bank-scope/v1` | `Semantic.Length.CounterexampleBank` |

### Offline SMT-LIB queries, replay, and origin probes

`Language.Haskell.Synthesis.Semantic.Length.SMTLib` is a pure Z3-facing
translation boundary that seals an opaque nominal QF_LIA query from one exact
`CheckedLengthProblem`. It emits bounded canonical check commands and an
input-only `get-value` command, but launches no solver and assigns no authority
to `sat`, `unsat`, or `unknown`. `validateLengthSMTLibCounterexample` accepts
decoded integer bindings only for that query's input symbols and independently
replays them against the retained problem; raw model text and even `unsat`
remain heuristic observations, never pruning permission or proof.

Internally the scalar and product halves of this layer are one
implementation with two vocabularies.  The solver-independent evaluation
core runs the counterexample, finite input-box, simplification, and
applicable-domain families once over a private per-domain record with a
shared three-way replay view; the query-owned operations (model
validation, input replay, the origin probe, bank recording and
retained-sample replay, and the validation associations) run once over a
private query-surface record.  The same holds at the translation boundary: queries are sealed by one shared plan
builder and one shared fingerprint builder (parameterized only by the
domain's role string, schema tag, and logic constant), and raw solver models
are decoded by one shared reader parameterized by the domain's model-error
constructors. The nominal separation survives at every entrance: the product
sealer maps the shared error vocabulary onto its own nominal query errors at
the boundary, and each domain's query, fingerprint subject, and model-error
types remain distinct, so scalar and product artifacts still cannot be
confused for one another.

`replayLengthSMTLibCounterexampleInputs` is the query-owned entrance for a
caller that already has source-ordered natural inputs. The caller supplies only
`[Natural]`: the sealed query owns the checked problem, modeled-input arity,
generated symbols, and exact behavioral-problem association. Every call runs a
new bounded concrete evaluation. It returns a fresh counterexample receipt only
after re-associating newly constructed evidence with that exact query problem,
or `Nothing` when the inputs are not a counterexample. It is neither a cached
verdict nor a raw-solver shortcut, and it gives `unsat` no authority. Adding
this entrance changes no SMT translation, checked-problem, query, execution,
response, or protocol identity/schema version. See the
[query-owned raw-input replay report](reports/2026-08-14-query-owned-length-input-replay.md).

`probeLengthSMTLibCounterexampleAtOrigin` is the narrower canonical cold-start
probe. The caller supplies only evaluation limits and the sealed query; Djex
derives one zero for every compact modeled input from the query's private
checked problem and delegates to the same fresh replay and association gate.
A hit is the ordinary `ValidatedLengthCounterexample`, including its exact
provider/model basis. `Nothing` says only that this one assignment did not
violate the contract, and an evaluation or association rejection remains the
existing `LengthSMTLibInputReplayError`. The probe emits no command, consumes
no solver status, exposes no arity or contract projection, and creates no new
receipt or identity schema. See the
[query-owned Length origin-probe report](reports/2026-08-14-query-owned-length-origin-probe.md).

### Query-owned bounded counterexample simplification

`simplifyLengthSMTLibQueryCounterexample` can turn an already validated scalar
counterexample into a deterministic, componentwise no-larger witness without
asking Z3 another question.  It treats the anchor inputs as inclusive maxima,
admits the complete box under `LengthInputBoxLimits`, revalidates the anchor
against the query's retained problem, and searches `[0..anchor_i]`
lexicographically with the last input varying fastest.  The binary-product
sibling is `simplifyLengthSpinePairSMTLibQueryCounterexample`.

For example, an anchor at `[3, 2]` can be simplified as follows:

```haskell
anchor <- case replayLengthSMTLibCounterexampleInputs
    defaultLengthEvaluationLimits checkedQuery [3, 2] of
  Left failure -> fail (show failure)
  Right Nothing -> fail "the anchor is not a counterexample"
  Right (Just receipt) -> pure receipt

simplified <- case simplifyLengthSMTLibQueryCounterexample
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedQuery
    anchor of
  Left failure -> fail (show failure)
  Right outcome -> pure outcome

case simplified of
  Nothing -> putStrLn "unavailable or already lexicographically first"
  Just receipt -> print
    ( validatedLengthCounterexampleSimplificationOriginalInputs receipt
    , validatedLengthCounterexampleSimplificationInputs receipt
    , validatedLengthCounterexampleSimplificationInspectedAssignmentCount
        receipt
    )
```

`Just` is returned only for a strict input-vector change.  Its opaque
`ValidatedLengthCounterexampleSimplification` receipt retains the original
inputs, exact number of search assignments inspected through the hit, and a
fresh ordinary `ValidatedLengthCounterexample`.  `Right Nothing` deliberately
makes no claim: the width or Cartesian product may exceed the supplied limits,
or the anchor may already be the first violation.  Arity/value rejection, an
invalid anchor, an admitted evaluation failure, an internal invariant, or an
exact-query association mismatch remains a closed failure.

This is bounded componentwise lexicographic simplification, not global
minimality, a proof, source-language evidence, or solver authority.  It emits
no SMT-LIB and changes no problem, query, protocol, process, or live-run
identity.  Scalar and binary-product receipts and schema tags remain nominally
distinct.  See the
[bounded counterexample simplification report](reports/2026-08-14-bounded-length-counterexample-simplification.md).

### Bounded input-box validation

`validateLengthProblemInputBox` adds solver-independent positive bounded
validation without turning a solver report into evidence. The caller supplies
one inclusive maximum for each compact modeled input. A sealed
`LengthInputBoxLimits` independently caps input width and total Cartesian
assignments; its defaults are eight inputs and 65,536 assignments. Existing
`LengthEvaluationLimits` still cap every maximum/input value and every
intermediate arithmetic value. Validation checks the problem input count before
demanding raw maxima, then checks exact maxima arity, maxima left-to-right, and
the saturating assignment product before evaluation. It enumerates
lexicographically with the last input varying fastest. The first evaluation
failure or counterexample stops traversal; a nullary box contains exactly the
single assignment `[]`.

The result is either ordinary exact-problem counterexample evidence or positive
bounded evidence created only after the complete box succeeds. Its opaque
`ValidatedLengthInputBox` receipt retains the additive
`finite-list-spine-length/bounded-input-box-validation/v1` verifier tag, exact
checked inclusive maxima, total assignment count, precondition-applicable
assignment count, and provider/model basis. The applicable count makes a
vacuous box explicit. Provider-backed validation remains conditional on the
same named assumed laws, and every receipt remains relative to the total finite
spine model rather than source-language bottoms, effects, or totality.
`validateLengthSMTLibQueryInputBox` is only a query-owned association wrapper:
it emits no SMT-LIB, reads no solver observation, and releases either receipt
only after replay against the query's exact behavioral problem. In particular,
bounded success is not universal establishment or pruning authority, and it
does not strengthen `unsat`, `sat`, or `unknown`.

This additive verifier changes no contract, provider-inventory, semantic
inventory, session-policy, candidate, concrete-encoding, complete-problem,
SMT-query, response, protocol, execution, process, worker, or live-observation
identity or canonical bytes. Its v1 tag belongs to the newly opaque receipt,
not to an existing semantic or solver envelope. See the
[bounded Length input-box validation report](reports/2026-08-14-bounded-length-input-box-validation.md).

### Current guarded recursive piecewise-affine applicable-domain validation

Djex exposes one guarded applicable-domain algorithm for checked Length
problems. The scalar problem entrance is
`validateLengthProblemApplicableDomain`; its
nominal binary-product sibling is
`validateLengthSpinePairProblemApplicableDomain`. The query-owned entrances
are `validateLengthSMTLibQueryApplicableDomain` and
`validateLengthSpinePairSMTLibQueryApplicableDomain`. Each takes, in order,
`LengthEvaluationLimits`, `LengthInputBoxLimits`,
`LengthBooleanFiniteUnionLimits`, and the exact checked problem or sealed
query. `LengthBooleanFiniteUnionLimits` remains the current resource-cap
bundle; its historical name does not select a separate validator.

The former progression of directly bounded, positive-affine, relational,
strict, quotient, extrema, monus, Boolean-union, atomic-branching, and
recursive validators is no longer a public API ladder. Djex is experimental,
has no stability or backward-compatibility promise, and has no compatibility
migration here: the predecessor functions, receipt families, error families,
and public receipt-schema-tag projections were deleted. Historical dated
reports still explain how the algorithm was developed, but they are not a
catalogue of current imports. The implementation still uses the lower-level
analyses privately because their exact fallback semantics are useful; callers
select only the complete current algorithm.

A scalar call using the defaults has this shape:

```haskell
validation = validateLengthProblemApplicableDomain
  defaultLengthEvaluationLimits
  defaultLengthInputBoxLimits
  defaultLengthBooleanFiniteUnionLimits
  checkedProblem

queryValidation = validateLengthSMTLibQueryApplicableDomain
  defaultLengthEvaluationLimits
  defaultLengthInputBoxLimits
  defaultLengthBooleanFiniteUnionLimits
  checkedQuery
```

The product calls have the same argument order. Direct validation returns
either `LengthApplicableDomainValidationError` or the three-way
`LengthApplicableDomainValidation`: ordinary conservative inapplicability,
the first exact counterexample found during bounded replay, or established
opaque evidence. Product failures use the nominally distinct
`LengthSpinePairApplicableDomainValidationError`. Query wrappers add
`LengthSMTLibApplicableDomainValidationError` and
`LengthSpinePairSMTLibApplicableDomainValidationError`, respectively, so an
exact evidence/problem association mismatch cannot be confused with semantic
validation failure.

#### Private ordered analysis and atomic-first fallback

Each normalized formula leaf passes through one private ordered progression:

```text
direct literal -> positive affine -> relational -> strict relational
  -> positive-literal quotient -> root extrema -> root monus
  -> Boolean finite union / atomic branching
  -> guarded recursive piecewise-affine fallback
```

The progression is semantic ordering, not a set of public modes. A leaf
already classified by an earlier stage is retained literally. Recursive
interpretation is attempted only when the complete atomic scanner returns its
single ignored alternative and the relation still contains a minimum,
maximum, natural monus, or `LengthIf` somewhere in either operand. Thus an exact
predecessor rule, alternative stream, or contradiction is never
reinterpreted.

The recursive fallback accepts the signed relational leaves supplied by the
Boolean normalizer:

```text
L <= R
not (L <= R)
L = R
```

Negative equality has already become two strict alternatives. Boolean
structure outside a leaf belongs to the bounded DNF layer, not the expression
case splitter.

One recursively admitted expression is an ordered finite stream of guarded
signed-affine values. The recursive grammar contains compact in-range input
variables, natural literals, normalized sums in left-to-right order, retained
positive-literal scales, binary minimum, maximum, and monus nodes, and guarded
`LengthIf` nodes. It does not descend through quotient, modulo, a result
reference, an out-of-range variable, a retained zero scale, or another
unsupported child. This exclusion does not remove the earlier private quotient
and atomic semantics: atomic-first handling may already have accepted such a
whole leaf. If any required recursive descendant is unsupported, the entire
fallback atom remains ignored; no descendant is erased or approximated.

#### Guarded `LengthIf` expansion

For `LengthIf condition whenTrue whenFalse`, support is all-or-nothing. Before
constructing the lazy guard DNF, Djex structurally requires every leaf of both
`condition` and its complement to have exact atomic-first coverage, and it
requires both selected expressions to belong to the recursive grammar. This
check includes the syntactically unselected arm of a constant condition. One
unsupported guard leaf or arm rejects the whole enclosing fallback atom; the
validator does not erase the arm, assume a truth value, or approximate the
conditional.

An admitted conditional expands in this exact order:

```text
positive DNF(condition) x recursive cases(whenTrue)
negative DNF(condition) x recursive cases(whenFalse)
```

The true arm precedes the false arm. Within an arm, condition-DNF alternatives
are outermost and selected-expression alternatives are innermost. Every guard
coverage fragment precedes the selected arm's descendant guards, which in turn
precede enclosing minimum/maximum/monus selectors and the final relation
rules. Negative equality contributes both strict alternatives. A contradictory
guard remains in surrounding Cartesian products and consumes raw
generated-branch admission before its coverage fragments collapse the
expanded branch to a contradiction.

Every binary piecewise node appends two exact selector cases:

```text
min(L,R)  -> [L <= R;     value L]
           | [R + 1 <= L; value R]
max(L,R)  -> [R <= L;     value L]
           | [L + 1 <= R; value R]
L monus R -> [L <= R;     value 0]
           | [R + 1 <= L; value L - R]
```

The first alternative owns equality. Cases use left-child order, then
right-child order, then first selector followed by second. Conditional guards
precede selected-arm guards; all descendant guards precede the current
selector. Once relation operands have expanded in the same left-first
Cartesian order, the final relation rules are appended:
`L <= R` for at-most, `R + 1 <= L` for strict at-most, and `L <= R` then
`R <= L` for equality. Equal-looking guards are not deduplicated, because
their order is observable at rule and closure caps.

The positive monus case can create signed constants or coefficients. These
remain private proof intermediates. For every generated inequality, negative
terms are moved exactly to the opposite side before the result becomes an
ordinary natural-coefficient relational rule. The existing synchronous
rule-once closure is then the sole upper-bound authority:

1. constant-right rules seed bounds in rule order;
2. each pass reads one immutable bounds snapshot;
3. eligible pending rules fire once, in order;
4. newly derived maxima merge with `min` after that pass; and
5. a pass with no firing ends closure.

This is not linear programming, a lower-bound database, or a numeric
least-fixed-point solver.

#### Boolean admission, boxes, and replay

The complete normalized formula DNF is the outer branch source. Every raw
conjunction takes the Cartesian product of the complete alternative streams
of its leaves. A guarded conditional contributes its positive-condition/true-
arm alternatives followed by its negative-condition/false-arm alternatives.
The generated-branch cap observes that lazy raw stream before complement
removal, literal or branch deduplication, absorption, conditional-guard or
selector contradiction, rule collection, closure, or box cleanup. Impossible
cases therefore still consume admission work, and every bounded witness stops
at `limit + 1` at the latest.

Only after raw admission succeeds does Djex canonicalize the original
formula-literal sets: duplicates disappear, exact complement branches drop,
equal sets deduplicate, strict supersets are absorbed, and surviving sets
remain in set order. Those sets are then re-expanded in set and recursive
alternative order. Rule and closure error indices name this canonical
expanded stream, not the earlier raw witness stream.

Each expanded branch processes coverage in literal order: ignored coverage
adds no rule, a coverage contradiction drops the branch, and rule coverage
appends every rule literally. A surviving branch enforces the per-branch rule
cap and completes bounded closure. Contradictory recursive cases have already
consumed raw branch-admission work, even if their expanded branch does not
survive to closure. All live branches finish rule collection and closure
before coverage is inspected. If any live branch lacks a maximum for one
compact input, the first such source input makes the whole result ordinarily
inapplicable; a bounded branch cannot hide an unbounded alternative.

Every completely bounded branch supplies one inclusive-maximum vector. Djex
deduplicates equal vectors, removes any vector componentwise contained in
another, retains incomparable vectors, and orders the resulting maximal
antichain lexicographically. It never replaces incomparable boxes with a
componentwise hull.

Assignment visits are the sum of every retained box cardinality, including
overlap. A bounded set union then deduplicates assignments. Djex replays that
union once in global lexicographic order against the original checked
precondition and postcondition. Selector guards and derived rules are only
coverage machinery; they never replace original-problem replay. The first
evaluation rejection or postcondition counterexample stops replay, and only a
complete successful traversal creates established evidence.

The exact operational precedence is:

1. input width;
2. lazy raw DNF, guarded-conditional, and recursive-alternative counting;
3. generated-branch cap;
4. original formula-literal-set canonicalization;
5. canonical-set re-expansion;
6. per-branch rule cap;
7. per-branch closure-inspection cap;
8. contradictory-branch removal;
9. first input missing a maximum in any live branch;
10. box antichain construction;
11. retained-box cap;
12. maximum-value checks in box and input order;
13. assignment-visit cap;
14. unique-assignment cap while materializing the union;
15. original-problem replay in global lexicographic order;
16. first indexed evaluation rejection or counterexample;
17. receipt construction after complete replay; and
18. exact query association, for a query wrapper, last.

The defaults are:

| Owner and projection | Default |
| --- | ---: |
| `lengthInputBoxInputLimit` | 8 inputs |
| `lengthInputBoxAssignmentLimit` | 65,536 unique assignments |
| `lengthBooleanFiniteUnionGeneratedBranchLimit` | 256 |
| `lengthBooleanFiniteUnionRuleLimitPerBranch` | 64 per branch |
| `lengthBooleanFiniteUnionClosureInspectionLimitPerBranch` | 4,096 per branch |
| `lengthBooleanFiniteUnionRetainedBoxLimit` | 256 |
| `lengthBooleanFiniteUnionAssignmentVisitLimit` | 262,144 |

`LengthEvaluationLimits` separately owns assigned and intermediate values
during maximum admission and original-problem replay.

For example,

```text
(if x <= 2 then x else 5) <= 3
```

has two raw alternatives. The false alternative is contradictory only after
raw admission; the retained box is `[[2]]`, with three visits, three unique
assignments, and three applicable assignments. Replacing the guard with
`x = 0`, the true arm with `1`, and the false arm with `x` produces three raw
alternatives because the complemented equality has two strict cases.

The guarded two-input fixture

```text
y <= 2
(if x <= 1 then max(x,y) else x monus y) <= 2
```

has four raw alternatives and retains exactly `[[4,2]]`: one box, 15 visits,
15 unique assignments, and 12 applicable assignments. The result is not
widened to another hull, and exhaustive replay of the original condition—not
the generated guard rules—decides which of those 15 assignments are
applicable.

For example, the scalar precondition

```text
max(x,y) <= 3 monus min(x,y)
x <= 3
y <= 3
```

retains the exact antichain `[[2,3],[3,2]]`: two boxes, 24 visits, 15 unique
assignments, and ten applicable assignments. The boxes overlap, but no
`[3,3]` hull is manufactured. The recursive atom has eight raw alternatives,
so a branch cap of seven observes eight.

For the product fixture

```text
u = min(x,y) + (x monus y)
v = min(x,y) + (y monus x)
max(u,v) <= 2
x <= 3
y <= 3
```

the four cases of `u`, four of `v`, and two outer maximum choices produce 32
raw alternatives. A cap of 31 observes 32. Closure retains `[[2,2]]`, with
one box and 9/9/9 visit, unique-assignment, and applicable-assignment counts.
Both fixtures use `ProviderIndependentFiniteSpineModel`.

#### Receipts and authority

Successful scalar validation uses the opaque
`ValidatedLengthApplicableDomain`; product validation uses the nominally
separate `ValidatedLengthSpinePairApplicableDomain`. Each receipt has exactly
six public projections:

| Scalar projection | Product projection |
| --- | --- |
| `validatedLengthApplicableDomainInclusiveMaximumBoxes` | `validatedLengthSpinePairApplicableDomainInclusiveMaximumBoxes` |
| `validatedLengthApplicableDomainBoxCount` | `validatedLengthSpinePairApplicableDomainBoxCount` |
| `validatedLengthApplicableDomainAssignmentVisitCount` | `validatedLengthSpinePairApplicableDomainAssignmentVisitCount` |
| `validatedLengthApplicableDomainAssignmentCount` | `validatedLengthSpinePairApplicableDomainAssignmentCount` |
| `validatedLengthApplicableDomainApplicableAssignmentCount` | `validatedLengthSpinePairApplicableDomainApplicableAssignmentCount` |
| `validatedLengthApplicableDomainBasis` | `validatedLengthSpinePairApplicableDomainBasis` |

Constructors are private. Each receipt internally retains an algorithm schema
tag, boxes, visits, unique count, applicable count, and exact
finite-spine/provider-law basis; box count is derived from the boxes. The
current private scalar and product schema bytes are, respectively:

```text
finite-list-spine-length/guarded-recursive-piecewise-affine-finite-union-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/guarded-recursive-piecewise-affine-finite-union-precondition-domain-establishment/v1
```

They replaced the previous private algorithm discriminators when guarded
conditionals became part of current coverage. No projection exposes them, and
they are not a caller persistence, migration, or compatibility contract.

Recursive cases, guards, rules, limits, boxes, and replay sets enter neither
the checked problem nor query fingerprint. A query wrapper emits no command,
launches no worker, and consumes no solver observation; it validates directly
and checks exact evidence/problem association last.

Established evidence means only that the retained finite union covers every
input satisfying the original checked precondition and that every unique
assignment in the union was replayed under the exact checked finite-spine
model and retained provider-law basis. It does not establish source-language
realization, termination, totality, or effects; validate a provider
implementation; make solver status authoritative; prove behavior outside the
checked model; or authorize candidate pruning.

See the dated
[current applicable-domain surface reset report](reports/2026-08-15-current-length-applicable-domain-surface.md)
for the public reset, the current
[guarded conditional applicable-domain report](reports/2026-08-15-guarded-conditional-length-applicable-domain.md)
for this extension, and the historical
[recursive piecewise-affine report](reports/2026-08-15-recursive-piecewise-affine-length-applicable-domain.md)
for the algorithm checkpoint that preceded it.

### Finite binary product spine lengths, offline and live SMT replay

`FiniteBinaryProductSpineLengthsV1` is an additive behavioral domain for one
exact boxed binary product whose two source-ordered result fields expose the
same checked finite-spine model as scalar Length. It is deliberately not a
general tuple or nested-product language. `LengthSpinePairContractVariable`
keeps compact observed inputs and gives the postcondition separate
`LengthSpinePairFirst` and `LengthSpinePairSecond` result components, so a
contract can express relational laws across both outputs. As in the scalar
domain, result references are rejected in preconditions and all admitted
syntax is bounded and normalized before identity construction.

The product domain shares one `CheckedLengthSession`'s exact spine schema,
target-role policy, provider summaries, conditional-discharge authority, and
case policy. That reuse is explicit rather than nominally porous: the product
behavioral problem has its own domain, contract, candidate, encoding, complete
problem, counterexample, and bounded-validation types. Its inventory
fingerprint structurally wraps the scalar session's complete semantic-inventory
bytes together with a versioned derivation policy; scalar evidence therefore
cannot replay against a product problem, even though both were checked from
the same inventory and model.

Candidate sealing applies every physical target argument under the existing
role policy, then requires the final semantic value to be exactly a two-field
tuple. It forces the first field and then the second, requires each to be a
modeled spine, and normalizes their symbolic length expressions under one
left-to-right syntax budget before substituting both into the relational
postcondition. The compatibility, role-aware, exact-case, and unified entrances
are the `sealLengthSpinePairTypedCandidateProblem` family. Scalar spine
providers and exact zero/step spine cases may occur inside either field when
the session already authorizes them. There is no provider law returning a
product and no product-valued case rule.

`validateLengthSpinePairProblemCounterexample` independently recomputes result
components from the checked candidate as the postcondition demands and, for a
violation, materializes both lengths into the receipt; callers supply only the
compact source-ordered natural inputs. `validateLengthSpinePairProblemInputBox`
enumerates those same inputs and can return either the first exact product
counterexample or `ValidatedLengthSpinePairInputBox` positive evidence after
complete traversal. Provider-relative receipts retain the same explicit
assumed-law basis.

#### Offline product SMT queries and replay

`sealLengthSpinePairSMTLibQuery` now adds the pure offline Z3-facing stage. It
seals an opaque `LengthSpinePairSMTLibQuery` from the exact checked product
problem, emits bounded canonical `QF_LIA` check bytes, and requests only the
compact input symbols. Both modeled result expressions were already
substituted into the bad-state formula, so neither result component crosses the
SMT model boundary. The product schema
`djex-length-spine-pair-z3-qf-lia-smtlib2/v1`, fingerprint subject, query role,
errors, behavioral problem, and evidence remain nominally distinct from the
scalar query even when the rendered bytes are identical.

For example, this source contract states that the two result lengths conserve
the first compact input length. Given a checked session, exact product target,
typed candidate, parser-decoded input bindings, and an already sealed
`executionConfig`, the same flow seals the contract, candidate problem, and
query, supports pure replay, and runs that exact query in a scoped live worker:

```haskell
let input0 = LengthVariable (LengthSpinePairInput 0)
    firstResult =
      LengthVariable (LengthSpinePairResult LengthSpinePairFirst)
    secondResult =
      LengthVariable (LengthSpinePairResult LengthSpinePairSecond)
    conservation = LengthSpinePairContractSource
      { lengthSpinePairContractPrecondition = LengthTruth True
      , lengthSpinePairContractPostcondition =
          LengthEqual (LengthSum [firstResult, secondResult]) input0
      }

pairContract <- either (fail . show) pure $
  sealLengthSpinePairContractInSession
    checkedLengthSession exactPairTarget conservation

checkedPairProblem <- either (fail . show) pure $
  sealLengthSpinePairTypedCandidateProblem
    defaultLengthProblemLimits
    checkedLengthSession
    pairContract
    typedCandidate

pairQuery <- either (fail . show) pure $
  sealLengthSpinePairSMTLibQuery
    defaultLengthSMTLibLimits checkedPairProblem

let checkProgram = lengthSpinePairSMTLibQueryCheckBytes pairQuery
    inputRequest =
      lengthSpinePairSMTLibQueryInputValueRequestBytes pairQuery

decodedCounterexample <- either (fail . show) pure $
  validateLengthSpinePairSMTLibCounterexample
    defaultLengthEvaluationLimits pairQuery decodedInputBindings

originCounterexample <- either (fail . show) pure $
  probeLengthSpinePairSMTLibCounterexampleAtOrigin
    defaultLengthEvaluationLimits pairQuery

boundedResult <- either (fail . show) pure $
  validateLengthSpinePairSMTLibQueryInputBox
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    pairQuery
    [8, 8] -- inclusive maximum for each compact input

liveProductResult <-
  withLengthSMTLibLiveSession executionConfig $ \liveSession -> do
    runResult <- runLengthSpinePairSMTLibLiveQuery
      defaultLengthEvaluationLimits liveSession pairQuery
    pure $ case runResult of
      Left queryError -> Left $ show queryError
      Right observation -> do
        counterexample <- either (Left . show) Right $
          replayLengthSpinePairSMTLibLiveQueryObservation
            pairQuery observation
        Right
          ( lengthSpinePairSMTLibLiveQueryObservationSolverStatus observation
          , lengthSpinePairSMTLibLiveQueryObservationResultStrength observation
          , lengthSpinePairSMTLibLiveQueryObservationUse observation
          , counterexample
          )
```

`validateLengthSpinePairSMTLibCounterexample` accepts only the exact generated
input-symbol set, rejects malformed or negative bindings, restores source
order, and independently recomputes both candidate results before releasing
product-domain evidence. Callers that already hold source-ordered naturals use
`replayLengthSpinePairSMTLibCounterexampleInputs`; the origin and input-box
entrances are query-owned specializations of the same exact association
boundary. A box success is positive only for that finite box and recorded
provider basis. A replay miss is only `Nothing`.

#### Live product queries

`runLengthSpinePairSMTLibLiveQuery` now supplies the nominal live product path.
It uses the same capability-probed, serial worker and zero-based ordinal space
as `runLengthSMTLibLiveQuery`. The default scope has one fixed 64-transaction
ordinal budget across any interleaving of scalar and product queries, not 64 of
each; maximum-plus-one is rejected before a write. Cumulative stdout,
per-query response, process, identity, and deadline limits can reject an
earlier transaction independently of that count.

The shared readiness transcript establishes only the exact common QF_LIA,
reset, status, input-valuation, framing, and transport profile needed by both
query shapes. It supplies no scalar authority to a product query. Product
execution instead has its own nominal plan, receiver, and decoded types and a
distinct phase-machine schema tag over the one shared phase machine, nominal
query-run schema and fingerprint role, public observation and failure types,
and `FiniteBinaryProductSpineLengthsV1` evidence association. A values-policy
`sat` run cannot succeed until the returned input symbols have been decoded
against the exact product query and both result components have been
independently recomputed. Public consumers must then pass the opaque live
observation and that exact query through
`replayLengthSpinePairSMTLibLiveQueryObservation` to reveal an optional receipt.
The status, derived strength, and observation use remain heuristic even when a
separate replayed receipt is present; `unsat` and `unknown` never become proof
or pruning authority.

All historical `FiniteListSpineLengthV1` constructors, signatures, nominal
types, tags, fingerprint fields, canonical bytes, replay behavior, and Z3 APIs
remain unchanged. See the stage-one
[finite binary product spine-length foundation report](reports/2026-08-14-finite-binary-product-spine-length-foundation.md)
and the subsequent
[offline product SMT and replay report](reports/2026-08-14-finite-binary-product-spine-smt-replay.md),
then the
[live binary-product Length/Z3 report](reports/2026-08-14-live-binary-product-spine-z3.md).
Leant now consumes this product observation through its nominal canonical-
`Prod` handoff and its product-specific ranking and presentation path,
selected by the `rankingDomain` field of its versionless startup
configuration. Scalar and product behavioral authority remain
separate across that downstream integration.

#### Shared live usable-work budget

The live facade has an additive shared-budget entrance for batches whose total
usable work must not receive a fresh host window for every query. A validated
`LengthSMTLibLiveUsableWorkBudget` is a positive millisecond duration; its pure
constructor rejects nonpositive values and values which cannot be represented
by both the host microsecond wait and monotonic nanosecond clock arithmetic.
The original v1 owner creates an opaque, generative
`LengthSMTLibLiveUsableWorkDeadline budget`. Its rank-N `budget` parameter
separates independently captured tokens at the type level, but does **not**
enforce dynamic non-escape: an `IO` closure can retain the token, and a forked
thread can use it while or after the owner callback returns. That runtime-
unscoped API is unsafe to retain or share and is kept only for source and byte-
identity compatibility. New callers should use the v2
`LengthSMTLibLiveScopedUsableWorkDeadline budget`.

The v2 owner records the creating thread and an open/closed lease state. Only
that owner thread may checkpoint or open a session, and the owner closes the
lease on both normal and exceptional callback exit. An action which captured
the token but runs after exit, or one invoked from a forked thread while the
scope is open, receives the sanitized
`LengthSMTLibLiveSessionUsableWorkScopeUnavailable`. That admission happens
before reading the monotonic clock and, for session opening, before evaluating
configuration or allocating a workspace, so scope unavailability wins even
when the absolute deadline has also expired.

This example starts a 30-second window before forcing two deferred sealed
queries, performs a cooperative checkpoint, then opens one session beneath the
same v2 authority and runs one scalar and one product transaction. The nested
`Either`s distinguish failure of the outer owner from checkpoint/session
failure and from each nominal query result:

```haskell
import Control.DeepSeq (force)
import Control.Exception (evaluate)

usableWorkBudget <- either (fail . show) pure $
  mkLengthSMTLibLiveUsableWorkBudget
    LengthSMTLibLiveUsableWorkBudgetSource
      { lengthSMTLibLiveUsableWorkBudgetSourceMilliseconds = 30000 }

scopedBatch <-
  withLengthSMTLibLiveScopedUsableWorkDeadline usableWorkBudget $ \deadline -> do
    -- Force application-deferred sealing/ranking work after deadline capture.
    (scalarQuery, pairQuery) <- evaluate $ force
      (deferredScalarQuery, deferredPairQuery)
    checkpoint <- checkLengthSMTLibLiveScopedUsableWorkDeadline deadline
    case checkpoint of
      Left failure -> pure (Left failure)
      Right () ->
        withLengthSMTLibLiveSessionUnderScopedDeadline
          deadline executionConfig $ \liveSession -> do
            scalar <- runLengthSMTLibLiveQuery
              defaultLengthEvaluationLimits liveSession scalarQuery
            pair <- runLengthSpinePairSMTLibLiveQuery
              defaultLengthEvaluationLimits liveSession pairQuery
            pure (scalar, pair)

case scopedBatch of
  Left budgetOwnerError -> handleSessionError budgetOwnerError
  Right (Left checkpointOrSessionError) ->
    handleSessionError checkpointOrSessionError
  Right (Right (scalarResult, pairResult)) ->
    consumeNominalResults scalarResult pairResult
```

`withLengthSMTLibLiveSessionWithScopedUsableWorkBudget` is the shorter v2 form
when no application work must run between deadline capture and session
configuration. `checkLengthSMTLibLiveScopedUsableWorkDeadline` is only a
cooperative observation of the same absolute deadline: it neither refreshes
the window nor consumes a query ordinal, writes SMT-LIB, records an
observation, or interrupts work which never calls it. It is therefore not a
watchdog.

The legacy `withLengthSMTLibLiveSession` remains byte-for-byte and
identity-for-identity on its historical policy: it has a private opener window
and derives one fresh local host deadline for every query. Both v1 and v2
budgeted policies instead use the earlier absolute deadline for opening and,
for every scalar or product call, select the minimum of the shared deadline and
a fresh local per-query deadline. The shared deadline wins an exact tie.
Waiting for the single serial query gate and the transaction, independent
replay, and run-identity work all remain beneath that effective deadline. The
existing shared 64-transaction ordinal ceiling is independent of this elapsed-
time budget.

This is a usable-work boundary, not an asynchronous watchdog. It does not
interrupt arbitrary callback IO or a nonterminating pure computation. A live
operation can observe expiry earlier; otherwise the session checks immediately
after its callback returns. The general two-step v2 owner then closes its lease
and checks the shared deadline when its callback returns normally. Callback
exceptions remain authoritative: v2 closes the lease before rethrow, and a
nested session begins its durable owned cleanup before an exception crosses
that session boundary. Final readiness and cleanup use fresh established
private windows rather than the shared operational deadline. The v2
convenience entrance deliberately closes the lease but performs no second
shared-deadline check after those stages. In the two-step example, however,
the general outer owner's normal-return check happens after the nested session
has completely returned, so it may truthfully observe that the shared time
elapsed while those fresh-window stages ran.

An expiry observed at the owner or session boundary is the existing sanitized
`LengthSMTLibLiveSessionDeadlineExceeded`; expiry observed by an individual
scalar or product query uses its corresponding existing byte-free query
deadline failure. Cleanup incompleteness remains a separate bit, and an
exception is never replaced by a budget result. V2 ready-worker, scalar-run,
and product-run identities are distinct additive scoped-v2 envelopes, separate
from both the retained v1 budgeted identities and the exact legacy identities.
They bind the duration, captured shared absolute deadline, minimum-selection
rule, effective cause, lifecycle/admission policy, and coverage/exclusion
policy. Owner thread identifiers, mutable lease state, and individual
checkpoint observations are not identity fields. Query/protocol bytes and
observation APIs remain unchanged.

The deadline establishes only bounded process/session causality. It does not
attest the executed image, validate Z3 soundness, turn `unsat` into proof, or
grant pruning authority. Scalar and product observations stay nominally
separate and `HeuristicRankingOnly`; only exact query association followed by
independent domain replay can reveal optional counterexample evidence. See the
[shared live usable-work budget report](reports/2026-08-15-shared-live-usable-work-budget.md).
The v1 limitation and v2 runtime-scope contract are detailed in the
[dynamically scoped live usable-work deadline report](reports/2026-08-15-dynamically-scoped-live-usable-work-deadline.md).

### Linear-arithmetic lowering and the SMT-LIB response stack

SMT-LIB's QF_LIA logic excludes the built-in `div` and `mod` operators. Djex
therefore lowers every remaining normalized quotient or modulo node to one
shared private Euclidean witness shape. For a positive literal divisor `k` and
operand `e`, the script asserts `e = k*q + r`, `q >= 0`, `r >= 0`, and
`r <= k-1` using only linear integer arithmetic, then projects `q` for
quotient or `r` for modulo. Witness names are operation-specific and allocated
in normalized expression preorder, declarations precede assertions, and no
witness enters `get-value`; only original input symbols cross the model
boundary. Both canonical rendering and structural fingerprinting cover the
declarations and constraints.

#### Shared typed SMT-LIB syntax and renderer

The package-private `Language.Haskell.Synthesis.Internal.SMTLib.QFLIA` module
now owns the reusable typed integer, Boolean, and command syntax together with
its canonical renderer and structural fingerprint-field projection. It fixes
the exact `QF_LIA` logic bytes once. Length still owns source translation,
helper and generated-name policy, Euclidean witnesses, limits, query/domain
identity, model validation, and replay. Its typed SMT plan remains transient
through the two shared projections. After sealing succeeds, the opaque query
retains only the checked problem, bounded check bytes, and complete fingerprint
needed by execution, association, and independent replay. Exact ordered input
symbols and optional canonical `get-value` bytes are rederived from the
problem's sealed arity; query sealing still constructs, bounds, and
structurally fingerprints both before discarding those parallel caches. The
structural `typed-plan` field gains operation-specific versioned
lowering-policy tags only for witness projections that occur. Modulo-only
queries retain their historical symbol names, command order, tag, canonical
scripts, and operation-specific structural fields. The later Length session
policy advance changes complete query keys which embed the checked problem,
even though this lowering remains identical; queries without either projection
are affected by that same containing-key change. The rendered script is not
promoted into the semantic source of truth. The extraction boundary and
byte-compatibility
evidence are recorded in the
[shared typed QF_LIA foundation report](reports/2026-08-13-shared-typed-qf-lia-foundation.md).
The exact admission, lowering, identity, and test boundary is recorded in the
[positive-literal quotient report](reports/2026-08-13-positive-literal-natural-quotient.md)
and the earlier
[positive-literal modulo report](reports/2026-08-13-positive-literal-natural-modulo.md).
The independent role authority, compact model boundary, compatibility bytes,
and higher-order map example are recorded in the
[role-aware target-argument report](reports/2026-08-13-role-aware-target-arguments.md).
The internal schema authority, exact case policy, and compatibility versions
are recorded in the
[exact zero/step case foundation report](reports/2026-08-13-exact-zero-step-length-cases.md).
The unified checked authority, strict association entrance, wrapper
compatibility, identity preservation, and demand boundary are recorded in the
[unified Length interpretation-policy report](reports/2026-08-13-unified-length-interpretation-policy.md).
Exference's later checker-owned retention of that one closed nonempty graph
shape is recorded in the
[exact zero/step Exference graph report](reports/2026-08-13-exference-exact-zero-step-graphs.md).

#### Raw-report observation association

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

#### Bounded response parsing

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response` adds the pure
response side without starting a process. It first retains one complete
response under a total byte bound, then applies an explicit-stack SMT-LIB 2.x
lexer and S-expression parser with independent nesting, node, token, and
integer-width limits. Token limits count source bytes, including both bytes of
a doubled quote. Below the Length compatibility surface,
`Language.Haskell.Synthesis.Internal.SMTLib.Response.Standard` owns canonical
`sat`/`unsat`/`unknown` bytes, bounded check-status classification, and the
standard `unsupported` and `(error "...")` failure shapes. Length maps that
closed vocabulary into its established errors and retains query-specific
valuation decoding; the readiness capability reuses only the canonical status
bytes and keeps exact frame comparison. A versioned response-schema tag covers
lexical, normalization, and shape-decoding policy for a future execution key.
The public decoder admits only exact check statuses and the input-only
valuation shape requested by a particular Length query. It
normalizes quoted symbols, rejects malformed, missing, extra, duplicate,
unknown, wrong-sort, and unsolicited bindings, and restores source input
order. Parsed negative integers remain raw values for the existing natural
domain validator to reject. Parsed statuses are observations only. A private
stream and protocol layer now supplies bounded lexical framing, exact echo
markers, and fail-closed phase sequencing. The package-private live Session now
owns worker identity, deadlines, recovery, query-specific execution, and raw
observation association. Only independent Length replay can create
model-relative counterexample evidence.

#### Lexical whitespace and incremental framing

`Language.Haskell.Synthesis.Internal.SMTLib.Lexical` is the schema-free leaf
owner of the exact SMT-LIB whitespace set and its canonical fingerprint order:
horizontal tab, line feed, carriage return, then space. The bounded response
parser and stream framer classify through that one predicate; cumulative
boundary accounting, causal transcript attribution, and the live process
drain enforce the same set; the drain returns an opaque receipt proving that
lexical content before the causal driver accepts it; and both Length plan
identities bind the same ordered bytes. The receipt proves content only, while
the concrete transport still owns FIFO origin, boundedness, and restoration on
non-whitespace rejection. Any vocabulary change must therefore revise the
affected response, framing, and plan schema identities rather than silently
changing a consumer.

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

#### Causal transaction cursor

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
The schema-free lexical leaf supplies the canonical ordered SMT-LIB whitespace
vocabulary used by the parser, framer, cursor, causal transport, process
boundary drain, and both plan fingerprints. The package-private
`Internal.SMTLib.Causal.BoundaryWhitespace` leaf admits finite strict bytes
into an opaque content-valid receipt. Process validates queued chunks inside
its all-or-nothing STM inspection, so a non-whitespace snapshot is restored
before poison; the driver cannot receive raw successful drain bytes. The
separate schema-free `Internal.SMTLib.Causal.StdoutChunk` leaf admits each
nonempty strict Process read before enqueue. Successful generic transport
reads therefore cannot represent zero progress; FIFO origin, configured byte
bounds, and process/deadline association remain concrete transport laws.

### Live Z3 protocol, session, and process ownership

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Protocol` composes
that cumulative cursor with an exact query and the artifact/response policy
needed after sealing. Its reversible plan key still embeds the complete
execution-policy key, but the structured launch profile, deadline, and other
launch-only fields are not retained as runtime protocol authority. Its initial
action is one exact reset/check/status-marker write. It decodes exactly one
status and accepts the marker only in the following position; only `sat` under
the input-value artifact policy may then expose a second value-request/marker
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

#### Scoped live session ownership

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session` now
provides that package-private live ownership checkpoint. It samples separate
secret barrier and public workspace material, launches the exact configured
argv with an empty environment in a fresh directory, and lends the resulting
worker only through a rank-N callback. On POSIX the workspace is observed
through a retained no-follow directory descriptor and matched by device,
inode, owner, mode, and canonical path; cleanup never traverses contents and
attempts only identity-checked empty-directory removal. The portable Windows
fallback explicitly claims only repeated pathname observations.

#### Shared raw Z3 process owner

The shared package-private
`Language.Haskell.Synthesis.Internal.SMTLib.Z3.Process` owner bounds a
pre-spawn SHA-256 observation of the configured executable file, compares an
optional pin, owns separate stdout/stderr pipes, poisons on the first stderr
byte, and continues draining a finite stderr flood so cleanup cannot deadlock.
FIFO stdout, absolute monotonic deadlines, cancellation, isolated writes,
staged direct-child shutdown, and idempotent cleanup remain associated with
that one opaque runtime. Shutdown uses bounded nonblocking exit polling, so it
also progresses in a non-threaded runtime.
Every successful stdout queue event carries the opaque nonempty-chunk receipt;
an empty OS read remains EOF, while a nonempty permitted prefix still precedes
the maximum-plus-one terminal event in the same FIFO transaction.
This is not executed-image attestation: pathname hashing plus portable direct
spawn has a same-UID namespace race, and neither the loader nor shared
libraries are measured. Descendant cleanup is best effort after the direct
child exits.

#### Resource acquisition and descriptor-child handoff

Every resource-producing action in the shared Process owner remains masked
from acquisition through publication to its deadline controller. The worker
publishes one terminal `Either` through one completion cell as its final
effect. Ordinary work may still run restored, while the dedicated resource
path lets an acquisition restore only interruptible regions whose cleanup it
already owns. An asynchronous exception therefore cannot land after a handle
or child has been returned but before that value becomes visible to the
controller.

Cancellation, deadline expiry, or an exception in the waiting controller
kills the worker, joins by reading that same terminal completion, and rolls
back any successfully acquired value before returning or rethrowing. A failed
final control check likewise rolls back the published value. There is no
second completion signal that can lag behind an already visible resource.

All three native descriptor launch strategies—the sealed descriptor launch,
its effective-ID sibling, and the execve-check sibling—then share one
`DescriptorCreated` handoff. Acquisition and result publication occur masked.
One deliberately restored post-acquisition checkpoint remains protected by
the raw child's rollback action; the consumer is entered masked and receives a
restorer which it uses for initialization only after installing the next
cleanup owner.
Allocation failure cleans the raw child and its three standard-stream handles.
After opaque `Z3SMTLibProcess` allocation succeeds, that process is the sole
owner and an initialization exception closes it. No asynchronous boundary can
leave the resource ownerless or make both cleanup paths own it.

This is package-private lifecycle hardening. It changes no launch selector,
execution policy, process observation, ready-worker/query-run identity,
protocol, behavioral receipt, or public API. The exact mask, rollback, and
test characterization is recorded in the
[descriptor spawn resource-ownership report](reports/2026-08-15-descriptor-spawn-resource-ownership.md).

The three descriptor-bound launches described next are one pipeline in that
module, `openDescriptorBoundProcessWith`, instantiated by a
`DescriptorBoundLaunchPolicy` value per launch. The spine — open the source
read-only, admit the working directory, capture and re-check the source
metadata, copy the bytes once into an anonymous staged image, compare the pin,
seal and verify the image, build the snapshot, spawn from the sealed
descriptor — is written once; a policy names only what differs: whether the
working directory is admitted before or after the source is opened, the
source and final access observations, the staged-image creator and sealer,
the snapshot schema (`DescriptorBoundSnapshotSchema`, which likewise lists
only the claims in which the three fingerprints differ), and the launch
strategy. The sections below therefore describe policy differences, not
separate mechanisms.

#### Descriptor-bound Z3 executable launch

`mkLengthSMTLibDescriptorBoundExecutionConfig` is the additive Linux launch
policy for callers that need the executable-file pin to name the main image
which is actually installed. Its pure source and all solver, response, and
artifact limits are the same as the legacy constructor:

```haskell
let source =
      defaultLengthSMTLibExecutionConfigSource
        "/usr/bin/z3"
        (Just expectedZ3SHA256)
in case mkLengthSMTLibDescriptorBoundExecutionConfig
    defaultLengthSMTLibExecutionLimits source of
  Left rejected -> Left rejected
  Right execution ->
    Right
      ( lengthSMTLibExecutionExecutableLaunchStrategy execution
      , execution
      )
```

Successful construction is still pure and performs no filesystem or process
IO. At the later live boundary, Linux opens the final source component with
no-follow discipline, requires a regular executable file, and streams each
bounded chunk once into both SHA-256 and a private anonymous image. The image
is made executable, sealed against writes and size changes, checked, rewound,
and passed to `execveat` with `AT_EMPTY_PATH`. The optional pin is compared
before any child is forked. A pathname replacement or in-place source rewrite
after sealing therefore cannot change the staged bytes which the kernel loads.
Before `execveat`, the child requires Linux `close_range` to close every
unrelated inherited descriptor around the two retained descriptors. If that
primitive is unavailable or rejects any segment, the child reports launch
failure and exits; there is no soft-`RLIMIT_NOFILE` fallback whose ceiling
could omit a descriptor opened before the limit was lowered.
Any unavailable primitive, admission failure, pin mismatch, seal failure, or
descriptor-exec failure closes the owned resources and fails without retrying
the pathname launcher.

The guarantee is deliberately narrow. It binds the staged main-image bytes,
not the configured pathname after opening, an ELF interpreter, dynamic loader,
shared library, file capability, set-id metadata, Z3 semantics, or a reported
solver status. The sealed image intentionally does not carry source privilege
metadata. The established `mkLengthSMTLibExecutionConfig` entrance and all of
its policy, process, ready-worker, and query-run identities remain literal;
descriptor launch uses additive identities and is never selected implicitly.
Non-Linux builds admit the pure policy but fail closed when a live opener would
need the unavailable native mechanism.

The mechanism, image-staging boundary, identities, and adversarial replacement
characterization are recorded in the
[descriptor-bound Z3 main-image launch report](reports/2026-08-15-descriptor-bound-z3-main-image-launch.md);
the common current ownership handoff is recorded separately in the
[descriptor spawn resource-ownership report](reports/2026-08-15-descriptor-spawn-resource-ownership.md).

#### Effective-ID executable-access descriptor launch

`mkLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessExecutionConfig` is
the additive Linux sibling for callers that also require the opened source to
pass an effective-credential VFS execute-access check. The closed public
classifier is
`LengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunch`; the existing
`lengthSMTLibExecutionExecutableLaunchStrategy` projection distinguishes all
four strategies without revealing a path, digest, descriptor, access result,
or process observation:

```haskell
let source =
      defaultLengthSMTLibExecutionConfigSource
        "/usr/bin/z3"
        (Just expectedZ3SHA256)
in case
    mkLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessExecutionConfig
      defaultLengthSMTLibExecutionLimits source of
  Left rejected -> Left rejected
  Right execution ->
    Right
      ( lengthSMTLibExecutionExecutableLaunchStrategy execution
      , execution
      )
```

Construction is pure. When a later live miss opens the policy, Linux opens the
final source component with no-follow, close-on-exec, no-controlling-terminal,
and nonblocking flags; requires a regular file and at least one execute mode
bit as a shape prefilter; then calls the raw
`faccessat2(fd, "", X_OK, AT_EMPTY_PATH | AT_EACCESS)` system call on that
opened descriptor. The first successful check precedes copying. Djex streams
each bounded chunk once to SHA-256 and a private memfd, rechecks source
metadata, compares the optional pin, sets the staged image to fixed mode
`0500`, seals and verifies writes/size/seals/type/mode, and rewinds it. After
the deterministic test hook, the same opened source descriptor must pass the
same access check a second time immediately before child allocation. Only the
sealed memfd reaches `execveat(AT_EMPTY_PATH)`; there is no pathname or launch-
strategy retry.

The two checks are point-in-time observations under the caller's effective
filesystem credentials. They cover the VFS access path, including ordinary
DAC, applicable POSIX ACLs, source-mount `noexec`, and inode permission hooks;
they are not a reservation and do not establish a complete source `exec`/
`bprm`/LSM/IMA/binfmt decision. The actual executable is a different memfd
inode whose `0500` mode is transport metadata. Source ownership, group, ACLs,
set-id bits, capabilities, extended attributes, security labels, and mount
identity are not copied. Interpreters, loaders, libraries, solver behavior,
and solver results remain unbound.

`EACCES` is a closed executable rejection. A build without the syscall,
`ENOSYS`, or rejection of the fixed flags with `EINVAL` is closed access-check
unavailability; every other checker error is a closed launch failure. Either
check remains under the existing cancellation and absolute opener deadline.
Unsupported platforms and kernels fail only when live work demands the
strategy and never fall back to either older launcher. An all-pure deferred
batch therefore still performs no source, access-check, staging, or process
IO.

The third policy has domain-separated execution, process, ready-worker, and
fresh/shared/scoped scalar and binary-product run identities. Its two
predecessors retain their literal identities. No query, protocol, contract,
behavioral receipt, or evidence schema changes. Linux 6.14's distinct
`AT_EXECVE_CHECK` facility is never selected opportunistically; callers obtain
that stronger and differently scoped operation only through the fourth
explicit selector described under
[`Internal.SMTLib.Z3.Execution` and `Semantic.Length.SMTLib.Execution`](#execve-check-executable-access-launch).

The precise lifecycle, failure mapping, kernel-source references, authority
limits, and characterization matrix are recorded in the
[effective-ID descriptor-bound Z3 launch report](reports/2026-08-15-effective-id-descriptor-bound-z3-launch.md).

#### Derived Length process identity

The raw process owner consumes only the admitted shared Z3 launch profile and
retains a schema-free ordered observation. The existing Length
`...Session.Process` facade maps the generic sanitized failures exhaustively
and derives the unchanged v2 Length root from that exact associated observation
and its process-owned limits whenever the identity is projected. The facade
retains no parallel cached root. The resulting identity binds the path
observation, pin result, argv, environment, working directory, deadline,
process limits, and launch flags, but no artifact/response policy or duplicate
complete Length execution key. The Session's v4 ready-worker identity contains
one complete-key occurrence beside the derived raw process field, rather than
another copy inside that field.
The extraction is detailed in the
[shared raw Z3 process report](reports/2026-08-12-shared-z3-process-runtime.md),
and the facade ownership cleanup in the
[derived Length process identity report](reports/2026-08-12-length-process-derived-identity.md).

#### Worker readiness and identity

Readiness requires four causally separated writes and fresh positional echo
barriers. The probe checks startup print suppression; reset/replay with an
`input = 0` satisfiable problem; exact input valuation; and a second reset with
contradictory zero/one assertions producing `unsat`. Delimiter and boundary
whitespace are charged once and canonically retained with the preceding write,
including a separately delivered final newline. The ready-worker identity
binds the complete Length execution policy once, the process launch
observation method, exact segmented capability transcript, secret-seed
commitment, workspace policy, and configured live-query limits. Readiness
itself creates no solver observation or evidence.
After this identity admission, the lent worker drops the structured Z3 launch
profile and retains only a strict post-launch policy containing the query host
deadline, artifact policy, response limits, and original complete execution
key. The reversible key still contains the original policy bytes; this narrows
structured authority without claiming byte scrubbing.
The ownership and threat-model details are recorded in the
[2026-08-11 scoped worker lease report](reports/2026-08-11-z3-worker-lease.md).

#### Ordinal-bound live query runs

The same Session now owns ordinal-bound live queries through the generic private
`Language.Haskell.Synthesis.Internal.SMTLib.Causal.Driver`; its Length-specific
`...Session.Transport` adapter binds one process, cancellation token, and
absolute deadline as a single transport handle. A masked serial gate allocates
zero-based ordinals and two HMAC-SHA256 marker roles from the unexposed session
seed. The default limit is one shared total of 64 scalar-plus-product
transactions, rather than a separate allowance per domain. The gate checks all
markers against a bounded lease-wide set and seals the exact domain-specific
pure protocol plan before reservation. The shared causal driver writes before
feeding admitted boundary bytes and activating their receiver. For the initial
adopted predecessor boundary it also defers receipt projection until that first
write succeeds. It attributes delayed predecessor whitespace exactly once and
requires exact stdout-delta and stderr accounting. Every marker,
protocol, transport, replay, or identity failure after reservation spends the
ordinal, cancels the lease, and closes the process; plan, capacity,
identity-admission, and query-count rejections before reservation remain
non-mutating.

Both domains run one private pipeline (`runQueryRunDomain` over a
`QueryRunDomain` record naming the plan sealer, replay validator, identity
role prefix, schema tags, and the product run's two explicit reuse fields);
each public entrance rebuilds its nominal failure and run types at the
boundary, so the shared flow cannot present a product receipt as a scalar one.
Successful scalar and product query runs retain separate opaque nominal
reversible identities over the common ready worker, their domain-specific plan,
ordinal, spent markers, absolute deadline, exact segmented transcript, decoded
branch, replay policy, and transport counters. The shared readiness identity is
only a common QF_LIA/input-value transport capability; it is not scalar
behavioral authority imported into the product run. `sat` under the input-value
policy yields counterexample evidence only after independent replay under
explicit evaluation limits, including recomputation of both result components
for a product query. Status-only `sat`, `unsat`, and `unknown` remain heuristic
observations and grant no pruning authority.
After replay and identity sealing, the package-private run retains its ordinal,
one strict status-indexed observation, reversible key, transcript digest, and
accounting boundaries. Only the satisfiable observation branch can carry
optional problem-bound evidence; impossible `unsat`/`unknown` plus evidence
pairs are unrepresentable. The run does not retain the terminal decoded value
or its parsed symbol/integer binding list. Validated evidence still owns
normalized compact source-ordered observed-spine inputs, while the reversible
run key still embeds
the exact bounded transcript bytes. This is removal of a parallel structured
authority, not byte scrubbing.
The exact design and threat boundary are recorded in the
[2026-08-11 ordinal-bound query-run report](reports/2026-08-11-z3-query-runs.md).

#### The public live facade

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Live` is the deliberately
narrow public edge over that owner. It lends an opaque worker only through a
rank-N scope. Scalar and product observations are nominally distinct; each
internally retains its exact query fingerprint and one strict status-indexed
observation whose satisfiable branch alone may carry independently replayed
counterexample evidence. Heuristic strength is derived from status rather than
stored as a second fact; domain-specific public selectors expose status, that
derived strength, and heuristic use. Neither fingerprint, evidence, nor whole
observation has a detached public projection.
`replayLengthSMTLibLiveQueryObservation` and
`replayLengthSpinePairSMTLibLiveQueryObservation` are the corresponding public
semantic extraction gates. Each checks the exact nominal query fingerprint
before inspecting the hidden observation, then replays any evidence against
that query's retained behavioral problem. The separate raw-input replay
entrances construct new evidence from caller-supplied naturals; they do not
extract anything from a live observation. A successful `Nothing` from either
live gate remains only an exactly associated heuristic status. Process handles,
cancellation, paths, executable observations, barriers, ordinals, decoded
valuations, transcripts, transport counters, and reversible run identities
remain private. Public session and query execution failures are mapped to
byte-free classes plus a
cleanup-incomplete bit; the pure replay gate returns its own closed byte-free
association error.
Child-controlled payloads and operating-system details never cross the facade.
The legacy entrance retains separate private opener and per-query deadlines.
The additive budgeted entrances can instead cap usable opening and query work
by one shared absolute monotonic deadline without claiming an asynchronous
hard deadline for arbitrary callback IO. The exact coverage and finalizer
distinction are described in
[shared live usable-work budget](#shared-live-usable-work-budget).

#### Shared execution profile and complete policy identity

`Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution` now owns the shared
pure launch profile below behavioral domains: bounded absolute executable path,
optional exact 32-byte SHA-256 expectation, timeout, `rlimit`, host deadline,
complete argv, startup/reset bytes, empty child environment, fresh working
directory policy, and their flat canonical fingerprint-field slice. It builds
no standalone fingerprint and grants no process or solver authority.
`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution` retains the
unchanged public source and error API, and wraps that profile with the
Length-specific protocol tags, artifact policy, response grammar/limits, one
fingerprint budget, and complete Length policy identity. V2 therefore still
fixes the direct prefix `-in -smt2 smtlib2_compliant=true`, derives exact
launch-time `timeout` and `rlimit` arguments, uses an empty child environment
and a fresh empty working directory, and retains the same path and optional
pin. Standard compliance makes `echo` responses quoted; exact startup bytes
immediately disable `:print-success`, and the reset prefix repeats that
suppression before every self-contained query. The canonical QF_LIA query now
emits start-mode options before `set-logic` and uses fixed nonzero random seed
`1`.
The legacy `lengthSMTLibExecutionArgumentVector` projection names only the
fixed prefix; launchers must use
`lengthSMTLibExecutionConfiguredArgumentVector` for the complete argv.

The shared raw Z3 Process boundary accepts only this profile and owns no
domain schema, fingerprint root, or fingerprint budget. The Length Process
facade derives its unchanged v2 field from the retained runtime's associated
launch observation and limits without rebuilding the IO runtime or caching a
second root. The scoped Session binds the complete Length policy once in its
ready-worker identity next to that raw process field. Removing the former
nested complete-policy duplicate changes and shortens the private ready-worker
key and every query-run key which embeds it. This can make a tight custom
identity-byte budget newly admit an otherwise unchanged worker or run; the
versioned identity deliberately treats that as an authority/admission
improvement rather than padding the removed duplicate.

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

## Length module narrative

*Moved from the synthesis foundation map, which now keeps only its module
table. This is the module-by-module account of the Length contract dialect,
its canonical SMT-LIB translation, and the Z3 live stack, in dependency
order; the sections above state the same design at a higher level.*

### Contract construction paths and target-argument roles

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
[role-aware target-argument report](reports/2026-08-13-role-aware-target-arguments.md).

### Unified interpretation policy

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
[unified interpretation-policy report](reports/2026-08-13-unified-length-interpretation-policy.md).

### Exact zero/step case sealers

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
[exact zero/step case foundation report](reports/2026-08-13-exact-zero-step-length-cases.md)
and the additive
[Exference graph report](reports/2026-08-13-exference-exact-zero-step-graphs.md).

### Associated-certificate carriers

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
[Length associated-provider report](reports/2026-08-13-length-associated-provider-certificates.md).

### Conditional provider summaries and ground discharge

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

### Retention identities and schema versions

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
[ground constraint-discharge report](reports/2026-08-13-length-ground-constraint-discharge.md).

### `Semantic.Length.CounterexampleBank`

`Language.Haskell.Synthesis.Semantic.Length.CounterexampleBank` is the curated
public surface for candidate-independent scalar and binary-product bank scopes
and bounded replay-input stores. `Language.Haskell.Djex` re-exports it. Scope,
bank, sample, origin, limits, statistics, error, schema-subject, and
target-subject families remain nominally separate across the two domains; the
package-private module shares only their strict bounded kernel and construction
edge.

The scalar scope schema is
`djex-length-counterexample-bank-scope/v1`; the product schema is
`djex-length-spine-pair-counterexample-bank-scope/v1`. Each scope composes the
complete domain-appropriate inventory/provider-law fingerprint, the exact
solver-neutral session-policy fingerprint, the exact contract fingerprint,
and a new target fingerprint. Target normalization alpha-normalizes lexical
binders by positional slots and free variables by first occurrence while
preserving flexible versus rigid class. The final scope records explicit
candidate, query/execution, preference, receipt, and verdict exclusions in its
canonical identity.

Problem sealing constructs and retains the scope only after contract
revalidation and complete problem fingerprinting; construction failure maps to
the new nominal counterexample-bank scope fingerprint part. A sealed SMT-LIB
query retains that same problem and exposes a read-only scope projector. The
scope is not part of the complete-problem or query fingerprint and does not
alter canonical query bytes. Its constituent facts are already exact checked
authorities; the separate composition exists to permit sharing across
candidates without confusing their distinct problem/query keys.

The public banks are immutable and opaque. Limit builders, default limits,
empty construction, bounded insertion, explicit attempt recording, sample and
statistics projections, and the two scope-match functions are the only
mutation-shaped operations. `lengthCounterexampleBankMatchesScope` and its
product sibling compare the complete scope fingerprint alone; they do not
record an attempt or validate a stored vector. The SMT-LIB operations which
associate the store with fresh query replay remain outside this module. See
[Candidate-independent counterexample-bank scopes and bounded stores](#candidate-independent-counterexample-bank-scopes-and-bounded-stores)
for the exact inclusion, retention, eviction, statistics, and authority rules,
the
[nominal bank report](reports/2026-08-16-nominal-length-counterexample-bank-foundation.md)
for the storage characterization, and the
[query-replay bridge report](reports/2026-08-16-length-counterexample-bank-query-replay-bridge.md)
for the association boundary.

### `Semantic.Length.SMTLib`

#### The canonical query and raw-input replay

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
[query-owned raw-input replay report](reports/2026-08-14-query-owned-length-input-replay.md).

#### Counterexample-bank query replay

The nominal scalar operation
`recordLengthSMTLibQueryCounterexampleInBank` and product operation
`recordLengthSpinePairSMTLibQueryCounterexampleInBank` accept an opaque
validated counterexample, a coarse origin, and a bank. They first require the
bank to match the current query's candidate-independent scope, then admit one
bank replay attempt. Only after that admission do they project the old
receipt's inputs and run ordinary exact query-owned replay. An ordinary miss
does not enter the store. A fresh hit delegates insertion to the bounded bank
kernel, which alone owns validation, input-only duplicate promotion, MRU
ordering, tail eviction, and storage statistics.

The returned pair always carries the authoritative bank state. Scope mismatch
or attempt-cap refusal occurs before replay and returns the original bank.
Once replay has begun, evaluation or association rejection, a miss, and
insertion rejection all retain the attempt-charged successor. Success returns
that recorded successor together with the newly minted current-query receipt;
the bank itself stores only inputs and the caller's non-attesting origin.

`replayLengthSMTLibCounterexampleBankSample` and its nominal product sibling
operate on one exact opaque retained sample. Their precedence is scope match,
full sample membership, attempt admission, then current-query replay. The first
three refusals leave the bank unchanged; any replay result returns the charged
successor. A hit is a fresh current-query receipt and a miss is ordinary
`Nothing`. This operation performs the bounded exact-membership scan, but no
whole-bank replay traversal, promotion, reinsertion, origin rewrite, or
storage-statistic mutation beyond the replay-attempt count.

Both bridges are pure and solver-independent. They do not issue SMT-LIB,
consume a model or status, operate a live worker, choose a candidate or sample,
schedule another synthesis lane, own bank lifetime, persist state, or change
Leant. They add no receipt reuse, proof, verdict, or pruning authority and
change no problem, query, execution, response, protocol, or solver identity.
See the
[counterexample-bank query-replay report](reports/2026-08-16-length-counterexample-bank-query-replay-bridge.md).

#### The all-zero origin probe

`probeLengthSMTLibCounterexampleAtOrigin` specializes that exact boundary to
the canonical all-zero assignment. It derives the compact input count only
from the checked problem privately retained by the query, constructs no symbol
or contract projection, and then delegates to ordinary query-owned replay.
Consequently a hit is a fresh ordinary counterexample receipt, a miss has no
positive authority, and replay rejection retains the established evaluation or
association error. The pure probe issues no SMT-LIB, consumes no observation,
and changes no identity bytes or schema. See the
[query-owned origin-probe report](reports/2026-08-14-query-owned-length-origin-probe.md).

#### Exhaustive input-box validation

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
[bounded input-box validation report](reports/2026-08-14-bounded-length-input-box-validation.md).

#### Current guarded recursive piecewise-affine coverage policy

`Semantic.Length.Evaluate` exposes only the complete current applicable-domain
analysis. The direct scalar and product functions are
`validateLengthProblemApplicableDomain` and
`validateLengthSpinePairProblemApplicableDomain`; the opaque positive receipts
are `ValidatedLengthApplicableDomain` and
`ValidatedLengthSpinePairApplicableDomain`. Their nominal operational error
types are `LengthApplicableDomainValidationError` and
`LengthSpinePairApplicableDomainValidationError`.

`Semantic.Length.SMTLib` adds the association-only wrappers
`validateLengthSMTLibQueryApplicableDomain` and
`validateLengthSpinePairSMTLibQueryApplicableDomain`, with
`LengthSMTLibApplicableDomainValidationError` and
`LengthSpinePairSMTLibApplicableDomainValidationError`. These wrappers emit no
SMT-LIB command and inspect no solver result. They run the same direct
analysis, then replay the resulting opaque evidence against the query-owned
behavioral problem; association is the final possible failure.

Internally, formula leaves retain the ordered semantic progression from
direct and positive-affine consequences through relational, strict,
positive-literal quotient, root extrema, root monus, Boolean finite-union,
atomic branching, and guarded recursive piecewise-affine fallback. Those
stages are private implementation structure, not independently selectable
contracts. The recursive stage runs atomic-first and opens an
extrema/monus/conditional relation only after the atomic scanner returns its
singleton ignored result.

Minimum, maximum, and monus recursively use exact two-way cases with the first
child owning ties. `LengthIf` uses exact positive-condition/true-arm cases
before negative-condition/false-arm cases. Both guard polarities and both arms
must be completely supported, even when one arm is syntactically unreachable;
otherwise the whole fallback atom remains ignored. Within an admitted
conditional, guard DNF precedes selected-arm alternatives, condition rules
precede arm guards, and descendant guards precede enclosing selectors and the
final relation. Signed affine cases transfer negative terms across the
inequality before entering the unchanged natural-coefficient, synchronous,
rule-once closure. Quotient, modulo, result references, out-of-range variables,
zero scales, and unsupported descendants are not admitted by the recursive
grammar, although an earlier private atomic stage may already have handled the
whole leaf exactly.

Raw DNF by atomic, conditional-guard, selected-arm, and selector-alternative
counting precedes every Boolean and guard cleanup, including contradiction.
Original literal sets are then canonicalized and re-expanded before
per-branch rule and closure work. All live branches must be bounded. Their
boxes are reduced to a lexicographically ordered componentwise-maximal
antichain without a hull. Box visits count overlap; a bounded union
deduplicates assignments; and the original checked problem is replayed in one
global lexicographic order. Derived guards and rules never replace that replay.

The default limits remain 256 raw branches, 64 rules per expanded branch,
4,096 closure inspections per branch, 256 retained boxes, 262,144 box visits,
eight inputs, and 65,536 unique assignments. Operational precedence is width,
raw branches, canonical expansion, rules, closure, missing coverage, boxes,
maximum values, visits, unique assignments, original replay, receipt, then
query association.

Each current receipt exposes inclusive maximum boxes, derived box count, raw
assignment visits, unique assignments, applicable assignments, and the exact
finite-spine/provider-law basis through the six short scalar or `SpinePair`
projections. Constructors and embedded receipt-schema bytes remain private.
The private scalar and product bytes were reset to the current guarded-
recursive algorithm and are neither persistence nor compatibility contracts.
The earlier public ladder and tag projections were deleted without migration:
Djex is experimental and promises neither stability nor backward
compatibility.

For the full selector grammar, closure order, cap precedence, fixtures, and
authority limits, see
[Current guarded recursive piecewise-affine applicable-domain validation](#current-guarded-recursive-piecewise-affine-applicable-domain-validation)
and the dated
[guarded conditional applicable-domain report](reports/2026-08-15-guarded-conditional-length-applicable-domain.md).

#### Finite binary product spine domains

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

#### The offline product query

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

#### Live product runs and the shared ordinal space

The product query now also has bounded query-specific response decoding, its
own nominal package-private protocol plan over the shared phase machine, a
nominal product query-run identity, and a public live observation/failure/replay surface. It
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
[finite binary product spine-length foundation report](reports/2026-08-14-finite-binary-product-spine-length-foundation.md)
and the
[offline product SMT and replay report](reports/2026-08-14-finite-binary-product-spine-smt-replay.md),
followed by the
[live binary-product Length/Z3 report](reports/2026-08-14-live-binary-product-spine-z3.md).
Leant now consumes the product facade through its exact canonical-`Prod`
handoff and its nominal pair ranking and presentation, selected by the
`rankingDomain` field of its versionless startup configuration. That downstream integration does not convert pair evidence
to scalar authority.

#### Euclidean witnesses and the shared typed QF_LIA plan

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
[shared typed QF_LIA foundation report](reports/2026-08-13-shared-typed-qf-lia-foundation.md),
the
[positive-literal quotient report](reports/2026-08-13-positive-literal-natural-quotient.md)
and the earlier
[positive-literal modulo report](reports/2026-08-13-positive-literal-natural-modulo.md)
for the exact admission, lowering, compatibility, and test boundary.

### `Semantic.Length.SMTLib.Observation`

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

### `Semantic.Length.SMTLib.Response`

`Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response` is the matching
pure response boundary. It retains at most the configured total (65,536 bytes by default) before
parsing, so cyclic or infinite lazy input is rejected productively, then enforces separate
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

### `Internal.SMTLib.Stream`

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

### `Internal.SMTLib.Causal.Stream`

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

### `Internal.Semantic.Length.SMTLib.Protocol`

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

### `Internal.Semantic.Length.SMTLib.Session`

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

#### The shared Z3 process layer

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
cancellation gate each operation. Resource-producing acquisitions stay masked
through publication to a single terminal completion cell; cancellation or
deadline loss joins that completion and rolls back any returned value. The
three native descriptor strategies share one masked raw-child handoff: raw
cleanup owns the restored checkpoint, process allocation accepts ownership
while masked, and only then may restored initialization run under process
cleanup. Cleanup closes stdin, polls the direct child without blocking a
non-threaded runtime, then applies bounded TERM/KILL stages and bounded reader/
handle cleanup. Descendant cleanup remains best effort. See the
[descriptor spawn resource-ownership report](reports/2026-08-15-descriptor-spawn-resource-ownership.md)
for the exact ownership transition and interruption characterization.

#### The capability probe and ready-worker identity

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

#### Retained worker policy and run commitment

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

#### Live facade observation replay

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

#### Usable-work budgets and scoped deadlines

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

#### Deadline coverage, expiry, and identities

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
[shared live usable-work budget report](reports/2026-08-15-shared-live-usable-work-budget.md).
The dynamically enforced v2 scope and the retained v1 limitation are recorded
in the
[dynamically scoped live usable-work deadline report](reports/2026-08-15-dynamically-scoped-live-usable-work-deadline.md).

### `Internal.SMTLib.Z3.Execution` and `Semantic.Length.SMTLib.Execution`

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
[shared raw Z3 process report](reports/2026-08-12-shared-z3-process-runtime.md)
and [derived Length process identity report](reports/2026-08-12-length-process-derived-identity.md).

#### Execution-policy construction and fingerprint

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

#### Descriptor-bound main-image launch

`mkLengthSMTLibDescriptorBoundExecutionConfig` is an additive selector over
the same admitted source. On Linux its later process branch opens the final
source component with no-follow discipline, streams each bounded chunk once
through SHA-256 and into a private anonymous image, compares the optional pin,
then makes that image executable and seals writes, growth, shrinkage, and
further seal changes. Only the verified sealed descriptor reaches
`execveat(AT_EMPTY_PATH)`. A pathname swap or in-place source rewrite after
staging cannot alter the bytes installed as the child main image, and every
native failure is fail-closed with no pathname retry. Pure construction remains
IO-free; `lengthSMTLibExecutionExecutableLaunchStrategy` exposes only the
closed strategy classifier.

The child also requires Linux `close_range` for exact unrelated-descriptor
closure before `execveat`. An unavailable or rejected call fails the launch;
there is deliberately no current-soft-limit scan, because a descriptor opened
before `RLIMIT_NOFILE` was lowered can lie above that limit.

This stronger branch deliberately drops source set-id and file-capability
metadata and does not bind an interpreter, dynamic loader, shared library,
solver implementation, or status. Its execution-policy, process observation,
ready-worker, and scalar/product run selections are additive; the established
constructor and all of its canonical bytes stay literal. Platforms without
the sealed Linux mechanism may construct the opaque policy but reject a live
open. The precise acquisition, cancellation, cleanup, identity, and adversarial
replacement contract is recorded in the
[descriptor-bound Z3 main-image launch report](reports/2026-08-15-descriptor-bound-z3-main-image-launch.md).

#### Effective-ID executable-access launch

`mkLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessExecutionConfig` is
the third mutually exclusive pure selector. Its classifier is
`LengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunch`; the two
established selectors and all of their canonical bytes remain literal. The
new Linux opener retains the descriptor launch's sealed-byte authority and
additionally requires the opened source to pass the exact raw
`faccessat2(fd, "", X_OK, AT_EMPTY_PATH | AT_EACCESS)` check twice: first
after regular-file and execute-bit shape admission but before copying, and
again after the image is hashed, pinned, assigned fixed mode `0500`, sealed,
verified, and rewound, immediately before child allocation.

Those are point-in-time VFS observations under the caller's effective
filesystem credentials. They cover ordinary DAC, applicable POSIX ACLs,
source-mount `noexec`, and inode permission hooks, but are not a reservation or
a complete `exec`/`bprm`/LSM/IMA/binfmt decision. The staged memfd is a
different inode: its fixed `0500` is transport metadata, and source ownership,
group, ACLs, set-id bits, capabilities, extended attributes, security labels,
and mount identity are not carried. Loaders, interpreters, libraries, solver
behavior, and results remain unbound.

Denial, checker unavailability, and checker failure are distinct internal
closed classes; the public live boundary maps denial to executable rejection
and the latter two to launch failure. Both checks share the existing opener
deadline and cancellation owner. Unsupported platforms, missing syscalls, and
fixed-flag rejection fail closed at the first demanded live open. There is no
mode-bit, pathname, libc-emulation, or older-strategy fallback, while an
all-pure deferred batch still performs zero executable/access/process IO.

The effective-ID sibling uses fresh process, ready-worker, and fresh/shared/
scoped scalar and product identities. It changes no query, protocol,
behavioral receipt, or evidence schema. See the
[effective-ID descriptor-bound Z3 launch report](reports/2026-08-15-effective-id-descriptor-bound-z3-launch.md)
for the exact lifecycle, failure mapping, kernel references, characterization,
and authority exclusions.

#### Execve-check executable-access launch

`mkLengthSMTLibDescriptorBoundExecveCheckExecutableAccessExecutionConfig` is
the fourth mutually exclusive pure selector. Its closed public classifier is
`LengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunch`; the public
`lengthSMTLibExecutionExecutableLaunchStrategy` projection cannot recover a
path, digest, descriptor, access result, or process identity. The policy role
and schema are exactly
`length-z3-descriptor-bound-execve-check-executable-access-execution-policy`
and
`djex-length-z3-smtlib2-execution-policy/descriptor-bound-execve-check-executable-access/v1`.
Its launch-strength tag is:

```text
opened-source-two-point-faccessat2-x-ok-at-empty-path-at-eaccess-plus-execveat-at-empty-path-at-execve-check-hash-copy-mfd-exec-fixed-0500-f-seal-exec-sealed-memfd-staged-execve-check-then-execveat/point-in-time-source-and-staged-kernel-executable-access-and-main-image-bytes/v1
```

The Linux opener performs the predecessor's effective-credential
`faccessat2` source check and a descriptor-bound `AT_EXECVE_CHECK` source
check before copying. It creates the anonymous image with
`MFD_CLOEXEC | MFD_ALLOW_SEALING | MFD_EXEC`, hashes and pins the copied
bytes, assigns fixed mode `0500`, and adds and verifies
`F_SEAL_WRITE | F_SEAL_GROW | F_SEAL_SHRINK | F_SEAL_FUTURE_WRITE |
F_SEAL_EXEC | F_SEAL_SEAL`. After the deterministic post-seal hook it repeats
the source `faccessat2` and source exec check, checks the sealed staged image
once, and only then may allocate a child. The raw check calls carry the fixed
sanitized `argv[0]` `djex-z3-execve-check` and an empty environment; the real
child retains the configured executable pathname as its exact `argv[0]` while
executing only the staged descriptor.

Every new primitive is fail-closed. There is no pathname, emulation,
non-`MFD_EXEC`, reduced-seal, older-maker, or unchecked-spawn fallback. On a
stock Linux 5.15 kernel, the first source exec check returns unavailable before
memfd creation or child allocation. Upstream Linux 6.14 introduced the needed
exec-check interface, but runtime primitive support—not a version comparison—
is necessary and still not sufficient for admission. The package-private
`lengthSMTLibDescriptorBoundExecveCheckExecutableAccessLaunchSupported`
classifier reports only that the Linux descriptor backend was compiled, so it
remains `True` on that 5.15 build. Pure construction and an all-pure deferred
batch still perform zero executable, access, staging, or worker IO.

Source or staged exec-check denial maps publicly to executable rejection;
checker unavailability or failure maps to launch failure. All six internal
source/staged failure constructors are append-only snapshot failures. The
policy, raw process, ready-worker, and fresh/shared/scoped scalar and product
identity families are new and domain-separated, while all three predecessor
constructors, ordinals, canonical bytes, query bytes, wire protocol, receipts,
and replay authority remain literal.

The checks are point-in-time kernel observations, not reservations or
authorization transfers. `AT_EXECVE_CHECK` deliberately ignores executable
format and interpreter dependencies; Djex claims neither every later `bprm`
decision nor credential transition, source metadata, loader, interpreter,
library, solver, result, proof, or pruning authority. The package-private test
opener may inject an older staged descriptor to exercise ordering and routing
on Linux 5.15, but its sanitized *requested* creation flags are not observable
descriptor provenance and do not give the injected descriptor an `MFD_EXEC`
claim. The complete lifecycle, exact identity strings, kernel sources,
failure table, authority boundary, and test-seam limitation are recorded in
the
[execve-check descriptor-bound Z3 launch report](reports/2026-08-15-execve-check-descriptor-bound-z3-launch.md).

### `Generated` and `TypedGenerated`

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

#### `Internal.TypedGenerated.Certificate`

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
[bounded certificate-plan report](reports/2026-08-13-bounded-type-application-certificate-plans.md).

#### `Internal.TypedGenerated.Certificate.Association`

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
[atomic association report](reports/2026-08-13-atomic-certificate-graph-associations.md).

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
[Exference wiring report](reports/2026-08-13-exference-certificate-association-wiring.md).

#### `TypedGenerated.Fingerprint`

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
[carrier-aware certificate fingerprint report](reports/2026-08-13-carrier-aware-certificate-graph-fingerprints.md).

### `Observability`

`Observability` is orthogonal to logical evidence and search progress. Its
`Natural` counts cannot wrap, zero-valued entries have one canonical absent
representation, mapped collisions sum exactly, and entries are inspected in
key order. Both fields of an `ObservationSnapshot` are intentionally lazy:
reading cumulative observations must not force a candidate stream, and reading
the result must not perform diagnostic accounting.
