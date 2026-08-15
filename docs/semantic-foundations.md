# Semantic foundations

*The design detail behind Djex's checked semantic stratum: typed candidate
identities, ground class resolution, and the finite list-spine Length
behavioral domain with its SMT-LIB/Z3 live stack. The [README](../README.md)
gives the one-paragraph orientation and the
[architecture guide](architecture.md) the ownership map; this document is
the specification, kept verbatim from where it used to sit inline in the
README.*

## What this stratum is, in one screen

Beside the two search engines, Djex carries a backend-neutral layer that
never runs a search. It answers a different question: given a candidate the
engines already produced and Lean or GHC already type-checked, *what can be
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
  - [Provider summaries as a trust boundary](#provider-summaries-as-a-trust-boundary)
  - [Split contract, inventory, and session identities](#split-contract-inventory-and-session-identities)
  - [Sealing typed-candidate problems and provider associations](#sealing-typed-candidate-problems-and-provider-associations)
  - [The symbolic interpreter and target-argument roles](#the-symbolic-interpreter-and-target-argument-roles)
  - [Exact-case and unified interpretation policies](#exact-case-and-unified-interpretation-policies)
  - [Case semantics and fail-closed boundaries](#case-semantics-and-fail-closed-boundaries)
  - [Checked problems and candidate keys](#checked-problems-and-candidate-keys)
  - [Spine exposure and bounded concrete evaluation](#spine-exposure-and-bounded-concrete-evaluation)
  - [Offline SMT-LIB queries, replay, and origin probes](#offline-smt-lib-queries-replay-and-origin-probes)
  - [Query-owned bounded counterexample simplification](#query-owned-bounded-counterexample-simplification)
  - [Bounded input-box validation](#bounded-input-box-validation)
  - [Directly bounded applicable-domain validation](#directly-bounded-applicable-domain-validation)
  - [Positive-affine applicable-domain validation](#positive-affine-applicable-domain-validation)
  - [Relational positive-affine applicable-domain validation](#relational-positive-affine-applicable-domain-validation)
  - [Strict relational positive-affine applicable-domain validation](#strict-relational-positive-affine-applicable-domain-validation)
  - [Strict relational positive-affine quotient applicable-domain validation](#strict-relational-positive-affine-quotient-applicable-domain-validation)
  - [Root-extrema applicable-domain validation](#root-extrema-applicable-domain-validation)
  - [Root-monus applicable-domain validation](#root-monus-applicable-domain-validation)
  - [Boolean finite-union applicable-domain validation](#boolean-finite-union-applicable-domain-validation)
  - [Atomic-branching Boolean finite-union applicable-domain validation](#atomic-branching-boolean-finite-union-applicable-domain-validation)
  - [Recursive piecewise-affine Boolean finite-union applicable-domain validation](#recursive-piecewise-affine-boolean-finite-union-applicable-domain-validation)
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
    - [Descriptor-bound Z3 executable launch](#descriptor-bound-z3-executable-launch)
    - [Effective-ID executable-access descriptor launch](#effective-id-executable-access-descriptor-launch)
    - [Derived Length process identity](#derived-length-process-identity)
    - [Worker readiness and identity](#worker-readiness-and-identity)
    - [Ordinal-bound live query runs](#ordinal-bound-live-query-runs)
    - [The public live facade](#the-public-live-facade)
    - [Shared execution profile and complete policy identity](#shared-execution-profile-and-complete-policy-identity)
- [Length module narrative](#length-module-narrative)
  - [Semantic.Length.SMTLib](#semanticlengthsmtlib)
  - [Semantic.Length.SMTLib.Observation](#semanticlengthsmtlibobservation)
  - [Semantic.Length.SMTLib.Response](#semanticlengthsmtlibresponse)
  - [Internal.SMTLib.Stream](#internalsmtlibstream)
  - [Internal.SMTLib.Causal.Stream](#internalsmtlibcausalstream)
  - [Internal.Semantic.Length.SMTLib.Protocol](#internalsemanticlengthsmtlibprotocol)
  - [Internal.Semantic.Length.SMTLib.Session](#internalsemanticlengthsmtlibsession)

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
arity,
normalized replay formulas, interpreted candidate, and one generic
`BehavioralProblem` envelope. That envelope is the sole field of the checked
problem which retains the inventory, concrete encoding, and complete problem
fingerprints; it also binds the candidate fingerprint retained by the
interpreted candidate receipt. The concrete encoding identifies the re-sealed
contract, normalized result and counterexample condition, interpreter policy,
and exactly the provider laws actually used. Its ordinary legacy/all-observed,
mixed-role, and exact-case versions remain 1/2/3; a conditional-capable session
uses 4/5/6. The candidate key wraps the fresh
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

### Offline SMT-LIB queries, replay, and origin probes

`Language.Haskell.Synthesis.Semantic.Length.SMTLib` is a pure Z3-facing
translation boundary that seals an opaque nominal QF_LIA query from one exact
`CheckedLengthProblem`. It emits bounded canonical check commands and an
input-only `get-value` command, but launches no solver and assigns no authority
to `sat`, `unsat`, or `unknown`. `validateLengthSMTLibCounterexample` accepts
decoded integer bindings only for that query's input symbols and independently
replays them against the retained problem; raw model text and even `unsat`
remain heuristic observations, never pruning permission or proof.

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

### Directly bounded applicable-domain validation

`validateLengthProblemApplicableDomain` can now derive the exact finite box
on which a checked scalar problem may apply when its normalized precondition
contains a direct top-level upper bound for every compact modeled input. The
version-one rule deliberately recognizes only clauses of the form
`input <= literal`; it does not infer bounds from equality, arithmetic,
negation, conditionals, implications, or a source program. When more than one
direct clause bounds the same input, Djex uses the tightest literal. Every
compact input of a nonnullary problem must be covered; a missing bound is the
ordinary `LengthApplicableDomainInapplicable` result rather than a failed
verification. A nullary problem derives maxima `[]` and validates its one
assignment `[]`.

For example, the relevant scalar precondition and query-owned validation call
can be written as follows once `checkedQuery` has been sealed from that
contract and exact candidate:

```haskell
-- Direct bounds for compact inputs 0 and 1.  The duplicate bound for input 0
-- is tightened from 7 to 3, so the derived inclusive box is [3, 2].
precondition = LengthAll
  [ LengthAtMost
      (LengthVariable (LengthInput 0)) (LengthLiteral 7)
  , LengthAtMost
      (LengthVariable (LengthInput 0)) (LengthLiteral 3)
  , LengthAtMost
      (LengthVariable (LengthInput 1)) (LengthLiteral 2)
  ]

validation = validateLengthSMTLibQueryApplicableDomain
  defaultLengthEvaluationLimits
  defaultLengthInputBoxLimits
  checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    validatedLengthApplicableDomainInclusiveMaximums receipt == [3, 2]
  Right (LengthApplicableDomainCounterexample _) -> False
  Right (LengthApplicableDomainInapplicable _) -> False
  Left _ -> False
```

After deriving `[3, 2]`, Djex delegates to the existing solver-independent
finite-box verifier. The first violation is therefore the ordinary exact-
problem counterexample evidence. Complete traversal instead yields the opaque
`ValidatedLengthApplicableDomain` receipt, which retains the derived maxima,
total and precondition-applicable assignment counts, and exact model/provider
basis. Query owners can call
`validateLengthSMTLibQueryApplicableDomain`; the query contributes only exact
problem association and emits no SMT-LIB command. Raw or live `sat`, `unsat`,
and `unknown` statuses have no role.

The nominal binary-product sibling is
`validateLengthSpinePairProblemApplicableDomain`, with the query-owned
`validateLengthSpinePairSMTLibQueryApplicableDomain` and opaque
`ValidatedLengthSpinePairApplicableDomain` receipt. Both domains remain
relative to their checked finite-spine model and any named assumed provider
laws. Establishment is neither source-language totality, universal proof,
provider-implementation validation, nor permission to prune candidates. See
the
[directly bounded applicable-domain report](reports/2026-08-14-directly-bounded-length-applicable-domain.md).

### Positive-affine applicable-domain validation

The original applicable-domain functions above remain version-one,
literal-direct entrances. In particular,
`validateLengthProblemApplicableDomain` still treats equality and scaled or
summed bounds as inapplicable. Callers which explicitly select the additive
rule instead use `validateLengthProblemPositiveAffineApplicableDomain`, or the
query-owned `validateLengthSMTLibQueryPositiveAffineApplicableDomain`.

The new scanner still examines only the normalized precondition itself or its
immediate top-level `LengthAll` clauses. It recognizes an expression of the
form `c + a0*x0 + ... + an*xn`, built only from compact input variables,
natural literals, `LengthSum`, and positive-literal `LengthScale`, when that
expression is at most or equal to a literal `k`. Whenever `c <= k`, each
positive coefficient yields the necessary bound
`xi <= (k - c) quot ai`. Duplicate bounds are combined with `min`. Unsupported
subtrees—including monus, minimum, maximum, quotient, modulo, conditionals,
negation, and nonliteral comparisons—grant no partial bound.

For example, suppose `checkedQuery` retains a candidate which satisfies its
postcondition on the following applicable inputs:

```haskell
input0 = LengthVariable (LengthInput 0)
input1 = LengthVariable (LengthInput 1)

precondition = LengthAll
  [ LengthAtMost
      (LengthSum
        [ LengthScale 2 input0
        , LengthLiteral 3
        ])
      (LengthLiteral 9)
  , LengthEqual input1 (LengthLiteral 2)
  ]

validation = validateLengthSMTLibQueryPositiveAffineApplicableDomain
  defaultLengthEvaluationLimits
  defaultLengthInputBoxLimits
  checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthPositiveAffineApplicableDomainInclusiveMaximums receipt
    , validatedLengthPositiveAffineApplicableDomainAssignmentCount receipt
    , validatedLengthPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([3, 2], 12, 4)
  _ -> False
```

The inequality gives `input0 <= (9 - 3) quot 2`, while equality gives
`input1 <= 2`; the existing verifier therefore exhausts `[0..3] × [0..2]`.
The total count is 12 and exactly the four assignments whose second component
is 2 satisfy this precondition. A violation still returns the ordinary scalar
counterexample instead of an establishment receipt.

A normalized `LengthTruth False`, including either orientation of an unequal
constant-only equality such as `1 == 2`, or a recognized affine atom whose
constant already exceeds its literal ceiling, proves the conjunction
contradictory. A true constant-only equality is non-binding. A
nonnullary validator then uses the all-zero maxima as a finite coverage carrier,
checks its one assignment, and records zero applicable assignments on success.
Contradiction takes precedence over an otherwise missing input bound. A nullary
problem does not need extraction: it retains maxima `[]`, validates the single
assignment `[]`, and records whether that assignment met the precondition.

The opaque positive receipts are
`ValidatedLengthPositiveAffineApplicableDomain` and the nominal product
`ValidatedLengthSpinePairPositiveAffineApplicableDomain`. Their projections
expose derived maxima, total and applicable assignment counts, and the exact
finite-spine/provider-law basis. Product callers use
`validateLengthSpinePairProblemPositiveAffineApplicableDomain` or
`validateLengthSpinePairSMTLibQueryPositiveAffineApplicableDomain`.

The query wrappers emit no SMT-LIB and consume no solver status; they only
replay either authoritative result against the exact problem retained by the
query before releasing its receipt. Establishment remains relative to the
checked total finite-spine model and any named assumed provider laws. It proves
no source-language realization or totality, provider implementation behavior,
universal behavior, or pruning authority, and it does not strengthen `sat`,
`unsat`, or `unknown`.

This checkpoint adds only the two positive-affine receipt tags. Every existing
contract, inventory, session, candidate, encoding, problem, query, response,
protocol, process, worker, run, and live-observation identity and canonical byte
sequence is unchanged; the direct v1 receipts and functions are also unchanged.
See the
[positive-affine applicable-domain report](reports/2026-08-14-positive-affine-length-applicable-domain.md).

### Relational positive-affine applicable-domain validation

The direct and literal-ceiling positive-affine validators above retain their
exact behavior, receipt tags, and authority. Callers that need bounds to flow
between compact inputs explicitly select
`validateLengthProblemRelationalPositiveAffineApplicableDomain` or its
query-owned sibling,
`validateLengthSMTLibQueryRelationalPositiveAffineApplicableDomain`.

This rule summarizes both sides of a top-level `LengthAtMost` or `LengthEqual`
as exact positive-affine expressions over compact inputs, natural literals,
`LengthSum`, and positive-literal `LengthScale`. It cancels common constants
and per-input coefficients before retaining a directed inequality. Equality
contributes the normalized left-to-right rule followed by its reverse.
Unsupported clauses contribute no rule and no partial authority.

For example, equality can transfer a literal bound between two scalar inputs:

```haskell
input0 = LengthVariable (LengthInput 0)
input1 = LengthVariable (LengthInput 1)

precondition = LengthAll
  [ LengthEqual input0 input1
  , LengthAtMost input1 (LengthLiteral 5)
  ]

validation =
  validateLengthSMTLibQueryRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([5, 5], 36, 6)
  _ -> False
```

Propagation is intentionally synchronous and rule-once. Rules with a constant
residual right side seed the initial bounds. Each later pass tests every
pending rule against one immutable bounds snapshot; all eligible rules fire,
their derived bounds are merged with `min` only after the pass, and those rules
are permanently removed. Ineligible rules retry in canonical stored order.
Processing stops when no pending rule fires, so a numeric tightening cycle is
not iterated to a least fixed point. For example, the stored clauses
`x <= y`, `y <= 10`, `y <= z`, and `z <= 2` derive the sound, deliberately
nonleast maxima `[10, 2, 2]`: `x <= y` fires from the same snapshot in which
`y <= z` tightens `y`, and it is not fired again.

The nominal binary-product entrance applies the same rule to its compact input
variables while retaining product-specific evidence:

```haskell
input = LengthVariable (LengthSpinePairInput 0)

precondition = LengthAtMost
  (LengthScale 2 input)
  (LengthSum [input, LengthLiteral 1])

pairValidation =
  validateLengthSpinePairSMTLibQueryRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedPairQuery

case pairValidation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthSpinePairRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthSpinePairRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([1], 2, 2)
  _ -> False
```

Here exact cancellation reduces `2*input <= input + 1` to `input <= 1`.
Multi-hop rules similarly retry until every rule that can fire has fired once.
If a fired rule's residual left constant already exceeds the maximum of its
bounded right side, or the normalized conjunction is `LengthTruth False`, the
precondition is contradictory and the existing verifier checks an all-zero
coverage carrier. Otherwise the first compact input still lacking a bound
produces ordinary `LengthApplicableDomainInapplicable`.

Successful traversal yields opaque
`ValidatedLengthRelationalPositiveAffineApplicableDomain` or the nominally
separate
`ValidatedLengthSpinePairRelationalPositiveAffineApplicableDomain`. Their
projections expose maxima, total/applicable counts, and the exact
finite-spine/provider-law basis. Query wrappers emit no SMT-LIB and consume no
solver status; they contribute only exact behavioral-problem association.
Establishment remains model-relative and does not prove source-language
realization or totality, validate provider implementations, establish
universal behavior, or authorize pruning.

Only the two new relational receipt tags add canonical bytes. Existing
contracts, inventories, sessions, candidates, encodings, problems, queries,
responses, protocols, processes, workers, runs, live observations, and the
older direct and positive-affine receipts retain their identities and bytes.
See the
[relational positive-affine applicable-domain report](reports/2026-08-15-relational-positive-affine-length-applicable-domain.md).

### Strict relational positive-affine applicable-domain validation

The direct, literal-ceiling positive-affine, and relational positive-affine
validators retain their exact behavior and receipt families. The additive
strict entrance is
`validateLengthProblemStrictRelationalPositiveAffineApplicableDomain`, with
query-owned
`validateLengthSMTLibQueryStrictRelationalPositiveAffineApplicableDomain`.
It accepts every ordinary top-level relation recognized by the relational
rule and adds exactly one normalized clause shape:

```text
not (L <= R)  ==>  R + 1 <= L
```

This equivalence is exact over the modeled natural lengths. Both `L` and `R`
must be positive-affine expressions over compact inputs, natural literals,
`LengthSum`, and positive-literal `LengthScale`. The proof-only successor is
added to the exact arbitrary-precision summary of `R` before ordinary constant
and coefficient cancellation; it does not construct checked syntax, consume
the contract's literal budget, or change contract or query identity.

For example, two strict scalar clauses form a bounded chain:

```haskell
input0 = LengthVariable (LengthInput 0)
input1 = LengthVariable (LengthInput 1)

precondition = LengthAll
  [ LengthNot
      (LengthAtMost (LengthLiteral 5) input0)
  , LengthNot
      (LengthAtMost input0 input1)
  ]

validation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedQuery

case validation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthStrictRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([4, 3], 20, 10)
  _ -> False
```

The first clause is `not (5 <= input0)`, hence `input0 + 1 <= 5` and
`input0 <= 4`. The second is `not (input0 <= input1)`, hence
`input1 + 1 <= input0`; the existing relational closure then derives
`input1 <= 3`. The finite-box verifier traverses 20 assignments, exactly ten
of which satisfy `input1 < input0 < 5`.

The nominal binary-product query entrance applies the same proof rule while
keeping product evidence separate. This example combines successor insertion
with exact coefficient cancellation:

```haskell
input = LengthVariable (LengthSpinePairInput 0)

precondition = LengthNot $ LengthAtMost
  (LengthSum [input, LengthLiteral 3])
  (LengthScale 2 input)

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    checkedPairQuery

case pairValidation of
  Right (LengthApplicableDomainEstablished receipt) ->
    ( validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainInclusiveMaximums
        receipt
    , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainAssignmentCount
        receipt
    , validatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomainApplicableAssignmentCount
        receipt
    ) == ([2], 3, 3)
  _ -> False
```

Here `not (input + 3 <= 2*input)` becomes
`2*input + 1 <= input + 3`; cancellation leaves `input <= 2`.

Negated equality, nested logical structure, and a negated comparison with
monus, minimum, maximum, quotient, modulo, a conditional, a result reference,
or any other non-affine subtree grant no rule and no partial bound. Such a
clause remains part of actual precondition replay if other clauses establish a
finite box. This is neither a general Boolean complement pass nor an SMT
inference step.

Width rejection precedes precondition demand. A nullary problem bypasses
extraction and validates `[]`. For a nonnullary problem, syntactic or
propagated contradiction wins over missing-bound inapplicability and selects
the existing all-zero carrier; otherwise the first source-ordered unbounded
input produces ordinary `LengthApplicableDomainInapplicable`. Derived maxima,
Cartesian cardinality, assignment evaluation, first counterexample, and final
query association retain the established finite-box precedence.

Successful traversal yields opaque
`ValidatedLengthStrictRelationalPositiveAffineApplicableDomain` or the
nominally distinct
`ValidatedLengthSpinePairStrictRelationalPositiveAffineApplicableDomain`.
Query wrappers emit no SMT-LIB and consume no solver status; the query supplies
only exact behavioral-problem association. Receipts retain maxima,
total/applicable counts, and the exact finite-spine/provider-law basis. Their
authority remains model-relative and grants no source-language realization,
provider-implementation validation, universal proof, or pruning authority.

Only the two strict-relational receipt tags add canonical bytes. Every older
explicit-box, direct, positive-affine, relational, counterexample, replay,
origin, simplification, and live validator retains its API, behavior,
authority, identity, and bytes, as do all existing contract-through-live
artifacts. See the
[strict relational positive-affine applicable-domain report](reports/2026-08-15-strict-relational-positive-affine-length-applicable-domain.md).

### Strict relational positive-affine quotient applicable-domain validation

The direct, positive-affine, relational, and strict-relational validators
retain their exact behavior and receipt families. The additive quotient
successor is
`validateLengthProblemStrictRelationalPositiveAffineQuotientApplicableDomain`,
with query-owned
`validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientApplicableDomain`.
For a positive checked divisor `d` and exact positive-affine `A` and `B`, it
recognizes one quotient at a directed relation operand's root through four
exact natural laws:

```text
q_d(A) <= B        <=>  A <= d*B + (d - 1)
A <= q_d(B)        <=>  d*A <= B
not (q_d(A) <= B)  <=>  d*(B + 1) <= A
not (A <= q_d(B))  <=>  B + 1 <= d*A
```

An equality with exactly one root quotient emits both directed rules,
left-to-right and then right-to-left. The quotient dividend and opposite
operand must each contain only compact inputs, natural literals, `LengthSum`,
and positive-literal `LengthScale`. Scaling, successor insertion, and ordinary
coefficient cancellation operate on arbitrary-precision proof summaries; they
construct no checked expression and spend no syntax or public literal budget.

For example:

```text
q_3(2*x + 1) <= 2  <=>  2*x + 1 <= 8  ==>  x <= 3
q_3(x) = 4         <=>  12 <= x <= 14
not (4 <= q_3(x))  <=>  x + 1 <= 12   ==>  x <= 11
```

The corresponding finite boxes have maxima `[3]`, `[14]`, and `[11]`.
Their total/applicable counts are respectively 4/4, 15/3, and 12/12.
Relational closure also composes quotient consequences: `x <= q_3(y)` and
`y <= 8` derive maxima `[2, 8]`, total count 27, and applicable count 18.
Conversely, `not (q_3(x) <= 4)` together with `x <= 14` proves contradiction
and selects the established all-zero carrier, with maximum `[0]`, total count
one, and applicable count zero.

The nominal binary-product entrances are
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientApplicableDomain`
and
`validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientApplicableDomain`.
For a safe product problem under `q_3(input) <= 2`, complete traversal yields
maximum `[8]` and total/applicable counts 9/9. Product evidence remains
nominally disjoint from scalar evidence.

Nested or embedded quotients, quotients at both relation roots, unsupported
quotient children or opposite operands, negated equality, and nested Boolean
structure contribute no rule and no partial bound. Modulo, natural monus,
minimum, maximum, conditionals, result references, and other non-affine
subtrees remain excluded. Quotient-free clauses delegate to the predecessor
strict scanner unchanged. An unsupported clause is not a validation failure;
it still participates in concrete precondition replay if other clauses derive
a complete box.

Input-width rejection precedes precondition demand, and nullary problems send
`[]` directly to the finite-box verifier. For nonnullary problems, syntactic
or propagated contradiction wins over missing coverage; otherwise the first
source-ordered unbounded input returns ordinary
`LengthApplicableDomainInapplicable`. Derived-value and Cartesian limits,
indexed evaluation, the first counterexample, complete receipt construction,
and exact query association retain the established order. Equality rule order
and synchronous rule-once closure remain deterministic.

Successful traversal yields opaque
`ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain` or
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain`.
Their four projections expose maxima, total/applicable counts, and the exact
finite-spine/provider-law basis. Query wrappers issue no SMT-LIB command and
consume no solver status; a query supplies only exact behavioral-problem
association. Establishment remains model-relative and grants no
source-language realization, provider validation, universal proof, or pruning
authority.

Only the two new quotient receipt tags add canonical bytes:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-precondition-domain-establishment/v1
```

Every predecessor API and receipt remains literal. Existing contracts,
inventories, sessions, candidates, encodings, problems, query commands,
symbols, value requests, fingerprints, protocols, processes, workers, runs,
and observations retain their identities and bytes. Root extrema, root monus,
and bounded Boolean finite unions are the cumulative successors specified
below. The finite-union receipt has separate work caps and retains explicit
branch boxes rather than replacing them with a componentwise-maximum
rectangle. See the
[strict relational positive-affine quotient applicable-domain report](reports/2026-08-15-strict-relational-positive-affine-quotient-length-applicable-domain.md).

### Root-extrema applicable-domain validation

The cumulative root-extrema successor keeps every quotient-free and
root-quotient consequence above literal. Its scalar checked-problem and
query-owned entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`
and
`validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`.
For exact positive-affine `A`, `B`, and `C`, it adds exactly these four
conjunctive natural laws:

```text
max(A, B) <= C        <=>  A <= C and B <= C
C <= min(A, B)        <=>  C <= A and C <= B
not (min(A, B) <= C)  <=>  C + 1 <= A and C + 1 <= B
not (C <= max(A, B))  <=>  A + 1 <= C and B + 1 <= C
```

All three operands must independently fit the predecessor's exact grammar of
compact inputs, natural literals, `LengthSum`, and positive-literal
`LengthScale`. The scanner summarizes all three before it emits either of the
two component rules. Failure of any child or the opposite operand therefore
ignores the whole clause for coverage; it never retains one convenient half.
Successor insertion for a strict law happens on the arbitrary-precision proof
summary before ordinary constant and coefficient cancellation. No derived
expression is checked syntax, and no syntax-node or public-literal budget is
spent.

Equality uses only the necessary conjunctive half of an extremum equality. A
single root maximum on either side of `=` contributes `A <= C` then `B <= C`;
a single root minimum on either side contributes `C <= A` then `C <= B`.
Canonical equality orientation therefore cannot change the result. The
opposite halves, `C <= max(A,B)` and `min(A,B) <= C`, are disjunctive and add
no rule. Thus `max(x, x + 1) = x + 1` supplies no finite upper bound, while
`2*x + 1 = min(x + 5, 7)` derives maximum `[3]`; exhaustive replay finds only
`x = 3` applicable in the four-assignment box.

Admission observes normalized syntax, not the caller's raw tree. Contract
sealing flattens each same-kind extremum, combines its literal children,
deduplicates and sorts all retained children, and left-associates the result.
It also canonicalizes equality operands and flattens, deduplicates, and sorts
top-level conjunctions. The validator traverses those canonical clauses in
order. A supported binary extremum emits its normalized first-child rule and
then its second-child rule. A retained extremum of three or more canonical
terms has a nested extremum child and is ignored atomically; an all-literal
extremum can instead fold away and reach the predecessor scanner.

The generated rules enter the unchanged relational closure. Constant-right
rules seed bounds. Each later pass reads one immutable bounds snapshot,
examines pending rules in canonical clause/component order, merges newly
derived maxima with `min` after the pass, and permanently removes every rule
that fired. The closure stops when a pass fires nothing; it remains a
synchronous rule-once consequence calculation, not a numeric least-fixed-point
solver. For example, `max(x,y) <= z` followed by `z <= 4` derives maxima
`[4,4,4]`. The resulting box contains 125 assignments, of which 55 satisfy
the original normalized precondition. The delegated predecessor chain
`x <= y, y <= 10, y <= z, z <= 2` still derives `[10,2,2]`, not the numeric
least box `[2,2,2]`.

The other three direct laws give small exact scalar boxes:

```text
max(x, 2*x + 1) <= 5        ==>  [2], total/applicable 3/3
2*x + 1 <= min(x + 5, 7)    ==>  [3], total/applicable 4/4
not (min(x + 4, 9) <= 2*x)  ==>  [3], total/applicable 4/4
not (5 <= max(2*x, x + 1))  ==>  [2], total/applicable 3/3
```

The nominal binary-product entrances are
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`
and
`validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`.
Successful scalar and product traversals yield the opaque, nominally disjoint
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`.
Each family exposes inclusive maxima, total assignment count, applicable
assignment count, and exact finite-spine/provider-law basis through the
correspondingly prefixed four projections.

The deliberately unsupported orientations are `C <= max(A,B)`,
`min(A,B) <= C`, `not (max(A,B) <= C)`, and
`not (C <= min(A,B))`: each would require a disjunction rather than two
necessary conjuncts. Both-root extrema, nested or embedded extrema, retained
n-ary extrema, mixed root-extrema/root-quotient clauses, unsupported affine
children, negated equality, and nested Boolean structure are also ignored as
whole clauses. Natural monus, modulo, quotient, conditionals, result
references, and other non-affine nodes cannot appear in any of the three
summaries. This checkpoint is neither a recursive extrema simplifier nor a
finite-union domain analysis.

Input-width rejection remains first, and nullary validation still replays the
singleton assignment `[]`. For a nonnullary problem, normalized extraction
and closure resolve contradiction before any missing-bound lookup;
otherwise the first source-ordered input without a maximum is ordinary
`LengthApplicableDomainInapplicable`. Derived-value and Cartesian-product
admission, last-input-fastest exhaustive replay, indexed evaluation failure,
the first exact counterexample, positive receipt construction, and query
association retain their established precedence. An ignored clause remains
in the original precondition and is still evaluated for every assignment if
other clauses establish a complete box.

Query-owned validation emits no SMT-LIB, launches no worker, and consumes no
solver observation. The existing sealed query commands, symbols, value
requests, behavioral problem, and fingerprint remain byte-for-byte unchanged;
the chosen offline validator is not added to query identity. Every predecessor
API, receipt, tag, behavior, and contract-through-live identity likewise
remains literal. Only these new nominal receipt tags add bytes:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1
```

Establishment remains bounded and relative to the checked total finite-spine
model and any retained assumed provider laws. It proves complete replay only
inside the derived rectangle. It is not source-language realization or
totality, validation of a provider implementation, universal proof, solver
status authority, or permission to prune candidates. Immediate natural-monus
consequences are the additive successor specified next. See the
[root-extrema applicable-domain report](reports/2026-08-15-root-extrema-length-applicable-domain.md).

### Root-monus applicable-domain validation

The cumulative root-monus successor preserves every quotient-free,
root-quotient, and root-extrema clause path above literally. Its scalar
checked-problem and query-owned entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`
and
`validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`.

Let `M = A monus B = max(A-B,0)`. The scanner first summarizes `A`, `B`,
and the opposite operand `C` in the exact positive-affine grammar. Writing the
summary of `C` as `c + sum(k_i*x_i)`, with nonnegative coefficients, gives the
following five admitted cases:

```text
M <= C                 <=>  A <= B + C
C <= M, when c > 0     <=>  B + C <= A
not (M <= C)           <=>  B + C + 1 <= A
not (C <= M)           <=>  1 <= C and A + 1 <= B + C
M = C or C = M          ==>  A <= B + C
```

The second case has a deliberate zero boundary. If `c > 0`, `C` is uniformly
positive for every natural assignment and the displayed rewrite is exact. If
`c = 0` and `C` has no coefficients, it is identically zero, so `0 <= M` is a
tautology and contributes no rule. If `c = 0` with at least one coefficient,
`C` may be zero and the exact formula is disjunctive:

```text
C <= A monus B  <=>  C = 0 or B + C <= A
```

That may-zero clause is ignored as a whole. Positivity is never borrowed from
another conjunct, and the scanner does not substitute the weaker `C <= A`.
For equality, `A <= B+C` is always a necessary supported half. When `c > 0`,
the scanner appends `B+C <= A`, making the two extracted affine rules exact;
when `C` is identically zero, the first rule is the exact `A <= B`; and when
`C` may be zero, only the necessary first rule is retained. Original-formula
replay, rather than the extracted rules, remains the final equality authority.

The strict reverse rule is atomic. It emits `1 <= C` first and
`A+1 <= B+C` second only after all three operands summarize. Omitting the
boundary rule is unsound at `A=0`, `B=1`, `C=0`: `A+1 <= B+C` holds while
`not (C <= A monus B)` does not. The same values refute an unconditional
rewrite of `C <= A monus B` to `B+C <= A`. Equality always emits
`A <= B+C` first and its uniformly-positive reverse second, independent of
which canonical equality side contains the monus.

Every operand must use only a compact input, natural literal, `LengthSum`, or
positive-literal `LengthScale`. Exactly one normalized relation operand may be
an immediate root `LengthMonus`. Both-root monus, nested or embedded monus,
mixed root-monus/root-extrema or root-quotient clauses, unsupported children
or opposite operands, negated equality, and nested Boolean formulas are
ignored whole. No rule survives a failed three-operand summary. A clause with
no immediate root monus delegates to the root-extrema predecessor unchanged.

Admission observes the normalized contract. Literal/literal monus is folded,
`A monus 0` becomes `A`, and `A monus A` becomes zero; otherwise operand order
is retained. Equality operands and top-level conjuncts are canonicalized and
conjuncts are traversed in that order. A raw monus which normalizes away is
therefore handled by the predecessor, while a retained immediate binary monus
is owned by the new scanner. Derived sums and successors exist only in exact
arbitrary-precision summaries; they create no checked syntax and consume no
syntax-node or public-literal budget.

Small scalar examples fix the boundary:

```text
(x monus 3) <= 5             ==> [8], total/applicable 9/9
1 <= (5 monus x)             ==> [4], total/applicable 5/5
not ((5 monus x) <= 2)       ==> [2], total/applicable 3/3
not (3 <= (x monus 2))       ==> [4], total/applicable 5/5
(x monus 3) = 5              ==> [8], total/applicable 9/1
```

The consequence `(x monus y) <= z`, together with `y <= 2` and `z <= 3`,
propagates through the existing closure to `[5,2,3]`; 42 of the 72 assignments
satisfy the original precondition. Conversely, `0 <= (0 monus x)` is
tautological and cannot bound `x`, so the may-zero entrance remains ordinarily
inapplicable rather than falsely establishing `[0]`.

The new scanner still contributes at most two rules per normalized clause,
matching the established relational, quotient-equality, and root-extrema
maximum. For formula-clause limit `F`, closure sees at most `2*F` rules; the
default sealed limit `F=32` therefore gives at most 64. It remains synchronous
and rule-once: each pass reads one immutable bounds snapshot, fired rules are
removed, and derived maxima merge afterward.
The existing syntax, input-width, value, and Cartesian-assignment caps bound
all other work; no monus-specific cap or lower-bound store was introduced.

The nominal binary-product entrances are
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`
and
`validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`.
Successful traversal returns opaque, nominally disjoint
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`
or
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`.
Each family exposes inclusive maxima, total assignment count, applicable
assignment count, and exact finite-spine/provider-law basis through its four
correspondingly prefixed projections.

Input-width rejection remains first; nullary problems bypass extraction and
replay `[]`. Canonical scan and closure resolve contradiction to an all-zero
carrier before missing-bound lookup. The first source-ordered missing maximum,
derived-value checks, Cartesian cap, last-input-fastest exhaustive replay,
first indexed evaluation failure or exact counterexample, nominal receipt,
and final query association retain predecessor order. Ignored clauses remain
in the original normalized precondition and are evaluated during replay.

Query validation emits no SMT-LIB, starts no worker, and consumes no solver
observation. Every predecessor API, receipt, tag, behavior, and normalized
contract-through-live identity remains byte-for-byte literal. Only the two new
nominal receipt tags add bytes:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1
```

Establishment proves complete bounded replay only in the derived rectangle,
under the checked finite-spine model and retained assumed-provider basis. It
is not universal proof, source-language totality, provider implementation
validation, solver authority, or permission to prune candidates. The additive
Boolean finite-union successor below handles bounded formula-level alternatives
without changing this single-box receipt. See the
[root-monus applicable-domain report](reports/2026-08-15-root-monus-length-applicable-domain.md).

### Boolean finite-union applicable-domain validation

The cumulative Boolean successor keeps every root-monus leaf rule literal and
adds an exact bounded DNF over the normalized formula. Its scalar problem and
query entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`
and
`validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`.
Both take `LengthEvaluationLimits`, `LengthInputBoxLimits`, and the new opaque
`LengthBooleanFiniteUnionLimits` before the problem or query.

The signed expansion has one fixed grammar:

```text
+true / -false       -> one empty branch
+false / -true       -> no branches
+not F / -not F      -> expand -F / +F
+all [F_i]           -> Cartesian conjunction of +F_i branches
-all [F_i]           -> union of -F_i branches
+(A<=B) / -(A<=B)    -> positive / strict at-most leaf
+(A=B)               -> one equality leaf
-(A=B)               -> not(A<=B) or not(B<=A)
```

The final split is exact natural disequality. Expansion never descends into
an expression-level `LengthIf`, and it does not expose a disjunction hidden
inside one extrema or monus relation. Every signed leaf delegates to the
existing root-monus clause scanner.

The raw complete-branch cap runs before cleanup. Each admitted branch then
becomes a sorted literal set: duplicate literals disappear, an exact
literal/complement pair drops the branch, equal branches deduplicate, and a
strict literal-set superset is removed by Boolean absorption. Remaining
branches are ordered canonically. Each branch independently enforces its rule
cap and closure-inspection cap. Closure retains constant-right seeding,
immutable-snapshot passes, canonical inspection order, `min` merging, and
eligible-rule-once removal. A contradiction drops that branch. All branches
finish bounded closure before the first source input missing from any live
branch returns ordinary inapplicability.

Every fully bounded branch yields one zero-origin maximum box. Equal boxes
deduplicate and a componentwise-contained box is removed; the remaining
componentwise-maximal antichain is lexicographically ordered. Incomparable
boxes are never widened to their componentwise hull. Thus `[1,3]` and `[3,1]`
remain two boxes: their raw visit count is 16 and their exact union count is
12, whereas hull `[3,3]` would add four cross-corner assignments outside both
alternatives.

The new raw visit count is the sum of retained box cardinalities, including
overlap visits. After that cap succeeds, last-input-fastest traversal inserts
assignments into `Set [Natural]`; the existing input-box assignment limit caps
new unique values. Original-problem replay uses `Set.toAscList`, so indexed
evaluation failures and counterexamples follow one global lexicographic order,
not box order. The original checked precondition remains authoritative on
every replayed value.

Empty union is a positive explicit result: it retains no boxes and records
zero visits, unique assignments, and applicable assignments without concrete
replay. Nullary true instead retains `[[]]`, consumes one visit and unique
assignment, and replays `[]`; nullary false is empty union. A nonnullary true
branch is ordinarily missing input zero. These semantics are additive and do
not change the predecessor's all-zero contradiction carrier or nullary path.

`LengthBooleanFiniteUnionLimitSource` declares, and
`mkLengthBooleanFiniteUnionLimits` validates in order, maximum generated
branches, rules per branch, closure inspections per branch, retained boxes,
and assignment visits. Their defaults are respectively 256, 64, 4096, 256,
and 262144. The five public projections begin
`lengthBooleanFiniteUnion` and end `GeneratedBranchLimit`,
`RuleLimitPerBranch`, `ClosureInspectionLimitPerBranch`, `RetainedBoxLimit`,
and `AssignmentVisitLimit`. The source fields are signed `Int` values;
negative construction fails through `LengthBooleanFiniteUnionLimitError`.

Failure precedence is width; raw branch cap; branch complement/dedup/
subsumption; canonical per-branch rule and closure caps; contradiction drop;
first missing input; box dedup/containment and retained-box cap; box/input value
checks; raw visits; unique assignment cap; global replay; first evaluation
failure or counterexample; receipt; final query association. Scalar direct
errors inhabit `LengthBooleanFiniteUnionApplicableDomainValidationError`.
Product direct errors inhabit the nominal
`LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError`; query
wrappers use the correspondingly nominal
`LengthSMTLibBooleanFiniteUnionApplicableDomainValidationError` and
`LengthSpinePairSMTLibBooleanFiniteUnionApplicableDomainValidationError`.

Complete traversal returns opaque, nominally disjoint
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`
or
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`.
Their six projections expose canonical inclusive maximum boxes, box count, raw
visit count, unique assignment count, applicable count, and the exact
finite-spine/provider-law basis. Their tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1
```

No DNF, rule, box, assignment set, or operational limit enters SMT-LIB or a
problem/query identity. Every predecessor API, receipt, scanner, tag, and
contract-through-live byte sequence remains literal. The query wrapper emits
no command and consumes no status; it supplies only exact evidence/problem
association. Establishment is complete only for the checked finite-spine model
and retained provider-law basis. It is not source-language realization,
provider validation, solver authority, universal proof, or pruning authority.

Disjunctive extrema orientations and may-zero monus laws remain atomic-leaf
gaps. Supporting them requires a separately named and tagged successor whose
atomic scanner emits exact ordered branches all-or-nothing under a frozen cap.
It must reuse explicit branch/box antichains and global union replay, never
silently modify this v1 or introduce a componentwise hull. See the
[Boolean finite-union applicable-domain report](reports/2026-08-15-boolean-finite-union-length-applicable-domain.md).

### Atomic-branching Boolean finite-union applicable-domain validation

The cumulative atomic-branching successor preserves the complete Boolean-DNF
finite-union pipeline above and opens the exact disjunction carried by one
admitted immediate root-extremum or may-zero root-monus atom. Its scalar
problem and query entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`
and
`validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`.
The nominal product entrances insert `SpinePair` after `Length`. All four take
the existing `LengthEvaluationLimits`, `LengthInputBoxLimits`, and
`LengthBooleanFiniteUnionLimits`; this checkpoint introduces no limit or
failure type.

Let `A` and `B` be the normalized children of the one immediate binary root,
and let `C` be the independently summarized positive-affine opposite operand.
The exact extremum alternatives are:

```text
C <= max(A,B)          -> [C<=A] | [C<=B]
min(A,B) <= C          -> [A<=C] | [B<=C]
not(max(A,B)<=C)       -> [C+1<=A] | [C+1<=B]
not(C<=min(A,B))       -> [A+1<=C] | [B+1<=C]
max(A,B)=C             -> [A<=C,B<=C,C<=A]
                           | [A<=C,B<=C,C<=B]
min(A,B)=C             -> [C<=A,C<=B,A<=C]
                           | [C<=A,C<=B,B<=C]
```

Equality accepts the root on either canonical side. Alternatives are ordered
first normalized child then second normalized child, and rules inside each
alternative have exactly the displayed order. Existing supported extremum
orientations remain singleton predecessor alternatives; they are not
reinterpreted by this successor.

For `M = A monus B` and a nonconstant affine `C` whose constant is zero, the
may-zero laws are:

```text
C <= M       -> [C<=0] | [B+C<=A]
M = C        -> [A<=B+C,C<=0] | [A<=B+C,B+C<=A]
C = M        -> [A<=B+C,C<=0] | [A<=B+C,B+C<=A]
```

These alternatives are ordered zero branch then bound branch. The equality
predecessor consequence `A<=B+C` remains first in both alternatives; the zero
branch is deliberately not rewritten as `A<=B`. Uniformly positive and
identically zero opposite operands retain their existing singleton root-monus
behavior. All other relational, strict, quotient, extremum, and monus leaves
also remain singleton predecessor results, including explicit ignored and
contradictory coverage.

All three operands are summarized before any new alternative is emitted.
They must use the predecessor positive-affine grammar of compact inputs,
naturals, sums, and positive-literal scales. Exactly one relation operand may
have the admitted immediate binary root. Both-root, nested, embedded, mixed,
or normalized effectively n-ary extrema/monus shapes, unsupported children,
expression-level conditionals, and other non-affine operands are ignored as
whole atoms. No convenient child rule survives a failed summary, and no
positivity is borrowed from another literal.

Admission counts the lazy Cartesian product of complete raw formula-DNF
branches and every per-literal atomic alternative under the existing generated
branch cap. This happens before complement removal, duplicate removal, or
Boolean absorption. Only after that cap succeeds are the original checked
formula literals converted to the existing canonical `Set` antichain. Each
surviving set is traversed in `Set` order and expanded into explicit
`RelationalPositiveAffineClauseCoverage` alternatives. The implementation
does not manufacture a `LengthFormula` for an atomic alternative, does not put
proof rules in a `Set`, does not add `Eq` or `Ord` to the rule type, and does
not deduplicate equal rule alternatives. Public branch indices name this
expanded canonical stream.

Each expanded branch then follows the predecessor finite-union order
literally: collect ordered rules, enforce the existing per-branch rule cap,
run bounded synchronous rule-once closure, drop contradiction, reject the
first source input unbounded in a live branch, derive the canonical box
antichain, enforce box/value/visit/unique-assignment caps, and replay the
original checked problem over one global lexicographic assignment set. With
the default rule limit 64, a 65th collected rule reports the existing bounded
`limit+1` rule-cap failure. The existing default generated-branch, closure,
box, and visit limits remain 256, 4096, 256, and 262144. No result is widened
to a componentwise hull: `[1,3]` and `[3,1]` still mean two boxes, 16 visits,
and 12 unique assignments.

Input width still precedes raw generation. Rule and closure work for all
expanded branches precedes missing coverage; box antichaining precedes value,
visit, and unique-assignment checks; original-formula replay keeps the first
global evaluation failure or counterexample; receipt construction precedes
final query association. Scalar and product validators reuse
`LengthBooleanFiniteUnionApplicableDomainValidationError`,
`LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError`, and their
existing SMT-LIB wrappers without new constructors or changed precedence.

Complete traversal returns the fresh opaque six-field receipts
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`.
Their six correspondingly prefixed projections expose inclusive boxes, box
count, assignment visits, unique assignments, applicable assignments, and the
finite-spine/provider-law basis. Their exact tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1
```

The new receipt embeds its new tag and result fields directly; it neither
wraps nor coerces predecessor evidence. Every predecessor problem validator,
query wrapper, receipt, error, tag, normalized problem/query byte sequence,
and runtime identity remains literal. Atomic alternatives, proof rules,
limits, boxes, and replay sets enter neither SMT-LIB nor query identity. Query
validation emits no command and consumes no solver observation. Establishment
remains bounded replay authority only for the checked finite-spine model and
retained provider-law basis, not source-language realization, provider
validation, universal proof, solver authority, or pruning authority. See the
[atomic-branching applicable-domain report](reports/2026-08-15-atomic-branching-length-applicable-domain.md).

### Recursive piecewise-affine Boolean finite-union applicable-domain validation

The recursive piecewise-affine successor preserves the atomic-branching
validator above literally, then opens an exact recursive case split only when
that predecessor returns the singleton ignored coverage for a relational atom
which still contains a minimum, maximum, or natural monus. Its scalar problem
and query entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`
and
`validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`.
The nominal product entrances insert `SpinePair` after `Length`. All four keep
the existing evaluation, input-box, and Boolean finite-union limit arguments.
This is an experimental current-tree specification, not a stability or
backward-compatibility promise. The public names, tags, errors, and exact bytes
may be revised before a stable release; predecessor parity below describes the
current regression boundary only.

Each recursively admitted expression is a finite ordered stream of guarded
signed-affine cases. Variables, natural literals, sums, and retained
positive-literal scales have their ordinary affine meaning. A minimum,
maximum, or monus takes the Cartesian product of the cases of its left child
and then its right child, and appends these exact selector choices:

```text
min(L,R)  -> [L<=R; value L] | [R+1<=L; value R]
max(L,R)  -> [R<=L; value L] | [L+1<=R; value R]
L monus R -> [L<=R; value 0] | [R+1<=L; value L-R]
```

Ties therefore choose the first normalized child. A sum expands its terms
left to right; an outer expression retains all left-descendant guards before
right-descendant guards and appends its own selector guard last. Once both
relation operands have expanded, positive at-most appends `L<=R`, strict
at-most appends `R+1<=L`, and equality appends `L<=R` then `R<=L`. This order
is public through generated-branch admission, per-branch cap failures, closure
work, and the eventual canonical box antichain.

The selected monus-positive value may have negative constants or
coefficients, so signed affine summaries are private proof intermediates.
Before closure, every inequality moves its negative terms to the opposite
side and becomes one ordinary positive-sided
`RelationalPositiveAffineRule`. The existing natural closure remains the sole
bound authority; no signed bound store, lower-bound solver, or unchecked
formula syntax is introduced.

The fallback accepts extrema and monus recursively on either relation side,
including nested, embedded, both-root, mixed extrema/monus, and normalized
effectively n-ary trees. Admission remains all-or-nothing: one unsupported
descendant rejects the complete recursive interpretation of that atom and
leaves the predecessor's ignored coverage in place. The recursive expression
grammar does not descend through quotient, modulo, or `LengthIf`, and it does
not admit an out-of-range variable or a retained zero scale. This adds no
recursive quotient, modulo, conditional, result-reference, or general
nonlinear authority. Any atom already handled by the atomic predecessor,
including its exact root laws and all delegated predecessor leaves,
retains that exact singleton or alternative stream and is never
reinterpreted.

Raw admission uses the same lazy Cartesian stream as the predecessor, now
including every recursive selector choice in every atomic alternative. It
counts complete formula-DNF branches before complement removal,
deduplication, absorption, guard contradiction, rule collection, or box
cleanup. After that cap succeeds, the original formula-literal sets are
canonicalized exactly as before and only then re-expanded in set order into
recursive coverage alternatives. Public branch indices name that expanded
canonical stream. Contradictory selector cases still consume raw admission
work, and all bounded cap failures retain the existing `limit+1` observation.

No new limit, default, or error type is added. The successor reuses
`LengthBooleanFiniteUnionLimits` with defaults 256 generated branches, 64
rules per expanded branch, 4096 closure inspections per branch, 256 retained
boxes, and 262144 raw assignment visits. Scalar and product direct validation
reuse `LengthBooleanFiniteUnionApplicableDomainValidationError` and
`LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError`; their
SMT-LIB wrappers likewise reuse the existing nominal query error types.
Precedence remains input width; raw generated branches; per-expanded-branch
rules; per-branch closure; first globally missing input; retained boxes;
maximum values; raw visits; unique assignments; global lexicographic replay
of the original formula; receipt; and final query association.

The frozen default 64/65 witness uses 31 clauses: one embedded recursive
maximum equality contributes three rules, two atomic maximum equalities
contribute three each, and 28 root-maximum upper relations contribute two
each. Its eight raw alternatives stay below the generated-branch default, so
the existing rule-cap error observes 65.

For compact inputs `x` and `y`, the normalized precondition

```text
max(x,y) <= 3 monus min(x,y), x <= 3, y <= 3
```

retains the exact recursive antichain `[[2,3],[3,2]]`: two boxes, 24 visits,
15 unique assignments, and ten applicable assignments. The atomic predecessor
ignores the recursive atom and therefore retains enclosing box `[[3,3]]`, one
box, 16 visits, 16 unique assignments, and the same ten applicable assignments
only because original-formula replay filters the box. The successor derives the
smaller exact cover without manufacturing a componentwise hull.
The recursive atom has eight raw alternatives; a cap of seven observes eight.

For a product fixture, let

```text
u = min(x,y) + (x monus y)
v = min(x,y) + (y monus x)
```

and use `max(u,v) <= 2`, `x <= 3`, and `y <= 3`. Each of `u` and `v` has four
recursive cases and the outer maximum has two, so the raw atom contributes 32
alternatives. A generated-branch limit of 31 observes 32; 32 admits it even
though contradictory cases later disappear. Recursive validation retains
`[[2,2]]`, one box, nine visits, nine unique assignments, and nine applicable
assignments. The atomic predecessor retains `[[3,3]]`, one box, 16 visits, 16
unique assignments, and nine applicable assignments.
Both canonical fixtures retain `ProviderIndependentFiniteSpineModel`.

Successful scalar and product traversals return the fresh opaque six-field
receipts
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`.
Their correspondingly prefixed projections expose canonical inclusive boxes,
box count, visits, unique assignments, applicable assignments, and the exact
finite-spine/provider-law basis. Their exact tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-recursive-extrema-monus-piecewise-affine-branching-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-recursive-extrema-monus-piecewise-affine-branching-precondition-domain-establishment/v1
```

These nominal receipts embed their own tags and result fields; they do not
wrap or coerce predecessor evidence. Recursive cases, guards, signed affine
summaries, proof rules, operational limits, boxes, and replay sets enter
neither checked problem nor query identity. Query validation emits no command
and consumes no solver observation. Establishment remains bounded replay
authority only for the exact checked finite-spine model and retained
provider-law basis. It does not establish source-language realization,
termination, totality, or effects; validate provider implementations; make
solver status authoritative; prove behavior outside the checked model; or
authorize pruning. See the
[recursive piecewise-affine applicable-domain report](reports/2026-08-15-recursive-piecewise-affine-length-applicable-domain.md).

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
execution instead has a distinct protocol plan and phase machine, nominal
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
`Prod` handoff, product-specific ranking and presentation path, and startup
configuration versions 4 and 6. Scalar and product behavioral authority remain
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

The mechanism, rollback boundary, compatibility identities, and adversarial
replacement characterization are recorded in the
[descriptor-bound Z3 main-image launch report](reports/2026-08-15-descriptor-bound-z3-main-image-launch.md).

#### Effective-ID executable-access descriptor launch

`mkLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessExecutionConfig` is
the additive Linux sibling for callers that also require the opened source to
pass an effective-credential VFS execute-access check. The closed public
classifier is
`LengthSMTLibDescriptorBoundEffectiveIDExecutableAccessLaunch`; the existing
`lengthSMTLibExecutionExecutableLaunchStrategy` projection distinguishes all
three strategies without revealing a path, digest, descriptor, access result,
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
fresh/shared/scoped scalar and binary-product run identities. Both established
strategies retain their literal identities. No query, protocol, contract,
behavioral receipt, or evidence schema changes. Linux 6.14's distinct
`AT_EXECVE_CHECK` facility is intentionally not selected opportunistically;
adopting that stronger and differently scoped operation would require a new
versioned strategy and identity.

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
[ground constraint-discharge report](reports/2026-08-13-length-ground-constraint-discharge.md).

### `Semantic.Length.SMTLib`

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

`probeLengthSMTLibCounterexampleAtOrigin` specializes that exact boundary to
the canonical all-zero assignment. It derives the compact input count only
from the checked problem privately retained by the query, constructs no symbol
or contract projection, and then delegates to ordinary query-owned replay.
Consequently a hit is a fresh ordinary counterexample receipt, a miss has no
positive authority, and replay rejection retains the established evaluation or
association error. The pure probe issues no SMT-LIB, consumes no observation,
and changes no identity bytes or schema. See the
[query-owned origin-probe report](reports/2026-08-14-query-owned-length-origin-probe.md).

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
[directly bounded applicable-domain report](reports/2026-08-14-directly-bounded-length-applicable-domain.md).

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
[positive-affine applicable-domain report](reports/2026-08-14-positive-affine-length-applicable-domain.md).

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
[relational positive-affine applicable-domain report](reports/2026-08-15-relational-positive-affine-length-applicable-domain.md).

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
[strict relational positive-affine applicable-domain report](reports/2026-08-15-strict-relational-positive-affine-length-applicable-domain.md).

The fifth explicit coverage policy adds exact consequences for one positive
checked quotient at a top-level relation operand's root. Its problem entrances
are
`validateLengthProblemStrictRelationalPositiveAffineQuotientApplicableDomain`
and
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientApplicableDomain`;
the query-owned entrances are:

```haskell
scalarValidation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits pairQuery
```

For positive `d` and exact positive-affine `A` and `B`, the proof-only laws
are:

```text
q_d(A) <= B        <=>  A <= d*B + (d - 1)
A <= q_d(B)        <=>  d*A <= B
not (q_d(A) <= B)  <=>  d*(B + 1) <= A
not (A <= q_d(B))  <=>  B + 1 <= d*A
```

An equality with exactly one root quotient contributes its left-to-right and
reverse inequalities, in that order. The dividend and opposite operand must
use only compact inputs, natural literals, `LengthSum`, and positive-literal
`LengthScale`. The implementation scales exact arbitrary-precision summaries
and then uses the existing coefficient cancellation and synchronous rule-once
closure. No generated proof term becomes checked syntax or changes a checked
contract or query.

Thus `q_3(2*x + 1) <= 2` derives `[3]` with total/applicable counts 4/4;
`q_3(x) = 4` derives `[14]` with counts 15/3; and
`not (4 <= q_3(x))` derives `[11]` with counts 12/12. The chain
`x <= q_3(y), y <= 8` derives `[2, 8]`, total count 27, and applicable count
18. The nominal product case `q_3(x) <= 2` derives `[8]` and counts 9/9.

Exactly one quotient must occur at the relation root. Nested, embedded, or
both-root quotients, unsupported quotient children or opposite operands,
negated equality, and nested Boolean structure contribute no rule. Modulo,
monus, extrema, conditionals, results, and other non-affine expressions remain
excluded. Any unsupported quotient clause is ignored as a whole for coverage,
while quotient-free clauses delegate to the strict predecessor unchanged.

Width and nullary handling remain first. Contradiction wins over missing
coverage; otherwise missing bounds are source-ordered inapplicability. Derived
values, Cartesian count, indexed evaluation, first counterexample, receipt,
and query association retain the established precedence. The new opaque
receipts are
`ValidatedLengthStrictRelationalPositiveAffineQuotientApplicableDomain` and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientApplicableDomain`.
Their maxima, counts, and basis projections are nominally separate. Query
validation emits no command and consumes no solver observation.

Only their scalar and product receipt tags add bytes. All predecessor APIs,
receipts, and contract-through-live identities remain exact, including sealed
query commands and fingerprints. Root extrema, root monus, and bounded Boolean
finite unions are the cumulative successors below. The finite-union entrance
has its own receipt and work caps and replays explicit branch boxes rather than
widening them to one componentwise-maximum rectangle.
See the
[strict relational positive-affine quotient applicable-domain report](reports/2026-08-15-strict-relational-positive-affine-quotient-length-applicable-domain.md).

The cumulative root-extrema policy preserves that complete predecessor and
adds four exact, conjunctive consequences for one immediate normalized binary
minimum or maximum at a relation operand's root. Its problem entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`
and
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`;
the query-owned entrances are:

```haskell
scalarValidation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits pairQuery
```

For exact positive-affine `A`, `B`, and `C`, the complete new rule set is:

```text
max(A, B) <= C        <=>  A <= C and B <= C
C <= min(A, B)        <=>  C <= A and C <= B
not (min(A, B) <= C)  <=>  C + 1 <= A and C + 1 <= B
not (C <= max(A, B))  <=>  A + 1 <= C and B + 1 <= C
```

All three operands must use only compact inputs, natural literals,
`LengthSum`, and positive-literal `LengthScale`. They are summarized before
either component rule is retained, so admission is all-or-nothing. Strict
successors are inserted before exact cancellation. Equality with a single
root maximum in either position contributes only `A <= C`, then `B <= C`;
equality with a single root minimum contributes only `C <= A`, then `C <= B`.
The omitted half is disjunctive. Consequently equality orientation is
irrelevant, and no unsafe reverse decomposition is performed.

Contract normalization already flattens, combines literals, deduplicates,
sorts, and left-associates extrema; it also canonicalizes equality sides and
top-level conjunct order. Admission is against that normalized tree. The
scanner retains rules in canonical clause order and normalized first-child,
second-child order. A retained three-term extremum has a nested extremum child
and is ignored whole, while a literal-only raw extremum may fold away and
delegate to the root-quotient predecessor.

Thus `max(x, 2*x + 1) <= 5` derives `[2]` and counts 3/3;
`2*x + 1 <= min(x + 5, 7)` derives `[3]` and counts 4/4;
`not (min(x + 4, 9) <= 2*x)` derives `[3]` and counts 4/4; and
`not (5 <= max(2*x, x + 1))` derives `[2]` and counts 3/3. The equality
`2*x + 1 = min(x + 5, 7)` derives `[3]`, then exhaustive replay records one
applicable assignment. `max(x,y) <= z, z <= 4` propagates to `[4,4,4]` with
125 total and 55 applicable assignments.

The ordinary closure remains synchronous and rule-once: every pass reads one
immutable bounds snapshot, fired rules are removed, and derived maxima merge
after the pass. It deliberately does not compute a numeric least fixed point.
Input width, contradiction-before-missing, derived values, Cartesian count,
indexed evaluation, first counterexample, receipt, and exact query association
retain predecessor precedence. Unsupported clauses still participate in
concrete precondition replay if other clauses establish a complete box.

Disjunctive orientations, both-root, nested, embedded, and retained n-ary
extrema, mixed extrema/quotient roots, unsupported children, negated equality,
and nested Boolean formulas contribute no rule or partial consequence. This is
not general extrema reasoning or finite-union analysis.

The opaque receipts are
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaApplicableDomain`.
Their correspondingly prefixed projections expose maxima, total/applicable
counts, and the retained model/provider basis. Only their two receipt tags add
bytes:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-precondition-domain-establishment/v1
```

Every predecessor receipt and sealed query command, symbol plan, value request,
fingerprint, protocol, process, worker, run, and observation identity remains
literal. Query-owned validation performs no live operation and consumes no
solver result. Establishment proves only complete bounded replay in the exact
checked finite-spine model and retained assumed-provider basis; it is not
source-language totality, universal proof, provider validation, or pruning
authority. Immediate natural-monus consequences are the cumulative successor
below. See the
[root-extrema applicable-domain report](reports/2026-08-15-root-extrema-length-applicable-domain.md).

The cumulative root-monus policy preserves the complete root-extrema
predecessor and adds immediate normalized `LengthMonus` consequences. Its
problem entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`
and
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`;
the query-owned entrances are:

```haskell
scalarValidation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain
    defaultLengthEvaluationLimits defaultLengthInputBoxLimits pairQuery
```

Let `M = A monus B`. After all three positive-affine operands have been
summarized, the five admitted shapes are:

```text
M <= C                 <=>  A <= B + C
C <= M, if min(C) > 0  <=>  B + C <= A
not (M <= C)           <=>  B + C + 1 <= A
not (C <= M)           <=>  1 <= C and A + 1 <= B + C
M = C or C = M          ==>  A <= B + C
```

Here `min(C)` is the exact constant in the nonnegative affine summary
`C = c + sum(k_i*x_i)`. When `c > 0`, `C` is uniformly positive. Direct
`C <= M` then emits the exact `B+C <= A`. An identically-zero `C` makes that
direct relation tautological and emits no rule. A zero-constant `C` with
coefficients may be zero, so its exact form is
`C=0 or B+C<=A`; the whole direct clause is ignored rather than approximated.
No cross-clause lower bound and no weaker `C<=A` grants authority.

Equality always emits `A<=B+C`. It appends `B+C<=A` when `c>0`; with
identically-zero `C`, the first rule is exact `A<=B`; with a may-zero affine
`C`, it is only the necessary supported half. The strict reverse case emits
`1<=C` first and `A+1<=B+C` second, atomically. Omitting the boundary rule or
unconditionally rewriting the direct reverse relation is refuted by
`A=0,B=1,C=0`.

Every `A`, `B`, and `C` must contain only compact inputs, natural literals,
`LengthSum`, and positive-literal `LengthScale`. Exactly one relation operand
may be an immediate normalized root monus. Both-root, nested, embedded, mixed
root-monus/root-extrema or quotient, unsupported operands, negated equality,
and nested Boolean syntax contribute no rule or partial consequence. Clauses
without an immediate root monus delegate to root-extrema unchanged.

Normalization folds literal/literal monus, `A monus 0`, and `A monus A`
before admission. It preserves retained monus operand order while
canonicalizing equality operands and top-level conjunct order. Proof-summary
addition and successor insertion use arbitrary-precision naturals and create
no checked syntax. At most two rules are emitted per clause, so a seal with
clause limit `F` yields at most `2*F` rules; the default `F=32` gives 64.
Synchronous immutable-snapshot, eligible-rule-once closure is unchanged.

Representative scalar results are:

```text
(x monus 3) <= 5        ==> [8], counts 9/9
1 <= (5 monus x)        ==> [4], counts 5/5
not ((5 monus x) <= 2)  ==> [2], counts 3/3
not (3 <= (x monus 2))  ==> [4], counts 5/5
(x monus 3) = 5         ==> [8], counts 9/1
```

The chain `(x monus y) <= z`, `y <= 2`, `z <= 3` derives `[5,2,3]`, with
72 total and 42 applicable assignments. The nominal product direct case has
the same `[8]` and 9/9 projections under its separate domain.

Input width, nullary singleton replay, contradiction-before-missing,
source-ordered missing bounds, derived values, Cartesian admission,
last-input-fastest indexed replay, first counterexample, receipt construction,
and query association retain predecessor precedence. Unsupported clauses are
still evaluated in the original normalized precondition when other rules
establish a complete rectangle.

The opaque receipts are
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusApplicableDomain`.
Their correspondingly prefixed projections expose maxima, total/applicable
counts, and basis. Only their tags add bytes:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-precondition-domain-establishment/v1
```

All predecessor APIs, receipt tags, normalized contracts, query commands,
fingerprints, protocols, executions, workers, runs, and observations remain
literal. Query validation emits no command and consumes no solver result.
Establishment remains bounded finite-spine/model-relative evidence, not
universal proof, source-language totality, provider validation, or pruning
authority. The cumulative Boolean finite-union entrance below handles exact
bounded formula-level alternatives without changing this single-box receipt.
See the
[root-monus applicable-domain report](reports/2026-08-15-root-monus-length-applicable-domain.md).

The Boolean finite-union policy expands the complete normalized precondition
under an exact signed DNF. Its problem entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`
and
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`;
its query-owned entrances use the same full suffix under the scalar and
`SpinePair` SMT-LIB prefixes:

```haskell
scalarValidation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    defaultLengthBooleanFiniteUnionLimits
    scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    defaultLengthBooleanFiniteUnionLimits
    pairQuery
```

Positive truth and negative false produce one empty conjunction; their
opposites produce no branches. Negation flips polarity. Positive `LengthAll`
takes the Cartesian conjunction of child DNFs, while negative `LengthAll`
takes their union. At-most is a positive or strict leaf. Positive equality is
one leaf; negative equality splits exactly into `not(A<=B)` or `not(B<=A)`.
The traversal opens no Boolean syntax inside `LengthIf` and delegates every
signed leaf to the unchanged root-monus clause scanner.

The raw complete-branch cap runs before canonicalization. Within the admitted
DNF, literals and branches deduplicate, an exact literal/complement branch is
dropped, and a strict literal-set superset is removed by absorption. Remaining
branches use canonical `Set` order. Each has an independent rule cap and
closure-inspection cap. Closure keeps the predecessor's seed partition,
immutable-snapshot passes, ordered eligible-rule-once firing, and `min`
merging. Contradiction drops one branch. After every branch closes, the first
source input unbounded in any live branch returns ordinary inapplicability.

Live branch maxima form zero-origin boxes. Equal boxes deduplicate and a
componentwise-contained box is removed. Incomparable maxima remain a
lexicographically ordered antichain; they are never replaced by their
componentwise hull. Alternatives `[1,3]` and `[3,1]` therefore retain two
boxes, 16 raw visits, and 12 unique assignments. Hull `[3,3]` would add four
cross-corner assignments outside both alternatives and has no authority here.

Raw visits sum each retained box's Cartesian cardinality, including overlap.
After the new visit cap succeeds, last-input-fastest enumeration inserts each
assignment into `Set [Natural]` under the existing unique-assignment cap.
Original-formula replay consumes `Set.toAscList`, so evaluation errors and
counterexamples follow one global lexicographic order rather than box order.
Ignored clauses still decide applicability during that replay.

Empty union retains no boxes and records zero visits, unique assignments, and
applicable assignments without concrete candidate-result evaluation. Nullary true
instead retains `[[]]`, admits one visit and unique assignment, and replays
`[]`; nullary false is empty. A nonnullary true formula is missing its first
compact input. These rules do not change predecessor contradiction or nullary
behavior.

The opaque `LengthBooleanFiniteUnionLimits` is built with
`mkLengthBooleanFiniteUnionLimits`. Its five default projections are:

```text
lengthBooleanFiniteUnionGeneratedBranchLimit                 = 256
lengthBooleanFiniteUnionRuleLimitPerBranch                   = 64
lengthBooleanFiniteUnionClosureInspectionLimitPerBranch      = 4096
lengthBooleanFiniteUnionRetainedBoxLimit                     = 256
lengthBooleanFiniteUnionAssignmentVisitLimit                 = 262144
```

`LengthBooleanFiniteUnionLimitSource` exposes the corresponding signed
`MaximumGeneratedBranches`, `MaximumRulesPerBranch`,
`MaximumClosureInspectionsPerBranch`, `MaximumRetainedBoxes`, and
`MaximumAssignmentVisits` fields. Negative fields fail in declaration order
through `LengthBooleanFiniteUnionLimitError`. Existing input-box limits still
own compact width and unique union cardinality; evaluation limits still own
assigned and intermediate values.

Failure order is width; raw branches; branch complement/dedup/subsumption;
canonical branch rule and closure caps; contradiction drop; first missing
input; box dedup/containment and retained-box cap; box/input values; raw
visits; unique assignments; global replay; first evaluation failure or
counterexample; receipt; query association. Direct failures use
`LengthBooleanFiniteUnionApplicableDomainValidationError` or the nominal
`LengthSpinePairBooleanFiniteUnionApplicableDomainValidationError`. Query
wrappers use the correspondingly nominal scalar/product SMT-LIB Boolean
finite-union error types.

The opaque receipts are
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionApplicableDomain`.
Their six projections expose inclusive maximum boxes, box count, raw visit
count, unique assignment count, applicable count, and basis. Their exact tags
are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-precondition-domain-establishment/v1
```

All predecessor scanners, receipts, errors, tags, normalized problem/query
bytes, and runtime identities remain literal. Query validation emits no
command and consumes no solver result. Establishment is bounded evidence for
the complete applicable domain in the exact checked finite-spine model and
retained provider-law basis; it is not source-language totality, provider
validation, solver authority, universal proof, or pruning authority.

Disjunctions hidden inside an atomic extrema or may-zero monus relation remain
excluded. They require a separately named and tagged branch-producing atomic
successor with all-or-nothing summaries, frozen branch accounting, the same
explicit antichains, and the same global replay. It must never silently widen
to a componentwise hull. See the
[Boolean finite-union applicable-domain report](reports/2026-08-15-boolean-finite-union-length-applicable-domain.md).

The atomic-branching finite-union policy is the separately named cumulative
successor which opens the exact disjunction of one admitted root-extremum or
may-zero root-monus atom. Its problem entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`
and
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`;
the query-owned entrances use the same full suffix under the scalar and
`SpinePair` SMT-LIB prefixes:

```haskell
scalarValidation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    defaultLengthBooleanFiniteUnionLimits
    scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    defaultLengthBooleanFiniteUnionLimits
    pairQuery
```

For normalized children `A` and `B` and positive-affine opposite `C`, it adds
these ordered extremum alternatives:

```text
C <= max(A,B)          -> [C<=A] | [C<=B]
min(A,B) <= C          -> [A<=C] | [B<=C]
not(max(A,B)<=C)       -> [C+1<=A] | [C+1<=B]
not(C<=min(A,B))       -> [A+1<=C] | [B+1<=C]
max(A,B)=C             -> [A<=C,B<=C,C<=A]
                           | [A<=C,B<=C,C<=B]
min(A,B)=C             -> [C<=A,C<=B,A<=C]
                           | [C<=A,C<=B,B<=C]
```

Equality admits the root on either side. Alternatives always follow first
normalized child then second child, and rules follow the displayed order.
With `M=A monus B` and nonconstant affine `C` having zero constant, it also
adds the zero-first exact alternatives:

```text
C <= M       -> [C<=0] | [B+C<=A]
M = C        -> [A<=B+C,C<=0] | [A<=B+C,B+C<=A]
C = M        -> [A<=B+C,C<=0] | [A<=B+C,B+C<=A]
```

The common equality consequence `A<=B+C` remains the first rule in both
choices. In particular, the zero alternative is not simplified to `A<=B`.
Uniformly positive and identically zero opposite operands, supported extrema
orientations, and all other predecessor leaves remain singleton alternatives
with their exact predecessor rule order.

Every new atom summarizes `A`, `B`, and `C` before emitting any branch. Each
must fit the established compact-input, literal, sum, and positive-scale
grammar. Exactly one normalized relation side may contain the admitted
immediate binary root. Both-root, nested, embedded, mixed, effectively n-ary,
unsupported, and expression-conditional shapes remain atomically ignored.
No partial branch survives a failed summary.

Raw branch admission now counts the lazy Cartesian product of complete
formula-DNF branches and every per-atom alternative. The existing generated
branch cap is applied to that complete product before complement cleanup,
deduplication, or absorption. After admission, canonicalization still operates
on sets of the original checked formula literals. Each retained set is then
traversed in `Set` order and expanded into explicit clause-coverage choices.
Ignored and contradictory results stay explicit. The implementation creates
no replacement `LengthFormula`, no proof-rule set, no rule deduplication, and
no `Eq`/`Ord` requirement for rules. Rule-cap and closure errors index the
resulting expanded canonical stream.

All downstream finite-union behavior is reused literally. Every expanded
branch has the existing rule and closure-inspection caps; contradictory
branches drop; all live branches must bound every source input; boxes form the
same componentwise-maximal antichain; visits include box overlap; unique
assignments are deduplicated; and the original precondition and postcondition
replay once in global lexicographic order. Incomparable boxes are never
replaced by their hull. Thus `[1,3]` and `[3,1]` still retain two boxes, 16
visits, and 12 unique assignments.

`LengthBooleanFiniteUnionLimits` and every scalar/product direct and SMT-LIB
Boolean finite-union error type are reused without extension. Defaults remain
256 generated branches, 64 rules per branch, 4096 closure inspections per
branch, 256 retained boxes, and 262144 visits. Under the default rule ceiling,
the 65th collected rule reports the existing bounded `limit+1` error. Input
width, raw product, branch rule/closure work, missing coverage, box/value,
visit, unique assignment, global replay, receipt, and query association retain
the predecessor precedence.

The new opaque receipts are
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingApplicableDomain`.
Their six projections expose canonical inclusive boxes, box count, visits,
unique assignments, applicable assignments, and basis. Their exact tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-precondition-domain-establishment/v1
```

These are fresh six-field nominal receipts, not wrappers around predecessor
evidence. Every earlier entrance, receipt, error, tag, normalized problem/query
byte sequence, and runtime identity remains literal. Atomic alternatives and
operational limits enter neither query bytes nor fingerprints; wrappers emit
no SMT command and consume no solver status. See the
[atomic-branching applicable-domain report](reports/2026-08-15-atomic-branching-length-applicable-domain.md).

The recursive piecewise-affine policy is the separately named cumulative
successor which retains every atomic-branching result and recursively opens
an extrema/monus relation only when that predecessor returns exactly one
ignored alternative. Its problem entrances are
`validateLengthProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`
and
`validateLengthSpinePairProblemStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`;
the query-owned entrances use the same full suffix under the scalar and
`SpinePair` SMT-LIB prefixes:

```haskell
scalarValidation =
  validateLengthSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    defaultLengthBooleanFiniteUnionLimits
    scalarQuery

pairValidation =
  validateLengthSpinePairSMTLibQueryStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain
    defaultLengthEvaluationLimits
    defaultLengthInputBoxLimits
    defaultLengthBooleanFiniteUnionLimits
    pairQuery
```

This documents the experimental current tree, not a stability or
backward-compatibility promise. Public names, tags, errors, and exact bytes may
be revised before a stable release. Predecessor parity here is a current
regression characterization only.

Recursive expressions retain compact inputs, naturals, sums, and
positive-literal scales, then add ordered exact cases for every nested minimum,
maximum, or monus:

```text
min(L,R)  -> [L<=R; value L] | [R+1<=L; value R]
max(L,R)  -> [R<=L; value L] | [L+1<=R; value R]
L monus R -> [L<=R; value 0] | [R+1<=L; value L-R]
```

The first child owns ties. Descendant guards follow left-to-right expression
order, the current selector guard follows them, and the final relation rule is
last: `L<=R` for at-most, `R+1<=L` for strict at-most, and `L<=R` then `R<=L`
for equality. Sums and relation operands form their Cartesian products in the
same left-first order. Signed coefficients created by the positive monus case
are private: each generated inequality moves negative terms across the
relation before entering the unchanged positive-sided closure.

The fallback therefore handles nested, embedded, both-root, mixed
extrema/monus, and normalized effectively n-ary shapes. It is all-or-nothing
per atom. An unsupported descendant leaves the predecessor ignored result;
the recursive grammar does not descend through quotient, modulo, or
`LengthIf`, and adds no conditional, result-reference, or general nonlinear
authority. Any exact predecessor alternative, rule result, or contradiction
is retained without recursive reinterpretation.

Raw branch accounting remains the lazy Cartesian product of the complete
formula DNF and all atomic alternatives, now including every recursive
selector case. The existing generated-branch cap runs before complement,
duplicate, absorption, guard-contradiction, rule, or box cleanup. After it
succeeds, canonical original-literal sets are re-expanded in set order, so
public rule and closure branch indices name the expanded canonical stream.
Contradictory cases consume raw work, and bounded cap errors retain the
existing `limit+1` observation.

No limits or errors are added. Defaults remain 256 generated branches, 64
rules per expanded branch, 4096 closure inspections per branch, 256 retained
boxes, and 262144 assignment visits. Width, raw branches, branch rules,
branch closure, global missing coverage, retained boxes, maximum values, raw
visits, unique assignments, global lexicographic original-problem replay,
receipt, and query association retain that exact precedence.

The default 64/65 discriminator fits in 31 clauses: one embedded recursive
maximum equality contributes three rules, two atomic maximum equalities
contribute three each, and 28 root-maximum upper relations contribute two
each. Its three binary choices make eight raw alternatives, so generated
admission succeeds and the existing rule-cap error observes 65.

The scalar fixture

```text
max(x,y) <= 3 monus min(x,y), x <= 3, y <= 3
```

retains boxes `[[2,3],[3,2]]`, two boxes, 24 visits, 15 unique assignments,
and ten applicable assignments. The atomic predecessor instead retains
`[[3,3]]`, one box, 16 visits, 16 unique assignments, and ten applicable
assignments. Its eight raw alternatives fail a cap of seven with observed
eight. For the product fixture

```text
u = min(x,y) + (x monus y)
v = min(x,y) + (y monus x)
max(u,v) <= 2, x <= 3, y <= 3
```

`u` contributes four cases, `v` four, and the outer maximum two. The 32 raw
alternatives fail a generated-branch cap of 31 with observed 32 and pass a cap
of 32. The recursive receipt retains `[[2,2]]`, one box and 9/9/9
visit/unique/applicable counts; the predecessor retains `[[3,3]]`, one box
and 16/16/9. Both fixtures retain `ProviderIndependentFiniteSpineModel`.

The new opaque receipts are
`ValidatedLengthStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`
and
`ValidatedLengthSpinePairStrictRelationalPositiveAffineQuotientRootExtremaMonusBooleanFiniteUnionAtomicBranchingRecursivePiecewiseAffineApplicableDomain`.
Their six projections expose canonical inclusive boxes, box count, visits,
unique assignments, applicable assignments, and basis. Their exact tags are:

```text
finite-list-spine-length/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-recursive-extrema-monus-piecewise-affine-branching-precondition-domain-establishment/v1
finite-binary-product-spine-lengths/strict-relational-positive-affine-quotient-root-extrema-monus-boolean-dnf-finite-union-root-extrema-may-zero-monus-atomic-branching-recursive-extrema-monus-piecewise-affine-branching-precondition-domain-establishment/v1
```

These are fresh nominal six-field receipts, not wrappers or coercions. Every
predecessor comparison in this checkpoint observes the current API, tag,
error, normalized problem/query bytes, and runtime identity literally; it is
not a compatibility commitment. Recursive cases, rules, limits, boxes, and
replay sets enter neither SMT-LIB nor fingerprints; query wrappers emit no
command and consume no solver status. The receipt is bounded authority only
for exhaustive replay under the checked finite-spine model and retained
provider-law basis. See the
[recursive piecewise-affine applicable-domain report](reports/2026-08-15-recursive-piecewise-affine-length-applicable-domain.md).

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
[finite binary product spine-length foundation report](reports/2026-08-14-finite-binary-product-spine-length-foundation.md)
and the
[offline product SMT and replay report](reports/2026-08-14-finite-binary-product-spine-smt-replay.md),
followed by the
[live binary-product Length/Z3 report](reports/2026-08-14-live-binary-product-spine-z3.md).
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
[shared live usable-work budget report](reports/2026-08-15-shared-live-usable-work-budget.md).
The dynamically enforced v2 scope and the retained v1 limitation are recorded
in the
[dynamically scoped live usable-work deadline report](reports/2026-08-15-dynamically-scoped-live-usable-work-deadline.md).

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
[bounded certificate-plan report](reports/2026-08-13-bounded-type-application-certificate-plans.md).

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

`Observability` is orthogonal to logical evidence and search progress. Its
`Natural` counts cannot wrap, zero-valued entries have one canonical absent
representation, mapped collisions sum exactly, and entries are inspected in
key order. Both fields of an `ObservationSnapshot` are intentionally lazy:
reading cumulative observations must not force a candidate stream, and reading
the result must not perform diagnostic accounting.
