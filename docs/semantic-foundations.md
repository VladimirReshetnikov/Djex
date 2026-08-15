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
and observations retain their identities and bytes. Root extrema and monus,
bounded Boolean finite unions, and further launch hardening remain separate,
unranked follow-up candidates requiring their own authority, work-cap,
identity, and compatibility designs. Any future finite-union design must not
replace explicit branch boxes with a componentwise-maximum rectangle. See the
[strict relational positive-affine quotient applicable-domain report](reports/2026-08-15-strict-relational-positive-affine-quotient-length-applicable-domain.md).

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

`Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Session` now
provides that package-private live ownership checkpoint. It samples separate
secret barrier and public workspace material, launches the exact configured
argv with an empty environment in a fresh directory, and lends the resulting
worker only through a rank-N callback. On POSIX the workspace is observed
through a retained no-follow directory descriptor and matched by device,
inode, owner, mode, and canonical path; cleanup never traverses contents and
attempts only identity-checked empty-directory removal. The portable Windows
fallback explicitly claims only repeated pathname observations.

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
