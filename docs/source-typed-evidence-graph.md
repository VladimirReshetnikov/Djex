# Djinn source-typed evidence graph

Djinn's typed runners now check the exact returned source clause and retain a
sealed source-typed graph beside its unchanged compatibility candidate. The
graph can be consumed by the shared Length interface under the same engine's
source inventory and an explicitly sealed interpretation policy.

**Canonical Djex validation has passed; downstream Leant validation is pending.**
The [acceptance report](reports/2026-09-06-djinn-source-graphs.md) records the
compiler, runtime, frontend and independent-replay receipts separately from
source review. This guide describes the implemented authority and its precise
acceptance requirements.

The earlier [typed-result seam report](reports/2026-08-11-djinn-typed-result-seam.md)
established the opaque candidate association and deliberately returned
`DjinnTermGraphSourceTypingContextUnavailable`. Its proposed next step was to
check the final generated clause from retained source authority, require exact
graph erasure, and add each source form only when its authority was available.
The current implementation follows that source-checking boundary for ordinary,
rank-N, and impredicative terms. The historical seam report remains a record of
the earlier graph-absence implementation.

## What a successful graph means

A graph belongs to one exact candidate occurrence from one checked query. Its
root has the requested source type, with nominal type constructors, higher-kinded
applications, and flexible versus rigid variables preserved. Each term, pattern,
application, and erased type operation has the source typing evidence needed to
justify it. Its compatibility erasure equals the associated `FunctionClause`
exactly, including local identities, patterns, visible type arguments, and lets.
Alpha equivalence is appropriate for comparing closed types; it is not a
replacement for exact candidate erasure.

The raw LJT proof establishes a proposition in Djinn's formula vocabulary. That
proof alone cannot restore nominal datatype applications, source schemes, or
erased instantiations. Conversely, the shared graph sealer validates graph
structure and typing relationships but does not look up arbitrary global names
in a source inventory. The producer must retain and use the exact checked
environment and request; a `TypedGlobal` annotation is not its own authority.

The retained association spans every transformation between proof and final term:
assumption-name restoration, provider-local symbol rewriting, implicit evidence
erasure, visible type-application lowering, constructor and pattern refinement,
case and let simplification, eta-sensitive cleanup, and local-name allocation.
The producer checks the final syntax against those retained declarations. It
does not recover declaration types from names or rendered formula atoms.

The full request type is retained separately from the implicit type prepared
for search. Both are checked and synonym-expanded where required, and a
source/search correspondence guard checks their jointly opened leading binders
and class contexts. Empty leading forall wrappers do not change that
relationship. Preserving the source goal changes no search input or logical
evidence, and a different source goal cannot acquire another query's result.

The declaration adapter also retains the exact intrinsic List family: one
proper element parameter, `[]`, and `(:)` with that element and recursive list
field in order. Recognition preserves the source binder and constructor
identities; a nominal datatype cannot claim either special constructor. The
formula adapter accepts `(:)` only as a constructor of this checked family,
never as a type constructor. This does not enable recursive input elimination.

## Scopes, selections, and graph availability

Checking introduces fresh rigid identities at quantified source boundaries and
keeps them separate from flexible inference identities. Lambda checking stops
at an intervening forall even when the generated clause groups several term
parameters. Application checking uses the expected result to constrain the
whole mixed term/type spine before checking its arguments. This supports
impredicative arguments and alternating quantifiers without losing the source
type at an intermediate application.

Erased forall introductions and implicit type selections are explicit graph
nodes with checked witnesses. They erase without adding visible syntax.
`TypedVisibleTypeApplication` retains the actual argument syntax as well as its
selected type; a visible wildcard is still a visible argument, not an erased
selection. A complete provider vector is ordered across both kinds of node.
Separate tests require actual all-visible vectors from the assignment runners
under bounded alternative search; an earlier implicit candidate cannot stand
in for that check.

Retained lets are checked in their lexical scope. Generalization includes only
checker-created variables unconstrained by the surrounding context or result,
and the exact right-hand side is rechecked under the resulting scheme. Shared
payloads remain one `TypedLet` with distinct uses; they are not replaced by
duplicate graph edges or duplicated source terms.

Graph kind checking uses the admitted declaration inventory and the source
scope of each binder. An externally supplied kind vector becomes authority only
after the existing provider-assignment checker has validated that exact owner,
the complete source arity, the kinds, and the correlated arguments. That
authority follows only its permitted contiguous type-application prefix; it
cannot be borrowed through a neighboring provider, term application, local
alias, or let. Omitted vacuous choices at proper kind are completed with
intrinsic unit. Higher-kind completion uses closed representatives built from
exact admitted type constructors and finite kind-compatible partial
applications. This supplies no term provider.

Graph availability is conditional. In particular, contextual source schemes
requiring dictionaries remain explicitly unsupported. A hole, escaped rigid,
unresolved choice without admitted source authority, incompatible source kind,
or exceeded graph bound yields `DjinnTermGraphSourceTypingFailure`, not a
fabricated graph. The legacy clause, ordering, evidence, and search status
remain unchanged, and compatibility projection does not force graph checking.

One deliberate kind matrix preserves this distinction. Suppose an exact
assignment admits the unused binder of `vacuous :: forall a. Token` at kind
`Type -> Type`, and `F` has that kind. Compatible bare `vacuous` and explicit
`vacuous @F` candidates must have graphs. If historical alternative search also
returns `vacuous @Token`, that exact clause remains in the compatibility result,
but its graph must fail with the proper-kind versus unary-kind mismatch.
Neither silently dropping that alternative nor accepting its graph satisfies
the matrix. Source-valid positives and deliberately source-invalid historical
alternatives have separate, exact expectations.

## Public producer acceptance

Every source-valid positive public test must produce at least one candidate.
Every candidate observed by that test must carry a graph, have its exact
associated erasure, and retain the closed requested source type at the root.
A missing candidate or graph fails the test rather than making an empty
traversal pass. The explicit kind matrix above additionally identifies its
known incompatible legacy clause and requires its precise graph failure;
unrelated absences are not skipped.

| Area | Required examples and checks |
| --- | --- |
| Ordinary terms | Identity, composition with distinct type variables, partial function forwarding, nested tuple construction and elimination. |
| Nominal data | Nullary and parameterized constructors, constructor-field elimination, recursive-container forwarding, a real two-constructor case, and empty-case elimination. Constructor fields come from the exact instantiated declaration. |
| Intrinsic lists | Exact source-declaration retention, forced `[]` construction, a candidate using the actual `(:)` global, recursive forwarding, and unchanged absence of recursive input elimination. |
| Rank-N introduction | A returned forall after an ordinary argument, higher-rank lambda parameters, and nested polymorphic forwarding. An erased introduction must retain the original quantified type and its exact fresh rigid opening. |
| Rank-N elimination | A local scheme used at distinct types, implicit specialization, successive foralls, and alternating term/type application spines. Erased and visible type operations must stay distinct. |
| Impredicativity | A type variable selected as a closed polymorphic type, correlated images under nominal constructors, two different selected polytypes, and construction of polymorphic arguments. |
| Scope | Shadowed source binder spellings, ambient rigid variables beneath a forall, higher-kinded substitution, and polymorphic constructor fields. |
| Source providers | Exact global identity, complete unspecialized source scheme, two separately returned providers with the same result type, ordered complete provider vectors, and session replacement with a changed provider scheme. |
| Church signatures | The original total `not`, `swap`, `map`, `append`, `reverse`, and `filter` source signatures. These checks establish typing and graph coverage only; operation names do not provide a behavioral oracle. |
| Result envelope | All four typed runners preserve the corresponding legacy clauses, evidence, progress, metadata, and ordering. Compatibility projection remains lazy in an unobserved candidate tail; zero-choice queries retain their original result status. |

The [public producer suite](../djinn/test-source-graph/Spec.hs) uses explicit
bounds of 10,000 choices and 16 raw candidates for ordinary positive cases, or
32 raw candidates for selected alternatives. Its separate
[kind and ownership matrix](../djinn/test-source-graph/KindCases.hs) uses explicit
interleaved alternatives with 128 raw candidates and 100,000 choices. These are
focused test settings, not a decision-completeness claim or new product
defaults. A search calibration failure is distinct from a returned candidate
whose source graph cannot be built.

Source graph checking has an allowance of 100,000 charged source-checker steps.
Inventory kind inference, the finite closed-kind representative census, and
shared graph sealing are outside that step counter. Checking also uses the
shared default graph limits: 4,096 nodes, 16,384 edges, 4,096 pattern nodes,
4,096 type nodes, collection width 256, and 16,384 projection nodes. Node
admission is bounded during construction, and sealing checks the retained
graph. These limits do not refill proof-search choices or raw candidate slots,
and they are not an end-to-end wall-clock deadline.

The public proof generator can simplify a let away before returning a clause.
A passing ordinary query therefore cannot stand in for a retained-let test.
The [private source-checker tests](../djinn/test-source-graph-private/Spec.hs)
supply an exact clause containing a let whose
payload is used twice, check `TypedLet` and its binding scope, and require
unchanged erasure. Sharing must not be encoded as repeated graph edges: the
shared sealer rejects repeated references because tree erasure would duplicate
source occurrences.

## Private rejection and transformation gates

The following acceptance gates use the private source-checker seam and shared
graph guards. They do not expose a public function that can associate an
arbitrary graph with a checked candidate.

| Deliberate corruption | Required outcome |
| --- | --- |
| A valid proof sidecar paired with a different final clause, target, or local binding structure | Reject the graph association; retain the original checked candidate and search status. |
| An unadmitted global, a provider whose scheme changed in another session, or a synthetic provider symbol without its exact source mapping | Reject the source authority. Do not borrow a same-spelled or same-typed provider. |
| A structurally identical constructor belonging to a different nominal datatype, wrong constructor arity, wrong field type, or mismatched constructor pattern | Reject against the exact declaration and instantiated result type. |
| A local use outside its lexical binder, duplicate local identity across siblings, captured ambient variable, or an escaped forall skolem | Reject. Distinct source identities must survive even when their printed spellings coincide. |
| A wrong, reordered, incomplete, overlong, or provider-mismatched type-argument vector | Reject the affected specialization; exact slots remain correlated and source-derived. |
| A visible type argument whose syntax disagrees with its selected type, or a certificate from another provider occurrence | Reject. Implicit specialization must not acquire visible-certificate authority. |
| A forall opening with a nonfresh or flexible witness, or an inconsistent opened body/result | Reject before sealing. The source binder's role and scope are evidence, not a naming convention. |
| A hole, unresolved type metavariable without an admitted source identity, unsupported constraint evidence, or unavailable source mapping | Return an explicit graph absence, never a fabricated complete graph. |
| A graph over its node, edge, pattern, type, collection, or projection bound | Reject before returning a sealed graph; enforce node admission during construction and the remaining graph/type bounds through bounded observation and sealing, without spending or refilling proof-search fuel. |

Transformation positives should use exact before/after pairs, not assertions
that merely mirror the implementation. Include repeated-use lets, shadowing,
tuple and constructor-pattern refinement, known-constructor case reduction,
eta-expanded versus eta-contracted candidates, and mixed visible/ordinary
application spines. For deduplicated or ranked candidates, the retained clause
and graph must come from the same association; a graph from another equivalent
candidate is not interchangeable metadata.

Existing foundation tests already cover malformed graph topology, bounded
collection observation, node/pattern identity, and projection limits. Existing
API abstraction tests reject `Coercible` relabeling and `Generic` reconstruction
of opaque `TermGraph` and `TypedCandidate` values. Reuse these guards and extend
them for new node forms rather than replacing them with producer-only tests.

## Consumer and behavioral acceptance

The [Length lifecycle](library-api.md)
requires an opaque typed candidate, exact session inventory, sealed model and
contract, and a matching root-opening plan. A successful Djinn graph must be
usable directly by the shared Length sealer for its supported scalar/list and
pair domains. At minimum, exercise an identity/forwarding list candidate, a
constructor-producing candidate, and an exact supported case with the declared
case policy. A wrong session, provider inventory, root role, or contract must
remain a rejection. A solver answer alone must not become proof or pruning:
only the established independent replay path can authorize its existing bounded
result categories.

The public adapter exposes `DjinnLengthTypedCandidate` and
`djinnTypedCandidateForLength`. This conversion changes only the invariantly
empty compatibility residual-constraint slot to the graph's tagged type domain.
The clause, graph, local and type identities, details, and private certificate
association stay intact. Its matching inventory is
`djinnSessionSourceInventory`; session, contract, and graph must use that exact
source authority. No graph coercion or relabeling is part of the production
adapter.

Djex's Length REPL dispatches Djinn and Exference through their own typed
candidates and inventories, and combined mode assesses the lanes separately.
Leant likewise retains each candidate's engine and exact rendered origin.
An Exference graph cannot substitute for a Djinn occurrence merely because both
print the same term. An unavailable graph or unsupported Length interpretation
retains the candidate with an unavailable-assessment diagnostic; rejection
still requires independently replayed counterexample authority. Frontend
execution and this association boundary require their own acceptance receipts.

For Djinn Length queries, the REPL makes Haskell's implicit source universals
explicit once before constructing both the request and contract. Thus `[a] ->
[a]` and `forall a. [a] -> [a]` use the same closed source boundary. The graph
is not relabeled, and the consumer's rigid-opening checks remain unchanged.
Direct Core callers may still supply deliberately open source goals.

Recursive source checking and recursive proof search are distinct. The private
[Djinn-to-Length integration fixture](../synthesis/test-length/DjinnSourceGraphSpec.hs)
retains the exact recursive `List` case that returns empty for empty input and
the tail for a step. Djinn checks that clause against its nominal source
declarations; the shared exact zero/step policy interprets its graph and
independently replays the identity-length counterexample `3 -> 2`. Ordinary
case policy and a different constructor-field schema must reject it. This
fixture does not claim that public Djinn search emits recursive list cases:
its historical abstraction of recursive input elimination is unchanged.
Public graph tests cover nonrecursive cases and recursive-container forwarding.
Graph availability does not broaden Length's chosen model or semantic domain.

The existing [natural behavioral assertions](behavioral-synthesis.md) evaluate
or prove the supplied host assertion for the exact emitted candidate. They do
not depend on a source-typed Djinn graph and must continue to work. The six-case
graph signature group is not a substitute for those behavioral checks. Closure
must retain at least one fresh exact-emitted-candidate integration receipt at
the behavioral frontend boundary, and independently replay any changed emitted
terms. If synthesis order or emitted terms change, rerun the affected operation
matrix at its documented calibrated settings rather than crediting an older
receipt to new outputs.

## Build and evidence bookkeeping

Acceptance includes the public producer and kind matrix, private source-checker
and kind-ownership guards, shared graph/fingerprint tests, the private
source-checker-to-Length module, and real frontend integration. Public producer
tests use only the public API; private checker tests and the test-only candidate
association constructor remain confined to their same-package components.

Required closure separates source review, compilation, producer/runtime tests,
independent compiler or kernel replay, Length integration, and publication. Keep
explicit graph absence for genuinely unsupported source authority and preserve
compatibility behavior. The graph certifies a particular returned source term;
it does not make impredicative type inference decidable, promise an inhabitant
within every finite search budget, or establish an operation's mathematical
specification from its type.
