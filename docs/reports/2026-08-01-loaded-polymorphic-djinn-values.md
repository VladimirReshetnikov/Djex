# Loaded polymorphic Djinn values

Date: 2026-08-01

## Outcome

Djinn now gives a checked loaded value its ordinary per-occurrence implicit
instantiation power. Given opaque proper types and declarations such as

```haskell
seed :: Input
consume :: forall item. item -> Result
```

a request for `Result` can compose the globals through an `item := Input`
instance. The exact depth-first spelling may contain additional valid uses of
`consume`, but it reaches the closed value rather than rejecting the request.

The same rule covers a provider whose free signature variables are implicitly
quantified, a provider used independently at two closed monotypes, a
higher-kinded binder instantiated with a closed constructor, and guarded
impredicative cargo already supplied by the query.

This also repairs negative evidence. Previously, environment sealing erased a
loaded leading `forall` into one fixed free-variable body and forgot that the
source was a scheme. A valid request could therefore return
`ProvedUninhabitable`. Every retained non-target loaded scheme now marks the
finite search incomplete. If the bounded family misses, the result is
`NoEvidence`, never a false proof of non-inhabitation.

## Retained scheme inventory

The historical loaded-premise caches remain unchanged. Environment sealing
still implicitizes leading binders and produces the same structural and
nominal premise variants in the same order.

Alongside those caches, the prepared environment now retains a private
instantiation inventory:

- the exact context-free scheme for each loaded signature with at least one
  explicit or implicit binder, compiled separately by the structural and
  nominal formula compilers;
- closed, forall-free source subtrees from synonym-expanded value signatures,
  alpha-deduplicated in source order.

Free signature variables are closed by an outer binder before the scheme is
sealed. Explicit binder structure and unused binders remain present. Schemes
of every arity are retained; the four-binder eligibility limit belongs to
query-time instantiation, not environment representation.

Target exclusion is applied before the incompleteness decision. A declaration
with the requested definition name is unavailable to safe proof search, so it
cannot by itself turn the self-reference diagnostic into an inconclusive
result. Every other retained scheme conservatively disables negative evidence
because finite candidate and tuple caps can omit a valid instance.

## Closed monotype candidates

The earlier hypothesis rule could instantiate only at sequent variables,
opened-forall skolems, premise-scope variables, and already mentioned
quantified subtrees. It could not recover `Input`, `Int`, `Nat`, or a saturated
nominal application from an ordinary source type after formula compilation.

The new loaded-value tail collects exact source subtrees from the elaborated
query and loaded value signatures. A candidate is admitted when it has no free
variables and contains no explicit `forall`. Candidates are not restricted to
kind `Type`: the constructor `F` inside `F A` is a valid image for a binder of
kind `Type -> Type`.

Each complete substituted scheme body is checked against the prepared kind and
synonym environment at kind `Type` before it is compiled. An incompatible
candidate drops only that optional tuple. It cannot create an ill-kinded axiom
or abort the checked query.

The scheme seed itself is not added to its own candidate vocabulary. Guarded
impredicative candidates still come from ordinary goal and premise formulas;
this avoids prioritizing artificial self-applications merely because a global
scheme was retained.

## Search order and bounds

Loaded-value instantiation is a new positive-only tail. The complete existing
schedule runs first:

1. structural plans without instantiation axioms;
2. reachable nominal plans and their historical axioms;
3. historical structural hypothesis-instantiation plans.

The new structural family follows that prefix, with its separately compiled
nominal counterpart last when the query-directed nominal projection is
relevant. Both inherit the remaining candidate cutoff and choice-point fuel.
They never receive a fresh query budget. A non-target retained scheme keeps
their misses positive-only; a target-only scheme is excluded from safe search
and therefore does not erase negative evidence already established without
recursion. If finite fuel ends after such a refutation, an optional diagnostic
tail cannot replace it with operational uncertainty.

Historical hypothesis candidates and one- through four-binder tuple ordering
are unchanged. The loaded tail has a separate reserved proof-symbol prefix and
uses a fair tuple schedule for every arity. It alternates the query/front edge
with the recently appended environment edge, then interleaves source windows,
diagonal tuples, sparse selections, and a Cartesian tail. A large standard
environment therefore cannot hide a newly loaded closed type behind the fixed
allowance. Loaded schemes themselves also take one tuple attempt per turn;
duplicate or ill-kinded tuples from a vacuous wide scheme cannot spend all 512
attempts before a later provider is considered. Historical query-local jobs
retain their exact depth-first order.

The existing hard limits apply independently to each structural or nominal
instantiation family:

- at most four binders per scheme;
- at most sixteen retained axioms per scheme;
- at most sixty-four retained axioms per family;
- at most 512 attempted tuples per family.

These are completeness boundaries. A five-binder provider with every closed
argument available remains `NoEvidence` when no other proof exists.

## Evidence and regression boundary

An internal loaded-instantiation axiom has the same semantic shape as the
existing hypothesis rule:

```text
Opaque(forall binders. body) -> compile(body[binders := candidates])
```

The exact opaque premise is owned by the real global symbol. Proof search and
the independent checker see the reserved axiom; after checking, evidence
erasure reduces the axiom application to the global occurrence. GHC performs
ordinary implicit instantiation at each use. A single global can therefore be
restored under two distinct internal specialized premise identities in one
generated term.

Instantiation jobs retain their source declaration provenance across target
exclusion. A target-derived reserved axiom enters only the diagnostic
environment, alongside the exact target scheme; it can justify the sharper
self-reference result but can never be restored as an ordinary candidate.

Focused stable-API and CLI regressions cover:

- closed candidates supplied only by the query;
- closed candidates supplied only by loaded value signatures;
- explicit and implicit global quantification;
- result-only and function providers;
- guarded rank-N cargo;
- one global instantiated independently at two monotypes;
- a higher-kinded binder and discarded incompatible tuples;
- a five-binder miss with conservative `NoEvidence`;
- fair progress past vacuous four-binder schemes;
- target-named scheme exclusion and polymorphic self-reference diagnostics;
- the original `consume`/`seed` CLI reproduction.

## Deliberate limits

This is bounded implicit instantiation, not general higher-rank subsumption or
a class-constraint solver. Loaded schemes with direct contexts are still
rejected by Djinn's declaration boundary. Five-or-more-binder schemes, useful
tuples beyond the fixed allowances, and types absent from the finite source
vocabulary can remain inconclusive. Generated terms transporting quantified
types may still require `RankNTypes` and `ImpredicativeTypes`.

Exference's separate loaded ambiguous-context problem is unchanged. Emitting a
global visible type application such as `provider @Int` requires preserving
specified-binder provenance and extending its expression checker; implicit
forall closure alone is not enough to do that safely.
