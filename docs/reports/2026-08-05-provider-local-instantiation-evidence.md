# Provider-local instantiation evidence — 2026-08-05

> **Follow-up.** This report documents the original scalar candidate-pool API,
> which remains supported unchanged. Frontends that have established one
> complete correlated leading-binder vector should instead use the exact
> assignment API described in the
> [exact provider-instantiation assignment report](2026-08-05-exact-provider-instantiation-assignments.md).
> The later API does not reinterpret or replace
> `ProviderInstantiationCandidate` lists.
> The shared
> [six-binder widening](2026-08-10-six-binder-instantiation.md) raises only the
> finite leading-chain limit used by scalar tuple reconstruction; the scalar
> association semantics documented below remain unchanged.

## Outcome

The stable Djinn and Exference adapters now accept a bounded association
between an exact loaded provider and a type choice established by a richer
frontend. This covers an integration gap in which that frontend knows why a
polymorphic provider may be selected, but its projection into Djex deliberately
erases the source evidence which would otherwise determine the type argument.
For example, a frontend may justify selecting

```haskell
polyGlobal :: forall a. Token
```

at the already available proper type `forall x. x -> x`. The new channel lets
an engine emit the equivalent of
`polyGlobal @(forall x. x -> x)` without pretending that the first-order query
invented or inferred that polytype.

The shared value is intentionally only an assertion:

```haskell
ProviderInstantiationCandidate
  { providerInstantiationCandidateProvider :: Name
  , providerInstantiationCandidateType :: Type variable
  }
```

The frontend owns the source-language proof that the association is valid.
The checked runner owns the narrower trust boundary required by Djex: exact
session identity, type elaboration, finite shape, and confinement to the named
provider.

## Stable contract

Both runners are exported by the curated parser-neutral facades and the
umbrella `Language.Haskell.Djex` module:

```haskell
runDjinnQueryWithInstantiationCandidates
runExferenceQueryWithInstantiationCandidates
```

The historical functions are exact delegates with empty evidence:

```haskell
runDjinnQuery session =
  runDjinnQueryWithInstantiationCandidates session []
runExferenceQuery session =
  runExferenceQueryWithInstantiationCandidates session []
```

This preserves the complete historical search-plan or candidate prefix,
diagnostic precedence, and finite-budget observations when a caller does not
use the extension.

`maximumProviderInstantiationCandidates` is 32. It bounds associations across
one execution, before grouping or alpha-deduplication, rather than granting 32
entries to every provider. Each runner observes at most one list cell beyond
that limit before entering any element. Over-wide and cyclic lazy lists
therefore fail before search and cannot trap a fairness scheduler which assumes
finite input.

After ordinary request validation, each runner checks associations in source
order:

1. The `Name` must resolve to an exact loaded global in the selected sealed
   session. Djinn further requires a retained polymorphic loaded scheme.
2. The type is elaborated with that session's exact synonym and kind inventory
   and must have kind `Type`.
3. The candidate must be closed and context-free. Exference admits a ground
   monotype or a complete forall-rooted closed context-free type; it does not
   treat an application merely containing a forall as independently
   kind-proven.
4. Alpha-equivalent repetitions are removed for that provider while retaining
   first-occurrence order.

Deduplication never discards the provider key. Two names may have
alpha-equivalent schemes, but evidence attached to one does not make the other
eligible. The same rule prevents a local provider from inheriting a global's
choice merely because their visible types happen to agree.

## Djinn: direct proof-producing specialization

Djinn translates each checked association into a direct specialized premise
for the exact provider. This differs from its historical instantiation axioms:
the premise already has the specialized body, and a private proof identity
retains the provider plus its specified visible arguments. The independent
proof checker validates the specialized formula before conversion rewrites
that private identity to an application of the real provider. Evidence cannot
be restored as another global with the same scheme. A specialization of the
requested target is kept only in the diagnostic environment, preserving the
no-self-reference result boundary.

The structural provider plan and its query-relevant nominal counterpart include
the historical query-local and loaded-value instantiation premises as well as
the new direct premises, so a proof may compose both evidence sources. For a
nonempty evidence call the order is:

1. historical structural plans;
2. historical nominal plans;
3. historical query-local instantiation plans;
4. evidence-enriched provider-specialization structural and nominal plans;
   and
5. evidence-free loaded-scheme structural and nominal plans.

The enriched plans are a strict superset of the loaded tails. Scheduling that
superset first prevents a productive historical loaded proof stream from
reaching the global candidate cutoff before a supplied choice is considered.
The complete historical order remains exact when evidence is empty, because
the enriched family is then omitted. The provider family opens at most four
binders, attempts at most 512 candidate tuples, retains at most sixteen
specializations per scheme, and retains at most 32 direct provider premises
across the family. It is positive-only: a miss in this finite family cannot
strengthen negative evidence.

## Exference: exact global-only visible tail

Exference retains the checked associations as a private map on one query. Only
the ordinary exact-global lookup reads it. Scoped providers use their existing
lexical path, and a different global lookup uses only entries under its own
canonical name. The independent checker continues to resolve the visible
application against that exact global scheme.

At a global occurrence, Exference preserves this order:

1. ordinary implicit instantiation;
2. visible choices selected by ground monomorphic instance heads;
3. visible choices drawn from checked proper-type positions in the query; and
4. caller-supplied provider-local choices.

The query-derived and supplied product generators are each capped at 32 before
their results are concatenated and alpha-deduplicated. The established query
route therefore keeps priority without consuming the supplied route's own
generation allowance. Supplied arguments can instantiate no more than four
leading binders and apply only when the provider is context-free, has no free
flexible variable, and has a completely vacuous leading prefix. These guards
keep the branch evidence-directed: ordinary unification is not allowed to use
a supplied argument to solve a non-vacuous provider occurrence.

## Deliberate limits

This channel is not general rank-N inference, general impredicative
instantiation, or a proof interchange format. In particular, it does not:

- derive the frontend fact which justifies an association;
- inspect arbitrary local scopes or donate evidence between providers;
- infer, enumerate, or invent a polytype absent from caller evidence;
- make quantified bodies decomposable by either first-order unifier;
- remove the four-binder, tuple, candidate, search-step, or proof-fuel bounds;
  or
- turn a bounded miss into proof that no inhabitant exists.

Focused regressions pin success when the selected type is absent from the
query, exact equality for the empty-list delegates, finite rejection of an
over-limit cyclic list, and non-donation between alpha-identical provider
schemes. The stable API import test also pins the shared constructor, the
32-association cap, and both checked runner names at the umbrella facade.
