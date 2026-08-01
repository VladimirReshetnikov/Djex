# Exference context-free forall introduction — 2026-07-29

> **Historical snapshot.** This report records the first context-free goal-side
> rule and its limits at that point. The later
> [contextual rank-N introduction report](2026-07-29-contextual-rank-n-introduction.md)
> extends the rule with lexical givens and per-obligation evidence provenance.

## Scope and outcome

Exference already preserved every nested quantified type as an alpha-aware
opaque atom, instantiated a scoped polymorphic provider at a monomorphic use,
and forwarded or shallowly subsumed one quantified provider to a quantified
goal. It still could not construct a new polymorphic value. In particular,

```text
((forall a. a -> a) -> result) -> result
```

needed a synthesized polymorphic identity as the callback argument, but no
provider of that complete scheme was in scope. Search therefore exhausted its
budget even though the ordinary term `\use -> use (\value -> value)` has the
requested rank-2 type.

This slice adds a deliberately bounded goal-side rule. When ordinary search
exposes a nested, context-free `forall` as an active goal, Exference may open
its complete leading chain with fresh rigid constants and continue searching
the body. The same expected-type rule is implemented in the independent
generated-expression checker. The stable backend metadata consequently
advertises `RankNIntroduction` for Exference as well as its existing
`RankNElimination` capability.

## The bounded rule

For an active goal

```text
forall a1 .. an. body
```

whose complete leading chain has no contexts, the introduction branch
allocates fresh rigid constants `r1 .. rn` and replaces the goal with
`body[a1 := r1, .., an := rn]`. Consecutive context-free layers are opened one
at a time until ordinary structure is visible. This can be reached through an
arrow result, a callback parameter, a constructor field, or another ordinary
search step which turns that nested type into a goal.

The rule is bounded semantically and operationally:

- only an exposed goal boundary is opened; ordinary unification continues to
  treat every other quantified subtree as one opaque atom;
- every opened layer must be context-free, so a nested
  `forall a. C a => body` remains opaque;
- exact opaque forwarding and checked shallow provider subsumption retain
  their earlier search priority before structural introduction;
- the branch uses Exference's existing positive step, queue, and optional
  depth bounds, plus the finite identifier namespace. Exhausting any of those
  resources loses that branch or truncates the search; it never proves the
  goal uninhabited.

Unlike Djinn's bounded hypothesis-instantiation rule, this introduction rule
does not impose a separate three-binder policy cap. Its fresh rigid allocation
is limited by the checked finite identifier domain and the enclosing search
budgets. That distinction matters when describing the two backends' bounded
rank-N support.

## Dynamic rigid scope and escape prevention

Opening a nested `forall` cannot reuse the fixed query-root skolem plan. It
happens after search has allocated flexible inference variables, and a rigid
constant introduced at that point must not escape into any of those older
variables. For example, an ambient result metavariable may not later be solved
to the new callback skolem merely because a provider application connects the
two indirectly.

The rigid-instantiation plan therefore retains a branch-local fresh supply.
Each nested layer reserves identifiers against the environment, query,
query-root skolems, and all earlier nested layers. At the moment a layer opens,
the search node records every flexible identifier already alive and associates
each of them with the newly forbidden rigids. Flexible variables allocated
inside the layer are younger and may mention those rigids while constructing
the quantified body.

Every later unifier result is checked before it changes the persistent node.
The escape relation follows flexible-to-flexible substitution edges to a fixed
point. Thus both a direct image

```text
old := rigid
```

and an indirect chain

```text
old := young
young := rigid
```

are rejected when `old` predates the rigid scope. The check consumes the
complete simultaneous unifier result, including temporary provider variables
whose age edges matter even when only part of the substitution persists in the
search node. Allocation and scope data live in the immutable branch state, so
a failed alternative cannot publish skolems or restrictions into a sibling.

Search schedules outstanding goals breadth-first, while the independent
checker traverses the finished expression tree depth-first. Those orders can
encounter two independent quantified arguments in opposite sequences. The
numeric rigid spellings retained in generated annotations are therefore local
alpha names, not nominal evidence. Search passes the set owned by its nested
rigid scopes as name provenance; the checker allocates a disjoint canonical
set and establishes an injective alpha-renaming only for proven candidate
annotation rigids which are also absent from the sealed environment, query,
and root plan. Environment, root, and standalone caller-supplied rigids remain
nominal, every proven candidate-local rigid must be claimed by an independently
reconstructed scope, and the ordinary unifier still decides type compatibility
after renaming. Existing search queue order and result prefixes are unchanged.

This is the soundness condition which turns skolemization into introduction
rather than accidental unification. It prevents a locally rigid type from
becoming an answer for an ambient inference variable while still allowing the
body to be synthesized under that rigid assumption.

## Independent candidate checking

Search output is not trusted as evidence of its own typing rule. The expression
checker is now bidirectional at the boundaries where an expected type supplies
information bottom-up inference cannot recover:

- a lambda is checked against a known arrow;
- an application argument is checked against a known parameter type;
- a let binding is checked against its annotation; and
- a context-free expected `forall` may be opened structurally.

For a quantified expected type, ordinary inference and opaque
forwarding/subsumption are attempted transactionally first. If that path
fails, the checker restores its original allocations, constraints, and
substitutions before trying structural introduction. The structural path uses
the same nested rigid allocator and escape relation as search, recursively
opens the context-free chain, and checks the expression body under the fresh
rigids. Candidate validation therefore independently rejects both malformed
introduction and a skolem escape.

Class solving runs before the result boundary. An unresolved residual
constraint containing a dynamically introduced rigid is rejected by both
search and the checker: the local skolem cannot become a caller-visible
top-level obligation. A constraint over a root-prenex skolem remains valid,
because that skolem belongs to the checked query boundary rather than a nested
scope. The rigid-scope state records owned rigids separately from flexible
escape restrictions so this rule still applies when no flexible variable was
alive as the scope opened.

## Deliberate limits

The change is not general higher-rank inference or subsumption. In particular,
it does not add:

- introduction for a nested quantified type with a class context;
- publication of a residual class constraint containing a nested skolem;
- polymorphic-let generalization or inference of an unannotated polymorphic
  binding;
- deep subsumption through arbitrary constructor or arrow structure;
- visible type application;
- decomposition of quantified atoms by the ordinary first-order unifier; or
- invented impredicative types which are absent from the checked query and
  environment.

Root prenex opening and provider-side rules retain their existing contracts.
A contextual provider may still be instantiated at a monomorphic use, where
its direct constraints become proof obligations; that does not imply that
Exference can synthesize a new contextual polymorphic value. An exact
contextual scheme may still be forwarded opaquely. These distinctions preserve
class-evidence semantics instead of silently dropping a dictionary premise.

Search remains heuristic. A completed bounded run with no candidate is
`NoEvidence`, and identifier exhaustion is an operational truncation. Neither
condition is a logical refutation.

## Djinn quantified-wrapper follow-up

The adjacent Djinn hypothesis-instantiation rule originally collected only
bare `forall` roots which were independent of their enclosing binders as
guarded impredicative candidates. That excluded a complete query-supplied
wrapper such as
`Maybe (forall a. a -> a)`, even though selecting the wrapper invents no
quantifier and follows the same sound principle.

Djinn now collects every subtree which contains explicit quantification and
does not acquire a free reference to an enclosing binder, including structural
ancestors around a quantified atom. The query

```text
(forall a. f a) -> f (Maybe (forall b. b -> b))
```

can therefore instantiate the provider binder at the complete
`Maybe (forall b. b -> b)` shape already present in the sequent. The
enclosing-binder check, deterministic rendering, alpha-key deduplication, the
three-binder scheme bound, and the per-scheme and global instantiation caps are
unchanged at this historical checkpoint. The later
[four-binder instantiation extension](2026-08-01-four-binder-instantiation.md)
widens only Djinn's binder bound and tuple priority while retaining those
axiom and attempt caps. The broader collector loses no soundness because it
still chooses only exact subtrees supplied by the query; it does not assemble
a new polytype.

## Regression and integration boundary

The focused Exference regressions cover a structurally synthesized quantified
callback, a quantified arrow result, independent checking of every returned
candidate, rigid-identifier exhaustion, breadth-first versus depth-first
skolem allocation, forced opening of a fully context-free leading chain,
nested-skolem residual rejection, and the counterexample in which an older
result metavariable would capture a nested skolem. Existing exact
forwarding, shallow provider subsumption, provider instantiation, and opaque
atom regressions continue to pin their earlier behavior.

The stable `Language.Haskell.Djex` capability list now includes
`RankNIntroduction` for `ExferenceBackend`. The facade integration test pins
that public expectation and continues to reject duplicate or accidentally
advertised capabilities. No separate CLI-only capability contract is needed:
the unified REPL and command frontends read the same stable metadata.
