# Contextual rank-N introduction — 2026-07-29

## Scope and outcome

Djex now admits a class context on a quantified type at the bounded positive
introduction boundaries already owned by Djinn and Exference. The syntax is
shared, but the evidence semantics deliberately remain backend-specific:

| Engine | Contextual positive `forall` | Evidence policy |
| --- | --- | --- |
| Djinn | Reopens the quantified body in a supported positive formula position. | The context is validated but contributes no LJT premise; only dictionary-independent inhabitants are accepted. |
| Exference | Opens an exposed goal's complete leading chain in search and independent checking. | Each substituted context is a lexical given for that body, and every generated obligation retains the givens active where it arose. |

This is an extension of the earlier
[context-free Exference introduction rule](2026-07-29-exference-forall-introduction.md),
not a general higher-rank solver. Ordinary unification still treats quantified
subtrees as opaque atoms, exact contextual forwarding still has priority, and
non-exact shallow subsumption remains restricted to context-free schemes.

## Exference: contextual goal introduction

For an exposed goal layer

```text
forall a1 .. an. (Q1, .., Qm) => body
```

Exference allocates fresh branch-local rigid constants `r1 .. rn`, substitutes
them through both the context and the body, and continues with

```text
body[a1 := r1, .., an := rn]
```

under the lexical givens

```text
Q1[a1 := r1, .., an := rn], .., Qm[a1 := r1, .., an := rn]
```

The structural branch commits through the complete leading chain. Later layers
inherit outer givens and append their own, including mixed context-free and
contextual layers, shadowed binder identities, and a contextual wrapper with no
binders. Unrelated sibling goals never inherit those givens. A deferred
obligation's own snapshot remains attached until resolution, even after search
has finished expanding the descendant goal which produced it.

Exact opaque forwarding and the existing context-free shallow provider
subsumption are attempted first. Contextual structural introduction is used
only when the quantified type has reached an actual goal boundary, such as a
callback argument, arrow result, or constructor field exposed by search.

## Scoped obligations

A single node can hold deferred class work from several lexical locations.
Root-prenex givens remain query-wide; only nested contexts are goal-local.
Keeping one node-global list of assumptions would be unsound: a nested
`C Int` given could accidentally solve an unrelated sibling's `C Int`
obligation after both were queued.

Exference therefore represents deferred work as a scoped pair:

```text
(givens active at origin, obligation)
```

The invariant is preserved throughout search and checking:

- a newly generated provider or class-method obligation snapshots the current
  goal's givens;
- a simultaneous type substitution rewrites both the givens and the
  obligation, because changing either side can change entailment;
- each obligation is resolved independently under the root query class
  environment extended by precisely its own givens;
- superclass closure is available from that local environment; and
- prerequisites returned by instance resolution retain the same given
  snapshot.

Only the remaining obligations are considered for publication. A residual
constraint mentioning a nested rigid is rejected, since a caller cannot supply
evidence for a skolem whose scope has ended. A root-prenex residual remains a
valid query obligation under the existing result contract.

This supports evidence-dependent synthesis. Given a class method with shape

```text
method :: forall a. C a => a -> Token
```

and a consumer of

```text
(forall a. C a => a -> Token) -> Result
```

the callback body may use `method`: its `C r` obligation is discharged by the
callback's local `C r` given. Removing the callback context makes the same body
fail independently in search and in the generated-expression checker.

## Independent checking and rigid scope

Search output is still not trusted as evidence of its own rule. At every
quantified expected type, the expression checker first attempts the established
opaque forwarding/subsumption route transactionally. Structural fallback then
allocates its own fresh rigids, substitutes each layer's contexts and body,
brackets the recursive check with those local givens, and records scoped
obligations with the same provenance discipline as search.

After checking, substitutions and candidate-owned rigid alpha-renaming are
applied to both halves of every scoped obligation. Each obligation is resolved
under its recorded givens, and the existing nested-rigid residual rejection is
applied to what remains. Search and checking can allocate local rigids in
different traversal orders without weakening the escape boundary; environment,
query-root, and caller-supplied rigids remain nominal.

The pre-existing skolem escape rule is unchanged. A flexible variable alive
before a nested layer opens may not later contain that layer's rigid, directly
or through a chain of flexible substitutions.

## Djinn: dictionary-independent contextual introduction

Djinn's checked request boundary already validates every context against the
selected class table and kind-checks the goal and class arguments together.
Its polarized formula compiler now reopens a positive quantified atom whether
its direct context is empty or nonempty. The binders receive the same
occurrence-scoped skolem treatment as before, while the context is deliberately
absent from the LJT formula.

Consequently,

```text
c -> (forall a. Eq a => a -> a)
```

can synthesize a quantified identity body, but the `Eq a` assumption cannot
supply `(==)` or any other class method. This matches Djinn's existing treatment
of a prenex query context and avoids pretending that its propositional core is
a dictionary solver.

The negative boundary remains stricter. Djinn's bounded hypothesis
instantiation still accepts only context-free leading schemes. A constrained
provider stays opaque because erasing its dictionary requirement and admitting
its body as an unconditional premise would be unsound.

## Deliberate limits

The extension does not add:

- general, deep, or impredicative higher-rank subsumption;
- non-exact subsumption between contextual quantified schemes;
- decomposition of quantified atoms by ordinary first-order unification;
- polymorphic-let generalization or visible type application;
- publication of a residual constraint containing a nested skolem;
- class-method proof power in Djinn; or
- contextual hypothesis instantiation in Djinn.

Exact alpha-aware forwarding of an identical contextual scheme remains valid.
Exference's provider-side monomorphic instantiation continues to turn direct
contexts into scoped proof obligations. Quantified types outside the explicit
positive-goal and provider-use boundaries remain opaque.

## Regression boundary

The focused suites pin both success and non-leakage:

- Djinn introduces contextual positive results and callbacks, while a
  constrained negative scheme remains opaque and a class method cannot leak
  into LJT proof search;
- Exference synthesizes and independently checks an evidence-dependent
  contextual callback;
- local superclass closure and instance prerequisites use the same lexical
  givens;
- outer and inner contexts compose across a complete mixed chain, including
  shadowed binder identities;
- a binderless local given cannot discharge an identical sibling obligation;
- substitutions update both a scoped given and its obligation;
- unresolved nested-rigid constraints are rejected; and
- the stable `Language.Haskell.Djex` facade exercises contextual introduction
  through both backend adapters.

Search-budget or finite-identifier exhaustion remains truncation rather than
negative evidence. The leakage fixture therefore also tests the scoped resolver
directly, independently of heuristic search completion.
