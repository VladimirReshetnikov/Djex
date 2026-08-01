# Hypothesis instantiation and guarded impredicativity — 2026-07-29

> **Historical snapshot.** This report records the hypothesis-instantiation
> slice before goal-side contexts were supported. The completed follow-ups add
> Exference goal introduction and then
> [contextual positive introduction](2026-07-29-contextual-rank-n-introduction.md)
> in both engines; Djinn's hypothesis-side rule remains context-free.
>
> **2026-08-01 follow-up.** The
> [four-binder instantiation extension](2026-08-01-four-binder-instantiation.md)
> raises Djinn's chain bound from three to four without raising its axiom or
> attempt caps. References below to a three-binder limit and a four-binder gap
> describe this historical snapshot.

## Scope

The 2026-07-28 review left one deliberate asymmetry in Djinn's bounded rank-N
model: positive context-free foralls open structurally, but a forall on the
hypothesis side of the sequent — a rank-2 argument such as
`(forall a. a -> a) -> b -> b`, or a premise producing a polymorphic result —
remained one inert opaque proposition. LJT could transport it exactly but
never use it at an instance, so semantically trivial eliminations stayed
`NoEvidence`. This slice adds the missing elimination rule as bounded,
independently erasable premise axioms.

## The rule

For every hypothesis-side opaque atom whose source is a context-free leading
chain `forall a1 .. an. t` with `1 <= n <= 3`, query search adds premise
axioms

```text
Opaque(forall a1 .. an. t)  ->  compile(t[a1 := s1, .., an := sn])
```

for candidate tuples `(s1, .., sn)` drawn from a finite, sequent-supplied
vocabulary:

- the goal's free type variables;
- every skolem allocated by opened positive foralls, in the goal and in
  premises (the polarized translation now returns those spellings instead of
  requiring anyone to parse rendered atoms);
- each premise's implicitized scope variables, collected once at sealing;
- every quantified atom the sequent already mentions, including nested
  closed forall subtrees of impredicative wrapper atoms.

The last family is a guarded form of impredicative instantiation: a binder may
be solved with a polytype, but only one the query itself supplies. Instantiated
bodies are compiled with the sealed environment's opaque translator, so
datatype structure expands normally while nested quantifiers stay alpha-stable
atoms; a worklist closure follows strictly shallower hypothesis-side foralls
exposed by those bodies. Everything is capped: three binders per chain,
sixteen axioms per scheme, sixty-four axioms and five hundred twelve
instantiation attempts per query. A failed substitution or compilation drops
that one axiom, never the query.

## Soundness

Each axiom is a self-contained semantic truth about Haskell types: any
inhabitant of `forall as. t` inhabits every instance, and GHC re-instantiates
value occurrences implicitly. The generated evidence is therefore the
hypothesis expression itself. Axiom symbols live in Djinn's private `$`
namespace, participate in LJT search and independent proof checking like any
premise, and are erased from checked proofs immediately before generated-code
conversion: an applied occurrence reduces to its argument, and a bare
occurrence becomes an explicit identity lambda that GHC checks against the
implication's rank-N domain bidirectionally.

Negative evidence is untouched by construction. A hypothesis-side forall
already marks its translation incomplete, so precisely the queries that gain
axioms were already barred from reporting `ProvedUninhabitable`; the caps lose
completeness only. Complete translations produce no axioms and keep their
decision-procedure semantics, including the sealed skolem-isolation
regression.

## Plan ordering

Axiom plans are appended after every historical plan and share the global
candidate cutoff and choice-point fuel. The first implementation ran axioms
inside the primary plan and immediately demonstrated why that is wrong: one
bottom-like hypothesis (`forall a. a`) floods the proof stream with junk-but-
sound instantiation chains, starving the frontier plans whose exact-transport
candidates rank best and rewriting the documented result prefixes of
first-only, exactly-bounded, and fuel-bounded searches. With the axiom family
appended instead, every query the historical family decides keeps its exact
results, rankings, budget accounting, and completion behavior, and the new
capability pays for itself only in the tail.

## Observable capability changes

- `(forall a. a) -> b`, `(forall a. a -> a) -> b -> b`, and sibling-instance
  transports such as `(forall a. a -> a) -> (b, c) -> (c, b)` are realized;
  the CLI regression that previously pinned the inconclusive message now pins
  `instantiate a = a`.
- Goal skolems are candidates, so the explicit
  `forall b. (forall a. a -> a) -> b -> b` spelling works identically.
- Impredicative transports such as
  `(forall a. a -> Maybe a) -> (forall b. b -> b) -> Maybe (forall b. b -> b)`
  are realized by instantiating the provider chain at a quantified atom.
- The documented four-site middle-opacity example is now realized: its
  transport components are derivable by instantiating the hypotheses at the
  opened siblings' skolems. The regression suite keeps the honest gap
  observable with four-binder chains, which the instantiation bound skips:
  that wider balanced-subset query still reports `NoEvidence` and no invented
  candidate.
- Alternatives for duplicated alpha-equal sites grow from four to nine
  clauses: the guarded impredicative self-application `a a` appears at either
  or both sites beside the historical transport/introduction combinations.

## Deliberate limits

Constrained chains stay opaque because Djinn has no premise vocabulary for
their class obligations. Chains beyond three binders, candidates outside the
sequent's vocabulary, and instantiations dropped by the caps remain
inconclusive searches. Generated code that transports quantified atoms or uses
impredicative instances may require `RankNTypes` and `ImpredicativeTypes`;
Djinn's independent checker validates the proof against the exact formula
plan, not GHC's elaboration. Candidate quality inside the axiom plans is
unranked beyond the existing unused-binder ordering, so degenerate
bottom-driven instances can appear among alternatives; they are sound, and the
appended ordering keeps them out of every historical prefix.

## Exference: guarded impredicative provider subsumption

The shallow quantified-provider rule used to reject every substitution image
containing a `forall`. That predicativity restriction was stricter than the
rule's own mechanics: the requested body is the rigid left side of
`unifyRight`, so a provider binder's image is always an exact subtree of the
requested scheme — first-order unification cannot assemble a new polytype.
The restriction is therefore replaced with an explicit Quick-Look-style
guard: an image may contain quantification when it occurs, up to alpha
equivalence, as a quantified subtree of the requested scheme itself. The
guard is presently equivalent to dropping the check, but it documents and
defends the principle against future matcher changes: no quantifier the query
did not supply is ever invented.

This turns, for example,

```text
(forall a. a -> a) ->
  (forall x. (forall y. y -> y) -> (forall z. z -> z))
```

into a realized forwarding `\f -> f` in both search and the independent
checker, which share the classifier by construction. A requested scheme whose
quantified atoms cannot share one binder image (for instance when the second
atom mentions the requested binder) still fails, as do wrong-direction
specializations, contexts, and free flexible variables. Generated impredicative
forwardings may require `ImpredicativeTypes`.

## Next slice: Exference goal-side forall introduction

The remaining large asymmetry is Exference's inability to synthesize a new
polymorphic value: a subgoal that is itself a quantified atom — a rank-2
provider argument such as the callback of
`((forall a. a -> a) -> c) -> c`, or `runST`'s `forall s. ST s a` — is only
ever satisfied by forwarding or subsumption, never by introduction. Probing
confirms the callback example exhausts the full default step budget without a
solution.

The sound design mirrors the engine's existing query-root opening but must
add scope discipline. Opening a subgoal's leading chain with fresh rigid
constants is unsound without an escape check: an ambient flexible variable
that predates the opened scope must never be solved to a type mentioning the
new rigids, directly or through later promotion. Because all ambient bindings
flow through one application point in the search step, the check can be
implemented as GHC-style level tracking: record, per dynamically allocated
rigid, the flexible variables alive at its creation; reject any binding of an
older variable to a type containing that rigid; and propagate age through
binding images. The independent checker needs the dual change, because its
bottom-up inference has no generalization step today: filling a
quantified-atom hole with a lambda must open the expected scheme with fresh
rigids under the same escape rule. Both sides must change together, and the
node state, renderer annotations, and identifier-capacity accounting all
participate, so this is deliberately its own future slice rather than a rider
on this one.
