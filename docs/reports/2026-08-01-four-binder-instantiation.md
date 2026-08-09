# Four-binder hypothesis instantiation

Date: 2026-08-01

> **Follow-up.** The
> [loaded polymorphic values extension](2026-08-01-loaded-polymorphic-djinn-values.md)
> retains this historical query-local ordering and adds a separate appended
> family for loaded schemes and closed source monotypes.
> The later
> [scoped closed-polytype extension](2026-08-01-scoped-closed-polytype-applications.md)
> retains a bounded visible application when a selected query-local binder is
> vacuous.
> The
> [five-binder successor](2026-08-09-five-binder-instantiation.md) widens this
> same fair scheduler and the shared exact-assignment contract to five leading
> binders. The four-binder results below remain the historical boundary for
> this report.

## Outcome

Djinn now instantiates context-free hypothesis-side `forall` chains with up to
four leading binders. This closes the practical rank-N shape

```text
(forall a b c d. a -> b -> c -> d -> result)
  -> w -> x -> y -> z -> result
```

without increasing any existing search cap. Five-binder chains remain opaque,
and an otherwise empty search involving one remains `NoEvidence` rather than a
false proof of uninhabitability.

The widened rule uses the same semantic axiom as the original implementation:

```text
Opaque(forall a1 .. an. body)
  -> compile(body[a1 := s1, .., an := sn])
```

Generated evidence is still the original polymorphic hypothesis occurrence;
GHC performs its ordinary implicit instantiation after Djinn's independent
formula proof has been checked.

## Stable bounded ordering

Simply admitting a fourth binder is ineffective under the existing allowance
of sixteen axioms per scheme: a four-dimensional Cartesian product can hide an
ordinary useful tuple deep in its prefix. Enlarging the allowance would also
raise formula size and LJT branching for every query. The implementation
therefore changes tuple priority only for four-binder schemes.

One- through three-binder schemes retain their exact historical behavior:
candidate variables are sorted lexically and the complete Cartesian product is
enumerated in the same order as before. Existing result prefixes and rankings
therefore do not move.

Four-binder schemes use source first-occurrence order and draw fairly from four
lazy streams:

- contiguous source-order windows for ordinary applications;
- diagonal tuples for repeated instantiation at one supplied type;
- monotone selections in both source directions for sparse arguments;
- the complete Cartesian product as a fallback.

Windows and diagonal candidates are visited from both ends of their finite
inventories. A round-robin merge prevents a long window list from consuming
the whole per-scheme allowance before the diagonal or sparse families get a
chance. Duplicate tuples are removed lazily before they spend an instantiation
attempt.

The hard bounds remain sixteen distinct axioms per scheme, sixty-four axioms
per query, and five hundred twelve attempted tuples per query. These are
completeness boundaries only.

## Regression coverage

The checked Djinn adapter now exercises:

- ordinary four-argument use of a polymorphic hypothesis;
- a non-lexical source-order instance of an abstract four-argument type
  constructor, which cannot be synthesized structurally;
- a diagonal instance where the only available value type must fill all four
  binders;
- a sparse ordered tuple which is outside the first sixteen Cartesian entries;
- a useful repeated instance after twenty unrelated variables, proving that
  one tuple family cannot starve the others;
- a five-binder miss with no candidate and conservative `NoEvidence`.

The pairwise and cubic rank-N frontier fixtures now use five-binder transports.
That keeps them beyond hypothesis instantiation, so they continue to prove that
the occurrence-frontier plans themselves are necessary rather than being
silently rescued by the widened rule.

The complete `djinn-tests` suite passes all 72 tests, `cabal check` reports no
errors or warnings, and the source diff passes whitespace validation.

## Deliberate limits

This remains a bounded elimination rule, not general higher-rank subsumption.
Constrained hypothesis chains stay opaque because Djinn does not synthesize
their class dictionaries. Chains with five or more binders, types absent from
the sequent-supplied candidate vocabulary, and useful tuples omitted by the
fixed allowances can still produce an inconclusive search. Soundness is
unchanged: every emitted axiom describes a valid instance of a supplied
polymorphic value, and every returned proof is checked against the exact
formula plan that produced it.
