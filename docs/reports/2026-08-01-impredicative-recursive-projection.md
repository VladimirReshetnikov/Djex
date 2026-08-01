# Impredicative recursive projection — 2026-08-01

## Outcome

Exference's finite recursive eliminator can project a quantified field from a
parameterized recursive value. Given

```haskell
data Headed a = HeadedValue a (Headed a)
```

the request

```haskell
Headed (forall x. x -> x) -> (forall x. x -> x)
```

can produce the generated-term equivalent of

```haskell
\headed -> let HeadedValue value _ = headed in value
```

The request needs `exferenceAllowUnused = True` (or `allow-unused` in the
merged REPL), because the recursive tail is deliberately not consumed. Source
containing these types may need `RankNTypes` and `ImpredicativeTypes`.

## Search, checking, and projection boundary

The deconstructor is retained generically. At the occurrence above, its fields
specialize to the exact quantified payload and the exact recursive tail. Search
opens one constructor layer, installs both fields as branch-local providers,
and returns the payload without feeding the tail back into eager decomposition.
The ordinary unused-variable policy then decides whether that finite candidate
is admissible.

The relaxed policy does not weaken typing. Search and the independent
expression checker both consume the complete annotated internal expression,
including the named tail binder and its specialized recursive type. Only after
that checker accepts the candidate does the stable projection convert it to
shared generated syntax and replace unused lambda or constructor-pattern
binders with wildcards. The historical typed compatibility expression retains
its annotations and binder identity.

Keeping cleanup after validation has two useful properties:

- wildcard rendering cannot make an unchecked candidate publishable; and
- the typed search and checker representations do not need an unannotated
  wildcard form merely for presentation.

## Deliberate limits

This follow-up does not add recursive calls, induction, or automatic matching
of the recursive field at a second layer. It does not invent a polytype or add
general impredicative subsumption: the useful quantified field comes from the
checked query's exact datatype specialization. The strict default is also
unchanged; without `exferenceAllowUnused`, the ignored tail keeps this
candidate out of the result set.

Focused regression coverage establishes that search terminates within a small
step bound, the internal candidate exposes exactly one constructor layer, the
payload and tail receive their specialized types, independent checking accepts
the term, and the stable candidate contains a wildcard for the unused tail.
