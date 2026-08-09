# Query-local closed-monotype instantiation

Date: 2026-08-09

## Outcome

Djinn can now specialize a polymorphic hypothesis at a closed monotype already
present in the requested type. For example, the checked request

```haskell
(forall a. (a -> Token) -> a -> Indexed a)
  -> (Closed -> Token)
  -> Closed
  -> Indexed Closed
```

requires `a := Closed`. `Closed` is neither a type variable nor a quantified
subtree, so it was absent from the historical query-local candidate vocabulary.
The new rule admits that instance without changing the established historical
or loaded-value prefixes.

## Three finite families

The Djinn backend keeps the three sources of instantiation evidence distinct:

| Family | Scheme seeds | Candidate extension | Position |
| --- | --- | --- | --- |
| Historical query-local | Hypothesis-side schemes found by the existing goal and premise formula traversal | Goal variables, opened skolems, premise-scope spellings, and guarded quantified query subtrees | Historical position and order |
| Query-local closed monotype | Only hypothesis-side schemes embedded in the elaborated requested goal | The historical pool plus closed, forall-free goal subtrees; every retained tuple must contain a closed query candidate | Final structural and nominal tail |
| Loaded scheme | Exact context-free schemes retained from loaded values | The historical pool plus closed query subtrees and closed subtrees of synonym-expanded loaded signatures | Established loaded structural and nominal tails |

The second row is not an enlargement of the loaded-scheme inventory. In
particular, an ordinary global premise is not rediscovered as a query-local
scheme under a second proof-symbol family.

## Candidate construction and bounds

`closedMonotypeSubtrees` walks the elaborated goal and retains subtrees which
have no free type variables and contain no `forall`. Candidates are
alpha-deduplicated. A closed candidate may itself have higher kind; it is usable
only when substituting the complete binder vector produces a body of kind
`Type` in the prepared environment.

The combined historical/closed pool uses the fair tuple scheduler rather than
a new lexical Cartesian prefix. Filtering happens after scheduling: a retained
tuple must use at least one closed query candidate. Historical-only tuples are
therefore not duplicated, while mixed variable, quantified, and closed tuples
remain available. The existing independent bounds remain:

- at most five leading binders per eligible scheme;
- at most 16 distinct axioms per scheme;
- at most 64 axioms in the family; and
- at most 512 tuple attempts.

A vacuous binder remains supported. Proof conversion erases inferable choices
and retains the shortest checked visible application when the selected source
type must be explicit.

## Plan ordering and composition

All established structural, nominal, provider-evidence, and loaded-scheme plans
retain their previous order. The query-local closed-monotype structural tail
runs after them, followed by its nominal counterpart when that projection is
relevant. Each final plan carries the already established historical axioms,
loaded premises and axioms, and caller-supplied provider specializations. A
single proof can therefore combine the new local specialization with loaded or
provider evidence.

These plans consume the same query-wide cutoff and choice-point budget as the
earlier families. Starting the final tail does not reset either resource.

## Soundness and evidence

For an eligible context-free hypothesis scheme, every generated axiom connects
the opaque source hypothesis to the formula compiled from its completely
substituted body. Source types and substituted bodies are checked in the sealed
prepared environment, and generated proof terms are checked before inferable
evidence is erased or a visible type application is retained.

The new structural and nominal plans are always positive-only. They may add a
checked inhabitant, but exhausting their finite vocabulary never contributes
negative evidence. A bounded miss therefore remains `NoEvidence` rather than a
proof of uninhabitability.

## Validation

Validation for this slice currently covers:

- the indexed local use and mixed query-local/loaded regressions;
- the focused public-facade/GHC and vacuous-visible-application regressions;
- the complete Djinn unit suite, with all 83 tests passing;
- the public facade suite, with all 84 tests passing;
- the shared synthesis suite, with all 292 tests passing;
- the Exference engine suite, with all 28 tests passing; and
- the downstream API suite, with all 24 tests passing.

## Deliberate limits

The rule does not invent monotypes, perform general higher-rank subsumption, or
open contextual hypothesis schemes. Closed candidates must already occur in
the elaborated requested goal, every eligible leading chain is complete and no
longer than five binders, and all fixed attempt and axiom caps still apply.
Those boundaries can lose completeness, never soundness.
