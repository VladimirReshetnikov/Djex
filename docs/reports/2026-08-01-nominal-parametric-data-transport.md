# Nominal parametric-data transport

Date: 2026-08-01

> **Follow-up.** The
> [loaded polymorphic values extension](2026-08-01-loaded-polymorphic-djinn-values.md)
> adds matching structural and nominal tails for retained global schemes. The
> historical nominal family and ordering recorded below remain unchanged.

## Outcome

Djinn now preserves its historical structural datatype search while adding a
focused nominal view for rank-N transport through reachable parameterized
datatype applications. Given

```haskell
data D a = EmptyD | FullD a
```

the checked goal

```haskell
(forall a. D a) -> D (forall b. b -> b)
```

can produce the direct implementation `\x -> x`. Constructor introduction and
case-elimination candidates remain available from the structural view; the
nominal view complements rather than replaces them.

The same representation composes through loaded declarations. With

```haskell
data R = R

finish :: D (forall b. b -> b) -> R
```

Djinn can synthesize

```haskell
\x -> finish x
```

for `(forall a. D a) -> R`.

The datatype need not be written in the requested result. Given a complete
loaded chain

```haskell
data Token = Token

token :: Token
poly :: Token -> (forall a. D a)
finish :: D (forall b. b -> b) -> R
```

the closed goal `R` can produce `finish (poly token)`. The reachability slice
matches loaded result types backward from `R`, specializes their variables,
and adds their argument demands until it reaches `D`. Merely sealing the same
declarations must not activate nominal work for an unrelated goal.

All three nominal candidates deliberately follow the complete historical
structural prefix. A request must enable alternatives when an earlier
constructor inhabitant such as `EmptyD` or `R` would otherwise end first-result
search before the nominal family.

These examples require two existing bounded features to cooperate. The source
hypothesis has a context-free leading `forall`, and its complete binder chain
is instantiated at the quantified subtree already supplied by the sequent.
The new nominal datatype view keeps `D` and that argument together long enough
for the instantiation to match.

## Why two datatype views are required

Djinn's ordinary formula compiler expands a finite datatype to its constructor
sum. That is the useful primary semantics for propositional search: it gives
the prover introduction and elimination rules without adding constructor
axioms. It also preserves historical candidate order and logical negative
evidence whenever the translation is complete.

Structural expansion is not sufficient for every Haskell transport. In the
example above, source typing can instantiate one value of type `forall a. D a`
at the impredicative argument `forall b. b -> b` without deconstructing the
value. Retaining the saturated application `D (...)` as one alpha-aware
nominal proposition exposes exactly that relation. Making every datatype
nominal, however, would discard the constructor and case proofs on which Djinn
normally relies.

The prepared environment therefore seals two matching formula compilers:

- the historical structural compiler expands datatype declarations;
- the complementary compiler leaves selected parameterized datatype
  applications nominal while compiling the rest of the type normally.

Each compiler owns matching goal plans, loaded-premise views, and
instantiation-axiom translation. Mixing a structural goal with a nominal
premise or translating a nominal instantiation body structurally would be an
incoherent proof environment.

## Query-directed reachability

Nominal search is query-directed rather than enabled merely because the sealed
environment contains a parameterized datatype. After the checked request has
been elaborated and type synonyms have been expanded, a backward slice rooted
at the goal decides whether its positive demand structure can reach a datatype
with at least one parameter. The complementary projection is admitted only
when that slice succeeds.

This boundary has several observable consequences:

- an alias of a parameterized datatype is transparent in both views;
- a datatype with no parameters retains its constructor sum in both views;
- loaded positive function results and tuple elements can expose a hidden
  consumer without making every aggregate structural globally;
- datatype fields are projected only from an available value of their actual
  owner and are specialized with that occurrence's type arguments;
- function parameters introduced along the query's positive result path seed
  the same aggregate projections as query-local, zero-domain providers;
- a declaration-only `data Box a = Box a`, an unrelated parameterized
  datatype, or an unrelated loaded value does not perturb the plan family,
  candidate count, or order of a historical query.

Field projection follows declaration templates only after the loaded or local
owner occurrence has been established. This avoids treating a flexible field
parameter as a global bridge to an arbitrary rigid goal. Each traversal carries
a per-path set of visited datatype heads, which bounds direct and mutual
recursive projection even if recursive structural declarations are admitted in
the future. For a query-local hypothesis, only its own explicit leading
`forall` binders are flexible; free variables and enclosing query binders remain
rigid. Proof search remains authoritative after this conservative reachability
approximation selects the nominal family.

Reachability also matters for loaded composition. The goal formulas for
`(forall a. D a) -> R` can happen to be identical under the two compilers even
though the cached formula for `finish` is not. Structural and nominal plans are
therefore not deduplicated solely because their goal formulas compare equal;
the premise projection is part of the plan's semantics.

## Search order, bounds, and evidence

The complete historical structural no-axiom prefix runs first and retains its
established order: the primary polarized form, exact opacity, singleton
frontiers, and the bounded pairwise and triple tails. Only after that prefix
does the focused nominal family run.

For each nominal formula, search tries the plain nominal premises and, when
bounded hypothesis instantiation produces evidence, the corresponding nominal
premises plus nominal axioms. The formula translator used to build those
axioms is the same nominal translator used for the goal and premise cache.
Every attempt inherits the remaining query-wide candidate cutoff and
choice-point fuel. A second projection never receives a fresh budget.

All nominal plans are positive-only. A returned proof is independently checked
against the exact formula and premises that produced it, but exhausting a
nominal plan cannot prove the Haskell request uninhabited. Incomplete
structural premises likewise keep the whole result conservative. Candidate
deduplication happens only after checked generated clauses from the plan family
have been merged.

The underlying hypothesis-instantiation rule remains deliberately bounded:

- only context-free schemes with at most four leading binders are eligible;
- candidates come from goal variables, opened-forall skolems, premise scopes,
  and query-supplied quantified subtrees independent of enclosing binders;
- each scheme admits at most sixteen distinct axioms, a query admits at most
  sixty-four, and at most 512 candidate tuples are attempted;
- one- through three-binder schemes preserve their lexical Cartesian order,
  while four-binder schemes fairly mix source windows, repeated arguments,
  sparse selections, and a Cartesian tail.

Those are completeness limits. They do not authorize unchecked coercions or
negative evidence.

## Evidence erasure and eta boundaries

An instantiation axiom is an internal, checked identity coercion. Proof search
and the independent checker see its reserved symbol; generated Haskell erases
the symbol and uses the original polymorphic hypothesis, leaving GHC to perform
ordinary implicit instantiation.

That erasure can expose an eta redex which is valid in the propositional proof
but unsafe to contract under GHC's simplified subsumption. For the loaded
consumer, `\x -> finish x` type-checks at `(forall a. D a) -> R`, while the
apparently equivalent `finish` has the less-general argument type
`D (forall b. b -> b)`. Generation detects actual instantiation-evidence use
before erasure. Every proof which consumes that evidence takes the
conservative no-eta conversion path, so later simplification cannot expose and
contract a higher-rank application boundary indirectly. Proofs which do not
use the evidence retain the historical cleanup and eta contraction behavior.

The rule also covers selector presentation. If record elimination normalizes
to a field selector but the argument crossed the same instantiation boundary,
the candidate remains `\x -> field x` rather than being reduced to `field`.
Generated modules using these types may require both `RankNTypes` and
`ImpredicativeTypes`.

## Regression coverage

Focused Djinn tests cover:

- direct nominal transport alongside structural constructor/case candidates;
- a loaded consumer whose structural and nominal goal formulas coincide while
  its premise projections differ;
- a closed result reached through a specialized global provider/consumer
  chain;
- consumers projected from a loaded `Holder`, a loaded tuple, and a
  query-local `Holder` function parameter;
- a declaration-only `Box a = Box a` that remains inactive for an unrelated
  rigid query;
- transparent aliases of a parameterized datatype;
- unchanged structural treatment of nullary datatypes;
- preservation of unrelated historical rank-N candidate prefixes and global
  cutoff/fuel accounting;
- eta-alpha deduplication of equivalent candidates across two or more nested
  unary lambdas without changing the retained original spelling.

The integration fixtures derive loaded-consumer candidates through the checked
Djinn adapter and ask GHC 9.12 to compile them with `RankNTypes` and
`ImpredicativeTypes`, including a record-field presentation that must retain
its applied higher-rank argument. Shared generated-expression tests separately
retain the same no-eta cleanup boundary used by the proof converter and
selector projection.

## Deliberate limits

This is not general higher-rank subsumption, polymorphic-let generalization, or
general impredicative inference. Djinn does not invent a polytype: the useful
quantified argument must already occur in the query-supplied candidate
vocabulary. Constrained provider chains, chains with five or more binders,
tuples omitted by the fixed instantiation caps, and applications outside the
query's reachable slice can still leave a request inconclusive.

The nominal view is limited to parameterized datatype applications. Nullary
data continues to use its structural logic, aliases remain transparent, and
structural datatype search remains the primary semantics. A nominal miss is
always `NoEvidence`; it never turns an approximation into a refutation.
