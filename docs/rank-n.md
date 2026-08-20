# Rank-N and impredicative types

*The bounded rule families both engines use for higher-rank and impredicative
goals — what each engine can introduce and eliminate, the numeric bounds, the
worked `:djinn`/`:exference`/`:compare` examples, and the exact places where a
search becomes inconclusive rather than negative. This is the canonical
rendering; the [README](../README.md#unified-command),
[REPL guide](repl.md#rank-n-and-impredicative-types), and
[library guide](library-api.md#djinn-example) each carry a shorter statement
and defer here for the rules and bounds.*

## What to keep in mind

Neither engine performs general higher-rank subsumption, polymorphic-let
generalization, or general visible type application. Instead each has a small
number of *deliberately bounded* rules — a `forall` may be introduced here,
eliminated there, instantiated at these candidates and no others — and every
one of them is positive-only beyond the historical structural core: reaching a
bound makes an empty search **inconclusive**, never a proof of
non-inhabitation.

Two words recur below. A rule is *positive-only* when it can only add
candidates to a search, so its running out of candidates says nothing about
whether the goal is inhabited. A search is *inconclusive* when it ended under
such a rule with no result: the evidence is then `NoEvidence` rather than
`ProvedUninhabitable` for Djinn, and Exference simply returns nothing. Only Djinn's
historical structural core — the complete LJT fragment with no bounded rule
involved — can turn an empty search into a proof of non-inhabitation. The bounds themselves (six leading binders, 512 tuples per
scheme, sixteen retained axioms per scheme, 64 in total, 512 quintuple
selections per orientation, exhaustive through eleven independent sites) are
stated where each rule is described and restated in the architecture guide's
[Djinn instantiation section](architecture.md#djinns-four-instantiation-axiom-families).

---

## Contents

- [Djinn: introduction, elimination, and guarded impredicativity](#djinn-introduction-elimination-and-guarded-impredicativity)
  - [The query-correlated tail](#the-query-correlated-tail)
  - [Query-closed and loaded families](#query-closed-and-loaded-families)
  - [Exference: introduction and elimination](#exference-introduction-and-elimination)
  - [Visible type application from providers](#visible-type-application-from-providers)
- [Provider instantiation evidence from a frontend](#provider-instantiation-evidence-from-a-frontend)
  - [Vacuous binders and higher-kinded evidence](#vacuous-binders-and-higher-kinded-evidence)
  - [The boxed-pair exception and existential observation](#the-boxed-pair-exception-and-existential-observation)
- [Declared datatypes: structural and nominal views](#declared-datatypes-structural-and-nominal-views)
  - [The query-directed slice and nominal atoms](#the-query-directed-slice-and-nominal-atoms)
  - [Plan order and instantiation-evidence erasure](#plan-order-and-instantiation-evidence-erasure)
- [What stays opaque, and what this is not](#what-stays-opaque-and-what-this-is-not)
- [Occurrence-plan families and their bounds](#occurrence-plan-families-and-their-bounds)

## Djinn: introduction, elimination, and guarded impredicativity

Rank-N support now uses deliberately bounded, backend-specific rule families.
Djinn can introduce a `forall`, including one with an already validated class
context, in a positive position: arrow results, products, and datatype fields
preserve that position, while each arrow parameter reverses it. As at the query
root, the context contributes no proof premises, so the result must remain
dictionary-independent. Djinn can also eliminate a hypothesis-side
context-free `forall` of up to six leading binders. Its historical family
instantiates the complete chain at sequent-supplied candidates: the goal's type
variables, skolems of opened positive occurrences, premise-scope variables,
and — as guarded impredicativity — query subtrees that are independent of
enclosing binders and contain quantification.

### The query-correlated tail

A separate positive-only query-correlated tail fairly revisits that same finite
variable and guarded-quantified vocabulary. It keeps only tuples which pair a
quantified candidate with a binder occurring free in the scheme body and
specialize that complete body to an alpha-equivalent subtree of the elaborated
query. Seeding its builder with the active historical axioms suppresses
duplicate logical formulas without blocking nested schemes discovered through
an excluded bridge. The fair producer observes at most 512 raw tuples per
scheme; the family builder separately charges at most 512 eligible attempts,
sixteen retained axioms per scheme, and 64 retained axioms in total.

### Query-closed and loaded families

The established query-closed positive-only family revisits only schemes
embedded on the hypothesis side of the requested goal. It adds closed,
forall-free monotype subtrees already present in that elaborated goal, retains
only tuples using at least one such closed candidate, and permits mixed tuples
with the historical candidates without changing the historical prefix. A
different appended family retains exact context-free schemes from loaded values
and may also use closed, forall-free subtrees of the query and synonym-expanded
loaded value signatures. Ordinary workspace values enter that loaded family
only after `:set djinn-axioms on`; value axioms are off by default. Every
substituted body is kind-checked, so a closed higher-kinded constructor is
admitted only when its complete use has kind `Type`. The query-correlated,
query-closed, and loaded extensions are bounded and positive-only; a missed
non-target retained loaded scheme makes the result inconclusive rather than
 proving non-inhabitation.

### Exference: introduction and elimination

Exference can introduce a nested
`forall`, with or without class contexts, once ordinary search exposes it as a
goal, for example as a callback argument or an arrow result. It opens the
complete leading chain with branch-local fresh rigid constants and treats each
layer's substituted context as lexical givens for that body only. Every
deferred obligation retains the givens under which it arose, so superclass and
instance solving can use them without letting evidence leak to a sibling goal.
Exference synthesizes the body and rejects any direct or indirect substitution
that would let a rigid escape through a flexible variable from an older scope.
Generated local skolem spellings compare up to a scope-owned alpha-renaming,
but environment and root constants remain nominal; unresolved constraints
containing a nested skolem cannot escape as result obligations. Exference can
also eliminate the complete leading `forall` chain of a scoped value at a
monomorphic use site, freshly and
independently for each occurrence; direct contexts become ordinary proof
obligations.

### Visible type application from providers

For an instantiable scoped or retained global provider, a separate bounded
branch can make the choice visible. A direct constraint may select its complete
leading binder prefix from an explicit ground instance head, producing an
application such as `provider @Int`. A context-free provider with no free
flexible variables whose leading binders are all vacuous may instead select
checked proper types already supplied below arrows or tuples in the query. Its
residual body may mention ambient rigids opened from that same query. The
candidates include complete closed context-free foralls, so search can emit
`provider @(forall a0_0. a0_0 -> a0_0)`. The query route retains at most six
binders and 32 combinations. Without a productive exact supplied assignment,
ordinary implicit instantiation remains first and the instance-head route
remains monotype-only. A productive exact assignment receives one leading
visible lane so downstream exact-spelling deduplication cannot discard its
checked association; the ordinary fallback and established
inferred/candidate-derived visible lanes remain available after it.
The scheduling correction and its empty/unusable-assignment compatibility
boundary are recorded in the
[exact-assignment association-priority report](reports/2026-08-13-exact-assignment-association-priority.md).
## Provider instantiation evidence from a frontend

A richer frontend may instead pass either backend one complete, correlated
provider-assignment vector. The legacy `ProviderInstantiationAssignment`
runners infer each positional ground kind from the exact provider body and
default an unconstrained vacuous binder to `Type`. The parallel
`KindedProviderInstantiationAssignment` runners,
`runDjinnQueryWithKindedInstantiationAssignments` and
`runExferenceQueryWithKindedInstantiationAssignments`, pair every argument
with a caller-attested `GroundKind`. Each adapter checks those supplied kinds
against all observable uses in the retained body, elaborates every argument at
its paired kind, requires repeated assignments for the same provider to agree
on the complete kind vector, and still checks the fully specialized body at
`Type`. For each assignment that passes provider, scheme, and exact-arity
checks, all its supplied kinds receive a productive node preflight under the
public `maximumProviderInstantiationKindNodes = 129` bound. That preflight
precedes the assignment's kind inference, same-provider kind-vector equality,
backend kind conversion, and paired-type forcing; cyclic kinds and finite trees
above the bound are therefore rejected without traversing an unbounded
remainder. The shared 64-tuple constructor's right-associated all-`Type` kind
has exactly 129 nodes and remains accepted.

### Vacuous binders and higher-kinded evidence

A vacuous binder supplies no body constraint from which its source kind could
be inferred; its higher kind is therefore frontend evidence, not a backend
inference. This admits a bare or partially applied higher-kinded constructor at
such a position, including vectors that also contain a closed impredicative
`Type` argument. An exact argument must be lexically closed but may itself be a
contextual polytype such as `forall a. Eq a => a -> a`; that nested context is
part of the selected type, not an obligation of the provider scheme. Multiple
distinct vectors may be retained for the same provider when their complete
kind vectors agree; regressions exercise two such
choices at the genuinely higher-order kind `(Type -> Type) -> Type`. Both
assignment forms retain the 32-vector and six-argument bounds, the kinded form
adds the 129-node per-kind bound, and contextual provider schemes remain
unsupported. The legacy scalar `ProviderInstantiationCandidate` entrances
remain proper-type-only.

### The boxed-pair exception and existential observation

Djinn's checked lowering deliberately recognizes one residual tuple head in
that higher-kinded evidence: the bare boxed arity-two constructor is preserved
and rendered canonically as `(,)`. A saturated pair such as `(Natural,
Boolean)` remains the existing structural `TupleType`; the exception does not
broaden tuple support generally. Bare or partially applied wider boxed tuple
constructors, and bare or partially applied unboxed tuple constructors, remain
fail-closed with `PartialTupleConstructorUnsupported`. Focused `djinn-tests` pin
the bare and partial pair adapter, the negative tuple boundary, non-vacuous
exact provider substitution, and contextual rank-N use. A `djex-tests` facade
regression sends `(,)` and `Either Natural` through both engines, renders their
evidence, and asks GHC to check the combined generated fixture.

Djinn marks each complete legacy scalar tuple or exact vector before
substituting it into a provider and admits the resulting non-vacuous premise to
its constructor-expanding search projection only when every reached datatype
argument remains observable through its own formal. That existential
observation is a boundary gate; Djinn then traverses the instantiated
constructor body and requires every marker-bearing field to preserve the
argument at each nested datatype boundary. This checks structure inside an
assigned type as well as later arguments applied to an assigned higher-kinded
head. It preserves useful elimination through fully faithful and correlated
constructor fields while routing wholly phantom parameters and any
independently erased occurrence through the nominal projection alone.
Recursive, unknown, and parameterized empty types retain the opaque
complete-application identity used by compilation.
Consequently structural equality cannot turn a selected visible application
into a nominally different one after phantom information is erased.
Exference can also forward a context-free quantified provider with no free
flexible variables to a less-general such goal; provider binders are solved
with monotypes or, in the guarded Quick-Look sense, with quantified subtrees
the requested scheme itself supplies. This covers, for example:

```text
:djinn c -> (forall a. a -> a)
:djinn c -> (forall a. Eq a => a -> a)
:djinn ((forall a. a -> a) -> c) -> c
:djinn (forall a. a -> a) -> b -> b
:djinn (forall a. a -> Maybe a) -> (forall b. b -> b) -> Maybe (forall b. b -> b)
:djinn (forall a. f a) -> f (Maybe (forall b. b -> b))
:compare (forall a b. f a b) -> f (forall x. x -> x) (forall y. y -> y -> y)
:exference ((forall a. a -> a) -> result) -> result
:exference (forall a. a -> a) -> Int -> Int
:exference (forall a b. a -> b -> a) -> (forall x. x -> x -> x)
```

## Declared datatypes: structural and nominal views

Nonrecursive declared datatypes remain fully structural in Djinn: their
constructor sums support introduction and case elimination and stay first in
search. Recursive datatypes use a narrower structural view. A positive logical
path may expose one real constructor layer from each of at most two distinct
alias-normalized recursive SCCs. Reopening the same SCC through direct, mutual,
alias-hidden, or parameter-mediated recursion, reaching a third SCC, every
negative occurrence, and the exact-opaque view all retain the complete
application as an atom. This lets an independent `Outer` and `Inner` compose
one finite layer each while preventing unbounded or exponentially duplicated
expansion. The exact view preserves forwarding such as `Rec a -> Rec a`, and
the positive view can construct `Done a :: Rec a` without admitting recursive
elimination, recursive calls, or induction. Every query translation that
touches this bounded rule is incomplete, so an empty search is inconclusive
rather than proof of non-inhabitation.

### The query-directed slice and nominal atoms

A query-directed backward slice additionally identifies reachable applications
of datatypes with at least one parameter. When the slice reaches one, Djinn can
run a complementary formula view that retains parameterized datatype
applications as alpha-aware nominal atoms. With these declarations loaded,

```haskell
data D a = EmptyD | FullD a
data R = R
data Token = Token

finish :: D (forall b. b -> b) -> R
token :: Token
poly :: Token -> (forall a. D a)
```

the complementary view covers direct transport, composition through a loaded
consumer, and a closed global provider/consumer chain:

```text
(forall a. D a) -> D (forall b. b -> b)  -- \x -> x
(forall a. D a) -> R                     -- \x -> finish x
R                                        -- finish (poly token)
```

These nominal candidates follow the complete structural prefix. Use
`:set select all` (or `:set select best`) to inspect them when `EmptyD` or
`R` already supplies an earlier structural inhabitant; the interactive default
`select = first` may stop before the nominal family.

Synonyms are expanded before the slice is computed, so an alias of `D` is
transparent. A nullary datatype keeps only its historical structural
constructor view. Unrelated parameterized declarations do not activate or
reshape a query's plan family.

The slice can look through positive function results, tuple elements, and
fields of an aggregate value that is actually available. Datatype fields are
specialized at that value's concrete owner arguments; a declaration such as
`data Box a = Box a` creates no reachability edge by itself. Function
parameters introduced while constructing the query result are treated as
query-local providers, so the same projection works for a goal such as
`Holder -> R`. A per-path datatype-head guard keeps nested and future recursive
field projection finite.

### Plan order and instantiation-evidence erasure

The full historical structural no-axiom prefix runs in its established order
before the focused nominal work. Each nominal formula is tried both plainly
and, when available, with its separately compiled bounded instantiation
axioms. These plans consume the same global candidate cutoff and choice-point
fuel as every structural plan. They are proof-producing, positive-only
approximations: failure of a nominal plan never establishes uninhabitability.

Inferable instantiation evidence is erased only after independent proof
checking. When a selected leading binder is vacuous, conversion instead
retains the shortest visible prefix, using `@_` for earlier inferable positions
and a specified closed argument—including a quantified one—for the selected
choice. Every proof that consumes instantiation evidence uses conservative
no-eta conversion. It retains the lambda when erasure would expose a
higher-rank application boundary under GHC's simplified subsumption rules.
Consequently, the second
example remains `\x -> finish x`, rather than being eta-contracted to
`finish`. The same protection applies when presentation turns structural
record elimination into a selector projection. Such signatures and generated
terms may require both `RankNTypes` and `ImpredicativeTypes`.

## What stays opaque, and what this is not

Every quantified subtree outside those explicit boundaries remains an opaque
atom. Alpha-renamed binders compare by lexical scope and declaration position,
while free variables remain significant. Ordinary structure outside the atom
is retained, including impredicative applications such as lists of Church
booleans:

```text
:compare forall item. (forall result. (item -> result -> result) -> result -> result) -> (forall answer. (item -> answer -> answer) -> answer -> answer)
:compare [(forall result. result -> result -> result)] -> [(forall answer. answer -> answer -> answer)]
```

This is not general higher-rank subsumption, polymorphic-let generalization, or
general visible type application. Explicit open arguments such as `@a` remain
unsupported. A closed quantified argument is admitted only through the bounded
query-supplied routes above; neither backend invents a polytype. Djinn can
retain the chosen application for a vacuous query-local or loaded scheme, while
its historical `HExpr` compatibility projection still rejects the shared node
explicitly. Unsupported Djinn positions remain opaque and make
an otherwise empty search inconclusive rather than manufacturing a logical
refutation. Exference still does not perform non-exact subsumption between
contextual schemes; quantified types outside an exposed goal/provider boundary
remain opaque. Finite identifier or search-budget exhaustion is truncation, not
negative evidence.
## Occurrence-plan families and their bounds

Djinn searches a quartic historical family followed by a capped quintic tail.
Its prefix remains the fully opened polarized plan, the exact-opaque plan, and
the two singleton frontiers: one positive `forall` stays opaque while its
siblings open, or one occurrence opens while unrelated siblings remain opaque.
A deterministic tail then makes the same choices for each unordered pair,
triple, quadruple, and bounded quintuple of sites. Quintuple-opaque plans begin
at ten sites and quintuple-open plans begin at eleven. Each orientation retains
at most 512 selections, alternated stably from both source-order edges, so the
new layer contributes at most 1,024 formula views on larger inputs.

Opening nested occurrences also opens the union of their enclosing chains.
Loaded functions expose those sound views together, so a reusable premise can
be consumed at different views in one proof. All 252 five-site selections at
ten sites and all 462 at eleven fit below the cap, making the family exhaustive
for eleven independent sites without a general power-set search. Twelve sites
expose the next central boundary: a proof requiring exactly six open and six
opaque occurrences may remain inconclusive.

After that complete structural
no-axiom prefix, bounded instantiation plans cover many omitted middle subsets,
but chains beyond six binders, constrained chains, and candidates outside the
finite query/value-signature vocabulary stay out of reach. Each structural or
nominal instantiation family is capped per scheme and per family. Four-,
five-, and six-binder historical query-local tuple selection fairly mixes source-order,
repeated, sparse, and Cartesian shapes while one- through three-binder schemes
retain their historical order. The appended loaded-value family uses the same
tuple shapes while alternating both ends of its source-ordered candidate list.

The established query-closed structural and optional nominal plans keep their
position after those loaded and provider plans. They schedule the combined
historical and closed-query pool and retain only tuples containing a closed
query candidate. Pure query-correlated structural and optional nominal plans
follow, carrying historical, loaded, and provider premises but leaving the
query-closed family unchanged. A final combined superset is present only when
both families contribute axioms, allowing one proof to compose their instances
without duplicating either single-family plan. Every added plan contributes
candidates but no negative evidence. Those caps lose completeness only, never
soundness.

The nominal parametric-datatype plans obey the same caps and add no negative
evidence. An incomplete primary premise also makes negative evidence
conservative for the whole query. The examples use the same
Church Boolean and Church List shapes as the
[church-encoding reference](https://github.com/VladimirReshetnikov/Haskell/blob/main/church-encoding/src/Church.hs);
a copy of that module, its test suite, and an annotated guide ship in
[docs/examples](examples/README.md).
