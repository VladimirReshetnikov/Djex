# Exact provider-instantiation assignments — 2026-08-05

## Outcome

The stable Djinn and Exference adapters now accept complete ordered
leading-binder assignments for exact retained providers. This closes the
correlation gap in the original provider-local candidate API.

The original payload remains useful and supported:

```haskell
ProviderInstantiationCandidate
  { providerInstantiationCandidateProvider :: Name
  , providerInstantiationCandidateType :: Type variable
  }
```

It contributes one scalar type to a provider-local pool. For a provider with
several leading binders, a backend may construct bounded tuples from that pool.
That model is correct when the frontend has independently justified every
combination, but it loses which types were selected together by one external
proof.

The original correlated payload retains that fact directly:

```haskell
ProviderInstantiationAssignment
  { providerInstantiationAssignmentProvider :: Name
  , providerInstantiationAssignmentArguments :: [Type variable]
  }
```

The argument list is the complete vector in the provider's leading-forall
order. Each vector is consumed once. Neither backend flattens it into a scalar
pool or reconstructs a Cartesian product, so evidence for `[T1, T2]` does not
also authorize `[T1, T1]`, `[T2, T1]`, or a tuple assembled from a different
external proof. For this legacy payload, each position is checked at the
corresponding provider binder's inferred ground kind rather than forcing every
vector member to have kind `Type`.

Commit `22206d9c` adds a parallel kind-aware payload without changing that
compatibility contract:

```haskell
KindedProviderInstantiationAssignment
  { kindedProviderInstantiationAssignmentProvider :: Name
  , kindedProviderInstantiationAssignmentArguments ::
      [(GroundKind, Type variable)]
  }
```

Each pair preserves the source frontend's exact positional ground kind beside
the already correlated type argument. This is necessary when the retained
provider body has erased the only source kind annotation: in particular, a
vacuous binder has no occurrence from which either backend could infer a
higher kind. The kind is caller-attested evidence, not a reconstructed backend
fact.

## Stable contract

The parser-neutral engine facades and umbrella `Language.Haskell.Djex` module
export:

```haskell
runDjinnQueryWithInstantiationAssignments
runExferenceQueryWithInstantiationAssignments
runDjinnQueryWithKindedInstantiationAssignments
runExferenceQueryWithKindedInstantiationAssignments
```

The shared limits are public as well:

```haskell
maximumProviderInstantiationAssignments = 32
maximumProviderInstantiationArguments = 4
maximumProviderInstantiationKindNodes = 129
```

The 32-vector bound is global to one call, before grouping or
alpha-deduplication; it is not 32 assignments per provider. The outer spine is
observed through at most the first extra cell before an assignment value is
entered. Each argument spine is likewise observed only through the fifth cell
before an argument is entered. An over-wide, infinite, or cyclic caller-built
list therefore fails finitely at the checked boundary.

The per-kind assignment limit applies independently to every caller-supplied
`GroundKind`. A productive node observer runs before kind inference,
same-provider kind-vector equality, backend kind conversion, or forcing the
paired type argument. It counts at most 129 constructors and, if work remains,
returns the sentinel 130 without entering the pending constructor. Cyclic kinds
and finite kind trees above the bound therefore fail without traversing an
unbounded remainder. The value `129 = 2 * 64 + 1` deliberately preserves the
shared 64-tuple constructor's complete right-associated all-`Type` kind. That
per-kind capacity is independent of the four type arguments allowed in one
assignment vector.

`ProviderInstantiationCandidate`,
`maximumProviderInstantiationCandidates`, and both
`WithInstantiationCandidates` runners retain their original proper-type-only
behavior. The legacy assignment type and runners also retain their inferred
kind behavior. The three payloads deliberately have separate entrances: an
exact vector is never silently downgraded to the scalar compatibility model,
and a kinded vector is never stripped of its caller-attested kinds.

## Checked assignment boundary

Ordinary request preparation and goal/context validation retain precedence.
Only then does an assignment cross the engine-specific trust boundary. The
caller owns the source-language proof which established the vector; Djex checks
that the assertion is finite, representable, and confined to the named
provider.

For every assignment, the stable runner checks:

1. The provider `Name` must resolve exactly in the selected sealed session to a
   retained polymorphic global. A monomorphic value, scoped value, missing
   name, or alpha-identically typed sibling is not a substitute.
2. The retained provider must have a context-free complete leading forall
   chain with arity from one through four. Contextual provider schemes remain
   unsupported.
3. The argument-vector length must equal that exact arity. Empty and partial
   vectors are rejected rather than completed from another source.
4. The legacy runner infers each leading binder's ground kind from the exact
   provider body. A binder whose kind the body does not constrain is vacuous
   and defaults to `Type`; the argument is synonym-elaborated at that inferred
   kind. This historical behavior remains unchanged.
5. The kinded runner productively preflights every supplied `GroundKind`
   independently. An over-limit or cyclic kind is rejected before kind
   inference, same-provider kind-vector comparison, backend conversion, or the
   paired type argument is forced.
6. The kinded runner then checks the provider body at `Type` in one scope
   shared with every caller-supplied binder-kind obligation. Every observable
   use must agree with its supplied `GroundKind`. A vacuous position supplies
   no such constraint, so its kind remains a caller assertion; the backend
   still synonym-elaborates the paired argument at exactly that kind.
7. Repeated kinded assignments for the same provider must agree on the
   complete binder-kind vector. This consistency check happens before any
   paired type argument is entered.
8. Every argument must be closed, context-free, and have the shared
   specified-visible-argument
   representation, including a higher-kinded constructor, a closed quantified
   `Type`, or a structural proper type containing one where its positional kind
   permits that form.

Both adapters perform an additional whole-assignment proof. They
capture-avoidably substitute the complete checked vector into the retained
provider body and kind-check that specialized body at `Type` in the sealed
environment. This independent guard rejects both a proper type supplied at a
`Type -> Type` position and a higher-kinded constructor supplied at a `Type`
position. The legacy runner permits a higher-kinded vector, or a vector mixing
a higher-kinded constructor with a closed impredicative `Type`, when the
provider body determines the higher-kinded position and every vacuous position
takes its default `Type` kind. The kinded runner additionally admits a bare or
partially applied higher-kinded constructor at a vacuous position whose exact
ground kind was retained by the frontend.

After that proof, alpha-equivalent type vectors are deduplicated as complete
ordered lists, retaining the first occurrence. Distinct type vectors remain
separate choices when their complete kind vectors agree. Provider identity
remains part of the key.

Exference lowers each checked argument only after those stable shared-type
checks. Its private engine boundary independently rechecks provider closure,
context freedom, leading arity, vector length, argument closure, and visible
application shape before producing an instantiation.

## Djinn: direct correlated premises

Djinn builds one direct specialized premise per retained vector. The vector is
substituted in order and its specified visible arguments are stored beside a
private proof identity. The proof search environment contains the specialized
formula, so every generated proof is independently checked before conversion
rewrites the private identity to the exact source provider followed by those
visible applications.

The assignment path shares the positive-only provider-plan family introduced
for scalar evidence. Historical plain structural, nominal, and query-local
instantiation plans remain first. The enriched provider structural and nominal
plans carry both supplied direct premises and historical query-local and loaded
instantiation axioms, permitting a single checked proof to compose the
evidence sources. They run before the evidence-free loaded tails so a
productive loaded stream cannot spend the global candidate cutoff before the
supplied route.

Scalar pools retain the historical four-binder, 512-tuple-attempt, and
sixteen-specializations-per-scheme windows. Exact assignments bypass both the
Cartesian construction and its per-scheme window: a retained vector contributes
one premise directly. The complete provider family still retains at most 32
direct premises and spends only the query cutoff and proof-search fuel left by
earlier plans.

The requested target is filtered from safe premises. An assignment naming it
can contribute only to the enlarged diagnostic environment used to distinguish
an impossible result from one requiring self-reference; it cannot manufacture
a candidate.

## Exference: exact vector before query-derived choices

Exference stores checked vectors in a private map keyed by the exact retained
global. Scoped values and other globals never consult an entry merely because
their schemes compare equal. Normal target exclusion removes the requested
definition from provider lookup.

Ordinary implicit instantiation remains the first provider-use branch. Within
the visible branch, either exact assignment runner tries:

1. complete choices proved by ground monomorphic Haskell instance heads;
2. caller-supplied exact assignment vectors; and
3. choices derived from proper-type positions in the checked query.

Each assignment vector is tried once. Unlike the legacy scalar route, it does
not require every selected binder to be absent from the provider body: the
complete vector supplies the information ordinary unification would otherwise
infer. The provider itself must still be closed and context-free, and the
leading chain remains bounded at four.

The legacy candidate runner keeps its established visible order of ground
instance-head choices, query-derived candidates, and then supplied scalar
candidates. Its query-derived and supplied products remain separately capped
at 32. Adding the exact entrance therefore does not perturb clients which rely
on the scalar prefix.

## Empty compatibility and scope

The historical functions still execute through their exact empty-candidate
paths:

```haskell
runDjinnQuery session =
  runDjinnQueryWithInstantiationCandidates session []
runExferenceQuery session =
  runExferenceQueryWithInstantiationCandidates session []
```

Calling either legacy or kinded assignment runner with `[]` yields the same
result, candidate ordering, diagnostic precedence, completion status, and
finite-budget observations. No assignment plan or visible branch is added in
that case.

Exact assignments are assertions, not a proof interchange or a general
impredicative solver. They do not inspect the frontend proof, search local
scopes, infer a missing vector member, donate evidence between provider names,
decompose quantified bodies in ordinary unification, remove the four-argument
or search bounds, or turn a bounded miss into a proof of uninhabitability. In
particular, the kinded boundary validates a supplied kind against every
observable use but does not claim to infer or prove the source kind of a
vacuous binder.

## Regression coverage

Current validation exercises the full project matrix without changing the
historical scalar or inferred-assignment fixtures. Focused shared-API coverage
pins both assignment records, accessors, traversal/deep-evaluation behavior,
public runner names, and all four public numeric limits. The shared kind
observer tests the exact 129-node 64-arrow all-`Type` chain, the first
over-limit arrow chain, both left- and right-cyclic trees, productive non-entry
of branches beyond the bound, and non-forcing of kind-variable payloads.

Djinn regressions cover empty-runner equality; finite outer and inner cyclic
list rejection; empty, wrong-width, five-binder, open, contextual,
higher-kinded mismatch, monomorphic-provider, and unknown-provider failures;
unary quantified, higher-kinded, and mixed higher-kinded/impredicative success;
kinded vacuous higher-kinded success; productive cyclic-kind rejection before
type forcing and same-provider comparison; same-provider kind-vector
consistency; two distinct vectors retained for one vacuous provider at the
genuinely higher-order kind `(Type -> Type) -> Type`; whole-vector alpha
deduplication; cross-provider non-donation; composition with loaded
instantiation evidence; and a correlated four-binder vector deliberately
outside the legacy first-sixteen Cartesian prefix.

Exference integration regressions, together with the existing core assignment
coverage, exercise direct vector consumption,
non-vacuous provider bodies, structural arguments containing nested
quantification, ordered four-binder visible applications, higher-kinded and
mixed higher-kinded/impredicative vectors, kinded vacuous higher-kinded
success, productive cyclic-kind rejection before type forcing and
same-provider comparison, same-provider kind-vector consistency, two distinct
same-provider vectors at the genuinely higher-order kind
`(Type -> Type) -> Type`, both directions of assignment kind mismatch, legacy
scalar rejection of a higher-kinded argument, finite outer and inner bounds,
exact arity, empty compatibility, and cross-provider non-donation. Every
positive integration candidate still passes the shared generated-syntax
boundary and the engine's independent expression checker.
