# Contextual evidence: implemented lexical foundation and remaining integration

Status: **shared lexical evidence, independent source checking, and Haskell
rendering are implemented and tested; complete contextual synthesis remains
open**. This report records the first increment of priority 3 in the
[priorities 1–4 register](2026-09-07-synthesis-priorities-1-4.md). Method search,
conditional-provider and superclass derivations, complete synthesis acceptance,
and production Lean integration remain completion requirements.

## Current boundaries

- Djinn's source checker now reconstructs exact lexical context introductions
  and applications. Its search still opens positive contextual goals without
  making class methods available; `Environment` rejects loaded constrained
  premises. Passing a supplied-clause source check does not establish that the
  search engine can find that clause.
- The shared typed graph now has explicit context introduction/application
  nodes and an opt-in context sealer. `sharedContextualForallTypeStructure`
  retains substituted constraints after type selection. The ordinary forall
  structure and ordinary sealer retain their previous contextual boundaries.
- Exference's `ScopedConstraint` pairs each obligation with its lexical givens.
  Its independent expression checker now reconstructs direct lexical Given
  applications from the complete original telescope and final substitutions.
  Instance and superclass resolution still lacks a retained derivation.
  Existing visible-application certificates
  retain provider/type-selection identity and activated obligations; those are
  useful authority, but are not explicit dictionary proofs.
- Leant still serializes ordinary query instance binders as legacy `FInst` markers.
  Djinn's provider projection erases `FExactContext`; Exference retains supported
  exact contexts. `renderLeanTermGraphProjection` immediately erases the graph
  into the compatibility renderer. A separate direct context renderer is under
  integration; source preparation and query routing must preserve its metadata
  before this becomes a production capability.

## Shared representation

The implemented API keeps witness constructors private. A Given reference is
the actual introduction occurrence plus its ordered source slot. Constructing a
reference grants no authority: sealing must find that exact binder in scope.

```haskell
givenContextEvidence :: OccurrenceId -> Natural -> ContextEvidence
contextIntroductionWitness
  :: ContextTypeStructure ty -> ty -> Maybe (ContextIntroductionWitness ty)
contextApplicationWitness
  :: ContextTypeStructure ty -> ty -> [ContextEvidence]
  -> Maybe (ContextApplicationWitness ty)

-- Additional TermNodeForm constructors:
TypedContextIntroduction
  OccurrenceId TermNodeId (ContextIntroductionWitness ty)
TypedContextApplication
  OccurrenceId TermNodeId (ContextApplicationWitness ty)
```

An introduction consumes one exact **binderless qualified layer**, introducing
its ordered constraints around the child. An application consumes that same
layer with an equally ordered proof vector. Type binders remain separate:
forall introduction/application may preserve substituted contexts, but must
never discharge them. A context outside a later forall remains outside it.

The sealer validates the source/body or source/result relation from the
actual qualified type; fresh evidence IDs; exact class identities and arguments;
lexical given scope; and existing forall scope/escape conditions. Evidence IDs
use a namespace distinct from ordinary term locals. Every evidence tree,
argument vector, and annotation needs existing-style depth, node, width, and
type bounds. Instance and superclass references are not part of the current
evidence API; adding them requires sealed source authority.

Compatibility erasure removes the two new term forms while retaining their
checked evidence in the graph. Traversal, normalization, kind checking, metrics,
and consumers handle the new forms explicitly. Fingerprinting and Length reject
them rather than assigning an old fingerprint or semantic authority.

Graphs mixing new explicit lexical evidence and visible-application certificate
origins remain unsupported in Exference. Existing certificate graphs that have
no explicit context nodes retain their previous independent source/result/
activated-obligation checks and availability. That legacy receipt does not claim
to contain a dictionary derivation. An additive association entrance and a
separate contextual receipt view are needed before combining the two contracts:
the old certificate result is post-discharge, while a type-application node must
retain its qualified intermediate type.

## Verified lexical increment

The GHC 9.12.4 strict build passes with `-Werror`. The shared suite passes all
502 tests, including 24 lexical-context controls and 12 contextual Haskell
renderer tests. Both complete private checker suites pass: 55 Djinn tests and
70 Exference tests, including twelve new lexical fixtures in each. These are
independent source-checker fixtures, not end-to-end synthesis receipts.

Four facade replay tests also pass. Three independently compile and execute the
actual rendered expression under its full original Haskell signature: a loaded
constrained provider, a constrained local callback, and nested rank-N contexts
with heterogeneous applications. The fourth rejects repeated or shadowed equal
Given choices before invoking GHC. Haskell implicit dictionary passing cannot
express a selection between multiple equal active predicates; its renderer
therefore rejects those graphs. The graph representation itself preserves the
distinct identities, which Lean can express with explicit dictionary arguments.

Logs are `dist-newstyle/priority-context-private-build-v6.log`,
`priority-context-exference-engine-v6.log`, `priority-context-djinn-private-v6.log`,
`priority-context-shared-renderer-tests-v4.log`, and
`priority-context-ghc-replay-v5.log`. These receipts do not establish methods,
instances, superclass projections, end-to-end query reachability, or Lean replay.

## Live Haskell Given acceptance

The native facade fixture additionally passes eleven acceptance tests: all five
Exference roles below, the unused-root/nested-callback/exact-forwarding Djinn
roles, and three scoped leakage controls. The source inventory contains only
an empty class, an abstract result type, and the exact constrained provider
signature when needed. No method, instance, constructor, observer, or target
implementation is available to search. Every observed candidate must retain its
graph and exact clause association before independent full-signature GHC replay.

Exference uses a 32-candidate cap, 20,000 steps, queue 256, and immediate checking
of currently resolvable constraints (`exferenceConstraintDeferralSteps = 0`).
Flexible constraints still retain their existing deferred treatment. Djinn uses
32 raw candidates and 20,000 choices. Each case has a 90-second outer limit.
The registered `ContextEvidenceSpec.tests` covers these supported roles;
`targetTests` retains the complete fourteen-test target, including Djinn's
remaining constrained global/local search paths.

Logs are `priority-context-live-native-build-v1.log`,
`priority-context-exference-global-v4.log`,
`priority-context-exference-live-v4.log`, and
`priority-context-djinn-live-v4.log` under `dist-newstyle/`. An earlier interpreted
driver using the default 8,192-step deferral timed out on forwarding and local/
global applications. Both the driver compilation and fixture policy differ,
so this is not an isolated measurement of deferral's performance. The production
default is unchanged. Methods, instance/superclass evidence, the three remaining
Djinn target cases, and live Lean synthesis remain unaccepted.

## Source authority and derivations

| Evidence | Authoritative source | Required retained identity |
| --- | --- | --- |
| Given | Original checked root or nested qualified telescope | Fresh binder ID, exact constraint, and lexical introduction site |
| Instance | Explicit declarations in the sealed inventory | Inventory-owned declaration ID, one correlated type vector, and proofs of all prerequisites |
| Superclass | Exact class declaration and direct superclass slot | Inventory-owned class ID, slot, correlated class arguments, and the parent dictionary proof |
| Method | Exact method declaration and full qualified scheme | Existing authenticated global provider identity, followed by type/context applications |

Djinn's checked source goal retains the telescope. Exference's `instantiateGoal`
produces ordered opened constraints. Its checker now retains the original source
opening telescope and ordered evidence identities before using `QueryClassEnv`.
Nested `withLocalGivens` and deferred substitutions preserve lexical scope.

Exference's `sClassEnv_explicitInstances` preserves the supplied instance order
without superclass inflation. Mint source instance IDs there or at the common
inventory boundary. Its resolver's inflated instance map is not a new source
declaration inventory. Likewise, `HsTypeClass.tclass_constraints` supplies direct
superclass slots, whereas `qClassEnv_inflatedConstraints` records only closure
membership and cannot supply a projection path.

The next increment must extend constraint solving to return a derivation alongside each successful
discharge. Keep `ScopedConstraint`-style provenance on pending obligations and
their prerequisites. Never insert a query-local given into a global instance
table or provider fact group.

Do not deduplicate dictionary identity merely because two constraints have
equal types. In Lean, two local instances of the same class type can contain
different values. An evidence choice must survive rendering rather than be
reselected by instance search.

## Implementation increments

1. **Lexical givens.** Support only `GivenEvidence` in the new proof form. Add
   checked contextual introduction, exact contextual forwarding, and local or
   loaded constrained applications discharged by an exact in-scope given.
   Dictionary-independent bodies retain their unused evidence binders. They do
   not become context-free source graphs.
2. **Methods and conditional providers.** Admit exact method schemes from the
   source class inventory, retain provider identity through applications, and
   add instance derivations with all correlated prerequisites checked.
3. **Superclass evidence.** Retain and replay each direct projection step from
   its exact declaration, including chains. Keep cycles and unresolved work
   bounded and distinguish missing evidence from search exhaustion.

For Djinn search, represent an exact constraint as an internal evidence atom
containing the class identity and structured arguments. Lower qualified layers
to arrows over those atoms, following their lexical type boundaries. Internal
dictionary binders/applications need explicit lowering roles; erase them only
through the checked proof/source association. The independent source checker
then reconstructs its own evidence from the original declarations and scopes.
Do not pool lexical evidence as unconditional LJT axioms.

## Lean projection

Extend ordinary query serialization to retain supported exact class applications
instead of only `FInst`. Unsupported and dictionary-dependent shapes must remain
unsupported; a pretty-printed marker cannot acquire evidence authority. Reuse
`FExactContext`'s kinded argument metadata and the exact class inventory retained
by `exactContextConstraint`. A generic type-name mapping cannot supply class
authority. Preserve full source binder visibility, kind, and universe domains;
missing metadata must not become an inferred Lean placeholder.

Add a typed contextual walk at `renderLeanTermGraphProjection`, preserving
existing rendering for graphs without these forms. Introductions bind stable
dictionary names; applications pass the selected dictionary explicitly, for
example `fun {α} [d : C α] => @provider α d`. Reconstruct the exact provider
telescope when placing type and dictionary arguments. A wildcard binder followed
by a fresh instance-search attempt does not preserve the graph's evidence choice.

## Acceptance

The first increment must synthesize, retain checked graphs, compile at the full
Haskell signatures, execute observations, and independently replay exact Lean
counterparts for at least:

- `forall a. C a => a -> a`, with an unused but retained given.
- `(forall a. C a => a -> a) -> (forall b. C b => b -> b)`, preserving exact
  contextual forwarding across renamed binders.
- `forall a. C a => (forall b. C b => b -> b) -> a -> a`, applying the local
  provider with the current given.
- A loaded constrained provider under a matching goal context, with exact
  provider identity and type/evidence selections.
- Independently scoped nested contexts in tuple components, and correlated
  higher-kinded/multiple class arguments.

Adversarial graph/source controls must change a given ID, use a sibling binder,
alter a class or argument, escape a forall, duplicate an evidence binder, change
a provider identity, or claim an instance/superclass step without authority.
Include a dictionary-dependent `Token` result outside the branch introducing its
required given, so a sibling leak has an observable rejection condition.

Later increments add actual method behavior, conditional providers with multiple
prerequisites, superclass chains, competing local dictionary values, and failed
instance branches. Preserve exact GHC and Lean rejection controls, axiom
inventories, deadlines, and candidate/source association. Passing the Given-only
increment does not complete priority 3.

## Source entry points

- Shared: [`TypedGenerated.hs`](../../synthesis/src/Language/Haskell/Synthesis/TypedGenerated.hs)
  (`TermNodeForm`, `TypeStructure`, `sharedForallTypeStructure`, sealing and
  erasure), [`TypeAtom.hs`](../../synthesis/src/Language/Haskell/Synthesis/TypeAtom.hs)
  (`isLeadingForallInstantiation`), and the `TypedGenerated` fingerprint,
  Haskell-rendering, and internal certificate-association modules.
- Djinn: [`TypeFormula.hs`](../../djinn/src-core/Djinn/Internal/TypeFormula.hs)
  (`lowerForall`), [`Environment.hs`](../../djinn/src-core/Djinn/Internal/Environment.hs)
  (constrained premises), [`Core.hs`](../../djinn/src-core/Djinn/Core.hs)
  (`inhabitSynthesisPreparedSearchChecked`, source/search correspondence),
  [`SourceGraph.hs`](../../djinn/src-internal/Djinn/Internal/SourceGraph.hs),
  [`SourceGraphKinds.hs`](../../djinn/src-internal/Djinn/Internal/SourceGraphKinds.hs),
  and [`SourceTypingContext.hs`](../../djinn/src-internal/Djinn/Internal/SourceTypingContext.hs).
- Exference: [`ExpressionCheck.hs`](../../exference/src-core/Language/Haskell/Exference/Core/Internal/ExpressionCheck.hs)
  (`prepareExpressionCheckContextUnchecked`, `introduceExpectedForallChain`,
  `instantiateScopedProvider`, `instantiateBinding`, `withLocalGivens`),
  [`ScopedConstraint.hs`](../../exference/src-core/Language/Haskell/Exference/Core/Internal/ScopedConstraint.hs),
  [`ConstraintSolver.hs`](../../exference/src-core/Language/Haskell/Exference/Core/ConstraintSolver.hs),
  and [`Types.hs`](../../exference/src-core/Language/Haskell/Exference/Core/Types.hs).
- Leant companion checkout: `src/Leant/Synth/Fragment.hs` (`FInst`,
  `FExactContext`, `exactContextFragment?`); `src/Leant/Synth/Engine.hs`
  (`ProviderContextProjection`, `projectedProviderFrag`,
  `exactContextConstraint`, semantic origins); `src/Leant/Synth/Render.hs`
  (`renderLeanTermGraphProjection`, contextual binder/application fitting).
