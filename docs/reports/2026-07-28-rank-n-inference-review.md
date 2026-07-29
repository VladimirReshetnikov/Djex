# Rank-N inference and convergence review — 2026-07-28

## Scope

This review followed quantified types from the shared `Type`/`TypeAtom`
representation through both checked adapters, both search engines, independent
candidate checking, the unified REPL, and user-facing documentation. It also
looked for places where Djinn and Exference had acquired parallel policy code
that could drift.

The resulting implementation deliberately adds two small, complementary
backend rule families rather than a general higher-rank solver:

| Engine | New rule | Still opaque |
| --- | --- | --- |
| Djinn | Introduce a context-free `forall` in positive formula position. Two linear frontiers retain one occurrence opaquely among open siblings or open one occurrence among opaque siblings. Arrow domains reverse polarity; tuples, sums, and expanded datatype structure preserve it. | Negative or constrained foralls, quantified children of otherwise opaque applications, and balanced subsets such as two open/two opaque among four independent sites. |
| Exference | Freshly instantiate a scoped provider's complete leading forall chain at a monomorphic use, or shallowly specialize context-free quantified schemes with no free flexible variables. Direct provider contexts become proof obligations. | Contextful nonidentical schemes, free flexible variables, deep or impredicative subsumption, and foralls not exposed at a scoped provider-use boundary. |

Both rule families retain the shared `TypeAtom` as the default boundary. Ordinary
unification does not decompose an atom.

## Correctness findings fixed

1. **Djinn could manufacture negative evidence from an approximation.** Every
   nested forall used to become one proposition. Exhausting that smaller search
   space was then reported as `ProvedUninhabitable`, including for inhabited
   types such as `c -> (forall a. a -> a)` and
   `((forall a. a -> a) -> c) -> c`. Polarized translation now records whether
   any occurrence remained opaque. Candidates from such a plan are sound, but
   an empty search is `NoEvidence`, not a refutation.

2. **Independent forall occurrences could not share source spellings safely.**
   Opened Djinn binders now receive occurrence-scoped internal skolems. Goal and
   prepared-premise translations use disjoint namespaces, and definition
   expansion retains the originating occurrence path.

3. **Djinn's structural and opaque views compose in a bounded way.** Positive
   opening finds structural inhabitants; exact opacity preserves polymorphic
   transport and loaded axioms. Search now retains the historical fully opened
   and exact plans, then adds two occurrence-local frontiers: one plan per
   positive forall kept opaque while its siblings open, and one per occurrence
   opened while unrelated siblings remain opaque. A nested selected site brings
   along its enclosing forall chain. The number of plans is at most `2n + 2`,
   making three independent sites exhaustive without constructing a power set.
   Reusable loaded premises expose all corresponding sound views under separate
   proof identities, while candidates share resource limits and are
   de-duplicated in the common output representation.

4. **Exference could not use a local polymorphic provider monomorphically.**
   `byProvided` compared only the complete opaque scheme. A shared helper now
   opens leading provider binders freshly per occurrence, carries their
   contexts into constraint solving, and annotates the generated occurrence
   with the instantiated type. Datatype fields reach the same rule after
   pattern elimination.

5. **Search and checking initially disagreed about forwarding.** Unifiability
   is too broad a test: a fresh monotype metavariable can unify with an entire
   forall atom. Provider use is now classified once by semantic root shape and
   consumed by both search and the independent expression checker. Vacuous
   empty-binder, empty-context forall wrappers are peeled by that classifier;
   binderless constrained wrappers remain significant.

6. **REPL setting syntax and diagnostics had no single owner.** `+NAME` and
   `-NAME` were accepted for value settings even though help and completion
   advertised them only for booleans. Boolean eligibility now lives on
   `ReplSetting`; signed value settings fail transactionally. Late validation
   for `:backend`, `:help`, `:show`, `:info`, and `:history` now retains the
   command's diagnostic family instead of being mislabeled as a setting error.

7. **Documentation described behavior the unifier no longer had.** Exference's
   README said quantified types were rejected even though the unified solver
   already compared them as opaque atoms. The package, architecture, library,
   REPL, foundation, and backend guides now distinguish representation support,
   the bounded backend rule families, and unsupported general subsumption.

8. **Empty-type elimination suppressed ordinary provider use.** The bundled
   catalogue contains constructorless declarations, and Exference treated an
   empty deconstructor as a mandatory branch. Introducing such an argument
   discarded the sibling search in which the value was passed to a provider,
   including a freshly instantiated rank-N provider. Empty elimination and
   ordinary scoped use are now alternative proof branches.

9. **The bundled catalogue confused hidden constructors with no constructors.**
   Most constructorless declarations are abstract signature stubs such as
   `Int`, `Char`, `Map`, and `TypeRep`; only `Void` and `V1` in the shipped set
   are genuinely empty. Lowering every stub as an empty datatype admitted
   bogus inhabitants such as `\x -> case x of {}` for `Int -> Bool`. A
   path-local visibility manifest now classifies every constructorless shipped
   declaration as `abstract` or `empty`, records its arity and ground parameter
   kinds, and is validated completely at load time. Explicit kinds preserve
   unconstrained higher-kinded stubs such as `Alt`, `Rec1`, and `M1`. Abstract
   entries enter the neutral Inventory but never receive a backend eliminator.
   User source without a manifest retains normal `EmptyDataDecls` semantics.

10. **The unified REPL dropped visibility manifests after discovery.** The
    directory loader classified bundled stubs correctly, but the module-aware
    REPL rebuilt its Exference session from module and rating snapshots alone.
    After startup or `:reload`, an abstract `Int` could therefore regain a
    bogus empty eliminator. Workspace targets now capture visibility sidecars
    transactionally with their modules and ratings, and the explicit
    `WithTypeVisibility` snapshot/session APIs apply that immutable manifest.
    Ordinary file and source APIs remain manifest-blind by design; loading one
    source file or named module does not grant an adjacent sidecar authority.

11. **Djinn's REPL projection erased genuine empty types.** Once the source
    boundary distinguishes an abstract stub from an empty declaration, a
    second blanket zero-constructor degradation is both redundant and wrong.
    Genuine user empties and manifested `Void` now retain structural
    empty-case elimination in Djinn, while abstract `Int` remains nominal and
    non-eliminable.

12. **A bounded Djinn miss was rendered as an internal error.** Completing all
    configured search while an unsupported quantified subtree stays opaque is
    an honest `NoEvidence` result, not a prover invariant failure. The
    standalone frontend now reports that inhabitation is undecided in the
    supported fragment, and its verbose help limits completeness claims to the
    rank-1 fragment.

13. **REPL type annotations fabricated a synthesis request just to parse a
    type.** `:type` now invokes the neutral checked source-type parser directly.
    Its scope projection is owned once beside `ReplScope` and shared with
    synthesis query parsing, removing dummy targets and backend options from a
    backend-neutral operation.

14. **Exact quantified forwarding was unnecessarily the only quantified-goal
    provider rule.** A shared Exference classifier now recognizes shallow
    predicative subsumption between two context-free schemes with no free
    flexible variables. Requested binders are rigid, only provider binders may
    be solved, and any solution containing a nested forall is rejected.
    Search records the requested scheme without leaking matcher substitutions;
    the independent checker proves the relation again. Exact alpha-aware
    forwarding, including constrained schemes, remains first priority.

## Architecture decisions

- `Language.Haskell.Synthesis.TypeAtom` remains the only alpha-equivalence and
  capture-avoiding transport authority for opaque quantified subtrees.
- Djinn owns polarity because it is a property of formula translation. Sealed
  environments cache the primary, exact, single-opaque-occurrence, and
  single-open-occurrence premise views once; query search never reparses or
  rebuilds the environment.
- Exference owns provider opening in one private `Internal.Polytype` module.
  Search supplies its finite test allocator, while checking supplies the
  production allocator. This keeps freshening, context order, lexical
  shadowing, use classification, and shallow quantified subsumption identical
  without coupling the two state machines.
- The Exference unifier remains first-order and opaque-forall-aware. Provider
  instantiation and quantified-provider specialization are explicit typing
  rules at the provider boundary, not special ordinary-unifier cases.
- The module-aware workspace owns immutable module, rating, and visibility
  snapshots. Loader APIs distinguish manifest-blind user-source semantics from
  an explicitly authorized visibility snapshot instead of inferring authority
  from a filename.
- `ReplScope` owns the one projection into the neutral source type parser, so
  synthesis queries and `:type` annotations cannot drift in qualification or
  import semantics.
- Candidate checking remains independent. Djinn proofs are checked against the
  exact formula plan that produced them, while Exference expressions are
  reconstructed against the original goal, lexical environment, and expected
  constraints. No new rule is trusted merely because search emitted a term.

## Deliberate limits

This slice does not implement general higher-rank subsumption, polymorphic-let
generalization, visible type application, or arbitrary impredicative
instantiation into a quantified goal. Djinn does not eliminate an opaque
negative forall; Exference does not synthesize a new polymorphic value for a
provider argument. Those cases remain explicit opaque boundaries or
inconclusive searches rather than being flattened unsafely.

Djinn's occurrence-local extension includes both singleton frontiers: one
opaque occurrence among opened siblings and one open occurrence among opaque
siblings. It is therefore exhaustive for three independent sites. With four
independent sites, however, a proof needing exactly two open and two opaque
occurrences remains inconclusive. Enumerating the full power set would make
negative searches exponential, so this is an explicit completeness boundary.
Likewise, one incomplete primary cached premise conservatively disables
negative evidence for the whole query, even if a relevance analysis could
later prove that premise unnecessary. Both restrictions lose completeness,
never soundness.

Large mechanical module splits were not mixed into the inference change. The
review extracted only policy with a forced shared invariant: Exference provider
classification, the REPL scope projection, and immutable workspace auxiliary
snapshots. The research rules therefore remain locally reviewable.

## Regression coverage

The focused suites cover positive Djinn results and tuples, double polarity,
exact opaque transport, both occurrence frontiers through direct, nested,
synonym, datatype, duplicated, and loaded-premise paths, the four-site bounded
gap, global cutoff/fuel, incomplete-search evidence, and skolem isolation.
Exference coverage includes direct and repeated provider use, contextual
providers, instance discharge, rank-N fields, exact forwarding, vacuous
wrappers, lexical shadowing, shallow quantified specialization, wrong-direction
and correlation failures, contexts, free variables, impredicativity, rigid-ID
collisions, invalid annotations, and finite identifier exhaustion. The loader
suites validate complete visibility manifests and preserve true user empty
datatypes; CLI regressions reject false empty cases for bundled abstract types
before and after reload. The REPL suite covers GHCi-style setting signs,
transactional state, diagnostic ownership, shared parsing, genuine Djinn empty
elimination, and loaded Djinn rank-N axioms.
