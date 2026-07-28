# Rank-N inference and convergence review — 2026-07-28

## Scope

This review followed quantified types from the shared `Type`/`TypeAtom`
representation through both checked adapters, both search engines, independent
candidate checking, the unified REPL, and user-facing documentation. It also
looked for places where Djinn and Exference had acquired parallel policy code
that could drift.

The resulting implementation deliberately adds two small, complementary typing
rules rather than a general higher-rank solver:

| Engine | New rule | Still opaque |
| --- | --- | --- |
| Djinn | Introduce a context-free `forall` in positive formula position. Arrow domains reverse polarity; tuples, sums, and expanded datatype structure preserve it. | Negative or constrained foralls, and quantified children of otherwise opaque applications. |
| Exference | Freshly instantiate a scoped provider's complete leading forall chain at a non-quantified use site. Direct contexts become proof obligations. | Quantified goals other than exact alpha-aware forwarding, and foralls not exposed at a provider-use boundary. |

Both rules retain the shared `TypeAtom` as the default boundary. Ordinary
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

3. **Djinn's polarized and opaque plans are complementary.** Positive opening
   finds structural inhabitants; the opaque plan preserves exact polymorphic
   transport and loaded axioms. Alternative enumeration now retains candidates
   from both completed plans while sharing resource limits and de-duplicating
   the common output representation.

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
   the two bounded inference rules, and unsupported general subsumption.

8. **Empty-type elimination suppressed ordinary provider use.** The bundled
   environment intentionally exposes abstract primitives such as `Int` without
   constructors. Exference treated every such binding as a mandatory empty-case
   elimination, so introducing an `Int` argument discarded the sibling search
   in which that value was passed to a provider—including a freshly instantiated
   rank-N provider. Empty elimination and ordinary scoped use are now alternative
   proof branches.

## Architecture decisions

- `Language.Haskell.Synthesis.TypeAtom` remains the only alpha-equivalence and
  capture-avoiding transport authority for opaque quantified subtrees.
- Djinn owns polarity because it is a property of formula translation. Sealed
  environments cache both opaque and polarized premise plans; query search
  never reparses or rebuilds the environment.
- Exference owns provider opening in one private `Internal.Polytype` module.
  Search supplies its finite test allocator, while checking supplies the
  production allocator. This keeps freshening, context order, lexical
  shadowing, and use classification identical without coupling the two state
  machines.
- The Exference unifier remains first-order and opaque-forall-aware. Provider
  instantiation is an explicit typing rule immediately before unification, not
  a special unifier case.
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

Djinn's polarized and opaque translations are whole-query plans. Candidate
sets from the two plans are merged when alternatives are requested, but one
proof cannot combine positive opening at one occurrence with exact opaque
forwarding at another. For example,
`(forall a. a) -> ((forall b. b), (forall c. c -> c))` remains inconclusive.
Likewise, one incomplete cached premise conservatively disables negative
evidence for the whole query, even if a relevance analysis could later prove
that premise unnecessary. Both restrictions lose completeness, never
soundness.

Large mechanical module splits were not mixed into the inference change. The
review extracted only policy with a forced shared invariant—the Exference
provider helper and the REPL boolean-setting predicate—so the research rules
remain locally reviewable.

## Regression coverage

The focused suites cover positive Djinn results and tuples, double polarity,
exact opaque transport, incomplete-search evidence, skolem isolation, direct
and repeated Exference provider use, contextual providers, instance discharge,
rank-N fields, exact forwarding, vacuous wrappers, lexical shadowing, invalid
annotations, quantified-goal exclusion, and finite identifier exhaustion. The
CLI suite covers GHCi-style setting signs, transactional state, diagnostic
ownership, shared parsing, loaded Djinn rank-N axioms, and monomorphic and
polymorphic provider use over abstract constructorless types.
