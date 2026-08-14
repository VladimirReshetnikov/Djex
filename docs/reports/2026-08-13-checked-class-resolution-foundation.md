# Checked class-resolution foundation

Date: 2026-08-13

## Outcome

Djex now has a package-private, bounded class-resolution authority in
`Language.Haskell.Synthesis.Internal.ClassResolution`. It seals one exact
already-checked `Inventory` into an opaque environment, resolves an admitted
ground constraint, and returns a successful discharge only as an opaque receipt
which retains that environment, the canonical goal, and its proof.

This is a standalone foundation. No Djinn, Exference, certificate,
fingerprint, Length, SMT-LIB, or Z3 entrance consumes it yet. It changes no
backend acceptance rule or identity schema. Z3 reports remain behavioral
heuristics and can never provide dictionary evidence.

## Admitted constraint language

The sealer deliberately consumes the raw, unexpanded declaration view of the
checked `Inventory`, not a `PreparedInventoryExpansion`. A generic inventory
variable does not give this module a package-owned fresh-name allocator for
capture-safe synonym expansion, so the boundary fails closed instead:

- only classes declared in the exact inventory are resolution heads;
- class names and exact declared arities are validated before their arguments
  are retained;
- types are structurally bounded, normalized, and kind-checked under the
  inventory-derived assumptions;
- every referenced type synonym is rejected, even though unrelated synonym
  declarations may remain in the inventory;
- every `ForallType` nested in a constraint argument is rejected;
- instance substitution is first-order, although binders may occur in
  higher-kinded applicative positions; and
- caller queries must be ground and have no given-constraint context.

Explicit instance heads and prerequisites may contain only their declared
first-order variables under the already-checked inventory scope, and only the
declared binders are eligible for substitution. A binder needed solely by a
prerequisite cannot be recovered by matching the head, so that rule produces no
evidence for the query. A malformed, free, ill-kinded, aliased, or quantified
query is a checked query error rather than ordinary absence of evidence.

The retained kind assumptions keep the bounded type-constructor table, including
external constructors inferred by an open inventory when its declarations
reference them. Class-parameter assumptions are projected to the inventory's
declared classes. An open-inventory assumption for an external class can
therefore never create resolver authority.

## Sealed environment policy

Classes and instances preserve inventory source order. For each explicit
instance, sealing substitutes its head arguments through the direct
superclasses of the declared head class, canonicalizes and revalidates those
derived constraints, and stably removes duplicate prerequisites. It does not
materialize a separate caller-supplied superclass claim. Superclass cycles are
rejected before resolution.

Duplicate instance heads are rejected. Every later same-class pair is also
checked by a symmetric overlap unifier, so this resolver has no priority or
overlap-selection policy. The overlap unifier and directional runtime matcher
both compare the same canonical applicative representation of constructors,
applications, functions, and tuples. Thus, for example, a higher-kinded head
which can instantiate to a function cannot evade overlap detection merely
because one source used structural arrow syntax and the other used constructor
application syntax.

The Paterson-style termination gate uses that same applicative kernel. For each
explicit or direct-superclass prerequisite whose variables can all be supplied
by matching the head, its node measure may not exceed the head measure and no
head binder may occur more frequently. A growing rule is rejected while
shrinking and size-preserving rules remain admissible. Resolution separately
tracks canonical constraints on its current path, so an exact cycle produces
ordinary absence instead of recursion. Prerequisites are instantiated and
resolved in their retained order.

## Admission, retention, and proof bounds

`ClassResolutionLimits` validates ten independent nonnegative budgets:

| Budget | Default | Boundary |
| --- | ---: | --- |
| declarations | 32,768 | recovered count accepted for resolver sealing |
| classes | 4,096 | retained declared-class list and index |
| instances | 16,384 | retained explicit/completed instance list and index |
| type-constructor kinds | 32,768 | retained constructor-kind assumption table |
| collection width | 256 | class parameters/superclasses, instance binders/prerequisites, completed prerequisites, and type-owned collections |
| type nodes | 4,096 | each retained or queried constraint argument |
| kind nodes | 256 | each retained constructor or declared-parameter kind |
| overlap comparisons | 262,144 | same-class instance-head pair checks |
| proof depth | 256 | one resolution branch |
| proof nodes | 4,096 | nodes explored by one query's proof search |

Completed superclass prerequisites are rechecked against the collection and
type budgets after substitution. Runtime-instantiated ground prerequisites are
also rechecked, because two individually bounded substitutions can combine into
a larger derived type. These limits bound accepted structural shapes and proof
exploration; they do not bound identifier/variable bytes or every unit of work
performed while canonicalizing a substituted type before its derived bound is
observed. Limit failures report capped finite observations, and proof counters
are checked before incrementing so `Int` overflow cannot become authority.

The declaration limit bounds the recovered count accepted by resolver-specific
sealing and, together with the table and kind limits, bounds the semantic state
recoverable into its opaque environment. It is not a new streaming work bound:
`sealClassResolutionEnvironment` receives an `Inventory` whose environment and
kinds have already been checked, recovers that source order in full, and only
then observes the resolver declaration cap.

## Receipt and replay authority

Successful resolution creates a `CheckedConstraintDischarge` which retains:

- the exact retained checked environment, including limits, relevant kind
  assumptions, known aliases, source-ordered classes, and completed instances;
- the canonical ground goal; and
- the source-ordered instance proof tree.

The receipt and proof constructors are hidden and have nominal roles. A proof
tree can be observed only after
`replayCheckedConstraintDischarge` compares the supplied environment with the
retained environment by structural equality, including limits, assumptions,
aliases, source variables, ordinals, classes, and completed instances. That
comparison deliberately happens before the replay goal is inspected. The raw
goal must then pass the same structural, alias, groundness, and kind checks and
canonicalize to the retained goal. A projected goal or detached diagnostic
proof therefore cannot transfer discharge authority to different retained
inventory facts, policy, or constraint.

This is a direct retained-value association rather than a new public
fingerprint. Derived indexes are excluded from the comparison because they are
deterministic views of the retained ordered classes and instances. No public
constructor, facade export, serialization format, or caller-provided evidence
seam was added.

## Trust boundary

This receipt proves only discharge under this exact closed-world
declared-class, alias-free, first-order, ground, no-givens policy. It does not
prove declaration provenance for a certificate row, candidate completeness,
expression inhabitation, behavioral correctness, or a solver result.
Conversely, empty certificate obligations do not imply that this resolver ran.

A later consumer must atomically bind the receipt to its own inventory,
candidate occurrence, activated obligation, and domain identity as applicable.
Length does not perform that binding in this checkpoint, so its existing
requirement for empty activated obligations remains a structural admission rule
only. Neither `sat`, `unsat`, a Z3 model, nor successful independent Length
counterexample replay can act as a Haskell dictionary.

## Validation surface

The focused package test covers source-ordered proof retention, shrinking
recursion and current-path cycles, repeated and higher-kinded binders, missing
prerequisite-only binders, grounded-but-unprovable prerequisites, and their
short-circuit order, runtime applicative canonicalization before derived bounds,
raw query validation order, synonym and
nested-forall rejection, applicative overlap, superclass cycles, growing
explicit and completed prerequisites, every environment/query/proof budget
class, post-completion and post-instantiation revalidation, exact
environment/goal replay mismatch, environment-before-goal replay demand, and
deep evaluation without retaining source annotations.

The public abstraction probe keeps the environment, proof, receipt, sealer,
discharger, and replay operation out of the curated Djex facade. This report
records the intended boundary; it does not claim backend integration or a new
public compatibility guarantee.
