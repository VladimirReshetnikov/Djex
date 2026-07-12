# Djinn shared constraint contexts and method scopes

- Date: 2026-07-11
- Djinn version: 2026.7.11
- Shared foundation: `haskell-synthesis` 0.1.1.0
- Integration target: Exference's nominal constraint boundary

## Result

Djinn and Exference now store a class use in the same finite syntax node:

```haskell
Constraint HType
```

The node contains a validated nominal class `Name` and Djinn's existing
`HType` arguments.  Djinn continues to own everything semantic: its class
declarations, inferred parameter kinds, closed lookup policy, method-premise
interpretation, and joint query checking are unchanged.  In particular, this
does not import Exference's superclass expansion or instance solver into
Djinn.

The public `Context` alias formerly exposed `(String, [HType])`.  It now names
`Constraint HType`; `mkContext` validates the historical unqualified class
spelling and constructs the shared value.  This is intentionally a
source-breaking API correction, recorded by the package version change from
the imported `2025.2.21` lineage to the local `2026.7.11` fork.

The CLI follows the same path for ordinary query contexts and `?instance`
targets.  Rendering delegates to the shared `Show (Constraint ty)` instance,
so parser, library, and display code no longer maintain a parallel
class-application representation.

## Scope defects found during migration

The representation change exposed two independent implicit-quantifier bugs.

### Capture during class-parameter substitution

Given:

```haskell
class Capture a where use :: f a
```

resolving `Capture f` formerly applied the raw substitution `a := f` and
produced `f f`.  The method-local `f` had captured the free `f` supplied as
the class argument, and kind checking then failed with `cyclic kind`.

Instantiation now identifies method-local variables that occur in an active
substitution image, alpha-renames only those collisions to deterministic
fresh Haskell variables, and then applies the existing simultaneous
substitution.  It deliberately does not rename unrelated method locals:
Djinn's shallow method-premise model still relies on non-colliding names such
as `return :: a -> m a` matching the query goal.

### Conflated variables across method signatures

Given:

```haskell
class Independent a where
  left  :: f a
  right :: f
```

the old kind inference built one local-variable environment for the entire
class.  It therefore forced both occurrences named `f` to denote one kind
variable, even though each method signature implicitly quantifies its own
locals.  `left` required `f :: * -> *`, while `right` required `f :: *`, and a
valid declaration was rejected.

Class parameters still share kind variables across every method, allowing all
signatures to constrain the class head.  Non-parameter variables now receive
a fresh local kind environment per method.  Resolved methods are likewise
kind-checked one at a time instead of being combined into an artificial tuple
that would reunify their local names.

## Preserved joint scope

The earlier joint-kind boundary remains intact.  Class arguments across all
query constraints, an instance target and its prerequisites, and the query
goal are still checked together.  Thus the same free variable cannot acquire
different kinds in separate parts of one request.  The scope split is exact:

- request variables are shared across the request;
- class parameters are shared across their class's methods;
- method-local variables receive signature-local kinds and capture handling.

That last boundary does not introduce full polymorphism.  Once methods become
proof premises, Djinn still compares their resulting string-named type atoms
shallowly with the goal; equal non-colliding spellings can intentionally match.
True private schemes would require explicit quantifiers and fresh
instantiation at each use, a later architectural layer rather than another
substitution tweak.

## Validation

The unit regressions pin the shared class name, backend arguments, arity, and
rendering; unchanged arity and joint-kind diagnostics; capture-free
instantiation; independent sibling method scopes; and successful search with
an otherwise unused colliding context.  The CLI regression runs the two
original failing declarations and retains the existing `Monad m => a -> m a`
result, guarding against over-eager alpha-renaming.

The final package matrix passes warning-clean on GHC 9.12.4:

```text
cabal test all --test-show-details=direct
  35 Djinn unit groups
  4 Djinn properties x 200 cases
  10 packaged CLI scenarios
  48 shared-foundation tests, including 8 x 1000-case properties
cabal check
cabal sdist
git diff --check
```

The `djinn-2026.7.11` source distribution contains the migration report and
both the core and CLI adapters.
