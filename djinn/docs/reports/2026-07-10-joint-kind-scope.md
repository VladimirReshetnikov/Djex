# Joint query kind scope and budget hardening

- Date: 2026-07-10
- Branch: `codex/refactor-djinn-codebase`
- Baseline: `5ef935f`
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0

## Finding

`resolveContext` checked every class argument with a separate invocation of
`htCheckTypeKind`, and `inhabit` checked the goal separately again. Each call
created a fresh kind-unification state. A free type variable shared by those
components could consequently receive incompatible kinds without rejection.

For example, a class can infer `f :: * -> *` from one method while an unused
parameter defaults to `a :: *`:

```haskell
class ApplyToBool f unused where
    applyToBool :: f Bool
```

The context `ApplyToBool shared shared` was accepted because each occurrence
of `shared` was checked in isolation. The same defect crossed the context/goal
boundary: a method-less `Value f` context could require `f :: *` while the goal
used `f Bool`, requiring `f :: * -> *`.

This is a frontend soundness defect rather than a proof-search defect. The
logical translation only runs after kind validation and therefore cannot
recover the lost equality between the source-level variables.

## Remediation

`htCheckTypesKinds` now checks a collection of expected-kind/type pairs in one
inference scope. `Djinn.Core` resolves class names and arities first, collects
the goal and all class arguments as labelled obligations, checks them jointly,
and only then substitutes class methods. A small `ResolvedContext` record makes
those phases explicit instead of passing another positional tuple.

Diagnostics retain their previous precision. If one component is independently
ill-kinded, its existing label is reported (for example, `argument Bool of class
Monad`). If all components are valid alone but conflict through a shared free
variable, the error says that their kinds are inconsistent across components.

The same pass hardened search budgets. `Djinn.Core.inhabit` rejects negative
budgets, while the raw internal search treats a directly constructed negative
budget as already expired. Previously it decremented farther below zero and
therefore behaved accidentally like an unlimited search.

## Merge relevance

The eventual Djinn/Exference frontend must establish one kind and type-variable
scope for an entire query before elaborating it into either backend. The new
multi-type checker and the lookup/check/substitute phase separation provide a
small, testable version of that boundary without prematurely sharing the two
engines' incompatible internal type representations.

## Regression coverage

The unit suite now covers:

- conflicting kinds for one variable across two class arguments;
- conflicting kinds across a class context and the query goal;
- direct joint-kind checking in `HCheck`;
- rejection of a negative public query budget; and
- safe expiry of a negative raw `SearchMode` budget.

The complete unit, property, and CLI matrix passes with `-Wall -Wcompat`.
