# Djinn class parameter kinds (R-08)

- Date: 2026-07-10
- Resolves: the actionable defect in finding R-08 of
  [2026-07-10-code-review.md](2026-07-10-code-review.md)
  ("the class model is intentionally shallow")
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0, Windows 11

## The defect

Class parameters had no inferred or stored kind, so class arguments were
checked only for arity. The review's reproduction:

```text
Djinn> class Empty a where
Djinn> bad ? Empty (Bool a) => b -> b
```

was accepted even though `Bool a` is ill-kinded, because an empty method
list gave `checkMethods` nothing to inspect. Non-empty classes were only
protected indirectly: substituting arguments into method types happened to
produce ill-kinded method types, which the method check caught with a
message pointing at the wrong thing.

## The fix

Exactly what the finding prescribed: infer and store every class
parameter's kind at declaration time, then check context and instance
arguments against that signature.

- **Inference** (`HCheck.htInferClassKinds`): each parameter and each
  non-parameter method variable receives a fresh kind variable; every
  method type is kind-checked to `*` jointly (the same joint scope the
  stored method types were already validated under); parameter kinds are
  then grounded, defaulting anything unconstrained to `*` — Haskell98
  semantics. `Monad m`'s parameter infers to `* -> *` from its method
  types; `class Empty a where` defaults to `a :: *`.
- **Storage**: `ClassDef` now carries `[(HSymbol, HKind)]` instead of bare
  parameter names. The parser still produces raw names (`RawClassDef`);
  kinds are attached in `runCmd` before the class enters the state, and the
  built-in `Eq`/`Monad` declarations go through the same inference at
  startup.
- **Enforcement** (`HCheck.htCheckTypeKind`, used by `ctxLookup`): every
  class argument in a query context or `?instance` command must now be
  well-kinded *and* fit its parameter's kind. A bare type variable fits any
  kind (its kind is fresh); applications and constructors are checked for
  real. `htCheckType` became the `KStar` instance of the new function:

  ```text
  Djinn> bad ? Empty (Bool a) => b -> b
  Error: argument Bool a of class Empty: kind mismatch: k0 -> k1 vs *
  Djinn> ?instance Monad Bool
  Error: argument Bool of class Monad: kind mismatch: * vs * -> *
  ```

- **No staleness**: `Environment.validateEnvironment` now re-infers every
  class's parameter kinds against the rebuilt type graph and returns the
  refreshed classes alongside the checked synonyms; `:delete` and type
  replacement install both atomically. Verified end to end: after
  `type T :: *`, `class D f where d :: f T -> Bool` accepts
  `?instance D Maybe`; replacing the type with `type T :: * -> *` refreshes
  `f` to kind `(* -> *) -> *` and the same instance query is then rejected.

The class-declaration path also became slightly stricter in a welcome way:
kind inference subsumes the old whole-tuple method check, so a class whose
method types are ill-kinded is rejected at declaration with the same
machinery that computes the signature.

## What deliberately remains shallow

R-08's broader observations are design boundaries, not defects, and remain
documented in the README: there is still no superclass or instance
database, no imported package environment, and no fresh instantiation of
polymorphic methods — a context is just a bundle of extra premises at
exactly the declared types.

One residual caveat inherited from the existing architecture: the goal and
its context arguments are kind-checked in separate inference scopes, so a
query variable used at incompatible kinds in the goal versus a context
(e.g. `f ? Monad m => m -> m`) is not detected as a conflict. Joining those
scopes naively would wrongly unify method-local variable names with query
variables of the same spelling; doing it right needs alpha-renaming of
method locals first and is left as future work (the output remains a
candidate that GHC will reject, per the standing caveat-emptor contract).

## Validation

- New unit group "infer and enforce class parameter kinds": inference for
  `Monad`, defaulting for method-less classes, rejection of self-applied
  parameters, and `htCheckTypeKind` acceptance/rejection cases including
  variables-fit-any-kind.
- New CLI scenario: the R-08 reproduction is rejected, `?instance Monad
  Bool` is rejected, a well-kinded phantom context and a higher-kinded
  context both still realize.
- Existing suites unchanged and green (`Eq`/`Monad Maybe`/`Monad (Either
  e)` behavior identical); builds warning-clean under `-Wall -Wcompat`;
  `cabal check` clean.
