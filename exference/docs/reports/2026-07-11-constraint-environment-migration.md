# Exference nominal-constraint and class-environment migration

- Date: 2026-07-11
- Exference version: 1.7.0.0
- Shared foundation: `haskell-synthesis` 0.1.1.0
- Integration target: Djinn `Context`

## Result

Exference no longer represents a class constraint by embedding a complete
`HsTypeClass`. A constraint is now a compatibility wrapper over the shared,
backend-independent value:

```haskell
data Constraint ty = Constraint
  { constraintClass :: Name
  , constraintArguments :: [ty]
  }
```

The public Exference pattern remains concise:

```haskell
HsConstraint :: QualifiedName -> [HsType] -> HsConstraint
```

`HsTypeClass` stores finite nominal superclass edges, and `HsInstance` stores
prerequisites plus one nominal `instance_head`. Self- and mutually referential
source declarations are therefore ordinary finite values; cycles are detected
as graph errors rather than encoded as lazy Haskell value cycles.

## Checked environment boundary

`mkStaticClassEnv` is the sole non-empty constructor for `StaticClassEnv` and
returns `Either ClassEnvError StaticClassEnv`. It validates, in order:

1. class names occupy the Haskell constructor namespace;
2. class declarations and parameter IDs are unique and non-negative;
3. every superclass and instance class is declared;
4. every known constraint has exactly the declared arity;
5. superclass arguments mention only the declaring class's parameters;
6. the superclass graph is acyclic;
7. only then are superclass-implied instances inflated and indexed.

The representation uses private positional fields with ordinary read-only
accessors. This is deliberate: exporting record labels would allow downstream
record updates to desynchronize the declaration and instance indexes even when
the constructor itself was hidden. `QueryClassEnv` is sealed for the analogous
raw-constraint/superclass-closure invariant.

Public search validation applies the same arity rule to goal and function
constraints. An unknown class remains a distinct nominal external constraint,
because Exference supports loading only part of a source environment; a class
known to the supplied environment must have exact arity.

## Frontend elaboration

`ClassEnvFromHaskellSrc` now performs two strict passes instead of tying a
`Data.Map.Lazy`/`MonadFix` knot:

1. collect and validate unique class headers into a strict name map;
2. bind head variables in declaration order, then elaborate superclass
   arguments against that complete header inventory.

Instances are elaborated afterward. Explicit instance `forall` binders are
installed before their context and head, duplicates/kinded variables are
rejected by the existing variable converter, and no additional variable may
appear outside an explicit binder list.

Unqualified names are resolved against the supplied closed module inventory:
a known same-module declaration wins, one matching external occurrence is
accepted, and multiple matches are diagnosed. This is intentionally a
best-effort policy for Exference's historical hand-maintained environment,
whose files omit imports; it is not a replacement for a full Haskell import
resolver. A future common source frontend should precompute import-aware
occurrence scopes and pass those to both backends.

## API migration

The main 1.7 source changes are:

- `HsConstraint HsTypeClass [HsType]` becomes
  `HsConstraint QualifiedName [HsType]`;
- `HsInstance constraints class parameters` becomes
  `HsInstance constraints headConstraint`;
- class lists become `Map QualifiedName HsTypeClass`;
- `mkStaticClassEnv` becomes checked and performs inflation itself;
- environment accessors are read-only functions rather than record fields;
- recursive-representation-dependent `Data` instances are removed;
- `haskell-synthesis >= 0.1.1` is required.

`toSynthesisConstraint` is total. `fromSynthesisConstraint` is checked because
the shared name domain includes unboxed tuple constructors that Exference's
legacy `QualifiedName` subset cannot represent.

## Validation

The deterministic Exference library/frontend suite now contains 98 cases,
including regressions for finite forcing, cycle detection, nominal
qualification, duplicate order independence, head-variable order, exact
superclass/head/prerequisite/query/binding arity, explicit instance binders,
instance inflation, shared conversion, and shipped-environment loading. The
five CLI cases and 48 shared-foundation cases remain separate. All components
build with GHC 9.12.4 under the common `-Wall -Wcompat` policy.

## Next integration step

Djinn should replace its `(HSymbol, [HType])` `Context` alias with the shared
`Constraint HType` while retaining its current joint kind scope and method
premise semantics. No Exference instance-solver policy should leak into Djinn.
After that adapter is pinned, the next larger common seam is the scope-safe
generated expression/pattern AST and renderer described in the integration
audit.
