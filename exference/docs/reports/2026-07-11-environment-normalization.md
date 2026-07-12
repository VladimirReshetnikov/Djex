# Exference shipped-environment normalization

- Date: 2026-07-11
- Exference version: 1.7.0.0
- Context: follow-up to the nominal class-environment migration

## Result

Loading the complete packaged environment is now internally clean and
deterministic:

```text
41 classes
485 accepted source instances
535 instances after superclass inflation
156 function declarations
```

The loader emits no unknown-class, unknown-type-constructor, unapplied-rating,
or discarded-class-environment diagnostics for the shipped corpus. These
counts are asserted by the regression suite so a typo, missing packaged file,
or catastrophic recovery path cannot silently erase the class inventory again.

## Source-data repairs

The checked environment exposed defects that the former recursive placeholder
model had hidden:

- `Control.Arrow.Arrow` now names its real `Control.Category.Category`
  superclass and its `Kleisli` instance requires `Control.Monad.Monad` rather
  than the nonexistent class `Control.Monad`;
- the real `ArrowApply` class/method and opaque `ArrowMonad` type are present,
  making the standard `Monad (ArrowMonad a)` evidence meaningful;
- `Travesable`, `Data.Data.Data.Eq.Eq`, `Data.Data.Data.Ord.Ord`, wrong
  `MonadTrans`/`MonadIO` qualifications, and `Data.Monoid.Down` are corrected;
- dead `Bifunctor`, `MonadFix`, `HasResolution`, `IsChar`, and `IsString`
  instances were removed rather than represented as evidence for absent
  classes;
- the duplicated `Int8` block is the missing `Int64` block, in numeric order;
- `Ordering` has one identity (`Prelude.Ordering`) instead of competing
  `Prelude` and `Data.Ord` declarations;
- exact duplicate `ST` instances and duplicate `Monoid Ordering` evidence were
  removed;
- stale ratings for deliberately omitted information-losing methods (`gmapQi`
  and `extract`) were removed.

Repeated external types now use their canonical public module names:
`Control.Applicative.WrappedMonad`/`WrappedArrow`,
`Control.Arrow.ArrowMonad`, `Control.Monad.ST.ST`,
`Control.Concurrent.STM.STM`, `Data.Functor.Const.Const`,
`Data.Functor.Identity.Identity`, `Control.Exception.Handler`, the three
`System.Console.GetOpt` types, and `ReadP`/`ReadPrec`. Small module files declare
these constructors opaquely: they provide kind and nominal identity without
inventing implementation constructors that search could synthesize.

## Loader behavior

Unqualified resolution retains three deterministic cases:

1. a declared same-module name wins;
2. one matching known external occurrence resolves to that declaration;
3. multiple matches are an ambiguity diagnostic.

If no declaration is known, the spelling now remains unqualified. This models
one external nominal type rather than fabricating a different current-module
type in every hand-maintained environment file. The policy remains a closed
inventory approximation, not a full Haskell import resolver.

Environment diagnostics now distinguish unknown type constructors from
unknown constraint classes. Constraint validation uses the strict loaded class
inventory, while public ad-hoc search input keeps its intentional open-world
unknown-class policy. Unknown constructor warnings from superclass-inflated
instances are deduplicated.

Finally, malformed rating files no longer discard successfully parsed
declarations, deconstructors, names, or classes. The loader reports the rating
error and assigns default zero penalties, matching the recovery semantics of
the directory-based loader.

## Validation

The Exference library/frontend suite contains 103 deterministic cases after
this work. New cases cover default-rating recovery, precise loader diagnostics,
strict signature-class checking, deduplicated inflated-instance warnings,
unqualified external identity, and the exact shipped inventory. The separate
five-case CLI suite continues to exercise the packaged environment end to end.
