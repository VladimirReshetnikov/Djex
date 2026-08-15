# Worked example: Böhm–Berarducci encodings

This directory holds a self-contained worked example imported from the
[church-encoding project](https://github.com/VladimirReshetnikov/Haskell/tree/main/church-encoding):

- [`Church.hs`](Church.hs) — `Bool`, pairs, lists, `Maybe`, and `Either`
  rebuilt as rank-N polymorphic functions, with a large slice of
  `Data.List`, `Data.Maybe`, and `Data.Either` on top;
- [`Spec.hs`](Spec.hs) — its HUnit suite (403 cases, validated in the
  home repository);
- [`Church.md`](Church.md) — a long-form annotated guide to both.

The material is here because it exercises exactly the type-system
territory Djex targets — rank-N types, `ImpredicativeTypes`, and
eliminator-shaped APIs — and several Djex queries in the
[README](../../README.md) use its `Church Boolean` and `Church List`
shapes. The two Haskell files are example sources, not components of
the `djex` Cabal package; build and test them in their home repository.
