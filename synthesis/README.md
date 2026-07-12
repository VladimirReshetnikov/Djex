# Haskell Synthesis

`haskell-synthesis` is the parser- and backend-independent destination for the
eventual Djinn/Exference library. Its first layers define validated Haskell
names, structured diagnostics, and non-recursive class constraints
parameterized over a backend's type representation. Djinn and Exference both
store query contexts through the shared `Constraint` value and consume the
validated name vocabulary; Exference additionally uses the shared diagnostic
facade. Their checked class environments, declaration semantics, resolution
policies, and search engines remain backend-specific while later common layers
are extracted behind this tested vocabulary.

Build and test it independently with:

```text
cabal test all
```
