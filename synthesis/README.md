# Haskell Synthesis

`haskell-synthesis` is the parser- and backend-independent destination for the
eventual Djinn/Exference library.  Its first layer defines validated Haskell
names and structured diagnostics.  Neither search engine depends on it yet;
their migrations can therefore proceed incrementally while this vocabulary is
pinned by black-box tests.

Build and test it independently with:

```text
cabal test all
```
