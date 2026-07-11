# Haskell Synthesis

`haskell-synthesis` is the parser- and backend-independent destination for the
eventual Djinn/Exference library.  Its first layer defines validated Haskell
names and structured diagnostics.  Djinn consumes the shared name lexer and
renderer; Exference uses the same structural name model and diagnostic record
through compatibility facades.  Their search engines remain independent while
later common layers are extracted behind this tested vocabulary.

Build and test it independently with:

```text
cabal test all
```
