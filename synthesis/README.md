# Haskell Synthesis

`haskell-synthesis` is the parser- and backend-independent destination for the
eventual Djinn/Exference library. Its first layers define validated Haskell
names, structured diagnostics, and non-recursive class constraints
parameterized over a backend's type representation. Djinn consumes the shared
name lexer and renderer. Exference uses the structural name and diagnostic
facades and stores every class context through the shared `Constraint` value;
its checked class environment remains backend-specific. Declaration semantics,
class resolution, and both search engines remain independent while later
common layers are extracted behind this tested vocabulary.

Build and test it independently with:

```text
cabal test all
```
