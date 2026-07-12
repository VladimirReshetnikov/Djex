# Haskell Synthesis

`haskell-synthesis` is the parser- and backend-independent destination for the
eventual Djinn/Exference library. Its layers define validated Haskell names,
structured diagnostics, non-recursive class constraints parameterized over a
backend's type representation, a scope-aware generated-code tree, and neutral
operational search status. Djinn
and Exference both store query contexts through the shared `Constraint` value
and consume the validated name vocabulary; Exference additionally uses the
shared diagnostic facade. Their checked class environments, declaration
semantics, resolution policies, and search engines remain backend-specific.

`Language.Haskell.Synthesis.Generated` is the common checked-output boundary.
It separates backend-owned local identities from structural global `Name`s and
represents lambdas, application, tuples, holes, lets, cases, constructor/tuple
patterns, as-patterns, and function clauses. Its independent scope checker
rejects free locals, repeated binders in one pattern, and identity reuse in an
overlapping scope. The renderer allocates stable Haskell variable spellings
against globals and caller reservations, supports the three qualification
policies needed by the existing frontends, and prints symbolic and tuple
applications in Haskell form. Search/proof terms keep their private types and
annotations; backends erase them into this tree only after their own checks.

`Language.Haskell.Synthesis.Search` distinguishes a finished exploration from
one truncated by step, choice-point, candidate, queue, or depth limits, and
supports continuing chunk streams. It deliberately carries no logical
inhabitation claim: each backend keeps its own evidence and search semantics.

`Language.Haskell.Synthesis.Type` is the common parser-independent source-type
tree: variables (optionally flexible/rigid), structural names, application,
functions, boxed or unboxed tuples, and explicit foralls with shared
constraints. It canonicalizes saturated function and tuple constructors,
validates lexical/arity/binder invariants, and computes free variables.
Datatype, synonym, and opaque declaration bodies deliberately do not inhabit
this AST.

Build and test it independently with:

```text
cabal test all
```
