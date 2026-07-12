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

`Language.Haskell.Synthesis.Kind` and `.Declaration` provide the next source
layer: kind variables/arrows, kinded type parameters, synonyms, data and
constructor declarations, opaque types, values, classes, and instances. The
validator enforces declaration namespaces, distinct parameters/members, and
the common type invariants. Synonym bodies, datatype fields, superclasses, and
instance constraints must be covered by their declaration binders; value
signatures and class methods retain Haskell's implicit local quantification.
The layer does not prescribe backend-specific class or instance resolution.

`Language.Haskell.Synthesis.KindInference` owns the common kind unifier. It
checks several types in one variable scope, gives class methods independent
local quantifiers around shared class parameters, validates constraint arity
and parameter kinds, recognizes intrinsic function/list/tuple constructors,
and infers legacy acyclic declaration lists in dependency order. Its
whole-inventory operation additionally admits recursive datatype groups,
rejects recursive synonym expansion, checks values and instances, and
generalizes otherwise unconstrained class parameters for poly-kinded classes
such as `Typeable`. Public fixed-kind results use an uninhabited kind-variable
parameter, so an unsolved monomorphic kind cannot escape.
Closed inventories reject unknown type and class names; open inventories infer
one stable kind per external type name and require every occurrence of an
external class to agree on arity.

`Language.Haskell.Synthesis.Environment` seals a declaration inventory and
builds deterministic type/class, value/method, constructor, and instance-head
indexes. It rejects cross-declaration namespace collisions and duplicate
instances while preserving qualified-name identity. Dependency, kind, and
backend resolution policies remain explicit later validation layers.

Build and test it independently with:

```text
cabal test all
```
