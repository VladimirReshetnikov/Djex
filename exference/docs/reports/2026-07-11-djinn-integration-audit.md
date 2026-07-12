# Djinn/Exference integration architecture and correctness audit

- Date: 2026-07-11
- Audited committed state: `c9684e1`
- Additional state inspected: the Exference migration to
  `Language.Haskell.Synthesis.Name`, subsequently committed as `6b2f6bf`,
  including the new compatibility module
  `Language.Haskell.Exference.Core.Name`
- Scope: `djex/djinn/`, `djex/exference/`, and the neutral
  `djex/synthesis/` foundation

## Implementation update

The findings and reproducers below describe the explicitly named audited
commits. Subsequent work on the same branch has completed roadmap stages 1–3
and the shared/Exference portion of stage 4: validated names and diagnostics,
capture-free checked Exference output, closed symmetric unification and prenex
handling, truthful search completion, and the shared non-recursive constraint
representation. Exference 1.7 now uses
nominal shared constraints, sealed strict class maps, two-pass frontend
elaboration, exact arity/duplicate/cycle validation, and explicit environment
lookup for superclass and instance resolution. Stage 4's remaining backend
task is adapting Djinn's `Context` without changing Djinn's method-resolution
or kind-checking semantics. The historical snippets remain useful as regression
rationales even though several no longer type-check against the 1.7 API.

## Executive summary

Djinn and Exference already have the right high-level integration shape: two
different synthesis engines behind parser-independent core components, with a
neutral `haskell-synthesis` package beginning to own shared vocabulary. Their
search algorithms should not be combined. Djinn's LJT decision procedure and
Exference's polymorphic, constraint-aware heuristic search make intentionally
different soundness, termination, and result-ordering promises.

The repeated code is concentrated at the boundaries around those engines:

- source type, constraint, kind, and declaration models;
- declaration-environment validation;
- generated expression and pattern trees;
- scope validation, simplification, binder naming, and rendering;
- search budgets, completion status, and candidate envelopes;
- session and command-line orchestration.

The safest high-value next shared abstraction is a non-recursive,
type-parameterized class constraint. The largest payoff comes from a shared,
scope-safe generated-code AST and renderer. A common type/kind/declaration IR
should follow those smaller seams, not precede them. A generic unifier should
not be extracted: the superficially similar unifiers implement materially
different semantics.

The audit also found concrete correctness defects in Exference's current
checked-output boundary, renderer, class graph, unifier, outer-forall handling,
type printer, search status, declaration ingestion, and recursive-datatype
classification. These are not merely architectural preferences; minimal
reproducers are recorded below.

## Architecture comparison

### Type representations

Djinn's type representation is `Djinn.Internal.HTypes.HType`:

```haskell
data HType
  = HTApp HType HType
  | HTVar HSymbol
  | HTCon HSymbol
  | HTTuple [HType]
  | HTArrow HType HType
  | HTUnion [(HSymbol, [HType])]
  | HTAbstract HSymbol HKind
```

`HTVar`, `HTCon`, `HTApp`, `HTArrow`, and `HTTuple` are source type
expressions. `HTUnion` and `HTAbstract` are declaration bodies encoded in the
same datatype. The same large module also owns the ReadP type parser, synonym
normalization, translation to LJT formulae, Djinn's generated Haskell AST,
simplification, scope analysis, and pretty-printing. This concentration makes
`Djinn.Internal.HTypes` the largest architectural knot on the Djinn side.

Exference's type representation is
`Language.Haskell.Exference.Core.Types.HsType`:

```haskell
data HsType
  = TypeVar TVarId
  | TypeConstant TVarId
  | TypeCons QualifiedName
  | TypeArrow HsType HsType
  | TypeApp HsType HsType
  | TypeForall [TVarId] [HsConstraint] HsType
```

`TypeVar` is flexible during search, while `TypeConstant` is a rigid skolem.
Exference represents arrows structurally but represents tuples and lists as
applications of structural shared names. `TypeForall` can occur recursively in
the raw datatype, although the public search boundary rejects rank-N positions.

The structural overlap is direct:

| Shared concept | Djinn | Exference |
| --- | --- | --- |
| variable | `HTVar String` | `TypeVar TVarId` or rigid `TypeConstant TVarId` |
| constructor | `HTCon String` | `TypeCons QualifiedName` |
| application | `HTApp` | `TypeApp` |
| function | `HTArrow` | `TypeArrow` |
| tuple | `HTTuple` | tuple `TypeCons` applied to elements |
| quantifier/context | implicit free names only | `TypeForall` with `HsConstraint`s |
| data/abstract declaration | embedded as `HTUnion`/`HTAbstract` | represented outside `HsType` |

The shared-name migration makes Exference's `QualifiedName` an
opaque compatibility view of `Language.Haskell.Synthesis.Name.Name`. Djinn's
`Djinn.Internal.HIdentifier` now delegates lexical rules to the same shared
module while retaining a string-compatible parser API. Semantic type identity
is nevertheless still backend-specific.

### Kinds

Djinn has an explicit `HKind`:

```haskell
data HKind = KStar | KArrow HKind HKind | KVar Int
```

`Djinn.Internal.HCheck` implements first-order kind inference and unification.
Important entry points include `htCheckEnv`, `htCheckTypeKind`,
`htCheckTypesKinds`, and `htInferClassKinds`. `Djinn.Core.Environment` is
rebuilt transactionally, and query goals plus every class argument are checked
in one inference scope so a shared free type variable cannot receive
inconsistent kinds in different signature components.

Exference has no core kind representation. `TypeFromHaskellSrc.tyVarTransform`
rejects `KindedVar`; `convertTypeNoDeclInternal` rejects `TyKind`; otherwise
ill-kinded type applications can enter search. The common checker should
eventually be extracted from Djinn after both backends have a common type and
declaration IR. Exference needs an explicit policy for unknown query-only type
constructors: infer an opaque kind or reject them, rather than silently treating
all applications as meaningful.

### Contexts and class environments

Djinn's public constraint is:

```haskell
type Context = (HSymbol, [HType])
```

A class declaration stores parameter kinds and method types. `resolveContext`
and `resolveInstanceMethods` check arity and kinds and substitute class
parameters into method signatures. Djinn has no superclass or installed
instance solver: a context simply contributes instantiated methods as proof
premises, and `?instance` prerequisites are likewise premises.

Exference models a richer class system:

- `HsTypeClass` stores a name, parameter IDs, and superclass constraints;
- `HsConstraint` stores a complete `HsTypeClass` plus argument types;
- `HsInstance` stores prerequisites, a complete class value, and head types;
- `StaticClassEnv` stores classes and instances;
- `QueryClassEnv` stores assumed constraints, superclass closure, and a
  per-variable index;
- `Core.Internal.ConstraintSolver` performs right-biased instance matching and
  cycle-aware prerequisite search.

Only the syntax and nominal identity of a constraint should be shared. The
backend-specific resolution semantics must remain separate.

### Declaration environments

`Djinn.Core.Environment` is opaque and constructed only through
`emptyEnvironment`, `standardEnvironment`, `declare`, and
`removeDeclaration`. `Djinn.Internal.Environment.validateEnvironment` enforces
namespace, constructor, method, dependency, kind, and acyclicity invariants for
the whole candidate environment before a mutation is committed.

Exference's environment is distributed among public records and frontend
tuples:

- `FunctionBinding` and `DeconstructorBinding`;
- `StaticClassEnv`;
- `TypeDeclMap`;
- the five-component result of `EnvironmentParser.parseModules`;
- `EnvDictionary`, which does not include every frontend component.

No single opaque validation boundary checks the complete declaration graph.
Several maps silently choose one duplicate declaration, as detailed in finding
9.

### Parser and elaboration boundaries

Djinn uses a deliberately small ReadP language. `Djinn.Internal.HTypes` parses
types and kinds, while `Djinn` parses commands and declarations. The stable
`Djinn.Core` facade requires full input consumption but still reports
`Either String` rather than the shared diagnostic type. Source spans are not
retained.

Exference uses `haskell-src-exts` in its unnamed frontend library:

- `TypeFromHaskellSrc` converts type syntax and names;
- `TypeDeclsFromHaskellSrc` expands type synonyms;
- `BindingsFromHaskellSrc` extracts functions, constructors, and methods;
- `ClassEnvFromHaskellSrc` ties classes and instances;
- `EnvironmentParser` coordinates files, ratings, built-ins, and warnings.

`parseType` and rating parsing use the shared structured diagnostic facade, but
many elaboration paths still report `Either String` or writer strings. The two
parsers need not be unified yet. They should first target a common validated
IR.

### Generated expressions, simplification, and rendering

Djinn converts proof terms into `HClause`, `HPat`, and `HExpr` inside
`Djinn.Internal.HTypes`. The converter alpha-renames binders, checks for escaped
free variables, simplifies under an explicit total-language assumption, and
uses a custom pretty-printer. Names remain strings, though rendering delegates
operator syntax to shared lexical rules.

Exference generates `Core.Expression.Expression`, independently checks it with
`Core.ExpressionCheck.checkExpression`, optionally simplifies it with
`Core.ExpressionSimplify.simplifyExpression`, and converts it to an HSE AST in
`ExpressionToHaskellSrc`. Binder identity is an integer during search but is
discarded during rendering in favor of a type-derived spelling.

Both backends need the same concepts: local binders, global shared names,
lambda/application, tuples, constructor and tuple patterns, as-patterns, lets,
cases, scope validation, alpha-equivalence, safe simplification, and contextual
rendering. This is the most valuable shared boundary.

### Substitution and unification

Djinn has four distinct operations:

- `HTypes.substHT` for source-type parameters and aliases;
- the kind unifier in `HCheck`;
- capture-safe proof-term substitution/copying in `LJT.subst` and `LJT.copy`;
- the independent sum/product/atom proof-type unifier in `ProofCheck`.

Exference has:

- capture-aware `applySubst`/`applySubsts` over `HsType`;
- symmetric, offset, right-only, and right-offset unifiers in
  `Core.Internal.Unify`;
- expression-hole replacement in `Expression.fillExprHole`;
- local expression substitution in `ExpressionSimplify.replaceVar`.

These operations should not be merged based on superficial similarity. A
common type IR can later own one binder-aware type traversal/substitution
library, but proof, kind, and search unifiers should remain backend-specific.

### Search and result APIs

Djinn exposes:

```haskell
inhabit
  :: QueryOptions
  -> Environment
  -> [Context]
  -> HSymbol
  -> HType
  -> Either String QueryReport
```

`QueryOutcome` distinguishes `Realized`, logically `Unrealizable`,
`UnrealizableWithoutSelfReference`, and budget-limited `Undecided`.
`QueryReport`, however, contains rendered clause strings rather than structured
candidates, and its formula/proof debug fields are also strings.

Exference exposes lazy candidate lists and chunks:

- `findExpressions` and `findExpressionsEither`;
- `findExpressionsChunked`;
- `findExpressionsWithStats`;
- tuple-shaped `ExferenceOutputElement`;
- `ExferenceChunkElement` with `SearchStatus`.

Compatibility functions erase invalid-input errors or completion status. The
common layer should share operational completion and candidate envelopes, not
equate Djinn's proof of uninhabitability with Exference's exhausted or pruned
heuristic exploration.

### CLI and session layers

Djinn's `Djinn` module combines command parsing, session state, file loading,
environment mutation, search configuration, query orchestration, and printing.
`Djinn.Internal.REPL` supplies the Haskeline loop and `app/Main.hs` is thin.

Exference's `src-exference/Main.hs` combines getopt processing, environment
loading, HSE query parsing, binding filtering, heuristic configuration,
candidate selection, simplification, rendering, and presentation. The unnamed
frontend library contains reusable pieces, but no reusable session object owns
those policies.

A shared session/CLI layer should be the last major integration step, after
types, generated candidates, diagnostics, and completion statuses are
structured uniformly.

## Concrete correctness findings

### 1. A checked Exference candidate is changed after checking

References:

- `Language.Haskell.Exference.Core.Internal.Exference.transformSolutions`
- `Language.Haskell.Exference.Core.ExpressionCheck.checkExpression`
- `Language.Haskell.Exference.Core.ExpressionSimplify.simplifyExpression`
- `simplifyId` and `simplifyCompose`
- `src-exference/Main.main`

`transformSolutions` independently checks the raw generated `Expression`.
The CLI later calls `simplifyExpression` before rendering. `simplifyId`
replaces an identity lambda with the unqualified global `id`, and
`simplifyCompose` can introduce `(.)`. Neither name is guaranteed to exist in
`input_envFuncs`, and no checker runs after the transformation.

Minimal reproducer:

```haskell
let ty = TypeConstant 0
    expression = ExpLambda 1 ty (ExpVar 1 ty)
    classes = mkQueryClassEnv (mkStaticClassEnv [] []) []
    goal = TypeArrow ty ty

checkExpression classes [] [] goal [] expression
-- Right ()

let simplified = simplifyExpression expression
checkExpression classes [] [] goal [] simplified
-- Left (UnknownBinding idName)
```

This is fixable safely inside Exference now. Remove environment-introducing
rewrites, or parameterize them by an explicit set of available globals and
recheck the transformed candidate. The broader shared AST is not required for
the immediate correction.

### 2. Rendering can capture a global name

References:

- `Language.Haskell.Exference.ExpressionToHaskellSrc.convert`
- `convertInternal`, `namedExpression`, and `toQName`
- `Language.Haskell.Exference.Core.Types.showTypedVar`

Binder spellings are selected without reserving global occurrences. At
qualification level zero, `toQName` also discards the module qualifier of an
ordinary global name.

Minimal reproducer:

```haskell
Right globalA = mkQualifiedName ["M"] "a"
Right intName = mkQualifiedName [] "Int"
Right boolName = mkQualifiedName [] "Bool"

let expression = ExpLambda 1 (TypeVar 10) (ExpName globalA)
    functions =
      [ FunctionBinding
          (TypeCons boolName) globalA 0 [] []
      ]
    goal = TypeArrow (TypeCons intName) (TypeCons boolName)
    classes = mkQueryClassEnv (mkStaticClassEnv [] []) []

checkExpression classes functions [] goal [] expression
-- Right ()

prettyPrint (convert 0 expression)
-- "\\a -> a" (modulo pretty-printer spacing)
```

The checked expression refers to global `M.a` and has type `Int -> Bool`; the
rendered expression refers to its local parameter and is identity-like.

A local renderer fix is possible now: allocate binder names after computing
the exact global spellings emitted by the selected qualification policy. Full
qualification avoids this particular qualified-global example, but it does not
fix unqualified globals or binder/binder collisions.

### 3. Distinct binder IDs can render to the same Haskell variable

References:

- `Language.Haskell.Exference.Core.Types.showVar`
- `showTypedVar`
- `Language.Haskell.Exference.Core.Expression.collectVarTypes`
- `Language.Haskell.Exference.ExpressionToHaskellSrc.convert`

`showTypedVar` is not injective. A binder with ID 6 whose inferred type begins
with constructor `T` becomes `t6`; a binder with ID 33 and a variable-like type
also becomes `t6`, because `showVar 33 == "t6"`.

Minimal reproducer:

```haskell
Right tName = mkQualifiedName [] "T"

let expression =
      ExpLambda 6 (TypeCons tName) $
        ExpLambda 33 (TypeVar 100) $
          ExpVar 6 (TypeCons tName)

prettyPrint (convert 0 expression)
-- "\\t6 t6 -> t6" (modulo pretty-printer spacing)
```

The internal term is well scoped and means `T -> a -> T`; the rendered body
refers to the inner binder. A fresh-name supply keyed by `TVarId`, checking
both previously allocated binders and emitted globals, is a safe immediate
fix. A shared generated-code renderer would prevent both backends from
implementing this invariant independently.

### 4. Cyclic superclass graphs are not safely forceable

References:

- `Language.Haskell.Exference.Core.Types.HsTypeClass`
- `HsConstraint`
- their generic `NFData` instances
- `Language.Haskell.Exference.ClassEnvFromHaskellSrc.getTypeClasses`
- its lazy recursive `resultMap`

Every `HsConstraint` embeds a complete `HsTypeClass`, and every class embeds
its superclass constraints. The frontend deliberately ties recursive class
values using a lazy map. Custom nominal `Eq` and `Ord` instances avoid
recursive comparison, but generic `NFData` follows the cycle.

Minimal reproducer:

```haskell
Right cName = mkQualifiedName [] "C"

let cls = HsTypeClass
      cName
      [0]
      [HsConstraint cls [TypeVar 0]]

timeout 100000 (evaluate (force cls))
-- Nothing: forcing does not terminate before the timeout
```

A shallow `NFData` instance would only conceal the modeling defect and would
not put the value in normal form. The durable fix is for a constraint to store
the class `Name` and its arguments, with superclass declarations resolved
through a separate environment map. This fix does not depend on the shared
generated AST.

### 5. Symmetric unification returns substitutions that do not close across sides

References:

- `Language.Haskell.Exference.Core.Internal.Unify.unify`
- `UniState2`, `StepLeftSubst`, and `StepRightSubst`
- `Language.Haskell.Exference.Core.Internal.Exference.byGenericUnify`
- its `byUnified`, `allSS`, and `constrs2` definitions

The symmetric unifier maintains separate left and right maps. When it learns a
substitution on one side, it updates equations and earlier substitutions on
that side, but does not zonk ranges already stored for the opposite side.

Minimal reproducer:

```haskell
Right pairName = mkQualifiedName [] "Pair"
Right intName  = mkQualifiedName [] "Int"

let pair x y = TypeApp (TypeApp (TypeCons pairName) x) y
    left  = pair (TypeVar 1) (TypeVar 1)
    right = pair (TypeVar 2) (TypeCons intName)

unify left right
-- can return maps equivalent to:
-- left:  1 -> Int
-- right: 2 -> TypeVar 1

let Just (leftSubsts, rightSubsts) = unify left right
    leftResult  = snd (applySubsts leftSubsts left)
    rightResult = snd (applySubsts rightSubsts right)

leftResult == rightResult
-- False: Pair Int Int /= Pair (TypeVar 1) Int
```

`byUnified` partially composes maps while updating goals, but applies only
`provSS` to `provConstrs`. A provided constraint can therefore retain a
goal-side variable after `goalSS` has grounded it.

The clean implementation uses tagged identities such as
`LeftVar TVarId | RightVar TVarId`, runs one unifier, fully zonks, and then
projects the result. A smaller fix may cross-zonk both maps before returning.
Required properties are:

```haskell
unify left right == Just (ls, rs)
  ==> applyFully ls left == applyFully rs right
```

and corresponding projection properties for `unifyOffset` and
`unifyRightOffset`.

### 6. Multiple outer foralls pass validation but fail independent checking

References:

- `Language.Haskell.Exference.Core.Internal.Exference.containsNestedForall`
- `validateExferenceInput`
- `Language.Haskell.Exference.Core.ExpressionCheck.instantiateGoal`
- `unifyTypes`
- the `Right () <- [checkExpression ...]` filter in `transformSolutions`

`containsNestedForall` strips an arbitrary chain of outer `TypeForall`s and
accepts it if no quantifier remains below that chain. `instantiateGoal`
instantiates exactly one outer layer. The checker then encounters the second
`TypeForall`, returns `UnsupportedNestedForall`, and `transformSolutions`
silently drops the candidate.

Minimal black-box reproducer using an otherwise trivial identity goal:

```haskell
let goal =
      TypeForall [0] [] $
        TypeForall [1] [] $
          TypeArrow (TypeVar 1) (TypeVar 1)
    input = identityInput { input_goalType = goal }

validateExferenceInput input
-- Right ()

findExpressionsEither input
-- The input is accepted, but the identity candidate is filtered when the
-- independent checker sees the remaining TypeForall.
```

`instantiateGoal` should consume every outer quantifier and allocate disjoint
rigid `TypeConstant` ranges. Search already adds each outer context to its
query class environment as it traverses the layers.

### 7. `showHsType` can print a quantifier whose body uses another name

References:

- `Language.Haskell.Exference.Core.Types.showHsType`
- `TypeVarIndex`
- `showVar`

The quantifier list is rendered with `showVar`; occurrences in the body are
rendered by reverse lookup through `TypeVarIndex`. The two policies disagree.

Minimal reproducer:

```haskell
showHsType
  (Map.fromList [("x", 0)])
  (TypeForall [0] [] (TypeVar 0))
-- "forall v0 . () => x"
```

The result binds `v0` but refers to `x`; it also prints an empty `() =>`
context. One stable lookup/allocation map must render both binders and
occurrences, and the empty context should be omitted.

### 8. Search pruning is reported as exhaustion, and invalid CLI input can reach `last []`

References:

- `Language.Haskell.Exference.Core.Internal.Exference.SearchCompletion`
- `transformSolutions` and its local `completion`
- `limitQueue` and `depthAllowed`
- `Language.Haskell.Exference.Core.findExpressionsWithStats`
- the `EnvUsage` branch in `src-exference/Main.main`

`completion` returns `SearchExhausted` whenever the retained priority queue is
empty. It does not account for nonzero queue- or depth-pruning counts. Existing
tests demonstrate the state directly:

```haskell
chunk <- onlyChunk identityInput
  { input_maxQueueSize = Just 0 }

searchCompletion (chunkStatus chunk)
-- SearchExhausted

searchQueuePruned (chunkStatus chunk)
-- 1
```

The only unexplored continuation was deleted, so the result is truncated, not
exhaustive. The same issue occurs when depth pruning empties the queue.

The CLI separately validates `ExferenceInput`, prints an error, and continues.
Compatibility result functions translate validation failure to an empty list.
The environment-usage path then does:

```haskell
last (findExpressionsWithStats invalidInput)
-- *** Exception: Prelude.last: empty list
```

Introduce a truncated/pruned completion carrying one or more reasons. The CLI
must stop after validation failure and avoid partial list operations. A
no-result truncated search must be presented as undecided, not as exhaustive
failure.

### 9. Duplicate declarations are silently overwritten or selected by list order

References:

- `Language.Haskell.Exference.ClassEnvFromHaskellSrc.getTypeClasses`
- its strict `secondMap = Map.fromList ...`
- `Language.Haskell.Exference.TypeDeclsFromHaskellSrc.getTypeDecls`
- its `declarationMap = Map.fromList ...`
- `Language.Haskell.Exference.EnvironmentParser.parseModules`
- its final `TypeDeclMap = Map.fromList ...`
- `Language.Haskell.Exference.Core.ExpressionCheck.instantiateBinding`

The HSE parser accepts many declaration collections that are syntactically
valid but semantically invalid. Exference never creates a single validated
module environment, so map construction chooses one exact duplicate.

Minimal frontend reproducer:

```haskell
module M where

type T = Int
type T = Bool
```

Parsing succeeds. `getTypeDecls` constructs a map by `M.T`; one declaration is
used during resolution, and the final `TypeDeclMap` again selects one by list
order instead of returning a duplicate-declaration diagnostic. Duplicate class
declarations take the analogous path through `getTypeClasses`.

The core has the same ambiguity for direct callers:

```haskell
let bindings =
      [ FunctionBinding intType  fName 0 [] []
      , FunctionBinding boolType fName 0 [] []
      ]

-- ExpressionCheck.instantiateBinding takes the first matching binding.
checkExpression classes bindings [] intType [] (ExpName fName)
-- Result depends on list order rather than a validated uniqueness invariant.
```

The common declaration environment should adopt Djinn's opaque,
transactionally validated model, extended to qualified `Name`s and full
source declarations.

### 10. Every parsed datatype is marked non-recursive

References:

- `Language.Haskell.Exference.BindingsFromHaskellSrc.getDataConss`
- `Language.Haskell.Exference.Core.FunctionBinding.DeconstructorBinding`
- `deconstructorRecursive`
- `Language.Haskell.Exference.Core.Internal.Exference.addScopePatternMatch`

`getDataConss` constructs every `DeconstructorBinding` with
`deconstructorRecursive = False`; a nearby TODO acknowledges that recursion is
not determined. `addScopePatternMatch` consequently treats recursive data as
eligible for the non-recursive path, while its fallback says recursive
deconstruction is unsupported.

Minimal reproducer:

```haskell
module M where

data Nat = Zero | Succ Nat
```

After parsing and `getDataConss`, the `Nat` binding has:

```haskell
deconstructorRecursive natBinding == False
```

The field type plainly refers to `M.Nat`. A declaration dependency/SCC pass
should classify recursive data once. Djinn can continue to reject recursive
structural expansion, while Exference can preserve the classification and
apply an explicit search policy.

## Ranked shared abstractions

### 1. Non-recursive constraints and class references

- Payoff: high
- Risk: low to medium
- Recommended module: `Language.Haskell.Synthesis.Constraint`

The minimal shared form is parameterized over the backend type:

```haskell
data Constraint ty = Constraint
  { constraintClass :: Name
  , constraintArguments :: [ty]
  }
```

Class declarations live separately in a `Map Name (ClassDeclaration ty kind)`.
Constraints never embed the complete declaration. This directly matches
Djinn's `Context`, removes Exference's recursive class values, centralizes
nominal class identity, and creates a natural arity-validation boundary.

Migration sequence:

1. Add the parameterized constraint and focused tests to `haskell-synthesis`.
2. Convert Exference's class table to a `Map Name ...`.
3. Make superclass inflation and instance resolution take that map explicitly.
4. Reject known-class arity mismatches before every current `zip`-based
   substitution; preserve an explicit policy for unknown classes.
5. Retain compatibility patterns or aliases where practical.
6. Adapt Djinn's `Context` and context parser to the shared value while keeping
   Djinn-specific method resolution and joint kind checking.

Required tests:

- forcing self- and mutually recursive superclass declarations terminates;
- superclass closure across cycles remains finite;
- too few and too many known-class arguments are rejected;
- unknown classes remain nominally distinct;
- `A.C` and `B.C` remain distinct;
- all existing Djinn joint-kind-scope context tests continue to pass.

### 2. Scope-safe generated-code AST and renderer

- Payoff: very high
- Risk: medium
- Recommended modules: `Language.Haskell.Synthesis.Expression`,
  `.Scope`, `.Simplify`, and `.Render`

Use opaque binder identities rather than rendered strings, and parameterize
type annotations:

```haskell
newtype BinderId = BinderId Int

data Expr ty
data Pattern ty
data Clause ty
```

The common superset needs:

- local references identified by `BinderId`;
- global references identified by shared `Name`;
- lambda, application, tuple, let, and case;
- variable, wildcard, constructor, tuple, and as-patterns;
- optional backend type annotations on binders/references;
- explicit holes only in an incomplete/search-state form, not checked output.

Migration sequence:

1. Add the AST, scope validator, free-local/global analysis,
   alpha-equivalence, and collision-free name supply.
2. Add adapters from both current ASTs and differential golden rendering tests
   without changing backend search.
3. Move Exference first because findings 1-3 are active correctness defects.
4. Port only semantics-preserving total-core simplifications. Treat `id` and
   `(.)` introduction as optional, environment-aware rewrites.
5. Port Djinn's proof-term conversion, Haskell simplification, and printer.
6. Change both public APIs to return structured candidates; render in session
   or CLI code.

Required tests:

- the three output reproducers in findings 1-3;
- allocated binder spellings are unique and disjoint from emitted unqualified
  globals and the clause target;
- simplification does not increase the set of global names unless explicitly
  authorized;
- alpha-renaming preserves alpha-equivalence and free globals;
- each case alternative has an independent lexical scope;
- every rendered candidate parses and passes `ghc -fno-code` in a generated
  module with its declarations;
- existing Djinn clause and Exference HSE goldens remain stable where their
  formatting is deliberate.

### 3. Operational search envelope

- Payoff: medium to high
- Risk: low to medium
- Recommended module: `Language.Haskell.Synthesis.Search`

Share operational completion, not backend theorem claims:

```haskell
data Completion
  = Finished
  | Truncated (NonEmpty TruncationReason)

data TruncationReason
  = StepLimitReached ...
  | ChoicePointLimitReached ...
  | CandidateLimitReached ...
  | QueueLimitPruned ...
  | DepthLimitPruned ...
```

Candidate payload, score/statistics, and negative evidence should remain
parameterized. Djinn may attach a proof-backed uninhabitability decision;
Exference may attach only operational exhaustion or truncation. A common
`NoSolution` constructor would be unsound.

Required tests:

- unbudgeted Djinn refutation is finished and logically uninhabitable;
- budgeted Djinn failure is truncated/undecided;
- Exference step, queue, and depth limits retain distinct reasons;
- pruning can never produce bare `Finished`/`SearchExhausted`;
- simple compatibility functions cannot silently discard invalid input or
  truncation without an explicitly named lossy adapter.

### 4. Common type, kind, and declaration IR

- Payoff: maximal
- Risk: high
- Recommended modules: `Language.Haskell.Synthesis.Type`, `.Kind`,
  `.Declaration`, and `.Environment`

A parameterized type preserves backend variable identity:

```haskell
data Type variable
  = TypeVariable variable
  | TypeConstructor Name
  | TypeApplication (Type variable) (Type variable)
  | FunctionType (Type variable) (Type variable)
  | TupleType Boxity [Type variable]
  | ForallType [variable] [Constraint (Type variable)] (Type variable)
```

`HTUnion` and `HTAbstract` belong in declarations, not the ordinary type AST.
Exference's flexible/rigid distinction can be represented in its variable
parameter or during lowering.

Migration sequence:

1. Define common types and lossless adapters for both existing ASTs.
2. Differentially test function/list/tuple canonicalization, variable
   traversal, substitution, free-variable analysis, and rendering.
3. Define common synonyms, data constructors, abstract types, signatures,
   classes, and instances with optional source spans.
4. Make both existing parsers elaborate to the common declarations.
5. Build an opaque validated module environment with Haskell namespace,
   duplicate, arity, synonym-cycle, and declaration-SCC checks.
6. Extract Djinn's kind checker over the common representation.
7. Give kind checking an explicit unknown-constructor policy: Djinn rejects;
   Exference may infer an opaque query-only constructor.
8. Lower the validated environment separately to Djinn formula/type
   declarations and Exference function/deconstructor/class bindings.
9. Remove Exference's parallel name, type-declaration, class, binding, and
   deconstructor collection maps.

Required tests:

- both adapters round-trip their supported subsets;
- structural list/function/tuple spellings have one canonical representation;
- forall substitution protects body and constraint binders;
- duplicate qualified declarations are rejected while `A.T` and `B.T` remain
  distinct;
- type/class and function/method namespace rules are explicit;
- synonym and datatype SCC paths are deterministic;
- class/query arguments share one kind-variable scope;
- Exference's unknown-constructor policy is tested separately from Djinn's
  rejection policy.

### 5. Shared session and frontend orchestration

- Payoff: medium
- Risk: medium and dependent on the preceding abstractions

Only after types, declarations, candidates, diagnostics, and completion status
are structured uniformly should a shared session own:

- the validated source environment;
- backend-specific elaborated caches;
- query parsing/elaboration;
- search configuration;
- candidate selection and checked simplification;
- rendering policy and diagnostics.

At that point both executables can become thin adapters, and Exference's
candidate-selection helpers and Djinn's CLI-local report printing can move
below a consistent presentation boundary.

## Machinery that must remain backend-specific

The following are not duplicated abstractions and should not be forced into a
common implementation:

- Djinn's `Formula`, `Term`, constructor descriptors, LJT antecedent indexes,
  proof normalization, and terminating LJT search;
- Djinn's independent proof checker, including sum/product/injection and empty
  eliminator constraints;
- Exference's flexible/rigid polymorphic unifier, offset namespace handling,
  typed holes, scoped search nodes, heuristic priority queue, and function
  ratings;
- Exference's instance-resolution policy and constraint relaxation;
- backend-specific scoring, enumeration order, completeness, and termination
  claims;
- the current parser implementations until both have a common elaboration
  target;
- backend typecheckers, even after both consume the shared generated-code AST.

In particular, a generic unifier would combine three different systems:
Djinn kind unification, Djinn proof-type checking, and Exference polymorphic
type unification. Their occurs checks, rigid variables, constraints, and result
contracts differ. Sharing them now would increase coupling and obscure
soundness boundaries.

## Staged commit and migration order

Each stage below is intended to be independently green and worth committing
and pushing with a detailed multi-line explanation before beginning the next.

### Stage 1: harden Exference's checked output boundary

1. Add regressions for post-check simplification invalidity, global capture,
   and rendered binder collision.
2. Remove or capability-gate the `id` and `(.)` rewrites.
3. Add a collision-free, reserved-name-aware allocator to both HSE conversion
   and textual expression display.
4. Run the independent checker on the exact transformed expression that will
   be rendered.

This stage fixes real bugs without waiting for a common AST.

### Stage 2: repair Exference type and unifier invariants

1. Add the successful-unification equality property.
2. Cross-zonk or reimplement symmetric unification over tagged sides.
3. Add offset/right-offset projection properties.
4. Instantiate every outer forall with disjoint rigid IDs.
5. Make `showHsType` use one binder-name map and omit empty contexts.

### Stage 3: make search completion truthful

1. Introduce truncation reasons for step, queue, and depth limits.
2. Update existing queue/depth tests to reject `SearchExhausted` after pruning.
3. Preserve validation and completion in checked APIs.
4. Stop the CLI after invalid input and eliminate `last []`.
5. Make no-result presentation distinguish finished from undecided.

### Stage 4: share constraints and normalize class environments

1. **Complete:** add `Language.Haskell.Synthesis.Constraint`.
2. **Complete:** remove recursive `HsTypeClass` values from `HsConstraint` and
   `HsInstance`.
3. **Complete:** validate class arity, names, variables, cycles, and exact
   duplicate declarations.
4. **Complete:** migrate superclass inflation and instance solving to explicit
   environment lookup.
5. **Pending:** adapt Djinn `Context` without changing its resolution semantics.

### Stage 5: share generated code

1. Add the common AST, scope validator, simplifier kernel, and renderer.
2. Pin adapters with differential goldens and GHC validation.
3. Migrate Exference output/checking first.
4. Migrate Djinn proof-term conversion and output second.
5. Return structured candidates from both stable APIs.

### Stage 6: share operational search envelopes

1. Add common completion/truncation and candidate-batch types.
2. Adapt both APIs without collapsing backend-specific evidence.
3. Move lossy compatibility helpers behind explicitly named adapters.

### Stage 7: share types, declarations, kinds, and validation

1. Add the common type and declaration IR plus lossless adapters.
2. Build the opaque validated environment.
3. Make both parsers target it.
4. Extract and generalize Djinn's kind checker.
5. Lower separately to both backend environments.
6. Remove backend-duplicated source declaration bookkeeping.

### Stage 8: share sessions and retire duplicate CLI policy

1. Add a reusable session with backend selection and validated caches.
2. Move environment loading, query elaboration, candidate selection,
   simplification, and rendering into that layer.
3. Thin both executables to argument/REPL adapters.

### Stage 9: cross-backend differential tests

Use the monomorphic propositional overlap: arrows, tuples, finite non-recursive
algebraic data, and monomorphic assumptions. Configure Exference to permit
unused arguments where Djinn does. For every candidate from either backend:

1. validate common AST scopes;
2. render in a generated module with supporting declarations;
3. run `ghc -fno-code`;
4. compare only typeability and operational completion, not term spelling or
   enumeration order.

For a curated small corpus, require both engines to find at least one checked
candidate within a pinned Exference budget. Never infer Exference
uninhabitability from a truncated search, and never weaken Djinn's proof-backed
negative result to match Exference's operational status.

## Conclusion

The eventual merged library should share its language-facing model and checked
output pipeline while preserving two recognizably different synthesis
backends. The best integration path is incremental and evidence-driven:
repair the currently unsafe Exference output and status boundaries, normalize
constraints, then share generated code, search envelopes, and finally the
source type/declaration environment. This order fixes observable defects early
and creates stable adapter seams before changing either engine's core search
representation.
