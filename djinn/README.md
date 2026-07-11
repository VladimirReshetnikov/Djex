# Djinn

Djinn generates a Haskell expression from its type. It reads a small Haskell-like
declaration language, translates types through the Curry–Howard correspondence,
and uses a terminating proof search for intuitionistic propositional logic to
construct a proof term. Each candidate is independently type-checked, converted
through a total error-reporting boundary, simplified, and printed as Haskell.

This directory is a reviewed local import of
[`djinn-2025.2.21`](https://hackage.haskell.org/package/djinn-2025.2.21), based on
the [upstream `augustss/djinn` repository](https://github.com/augustss/djinn).
The original one-line import note has been expanded here because upstream ships
its user guide inside the executable rather than as a README.

## Build and run

The package is an independent Cabal project and is tested with GHC 9.12.4 and
Cabal 3.16.1.0:

```console
cd djinn
cabal build
cabal test
cabal run djinn
```

Djinn can also execute one or more command files non-interactively:

```console
cabal run djinn -- examples.djinn
```

Each non-comment line in a command file is one REPL command. `--` starts a line
comment. Files are processed from left to right in the same evolving environment.

## Testing

Run the complete test matrix with independently reported test names:

```console
cabal test all --test-show-details=direct
```

| Suite | Scope |
| --- | --- |
| `djinn-tests` | 23 focused Tasty/HUnit regressions over parsing, kinds, proof search/checking, rendering, environments, and identifiers. |
| `djinn-property-tests` | Three QuickCheck properties, 200 generated cases each, covering proof production/checking/rendering, arbitrary identity, and `HType` display/parser round-trips. |
| `djinn-cli-tests` | Seven subprocess scenarios against the packaged executable, including EOF, diagnostics, mutation rollback, and stateful query behavior. |

Each suite can be selected independently, and Tasty patterns can isolate one
named test. For example:

```console
cabal test djinn-property-tests --test-show-details=direct
cabal test djinn-tests --test-options='-p /nominal empty/'
```

The core and frontend live in the named internal `djinn-core` library, so tests
and the executable consume one authoritative compilation. HPC coverage is
available for the in-process unit and property suites:

```console
cabal test djinn-tests djinn-property-tests --enable-coverage
```

The CLI suite is intentionally run without HPC: it launches multiple copies of
the instrumented executable, whose shared `.tix` file would conflate processes.

## Quick tour

Ask for an implementation by putting `?` between a name and a type:

```text
Djinn> identity ? a -> a
identity :: a -> a
identity a = a

Djinn> swap ? (a, b) -> (b, a)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

Djinn> impossible ? a -> b
-- impossible cannot be realized.
```

The alternative query spelling `? name :: type` is equivalent. Djinn searches
for total, terminating inhabitants: `undefined`, exceptions, and general
recursion are deliberately outside its logic.

The initial type environment contains `()`, `Bool`, `Either`, `Maybe`, `Void`,
and the synonym `Not a = a -> Void`. Tuple and function types are intrinsic;
list syntax is accepted as an abstract type constructor. The initial class
environment contains small `Eq` and `Monad` declarations.

### Add declarations

Function declarations make monomorphic building blocks available to proof
search:

```text
Djinn> type Input :: *
Djinn> type Middle :: *
Djinn> type Output :: *
Djinn> first :: Input -> Middle
Djinn> second :: Middle -> Output
Djinn> pipeline ? Input -> Output
pipeline :: Input -> Output
pipeline a = second (first a)
```

An assumption may use a qualified external name, allowing emitted code to retain
an imported reference such as `Data.Function.id`. Generated query, class, and
method names are deliberately unqualified because they introduce local bindings.
Names follow Haskell lexical rules: leading underscores are accepted, reserved
words and reserved operators are rejected, and operators are written in
parentheses in prefix positions.

`type T :: kind` declares an abstract type constructor. This is the right way to
introduce an opaque type such as `Int`; `data T` instead declares an empty type.
Ordinary synonyms and finite algebraic data types use familiar syntax:

```text
type Pair a = (a, a)
data Choice a b = This a | That b
data Empty
```

Recursive type declarations are rejected because the logical translation
expands data types structurally.

### Type-class contexts

A context is interpreted as an extra collection of available class methods:

```text
Djinn> reflexive ? Eq a => a -> Bool
reflexive :: (Eq a) => a -> Bool
reflexive a = a == a

Djinn> ?instance Monad Maybe
instance Monad Maybe where
   ...
```

This is not Haskell instance resolution. Djinn neither imports the installed
package environment nor instantiates arbitrary polymorphic methods. Classes and
methods needed beyond the small initial environment must be declared explicitly.

## Command reference

Commands may be abbreviated to an unambiguous prefix.

| Command | Effect |
| --- | --- |
| `name ? [context =>] type` | Search for an inhabitant. |
| `? name :: [context =>] type` | Equivalent query syntax. |
| `name :: type` | Add a monomorphic axiom/function. |
| `type T vars = type` | Add or replace a type synonym. |
| `type T :: kind` | Add or replace an abstract type constructor. |
| `data T vars = C types | ...` | Add or replace a finite data type. |
| `data T vars` | Add an empty data type. |
| `class C vars where methods` | Add or replace a class declaration. |
| `?instance [context =>] C types` | Generate every method of an instance. |
| `:environment` | Print types, axioms, and classes in scope. |
| `:delete name` | Delete a declaration from the environment. |
| `:clear` | Restore the initial environment and settings. |
| `:load file` | Execute a command file. |
| `:set option` | Change a search/output option. |
| `:help` / `:verbose-help` | Print short or extended help. |
| `:quit` | Exit. |

Methods in a `class` declaration are separated with semicolons. Kinds use `*`
and right-associative `->`, for example `type F :: * -> *`.

Type replacement and deletion are transactional. Djinn rebuilds inferred kinds
and revalidates every remaining synonym, axiom, and class method; a mutation that
would leave a dangling or ill-kinded dependency is rejected without changing the
environment.

### Settings

The Boolean settings use `+name` to enable and `-name` to disable them:

| Setting | Default | Meaning |
| --- | ---: | --- |
| `multi` | off | Print alternative, de-duplicated solutions. |
| `sorted` | on | Rank by the fraction of unused binders, then binder count. |
| `debug` | off | Print the translated formula and internal proof term. |
| `cutoff=N` | `200` | Consider at most positive `N` proof candidates. |

For example:

```text
:set +multi
:set cutoff=20
```

The same Boolean options can precede file names on the command line, such as
`cabal run djinn -- +multi examples.djinn`.

## How the code is organized

| Module | Responsibility |
| --- | --- |
| `app/Main.hs` | Thin executable launcher. |
| `Djinn` (`src/Djinn.hs`) | Frontend state, command parser, and query orchestration. |
| `REPL` | Haskeline loop and EOF handling. |
| `HCheck` | Kind inference and validation for declared Haskell-like types. |
| `Environment` | Transactional rebuilding and validation of stored declarations. |
| `HIdentifier` | Shared Haskell identifier, qualification, and operator rules. |
| `HTypes` | Type parser, logical translation, proof-term conversion, simplification, and pretty-printing. |
| `LJTFormula` | Formula and proof-term data types. |
| `LJT` | Dyckhoff-style contraction-free proof search and proof normalization. |
| `ProofEnv` | Isolation of external proof identities from printable names. |
| `ProofCheck` | Independent type checking of generated proof terms. |
| `Help` | Extended in-program help. |

These modules form the internal `djinn-core` library. Keeping the launcher in a
separate source directory prevents Cabal from recompiling imported core modules
as executable home modules.

The central pipeline is:

```text
command -> HType -> kind check -> Formula -> LJT proof search
        -> proof-term normalization -> independent proof check
        -> scope-safe Haskell conversion -> Haskell AST cleanup -> printed clause
```

## Important limitations

- Djinn implements propositional intuitionistic reasoning, not the full Haskell
  type system. It has no higher-rank types, GADTs, type families, constraints
  imported from packages, or general type-class solver.
- Added functions are used at exactly their declared type; their polymorphic
  type variables are not freshly instantiated at each use.
- Keep type synonyms fully saturated. Djinn does not yet reject every
  unsaturated synonym use that GHC will reject.
- Recursive data types cannot be declared structurally. List types are therefore
  treated opaquely rather than expanded into `[]` and `(:)`.
- Empty data types are logically false but retain nominal tags. Identity is used
  only for the same empty type; conversion to another result uses explicit empty
  elimination.
- The simplifier assumes total semantics. Reordering or eliminating pattern
  matches need not preserve Haskell's behavior in the presence of bottoms or
  `seq`.
- Empty-type elimination is printed as `void value`. Generated code using it
  needs an appropriate eliminator, for example one backed by an empty case or
  `Data.Void.absurd`.
- Proof search is a decision procedure, but the search space and the number of
  inhabitants can grow rapidly. Keep `cutoff` modest when requesting multiple
  or sorted solutions.
- Proof terms are independently checked, but generated clauses are not passed
  through GHC automatically. Treat the output as a strong candidate that still
  belongs in the normal compile/test loop.

See [`docs/reports/`](docs/reports/) for the local source review, fixed defects,
remaining engineering risks, and validation details.

## License and provenance

The imported source is Copyright 2005–2014 Lennart Augustsson and is distributed
under the BSD 3-Clause license in [`LICENSE`](LICENSE). Local changes retain that
license. The imported version was downloaded from:

<https://hackage.haskell.org/package/djinn-2025.2.21/djinn-2025.2.21.tar.gz>
