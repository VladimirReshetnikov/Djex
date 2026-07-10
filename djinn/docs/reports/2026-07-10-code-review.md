# Djinn source code review

- Date: 2026-07-10
- Reviewed Djinn baseline: `7eac947` (`Add djinn/LICENSE`)
- Integrated `origin/main` tip before commit: `e6b0097`
- R-01 through R-06 remediation commits: `9b3e383` through `6f089d5`
- Imported release: `djinn-2025.2.21`
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0, Windows 11

## Executive summary

This review covered every module under `djinn/src/`, the Cabal package, the
interactive and batch interfaces, and the absence of automated tests. The source
at the start of the review matched the Hackage 2025.2.21 source byte-for-byte.
Upstream history and open issues were also checked, most importantly the
higher-kinded `IntMap.!` crash reported as upstream issue
[#6](https://github.com/augustss/djinn/issues/6).

The review found two critical proof-freshness failures. Caller-controlled names
could collide with proof-search binders or with the synthetic atom used to encode
disjunction choice. One collision produced ill-typed Haskell; another made Djinn
claim an unrelated disjunction was provable. Both are fixed by seeding fresh-name
generation with every caller term/formula symbol and tracking generated names in
a `Set`.

The review also fixed the historical higher-kinded crash, two proof-term to
Haskell conversion errors, several unsafe prefix comparisons, a zero-cutoff
crash, broken intrinsic list/function-constructor syntax, silently truncated
class contexts, EOF looping, unchecked declarations, and multiple smaller CLI
and packaging defects. A new Cabal regression suite locks down the core failures.

The proof calculus itself remains compact and recognizable. Follow-up work closed
R-01 through R-06 with isolated proof identities, an independent proof checker,
nominal empty propositions, transactional declaration validation, total and
scope-safe proof rendering, and centralized Haskell lexical rules. The principal
remaining risks are search fairness, the intentionally shallow class model,
batch diagnostics, public raw AST constructors, totality assumptions, and
unsaturated type synonyms.

## Scope and method

The review used five complementary passes:

1. Read the repository guidance, package metadata, all 1,980 original source
   lines, and the embedded verbose help.
2. Downloaded the exact Hackage tarball and cloned upstream Git history to verify
   provenance and inspect known issues and prior fixes.
3. Built the untouched release with aggressive warnings and reproduced suspected
   defects against that baseline executable.
4. Reviewed modules along three seams: type/kind handling, LJT proof search, and
   parser/REPL orchestration. Every partial function and stale commented block was
   inventoried.
5. Added focused regression tests, warning-clean builds, CLI smoke tests, package
   checks, and source-distribution checks.

The review deliberately did not enable the dormant proof-ordering heuristic or
replace the list nondeterminism monad. Either change can alter first-result latency
and termination behavior, so it needs a benchmark corpus rather than a cleanup
commit.

## Architecture

| Module | Responsibility | Review result |
| --- | --- | --- |
| `Djinn` | CLI, command parser, mutable environment, query orchestration | Validation and error paths strengthened; duplicated state-update logic simplified. |
| `REPL` | Haskeline loop | EOF/interrupt handling fixed; obsolete implementation removed. |
| `HCheck` | Kind inference/unification and declaration ordering | Higher-kinded state leak fixed; intrinsic kinds and total errors added. |
| `Environment` | Pure declaration-graph transactions | Type mutations now revalidate every dependent declaration atomically. |
| `HIdentifier` | Haskell lexical validation | Parser and printer now share identifier, qualification, and operator rules. |
| `HTypes` | Haskell-like types, parsing, logical translation, Haskell AST cleanup/printing | Conversion is total and alpha-renames external terms; responsibilities remain broad. |
| `LJTFormula` | Formula and proof-term representation | Empty propositions preserve nominal identity; display is total. |
| `LJT` | Dyckhoff-style proof search and proof normalization | Freshness soundness fixed; dead tracing/heuristic code removed; invariants guarded. |
| `ProofEnv` | External proof-identity isolation | Self-reference collisions are excluded and display names restored after checking. |
| `ProofCheck` | Independent proof-term checking | Every selected proof is unified with the requested formula before rendering. |
| `Help` | Extended built-in manual | Incorrect examples, option descriptions, command spelling, and typos corrected. |

The operational pipeline is:

```text
command
  -> HType parse
  -> kind check
  -> intuitionistic Formula
  -> LJT proof search
  -> proof-term normalization
  -> independent proof check
  -> external-name restoration
  -> scope-safe Haskell conversion
  -> Haskell expression cleanup
  -> pretty-printed clause
```

The two historically delicate boundaries are `HType -> Formula` (where nominal
Haskell types become propositions) and `Term -> HExpr` (where proof combinators
become surface Haskell patterns and cases). Nominal empty tags, independent proof
checking, alpha-renaming, and explicit conversion errors now guard both seams.

## Fixed findings

### F-01 — Critical: caller proof symbols were captured

Baseline reproduction:

```text
Djinn> x2 :: a
Djinn> f ? b -> a
f :: b -> a
f a = a
```

The result is ill-typed: the available `x2 :: a` should produce a constant
function, but the generated binder `x2` captured the caller's free term during
normalization.

`newSym` previously started from suffix 1 without knowing caller names. `prove`
now reserves environment names and formula atoms before search, and the proof
state records every generated `Symbol` in a `Data.Set`. Substitution also honors
ordinary shadowing. The corrected output is:

```text
f :: b -> a
f _ = x2
```

This has a permanent rendering regression test.

### F-02 — Critical: a synthetic disjunction atom could collide with an input atom

Djinn proves a disjunction by introducing a fresh continuation proposition named
with an underscore prefix. On the baseline, the environment assumption
`u : _2` collided with that proposition and incorrectly proved `a | b`.

Fresh-name reservation now covers formula atoms as well as proof variables. The
regression asserts that:

```haskell
prove False [(Symbol "u", PVar (Symbol "_2"))] (a |: b) == []
```

### F-03 — Critical/high: higher-kinded declarations leaked local unification IDs

`ground (KVar i)` returned a solved composite kind without recursively grounding
the variables inside it. `clearState` then reused/emptied the local `IntMap`, so a
later check followed stale `KVar` IDs and crashed.

Minimal baseline reproduction:

```text
type Foo f a = f a
f ? Foo Maybe Bool -> Foo Maybe Bool
```

The second line aborted at `IntMap.!: key 1 is not an element of the map`. This is
the minimized form of upstream issue #6. `follow` and `ground` now recursively
resolve mappings; missing IDs return a descriptive `Left` rather than throwing.
`Foo` is persisted with the fully ground kind:

```text
(* -> *) -> * -> *
```

The exact upstream `Arrow (Kleisli m)` example now completes with controlled
"cannot be realized" results rather than crashing.

### F-04 — High: ordinary synonyms could have a non-`*` result kind

The checker accepted declarations such as `type Endo a = (->) a`. That is not a
valid Haskell98 synonym body, and exact-arity expansion subsequently treated
overapplications as unrelated opaque atoms. Ordinary synonym/data bodies are now
constrained to `KStar`; only explicit declarations such as
`type F :: * -> *` may introduce a higher-kinded abstract constructor.

### F-05 — High: `Csplit` applied residual arguments backwards

When a normalized tuple split still had arguments after the split operation,
`termToHExpr` used `foldr HEApply`. For arguments `[a, b]` this constructs
`a (b splitResult)` rather than `splitResult a b`. It now uses left-associated
application and has a direct proof-term rendering regression.

### F-06 — High: `Ccases` silently discarded residual arguments

`zipWith cAlt handlers constructors` consumed only the constructor handlers and
dropped every later argument in the flattened application spine. Conversion now
splits exactly the required handler count, constructs the case, and reapplies the
remaining arguments in order. A regression checks both alternatives and the
residual suffix.

### F-07 — Critical/high: tuple normalization reversed constructed payloads

The LJT normalizer's `viewTuple` descended an application spine by prepending
each argument after the recursive call. A direct white-box reduction therefore
turned:

```text
split2 (\a b -> f a b) (Tuple2 x y)
```

into `f y x`. The traversal now carries an accumulator while descending toward
`Ctuple`, producing `[x, y]` in source order. This is distinct from F-05: F-07
reordered the tuple payload during proof normalization, while F-05 reordered
arguments left over after Haskell-AST conversion.

### F-08 — High: prefix-only comparisons could justify invalid simplification

Several uses of `zipWith` failed to compare list lengths:

- tuple pattern/expression equality;
- `HECase` alpha-equivalence;
- tuple-pattern merging.

A shorter prefix could therefore compare equal to a longer structure, allowing a
case to collapse incorrectly. All three sites now require equal lengths first.
Formula pretty-printing and tuple display were also made total for their empty
representations.

### F-09 — High: `cutoff=0` reached a partial pattern

The setting accepted zero, `take 0` erased a known nonempty proof list, and the
binding `e:es` failed. The parser now accepts only positive values bounded by
`maxBound :: Int`, and query processing preserves the already matched head proof.
Invalid values produce a normal parse error rather than an exception.

### F-10 — High: class contexts silently truncated arguments

`ctxLookup` substituted with `zip` but never compared arities. For example:

```text
f ? (Eq a b) => a -> Bool
```

was accepted and emitted `a == a`, silently ignoring `b`. Context and instance
lookups now require exact arity and kind-check every instantiated method type
before output begins. The command reports:

```text
Error: Class Eq expects 1 type argument(s), but got 2
```

### F-11 — High: parsed intrinsic type syntax failed kind checking

The type grammar explicitly accepted `[a]` and `(->) a b`, but `HCheck` knew no
kinds for `[]` or `->`. Both therefore failed as undefined types. Intrinsic kinds
are now part of the checker, and fully applied prefix arrows are canonicalized to
`HTArrow`. List types remain intentionally opaque propositions because recursive
datatype expansion is outside the architecture.

### F-12 — High: REPL EOF looped forever

Haskeline returns `Nothing` at EOF; the old branch immediately called `loop`
again. Piped input and Ctrl-D could therefore spin indefinitely. EOF now calls the
exit hook and terminates. Interrupts are handled per iteration while retaining the
previous state. A 22-line obsolete readline implementation was deleted.

### F-13 — Medium: declarations accumulated invalid or duplicate state

Type/class redefinitions previously prepended a second binding indefinitely.
They now replace the prior declaration. The frontend also rejects duplicate:

- type parameters;
- class parameters;
- class method names within a declaration or already owned by another class;
- constructors within one data declaration;
- constructors already owned by another data type (which previously allowed
  identically labelled formulas for nominally distinct types and ill-typed
  identity coercions such as `Bool -> MyBool`);
- type and class declarations trying to reuse the same type-constructor name.

Class method types are kind-checked at declaration time. `data Foo =`, which was
an accidental second spelling for an empty type, is rejected; documented
`data Foo` remains valid.

### F-14 — Medium: file I/O errors escaped the command loop

Missing, unreadable, or lazily failing `:load` files used to terminate the
process. File reading and evaluation now run under `tryIOError`; errors are
reported and the prior state is returned.

### F-15 — Medium: accepted and documented command languages disagreed

- `:verbose-help` was documented but did not parse; it is now canonical, with
  `:verboseHelp` retained as a compatibility alias.
- Zero-parameter classes could be declared but `?instance Marker` required at
  least one type argument; it now succeeds.
- Help omitted `debug`, described `cutoff` incompletely, contained an invalid
  `Int`/`Char` example, and had several stale spellings and version strings.
- `:clear` now explicitly says that it restores settings as well as declarations.

### F-16 — Medium/low: internal totality and readability

The following cleanup was applied without changing valid proof search:

- malformed tuple/case normalization now validates arity, constructor metadata,
  and injection index rather than indexing with `!!`;
- list selection is structurally recursive rather than length/index/error based;
- `getHTVars` handles unions and abstract types;
- unit types, kinds, and abstract declarations render in reloadable syntax;
- nested `HTAbstract` kind checking is total;
- impossible kind-map and graph lookups return `Left`;
- `undefined` kind placeholders were replaced by a documented `rawType` helper;
- disabled trace wrappers, a stale alternative proof-search block, a dormant
  misleading heuristic, duplicate imports, and broad/empty imports were removed;
- helper names now describe logical roles (`reduceAntecedent`, `reduceImp`,
  `goalMayBeReachable`, `curryTuple`, `collapseCase`).

### F-17 — Packaging and maintainability

- Added an `exitcode-stdio-1.0` regression suite without new dependencies.
- Enabled `-Wall -Wcompat` for executable and tests.
- Removed the unused `array` dependency.
- Corrected Cabal's package description.
- The banner now derives its version from generated `Paths_djinn` rather than a
  2011 hard-coded string.
- Expanded `djinn/README.md` with build instructions, examples, commands,
  architecture, semantics, and limitations.
- Updated the repository layout documentation to include this third Cabal
  project.

## Module-by-module notes

### `Djinn.hs`

The main improvement is moving validation to state-update boundaries. `replace`,
`requireDistinct`, `checkMethods`, and `checkContexts` make command handlers
shorter and keep invalid definitions out of normal query paths. Query result
handling no longer relies on a warning-suppressed nonempty pattern. Help columns
are computed rather than maintained manually.

The module is still responsible for process startup, state, parsing, validation,
query scoring, rendering orchestration, and batch loading. Separating a pure
command evaluator from the IO shell would make exit status, file diagnostics,
and parser tests much easier.

### `HCheck.hs`

The union-find-like kind state is small and readable after centralizing `follow`.
Recursive grounding is the essential correctness fix. Intrinsic grammar types
now have one explicit kind table. Proper-type enforcement restores Haskell98
synonym semantics.

`htCheckEnv` is exported but still assumes unique graph keys; `Main` now enforces
that invariant. A future public-library API should reject duplicates itself and
hide raw `KVar` construction.

### `HTypes.hs`

The module contains four distinct layers: parser, kind/type representation,
logical translation, and a bespoke Haskell surface AST with optimization and
pretty-printing. Local helper cleanup improved readability, but this remains the
largest concentration of representation assumptions.

The comments now make the global-freshness dependency explicit. Length checks
and residual-application fixes close concrete correctness holes. The remaining
partial conversions are discussed below.

### `LJT.hs` and `LJTFormula.hs`

The proof search remains the contraction-free LJT implementation. Removing
always-disabled tracing made the recursive structure substantially easier to
follow. Fresh-name state now carries `(nextSuffix, usedSymbols)` rather than just
an integer. Valid search order is unchanged.

The old `heuristics = True` branch actually bypassed all heuristic code, contrary
to its name and comments. That unreachable experiment was removed rather than
silently activated.

Across these two modules the result is nine fewer lines despite the added
freshness checks, metadata validation, and explanatory comments.

### `REPL.hs` and `Help.hs`

`REPL.hs` is now a compact Haskeline loop with explicit outcomes for command,
interrupt, quit, and EOF. `Help.hs` remains a large escaped string, but it now
matches the parser and settings. The expanded README should be treated as the
maintainer-facing documentation; built-in help remains the offline user guide.

### Change footprint

Raw physical source size moved from 1,980 to 2,101 lines. The proof-search pair
(`LJT`/`LJTFormula`) is nine lines shorter and `REPL` is 23 lines shorter; the net
increase is concentrated in boundary validation, controlled error paths,
freshness state, and clarifying comments. The review also adds a 242-line test
harness, a 209-line user README, and this detailed report. In other words, dead
and duplicated implementation was shortened where safe, while formerly implicit
correctness invariants were made explicit rather than optimized for a negative
line-count diff.

## Resolved follow-up findings

### R-01 — Resolved: emitted definitions self-referenced same-named assumptions

Current reproduction:

```text
Djinn> token :: a
Djinn> token ? a
token :: a
token = token
```

`ProofEnv` now removes target-named assumptions before search and assigns every
remaining external assumption a fresh internal identity. It restores printable
names only after independent checking. A lone collision produces an explicit
"cannot be safely realized without a recursive self-reference" diagnostic; if a
different assumption is available, the generated RHS names that safe fallback.
This policy applies uniformly to explicit queries and generated instance methods.

### R-02 — Resolved: proof objects were not independently checked

`ProofCheck` independently infers proof-term types using metavariables and
unification. It checks variables, lambdas, applications, products, splits, sums,
injections, and empty elimination, validates constructor metadata and indices,
and rejects ambiguous or legacy `Xsel` terms. Every bounded candidate selected by
the query is checked against its requested formula before name restoration or
rendering. Tests both validate representative LJT output and forge malformed
terms that must be rejected.

### R-03 — Resolved: distinct empty types collapsed to one proposition

This was the root of upstream issues
[#2](https://github.com/augustss/djinn/issues/2) and
[#3](https://github.com/augustss/djinn/issues/3). `Formula` now has a tagged
`Empty` proposition. Expansion retains the datatype application that introduced
the empty type while aliases inherit the underlying tag. LJT uses identity only
for the same tag and emits `Ccases []` for elimination into a different type; the
checker permits that eliminator for every empty tag but rejects direct nominal
casts. Thus `EmptyA -> EmptyA` renders as identity and `EmptyA -> EmptyB` as
explicit `void`.

### R-04 — Resolved: environment mutation staled dependent declarations

`Environment.validateEnvironment` rebuilds every inferred kind, then checks every
stored axiom and class method against the rebuilt type graph. Type replacement
and `:delete` construct a candidate state and install it only after this entire
transaction succeeds. Diagnostics identify the dependent synonym, axiom, or
method; rejected deletion and arity-changing replacement leave the prior state
unchanged.

### R-05 — Resolved: `Term -> HExpr` was partial and scope-sensitive

`termToHExpr` and `termToHClause` now return `Either String`. Every former
`error` in tuple destructuring, case conversion, lambda extraction, and pattern
merging is a controlled shape diagnostic; unsupported `Xsel` is reported the
same way. Before conversion, a scope-aware alpha-renamer gives all binders unique
internal names disjoint from free assumptions, restoring the invariant required
by the Haskell-AST simplifiers even for externally constructed terms. The query
prints conversion failures without terminating the REPL.

### R-06 — Resolved: accepted identifiers did not match Haskell lexical rules

`HIdentifier` is now the shared lexical authority for parser and printer. It
recognizes Haskell variable and constructor identifiers, leading underscores,
well-formed qualified names, the full ASCII symbol alphabet, and Unicode symbol
operators, while rejecting reserved identifiers/operators, constructor
operators as binding names, empty module segments, lowercase module segments,
and trailing dots. Generated declaration names stay local; external assumptions
and type constructors may be qualified. Printer decisions use the same
predicates, fixing the previous misrendering of `_name` and qualified variables.

## Remaining findings and recommendations

### R-07 — Medium: list nondeterminism is left-biased and unbudgeted

An expensive earlier proof branch can starve later branches. `MoreSolutions=True`
also grows cross-products quickly, and the user cutoff is applied to the lazy
result stream rather than carried as a search budget. `AtomFs`, `AtomImps`, and
`NestImps` are list-backed, so lookup, deduplication, and removal are linear (or
quadratic across all alternatives).

Introduce a named search mode and explicit fuel/result budget, then benchmark
fair interleaving and indexed structures against a representative formula corpus
before changing proof order.

### R-08 — Medium: the class model is intentionally shallow

There is no superclass or instance database, imported Prelude/package
environment, or true fresh instantiation of polymorphic methods. Phantom class
parameters have no inferable or stored kind. In particular:

```text
class Empty a where
bad ? Empty (Bool a) => b -> b
```

is accepted even though `Bool a` is ill-kinded, because an empty method list
provides nothing for `checkMethods` to inspect. The fix is to infer/default and
store every class parameter kind at declaration time, then check context and
instance arguments against that signature (`Monad m` prevents simply assuming
all parameters have kind `*`). This is a semantic boundary rather than a local
frontend check and should remain prominent in user documentation.

### R-09 — Medium/low: batch command outcomes need structure

`(Bool, State)` distinguishes only quit from continue. A missing command-line
file is reported but still exits with status 0; parse errors lack filename/line
and processing continues. `:load` has no quoted paths, nested paths resolve from
the process working directory, and there is no `--` option terminator for
filenames beginning with `+` or `-`.

Replace the Boolean with a command-result type carrying success/failure, source
location, and continuation policy.

### R-10 — Low: API and display invariants remain public

`Formula(..)`, `Term(..)`, `HType(..)`, and `HKind(..)` permit negative arities,
bad injection indices, duplicate constructor descriptions, dangling `KVar`s,
and unchecked cyclic synonym environments. Some `Show` forms (singleton tuples
and empty unions) do not round-trip through the parser. These are acceptable for
an executable-internal AST but not a robust library API.

### R-11 — Semantic caveat: simplification assumes totality

Case/lambda transformations and unused-scrutinee removal are valid in the total
intuitionistic model but can change strictness in real Haskell with `undefined`,
exceptions, or `seq`. This is now documented in the README and should remain an
explicit design assumption.

### R-12 — Medium: type-synonym saturation is not enforced at use sites

F-04 ensures that synonym declarations produce proper types, but the kind
environment records only the resulting kind, not whether a constructor is a
synonym with a mandatory arity. Djinn therefore accepts:

```text
type Pair a b = (a, b)
type HK :: (* -> *) -> *
bad ? HK (Pair a) -> HK (Pair a)
```

GHC rejects the unsaturated `Pair a`, whereas abstract/data constructors may
legitimately be partially applied. Enforcing this distinction requires carrying
constructor category and synonym arity through kind checking, not merely adding
another kind constraint.

## Automated regression coverage

`test/Spec.hs` is a lightweight, dependency-free harness with named groups for:

1. prefix function-constructor parsing/canonicalization;
2. intrinsic list parsing, display, and kind checking;
3. canonical unit and kind rendering;
4. recursive higher-kinded grounding and reuse;
5. rejection of ill-kinded higher-order application;
6. rejection of non-proper synonym bodies;
7. representative intuitionistic theorems;
8. representative non-theorems, including Peirce's law;
9. preservation of named assumptions;
10. caller proof-symbol capture;
11. synthetic disjunction-atom capture;
12. `Csplit` residual argument order;
13. `Ccases` residual argument preservation and order;
14. external proof-identity isolation and self-reference prevention;
15. independent checking of representative generated proofs;
16. rejection of forged malformed proof terms;
17. nominal empty types, aliases, identity, and explicit elimination;
18. transactional validation of dependent declaration graphs;
19. capture-free rendering of externally shadowed terms;
20. controlled conversion errors for malformed proof shapes;
21. Haskell identifier, qualification, reserved-token, and operator rules.

The suite intentionally compiles the core modules directly. Pure environment,
proof-checking, conversion, and lexical boundaries are therefore covered without
driving terminal IO. A future split of the remaining command evaluator from
`Main` should add direct state-machine tests for cutoff, contexts, and batch
error statuses.

## Validation performed

The original review and R-01 through R-06 follow-up were finally revalidated
from a new build directory with:

```text
cabal build all --builddir=dist-completion-audit
cabal test all --builddir=dist-completion-audit --test-show-details=direct
cabal check
cabal sdist --builddir=dist-completion-audit
git diff --check
```

All source and test targets compile with `-Wall -Wcompat` and no warnings; all 21
named regression groups pass. `cabal check` reports no errors or warnings. The
source distribution contains the license, README, report, tests, and all new
modules (`Environment`, `HIdentifier`, `ProofCheck`, and `ProofEnv`).

The final CLI matrix additionally verified that a same-named assumption is
diagnosed instead of emitted recursively, a safe fallback is retained, same-type
empty identity differs from cross-empty elimination, invalid deletion preserves
a dependent synonym, qualified external assumptions and underscore targets
render correctly, and reserved or malformed names fail to parse. Earlier manual
checks also covered tuples, lists, prefix arrows, higher-kinded reuse, cutoff,
contexts, zero-parameter classes, file errors, verbose help, quit, and EOF.

## Conclusion

The imported Djinn core is ingenious and compact, but it relied on namespace and
shape invariants that were never made explicit. The most important outcome of
this review is that caller input can no longer corrupt proof freshness, and the
historical higher-kinded crash is gone. The new tests cover those boundaries
directly rather than only sampling friendly type queries.

The follow-up work supplies the independent proof-object checker and extracts the
pure declaration transaction boundary. The next highest-value engineering work
is an explicit proof-search budget/fairness benchmark, followed by richer class
kind signatures and structured batch-command outcomes. Those changes should be
measured carefully because they can alter search order or the accepted language.
