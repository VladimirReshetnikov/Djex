# Exference code review and simplification pass

- Date: 2026-07-10
- Reviewed baseline: `42ce0fa`
- Review branch: `codex/review-exference-codebase`
- Toolchain attempted: GHC 9.12.4, Cabal 3.16.1.0, Stack 3.11.1
- Related project: [`djinn/`](../../../djinn/README.md)

## Executive summary

This review covered every Haskell module under `exference/src/` and
`exference/src-exference/`, the package descriptions, the environment parser,
the hand-maintained environment, the historical test driver, and the reviewed
Djinn implementation with which Exference is eventually intended to merge.

The production source is 195 lines shorter even after adding comments and a
shared closure helper. Most of that reduction came from deleting roughly 180
lines of commented-out, decade-old dictionary experiments in `SimpleDict`; the
remainder removes dead imports and duplicated closure machinery. A real Cabal
test suite now records focused regressions instead of relying exclusively on the
executable's manual/performance-oriented `MainTest` mode.

The review fixed eleven local defects:

1. superclass and inflated-instance traversal repeatedly expanded the whole
   frontier and could not recognize a fixed point;
2. type classes were compared structurally, which makes mutually recursive
   class definitions diverge during comparison;
3. incrementally adding constraints rebuilt the variable index from only the
   new constraints and silently dropped old entries;
4. substitutions under `forall` protected binders in the body but not in the
   constraint context;
5. `freeVars` and `largestId` ignored variables in a `forall` context (and the
   latter also ignored explicit binders);
6. instance matching silently truncated arity mismatches through `zipWith`;
7. processed-node counts in search trees omitted every processed descendant of
   an already processed node because of `if`/`+` precedence;
8. a failed type-synonym declaration caused applications of that synonym to
   lose all their type arguments;
9. every undeclared class was represented by the same `EXFUnknownTC`, conflating
   unrelated constraints;
10. ratings parsing used partial list patterns and `read`, allowed `NaN` and
    infinities into priority calculations, and did not force lazy file reads
    inside the exception handler;
11. lookahead helpers used arbitrary numeric sentinels, so sufficiently large
    ratings or searches longer than 999,999 steps could return the wrong result.

The most important conclusion is architectural: **do not merge the two search
engines by trying to make their internal proof representations identical.**
Djinn is a terminating propositional proof procedure; Exference is a heuristic,
polymorphic, constraint-aware typed-expression search. Their reusable overlap is
above and below proof search: identifiers, source types, declarations,
environments, diagnostics, generated-expression ASTs, simplification, printing,
budgets/statistics, and verification. Preserve two search backends behind one
front end and one checked output pipeline.

## Scope and method

The review proceeded in dependency order:

1. package metadata and build baseline;
2. `Core.Types`, type utilities, substitutions, unification, and constraints;
3. expression construction, simplification, scopes, nodes, and search-tree
   accounting;
4. the main priority-queue search loop and result-selection helpers;
5. `haskell-src-exts` conversion, declarations, classes, synonyms, environment
   extraction, ratings, and rendering;
6. executable configuration and the historical test harness;
7. Djinn's corresponding type/environment/proof/rendering layers and its prior
   review reports.

Recent history was examined before changing search-related code. `exference/`
was imported unchanged at `8f0034a` (plus the paper at `64227f3`), whereas the
adjacent Djinn project has already received correctness, simplification, budget,
benchmark, and layered-test passes.

## Changes made

### F-01 — Terminating, shared transitive closure

`inflateHsConstraints` and `inflateInstances` each used a variation on
`takeWhile (not . null) . iterate ...`. This stops only when an expansion is
empty; it does not stop when expansion produces elements already seen. A class
cycle therefore repeats forever.

Both now use one work-list implementation in
`Core.Internal.Closure`. It tracks `seen` and expands only a separate frontier,
terminating at the first fixed point for every finite reachable graph.

### F-02 — Nominal class identity

`HsTypeClass` previously derived structural `Eq` and `Ord`. A superclass stores
another complete `HsTypeClass`, so recursive class declarations form cyclic
values; comparing those values recursively never reaches a constructor that can
decide equality. Class identity in Haskell is nominal anyway. `Eq` and `Ord` now
compare `tclass_name` only. This both states the intended model and makes F-01
effective for cyclic class graphs.

The recursive embedding remains unnecessarily heavy; the merge should replace
it with a class identifier plus lookup in an environment.

### F-03 — Incremental constraint indexing

`addQueryClassEnv` correctly unioned old and new constraints into `csSet`, but
built `qClassEnv_varConstraints` from free variables in the new argument only.
Adding a later `forall` context could make prior variable constraints
unreachable through the index. The index now derives its keys from all of
`csSet`, reuses the already-computed inflated closure, and has a regression that
adds constraints for two variables in separate calls.

### F-04 — Capture-avoiding traversal through `forall`

`applySubsts` deleted bound variables before visiting the body but applied the
unfiltered substitution to the constraints. For
`forall a. C a b => a`, a substitution for `a` therefore rewrote the context
while leaving the body bound — an internally inconsistent type. One filtered
map is now used for both.

`freeVars` now includes constraint parameters before removing binders, and
`largestId` includes explicit binders and context parameters. These functions
feed `forallify` and the fresh-variable offset logic, so omitting context-only
variables could cause capture or ID reuse even when the displayed result looked
reasonable.

### F-05 — Instance arity validation

The instance solver constructed equations with `zipWith`, accepting the common
prefix when a malformed constraint and instance had different arities. It now
requires equal lengths before unification. In the merged design, constructors
should make ill-arity class applications unrepresentable after elaboration.

### F-06 — Correct search-tree statistics

The processed count was parsed as:

```haskell
if processed then 1 else (0 + processedChildren)
```

rather than `(if processed then 1 else 0) + processedChildren`. A processed
parent consequently hid all processed descendants. Parentheses now make the
intended tree fold explicit, with a three-node regression.

### F-07 — Preserve applications of invalid synonyms

`applyTypeDecls` intentionally avoids repeating an error already reported for
an invalid synonym declaration. Its fallback returned only `TypeCons qn`,
however, dropping every application argument. `Bad a b` became `Bad`, changing
kind and identity and potentially affecting later search. The fallback now
recursively converts and reapplies all arguments.

### F-08 — Distinct unknown classes

All undeclared classes previously used one value named `EXFUnknownTC`. Thus
`Foo a` and `Bar a` could compare equal and satisfy each other. The fallback is
now constructed from the class's actual qualified name. This is still a
permissive parsing policy, but it is no longer unsound merely because two names
are absent from the loaded environment.

### F-09 — Total ratings parsing and deterministic discovery

Ratings now have a total pure parser with useful errors for an odd token count,
malformed floats, `NaN`, and infinities. The file is forced while still inside
`try`, so lazy I/O and decoding failures cannot escape the handler. Environment
filenames are sorted before parsing, making warnings and duplicate resolution
stable across filesystems.

### F-10 — Remove result-selection sentinels

Lookahead began with rating `99999.9`; a valid first solution with a larger
rating was ignored. The initial upper bound is now positive infinity, while the
pre-first-result step allowance uses `maxBound` instead of 999,999. Non-finite
external ratings are rejected by F-09, so infinity remains an internal sentinel
only.

### F-11 — Delete dead code and clarify simplifier assumptions

`SimpleDict` contained about 180 lines of commented-out bindings and class
experiments that had not compiled for years. It now contains only its two public
values. Dead imports were removed from that module, `Core`, `FunctionDecl`,
`TypeUtils`, and `Types`; the obsolete pre-`base-4.8` `Alt` compatibility shim
and an abandoned parser implementation were deleted.

The variable-name index now keeps plain spellings rather than parser AST nodes,
which both states the intended identity rule and starts the common-IR
decoupling. `QualifiedName`'s operator rendering was also rewritten without
partial `head`/`tail` calls.

`ExpressionSimplify` now documents two load-bearing assumptions: binder IDs are
globally unique, and generated expressions are interpreted in a total language.
Without those assumptions, its substitution and dead-let elimination are not
semantics-preserving for arbitrary Haskell ASTs.

## Test infrastructure

`test/Spec.hs` adds Tasty/HUnit regressions for:

- superclass cycles;
- incremental constraint indexing;
- nominal unknown-class identity;
- binder protection in `forall` contexts;
- context-aware free variables and largest-ID calculation;
- failed-synonym argument preservation;
- malformed and non-finite ratings;
- recursive processed-node counts.

The historical `src-exference/MainTest.hs` is not a Cabal test suite. It mixes
large expected-result tables, performance statistics, tree dumping, console
printing, and external environment assumptions, and it runs only through the
CLI. It should eventually be split into deterministic unit/golden tests and a
separate benchmark corpus, following the structure now present in `djinn/`.

## Build and compatibility audit

The imported package is not buildable as written on the repository's documented
GHC 9.12.4 toolchain.

1. A normal `cabal build` cannot solve dependencies because bounds stop around
   the 2016 ecosystem (for example, `haskell-src-exts < 1.18`, `lens < 4.15`,
   `template-haskell < 2.12`).
2. With `--allow-newer=all`, the support dependency graph builds and Cabal
   compiles the search core. The review removed one needless coupling discovered
   here: `TypeVarIndex` now stores parser-independent spellings instead of HSE
   `Name` nodes (whose source annotations must never become semantic identity).
   The remaining frontend build reaches the removed legacy `EitherT` module and
   HSE's replacement of `SrcLoc` by an annotated, parameterized AST; many parser
   constructors and the module representation also changed.
3. Forcing the original `haskell-src-exts-1.17.1` under GHC 9.12 fails inside
   that package because its pretty-printer's local `(<>)` conflicts with the
   modern Prelude operator.
4. Stack 3.11 can install the snapshot's GHC 7.10.3 toolchain, but refuses to
   build with that compiler's Cabal 1.22 because current Stack requires Cabal
   2.2 or newer. Reproducing the historical build therefore needs an old Stack
   binary or a separately curated legacy environment.
5. `cabal check` now parses the package without the former indentation warning.
   Remaining metadata warnings concern `-O2`, old executable dependency bounds,
   and trailing-zero upper bounds.

Consequently, the new tests are present but cannot yet execute on GHC 9.12
without the frontend migration described below. This is not hidden as a green
validation claim: `git diff --check` and Cabal metadata parsing succeed; both
modern and legacy parser build attempts fail for the independently reproduced
compatibility reasons above.

## Open findings

### R-01 — High: migrate or replace `haskell-src-exts`

This is the immediate integration blocker. Porting to 1.24 is possible but not
mechanical: annotations must be normalized before names become map keys, module
headers are now optional, contexts changed shape, and newer declaration forms
must be handled explicitly. Merely adding location type parameters would make
equal variable spellings at different source locations compare unequal.

The better merge-oriented option is to adopt one source-syntax frontend shared
with Djinn, elaborate immediately into a location-free common type/declaration
IR, and retain source spans only for diagnostics. `ghc-lib-parser` gives the most
faithful Haskell syntax but is heavy; a deliberately small Megaparsec parser is
reasonable if the accepted language is explicitly a subset. Continuing with
`haskell-src-exts` is viable only as a short-term compatibility step.

### R-02 — High: `input_allowConstraintsStopStep` is dead configuration

The field claims that constraints will stop being ignored after a given search
step, but `findExpressions` binds it as `_allowConstraintsStopStep` and never
uses it. Callers can believe they requested a soundness/performance boundary
that has no effect. Decide the intended policy and implement it in the search
state, or delete the field in the eventual API redesign.

### R-03 — High: instance solving can recurse forever

`checkPossibleGeneric` recursively follows matching instances with no visited
set or decreasing measure. An instance cycle such as `C a => C a`, or a cycle
across classes, can diverge. The superclass closure fix does not address this
separate proof search. Use a memoized goal stack keyed by normalized class
application and distinguish "proved", "refuted", and "cycle/undecided".

### R-04 — High: higher-rank behavior is inconsistent and can be unsound

The README advertises experimental `RankNTypes`, but symmetric unification
throws on `TypeForall`, while offset and right-biased variants strip foralls and
unify their bodies. Comments explicitly say this is wrong. Rank-N subsumption
needs skolemization, instantiation, and escape checks; erasing quantifiers can
accept invalid programs. Until implemented, reject unsupported rank-N positions
with a structured diagnostic rather than mixing crashes and erasure.

### R-05 — High: generated programs are not independently checked

Search nodes build an `Expression`, which is simplified and converted to a
Haskell AST, but no independent checker validates the final term against the
query and remaining context. Djinn's `ProofCheck` demonstrates the value of a
separate checker: it caught reachable proof defects after search changes.

Exference's checker must understand instantiation and class constraints, so the
eventual strongest boundary is GHC typechecking of a generated temporary module.
A lightweight internal checker is still useful for fast invariant checks before
that boundary.

### R-06 — Medium/high: memory limiting is opaque and likely mis-keyed

The queue cutoff activates when `qsize > maxSteps`, not when it exceeds the
configured memory-limit scale. It derives a rating cutoff from the previous
worst score and silently discards states, potentially changing result order and
completeness. The public field describes memory scaling, not this condition.
Replace it with an explicit maximum queue size and a documented eviction policy;
report truncation in the result status.

### R-07 — Medium: a hard-coded depth cutoff silently prunes search

`stateStep` contains `guard . (<= 200.0) =<< use depth`. This limit is neither an
input field nor reported to callers, and "depth" is a heuristic floating score,
not structural depth. A query may therefore return no result because of an
undocumented heuristic ceiling. Make every termination resource an explicit
budget and distinguish exhausted/undecided from searched-without-results, as
Djinn now does.

### R-08 — Medium: priority and result APIs encode policy with raw `Float`

Heuristic weights, queue priorities, function ratings, solution complexity,
and sentinels are undifferentiated `Float`s. Although external non-finite values
are now rejected, programmatic callers can still construct them. Introduce
validated newtypes (`Priority`, `Penalty`) and use `Double` or exact ordered
components. Result selection should compare a named score record rather than
destructure statistics repeatedly.

### R-09 — Medium: type-synonym expansion lacks cycle detection

`getTypeDecls` ties a recursive map and recursively expands synonyms. Direct or
mutual synonym cycles can diverge, and arity errors are represented indirectly
through `Either` values in the same map. Resolve declarations with a DFS state
(`unseen`, `visiting`, `done`) and emit a cycle path. Decide and test the policy
for unsaturated synonyms explicitly.

### R-10 — Medium: output AST constructors are not syntactically faithful

`ExpressionToHaskellSrc` represents every `ExpName` as `Con`, even lowercase
functions, and constructs patterns with `Ident` even for symbolic or special
constructors. Pretty-printing can mask the wrong AST node, but downstream AST
consumers and some names will fail. A shared merged expression AST should
distinguish local variables, qualified value names, data constructors, and
operators; rendering should use the same identifier rules already centralized
in Djinn's `HIdentifier`.

### R-11 — Medium: parser failures still contain partial paths

Examples include `convertQName` calling `error` for `FunCon`, `addConstraint`
assuming a `TypeForall`, `unsafeReadType*`, and `parseQualifiedName` using
`init`/`last` without validating malformed input. Convert parser and elaborator
boundaries to `Either Diagnostic` and reserve exceptions for violated internal
invariants. Diagnostics should carry file and source span after R-01.

### R-12 — Medium: the public data model is tuple-heavy and mutation-prone

`FunctionBinding`, `DeconstructorBinding`, `EnvDictionary`, `VarPBinding`, and
`TGoal` are positional tuples. Their adjacent fields share types, making swaps
easy and call sites hard to read. Replace them with records during extraction of
the common IR. In particular, a `Binding` record can be shared between Djinn and
Exference while backend-specific elaborated forms remain separate.

### R-13 — Medium: scopes assume an undocumented acyclic graph

`scopeGetAllBindings` recursively follows parent IDs without a visited set.
Current constructors create a parent DAG, so this works if all callers preserve
the invariant; the constructors are nevertheless exported and malformed or
future merged state can loop. Hide constructors, document the invariant, and
use a checked lookup with an impossible-state diagnostic.

### R-14 — Medium: the CLI/test module boundary is inverted

The executable owns test data, test execution, rendering policy, environment
loading, CLI parsing, and query orchestration. No-argument execution runs tests,
and help still prints `TODO`. Extract a pure library/session API first, make the
CLI a thin adapter, and move examples into golden files or benchmarks. Djinn's
current internal-library plus thin-launcher organization is the useful model.

### R-15 — Low/medium: stale feature flags and debug dependencies remain

CPP flags (`LINK_NODES`, `BUILD_SEARCH_TREE`) materially change node shape and
use `unsafePerformIO`/stable names to reconstruct trees. The executable also
retains Hood, `Debug.Trace`, external pointfree tools, and parallel flags whose
branches say the parallel version is unimplemented. Remove or isolate these
before merging; observational tooling should not alter production data types.

## Djinn/Exference merge analysis

### What should be shared

| Layer | Djinn today | Exference today | Proposed shared form |
| --- | --- | --- | --- |
| Names | validated strings in `HIdentifier` | `QualifiedName` plus ad hoc string parsing | `Name`/`QualifiedName` ADTs with one lexer and rendering policy |
| Source types | `HType` | `HsType` plus parser AST | common surface `Type`, explicit `Forall` and constraints |
| Declarations | `Environment` records | tuple bindings and parsed maps | record-based `ModuleEnv` with spans and validated arities/kinds |
| Diagnostics | strings / `Either String` | writer strings / exceptions | structured `Diagnostic` with severity, span, code, message |
| Generated code | `HExpr`/`HPat` | `Expression`, then HSE AST | one scope-safe expression/pattern AST |
| Simplification | Djinn AST cleanup | `ExpressionSimplify` | shared total-core simplifier with stated invariants |
| Rendering | custom pretty printer | HSE pretty printer | one renderer using shared name rules |
| Search control | named mode, cutoff, budget | heuristic config, max steps, hidden depth/memory pruning | common `SearchBudget` and explicit completion status |
| Verification | independent proof checker | none | backend checker plus optional GHC validation |
| Tests/benchmarks | layered Tasty/QuickCheck/CLI/bench | executable-driven tables | shared frontend/golden suites, backend-specific properties and corpora |

### What should remain separate

- Djinn's `Formula`, LJT antecedent indexes, proof terms, normalization, and
  terminating proof search.
- Exference's polymorphic unifier, constraint solver, scoped typed holes,
  heuristic queue, function ratings, and type-class-aware expansion.
- Backend-specific completeness/termination claims. The common API must report
  Djinn's proof of uninhabitedness differently from Exference's exhausted or
  pruned search.

### Suggested migration sequence

1. **Restore a green Exference core.** Split a named `exference-core` Cabal
   component from the obsolete HSE/CLI frontend, run the new regressions there,
   and migrate the frontend independently.
2. **Create common names and diagnostics.** Move Djinn's mature lexical rules
   into a neutral module and replace Exference's parsing/show-based matching.
3. **Define a common surface type/declaration IR.** Preserve `forall`, contexts,
   kinds, synonyms, constructors, and source spans; elaborate separately to
   Djinn formulas or Exference internal types.
4. **Create a common generated-code AST.** Port both converters, then reconcile
   simplifiers under explicit freshness and totality invariants.
5. **Unify search envelopes, not engines.** A backend interface should accept an
   elaborated query, environment, and budget and stream checked candidates plus
   status/statistics.
6. **Add cross-backend differential tests.** On the monomorphic propositional
   overlap, every Djinn inhabitant should be discoverable/typeable in Exference;
   Exference candidates should pass the common/GHC checker. Do not require the
   same term or enumeration order.
7. **Retire duplicated frontends and CLIs** only after golden behavior is pinned.

## Validation record

Successful checks:

```text
cabal check
git diff --check
```

Build attempts and expected current blockers:

```text
cabal build all
  dependency solver failure from historical upper bounds

cabal build all -f-build-executables --allow-newer=all
  dependencies build; Exference fails against annotated HSE 1.24 AST

cabal build all -f-build-executables --allow-newer=all \
  --constraint "haskell-src-exts == 1.17.1"
  HSE 1.17.1 fails on ambiguous (<>) under modern Prelude

stack test
  installs GHC 7.10.3, then Stack 3.11 rejects its Cabal 1.22
```

No claim is made that the added test suite ran on GHC 9.12. The suite is the
executable specification for the next compatibility milestone.

## Conclusion

Exference contains a genuinely interesting capability that Djinn deliberately
lacks: heuristic synthesis across polymorphic functions and type-class
constraints. Its core ideas are worth preserving, but the 2016 frontend and
package shell should not set the architecture of the merged library.

This pass removes substantial archaeology, fixes several correctness bugs in
the type/class/search bookkeeping, and pins those fixes as tests. The next
highest-value chunk is not more local cleanup; it is the `exference-core`
component split plus a modern, location-normalizing frontend. Once that boundary
exists, the common Djinn/Exference IR and checked output pipeline can be built
without destabilizing either search engine.
