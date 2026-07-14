# Djex remaining-convergence audit

Date: 2026-07-14

## Scope

This is a current-state audit of what remains before Djex can honestly be
described as one library rather than one package containing two engines and a
shared foundation. It follows the consolidation, checked-session, shared-query,
candidate, diagnostic, generated-output, exact-count, and dependency-bound
work already present on the `codex/unify-djinn-and-exference` branch.

The existing work is substantial, but the original merger objective is not
complete. Search algorithms may remain backend-specific; duplicate language
IRs, result envelopes, general-purpose environments, and public component
identities should not.

## Evidence from the current tree

`djex.cabal` still defines six library components:

1. the unnamed `djex` facade;
2. `synthesis`;
3. `djinn-core`;
4. `djinn-frontend`;
5. `exference-core`; and
6. `exference-frontend`.

That component DAG currently enforces a useful direction:

```text
synthesis <- backend cores <- compatibility frontends
          \- checked adapters -/        |
                    \- djex facade -----/
```

The default library and parser-independent cores consequently do not acquire
`haskell-src-exts`, `directory`, `filepath`, or `haskeline`. The core and
facade API suites exercise those package boundaries rather than merely checking
that a module happens to import.

A mechanical six-to-one Cabal collapse is possible: the 79 live library source
files have distinct module names and an acyclic component import graph. Its
external dependency union would be `base`, `containers`, `deepseq`, `directory`,
`filepath`, `haskeline`, `haskell-src-exts`, `pqueue`, `pretty`, and
`transformers`. Doing that now, however, would broaden every client's
dependency closure and erase the isolation tests without removing the largest
source-level duplication described below.

## Priority 1: make the shared vocabulary native to Exference

`Language.Haskell.Exference.Core.Types` still defines a second source-type
model beside `Language.Haskell.Synthesis`:

- `QualifiedName` wraps the shared `Name` and repeatedly crosses
  `toSynthesisName` / `fromSynthesisName`;
- `HsType` duplicates variables, constructors, applications, arrows, and
  quantified constraints already represented by
  `Type (Variable Int)`; and
- `HsConstraint` duplicates `Constraint HsType` while retaining another
  nominal-name wrapper.

This is not confined to a compatibility parser. Thirty-one other Exference
source or test files import `Core.Types`, and fifteen Exference files contain
explicit `toSynthesis*` or `fromSynthesis*` seam calls. In the stable adapter,
`ExferenceType` is already exactly `Type (Variable Int)`, but
`runExferenceQuery` converts it back to `HsType`; generated candidates,
constraints, and binding names are then traversed in the opposite direction.

The migration should proceed in compile-checked stages:

1. Use shared `Name` and `Constraint` values in the search core. Preserve the
   historical constructors, where worthwhile, as checked compatibility
   patterns at the public edge rather than as an engine-owned representation.
2. Move pure type operations—free-variable collection, substitution,
   rendering hints, and constraint traversal—to `Type (Variable Int)`.
   Exference-specific unification remains backend code, but should operate on
   the shared tree.
3. Teach the unifier, rigid-instantiation, expression checker, constraint
   solver, and search-node builders to consume the shared type directly.
4. Delete the now-identity type/constraint conversion passes in the checked
   adapter and candidate projection. Preserve laziness: attaching a checked
   target to an output clause must remain a lazy `fmap`, not a traversal that
   forces later search batches.
5. Make the HSE frontend construct shared types once. Any old `HsType` surface
   that remains should be explicitly deprecated compatibility syntax, not the
   representation stored in a sealed session.

Tuple representation needs deliberate treatment. The shared IR has structural
`TupleType`, whereas historical Exference encodes tuples as constructor
applications. Algorithms must either learn the structural case or consume one
documented canonical form; a misleading `COMPLETE` pattern set must not hide an
unhandled tuple at runtime.

Required regression evidence includes flexible-versus-rigid identity,
capture-avoiding forall substitution, tuple canonicalization, residual
constraint order, unknown-class policy, type-variable rendering hints, and
lazy batch tails.

## Priority 2: remove duplicate result envelopes

Both engines still have an older result model immediately below the shared
one.

Djinn constructs `GeneratedQueryReport`, which the checked adapter repackages
as `QueryResult`. The compatibility-only `QueryOutcome`, `QueryReport`,
`inhabit`, `outcomeEvidence`, and `evidenceOutcome` retain another interpretation
of the same evidence and completion states.

Exference retains `SearchCompletion`, `SearchStatus`, `ExferenceChunkElement`,
and conversion functions leading to shared `Progress` and `SearchBatch`.
Candidate statistics and batch metadata also have core and stable-adapter
records copied field by field by `resultBatch`, `projectBatchMetadata`, and
`projectCandidate`.

After the native-type migration, both cores should produce their stable
`QueryResult` payloads directly. Compatibility functions can render or classify
that value at the outermost API edge. Tests must pin the distinction between
logical evidence and operational truncation, exact `Natural` accounting,
target-name preservation, candidate validation, and non-forcing of lazy tails.

## Priority 3: make shared inventories authoritative

Exference source loading currently retains a `SourceEnvironment`, a shared
inventory, a prepared neutral inventory/backend projection, and additional
type/class indexes. HSE extraction should create shared declarations once;
source order, ratings, spans, and method ownership are frontend metadata rather
than competing semantic environments. Session type and class lookup can then
derive from the retained shared inventory.

Djinn likewise retains its raw editable `Environment` beside a shared
`Inventory`, with bidirectional declaration/environment bridges. The REPL
stores the raw environment together with a sealed `DjinnSession` and converts
the complete environment after each mutation. Its eventual authority should be
the shared transactional declaration environment; parsing remains a one-way
compatibility edge, and proof-oriented indexes remain private derived state.

These changes must preserve source diagnostic precedence, declaration
replacement/removal order, alias saturation and recursion checks, inferred
kinds, class-method instantiation, policy exclusions/ratings, and transactional
rollback.

## Packaging end state

Only after the duplicated type/result/environment authorities are removed
should the named core and foundation sublibraries be folded into the unnamed
`djex` library. Compatibility frontends may be folded in at the same time if a
single dependency closure is the desired final contract, or retained briefly
as deprecated migration shells with no independent semantic representation.

This ordering makes the final Cabal change evidence of a source merger rather
than a relabeling. It also makes deletion measurable: internal `djex:*`
dependencies disappear because their implementations already share one native
vocabulary and result boundary.

## Validation gates for each stage

Every migration milestone should retain the current release-style gates:

- focused unit/property tests for touched core operations;
- all 12 Djex test suites and both benchmarks compiled with `-Werror`;
- GHC 9.12.4 and supported GHC 9.8.4 builds;
- `--prefer-oldest` resolution for the declared bounds;
- `cabal check`, source-distribution inspection, and Haddock generation; and
- a clean tracked tree with the milestone commit pushed before the next
  representation is removed.

The immediate next implementation milestone is Priority 1, beginning with a
compatibility-aware shared-name/constraint layer and continuing until the
stable Exference query path no longer performs a round trip through `HsType`.
