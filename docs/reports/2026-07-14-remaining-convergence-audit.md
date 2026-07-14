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
dependency closure and erase the isolation tests without removing the
result-envelope and environment-authority duplication described below.

## Priority 1: make the shared vocabulary native to Exference

**Completed on 2026-07-14:** `QualifiedName`, `HsConstraint`, and `HsType` now
use the shared `Name`, `Constraint`, and `Type (Variable Int)` values directly.
Historical constructor spellings survive as explicit compatibility patterns;
the structural conversion functions are identity shims, and checked adapters
only validate and canonicalize. This removes all three duplicate source-type
representations without collapsing the useful component dependency checks.

At audit time `Language.Haskell.Exference.Core.Types` defined three pieces of a
second source-type model beside `Language.Haskell.Synthesis`: a `QualifiedName`
wrapper, the recursive `HsType`, and a duplicate `HsConstraint`. The first
stage removed the name and constraint representations, including every
production `toSynthesisName` / `fromSynthesisName` call. The completed type
stage then replaced the recursive `HsType` tree with the shared type directly.

The migration reached the search core, stable request/session storage,
generated candidates, residual constraints, rendering hints, class
environments, and the HSE compatibility frontend. Production code no longer
uses the old total structural-conversion shims; only named validation and
canonicalization boundaries remain.

The migration proceeded in five compile-checked stages:

1. **Completed:** moved shared `Name` and `Constraint` values into the search
   core, preserving historical constructor spellings as explicit compatibility
   patterns rather than as engine-owned representations.
2. **Completed:** moved free-variable collection, substitution, rendering hints,
   and constraint traversal to `Type (Variable Int)`. Exference-specific
   unification remains backend code but now operates on the shared tree.
3. **Completed:** taught the unifier, rigid-instantiation, expression checker,
   constraint solver, and search-node builders to consume the shared type
   directly.
4. **Completed:** deleted the now-identity type/constraint conversion passes in
   the checked adapter and candidate projection. Attaching a checked target to
   an output clause remains a lazy `fmap`, so later search batches are not
   forced.
5. **Completed:** made the HSE frontend construct shared types once. The old
   `HsType` surface remains as a compatibility alias and patterns over the
   native value, not as the representation stored in a sealed session.

Tuple representation received explicit treatment. The shared IR's structural
`TupleType` is the canonical checked form; the search algorithms handle it
directly and normalize saturated constructor applications to it. The
documented `COMPLETE` set includes the structural tuple case and the total
`TypeForallNative` view, so compatibility patterns do not conceal an unhandled
native constructor.

Regression evidence now covers flexible-versus-rigid identity,
capture-avoiding forall substitution, tuple canonicalization, residual
constraint order, unknown-class policy, type-variable rendering hints, and
lazy batch tails. It additionally pins malformed native-value rejection,
higher-kinded tuple/function unification, exact historical tuple ranking, and
both supported compiler graphs. Priority 2 is therefore the active frontier.

## Priority 2: remove duplicate result envelopes

Both engines still have an older result model immediately below the shared
one.

**Djinn progress on 2026-07-14:** the proof core now constructs
`QueryResult DjinnQueryMetadata DjinnCandidate` directly, retaining the exact
checked `DefinitionName` in every candidate. The checked adapter returns that
same value without a metadata, completion, evidence, or candidate rebuild.
`GeneratedQueryReport`, `QueryOutcome`, and `QueryReport` remain outer
compatibility projections only; they no longer own the canonical search
result. Structured invariant failures also remain distinct from ordinary
query rejection.

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

Only after the remaining duplicated result/environment authorities are removed
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

The immediate next implementation milestone is Priority 2: remove the backend
result envelopes beneath `QueryResult` and have both cores construct their
stable payloads directly, while preserving logical evidence, operational
truncation, exact counts, target names, and lazy search tails.
