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
IRs targeted by Priority 1 and the competing result authorities have now been
removed, while general-purpose environments and public component identities
remain duplicated.

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
environment-authority duplication described below.

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
both supported compiler graphs. Priority 2 subsequently removed the remaining
result authority beneath the shared envelope.

## Priority 2: remove duplicate result envelopes

**Completed on 2026-07-14.** Both cores now construct their stable
`QueryResult` payloads directly; neither checked adapter owns a second result
authority.

The Djinn proof core constructs
`QueryResult DjinnQueryMetadata DjinnCandidate` directly, retaining the exact
checked `DefinitionName` in every candidate. The checked adapter returns that
same value without a metadata, completion, evidence, or candidate rebuild.
`GeneratedQueryReport`, `QueryOutcome`, and `QueryReport` remain outer
compatibility projections only; they no longer own the canonical search
result. Structured invariant failures also remain distinct from ordinary
query rejection.

The Exference core's `findQueryResultsInEnvironmentEither` excludes the exact
checked target, validates and prepares the query once, derives rendering hints
from the retained plan, and lazily attaches that target to every generated
`FunctionClause`. `runExferenceQuery` returns those results directly. Candidate
statistics, candidate details, and batch metadata each retain one core-owned
record; the stable polished names are zero-cost type aliases and bidirectional
record-pattern views, not records copied field by field.

`SearchCompletion`, `SearchStatus`, `ExferenceChunkElement`, and the checked
status-to-progress conversions remain only at Exference's historical API edge.
The canonical path projects private engine chunks straight into the common
search and query envelopes, so retaining those source-compatible types does
not retain a competing modern result model. Explicit imports of the stable
record views migrate from `T(..)` to `T`, `pattern T`, and the used field
selectors under `PatternSynonyms`; their pattern fields cannot be used by GHC
record-update syntax, so updates match and reconstruct instead. This
experimental representation migration also adopts the core records' derived
`Show`, `Typeable`, generic, and ABI identities; dependants must recompile and
must not treat the former derived `Show` text as a stable serialization.

Regression coverage pins direct core/adapter equality, exact operator targets,
target exclusion without excluding qualified homonyms, candidate-derived
logical evidence, simultaneous operational truncation reasons, exact
`Natural` accounting, and lazy batch and candidate tails. Priority 3 is now the
active convergence frontier; completing Priority 2 does not merge the two
backend-specific search algorithms.

## Priority 3: make shared inventories authoritative

**Exference source authority completed:** source checking now retains one
opaque annotated prepared-inventory witness instead of a `SourceEnvironment`,
an annotated Inventory, and a neutral Inventory/backend projection. The
witness owns the sealed alias-bearing Inventory, its synonym table, and the
exact ordered/rated backend. Alias-aware recursive-datatype metadata is
attached through an annotation-only Inventory adjustment, so sealing and kind
inference run once. The compatibility `SourceEnvironment` projection is
derived on demand; method ownership comes from nested shared class methods,
while order, ratings, classes, constructors, and recursion come from the
prepared backend. Only historical HSE synonym spellings remain separately as
presentation data. Stable sessions erase annotations without re-preparing the
synonym table or backend, and HSE query parsing derives its minimal known-type
and class-arity resolver from that same session inventory.

**Djinn REPL authority completed:** a session now retains its editable shared
environment with the exact prepared projection derived from it, and declaration
replacement/removal edits that shared environment directly. The REPL stores no
raw `Environment`; display and instance lookup consume the already prepared
backend. Raw preparation also reprojects embedded kinds from the inferred
inventory, fixing a split-brain bug in which a forged raw class parameter kind
could override the inventory during context checking. Raw declarations remain
compatibility parser inputs, while proof-oriented tables are private derived
indexes. Further internal work can narrow `PreparedEnvironment` into dedicated
class, premise, kind, synonym, and formula caches without changing authority.

These changes must preserve source diagnostic precedence, declaration
replacement/removal order, alias saturation and recursion checks, inferred
kinds, class-method instantiation, policy exclusions/ratings, and transactional
rollback.

## Packaging end state

Only after the remaining duplicated environment authorities are removed
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

The immediate next implementation milestone is the remaining Priority 3
Djinn work: narrow `PreparedEnvironment` into private class, kind, synonym,
formula, and cached global-premise indexes without restoring a raw editable
environment. Once that cache boundary is stable, fold the parser-free
foundation and backend cores into the unnamed library and retire component
seams that no longer enforce a semantic boundary.
