# Djex deferred-item resolution

Date: 2026-07-17

## Scope

The 2026-07-16 review sweep deferred five items. This pass investigated
each with a dedicated implementation plan (feasibility, pinned-behavior
inventory, and risk analysis) before touching code, then landed the four
that survived scrutiny. One resolved *against* change. The package
version advanced to `2026.7.17` with the first contract-changing commit,
and every commit kept all eleven suites, `cabal check`, and Haddock
green under the release warning set.

## Resolved as kept architecture: Djinn display naming

Unifying `niceNames` with the shared render-time allocator was rejected,
not merely deferred again. The investigation established that the baked
`a`..`z` spellings are the pinned public stored-tree contract —
`candidateOutput`, the `HClause` compatibility view, and every
identity-preference render site observe them directly — and that
`Djinn.Core`'s structural candidate de-duplication is correct only
because proof lowering canonicalizes binder spellings before clauses are
compared. Deferring naming to rendering would break every exact-output
pin, silently weaken dedup, and grow the public candidate surface with
hint machinery for no behavioral gain. The invariant is now guarded by
documentation at `niceNames` and at the dedup site. Exference's
opposite choice (search-native locals stored, hints applied at render
time) remains correct for its own contract: its locals are `Int`s with
no historical stored spelling.

## Ground edit transaction

The stable adapter's declare/remove operations no longer weaken the
sealed `Void`-kinded session environment to `Int` merely to reuse the
raw editor. The replacement transaction is kind-polymorphic
(`declareSharedDeclaration` / `removeSharedDeclaration`, taking the
resealing operation explicitly), the `Int` entrances are unchanged
delegations preserving the raw string API exactly, and new ground
entrances mirror `prepareGroundSynthesisEnvironment`. Only the one new
declaration crosses the grounding boundary, rejected with the identical
`UngroundedInventoryKind` error value. An agreement suite drives the
same edits through both transactions and compares every observable
projection, so the paths cannot drift. One deliberate precedence change:
the ground declare path rejects an unsolved kind variable at the
entrance rather than during resealing, with identical error value and
rendered text, pinned by test.

## Exference error-taxonomy split

`Language.Haskell.Exference.Core.Declaration` now separates the
21-constructor per-declaration `SynthesisDeclarationError` from a new
`SynthesisEnvironmentError` owning the 15 environment/inventory/
projection constructors, nested through
`SynthesisEnvironmentDeclarationError` exactly as in Djinn's environment
adapter. Four constructors with no producers anywhere were deleted
first in a separate commit. Declaration failures cross into environment
operations at exactly one wrapping boundary per operation, preserving
failure precedence; `EnvironmentParser`'s `InvalidSourceInventory` keeps
its code and message. The Diagnostic-typed stable session surfaces are
unchanged. This is a public type change on two exposed modules and drove
the version advance.

## Located extraction errors

The new exposed `Language.Haskell.Exference.ExtractionError` pairs each
extraction failure's exact historical message with an optional shared
`SourceLocation` validated from HSE coordinates (full span, else point,
else none). Every extractor gained a located core with the historical
string entry point as its exact message projection; class-method
failures locate at their class-body declaration, and a duplicate class
locates at its first declaration in source order (pinned in both
declaration orders). `EnvironmentLoadError` and
`ClassEnvironmentLoadError` payloads now carry `ExtractionError`
(`BuiltInEnvironmentErrors` deliberately keeps strings: the hard-coded
table has no location even in principle). The merged `djex` command now
reports `Broken.hs:3:1-17: error [EXF_TYPE_DECLARATION]: ...` where it
previously printed an unlocated string, pinned end-to-end through
`environmentFromModule` and the rendered diagnostic.

## Confirmed decision: session bridge overlap

`loadExferenceSessionWithPolicy`'s four-line overlap with
`Language.Haskell.Exference.Session` is the recorded
dependencies-point-inward architecture (the stable loader calls the
private sealer directly rather than sideways through a peer facade) and
stands unchanged.
