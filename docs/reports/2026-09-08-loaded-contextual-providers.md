# Loaded contextual providers retain their source schemes

Status: accepted for the explicit-forall behavioral fixture below. The strict
build and all 2,306 tests in 15 complete regression suites pass. This is a frontend increment in priority 3. It does not close
implicit-root behavioral scoping, ordinary contextual presentation, derived
class-method schemes, or the broader priorities 1–4 goal.

## What changed

The Haskell source loader previously opened each top-level signature into a
flat search binding before constructing its checked inventory. Its later
scheme table therefore retained an open reconstruction, losing the declared
leading `forall` order. A constrained provider could reach Exference search
without the closed source scheme needed for checked dictionary application.

Extraction now retains a closed scheme and its flat projection together.
`getDeclSchemesSourcedWithResolver` supplies the complete form; the historical
extractor remains its flat projection. `SourceFunctionWithScheme` distinguishes
this metadata from caller-supplied compatibility bindings. Source sealing
checks closure and exact correspondence with the binding before admitting the
scheme to the shared inventory. Ratings change only the binding's penalty.
Checked projections retain the scheme through a subsequent sealing operation.

Fresh extraction and prepared lowering use different established variable
openers. Correspondence checks accept the exact complete binding produced by
either opener, including its parameter order, result and constraints. They do
not match only a provider name or infer specified binders from a flat result.
A new round-trip test checks `forall z a. a -> z -> a`; a separate control
rejects a scheme substituted independently of its binding.

Djinn's REPL projection previously omitted all values with a leading class
context. Its core already retains such providers as opaque complete schemes,
requiring checked dictionary application. The frontend now admits supported
schemes through that core boundary. It still honors the explicit
`djinn-axioms` setting and preserves conservative negative evidence while
source instances remain unavailable to the lexical search. The existing
instance-gap control verifies that no unconditional result or false refutation
is introduced.

Djinn also renames source declarations into its prompt vocabulary. Behavioral
elaboration now applies that same name projection to the requested signature
before comparing it with the candidate's graph. Exference uses its original
qualified vocabulary. Neither route relabels a sealed graph to make it fit.

## Acceptance fixture

The CLI regression loads an ordinary Haskell module with these shapes:

```haskell
class C a where payload :: Int
method :: forall a. C a => Token
```

The source defines dictionaries carrying 37 for `Int` and 91 for `Bool`.
Both engines must synthesize the original `forall a. C a => Token` signature
through a named `where` query and distinguish both payloads. The exact displayed
definition is then executed in a separate GHC process at that full signature.
Its replay source stays outside the synthesis environment. A second live query
uses literal `False` and must reject candidates without displaying a result or
reporting a checker error or timeout.

Both queries retain a 32-candidate/quality window and 20,000 choice/step budget.
Djinn explicitly enables value axioms. The test requires actual negative
checking rather than a fixed number of rejected candidates: Exference can
enumerate equivalent constructions until the window is consumed.

## Validation

The [regression receipt](../../test-church/receipts/loaded-contextual-providers.json)
records **2,306 passing tests in 15 complete suites**, totaling 417.56
seconds in their suite summaries. Counts match each complete unique inventory;
source, executable and helper hashes remained unchanged. This includes the
complete 515-test Exference suite and 102-test Djex CLI suite. The strict serial
build uses `--ghc-options=-Werror`. The receipt retains process output, source
snapshots, the driver, the build log and failed diagnostics.

Reproduce the suites with `cabal test` using `regression.selected_suites` from
the receipt, `-j1` and `--test-options="-j1 --color=never --timeout=180s"`.
Direct executable invocation additionally needs Cabal's package-data directory
(`djex_datadir`) and the built helper executables on `PATH`.

The earlier direct unit invocation omitted Cabal's `djex_datadir` setting and
failed on missing installed package-data paths. With the actual checkout data
path supplied, the complete original 513-test suite passed. The new 515-test
suite subsequently caught and verified the projection round-trip correction.
The earlier CLI assertion requiring exactly one False rejection also failed;
the live Exference trace recorded 32 actual falsifications and no errors or
timeouts. The final test checks that rejection contract without imposing an
unrelated candidate count.

## Remaining frontend work

This delivery concerns ordinary loaded value declarations used by explicit
`forall` behavioral queries. Derived class methods still have their separate
source-scheme boundary. Implicitly quantified roots need correctly ordered
scoped binders and independently replayable displayed definitions. Ordinary
contextual queries also need faithful presentation of the selected type and
dictionary; success of the named-`where` route does not establish that route.

Leant's accepted native global-method integration remains pinned to Djex
`4a4ed0fc`. It is a separate validation result and does not validate this newer
Haskell source-loader change. The full Church matrix, mixed Lean dictionary
inventories, selected dictionary occurrences and supplied tree folds remain
required by the [current roadmap](2026-09-07-synthesis-retriage.md).
