# Djex library quick start

This guide uses the checked Djex APIs. Historical Djinn and Exference modules
remain importable for compatibility, but new integrations should build around a
sealed session, a checked request, and the shared result envelope.

## Build and install

Djex currently supports GHC 9.12.4. From the repository root:

```console
cabal build all
cabal test all --test-show-details=direct
```

Run without installing:

```console
cabal run exe:djex
cabal run exe:djex -- djinn --render expression "a -> a"
cabal run exe:djex -- exference --select first "a -> a"
cabal run exe:djex -- download PACKAGE
cabal run exe:djex -- install PACKAGE
```

Or install the three commands into Cabal's executable directory:

```console
cabal install exe:djex exe:djinn exe:exference
```

A downstream Cabal component needs one dependency:

```cabal
build-depends: djex
```

## Choose a backend

| Requirement | Djinn | Exference |
| --- | --- | --- |
| Terminating unbudgeted search for the supported logic | Yes | No |
| Proof-backed non-inhabitation result | Yes | No |
| Ranked heuristic candidates | No | Yes |
| Explicit prenex polymorphism | No | Yes |
| Type-class participation | Declared methods as proof assumptions | Class/instance-aware; accepted nominal resolution terminates |
| Main controls | Candidate and choice-point limits | Step, queue, depth, constraint, and pattern controls |

Neither backend guesses the other's semantics. One-shot commands and checked
library calls select an engine explicitly; the shared REPL stores an explicit
active selection that can be `djinn`, `exference`, or `both`.

## Embed the shared terminal REPL

Import the interactive launcher separately from the main facade:

```haskell
import Language.Haskell.Djex.REPL
  ( ReplBackend (BothBackends)
  , ReplOptions (..)
  , defaultReplOptions
  , runRepl
  )
import System.Exit (ExitCode)

runProjectRepl :: IO ExitCode
runProjectRepl = runRepl defaultReplOptions
  { replInitialBackend = BothBackends
  , replEnvironmentPath = Just "./environment"
  , replAllowFix = False
  , replHistoryFile = Just "./.djex-history"
  }
```

`ReplBackend` also provides `OneBackend DjinnBackend` and
`OneBackend ExferenceBackend`; import `Backend(..)` from
`Language.Haskell.Djex` when constructing those values. `defaultReplOptions`
starts on Djinn, loads the installed Exference environment with the
command-safe no-fix policy, and keeps history only for the process lifetime.

`runRepl` returns an `ExitCode` and does not terminate the host application.
EOF and `:quit` are successful. Individual query, setting, shell, script,
package-command, and environment-load diagnostics are recoverable and leave
the loop running;
failure before a usable loop can be constructed returns a failure status. An
initial Exference load failure is recoverable because the independent standard
Djinn session is still usable.

This API embeds a terminal frontend, not an abstract protocol: it reads through
Haskeline, writes results and diagnostics to the process streams, can change
the process working directory, and permits both `:!` shell commands and
`:download`/`:install` Cabal effects. Package installation can execute
package-supplied build code and does not add compiled modules to Djex's
source-only inventory. Use the checked
adapters below when an editor, service, or GUI needs to own input, output,
authorization, or session persistence. The complete interactive contract,
including commands, settings, transactional reloads, both-mode isolation, and
the distinction from the historical Djinn REPL, is in the
[shared REPL guide](repl.md).

## Common lifecycle

Both checked adapters follow the same shape:

1. Build or load a neutral declaration environment and seal a backend session.
2. Parse or construct a `QueryRequest`, then cross the backend's smart
   constructor to obtain an opaque checked request.
3. Run the request against a session.
4. Inspect `resultEvidence` independently from `batchProgress`.
5. Select candidates with `selectQueryResults` and render them through the
   backend convenience renderer or the shared generated-code renderer.

Parsing and source loading return structured `Diagnostic` values. Use
`renderDiagnostic` for compiler-shaped text, but retain the structure when an
editor or service can present codes, spans, and context separately.

The checked construction paths also bound width-bearing list spines before a
complete traversal: known class arguments are observed only through the first
extra cell, and tuples only through the first unsupported arity. This matters
for programmatic callers because a cyclic lazy list is otherwise a valid input
value. Such input returns a structured arity/type failure instead of making
environment construction, kind inference, or a backend request hang.

Construction and result consumption have intentionally different evaluation
contracts. Finite declaration indexes and application spines are built
strictly, so sealing a large environment or lowering a wide generated term
does not leave a deferred fold behind. Result sequences remain lazy: taking
the first admissible candidate does not inspect later batches, and a checked
result does not force candidate payloads or tails merely to prove that its
batch is nonempty. Keep the returned Exference result list lazy when streaming,
but apply an explicit selection or fold when a global best candidate is
required.

## Djinn example

This function uses the standard Djinn environment, parses the contextual Djinn
type grammar, runs one checked query, and renders every returned candidate:

```haskell
import Data.Bifunctor (first)
import Language.Haskell.Djex

djinnDefinitions :: String -> Either String [String]
djinnDefinitions source = do
  session <- first renderDiagnostic standardDjinnSession
  target <- first show $ mkIdentifier "result"
  request <- first renderDiagnostic $
    parseDjinnRequest
      session defaultQueryOptions target "<memory>" source
  result <- first renderDiagnostic $ runDjinnQuery session request
  traverse
    (first show . renderDjinnCandidateDefinition FullyQualified)
    (batchCandidates $ resultSearch result)
```

For example, `djinnDefinitions "(a, b) -> (b, a)"` returns a checked definition
for `result`. An empty candidate list must be interpreted together with
`resultEvidence` and `batchProgress`: Djinn may have proved the goal
uninhabited, found that only a target self-reference works, or stopped at a
budget.

Use `mkDjinnSession` for a caller-built
`Environment DjinnTypeVariable Void ()`. Checked sessions are immutable. To
change declarations, retain or recover the neutral environment, build the
complete replacement with `mkEnvironment`, and seal a new session with
`mkDjinnSession`. This is the same lifecycle as Exference and keeps historical
Djinn `HType`, `HKind`, `Declaration`, and `Context` values out of the curated
API. The separate historical `djinn` executable retains its transactional
raw-declaration editor solely inside the compatibility frontend. The shared
`djex` REPL intentionally exposes read-only `:browse` and `:info` views, plus
non-evaluating term inspection through `:type`, instead of that legacy
mutation language.

`:type` is a private REPL frontend over the authoritative neutral inventory
retained by the Exference runtime. It derives callable signatures for ordinary
values, data constructors, and class methods, resolves them in the current
loaded module scope, and uses the prepared class environment to check residual
constraints. It does not consume the policy-filtered Exference search
dictionary and is not routed through the selected synthesis backend. Only the
shared qualification policy affects its presentation. The command performs
structural inference for the expression subset documented in the
[REPL guide](repl.md#inspecting-expression-types); it neither evaluates terms
nor infers types for loaded function bodies.

## Exference example without a parser

The parser-neutral adapter accepts the same shared `Environment` and `Type`
vocabulary. This example uses an empty environment and asks for the identity
function:

```haskell
import Data.Bifunctor (first)
import Language.Haskell.Djex

exferenceDefinitions :: Either String [String]
exferenceDefinitions = do
  environment <- first show $ mkEnvironment []
  session <- first renderDiagnostic $ mkExferenceSession environment
  name <- first show $ mkIdentifier "result"
  target <- first show $ mkDefinitionName name
  let variable = FlexibleVariable 0
      goal = FunctionType (TypeVariable variable) (TypeVariable variable)
      query = QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
        }
  request <- first renderDiagnostic $ mkExferenceRequest query
  results <- first renderDiagnostic $ runExferenceQuery session request
  let selected = selectQueryResults
        SelectFirst (const ()) (const True) results
  traverse
    (first show . renderExferenceCandidateDefinition FullyQualified)
    (selectionCandidates selected)
```

`runExferenceQuery` returns a lazy sequence of result batches. Selection is a
separate presentation policy, so a caller can take the first candidate, retain
all globally best candidates, use bounded lookahead, or stream every admissible
candidate without changing search semantics.

Class-environment construction rejects any groundable instance prerequisite
that can grow its head by type-node count or by occurrences of a head variable,
including rules produced by superclass inflation. For example,
`C [a] => C a` is rejected because resolving `C Int` would grow forever.
Shrinking rules and size-preserving cycles are accepted; path tracking closes
the cycles, and a prerequisite with a variable absent from the head remains an
unresolved obligation. This makes nominal resolution terminate for accepted
finite ground constraints. Exference's surrounding ranked expression search
still is not an inhabitation decision procedure, so keep its step, queue, and
depth controls appropriate for the application.

## Loading an Exference source environment

Import the explicit source boundary in addition to the neutral adapter:

```haskell
import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.HaskellSrc
```

`loadExferenceSession directory` parses an explicit directory's Haskell modules
and ratings, validates the complete shared inventory, and returns an
`ExferenceSessionLoadReport`. `loadDefaultExferenceSession` does the same for
Djex's installed environment; `defaultExferenceEnvironmentPath` exposes that
resolved path when an application needs to display or inspect it. The
policy-aware default loader is `loadDefaultExferenceSessionWithPolicy`.

Module-aware callers that already own discovery and dependency ordering can
use the explicit-file boundary:

```haskell
loadExferenceSessionFromFiles
  :: [FilePath] -- Haskell modules, in caller order
  -> [FilePath] -- rating files, in caller order
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromFilesWithPolicy
  :: ExferenceSessionPolicy
  -> [FilePath]
  -> [FilePath]
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromSources
  :: [(FilePath, String)] -- already-read modules, in caller order
  -> [(FilePath, String)] -- already-read ratings, in caller order
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromSourcesWithPolicy
  :: ExferenceSessionPolicy
  -> [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO ExferenceSessionLoadReport
```

These functions run the same read, parse, inventory, rating, and sealing
pipeline as directory loading. The `FromSources` variants never reopen their
paths; paths are retained as parser filenames and diagnostic provenance. They
are appropriate when a caller already owns an immutable filesystem snapshot.
None of these functions discover imports: the caller owns the complete ordered
source closure. An empty module list is valid and retains Exference's built-in
syntax-level constructor inventory.

Callers that need the checked source projection before session policy and
sealing can use the corresponding lower-level boundary from
`Language.Haskell.Exference.EnvironmentParser`:

```haskell
parseModuleSources
  :: [(FilePath, String)]
  -> IO (LoadReport SourceEnvironment)

environmentFromSources
  :: [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO (LoadReport CheckedSourceEnvironment)
```

These functions have the same ordered snapshot and diagnostic-path contract.

Always inspect both report fields:

- `exferenceSessionLoadResult` contains either fatal diagnostics or the sealed
  session;
- `exferenceSessionLoadDiagnostics` contains warnings and informational
  diagnostics produced while loading and sealing.

Fatal source-loader phases use stable `EXF_*` codes. Read, parse, unsupported-
vocabulary, and source-aware extraction failures retain the source spans
available at those phases. A later neutral-inventory, sealing, or policy
failure uses its `DJEX_EXF_*` code and may be source-free because no single
token owns that whole-environment invariant.

The loader rejects unsupported source meaning before building a partial
inventory. The authoritative emitted `UnsupportedVocabularyForm` set includes
pattern-synonym signatures and XML page/hybrid modules as well as the
documented type-family, GADT, deriving, class, and instance limitations.
Ordinary term patterns and pattern-value bodies remain accepted; do not
interpret the pattern-synonym-signature restriction as a ban on ordinary
pattern matching. `ExplicitExportList` is retained as a source-compatible
constructor but is no longer emitted.

Explicit module export lists are accepted. Loading deliberately retains both
exported and private declarations in the checked inventory. A module-aware
caller applies an export list when constructing its interactive visibility
scope, while retaining the full inventory for `*MODULE` access and canonical
qualified lookup. Directory and explicit-file loaders themselves build an
inventory, not a prompt scope.

With `loadExferenceSessionWithPolicy`, unknown exclusions are harmless no-ops.
This lets a reusable policy exclude an optional binding absent from a particular
environment. Rating overrides are stricter: every rating must be finite and
every overridden name must still be available to search after exclusions and
capability filtering, otherwise session sealing returns a fatal policy
diagnostic.

With a session, use
`parseExferenceRequest session options target sourceName sourceText` so type
names, class arities, synonyms, source spellings, and later diagnostic spans all
come from the same checked inventory.

An interactive source frontend should instead construct an
`ExferenceQueryScope` and call `parseExferenceRequestInScope` (or the
checked-target counterpart). Its fields have intentionally narrow meanings:

- `exferenceQueryVisibleNames` is the exact set admitted for unqualified
  lookup;
- `exferenceQueryCurrentModule` gives one full-top-level module's local names
  precedence;
- `exferenceQueryModuleAliases` maps prompt qualifiers introduced by imports
  to canonical loaded modules; and
- `exferenceQueryQualifiedNames` optionally restricts each written qualifier
  to an exact canonical-name set, allowing aliases to enforce explicit import
  lists and `hiding`.

Canonical qualification still resolves names from the complete sealed
inventory when no written-qualifier restriction applies. An empty outer
`exferenceQueryQualifiedNames` list preserves the original permissive scoped
API behavior; a present qualifier paired with an empty name list admits
nothing through that spelling. Scope is therefore a visibility projection
over one checked inventory, not a second independently loaded environment. The
shared REPL also narrows Exference's searchable binding projection to the same
visible names while keeping the complete inventory for qualified type
elaboration.

The `djex` and `exference` executables use the same default-path operation.
Cabal's generated `Paths_djex` module remains private; applications do not need
to import a package-specific generated module. From a source checkout, the
bundled directory is `exference/environment`.

## Reading results correctly

`QueryEvidence` and `Progress` answer different questions:

- `ValidatedCandidates` means the same result batch contains at least one
  independently checked candidate.
- `ProvedUninhabitable` is a logical Djinn conclusion, not an empty-list alias.
- `RequiresTargetReference` means a safe nonrecursive definition was excluded.
- `NoEvidence` makes no logical claim.
- `Completed Finished` means the configured exploration ended normally.
- `Completed (Truncated reasons)` records resource limits such as step,
  candidate, choice-point, queue, depth, or identifier-space limits.
- `Continuing` means more batches may follow.

A truncated batch can still contain useful validated candidates. Conversely, a
finished Exference batch with no candidates is not a proof of non-inhabitation.
Frontends should preserve both dimensions.

## Rendering and residual constraints

The backend render helpers accept `Unqualified`, `QualifyIdentifiers`, or
`FullyQualified` and return the common `RenderError` type. Exference candidates
may retain residual class constraints; inspect
`candidateResidualConstraints` or call
`renderExferenceResidualConstraintsWithQualification` with the same policy used
for the candidate rather than presenting the generated term as obligation-free.
`renderExferenceResidualConstraints` retains the historical fully qualified
policy. Both helpers return
`Either ExferenceResidualRenderError [String]`: they validate each nominal
class and every complete shared type argument before sorting or deduplicating,
then use the candidate's validated type-variable hints. The
qualification-aware helper applies its policy to the class and to every
constructor nested in the constraint arguments.

The checked `Either` result intentionally replaces the earlier pure `[String]`
signature. `Candidate` has a public compatibility constructor, so a pure
renderer could otherwise present caller-forged malformed residuals as Haskell
source. Failures carry zero-based constraint and argument positions and the
shared structural error; validation reports the first class or argument error
in the candidate's original order. Existing callers can migrate by handling
the result alongside `renderExferenceCandidateExpression` or
`renderExferenceCandidateDefinition`.

For custom presentation, use `candidateOutput` to obtain the shared
`FunctionClause` and the operations in
`Language.Haskell.Synthesis.Generated`. Scope validation and collision-safe
local naming remain part of that shared rendering boundary. In particular,
`Candidate` keeps a public constructor for compatibility, so the stable
expression and definition helpers validate the complete clause on every call;
caller-forged free local identities and duplicate pattern-binder identities
produce `RenderError` instead of unchecked Haskell text. Djinn's helpers then
reject any nonempty residual list with `UnexpectedResidualConstraints`, since
every genuine Djinn result is closed. Generated-clause failures deliberately
take precedence if a caller forges both the output and residual fields.

## Import guidance

- Start with `Language.Haskell.Djex` for a compact application or an API tour.
- Prefer explicit `Language.Haskell.Djex.Djinn` and
  `Language.Haskell.Djex.Exference` imports in larger modules to make backend
  ownership obvious.
- Import `Language.Haskell.Synthesis.*` modules directly when defining reusable
  neutral infrastructure.
- Use historical `Djinn*` or `Language.Haskell.Exference*` modules only when
  maintaining a compatibility integration.

`Language.Haskell.Djex.Djinn`, not `Djinn.Core`, is the curated checked Djinn
facade. Import `Djinn.Core` only when a compatibility integration deliberately
needs its historical representation and operations.

See [the architecture guide](architecture.md) for the stability tiers,
[the shared REPL guide](repl.md) for interactive use, and
[the synthesis API map](../synthesis/README.md) for the neutral modules.
