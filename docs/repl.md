# Shared Djex REPL

The `djex` REPL keeps Djinn and Exference available in one terminal session.
Its source-workspace and module-context commands deliberately follow the model
described in the
[GHCi User's Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html):
`:load`, `:add`, `:unadd`, and `:reload` control loaded source modules, while a
bare `import` or `:module` controls which of those modules is in scope at the
prompt. Djex also borrows GHCi's colon-command, completion, history, interrupt,
and multiline conventions.

Djex is nevertheless not a GHCi-compatible evaluator. A bare line is a type
that Djex should inhabit, not a Haskell expression to evaluate. Source modules
provide checked type, data, class, instance, and signature information to
Exference; their function bodies are not compiled or interpreted. The two
synthesis engines also retain different parsers and search semantics. The
`:type` command is a read-only exception at the command boundary: it infers an
expression's type from loaded signatures, but still never evaluates the
expression or compiles a source body.

Use the shared REPL when exploring or comparing types. Use the one-shot
`djex djinn` and `djex exference` subcommands when a script needs generated
Haskell and a per-invocation exit status without an interactive prompt.

## Starting a session

The following forms start the same frontend:

```console
djex
djex repl
djex repl [OPTION...]
```

From a checkout, use `cabal run exe:djex --` in place of `djex`. Startup
options initialize state before the first prompt:

| Option | Meaning | Default |
| --- | --- | --- |
| `--backend djinn\|exference\|both` | Initial active backend selection. | `djinn` |
| `--environment DIR` | Initial Exference directory target (the recursive directory compatibility form). | Installed Djex environment |
| `--fix` | Retain the known nonterminating Exference recursion helpers while loading. | Off |
| `--history FILE` | Read and write Haskeline history in `FILE`. | In-session history only |

Run `djex repl --help` for the startup table. Options for a one-shot backend,
such as `--select` or `--max-steps`, are changed inside the REPL with `:set`;
they are not REPL startup flags.

A normal startup looks like:

```text
Djex REPL <package-version>
Djinn session ready (standard checked environment).
Exference environment: "/path/to/environment"
Type :help for help.
djex[djinn]>
```

The placeholders above are the installed package version and resolved initial
directory target. The default prompt contains `%b`, which is replaced
dynamically by `djinn`, `exference`, or `both`. It does not duplicate the
potentially long module context; use `:show imports` to inspect that context.

## Querying and switching backends

Enter a type directly to use the active selection:

```text
djex[djinn]> a -> a
djexResult a = a
```

Backend commands distinguish persistent selection from one-query routing:

```text
:backend                         -- print the active selection
:backend exference              -- make bare queries use Exference
:backend both                   -- make bare queries use both engines
:djinn TYPE                     -- Djinn for this query only
:exference TYPE                 -- Exference for this query only
:compare TYPE                   -- both engines for this query only
:synth TYPE                     -- the active selection, like bare TYPE
```

`:backend` accepts unique prefixes and does not recreate either session.
`:djinn`, `:exference`, and `:compare` do not change the active selection.
Entering `:` repeats the last type with the backend selection recorded for
that query; current rendering and search settings are used for the repeat.

The shared REPL does not force the engines into a common type language. Djinn
parses its contextual type grammar against its checked standard environment.
Exference parses its Haskell type grammar against the currently loaded source
workspace and prompt module context. That context controls unqualified source
names and the source declarations available to Exference's search. It never
changes Djinn's independent standard session. A spelling can therefore be
valid for only one backend or mean something supported by only one search
engine.

In `both` mode Djex prints labelled sections in a deterministic order:

```text
-- Djinn
...
-- Exference
...
```

Each backend parses and runs independently. A diagnostic from Djinn does not
prevent Exference from running, or vice versa. The sessions do not share
declarations, type-variable identities, caches, or class-resolution policy;
only query routing and presentation settings are shared. In particular, an
empty Exference result is not upgraded to Djinn's proof-backed
non-inhabitation evidence.

## Commands

Exact aliases win; otherwise any nonempty, unique prefix of a canonical
command is accepted. For example, `:q` is `:quit`, `:s` is deliberately
`:set`, and `:sy` is `:synth`. Tab completion offers commands, backend names,
settings, `:show` subjects, and paths where appropriate.

| Command | Purpose |
| --- | --- |
| `:add [TARGET ...]` | Add explicit Exference source targets and reload their local dependencies. |
| `:backend [djinn\|exference\|both]` (`:b`) | Show or change the active selection. |
| `:browse [[*]MODULE]` | Browse the current scope, a module's exports, or a source module's full source scope. |
| `:cd DIR` | Change the process working directory. |
| `:compare TYPE` | Run one independently parsed query with both backends. |
| `:djinn TYPE` | Run one Djinn query. |
| `:exference TYPE` | Run one Exference query. |
| `:help [COMMAND]` (`:h`, `:?`) | Show the command summary or detailed command help. |
| `:history [N]` (`:hist`) | Show all history, or its last `N` entries, oldest first. |
| `:info NAME` (`:i`) | Show exact-name declarations for the active backend(s), including constructors and class methods. |
| `import DECLARATION` | Append a Haskell import to the Exference prompt context. |
| `:load [TARGET ...]` (`:l`) | Replace the Exference target set and load local dependencies. No targets clears it. |
| `:module [+\|-] [[*]MODULE ...]` (`:m`) | Replace, add to, or remove from the Exference prompt context. |
| `:pwd` | Print the process working directory. |
| `:quit` (`:q`) | Leave successfully. |
| `:reload` (`:r`) | Re-read the retained canonical Exference targets and dependencies. |
| `:script FILE` | Execute REPL inputs from a file. |
| `:set [OPTION [VALUE]]` (`:s`) | Show all settings, or change one. |
| `:show [SUBJECT]` | Show settings, loaded-source state, or selected backend state. |
| `:synth TYPE` (`:sy`) | Query the active backend(s). |
| `:type [+d] EXPRESSION` (`:t`) | Infer an expression's type in the current loaded module scope without evaluating it. |
| `:unadd [TARGET ...]` | Remove explicit Exference targets and reload the surviving dependency closure. |
| `:unset OPTION` | Restore one built-in default. |
| `:version` (`:v`) | Print the Djex package version. |
| `:! COMMAND` | Run a shell command. |
| `:` | Repeat the previous query. |

Target and path arguments may be bare words, Haskell string literals, or a
whole Haskell `[String]` value. The list form is particularly useful in
scripts. `:module+ M` and `:module- M` are accepted conveniences for the
canonical GHCi-style `:module + M` and `:module - M` spellings.

`:show` accepts `settings`, `backends`, `environment`, `imports`, `modules`,
`targets`, `omissions`, `diagnostics`, or `directory`; with no subject it
shows settings. The three workspace views answer different questions:

- `:show targets` lists the explicit module, file, and directory targets in
  admission order.
- `:show modules` lists the complete loaded source closure in deterministic,
  dependency-first order, including modules found through imports.
- `:show imports` lists the ordered prompt context that controls Exference
  name visibility and search.

## Source targets and dependency loading

`:load`, `:add`, and `:unadd` accept three target forms:

- a hierarchical module name such as `App.Main`, resolved as `App/Main.hs` or
  `App/Main.lhs` from the current source root;
- an existing `.hs` or `.lhs` file, which may declare a differently named
  module; or
- a directory, Djex's compatibility extension, recursively expanded to its
  source and `.ratings` files.

For example:

```text
:load App.Main test/Support.hs
:add "examples/With Spaces/Extra.hs"
:unadd test/Support.hs
:reload
```

Like [GHCi source loading](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#loading-source-files),
the loader follows resolvable local, non-package imports transitively and loads
the resulting graph dependency-first. Import cycles not broken by a
`{-# SOURCE #-}` import are rejected. An imported module found at the expected
hierarchical path must declare that module name, and two files may not declare
the same module. A non-package import with no local source remains unresolved
and is reported at the importing file by a
`DJEX_REPL_IMPORT_UNRESOLVED` warning; it contributes no declarations to the
session. These checks happen before the new workspace is published.

Filesystem targets are canonicalized when admitted. `:reload` therefore keeps
referring to the same files after `:cd`; it does not reinterpret the original
relative spelling in the new directory. `:show targets` uses canonical paths
for file and directory targets and retains the readable module spelling for a
module-name target. Recursive directory expansion skips directory symlinks so
it cannot escape or cycle outside the admitted tree; symlinks to regular
source or rating files remain valid and are canonicalized. Re-adding the same
canonical target is idempotent.
`:unadd` removes explicit targets, not arbitrary dependency modules; remove
the explicit targets that keep a dependency reachable if it should leave the
closure.

The target commands affect state as follows:

| Command | Target set | Prompt context after success |
| --- | --- | --- |
| `:load TARGET...` | Replace it. | Replace it with starred entries for the first explicit target that contributes source modules, or clear it when there is none. |
| `:load` | Clear it. | Clear it. |
| `:add TARGET...` | Append new canonical targets. | Replace it with starred entries for the last newly requested target that contributes source; retain the prior automatic target when none does. |
| `:unadd TARGET...` | Remove matching explicit targets. | Replace it with starred entries for the retained automatic target when it survives, otherwise the most recently admitted surviving target. |
| `:reload` | Re-read the retained targets. | Preserve and prune explicit context entries, then append fresh starred entries for the retained automatic target. |

Automatic-target choice is history-sensitive like GHCi: a fresh `:load` picks
the first contributing target, `:add` picks the last newly requested
contributing target, and reload/removal retain that choice while it survives.
If removal deletes it, the most recently admitted contributing target becomes
automatic. A file or module-name target contributes one module; a directory
contributes all of its loaded source files in deterministic order. All Djex
target modules are source-only, so automatic context uses full-source-scope
`*M` entries even when the target was written without `*`. A leading `*` on a
target is accepted and retained for GHCi-shaped syntax and display, but Djex
has no compiled-versus-interpreted loading mode for it to select.

## Prompt module context

Loaded modules and modules visible at the prompt are deliberately separate,
following [GHCi's scope model](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-scope).
Only loaded source modules can be added to Djex's context. A bare import
appends one entry, in order, and accepts the useful Haskell import forms:

```haskell
import Geometry
import qualified Data.Shapes
import Data.Shapes as Shapes
import Geometry (Point, translate)
import Geometry hiding (origin)
```

The declaration must parse and name a loaded source module. `qualified`, `as`,
an explicit import list, and `hiding` project the admitted unqualified and
alias-qualified names. Imports are cumulative; use `:module` when the context
must be replaced or an entry removed:

```text
:module Geometry Data.Shapes        -- replace the complete context
:module + Util                       -- append exported names from Util
:module + *Internal.Geometry        -- append every local declaration
:module - Geometry                  -- remove matching plain/starred entries
:module                             -- clear the context
```

Without a sign, `:module` replaces the entire context, including earlier bare
imports. `+` appends entries and `-` removes them. Plain `MODULE` exposes its
declared exports; `*MODULE` exposes the full source scope: private local
declarations plus names admitted by that module's own imports. Names from its
qualified imports remain qualified. Explicit module export lists are therefore
meaningful in the REPL rather than being rejected by the source loader.
Parent/child items such as `T(..)` and `C(method)` use the checked constructor
and class-method groups; record selectors are bundled children of their owning
datatype as well. Child imports such as `T(..)` or `T(child)` are rejected for
type synonyms and abstract datatypes, which have no checked children to
expose. Bundling provenance is retained across imports and re-exports:
exporting `T` and `child` separately does not authorize a downstream
`T(child)` import. `module M` re-exports retain the filters from the module's
source import of `M`. Every loaded module's export surface is validated during
the workspace transaction, including modules not selected by the prompt
context.

Scope controls unqualified lookup and the source bindings available to
Exference search. A current `*MODULE` entry gives that module's local names
precedence. Aliases introduced with `as` are valid qualifiers and respect the
entry's explicit list or `hiding` filter. A loaded name's canonical module
qualifier remains a full-inventory escape hatch even when the name is not
imported unqualified. This is deliberate compatibility behavior, not exactly
the visibility of a compiled Haskell module. Ambiguous unqualified use is
diagnosed when the query is parsed.

Djex's shared structural `Name` does not carry a separate Haskell namespace
tag. If a module has distinct type- and value-namespace entities with the same
canonical spelling, an import list cannot expose or hide those two identities
independently.

Module-context operations are transactional. A malformed import, an unknown
module, or an invalid import item leaves the previous context and searchable
session intact. `:browse MODULE` lists that module's exports,
`:browse *MODULE` includes its complete source scope, and bare `:browse` lists
the current visible scope. Bare `:browse` respects the active backend selection
and can therefore show Djinn's standard inventory, Exference's current scope,
or both. Supplying a module is an explicit source-workspace selector, so
`:browse M` and `:browse *M` show only Exference declarations regardless of
the active backend. The default prompt shows the backend selection rather than
the whole context; `:show imports` is authoritative.

Module context applies only to Exference. Djinn continues to parse and search
its independent standard checked session, including when `both` is selected.

## Inspecting expression types

`:type EXPRESSION`, abbreviated `:t EXPRESSION`, parses a term-level Haskell
expression, infers its type, and prints the trimmed source followed by `::` and
the inferred type. It never runs the expression. For example, loaded `id` and
`map` signatures permit queries such as:

```text
:t id
:t \x -> id x
:type map id
:type +d 1
```

Name lookup uses the current loaded module context, including unqualified
visibility, import aliases, explicit import lists, `hiding`, current-module
precedence, and canonical loaded-module qualification. Only term declarations
are candidates: ordinary value signatures, data constructors, and class
methods contribute types; a same-spelled type or class name does not make a
term available. A loaded source workspace is required.

Type inspection reads the authoritative neutral declaration inventory behind
the Exference runtime, not Exference's policy-filtered synthesis dictionary.
It therefore does not depend on whether `djinn`, `exference`, or `both` is the
active backend, and synthesis search controls do not change inference. The
shared `qualification` setting does control how type constructors and
constraints are printed.

The supported expression subset covers names and constructors, applications,
infix applications and sections, negation, lambdas, `if`, unguarded `case` and
lambda-case expressions, tuples and tuple sections, lists, enumerations,
parentheses, and ground expression type annotations. Ordinary character,
string, integer, and fractional literals are supported. Lambda and case
patterns may use variables, literals, prefix or infix constructors, tuples,
lists, parentheses, as-patterns, wildcards, irrefutable patterns, ground
pattern signatures, and bang patterns. A bare loaded name may retain and
display a higher-rank signature, but applying a higher-rank value is not
supported.

Loaded fixity declarations are not retained by the neutral inventory. An
unparenthesized chain of infix expressions or constructor patterns is therefore
rejected instead of risking the wrong association; add explicit parentheses to
state the intended tree. Negation immediately adjacent to an infix operator
must be parenthesized for the same reason.

Forms that require semantics outside this structural inferencer are rejected
explicitly. These include `let` and local `where` declarations, guarded case
alternatives, multi-way `if`, `do`/`mdo`, record construction, update, and
patterns, comprehensions and parallel arrays, unboxed sums, overloaded labels,
implicit parameters, visible type application, Template Haskell and
quasiquotes, arrow and XML syntax, typed holes, primitive literals, and
`n+k`, view, or regular patterns. Polymorphic expression and pattern
annotations are also rejected until the inferencer has skolemization and
context-entailment support. An unsupported form reports a structured diagnostic
and leaves the session usable.

Ordinary inference defaults an eligible numeric variable only when it remains
in constraints but not in the reported result type. Consequently `:t 1`
normally retains a `Num a => a` result, while an ambiguity used only internally
may default. `:type +d EXPRESSION` additionally defaults eligible numeric
variables that occur in the result. It tries the built-in candidates in
order—currently `Integer`, then `Double`—and selects the first whose loaded
instances satisfy every obligation. Only the canonical standard numeric
classes participate; a user-defined class that happens to be named `Num` is
not defaulted. This is intentionally narrower than GHCi's extended `+d`
defaulting for nonnumeric interactive classes. The former GHCi `+v` spelling
is deliberately rejected; plain `:type` already preserves eligible result
variables. If no loaded default type can satisfy an ambiguity that is absent
from the result type, inference reports that ambiguity instead of inventing
evidence.

## Settings and defaults

Use any of these forms:

```text
:set render expression
:set render=expression
:set +allow-unused
:set -allow-unused
:unset render
:set                            -- show every current value
```

Boolean values also accept `true`/`false` and `yes`/`no`. Invalid values emit a
diagnostic and leave the previous state unchanged.

| Setting | Accepted values | Default | Owner |
| --- | --- | --- | --- |
| `backend` | `djinn`, `exference`, `both` or a unique prefix | `djinn` | Routing |
| `target` | Unqualified Haskell value identifier or operator other than `_` | `djexResult` | Shared output |
| `select` | `first`, `best`, `all` | `first` | Shared presentation |
| `render` | `definition`, `expression` | `definition` | Shared presentation |
| `qualification` | `none`, `identifiers`, `full` | `full` | Shared presentation |
| `prompt` | Text; `%b` expands to the active selection | `"djex[%b]> "` | Interactive UI |
| `candidate-limit` | Positive integer | `200` | Djinn |
| `choice-budget` | Non-negative integer; `0` means unbounded | `0` | Djinn |
| `allow-unused` | Boolean | Off | Exference |
| `allow-constraints` | Boolean | Off | Exference |
| `constraint-deferral-steps` | Non-negative integer | `8192` | Exference |
| `multi-constructor-patterns` | Boolean | Off | Exference |
| `max-steps` | Positive integer | `65536` | Exference |
| `max-queue` | Non-negative integer or `unbounded` | `8192` | Exference |
| `max-depth` | Finite non-negative number or `unbounded` | `unbounded` | Exference |
| `fix` | Boolean | Off | Exference load policy |

The interactive default is `select = first`, unlike the one-shot command's
global-best default. It makes an exploratory prompt responsive and preserves
Exference's lazy result stream. `best` may need to consume the complete
configured search; `all` prints every admissible result. Rendering and
selection use the same checked presentation path as the one-shot frontend,
including residual-constraint reporting and truncation or evidence messages.

Changing a setting affects subsequent queries but does not mutate a sealed
backend session, except for `fix`, whose meaning belongs to Exference session
construction and therefore transactionally rebuilds the current workspace.
`:unset` restores the built-in value, not the corresponding startup option;
thus `:unset backend` returns to `djinn` even if the session started with
`--backend both`.

## Workspace replacement is transactional

The shared REPL always constructs Djinn's standard checked session. It loads
Exference separately from the installed directory target or
`--environment DIR` using the command-safe policy. A directory is admitted
through the same recursive target machinery used by an interactive `:load`;
it is retained as one explicit target even though it contributes several
modules and rating files. By default the policy excludes
`Data.Function.fix`, `Control.Monad.forever`, and
`Control.Monad.Loops.iterateM_`; `--fix` or `:set +fix` retains them.

An initial Exference load failure is reported, but Djinn remains usable and
the REPL still starts. Exference queries then explain that no environment is
available until a `:load` succeeds.

`:load`, `:add`, `:unadd`, `:reload`, and a change to `fix` first resolve and
parse every affected source, build the complete neutral inventory, apply
ratings and session policy, derive the prompt scope, and seal the replacement.
Each module and rating file is read strictly once per attempt. Dependency
discovery, retained module syntax, export visibility, ratings, and the sealed
inventory all consume that same immutable text snapshot, so an edit racing a
load cannot publish a session assembled from two file versions.
Only then are the target set, dependency closure, context, and searchable
session published together. On failure the prior Exference session, targets,
context, fix policy, and search settings remain active. The failed attempt's
diagnostics remain available through `:show diagnostics`. Rebuilding is
necessary for `fix` because source ratings and policy inputs are not
reconstructed from the annotation-erased sealed session.

On success, `:show environment` reports both independent declaration counts
and the active Exference workspace. `:show omissions` explains source
capabilities that could not enter Exference search. `:browse` displays
declarations rather than changing them; unlike the historical `djinn`
executable, the shared REPL has no mutable declaration editor.

## What source loading does not do

Djex's GHCi-shaped commands are a source-inventory frontend, not a compiler or
linker:

- only ordinary `.hs` and `.lhs` source modules are discovered;
- `.hs-boot` interfaces are not loaded; a `{-# SOURCE #-}` edge breaks
  dependency cycles but projects the checked implementation module's names
  rather than a separate boot-file export surface;
- there is no GHC package database lookup and no loading of `.hi`, `.o`, or
  shared-library code;
- a bare `import` or `:module` can mention only a module already present in the
  loaded source closure; and
- declarations and explicit signatures feed Exference's checked inventory,
  but ordinary function, method, and pattern-binding bodies are neither
  compiled nor used to infer missing signatures.

An unresolved external import contributes no declarations: sessions contain
only the workspace snapshots and structural built-ins. Package-qualified
imports are rejected with `DJEX_REPL_IMPORT_PACKAGE`: the shared canonical
name model cannot preserve package identity and must not silently bind a
same-spelled local module. Source that depends on external types or
declarations must provide enough loaded source vocabulary for Exference's
inventory checks, or loading fails with a structured diagnostic. No implicit
package `Prelude` is added to a prompt context.

Explicit module export lists are accepted. The loader retains a complete
module inventory so `*MODULE`, qualified parsing, and later context changes do
not require reparsing; the prompt-scope layer separately projects the public
exports for plain imports and plain `:module`/`:browse`. Other unsupported
nominal source constructs remain fail-closed as described in the
[library API guide](library-api.md).

## Multiline input, scripts, history, and the shell

Use GHCi-style explicit delimiters for a type spanning lines:

```text
djex[djinn]> :{
djex| (a, b)
djex|   -> (b, a)
djex| :}
```

The collected text is parsed as one logical input. EOF before `:}` reports an
unterminated block and exits. The same `:{` and `:}` structure works in script
files.

`:script FILE` reads the complete file before execution, then evaluates its
logical inputs in the current session. Setting and backend changes persist
after the script. Scripts can invoke other scripts, but a recursive inclusion
cycle is rejected. A command diagnostic normally leaves state unchanged and
execution continues; `:quit` in a script exits the enclosing REPL.

Haskeline keeps history for the current session automatically. Supply
`--history FILE` to load and save it across sessions, and inspect it with
`:history` or `:history N`. Command-aware tab completion and ordinary Haskeline
line editing are enabled.

`:! COMMAND` delegates one command to the platform shell. Its output is the
child process's output, and a launch or nonzero-exit failure becomes a REPL
diagnostic. The shell runs as a child: a shell `cd` cannot change the REPL's
working directory, so use `:cd` for that.

Pressing Ctrl-C interrupts the current read, query, or command, prints
`Interrupted.`, and returns to the prompt with the preceding REPL state. EOF
at the main prompt and `:quit` are successful exits.

## Diagnostics and process status

Parse, validation, load, search, rendering, setting, script, and shell failures
are rendered as structured Djex diagnostics, normally on stderr. They are
recoverable inside a session: the prompt continues, and the eventual process
status remains success when the user leaves with `:quit` or EOF. Both-mode
failure isolation follows the same rule.

Failures needed before the loop can exist, such as failure to build the
standard Djinn session or validate a requested history path, make `runRepl`
return a failure status. An Exference startup-load failure is intentionally not
one of these fatal conditions because the Djinn half is already usable.

This differs from one-shot operation. `djex djinn ...` and
`djex exference ...` write generated Haskell to stdout, diagnostics and
negative/search-status messages to stderr, and return exit 1 for a query/load/
render failure or exit 2 for malformed command arguments. Proof-backed
negative results and bounded no-result searches remain successful one-shot
outcomes.

## Embedding the REPL

`Language.Haskell.Djex.REPL` is an exposed module, separate from the
`Language.Haskell.Djex` facade:

```haskell
import Language.Haskell.Djex (Backend (..))
import Language.Haskell.Djex.REPL
  ( ReplBackend (..)
  , ReplOptions (..)
  , defaultReplOptions
  , runRepl
  )
import System.Exit (ExitCode)

runComparisonRepl :: IO ExitCode
runComparisonRepl = runRepl defaultReplOptions
  { replInitialBackend = BothBackends
  , replEnvironmentPath = Just "./environment"
  , replAllowFix = False
  , replHistoryFile = Just "./.djex-history"
  }
```

`OneBackend DjinnBackend` and `OneBackend ExferenceBackend` select one initial
engine; `BothBackends` selects both. `runRepl` returns an `ExitCode` instead of
terminating the host process, which makes it suitable for an application
launcher. It is still a terminal-oriented interface: it owns Haskeline input,
stdout/stderr presentation, process working-directory changes, and requested
shell commands. Applications needing custom transport or mutable caller-owned
state should embed the checked adapters documented in
[the library API guide](library-api.md) instead.

## Relationship to the historical frontends

The executable names intentionally expose three different contracts:

- `djex` is the shared persistent frontend described here, plus explicit
  one-shot `djinn` and `exference` subcommands.
- `djinn` is the historical compatibility REPL. It includes its own Djinn
  declaration editor and legacy command language, and it cannot query
  Exference. Its syntax and mutable-environment behavior should not be assumed
  for the shared REPL.
- `exference` is the historical compatibility one-shot command, not an
  interactive session.

This separation preserves existing scripts while giving new interactive use a
uniform command and presentation layer over the checked backend APIs.
