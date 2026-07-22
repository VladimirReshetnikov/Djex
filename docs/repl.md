# Shared Djex REPL

The `djex` REPL keeps Djinn and Exference available in one terminal session.
It borrows GHCi's prompt, colon-command, completion, history, interrupt, and
multiline conventions, but it is not a GHCi-compatible evaluator. A bare line
is a type that Djex should inhabit, not a Haskell expression to evaluate, and
the two synthesis engines deliberately retain different parsers and search
semantics.

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
| `--environment DIR` | Exference Haskell-source environment directory. | Installed Djex environment |
| `--fix` | Retain the known nonterminating Exference recursion helpers while loading. | Off |
| `--history FILE` | Read and write Haskeline history in `FILE`. | In-session history only |

Run `djex repl --help` for the startup table. Options for a one-shot backend,
such as `--select` or `--max-steps`, are changed inside the REPL with `:set`;
they are not REPL startup flags.

A normal startup looks like:

```text
Djex REPL <package-version>
Djinn session ready (standard checked environment).
Exference environment: /path/to/environment
Type :help for help.
djex[djinn]>
```

The placeholder above is the installed package version. The default prompt
contains `%b`, which is replaced dynamically by `djinn`, `exference`, or
`both`.

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
environment. A spelling can therefore be valid for only one backend or mean
something supported by only one search engine.

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
| `:backend [djinn\|exference\|both]` (`:b`) | Show or change the active selection. |
| `:browse` | List declarations for the active backend or both selected backends. |
| `:cd DIR` | Change the process working directory. |
| `:compare TYPE` | Run one independently parsed query with both backends. |
| `:djinn TYPE` | Run one Djinn query. |
| `:exference TYPE` | Run one Exference query. |
| `:help [COMMAND]` (`:h`, `:?`) | Show the command summary or detailed command help. |
| `:history [N]` (`:hist`) | Show all history, or its last `N` entries, oldest first. |
| `:info NAME` (`:i`) | Show exact-name declarations for the active backend(s), including constructors and class methods. |
| `:load DIR` (`:l`) | Load and seal an Exference source environment. |
| `:pwd` | Print the process working directory. |
| `:quit` (`:q`) | Leave successfully. |
| `:reload` (`:r`) | Reload the current Exference directory and fix policy. |
| `:script FILE` | Execute REPL inputs from a file. |
| `:set [OPTION [VALUE]]` (`:s`) | Show all settings, or change one. |
| `:show [SUBJECT]` | Show settings or selected state. |
| `:synth TYPE` (`:sy`) | Query the active backend(s). |
| `:unset OPTION` | Restore one built-in default. |
| `:version` (`:v`) | Print the Djex package version. |
| `:! COMMAND` | Run a shell command. |
| `:` | Repeat the previous query. |

Paths may be written directly or as a Haskell string literal when quoting or
escapes are useful. `:load` always means an Exference environment directory;
use `:script` for a file of commands. `:show` accepts `settings`, `backends`,
`environment`, `omissions`, `diagnostics`, or `directory`; with no subject it
shows settings.

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
construction. `:unset` restores the built-in value, not the corresponding
startup option; thus `:unset backend` returns to `djinn` even if the session
started with `--backend both`.

## Exference environments are transactional

The shared REPL always constructs Djinn's standard checked session. It loads
Exference separately from the installed environment or `--environment DIR`
using the command-safe policy. By default that policy excludes
`Data.Function.fix`, `Control.Monad.forever`, and
`Control.Monad.Loops.iterateM_`; `--fix` or `:set +fix` retains them.

An initial Exference load failure is reported, but Djinn remains usable and
the REPL still starts. Exference queries then explain that no environment is
available until `:load DIR` succeeds.

`:load`, `:reload`, and a change to `fix` parse the source, build the neutral
inventory, apply policy and ratings, and seal a complete replacement before
publishing it. On failure the prior Exference session, directory, fix policy,
and search settings remain active. The failed attempt's diagnostics remain
available through `:show diagnostics`. This reload is necessary for `fix`
because source ratings and policy inputs are not reconstructed from the
annotation-erased sealed session.

On success, `:show environment` reports both independent declaration counts
and the active Exference path. `:show omissions` explains source capabilities
that could not enter Exference search. `:browse` displays declarations rather
than changing them; unlike the historical `djinn` executable, the shared REPL
has no mutable declaration editor.

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
