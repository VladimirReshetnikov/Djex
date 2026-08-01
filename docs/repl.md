# Shared Djex REPL

The `djex` REPL keeps Djinn and Exference available in one terminal session.
Its source-workspace and module-context commands deliberately follow the model
described in the
[GHC 9.12.4 GHCi User's Guide](https://downloads.haskell.org/ghc/9.12.4/docs/users_guide/ghci.html):
`:load`, `:add`, `:unadd`, and `:reload` control loaded source modules, while a
bare `import` or `:module` controls which of those modules is in scope at the
prompt. Djex also borrows GHCi's colon-command, completion, history, interrupt,
and multiline conventions. These commands are GHCi-inspired; they are not a
claim of command-for-command compatibility.

Djex is nevertheless not a drop-in GHCi evaluator. A bare line is a type that
Djex should inhabit, not a Haskell expression to evaluate. The synthesis
workspace reads type, data, class, instance, and signature information without
compiling function bodies. Loaded-workspace synthesis queries are parsed once
into Djex's shared type tree; the two engines retain different lowering and
search semantics. `:type` and `:kind` inspect that structural
workspace without invoking synthesis or executing code. The deliberately
explicit `:eval EXPRESSION` command is a separate boundary: it invokes the real
GHC API and may compile and execute loaded Haskell code.

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
| `--environment DIR` | Initial source-workspace directory target (the recursive directory compatibility form). | Installed Djex workspace |
| `--fix` | Retain the known nonterminating Exference recursion helpers while loading. | Off |
| `--history FILE` | Read and write Haskeline history in `FILE`. | In-session history only |
| `--ignore-startup` | Skip `.djexrc` startup files. | Startup files run |

After the banner and before the first prompt, the REPL runs `.djexrc` from the
home directory and then from the current directory. This ordering is
GHCi-inspired, not identical to GHC 9.12.4, which considers
`ghcappdata/ghci.conf` and then `./.ghci`; Djex deliberately uses two `.djexrc`
locations.
Each file is executed through the ordinary `:script` machinery: any REPL input
is accepted, failed lines report and continue, settings persist into the
session, and `:quit` exits. Missing files are skipped silently; path discovery,
canonicalization, read, and whole-script parse failures are diagnosed. A file
is announced only after it has been read and parsed, and `--ignore-startup`
suppresses both files. When the two paths resolve to the same file, it runs
only once. On POSIX, Djex adopts GHCi's
startup-file trust test: both the file and its containing directory must be
owned by the current user or root and must not be writable by the group or by
others. An unverifiable or untrusted file is skipped with a warning. Windows
lacks the corresponding ownership/mode check and accepts the file, following
GHCi's platform convention.

Run `djex repl --help` for the startup table. Options for a one-shot backend,
such as `--select` or `--max-steps`, are changed inside the REPL with `:set`;
they are not REPL startup flags.

A normal startup looks like:

```text
Djex REPL <package-version>
Djinn environment: <count> declarations (projected from the module scope, <count> omissions)
Source workspace: "/path/to/environment"
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

Both engines see the same loaded workspace and the same parsed query. Djex
parses the type once against the loaded source inventory and prompt module
context, which control unqualified source names and the declarations available
to search. Exference consumes that shared type directly. Djinn receives a
structural projection whose constructor and class names use its checked prompt
scope (see
[the Djinn scope projection](#the-djinn-scope-projection)), so a datatype
loaded with `:load` or exposed with `:module` is available to both backends.
The historical backend parsers remain available to standalone compatibility
clients and as a recovery path when the source runtime or requested Djinn
projection is unavailable, but are not a source of disagreement during the
normal loaded-workspace path.

Both engines lower class obligations to the same shared
`Constraint (Type variable)` representation, but a backend name still selects
an evidence policy. Exference can discharge exact givens, follow superclasses,
and resolve non-overlapping explicit instances. Djinn validates every context
class, arity, and argument kind, then searches only for an inhabitant that does
not use a class method. Consequently `Eq a => a -> a` is a useful Djinn query,
whereas `Monad m => a -> m a` does not receive `return` as an implicit proof
premise. Invalid or ill-kinded Djinn contexts are diagnosed; they are not
silently discarded.

### Rank-N and impredicative types

The shared parser enables `RankNTypes` and `ImpredicativeTypes`. Quantification
may therefore occur below arrows and type constructors, including a list of
Church booleans:

```haskell
[(forall result. result -> result -> result)]
```

Both engines open the leading quantifiers of the query itself. They also expose
deliberately bounded rank-N rule families:

- Djinn introduces a `forall`, including one with a validated class context,
  in a positive formula position. Arrow results, tuples, and datatype structure
  preserve polarity; crossing an arrow parameter reverses it. Context methods
  are not LJT premises, so the body must be dictionary-independent. Djinn also
  eliminates a hypothesis-side
  context-free chain of at most four leading binders by instantiating it at
  candidates the sequent itself supplies — goal variables, opened-forall
  skolems, premise-scope variables, and query-supplied subtrees that are
  independent of enclosing binders and contain quantification, including
  wrappers around quantified atoms. The last family gives a guarded form of
  impredicative instantiation.
- Exference can introduce a nested `forall`, with or without class contexts,
  when it reaches an active goal such as a callback argument or arrow result.
  It opens the complete leading chain with branch-local fresh rigids. Each
  substituted context is a lexical given for that body only, and each deferred
  obligation records its own given snapshot. Local methods, superclasses, and
  instances can therefore discharge body work without evidence leaking into a
  sibling goal. Flexible variables from an older scope may not be solved
  directly or indirectly to those rigids, so skolems cannot escape.
- Exference instantiates the complete leading `forall` chain of a scoped value
  freshly at each monomorphic use. Its direct contexts become proof
  obligations. Exact polymorphic forwarding takes priority. A context-free
  quantified provider with no free flexible variables may also be forwarded to
  a less-general such goal when shallow instantiation proves the relation
  without solving ambient inference variables; a provider binder may be solved
  impredicatively with a quantified subtree the requested scheme itself
  supplies.
- For a constrained scoped rank-N provider, Exference has an additional
  evidence-directed branch when a matching explicit ground instance head
  determines the complete leading binder prefix. That branch emits specified
  closed applications such as `provider @Int`; its type arguments are
  variable-free, `forall`-free monotypes. The ordinary implicit branch remains
  available and global bindings retain their existing behavior. Shared
  generated syntax can also carry the inferred argument `@_`, but Exference
  search emits only specified ground arguments from this rule.

For example:

```haskell
c -> (forall a. a -> a)
c -> (forall a. Eq a => a -> a)
((forall a. a -> a) -> c) -> c
(forall a. a -> a) -> b -> b
(forall a. a -> Maybe a) -> (forall b. b -> b) -> Maybe (forall b. b -> b)
(forall a. f a) -> f (Maybe (forall b. b -> b))
((forall a. a -> a) -> result) -> result
(forall a. a -> a) -> Int -> Int
(forall a b. a -> b -> a) -> (forall x. x -> x -> x)
```

Parameterized datatype applications have one additional Djinn boundary. The
historical structural projection still expands declarations into constructor
sums and is searched first. A query-directed backward slice then decides
whether the goal's positive demands reach a datatype with at least one
parameter. If so, a complementary nominal projection retains complete
saturated applications as alpha-aware atoms. For example, after loading

```haskell
data D a = EmptyD | FullD a
data R = R
data Token = Token

finish :: D (forall b. b -> b) -> R
token :: Token
poly :: Token -> (forall a. D a)
```

Djinn can find all three forms:

```haskell
(forall a. D a) -> D (forall b. b -> b)  -- \x -> x
(forall a. D a) -> R                     -- \x -> finish x
R                                        -- finish (poly token)
```

Run `:set select all` (or `:set select best`) to inspect these later nominal
candidates when `EmptyD` or `R` supplies an earlier structural inhabitant. The
interactive default `select = first` may stop before the nominal family.

The slice is computed after synonym expansion, so aliases of `D` are
transparent. Nullary datatypes keep their structural constructor formula, and
unrelated parameterized declarations do not add nominal plans to the query.
The nominal goal, loaded-premise cache, and hypothesis-instantiation axioms are
all compiled through the same view.

Loaded tuple elements, positive function results, and fields of an available
aggregate can continue the slice. A field is specialized from that value's
actual datatype occurrence; its declaration alone is not a global provider.
Function parameters introduced on the positive query path count as local
providers too, so a `Holder -> R` request can project a hidden consumer from
its `Holder` argument. Projection visits each datatype head at most once per
path to remain finite through nested or recursive shapes.

Visible recursive constructors can carry the same quantified payloads without
making recursion itself transparent. With `Seed` opaque and
`data Rec a = Done a | Again (Rec a)`, both engines can synthesize `Done` (or
its eta-expanded form) for:

```haskell
(forall x. (Seed -> x) -> x)
  -> Rec (forall y. (Seed -> y) -> y)
```

Djinn reaches that term through the first layer of `Rec`'s recursive SCC;
Exference uses the retained constructor binding. Djinn may compose one layer
from one other independent recursive SCC, but same-SCC revisits, a third SCC,
and every negative recursive occurrence stay opaque. Its exact fallback
preserves `Rec a -> Rec a` identity.

Exference can also inspect one layer of a parameterized recursive input while
preserving a quantified field at its exact specialization. For example, with

```haskell
data Headed a = HeadedValue a (Headed a)
```

the request
`Headed (forall x. x -> x) -> (forall x. x -> x)` can return the first field.
Enable `allow-unused` because the recursive tail is intentionally ignored. The
candidate is searched and independently checked with its complete typed
pattern; stable output then renders the unused tail binder as `_`. This is
still one-layer elimination, not recursion or general impredicative inference.

Every nested `forall` outside those boundaries is a shared opaque type atom.
Both engines can carry it through constructors, arrows, declarations,
equality, substitution, and rendering without decomposing it in ordinary
unification. For example, the Church-list encoding can be transported as a
value even though search does not derive its eliminator laws:

```haskell
forall item. (forall result. (item -> result -> result) -> result -> result)
          -> (forall answer. (item -> answer -> answer) -> answer -> answer)
```

Opaque atoms compare modulo lexical alpha-renaming. The two inner Church-list
types above are therefore equal despite `result` becoming `answer`. Binder
position, scope, and free-variable identity remain significant, so shadowing
and impredicative wrappers cannot accidentally capture or conflate variables.
Rendering chooses fresh binder spellings when a source hint would capture a
free name. These bounded rules do not add general higher-rank subsumption,
polymorphic-let generalization, or general visible type application. Explicitly
visible open arguments such as `@a` and visible impredicative type arguments
remain unsupported; the nominal rule above performs only bounded implicit
instantiation from the sequent's candidate vocabulary. Djinn search does not
gain the Exference ground-instantiation rule, and its historical expression
projection rejects visible type application explicitly.
Exference does not perform non-exact subsumption between contextual schemes,
while unexposed quantified atoms remain opaque; finite identifier or
search-budget exhaustion is an inconclusive truncation. In particular,
Djinn keeps constrained hypothesis occurrences and chains beyond four binders
opaque; if the bounded approximation finds no term, the result is inconclusive
rather than a proof of uninhabitability. Its plan family is deliberately cubic
rather than exponential. The historical prefix is fully opened,
exactly opaque, one plan per positive occurrence retained opaquely while the
others open, and the dual plans opening one occurrence while unrelated
siblings stay opaque. A deterministic tail makes the same opaque/open choices
for each unordered pair and triple. Opening nested occurrences includes the
union of their enclosing forall chains. A single proof can therefore mix exact
transport with structural introduction at sibling occurrences; the family
covers every combination of seven independent sites without enumerating a
power set.
The full historical structural no-axiom prefix runs before the focused nominal
family. Every nominal formula is tried plainly and, when an instantiation is
available, again with its separately compiled nominal axioms. Those attempts
are positive-only and share the one query-wide cutoff and fuel with structural
search: they may add independently checked candidates, but their failure never
supports a proof of uninhabitability. The structural base family still omits
central subsets from eight sites onward,
such as exactly four open and four opaque sites among eight, though
instantiable hypotheses often cover such middle subsets through bounded axiom
plans. Reusable loaded premises expose the same sound views
simultaneously. An incomplete primary premise also conservatively disables
negative evidence for the whole query, even when that premise would turn out
to be irrelevant.

Every proof that consumes instantiation evidence uses conservative no-eta
conversion. This retains a lambda when erasure would otherwise expose an eta
redex across a higher-rank application. In the
loaded example, the result stays `\x -> finish x`; contracting it to `finish`
is rejected by GHC's simplified subsumption. Selector projection normalization
uses the same boundary, so a required `\x -> field x` is not collapsed to
`field`. Generated code that transports quantified atoms or uses impredicative
instances may require `RankNTypes` and `ImpredicativeTypes`.
The projection and evidence boundary is detailed in the
[nominal parametric-data transport report](reports/2026-08-01-nominal-parametric-data-transport.md).
A generated visible application requires `TypeApplications`; its surrounding
rank-N provider signature commonly also requires `RankNTypes`, and an ambiguous
contextual signature may require `AllowAmbiguousTypes`.

In `both` mode Djex prints labelled sections in a deterministic order:

```text
-- Djinn
...
-- Exference
...
```

When the source workspace and its Djinn projection are available, the shared
query is parsed and kind-checked once, so a source diagnostic is printed once.
If either shared component is unavailable, both mode falls back to each
backend's legacy parser so the remaining session can still run. Backend
lowering and search are always independent: a Djinn diagnostic does not prevent
Exference from running, or vice versa. The sessions share the module scope, but
not caches or class-resolution policy; each seals its own independent
projection of the loaded declarations. In particular, an empty Exference
result is not upgraded to Djinn's proof-backed non-inhabitation evidence.

## Commands

Exact aliases win; otherwise any nonempty, unique prefix of a canonical
command is accepted. For example, `:q` is `:quit`, `:s` is deliberately
`:set`, and `:sy` is `:synth`. Tab completion offers commands, backend names,
settings, `:show` subjects, and paths where appropriate. Argument completion
uses the same command descriptor as parsing, so exact aliases and accepted
unique prefixes behave like the canonical command; `:module` completion also
preserves a typed `+`, `-`, or `*` marker. Completion follows the loaded
workspace, offering module names after bare `import`, `import qualified`,
`:module`, and `:browse`, and in-scope identifiers (qualified and unqualified)
at type-query, `:info`, `:type`, and `:kind` positions. Bare synthesis and
`:kind` completion use only the type namespace; expression, inspection, and
import-list positions retain both namespaces because their grammar can mention
either. Completed paths that need quoting are inserted as Haskell
string literals, matching the path grammar rather than shell escape syntax.

| Command | Purpose |
| --- | --- |
| `:add [TARGET ...]` | Add explicit source targets and reload their local dependencies. |
| `:backend [djinn\|exference\|both]` (`:b`) | Show or change the active selection. |
| `:browse [[*]MODULE]` | Browse the current scope, a module's exports, or a source module's full source scope. |
| `:cd DIR` | Change the process working directory without unloading the Djex workspace. |
| `:compare TYPE` | Run one query with both backends, sharing parsing when the source workspace and Djinn projection are available. |
| `:djinn TYPE` | Run one Djinn query. |
| `:download CABAL_TARGET ...` (`:dl`) | Ask Cabal to fetch targets and dependencies into its configured source cache. |
| `:exference TYPE` | Run one Exference query. |
| `:help [COMMAND]` (`:h`, `:?`) | Show the command summary or detailed command help. |
| `:history [N]` (`:hist`) | Show all history, or its last `N` entries, oldest first. |
| `:edit [FILE]` (`:e`) | Open the `VISUAL` (or `EDITOR`) editor on `FILE`, or on the most recently loaded file target. |
| `:eval EXPRESSION` | Evaluate a Haskell expression with real GHC and print its shown value. |
| `:info NAME` (`:i`) | Show exact-name declarations for the active backend(s). A constructor or class method resolves to its owning declaration. The Exference view also lists instances in which that datatype or class participates; Djinn's checked projection does not retain instances. |
| `:install [--lib] CABAL_TARGET ...` | Build and install package executables, or libraries with `--lib`. |
| `import DECLARATION` | Append a Haskell import to the shared prompt context. |
| `:kind[!] TYPE` (`:k`) | Infer a type's kind in the current loaded module scope; attached `!` also shows its synonym-normalized form. |
| `:load [TARGET ...]` (`:l`) | Replace the source target set and load local dependencies. No targets clears it. |
| `:module [+\|-] [[*]MODULE ...]` (`:m`) | Replace, add to, or remove from the shared prompt context. |
| `:pwd` | Print the process working directory. |
| `:quit` (`:q`) | Leave successfully. |
| `:reload` (`:r`) | Re-read the retained canonical source targets and dependencies. |
| `:script FILE` | Execute REPL inputs from a file. |
| `:set [OPTION [VALUE]]` (`:s`) | Show all settings, or change one. |
| `:show [SUBJECT]` | Show settings, loaded-source state, or selected backend state. |
| `:synth TYPE` (`:sy`) | Query the active backend(s). |
| `:type [+d] EXPRESSION` (`:t`) | Infer an expression's type in the current loaded module scope without evaluating it. |
| `:unadd [TARGET ...]` | Remove explicit source targets and reload the surviving dependency closure. |
| `:unset OPTION` | Restore one built-in default. |
| `:version` (`:v`) | Print the Djex package version. |
| `:! COMMAND` | Run a shell command. |
| `:` | Repeat the previous query. |

Target and path arguments may be bare words, Haskell string literals, or a
whole Haskell `[String]` value. The list form is particularly useful in
scripts. `:module+ M` and `:module- M` are accepted conveniences for the
canonical GHCi-style `:module + M` and `:module - M` spellings.

`:edit` treats `VISUAL`, falling back to `EDITOR`, as a whitespace-separated
executable and argument vector; use Haskell double-string literals for an
executable or fixed argument containing whitespace. Djex launches that
executable directly and appends the selected path as exactly one argument, so
shell metacharacters in a source filename are never evaluated. `:!` remains
the intentional shell-command boundary.

`:show` accepts `settings`, `backends`, `environment`, `imports`, `modules`,
`targets`, `omissions`, `diagnostics`, or `directory`; with no subject it
shows settings. The three workspace views answer different questions:

- `:show targets` lists the explicit module, file, and directory targets in
  admission order.
- `:show modules` lists the complete loaded source closure in deterministic,
  dependency-first order, including modules found through imports.
- `:show imports` lists the ordered prompt context projected into both
  backends. It controls Exference visibility and search and the corresponding
  Djinn declaration environment.

## Downloading and installing packages

Package operations have both scriptable and interactive forms:

```console
djex download CABAL_TARGET ...
djex install [--lib] CABAL_TARGET ...
```

```text
:download CABAL_TARGET ...
:dl CABAL_TARGET ...
:install [--lib] CABAL_TARGET ...
```

`download` invokes `cabal fetch -- CABAL_TARGET ...`. Cabal resolves
dependencies and fetches any needed source archives into its configured cache.
Ordinary `install` invokes
`cabal install --ignore-project -- CABAL_TARGET ...` and follows Cabal's
executable-install semantics. A leading Djex-owned `--lib` instead invokes
`cabal install --lib --ignore-project -- CABAL_TARGET ...` and selects Cabal's
library mode. The surrounding checkout's `cabal.project` therefore cannot
silently change either install plan. Cabal still applies its configured
repositories, solver, compiler, store, executable directory, and default GHC
package environment. Calling `install` directly may download anything not
already cached.

REPL Cabal targets use the same word, quoted-string, and whole `[String]`
argument forms as source commands. Empty targets and control characters are
rejected before Cabal starts. Djex uses a direct process argv, not a shell, and
inserts `--` before every target. The only recognized install option is a
leading `--lib`; write a leading `--` first to make even that spelling a target.
Other option-shaped values such as `"--dry-run"` are targets rather than
injected Cabal options. Cabal's stdout and stderr are streamed unchanged, while
its stdin is closed so a child cannot consume later REPL or script input.
Unrelated inherited file descriptors are also closed. `:d` is ambiguous
between `:djinn` and `:download`, while `:in` is ambiguous between `:info` and
`:install`. The exact `:e` alias means `:edit`; `:ev` and `:ex` are useful
unique prefixes for `:eval` and `:exference`. The exact `:i` alias continues
to mean `:info`.

`CABAL_TARGET` is intentionally broader than a Hackage package name. Cabal
accepts repository package/version and component selectors, local package
directories or `.cabal` files, local source archives, and source-archive URLs.
Relative paths are resolved from the REPL's current directory. Repository
security metadata protects repository-selected archives, but it does not
authenticate an arbitrary local path or URL supplied as a target.

These commands authorize external effects. Fetching uses the network according
to Cabal's configuration. Installing a Haskell package can execute code shipped
by that target through custom setup programs, hooks, preprocessors, compiler
plugins, Template Haskell, and build tools; it can also update Cabal's store and
executable or package environment. Hackage archive verification is not a build
sandbox and may not apply to the selected target. Inspect and trust a target
before running `install`, and use an operating-system sandbox when its build
must not see credentials, the network, or unrelated writable files.

Package-manager state and Djex source-workspace state are deliberately
independent. Success or failure does not change the selected backend, search
settings, last synthesis query, targets, loaded modules, or prompt imports.
More importantly, Cabal installation produces executables and/or compiled
package interfaces; Djex does not read the GHC package database or `.hi` files,
so an installed module does not become available to `import`, `:module`,
synthesis, or `:type`.
To expose an API to Djex, acquire compatible `.hs`/`.lhs` source (often a small
signature-stub environment is more practical than a package's complete source
tree) and admit the appropriate source directory with `:load` or `:add`.

The top-level forms propagate ordinary Cabal exit codes, return 1 if Cabal
cannot be launched, return 2 for malformed Djex arguments, and return 130 for
an interrupt. Inside the REPL the same structured failure is recoverable: the
prompt continues and a later `:quit` remains successful. Cabal runs in a
dedicated process group; Ctrl-C requests group interruption, waits briefly for
cleanup, then terminates Cabal if it has not exited. Windows job support extends
that termination to descendants. A process that deliberately detaches from its
group remains outside Djex's portable process-control boundary.

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

Like [GHC 9.12.4 GHCi source loading](https://downloads.haskell.org/ghc/9.12.4/docs/users_guide/ghci.html#loading-source-files),
the loader follows resolvable local, non-package imports transitively and loads
the resulting graph dependency-first. Import cycles not broken by a
`{-# SOURCE #-}` import are rejected. An imported module found at the expected
hierarchical path must declare that module name, and two files may not declare
the same module. A non-package import with no local source remains unresolved
and is reported at the importing file by a
`DJEX_REPL_IMPORT_UNRESOLVED` warning; it contributes no declarations to the
session. These checks happen before the new workspace is published.

Package-qualified imports are rejected instead of being treated as unresolved.
Djex's source inventory has no package-database identity to distinguish two
same-named modules, so accepting the import would make its nominal references
ambiguous. The failed load remains transactional like every other workspace
failure.

Once that closure is fixed, the shared source loader elaborates every
declaration in its defining module's own import scope. Bare imports entered at
the prompt and `:module` operate later: they select the query/search scope from
the already checked inventory and never reinterpret source declarations.

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

This is an intentional `:cd` divergence. GHCi 9.12.4 unloads the current
modules after changing directory; Djex retains its canonical source targets,
loaded workspace, and prompt context. Only later relative path arguments are
resolved from the new working directory.

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
following [the GHC 9.12.4 GHCi scope model](https://downloads.haskell.org/ghc/9.12.4/docs/users_guide/ghci.html#ghci-scope).
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

Djex's shared structural `Name` remains namespace-neutral, but every prompt
route retains whether that identity was admitted in Haskell's type namespace,
value namespace, or both. Thus `import M (T)`,
`import M (pattern T)`, and `import M hiding (pattern T)` remain distinct even
for `data T = T`, including through aliases and re-exports. `:kind` and query
type parsing consume only the type surface; `:type` and backend search consume
only the value/constructor surface. Djinn receives the two projections
separately, so hiding a constructor makes its datatype abstract rather than
silently reintroducing the constructor.

Module-context operations are transactional. A malformed import, an unknown
module, or an invalid import item leaves the previous context and searchable
session intact. `:browse MODULE` lists that module's exports,
`:browse *MODULE` includes its complete source scope, and bare `:browse` lists
the current visible scope. Bare `:browse` respects the active backend selection
and can therefore show Djinn's projected scope, Exference's current scope,
or both. Supplying a module is an explicit source-workspace selector, so
`:browse M` and `:browse *M` show only Exference declarations regardless of
the active backend. The default prompt shows the backend selection rather than
the whole context; `:show imports` is authoritative.

Module context changes apply to both backends: Exference reprojects its
search scope and Djinn reprojects its declaration environment from the same
visible names. Only when no source workspace is loaded at all does Djinn fall
back to its historical standard checked session.

## Inspecting type kinds

`:kind TYPE`, abbreviated `:k TYPE`, parses a Haskell type in the current
loaded module scope and reports its kind without running either synthesis
backend. Djex spells the proper type kind `Type`, retains genuinely free kind
variables, and displays a class head as a function ending in `Constraint`:

```text
:kind Maybe
Maybe :: Type -> Type
:kind Either Int
Either Int :: Type -> Type
:kind f a
f a :: k
:kind Functor
Functor :: (Type -> Type) -> Constraint
```

The bang is part of the command token. `:kind! TYPE` and `:k! TYPE` add an
`= TYPE` line whose saturated type synonyms have been expanded; `:kind ! TYPE`
instead passes `! TYPE` to the ordinary type parser. The normalized line uses
the current `qualification` setting and retains source variable spellings:

```text
-- Given type Text = [Char] in a loaded source module:
:kind! Text
Text :: Type
= [Char]
```

Djex permits an undersaturated synonym only when that synonym is the complete
input's operational head, matching the useful `:kind! Alias` case. Leading
context-free `forall` binders may wrap that head and are omitted from the
normalized presentation. Supplied arguments are still normalized, and an
undersaturated synonym nested inside another argument, function, tuple,
constrained forall, or non-head subtree is rejected. Kind checking happens
before expansion, so a phantom synonym cannot erase an ill-kinded argument.
Ordinary `:kind` performs the same saturation validation without constructing
or printing an expanded tree.

Lookup follows the same current-module precedence, unqualified visibility,
canonical loaded-module qualification, import aliases, explicit lists, and
`hiding` rules as synthesis and `:type`. Type constructors, synonyms, and
classes participate; term declarations do not become type names. Lowercase
spellings in type syntax remain ordinary free type variables. The command
reads the authoritative neutral inventory behind the loaded Exference runtime,
not its policy-filtered search dictionary, so changing `:backend` or search
settings cannot change the answer. It also does not replace the last synthesis
query repeated by `:`. A loaded source workspace is required.

This is a structural Djex kind inspector, not GHC's complete kind checker.
Type-family reduction, promoted types and DataKinds, explicit kind annotations,
unboxed tuple kinds, and runtime-representation polymorphism are not supported.
Synonym normalization does not reduce type families. `Constraint` is supported
as the result of the whole inspected class application, optionally beneath
leading context-free `forall` binders; nested Constraint-kinded class
constructors are rejected explicitly. `Constraint` is not added to the shared
synthesis kind tree. Parse, kind-inference, and normalization failures use
distinct recoverable diagnostics and leave the session unchanged.

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
implicit parameters, caller-written visible type application, Template Haskell
and quasiquotes, arrow and XML syntax, typed holes, primitive literals, and
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

## Evaluating expressions

`:eval EXPRESSION` compiles and runs one Haskell expression with the real
GHC that built Djex (through the GHC API, via hint) and prints its shown
value. It is the only command that executes code; synthesis queries and
inspection commands never do. Because bare input is a synthesis query, there
is no GHCi-style bare-expression evaluation — the command is always
explicit.

Evaluation targets the real package universe, not the synthesis environment.
Every module in the loaded dependency closure is compiled, but compilation
does not put every declaration in scope. Djex translates the current prompt
context to GHC's interpreter context:

- `*M` opens the full top-level scope of `M`, including its private
  declarations and the names admitted by its own imports.
- Plain `M` behaves as an ordinary `import M`, exposing only exports.
- Bare imports preserve `qualified`, `as`, explicit import lists, and
  `hiding` exactly; a loaded dependency does not leak merely because another
  target needs it.
- A `safe` import remains valid synthesis-scope syntax, but the evaluator's
  structured GHC context cannot preserve its safety flag. `:eval` refuses to
  weaken that request to an ordinary import and uses the complete
  Prelude-only fallback described below.
- With no starred module and no explicit Prelude entry, the installed Prelude
  is imported implicitly. A starred module instead supplies its own source
  scope, so `NoImplicitPrelude` remains effective.

Consequently a record admitted by the prompt scope can be built, projected,
and passed to a synthesized candidate spliced in with `let`:

```text
djex[djinn]> :eval let djexResult = unwrapped in djexResult (MkWrapped True)
True
```

The bundled synthesis environment is deliberately parser-level pseudo-Haskell
that real GHC rejects. If either the workspace or the translated prompt
context fails under GHC, evaluation resets the interpreter to Prelude-only
scope and one `DJEX_REPL_EVAL_SCOPE` advisory explains why; plain expressions
such as `:eval 1 + 1` keep working. The failed context is never partly
retained. A rejected expression is an ordinary `DJEX_REPL_EVAL` diagnostic.

Each `:eval` runs a fresh, isolated interpreter session that always reflects
the current workspace: bindings do not persist between evaluations and there
is no `it`. A diverging evaluation is interruptible with Ctrl-C. Evaluation
needs the GHC library directory recorded when Djex was built, so a relocated
installation without that toolchain reports a launch failure rather than
evaluating.

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

Boolean values also accept `true`/`false` and `yes`/`no`. The compact
`+NAME`/`-NAME` forms are available only for the boolean settings listed below;
using a sign with a non-boolean setting is rejected before value parsing and
leaves the previous state unchanged. Other invalid values have the same
transactional behavior.

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
| `djinn-axioms` | Boolean | Off | Djinn |
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

## The Djinn scope projection

Whenever a source workspace is loaded, the REPL projects the unqualified
prompt scope into a fresh Djinn session, so `:load`, `:add`, `:unadd`,
`:reload`, `import`, and `:module` change what both backends can see. The
projection reprojects on every scope change and is transactional: a failed
load, scope edit, or projection-affecting setting retains the previous
sessions and settings of both backends.

Djinn's declaration grammar is stricter than the shared neutral vocabulary,
so the projection degrades rather than fails:

- Names are projected at their unqualified in-scope spellings; a name whose
  unqualified spelling is ambiguous in scope is omitted.
- A visible recursive datatype retains its declaration and constructors. Djinn
  may expose one positive constructor layer from each of at most two distinct
  alias-normalized recursive SCCs on a logical path. Same-SCC revisits, a third
  SCC, negative occurrences, and the exact-opaque view keep the complete
  application atomic. The exact fallback consequently preserves identity such
  as `Rec a -> Rec a`. Any translation
  that touches this bounded rule is incomplete, so exhausting the search cannot
  establish non-inhabitation. `:show omissions` reports that its constructors
  are introduction-only.
- A datatype with hidden constructors is still projected as an opaque abstract
  type, so no hidden constructor can enter search or generated output. A later
  scope or sealing repair can likewise degrade a declaration before the
  recursive-introduction report is produced. Constructorless catalogue stubs
  have already become explicit abstract declarations at the visibility-aware
  source boundary, while genuine zero-constructor datatypes remain concrete and
  keep Djinn's explicit empty-case elimination. Every abstract replacement
  retains the exact kind inferred by the shared source inventory, including
  higher-kinded parameters.
- Instance declarations and classes with superclasses are omitted. Ordinary
  classes without superclasses remain available for validating Djinn query
  contexts.
- A value whose leading `forall` chain retains class constraints is omitted
  from Djinn's axiom projection. Stripping that context and admitting the value
  as an unconditional proof premise would be unsound. Context-free leading
  quantifiers are implicitized as Djinn assumptions, and nested rank-N atoms
  remain intact at the projection boundary. Djinn's polarized formula
  translation may subsequently open a supported positive atom even when its
  context is nonempty; that context is validated but supplies no proof premise.
  A Djinn query may also carry a prenex context, provided the synthesized
  inhabitant is dictionary-independent.
- Type constructors referenced from signatures but not declared in scope are
  stubbed as abstract types. A kind already inferred by the shared inventory is
  authoritative; an arity-derived kind is only the fallback for a genuinely
  absent external name. Type references remain distinct from value ownership
  during this repair, so a same-named data constructor does not suppress the
  abstract type stub required by Haskell's separate namespaces.
- Value declarations are omitted by default and become LJT axioms with
  `:set djinn-axioms on`. Axioms are off by default because even
  moderate axiom sets make Djinn's otherwise-terminating proof search
  intractable; structural proving over the projected datatypes stays fast.
- Scope-visible record selectors are the exception to the axiom policy exactly
  where they add proof power: a selector whose parent datatype Djinn cannot
  case-eliminate (recursive, hidden, or out-of-scope constructors) enters the
  session under either axiom setting, so a recursive or opaque record can stay
  field-accessible. Hidden selectors do not enter or leak into presentation.
  Selectors of fully eliminable records stay out of the axiom set — they would
  only multiply equivalent proofs of what structural elimination already
  derives.

The recursive-head set is not rediscovered from the already shaped prompt
declarations. It comes from the prepared Exference session's exact
deconstructor metadata after source-synonym expansion, then is translated
through the prompt renaming used by Djinn. Alias-hidden recursion therefore
cannot accidentally make a record appear eliminable or withhold its visible
selector. Recursive records are never classed as fully eliminable, so their
visible selectors remain axioms under either `djinn-axioms` policy.

Field projections are also normalized at presentation for both backends: a
candidate that eliminates a record merely to return one field, or that
faithfully rebuilds a deconstructed record, is shown as an application of
the field's scope-visible selector (for example `secondPart` rather than
`\a -> case a of MkPair _ c -> c`). When selectors are in scope, an
Exference `select first` request additionally looks a few candidates ahead
and shows the one with the smallest normalized spelling, because search
order distinguishes rebuild spellings that presentation renders identically
simple or not at all.

Every compromise is recorded: `:show environment` reports the projected
declaration and omission counts, and `:show omissions` lists each omitted
name with its reason next to Exference's own omissions. If projection fails
outright during an interactive mutation, the failure is reported and the
entire candidate state is rejected; Exference and Djinn therefore never
publish different prompt scopes. Only an initial startup projection failure,
where no earlier source state exists, falls back to Djinn's standard checked
environment while leaving the REPL available for recovery commands.

## Workspace replacement is transactional

The shared REPL constructs Djinn's standard checked session as the fallback
used until a workspace projection is available. It loads the shared source
workspace from the installed directory target or `--environment DIR` using
the command-safe policy. A directory is admitted
through the same recursive target machinery used by an interactive `:load`;
it is retained as one explicit target even though it contributes several
modules and rating files. By default the policy excludes
`Data.Function.fix`, `Control.Monad.forever`, and
`Control.Monad.Loops.iterateM_`; `--fix` or `:set +fix` retains them.

An initial workspace load failure is reported, but Djinn remains usable and
the REPL still starts. Exference queries then explain that no source workspace
is available until a `:load` succeeds.

`:load`, `:add`, `:unadd`, `:reload`, and a change to `fix` first resolve and
parse every affected source, build the complete neutral inventory, apply
ratings and session policy, derive the prompt scope, and seal the replacement.
Each module, rating file, and directory target's `*.visibility` manifest is
read strictly once per attempt. Dependency discovery, retained module syntax,
export visibility, constructorless-type classification, ratings, and the
sealed inventory all consume that same immutable text snapshot, so an edit
racing a load cannot publish a session assembled from different file versions.
Loading an individual source file or named module does not implicitly grant an
adjacent manifest authority; only an admitted directory discovers sidecars.
Only then are the target set, dependency closure, context, and searchable
session published together. On failure the prior workspace, backend sessions,
context, fix policy, and search settings remain active. The failed attempt's
diagnostics remain available through `:show diagnostics`. Rebuilding is
necessary for `fix` because source ratings and policy inputs are not
reconstructed from the annotation-erased sealed session.

On success, `:show environment` reports both declaration counts and the
active source targets. `:show omissions` explains both source
capabilities that could not enter Exference search and declarations the
Djinn scope projection had to omit or degrade. `:browse` displays
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

An unresolved external import contributes no loaded declarations: sessions
contain only the workspace snapshots and structural built-ins. A positive
import list nevertheless provides a finite interface spelling from which the
loader can preserve an external type constructor or class under its canonical
module name. An unrestricted or `hiding` import has no enumerable external
surface, and declarations that require absent inventory facts may still fail
with a structured diagnostic. Package-qualified imports are rejected with
`DJEX_REPL_IMPORT_PACKAGE`: the shared canonical name model cannot preserve
package identity and must not silently bind a same-spelled local module. No
implicit package `Prelude` is added to a prompt context.

Explicit module export lists are accepted. The loader retains a complete
module inventory so whole-environment validation, `*MODULE`, and later context
changes do not require reparsing; the prompt-scope layer separately projects
the public exports for plain imports and plain `:module`/`:browse`. In prompt
queries only, canonical qualification remains a deliberate full-inventory
escape hatch. Source declaration elaboration has no such escape: its loaded
module scope is exact. A `module M` export item includes exactly the identities
available both unqualified and through the written qualifier `M`; self locals
and unqualified aliases therefore work, while qualified-only imports do not
leak. Other unsupported nominal source constructs remain fail-closed as
described in the [library API guide](library-api.md).

## Multiline input, scripts, history, and the shell

Use GHCi-style explicit delimiters for a type spanning lines:

```text
djex[djinn]> :{
djex| (a, b)
djex|   -> (b, a)
djex| :}
```

The collected text is parsed as one logical input. Haskell `--` line comments
are removed from bare type and import input; colon-command payloads remain
literal. EOF before `:}` reports an unterminated block and exits. The same
`:{` and `:}` structure works in script files.

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

Pressing Ctrl-C interrupts the current read, query, or ordinary command, prints
`Interrupted.`, and returns to the prompt with the preceding REPL state. A
package operation instead reports `DJEX_PACKAGE_INTERRUPTED` after its managed
process-group cleanup. EOF at the main prompt and `:quit` are successful exits.

## Diagnostics and process status

Package, parse, validation, load, search, rendering, setting, script, and shell
failures are rendered as structured Djex diagnostics, normally on stderr. They
are recoverable inside a session: the prompt continues, and the eventual
process status remains success when the user leaves with `:quit` or EOF.
Both-mode failure isolation follows the same rule.

Late argument validation retains the command's diagnostic family:
`:backend`, `:show`, `:info`, and `:history` use `DJEX_REPL_BACKEND`,
`DJEX_REPL_SHOW`, `DJEX_REPL_INFO`, and `DJEX_REPL_HISTORY`, respectively;
an unknown `:help` subject is a `DJEX_REPL_COMMAND` error.
`DJEX_REPL_SETTING` is reserved for `:set` and `:unset`, including an invalid
sign form.

Failures needed before the loop can exist, such as failure to build the
standard Djinn session or validate a requested history path, make `runRepl`
return a failure status. An Exference startup-load failure is intentionally not
one of these fatal conditions because the Djinn half is already usable.

This differs from one-shot operation. `djex djinn ...` and
`djex exference ...` write generated Haskell to stdout, diagnostics and
negative/search-status messages to stderr, and return exit 1 for a query/load/
render failure or exit 2 for malformed command arguments. Proof-backed
negative results and bounded no-result searches remain successful one-shot
outcomes. Top-level package operations follow the exit-status contract in
[Downloading and installing packages](#downloading-and-installing-packages).

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
  , replIgnoreStartupFiles = False
  }
```

`OneBackend DjinnBackend` and `OneBackend ExferenceBackend` select one initial
engine; `BothBackends` selects both. `runRepl` returns an `ExitCode` instead of
terminating the host process, which makes it suitable for an application
launcher. It is still a terminal-oriented interface: it owns Haskeline input,
stdout/stderr presentation, trusted startup-file execution, process
working-directory changes, and requested evaluation, editor, shell, and Cabal
package commands. Applications needing custom transport or mutable
caller-owned state should embed the checked adapters documented in [the
library API guide](library-api.md) instead.

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
