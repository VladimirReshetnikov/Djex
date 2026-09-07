# Executable behavioral constraints

The shared REPL accepts a named implementation and a Haskell Boolean predicate:

```haskell
:load
:synth choose :: forall a. a -> a -> a where choose True False == False
:synth identity :: forall a. a -> a where identity (42 :: Int) == 42
```

For these self-contained examples, bare `:load` clears the default declaration
models and establishes an empty compilable workspace. A project query should
load its actual Haskell modules, because checking executes their retained source
snapshots; signature-only synthesis models cannot supply runtime definitions.

`:djinn`, `:exference`, and `:compare` accept the same syntax. The selected
backend still synthesizes from the type and current declaration inventory.
The predicate is checked separately and does not supply implementations or
providers to synthesis. Ordinary unnamed queries keep their existing meaning.
The existing `--where CLAUSE -- TYPE` syntax remains the separate symbolic
Length/Z3 path for both engines, with its existing execution policy and
evidence requirements.

A named query checks the predicate's type before searching. Each candidate is
then compiled with the original polymorphic type annotation and evaluated in
the predicate. Only `True` admits the complete candidate handle to presentation.
`False`, a compilation error, a runtime exception, an unavailable worker, and a
timeout are distinct observations. None proves that the constrained problem
has no solution. The predicate may express examples, a finite oracle comparison,
or any other Haskell expression returning the real runtime `Bool`; a source
type alias also named `Bool` does not change that requirement.
The interpreter's result annotation uses a private helper exporting only a
type alias for package-qualified `base`'s `Prelude.Bool`. It does not rely on
Hint's unqualified `Typeable` spelling, and adds no synthesis value providers.

One child process and GHC interpreter session belongs to each backend query.
It compiles immutable copies of the retained workspace sources and installs
the exact prompt import surface once. Candidate bindings remain inside each
checked expression, so an earlier candidate cannot become a later provider.
A fresh outer binding protects the candidate from capture by the predicate's
named function. Named behavioral output uses fully qualified global references
so a displayed definition cannot accidentally recurse into its own name.
An incompatible scope or a source workspace that real GHC cannot compile fails
closed; this path never falls back to Prelude.

`:set quality-window N` bounds raw candidate observations, including `False`,
errors, and duplicates, for every behavioral selection mode. `first` stops at
the first passing candidate; best/all selection stays inside that same window.
Djinn enables alternative enumeration for a constrained first query and yields
checked typed candidates incrementally, retaining its configured candidate
cutoff and choice budget. Exference retains its step,
depth, and queue bounds. Predicate rejection never refills an exhausted search
or observation allowance. The `DJEX_REPL_BEHAVIORAL_OBSERVATIONS` diagnostic
reports the counts, and `DJEX_REPL_BEHAVIORAL_NO_MATCH` identifies an observed
search with no accepted candidate.

Worker startup and each candidate compilation/evaluation have a 30-second wall
deadline. `:set timeout N` can stop the encompassing operation earlier;
`:set timeout 0` removes that encompassing limit.
Timeout or protocol failure retires the child; it does not start a new worker
with weaker context. UTF-8 protocol lines and responses are bounded, diagnostic
output is drained without an unbounded buffer, and owned child process groups
are cleaned up on cancellation. This provides execution isolation and bounded
waiting, not an operating-system security sandbox: use predicates and loaded
Haskell source that you trust. A finite successful observation is not a proof
of totality or a universal behavioral law.

The implementation is covered by focused CLI regressions for predicate typing,
rejection before presentation, runtime failures, repeated queries, exact scope,
same-named providers, polymorphic annotations, and worker binding isolation.
Runtime acceptance of the six Church operations is recorded separately by the
behavioral corpus harness; this guide does not substitute source inspection for
those execution receipts.
