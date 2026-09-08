# Parenthesized forall checking and the remaining Church search miss

The behavioral worker now accepts enclosing parentheses around an explicit
`forall` without losing lexical access to its type variables. This repairs a
preflight regression exposed while rerunning the original `maybeEither`
fixture. The Church operation itself remains unaccepted.

## Checking correction

The worker protects a candidate from accidental recursive capture by binding
it under a fresh name, then giving the predicate's requested name an alias at
the same complete type. That alias explicitly forwards the signature's type
arguments, which is necessary for parameters occurring only in constraints.

GHC distinguishes these signatures for lexical `ScopedTypeVariables`:

```haskell
f :: forall z a. (z -> a) -> z -> a
f :: (forall z a. (z -> a) -> z -> a)
```

They describe the same type, but the second spelling does not bring `z` and
`a` into scope over the binding's right-hand side. The worker previously
extracted both binders through the parentheses and then used `@z @a` in the
alias, causing preflight to fail before search. An independent GHC probe
reproduces the failure; the preserved Church command was unchanged.

The private checking signatures now remove only enclosing type parentheses.
Inner quantifiers, constraints and binder order are preserved. The original
requested type, emitted candidate and search settings are unchanged. This is
not implicit-root synthesis or a general solution for scoped names in ordinary
contextual output.

The new CLI regression exercises both engines with a doubly parenthesized
`forall z a. (z -> a) -> z -> a`. It uses the synthesized function at `Int/Bool`
and `Bool/Int`, checks actual False rejection, and executes the exact displayed
definition in a separate GHC process under the original parenthesized signature.

## Search diagnosis

Temporary observation of reference-compatible branches used the real loaded
environment and original 256-candidate prefix. It neither supplied the
reference as a provider nor filtered or ranked search nodes. The captured
prefix matches all 256 earlier candidate expressions, admission steps and
graph-availability results. It observes 3,346 batches and retains
unchanged source, library, executable, tool and input hashes during execution.
This diagnostic does not evaluate the behavioral predicate.

At step 73 the search has already constructed this partial expression, up to
bound-variable renaming and explicit type arguments:

```haskell
\e zero some ->
  e (\m -> m zero (\x -> some leftHole))
    (\m -> m zero (\y -> some rightHole))
```

Both holes have the required polymorphic `Either` type. Opening the first
injection changes the branch priority from approximately -38.08 to -60.28:
the heuristic charges the newly exposed handler types as pending construction
work. No compatible descendant is popped again before the original observation
window ends. This identifies an actual scheduling obstacle; the capture does
not determine whether that descendant remains queued or is later pruned, nor
does it exclude other successful derivations.

A generic experiment estimated a function goal from its resulting body,
treating introduced parameters as supplied values and forall-bound variables
as future rigids. It preserved all search alternatives and existing limits.
After the worker fix, both baseline and experiment checked 256 candidates,
all false, with zero errors and timeouts. Both retained independent positive
and wrong-reference controls and a separate live False query. The experiment
was removed; the production search source is byte-identical to the baseline.
The earlier experimental run stopped at the worker's preflight regression and
is retained separately, not counted as a search comparison.

The next search investigation is the exact branch's queue retention/pruning
and the competing completions occupying the observation window. Correcting
the observed priority estimate alone did not establish useful synthesis.
The full 13 extended operations and 19 supplied-default counterparts across
both Haskell engines and all three Lean modes remain required by the
[current roadmap](2026-09-07-synthesis-retriage.md).

## Validation

The strict serial build passes with `--ghc-options=-Werror`. All **2,307 tests
in 15 complete suites** pass, including the full 103-test CLI suite. Their
summaries total 429.63 seconds. Each passing count matches its complete unique
test inventory; source, executable and helper hashes remain unchanged.

The [receipt](../../test-church/receipts/parenthesized-forall-checking.json)
contains the regression inventories and process captures, the source changes,
the diagnostic observer and rejected experiment patches, the complete prefix
comparison, and both behavioral comparisons with their controls. It also
preserves the earlier experimental preflight failure, compile correction, and
the new test's initial definition-extraction failure. The corrected test
accepts a displayed definition with ordinary value parameters and replays that
exact line; it does not require the unrelated spelling `f = ...`.

Reproduce the regression through `cabal test` with the receipt's
`regression.selected_suites`, `-j1`, and
`--test-options="-j1 --color=never --timeout=180s"`. Direct test execution also
requires Cabal's `djex_datadir` and the built helper commands on `PATH`.
The receipt embeds the driver used for this run.

Leant's companion update is documentation only. Its Djex `4a4ed0fc` pin and
native acceptance remain separate; this run does not claim a newer native
integration or completion of priorities 2–4.
