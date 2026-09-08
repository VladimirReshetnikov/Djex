# Conditional provider use under root Givens

Djinn can now synthesize a call to a constrained local or global provider using
the query's actual root dictionary assumptions. For example, with only an
empty class `C` and an abstract `Token`, it can construct a source-checked
implementation of:

```haskell
forall a. C a => (forall b. C b => b -> Token) -> a -> Token
```

The corresponding global-provider case supplies only the declaration
`token :: forall b. C b => b -> Token` to search. Neither fixture supplies a
class method, instance, concrete `Token`, or target implementation. Full-source
GHC replay checks the actual synthesized program independently.

## Source and proof boundary

The provider's complete quantified and qualified scheme remains one opaque
source proposition. A conditional specialization has a separate rule:

```text
exact provider scheme -> dictionary 0 -> ... -> specialized body
```

Dictionary propositions are disjoint from ordinary source values, including
values whose type is itself qualified. Every leading type argument is selected
in one correlated vector. The admission gate preserves the original kinds of
both provider binders and ambient query variables; a substitution cannot retune
either merely because the resulting whole type happens to be well-kinded.

The source root and its dictionary telescope are opened together. Dictionaries
remain actual lexical assumptions in that proof, rather than global premises.
The existing finite proposal pool bounds conditional specializations, and
batch/stream search uses the caller's existing choice and raw-candidate limits.
Root introductions are charged before proof search. Conditional plans provide
positive evidence only.

After independent propositional checking, lowering removes only the checked
root dictionary prefix and the corresponding helper arguments. Each removed
argument must name its actual root binder and have its exact dictionary
proposition. Ordinary binders, argument order, and complete provider identity
are preserved. Dictionary escape, shadowed authority, foreign equal premises,
unsaturated helpers, and evidence from another proof environment are rejected.
The final source checker must still accept the exact original qualified type
and associate the complete term graph with the emitted clause.

Three already-private modules moved from `djinn/src-core` to
`djinn/src-internal` without content changes so the private tests can exercise
this lowering directly. Their Cabal visibility and public API are unchanged.

## Truthful negative evidence

Class methods remain outside Djinn's search vocabulary. Consequently, an
exhaustive projected search cannot refute a source type that may use them.
For `class C a where make :: a -> Token`, both of these types have source
inhabitants:

```haskell
root   :: forall a. C a => a -> Token
root   = make
nested :: () -> (forall a. C a => a -> Token)
nested _ = make
```

Retained qualifications anywhere in the source type, and historical requests
carrying contexts separately, therefore disable source negative authorization.
A miss reports `NoEvidence` rather than `ProvedUninhabitable`; the raw facade
reports `Undecided` rather than `Unrealizable`. The same inventory still permits
a genuine unconstrained `A -> Token` refutation. This change preserves candidate
checking, search budgets, and the intentional omission of methods.

## Acceptance and remaining limits

The focused contextual target passes all sixteen tests in 23.56 seconds,
including local/global provider use in both engines, leakage controls,
independent GHC replay, and root/nested omitted-method evidence checks through
batch and stream under both search strategies. All 92 private source-graph
tests pass, including 26 conditional preparation/kind/proof tests and eleven
direct adversarial dictionary-erasure tests.

The production budget matrices use caps 1, 2, and 8 with choice budgets
0, 1, 2, 4, and 40. A separately measured duplicate boundary uses raw cap 3
and 64 choices: both APIs return exactly two distinct, source-checked programs
with `CandidateLimitReached`. The duplicate consumes its original slot.
These tests observe the actual public API; it does not expose an exact consumed
choice counter, so the tests do not claim one.

Strict builds pass with `-Werror -j1`. The
[aggregate receipt](../../test-integration/receipts/conditional-givens-checkpoint.json)
records **2,168 passing tests across twelve complete affected suites**. This
combines nine complete passing suites from the first run with the complete
corrected Djinn unit, facade, and historical Djinn CLI suites; it is not one
unfiltered twelve-suite invocation. The full 432-test Length suite passes in
60.39 seconds in the first run.

The [first run](../../test-integration/receipts/conditional-givens-before-fixture-correction.json)
exposed older expectations of the unsound method-based refutations described
above. Only three test files changed afterward; production source stayed
identical. Those corrections preserve the absence of method candidates and
all original search bounds. The
[complete reruns](../../test-integration/receipts/conditional-givens-corrected-suites.json)
pass all 133 Djinn unit tests, 139 facade tests, and 24 historical CLI tests.
Receipts retain source and executable hashes, commands, and exact suite
summaries. The [current re-triage](2026-09-07-synthesis-retriage.md) records the
remaining integration and capability work.

This increment does not support duplicate equal root Givens, conditional use
under arbitrary nested qualifications, general partial constrained type
instantiation, class-method search, instance/superclass derivation, or contextual
certificate association. Duplicate slots need the proof's actual selection
retained through graph construction. Partial instantiation must preserve its
residual forall and dictionary obligations. Vacuous selected type slots remain
conservative until kind checking and structural preparation share one checked
entrance. Production Lean routing still needs exact source metadata and explicit
dictionary projection; the isolated renderer is separate evidence.
