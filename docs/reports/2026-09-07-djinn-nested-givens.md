# Guarded nested-Given provider use

Djinn now supports conditional provider use beneath monomorphic nested Givens,
with conservative admission guards and independent source checking. This is
an accepted increment of [contextual synthesis](2026-09-07-synthesis-priorities-1-4.md),
following [root-Given provider use](2026-09-07-djinn-conditional-givens.md).
Full contextual synthesis and the larger four-priority goal remain in progress.

## Behavior and rationale

The new nested qualification may refer to variables jointly opened from the
original query root. For example, the forced local-provider target is:

```haskell
forall a. (forall b. C b => b -> Token)
       -> a -> ((C a => Token) -> Token) -> Token
```

The callback's `C a` is available only while constructing that qualified
argument. A corresponding test requires an exact loaded global provider.
Acceptance requires actual provider use, the original complete source graph,
its exact clause erasure, and independent GHC replay at the full signature.
An opaque forwarding term cannot satisfy these forced targets.

[Preparation](../../djinn/src-core/Djinn/Internal/Environment.hs) retains the
original qualification and joint ambient kind scope. A
[checked lexical introduction rule](../../djinn/src-internal/Djinn/Internal/ContextualInstantiation.hs)
accepts a proof that binds the required dictionaries. A conditional
specialization rule consumes the unchanged opaque provider scheme followed by
its ordered dictionary arguments. Neither adds an unconditional provider body
or a dictionary to a query-wide pool.

[Checked erasure](../../djinn/src-internal/Djinn/Internal/SourceEvidence.hs)
requires the actual dictionary-lambda prefix and references to those exact
lexical binders. It removes only that evidence and the checked helper syntax,
preserving ordinary arguments and their order. A valid propositional proof
outside this supported source fragment is omitted as a refused raw candidate;
later supported candidates can still survive. A mismatched checked environment,
helper formula, or source association remains an internal failure.

The source checker currently reconstructs the nearest matching lexical Given.
It does not yet transport every proof-selected dictionary occurrence and
ordered slot. The admission guard therefore refuses equal active Givens before
they can be silently reassigned. It inspects residual qualified results,
explicit type arguments, and reachable checked constructor fields, using
alias-expanded declarations and capture-avoiding specialization. Sibling scopes
remain separate. Exact recursive field states terminate inspection; changed
arguments, scope, or polarity conservatively refuse the optional contextual
plan. Shared acyclic declaration paths may be revisited: this is a finite
traversal, without a linear-time or memoization claim.

## Hidden qualifications cannot authorize refutation

A separate correctness fix covers qualification absent from the surface query
but introduced by a constructor field:

```haskell
class C a where make :: a -> Token
data Hidden a = Hidden (C a => a -> Token)

witness :: Hidden A
witness = Hidden make
```

With `A` and `Token` abstract to synthesis and no ordinary providers, the
current candidate vocabulary omits `make`. Searching the stripped field body
can therefore find no term even though the complete source has this inhabitant.
The [polarized translation](../../djinn/src-core/Djinn/Internal/TypeFormula.hs)
now marks every opened positive qualification with omitted dictionary premises
as incomplete, including after alias or constructor expansion. It preserves
the body formula, skolems, and occurrence order. An empty search then supplies
no source-level refutation. The same inventory still supports genuine negative
evidence for the unrelated unqualified query `A -> Token`.

This completeness flag can enable existing fallback formula plans; unchanged
formulas do not imply an identical finite candidate prefix. No new search
grammar, limit, or hidden allowance is introduced.

## Scope, API, and accounting

- The new supported scope is monomorphic qualification over jointly opened
  root variables. Fresh eigenvariables beneath nested `forall`, equal active
  dictionary choices, unsupported recursive field states, and wholly vacuous
  leading provider binders remain refused.
- Constructor-field inspection is a correctness guard. This increment does
  not add general constructor-field introduction, instance/superclass search,
  class-method synthesis, or general partial constrained specialization.
- Checked session/query signatures, user syntax, query options, and shared
  graph node forms do not change. Backend preparation gains nested-opening
  receipts; lowering distinguishes unsupported erasure from malformed checked
  evidence. Datatype and synonym parameter-kind syntax is unchanged.
- [Core integration](../../djinn/src-core/Djinn/Core.hs) uses the existing
  bounded type-vector proposals and shared raw-candidate and choice allowances.
  Each searched proof remains charged even when lexical erasure rejects it.
  Batch and stream execution do not restart or refill after rejection.
  Contextual plans confer no negative evidence.
- Production Lean integration is a separate gate. Exact selected-slot
  transport through lowering, normalization, and graph reconstruction remains
  required before removing overlap restrictions; see the
  [source-typed graph guide](../source-typed-evidence-graph.md).

## Acceptance

The [checkpoint receipt](../../test-integration/receipts/guarded-nested-givens-checkpoint.json)
records twelve complementary complete suites over unchanged production:
**1,048 Tasty tests, 700 Church signature checks, and 100 rank-N scope probes**.
This combines ten passing suites from the broad run with complete private and
facade reruns after a fixture-only correction. It is not one all-pass invocation.

| Complete suite | Passing inventory |
| --- | ---: |
| Private source graphs | 106 |
| Public source graphs | 59 |
| Djinn unit | 133 |
| Djinn frontend API | 5 |
| Djex API | 38 |
| Djex facade | 147 |
| Djex CLI | 100 |
| Djinn CLI | 24 |
| Length | 432 |
| Djinn properties | 4 |
| Church signatures with independent GHC modules | 350 per engine, 700 total |
| Rank-N scope probes with independent GHC modules | 50 per engine, 100 total |

The complete 147-test facade run passes in 234.23 seconds and includes all
24 contextual tests, independent full-signature GHC witnesses, and the existing
production budget matrices. The private suite covers checked erasure, malformed
evidence, sibling/shadow rejection, joint kinds, field/alias guards, recursive
inspection, and projection incompleteness. Church signature acceptance remains
distinct from behavioral acceptance of those implementations.

The broad run initially passed 102/106 private and 144/147 facade tests. Seven
new controls stopped at environment setup because their datatype or synonym
parameters used unsupported explicit kind annotations. Five fixture lines were
changed to use inferred parameters; explicit class-kind authority, source
signatures, budgets, and semantic assertions remain unchanged. Both complete
suites then passed. The receipt preserves the failed-run identities and verifies
that only those two fixture files changed between runs.

Strict GHC 9.12.4 builds pass with `-Werror -j1`: the broad affected library,
executables, helpers, and twelve test components, followed by a rebuild of the
two corrected test components. Runtime receipts retain source, helper,
executable, command, inventory, and output hashes with unchanged-source gates.
The focused specifications are
[NestedGivenErasureSpec](../../djinn/test-source-graph-private/NestedGivenErasureSpec.hs)
and [ContextEvidenceSpec](../../test-integration/ContextEvidenceSpec.hs).
The [current re-triage](2026-09-07-synthesis-retriage.md) orders the remaining work.
