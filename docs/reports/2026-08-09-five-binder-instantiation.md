# Five-binder instantiation across Djinn and Exference

Date: 2026-08-09

> **Successor.** The
> [six-binder widening](2026-08-10-six-binder-instantiation.md) raises the
> shared limit from five to six and moves the conservative rejection and
> inconclusive-search boundary to seven. The results and limits below describe
> this report's historical revision.

## Outcome

Djex now admits complete context-free leading `forall` chains of one through
five binders at every bounded instantiation entrance:

- Djinn query-local hypotheses and retained loaded schemes;
- Djinn provider-local scalar reconstruction and exact ordered assignments;
- Exference query-derived provider specialization; and
- Exference exact ordered assignments.

The public contract is now:

```haskell
maximumProviderInstantiationArguments = 5
```

Six binders remain outside the finite capability. A candidate-free Djinn miss
at that boundary remains `NoEvidence`, and checked provider APIs reject a
six-argument vector before entering its elements. This changes completeness,
not the soundness or meaning of negative evidence.

## One shared eligibility boundary

The earlier implementation exposed a shared four-argument assignment limit
while Djinn and Exference each repeated a private literal `4`. The private
workers now consult `maximumProviderInstantiationArguments` directly. This
keeps the checked adapter, Djinn axiom construction, Exference specialization,
and downstream frontends on one auditable boundary.

No search allowance increased. Djinn still retains at most sixteen distinct
instantiation axioms per scheme, sixty-four per family, and five hundred twelve
attempted tuples per query. Direct provider evidence remains bounded to 32
premises, Exference query-derived products remain bounded to 32 combinations,
and exact assignments still consume each retained vector once.

## Djinn scheduling

One- through three-binder schemes preserve their historical lexical Cartesian
order. The fair scheduler introduced for four binders was already arity
generic; five-binder schemes now use the same stable round-robin mixture of:

- contiguous source-order windows;
- repeated diagonal tuples;
- monotone selections from both source-order directions; and
- the complete Cartesian stream as a bounded fallback.

That schedule makes an ordinary five-argument chain useful without allowing a
large Cartesian prefix to monopolize the sixteen-axiom window. Query-local and
loaded-scheme regressions pin both a direct five-argument proof and a
non-lexical source-order application of an abstract five-argument constructor.
Exact provider evidence separately pins all five visible type applications in
source order.

## Exference specialization

At this checkpoint Exference's ordinary implicit provider use remained first.
Its bounded visible branch accepted five leading binders for both query-derived
choices and caller-supplied exact vectors. The later certificate-association
integration gives a productive exact supplied vector one leading visible lane
before that ordinary fallback so exact-spelling deduplication retains its
checked association; absent or unusable assignment input keeps the historical
order. Query-derived specialization still requires a closed, context-free
candidate and obeys its existing vacuity and combination rules. An exact vector
may specialize a non-vacuous provider body because the caller has supplied the
complete correlation; the specialized body is still kind-checked and
independently expression-checked.

Private-engine coverage demonstrates a five-binder query-derived
specialization, five distinct closed quantified arguments retained by the
exact path, and otherwise valid six-binder inputs rejected by both routes. The
public facade additionally observes the five-element visible-application
spine on a synthesized candidate.

## Preserving formula-frontier regressions

The pairwise through quintic rank-N tests historically used five-binder
transport schemes precisely because four-binder instantiation could not rescue
them. Those sentinels now use six binders, including prepared-premise and
public GHC fixtures. Their number and placement of positive `forall`
occurrences are unchanged: one adjacent binder chain is still one occurrence
site. The tests therefore continue to isolate the pair, triple, quadruple, and
quintuple formula plans rather than accidentally succeeding through the newly
admitted instantiation rule.

The twelve-site six-open/six-opaque occurrence gap is deliberately unchanged.
A sextic structural layer would add 924 views at twelve independent sites and
would amplify the existing prepared-premise search cost. Prepared views must
coexist under distinct proof identities because one term may use different
specializations of the same source function; splitting them into isolated
cohorts would lose valid cross-view proofs and invalidate exhaustive negative
evidence. That separate scaling problem is not part of this bounded change.

## Validation

The focused Djinn rank-N unit regression passes with its five-binder positives,
six-binder conservative boundaries, and six-binder formula sentinels. The
complete shared synthesis suite passes 292 tests, the private Exference engine
suite passes 28 tests, the downstream API suite passes 24 tests, and the public
facade suite passes 84 tests. The facade sweep includes GHC compilation of the
widened quartic and quintic rank-N witnesses.

## Deliberate limits

This remains bounded instantiation, not general impredicative inference or
higher-rank subsumption. Contextual hypothesis-side chains remain opaque;
candidate vocabularies remain finite; useful tuples can still fall outside the
fixed attempt and axiom windows; and six or more leading binders remain
unsupported. Every returned proof is checked against the exact specialization
that admitted it, while an incomplete bounded miss remains operationally
inconclusive.
