# Nominal Length counterexample-bank foundation

Date: 2026-08-16

## Outcome

Djex now exposes candidate-independent replay scopes and bounded input banks
for both the scalar finite-list-spine Length domain and its exact
binary-product sibling. The production checkpoint is
`7bec4edf34b31770f14430a476236bff0530b21f`; the frozen characterization is
`c69eef219462d89d4e83d5e3d6df64eab1af8b70`.

This is a foundation for later behavioral search integration, not that
integration itself. Checked problems seal and retain a scope, pure SMT-LIB
queries project it, and callers can maintain a bounded immutable store of
natural input vectors. No current offline replay entrance, live Z3 session,
candidate scheduler, persistent session state, or Leant command consults the
bank.

A stored sample has no verdict. A scope match grants only permission to
attempt fresh replay against a current checked problem. Neither operation can
mint evidence, prove an `unsat` claim, or authorize candidate pruning.

Djex is experimental and promises neither stability nor backward
compatibility. This report records the exact current contract; it does not
commit the project to preserve these names, schemas, defaults, or behaviors in
a later integrated design.

## Candidate-independent scope identity

A scope is constructed only inside the scalar or product typed-candidate
problem sealer. At that point these exact authorities coexist:

1. the checked Length session;
2. the detached contract freshly revalidated through that session; and
3. the checked normalized target owned by the contract.

The scalar `LengthCounterexampleBankScope identity` binds:

- the session's complete annotation-erased source inventory, checked scalar
  spine model, and admitted provider-law/trust table;
- the session's solver-neutral interpretation/model policy;
- the exact revalidated scalar contract fingerprint;
- the exact normalized target fingerprint; and
- the complete `djex-length-counterexample-bank-scope/v1` composition.

The product `LengthSpinePairCounterexampleBankScope identity` binds the same
classes of fact under the domain-appropriate binary-product inventory,
contract, target, and
`djex-length-spine-pair-counterexample-bank-scope/v1` composition. The two
schema-tag functions return distinct `[Word8]` values. The scope types,
scope/target fingerprint subjects, and identity parameters are nominally
separate.

The target needs a separate key because the historical contract fingerprint
deliberately omits it. Target normalization uses the checked type normalizer,
positional lexical binder slots, and first-occurrence free-variable slots; a
free slot also records whether the variable is flexible or rigid. Alpha-renamed
targets therefore share a target key, while a real target shape, free-variable
class, or first-occurrence change does not.

### Explicit exclusions

Scope composition records that all of these are absent:

- candidate term graph, interpreted result, and counterexample condition;
- the subset of provider laws actually reached by one candidate;
- SMT-LIB query, solver, process, protocol, and execution identity;
- preferences and ranking policy; and
- raw observations, evidence receipts, cached verdicts, and proof claims.

Different candidates under one exact session, contract, and target can thus
share a bank while retaining distinct candidate, complete-problem, and query
identities. Conversely, changing the canonical complete
inventory/provider-law basis, solver-neutral interpretation policy, normalized
contract, or normalized target changes the scope. The scope is the
candidate-independent replay basis, not a substitute for either the candidate
key or a runtime execution key.

`lengthCounterexampleBankMatchesScope` and its product sibling compare only
the final collision-free scope fingerprint. They do not compare or demand the
bank's limits, entries, origins, or statistics. A `True` result says only that
the candidate-independent basis is suitable for trying the stored inputs
again.

## Problem and query retention

`CheckedLengthProblem` now retains its scalar scope beside the interpreted
candidate, replay formulas, and generic `BehavioralProblem`. The product
problem retains the nominal product counterpart. The public projections are:

- `checkedLengthProblemCounterexampleBankScope`;
- `checkedLengthSpinePairProblemCounterexampleBankScope`;
- `lengthSMTLibQueryCounterexampleBankScope`; and
- `lengthSpinePairSMTLibQueryCounterexampleBankScope`.

A query's projector returns the scope inside its exact retained problem. It
does not rebuild a scope from query bytes or caller-supplied facts. Different
SMT-LIB resource limits can therefore seal the same canonical query and
project the same scope; different candidates may produce distinct queries
that still project the same scope.

Scope construction runs after the existing complete-problem fingerprint and
under the checked session's fingerprint-byte cap. A failure uses the existing
nominal problem error with the new
`LengthCounterexampleBankScopeFingerprint` or
`LengthSpinePairCounterexampleBankScopeFingerprint` part. Product failure
never leaks a scalar error family.

The bank scope is not folded back into the complete-problem or SMT-query key.
It composes facts those checked authorities already own for a different,
candidate-independent association purpose. Canonical SMT-LIB check bytes,
input-value request bytes, query fingerprints, response parsing, protocol
plans, live runs, and evidence association are unchanged.

## Nominal bounded bank families

`Language.Haskell.Synthesis.Semantic.Length.CounterexampleBank` exports two
closed public families:

| Scalar | Binary product |
| --- | --- |
| `LengthCounterexampleBankScopeFingerprintSubject` | `LengthSpinePairCounterexampleBankScopeFingerprintSubject` |
| `LengthCounterexampleBankTargetFingerprintSubject` | `LengthSpinePairCounterexampleBankTargetFingerprintSubject` |
| `LengthCounterexampleBankScope identity` | `LengthSpinePairCounterexampleBankScope identity` |
| `LengthCounterexampleBankLimits` | `LengthSpinePairCounterexampleBankLimits` |
| `LengthCounterexampleBankLimitField` | `LengthSpinePairCounterexampleBankLimitField` |
| `LengthCounterexampleBankLimitError` | `LengthSpinePairCounterexampleBankLimitError` |
| `LengthCounterexampleBankOrigin` | `LengthSpinePairCounterexampleBankOrigin` |
| `LengthCounterexampleBankSample` | `LengthSpinePairCounterexampleBankSample` |
| `LengthCounterexampleBankStats` | `LengthSpinePairCounterexampleBankStats` |
| `LengthCounterexampleBankError` | `LengthSpinePairCounterexampleBankError` |
| `LengthCounterexampleBank identity` | `LengthSpinePairCounterexampleBank identity` |

Scope, limits, origin, sample, statistics, and bank constructors remain
private. Limit-field, limit-construction-error, and operational-error
constructors are public closed vocabularies. Domain wrappers share a
package-private strict bounded kernel, but callers cannot coerce scopes,
limits, origins, samples, statistics, or banks between scalar and product
domains. The bank identity parameter is nominal as well. In particular, a
sample cannot be coerced into either domain's validated counterexample receipt.

The new module is exposed directly and re-exported from
`Language.Haskell.Djex`. The public problem and SMT-LIB modules expose only
their corresponding scope projectors; private scope construction does not
cross either facade.

## Limits and defaults

Both domains currently use these defaults:

| Limit | Default |
| --- | ---: |
| Retained entries | 4 |
| Inputs in one sample | 8 |
| Bits in one natural input | 256 |
| Aggregate retained modeled bytes | 4,096 |
| Cumulative replay attempts | 256 |

`mkLengthCounterexampleBankLimits` and
`mkLengthSpinePairCounterexampleBankLimits` take those five caps in the same
order. Entries, width, and natural bits use `Int`; modeled bytes and attempts
use `Natural`.

Limit construction has fixed precedence:

1. reject a negative entry limit;
2. reject a negative width limit;
3. reject a negative natural-bit limit;
4. reject a width of `maxBound`; then
5. reject a natural-bit cap of `maxBound`.

The last two checks preserve productive maximum-plus-one observation. An entry
cap may be `maxBound` because insertion checks zero capacity directly and does
not need to compute its first excess. Natural byte and attempt caps cannot be
negative.

## Productive sample admission

Insertion first checks whether the retained-entry cap is zero. If so, it
returns the domain's nominal entry-limit error with limit zero and first
observed count one, without demanding the origin or any part of a poisoned or
cyclic vector.

For positive capacity the admission order is:

1. observe at most one list cell beyond the width limit and reject width
   overflow before inspecting any element value;
2. validate each retained natural's bit width from left to right, observing at
   most one bit beyond the bit limit; and
3. reject an individual sample whose modeled encoded byte count exceeds the
   cap.

The modeled encoding charges one byte for origin, a variable-width natural for
the sample arity, and for each input a variable-width magnitude-byte count plus
at least one magnitude byte. Public byte-overflow errors expose only the cap
and a saturated first-excess observation. This accounting is a deterministic
resource policy; it is not a persistence or wire format.

A successful insertion deeply forces the retained sample vectors and all
statistics. It cannot hide a cyclic tail or a bottom inside a newly retained
sample or the updated statistics. Immutable failure leaves the original bank
available to the caller.

The byte cap has two roles. It bounds each candidate sample during admission,
then bounds the aggregate modeled bytes of the retained newest prefix. The
newly admitted sample always fits by itself; older samples are retained only
while both the entry and aggregate-byte caps continue to hold.

## Deduplication, recency, eviction, and statistics

Banks expose samples newest first. Deduplication compares the complete input
vector only. Origin is deliberately not part of the bank's duplicate key:

- a new vector is prepended;
- reinserting an existing vector removes its older occurrence, replaces its
  origin with the newest caller-supplied label, and promotes it to the front;
  and
- retained order among every other vector is unchanged.

After promotion or insertion, retention takes one deterministic newest prefix.
It stops at the first entry-count or aggregate-byte excess and evicts that
entry and the whole remaining oldest tail. It does not scan past an older
large sample to keep a still older small one.

The three opaque origin constants describe only the caller's coarse intended
source:

- live-model replay;
- solver-independent replay; or
- simplification replay.

The bank does not verify those claims. An origin is not an observation,
transcript, run identity, evidence association, or attestation.

Statistics retain six exact natural counts:

1. currently retained entries;
2. currently retained modeled bytes;
3. all successful records, including duplicate promotions;
4. duplicate promotions;
5. cumulatively evicted samples; and
6. cumulatively recorded replay attempts.

Entry and byte counts are recomputed from the strict retained prefix after
each successful insertion. Record and promotion counts advance only on a
successful insert. Eviction count advances by the number of samples removed
from the promoted candidate list. Replay attempts are independent of
insertion and advance only through
`recordLengthCounterexampleBankReplayAttempt` or its product sibling.

Attempt recording checks the cap before incrementing. At exhaustion it returns
the nominal cap and cap-plus-one observation, without demanding samples or
changing the bank. Scope matching neither records nor reserves an attempt.
The immutable bank cannot observe a replay performed elsewhere, so this cap
bounds explicit bookkeeping operations rather than acting as a runtime
executor capability.

## Authority boundary and absent integration

A bank sample stores only a strict source-ordered `[Natural]`, its modeled byte
count, and one coarse origin. Insertion checks storage width, natural size, and
modeled bytes; it does not check the current problem's compact input arity,
precondition, candidate result, postcondition, or bad-state formula.

The only sound use is therefore:

1. obtain the current problem or query scope;
2. require a bank scope match;
3. record a bounded replay attempt;
4. project one stored input vector; and
5. pass it through the existing exact bounded replay entrance.

That fresh replay can reject input arity, fail under evaluation limits, return
`Nothing`, or mint a new ordinary counterexample receipt. A prior hit against
another candidate cannot be carried across as a verdict. Scope matching is
candidate-independent precisely so the vector can be retried, not so the old
candidate's result can be reused.

The current implementation deliberately does not provide:

- automatic insertion from decoded models, live observations, direct replay,
  origin probes, simplification, or applicable-domain validation;
- automatic bank replay before a Z3 query;
- candidate iteration, scheduling, survivor refill, or all-rejected control;
- persistence, serialization, concurrency ownership, or session reset policy;
- receipt/verdict/proof storage; or
- any Djex frontend or Leant runtime behavior.

Those are later integration decisions. In particular, a future command-local
CEGIS loop must decide when an attempt is charged, how replay errors affect a
candidate, which successful vectors are recorded, and who owns bank lifetime.
None of those policies is smuggled into this storage checkpoint.

## Characterization

The focused nominal bank group passed 15/15. It covers:

- scalar/product limit construction, defaults, accessors, and precedence;
- candidate independence plus inventory, policy, contract, and target
  sensitivity;
- target alpha-normalization and flexible/rigid distinction;
- exact problem/query retention, query-limit independence, and exclusion of
  candidate-used laws and solver launch policy;
- productive width and bit observation, poisoned/cyclic tails, zero-capacity
  demand, strict success, and immutable failure;
- modeled byte accounting and aggregate-byte tail eviction;
- input-only duplicate promotion with origin replacement;
- entry-pressure MRU eviction and all six statistics; and
- independent replay-attempt exhaustion.

Facade characterization pins the public signatures, re-export, schema tags,
closed errors, default values, accessors, origins, and deep evaluation.
Abstraction-boundary compilation checks keep scope, limits, origin, sample,
statistics, and bank constructors private, reject identity and scalar/product
coercions across the nominal families, and reject coercing either sample
family into behavioral evidence.

Frozen validation completed with:

- strict warning-as-error builds;
- 15/15 focused bank cases;
- 385/385 Length cases;
- 38/38 Djex API cases;
- all 16 repository suites green, totaling 1,823 tests;
- clean `cabal check`; and
- clean `git diff --check`.

The frozen test sources have these sizes and literal `testCase` token counts:

| File | Lines | `testCase` tokens | SHA-256 |
| --- | ---: | ---: | --- |
| `synthesis/test-length/Spec.hs` | 16,231 | 317 | `1a7e1b54b493e2db039042dfbdaae68882863f0f10b87a5b7b0761636f0499b4` |
| `test-api/FacadeSpec.hs` | 2,729 | 31 | `090d9677bdf596ca051ab7b1b280426435eaf0b18e5c61ce33c42f2ef474d582` |
| `test-api/AbstractionBoundary.hs` | 2,058 | 0 | `63cdecb2431d0b572a3e7959accbab9451c85729c743b03eb4e789f86459fc03` |
| `test-api/Spec.hs` | 672 | 9 | `35a4cadc95255d7c16f4c4aad25dc80db961f42a77fd904e908fad4511c95b5c` |

The ordered newline-terminated manifest hash is
`2f0c52ee5e2368b56a4c9fc925b6dc8ebeb48cb9b5dc260c22137aa76cfd5252`:

```bash
sha256sum \
  synthesis/test-length/Spec.hs \
  test-api/FacadeSpec.hs \
  test-api/AbstractionBoundary.hs \
  test-api/Spec.hs | sha256sum
```

For an independent content-order check, raw concatenation of those same four
files produces SHA-256
`85c4393bf7d877d62de585f4be597fc36a911f90ac85a38fd020c1b42aa0c0a2`.

## Documentation boundary

The current normative description lives in the
[library guide](../library-api.md#prepare-a-bounded-replay-input-bank) and the
[semantic foundations](../semantic-foundations.md#candidate-independent-counterexample-bank-scopes-and-bounded-stores).
The [synthesis foundation map](../../synthesis/README.md) records module
ownership.

Earlier dated Length, SMT-LIB, Z3, and Leant reports remain historical and
non-normative. They do not imply that bank matching reuses a verdict, that a
query currently consumes a bank, or that a runtime bank already exists. This
report likewise describes an experimental checkpoint, not a compatibility
promise for the next one.
