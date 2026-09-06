# Candidate quality and bounded selection

Djex and Leant can prefer smaller, simpler implementations before their output
and verification cutoffs. The policy operates at two points: it orders finite
search choices before the existing work limit, then ranks the checked
candidates admitted by that search. It does not increase the proof, choice,
step, queue, or time allowance.

For example, a known-constructor elimination such as

```haskell
case Right value of
  Left a  -> use a
  Right b -> b
```

can have the same useful result as `value`. Removing that administrative
structure can improve the first result, collapse redundant alternatives, and
save target-language verification work. A preference for another inhabitant
does not assert that the two programs implement the same intended algorithm.

## Choosing a policy

The shared profiles are `legacy`, `balanced`, `compact`, and `diverse`.
`balanced` is the default. In Djex:

```text
:set ranking balanced
:set ranking compact
:set ranking diverse
:set provider-cost Slow.provide=50
:set provider-cost Fast.provide=0
:set quality-window 60
:show settings
:unset provider-cost
:unset ranking
```

The one-shot command accepts the corresponding options for either engine:

```text
djex djinn --ranking compact --candidate-limit 60 "a -> a"
djex exference --ranking diverse --select all --quality-window 60 --max-steps 4096 "a -> a"
```

`--provider-cost NAME=COST` accepts a nonnegative integer and can be repeated
for distinct exact names. It influences preference only; it neither imports a
provider nor makes an unavailable provider admissible. Qualified names are
distinct, and their printed length has no effect on the score. The
`provider-cost` REPL setting updates one entry; unsetting it clears overrides.
The REPL stores canonical source names. For Djinn, it projects those names to
the current checked module scope at query time and uses that same cost map for
search and presentation. Reloading or changing imports cannot leave a cached
cost attached to another module's identically spelled provider. For loaded
declarations, use their canonical qualified names, such as `Fast.provide`.

These controls belong to the modern `djex` frontend. The historical standalone
`exference` executable keeps its legacy search defaults, including its existing
`--short` preference. Use `djex exference --ranking ...` for structural policies.

In Leant, use `:set synth-ranking legacy|balanced|compact|diverse`. The selected
profile applies to ordinary, provider, library, and classical lanes. Both
engines use the shared default structural prices: each named value occurrence
costs one, and constructors have no provider surcharge. Provider discovery
and lane order keep their relevance priorities. Exference also retains its
existing source ratings of 0, 20, 40, and so on under exact private identities;
these remain search inputs and are not charged again as structural prices.
Changing the ranking profile preserves the configured resource bounds and
eligibility for the existing parallel search schedule.

## Structural cost and diversity

The library exposes `Language.Haskell.Synthesis.CandidateQuality`, also
re-exported by `Language.Haskell.Djex`. `CandidateQualityWeights` supports
custom nonnegative, arbitrary-precision weights. The built-in profiles use:

| Profile | Term size | Elimination structure | Provider cost | Repeated-family cost |
| --- | ---: | ---: | ---: | ---: |
| `compact` | 1 | 1 | 1 | 0 |
| `balanced` | 1 | 3 | 2 | 4 |
| `diverse` | 1 | 3 | 2 | 12 |

The base cost is the weighted sum of structural size, elimination structure,
and provider costs. Term size counts syntax and pattern sites; a grouped
lambda and the corresponding successive lambdas have the same cost. Required
visible type applications remain part of the term and are not charged by the
length of their type spelling. A let-bound computation is counted once, so
sharing is not priced as repeated evaluation. Explicit cases, alternatives,
and destructuring binders contribute elimination cost. This is a structural
preference, not a proof that every elimination is unnecessary.

Without an override, a named value has provider cost one and a constructor
has no provider surcharge. The backend's existing source ratings and search
heuristics remain separate search inputs. Quality does not replace typing,
class resolution, or provider admission.

After each selection, diversity adds the configured repeated-family cost to
other candidates in that family. Families describe the operations and
dependencies used, ignoring repetition: `f x` and `f (f x)` are related, while
the two projections of `a -> a -> a`, different constructors/providers, and
different explicit type choices remain distinct. Alpha-renaming does not
create a new family. This coarse relation changes ordering only. It is never
used as a test for semantic equality or as permission to discard a candidate.
Equal costs retain encounter order, making selection deterministic.

`legacy` preserves the historical search and ranking path. In Leant it also
retains the historical Djinn size sort and Exference selection path. It is
useful for comparisons and for workflows that rely on an established result
order.

## Search work, candidate observations, and output quotas

These limits have different meanings:

- Djinn's `candidate-limit` remains a global **raw proof allowance** across
  formula plans. Rejected proofs and normalized duplicates consume their
  original allowance. The choice-point budget remains independent.
- Exference's steps, queue, and depth bounds continue to govern raw search.
  Simplification, rejection, and deduplication do not refund search steps.
- For structural `all` selection in the Djex frontend, `quality-window`
  bounds the number of Exference candidates inspected before selection. The
  default is 60. Rejected candidates and duplicates consume observation slots.
  `first` preserves early stopping: its quality comes from backend frontier
  ordering, checked-batch ranking, and normalization, without waiting to fill
  this pool. The existing bounded record-selector lookahead still applies
  when needed. `best` retains search-wide selection within the engine bounds.
- Leant's `synth-window` limits the collection passed to later verification.
  Under structural Exference policies, it additionally bounds raw backend
  candidate observations before rendering and deduplication. In `legacy`,
  Exference retains its distinct-rendered-group window: duplicate derivations
  may be inspected before that many different groups have been collected.
  `synth-verify` and `synth-shown` remain separate limits on target-language
  checks and accepted output. The defaults remain 60, 12, and 5 respectively.

Structural Exference search puts completed branches and unfinished work in
the same priority frontier. A completed expensive provider can therefore wait
behind a cheaper application whose argument still needs synthesis. Deferred
completions occupy the existing queue allowance; admitting them adds no search
steps. At the final allowed step, the engine emits retained known completions
even when better-priority work remains unfinished. It also preserves a known
completion if a zero-capacity queue prevents further exploration. Queue
pruning remains explicit. Graph identities retain the original discovery
step and branch, while operational candidate statistics report the step at
which the candidate is admitted. `legacy` keeps immediate completion delivery.

Structural selection does not refill observation slots lost to rendering
failures, rejection, or duplicates. All policies retain their independent
engine work limits; the legacy rendered-group window is not a raw-observation
bound. Structural Exference `all` selection buffers its admitted pool
before printing. If the command times out while collecting that pool, an
already discovered prefix may not be printed. Lower `quality-window` for
earlier output, or use `legacy` to retain the historical streaming
all-selection behavior. At an
observation cap it does not inspect the next candidate merely to decide
whether the stream ended. The reported status is conservatively truncated;
it cannot become a proof of uninhabitation. A better candidate outside the
admitted search remains outside the result. These policies improve finite
search results rather than compute globally minimal inhabitants.

The interactive `first` policy does not collect a quality window merely to
compare an immediately available result with later search. Bounded-pool API
comparisons and Leant's verification-window selection remain separate contracts.
Full-trace timings are diagnostic measurements; the policy does not imply a
speedup for every query or selection mode.

Combined-engine Leant searches rank each engine's candidates before the
existing fair merge. Under structural profiles, proper provider-inventory
prefixes use Djinn alone; the full inventory runs both engines, so a singleton
inventory still runs both. Legacy retains its combined singleton/full stages
and Djinn-only intermediate prefixes. This avoids spending Exference's search
on an incomplete provider prefix before reaching a required composition.
Discovery order, standalone engine schedules, shared deadlines, raw budgets,
verification quotas, and the final merge are unchanged. Ranking does not move
candidates across sealed Length decisions.

Structural pool and global-best scoring use record-selector-normalized
expressions when a checked selector table is available, matching the form
printed. This includes the cost of selectors introduced by normalization.
The `first` selector lookahead keeps its existing bound. Structural profiles
use the configured structural cost on the selector-normalized clause; only
`legacy` retains the size-only metric.
Scoring projects an expression for inspection; it retains the original checked
candidate handle and uses that handle for admission and evidence.

## Checked normalization and retained evidence

Known-constructor reductions require exact constructor identity and arity,
decidable branch selection, valid lexical scopes, and retained authority for
the relevant field behavior. Unknown scrutinees and unsupported patterns are
left intact. Payloads become nonrecursive lets so repeated uses keep sharing;
the existing capture-safe cleanup may remove an unused binding or inline a
single use. Local type annotations and visible applications in retained
payloads are preserved. Required higher-rank eta expansion is not removed.

Exference first exposes safe single-use let aliases and removes unused lets
with the capture-safe simplifier that does not contract eta expansions. It
then applies the shared one-pass constructor reducer and simplifies the
resulting field lets. If this reveals another reducible match, it repeats only
while the number of case nodes strictly decreases. Neither step duplicates
cases, so the original case count bounds this closure. Repeated payload uses
remain shared, and visible type applications on a constructor head still
block its reduction. This iteration belongs to the Exference adapter; the
shared `Generated` reducer retains its one-pass contract.

Exference independently checks a reduced term before constructing its typed
candidate and graph. If reduction cannot retain the necessary typing context,
the checked original remains available. Selection keeps the whole candidate
and its exact evidence association. Leant scores the authoritative graph when
available, retains rendering provenance, and kernel-checks every displayed
term as before.

The additional known-constructor reduction is enabled by structural profiles;
`legacy` retains its existing normalization sequence. Boxed tuples and nullary
constructors need no non-strict-field certificate. For named constructors
with fields, the Haskell source loader retains lazy-field information before
type annotations are erased. It carries known `ParseMode` extension switches
into extraction, then applies source `LANGUAGE` and `OPTIONS_GHC` overrides in
order. `Strict` implies `StrictData`; a later `NoStrictData` restores lazy
field defaults. Explicit lazy fields can still authorize reduction under a
strict default, while strict fields cannot. Compiler flags absent from both
the supplied parser mode and the source are not inferred from a type signature.
Generic neutral declarations carry no field-evaluation guarantee.
The field rules follow [GHC's strictness and modularity semantics](https://downloads.haskell.org/ghc/9.12.4/docs/users_guide/exts/strict.html).
Trusted host integrations can supply exact constructor/arity authority through
`ExferenceSessionPolicy.exferenceNonStrictConstructors`; sealing validates
those names and arities, while the host remains responsible for the evaluation
claim. Leant derives that authority from its actual total family translation
and active constructor inventory.

Leant also preserves the boundary between provider evidence and target
reconstruction. A marked fallback scheme can erase a leading class context
whose quantified assignment is not representable as a resolver fact, while
retaining complete source-derived Lean vectors. Rendering may restore a whole
closed vector at a bare provider occurrence only when every leading variable
is absent from the residual value type, including ordinary argument domains
and later contexts. Existing visible choices and nonvacuous dependencies are
not overwritten. Whole-vector alternatives retain their Lean binder metadata
inside the existing bounded rendering cohorts, without new raw candidate
slots or refunded work.

Those inserted choices are not certified by the original bare expression's
graph. Reconstructed groups use `RouteUnobserved` and carry no typed semantic
sidecar or exact typed origin. Lean must check each resulting term before
display; Length cannot inherit a certificate from the uninstantiated graph.

Length assessment still consumes the exact verified batch. Its behavioral
ranking, authorized filtering, and failure-preservation rules are unchanged.
A structural score is never a type certificate or behavioral proof.

## Validation boundaries

The [focused quality probes](../test-church/quality.md) compare policies at
equal configured budgets, independently replay emitted terms, and distinguish
projection alternatives by their results on concrete inputs. The Haskell
acceptance is complete for the recorded build and executables:

| Receipt | Confirmed result |
| --- | --- |
| `build-all-05.log` | All 19 test components passed, including 437 shared tests, 512 Exference tests, 49 private engine tests, 102 Djinn tests, 96 integration tests, 429 Length tests, and CLI suites of 93, 25, and 24 tests. The same aggregate reran all 700 Haskell Church queries and all 100 rank-N scope queries. |
| `compiled-closure` | 56 policy/backend queries passed; all 104 retained terms passed independent GHC replay, and the diverse projections passed evaluation on distinct inputs. |
| `quality-cli-closure` | 14 exact outputs passed GHC, ten invalid options were rejected, and settings/reset, qualified provider costs, reload, and changed module scope passed. |

The standalone probe executable has SHA-256
`3bee140a0d864c7f4016722aaadc954363ddb2ab956311614d574e5193235604`.
The separately recorded CLI executable has SHA-256
`485bc35ea11cc4e6d5b9ba04009b4bb69386f3a932c38c53b6cd639ce3ce1982`.
The [focused guide](../test-church/quality.md#recorded-haskell-acceptance)
identifies their reports and measurement boundaries.

The strict Haskell preference improvement is the Exference provider fixture:
legacy selects `expensive`, while all three structural profiles select
`cheap ()`. Provider cost falls from 40 to zero; evaluating both terms with
the balanced metric gives 81 versus 3, despite structural size increasing
from one to three. Djinn already chose `cheap ()` under legacy, and Haskell
`nil` had zero eliminations under every profile. Those checks demonstrate
preservation, not additional strict improvements.

The final compiled Exference `nil` alternatives queries, configured with a
10,000-step budget, completed in approximately 0.10–0.17 seconds wall time. The cause of
the difference from earlier diagnostic runs has not been established, so
these timings are not an attributed source-level speedup claim or a
first-result latency measurement.

Recorded E0 Lean quality acceptance belongs to Leant `5629936`, which includes
the corrected test assertion and reviewed compact goldens. Its production
code and E0 executable remain those introduced at `a970d1f`, with vendored
Djex `ae986bf5` and unchanged synthesis code `2954b6d2`. The E0 executable
remained unchanged and has SHA-256
`e0b9c87cae0bc34d59c8d5a34a58fdd5005676913969a80d5503d7281081d025`.
Its receipts under Leant's `test-church/quality-results/` establish:

| Receipt | Confirmed result |
| --- | --- |
| `build-leant-06.log` | All 569 Leant unit tests passed in 389.71 seconds; the process exited zero and the E0 executable remained unchanged. |
| `matrix-final/results.json` | All 84 policy/engine/example queries and 136 exact displayed terms passed live synthesis and independent kernel replay: 112 empty axiom inventories and 24 containing only declared provider premises. All three paired nil improvements passed. |
| `fixtures-repair/results.json` | All 90 compact rank-N/provider queries passed live and kernel checks: 78 empty axiom inventories and 12 exact declared-premise inventories. |
| `church-djinn-final/results.json` and `church-exference-final/results.json` | Fresh 350/350 candidates per engine on unchanged E0; all 700 exact displayed terms passed independent kernel replay with empty axiom inventories. |
| `compact-comparison/results.json` | All four reviewed goldens match the preserved compact captures by offline comparison; the original live receipt retains its three golden mismatches. |
| `ordinary-final/results.json`, `ordinary-review-audit.json`, and `synth-prove-review/review.json` | All 26 ordinary fixtures completed on E0. Review checked 99 exact changed-term kernel replays and their query-specific axiom allowances; separate proof-mode validation covered six terms, two exact tactic applications, and the expected evaluation type error. |
| `ordinary-golden-application/application.json` and `composite-comparison/results.json` | Twenty reviewed ordinary goldens were updated; offline comparison then matched all 30 ordinary/compact goldens. The original ordinary live exit 1 remains preserved. This comparison reran neither synthesis nor kernel checking. |

The matrix used a window of 12, display cap of four, 10,000 Exference steps
or Djinn choice points, and a 30-second synthesis timeout for each policy.
Exference's freshly observed legacy nil retains
`match Sum.inr x with | .inl a => f a x | .inr b => b`; balanced, compact, and
diverse return `fun _ _ _ x => x`. Three independent Lean proofs also show
that diverse projection outputs include both results on inputs 11 and 29,
one proof per engine selection. These are quality and behavioral distinctions
within the tested allowance, not global minimality claims.

The ordinary review also found a concrete first-result regression. In
`synth-manual.txt` Q20, Djinn's implementation of
`Decidable p → Decidable q → Decidable (p ∧ q)` grew from two syntactic
matches to three. The new term passed exact kernel replay. No final-score
inversion was identified, and the transcript has no truncation diagnostic:
whether the smaller old term entered the new finite candidate pool is unknown.
The evidence therefore establishes neither a scoring inversion nor budget
exhaustion. It does refute any claim that every first result becomes smaller.

The separate fresh Church replay covers, per engine, 315 total cases,
16 integer-provider cases, and 19 cases made total by explicit input defaults.
It uses one-candidate observation/verification/display windows and a
30-second synthesis timeout. Exference has 4,096 steps; Djinn retains its
default unbounded choice-point budget, subject to the shared deadline and
intrinsic planning caps. These settings do not bound startup, serialization,
or separate kernel replay. Exact candidate texts match the earlier corpus
run, establishing preservation on E0 rather than another quality improvement.
Those E0 receipts precede a later Leant-only accepted-exact-spelling repair.
Within the already bounded verification groups, it remembers only spellings
that actually passed verification, tries fresh alternatives in the same group,
and preserves the accepted representative's original metadata and authority.
It neither refills the raw search prefix nor borrows evidence from a later
duplicate. Leant production commit
`043a6a3d1562578f9aee8ad73ada4e02ddd4a52d` passed **all 578 unit tests serially
in 533.23 seconds** and its executable build passed. The receipt
`quality-results/dedup-build-acceptance.json` and its `dedup-02` test/build logs
record successful process exits and executable SHA-256
`42c0c9c0a46a35302a04691a93fc68099c5e80bd91305e9a927ba9de9cec5cae`.
The fresh full **30-fixture/265-command** live run completed in 1,124.61
seconds with a 30-second synthesis timeout, preserving every prior success,
first result, and control/proof output. Its sole golden drift removed one
duplicate accepted spelling; both retained terms passed exact kernel replay
using only `Gap.Token` and `Gap.polyGlobal`. After that one reviewed update,
offline comparison matched all 30 goldens, preserving the original live
exit 1. No retry at the earlier 600-second allowance was needed.

Two disjoint fresh matrix runs covered Djinn/Exference (56 queries, 87 terms)
and Both (28 queries, 49 terms). `matrix-dedup-complete.json` records **84
queries and 136 exact kernel-accepted terms**, with 112 empty axiom inventories,
24 declared-premise inventories, three paired Exference nil improvements,
and three diversity proofs. All type/term lists match E0 under the same
12/4 observation/display caps, 10,000-step/choice-point allowances, and
30-second synthesis timeout. The [focused guide](../test-church/quality.md#recorded-lean-acceptance)
links the live, independent-review, kernel, and offline-comparison receipts.
The E0 700-term Church corpus was not rerun for this Leant-only repair.
Canonical Djex synthesis code `2954b6d2` is unchanged, and its recorded
19-component acceptance above is not a new rerun.

The earlier Leant `fb84b96`
executable `dab110ad2a7903ac4ef4883898d48532c00cc8c3b1b8d8748aac7744eedffb61`
passed 565 tests, an 84-query/139-term matrix, and all 700 Church terms; those
receipts remain historical and do not establish acceptance of E0. The
[focused guide](../test-church/quality.md#recorded-lean-acceptance) records
current hashes and the [Church guide](../test-church/README.md) preserves the
separate corpus budgets, universe/default policy, and earlier
Leant `4757569`/Djex `e2eb71e` rank-N and ordinary compatibility report.

## Related documentation

- [Rank-N synthesis rules and bounds](rank-n.md)
- [Shared query and embedding API](library-api.md)
- [REPL settings and commands](repl.md)
- [Church signature acceptance](../test-church/README.md)
- [Focused quality comparison and compiler replay](../test-church/quality.md)
- [Leant synthesis internals](https://github.com/VladimirReshetnikov/Leant/blob/main/docs/synth-internals.md)
