# Focused candidate-quality acceptance

These probes compare `legacy`, `balanced`, `compact`, and `diverse` with the
same search settings. They complement the shared structural-scoring,
raw-observation-limit, scope, sharing, visible-type-argument, and typed-handle
unit tests. They do not replace the Church corpus, the rank-N scope corpus,
or the existing Length behavioral contracts.
The [policy guide](../docs/candidate-quality.md) defines the score and the
distinction between raw search work, observed candidates, and output quotas.
Haskell compiler, CLI, and live Lean quality acceptance are recorded below.
The E0 quality matrix, compact fixtures, full 700-query Lean Church replay,
and reviewed ordinary compatibility have separate receipts. They precede the
Leant-only accepted-spelling deduplication repair, which passed all 578 unit
tests, its executable build, the full 30-fixture compatibility review, and a
fresh 84-query/136-term quality matrix. The full 700-term Church corpus was
not rerun for this repair; its E0 receipt retains that executable identity.

Run against an already built checkout, with the repository's build owner
having released the build slot:

```powershell
New-Item -ItemType Directory -Force test-church/results/quality-build
cabal exec -- ghc -O1 -package=djex -threaded -rtsopts -outputdir test-church/results/quality-build test-church/quality_probe.hs -o test-church/results/quality-probe.exe
& test-church/results/quality-probe.exe test-church/results/quality
python test-church/quality_cli.py --djex PATH_TO_BUILT_DJEX_EXE
```

Compile the standalone probe once, then run it after the build completes.
This avoids keeping the interactive GHC process resident during the search
matrix, which can distort wall-clock results when memory is scarce. The
command optimizes the probe module; the separately built library retains its
own recorded build configuration.

`quality_probe.hs` makes 56 public-API queries: two engines, four policies,
and seven signature-only examples. Each gets a 12-candidate observation
window, at most four retained outputs, 10,000 Djinn choice points or Exference
steps, and a separate 15-second harness timeout. Provider declarations and
type signatures are the only synthesis input. The generated replay module
supplies total provider bodies only after synthesis.
Every Exference profile uses the same shared raw-pool selector; `legacy`
keeps encounter order within that pool. If search produces fewer than 12
backend candidates, the probe may exhaust all 10,000 steps to establish the
pool even though only four outputs are retained. This is an alternatives
comparison, not a first-result latency test.

This API comparison deliberately inspects a bounded frontier under each
profile. It does not change the interactive `first` contract: the Djex
frontend stops early, retaining its established record-selector lookahead,
and applies `quality-window` pooling only to structural Exference `all`.
The CLI probe uses the modern `djex exference` subcommand; the historical
standalone `exference` executable retains its legacy defaults and `--short`.

After the entire synthesis matrix passes, every retained alternative is
independently checked by GHC. The probe records
its structural size, elimination count, exact provider cost, and complete
expression in `candidates.tsv`. The first nonlegacy `nil` candidate must have
no elimination; the provider fixture must prefer a zero-cost function applied
to unit over a 40-cost direct value. A separate runtime check applies the
`diverse` projection candidates to distinct integers and requires both
projections. This behavioral distinction is independent of local spelling or
the implementation's structural-family key. The impredicative examples check
closed and ambient type choices through the original signatures.

`quality_cli.py` exercises eight successful policy/backend combinations,
compiler-replays their exact expression output, rejects ten invalid CLI
options, and verifies REPL setting/reset behavior. Invalid settings must leave
the last valid policy, provider map, and window intact. Six additional loaded
provider queries check that canonical qualified costs affect both engines,
reverse preference after `:reload`, and follow a changed module scope even
when two modules use the same short provider names. Their exact emitted terms
are compiler-replayed in the corresponding source scope. It captures stdout,
stderr, arguments, and the immutable executable hash; it never updates goldens.
Each CLI or compiler process has a separate 60-second wall-clock guard
(`--process-timeout`), without changing the configured engine search bounds.
`processes.json` is flushed after each CLI invocation and records its elapsed
wall time. These end-to-end diagnostics include startup and are not a general
first-result latency or speedup claim.

The measurements compare preferences within the candidates actually found.
`query-metrics.tsv` and stdout record each query's monotonic wall time,
process CPU time, status, and retained count. Rows are flushed even when a
query times out or fails. The timed interval includes request construction,
search, retained-term rendering and measurement, and synthesis assertions;
it excludes the later external compiler and projection evaluation. Process
CPU time includes garbage collection, while wall time also reflects scheduling
and memory pressure. A single run is diagnostic evidence, not a controlled
cross-profile speed comparison.

External GHC replay and projection evaluation run only after the entire
matrix succeeds. `validation-status.txt` records those separate stages for
the current invocation. Partial synthesis rows do not establish compiler
acceptance, and an interrupted run must not reuse older replay files as its
own successful receipt.

Different queue preferences can explore different terms under the same work
budget. Neither a lower score nor inhabiting a Church operation's type proves
that its behavior matches that operation's conventional meaning. The probes
also do not assert that a finite observed frontier contains a global minimum.

## Recorded Haskell acceptance

The aggregate in `results/quality/build-all-05.log` passed all **19 test
components**, including the following checks:

| Test group | Passed |
| --- | ---: |
| Shared synthesis | 437 |
| Exference | 512 |
| Private Exference engine | 49 |
| Djinn | 102 |
| Djex integration | 96 |
| Length | 429 |
| Modern Djex CLI | 93 |
| Standalone Exference CLI | 25 |
| Standalone Djinn CLI | 24 |
| Haskell Church queries | 700 |
| Rank-N scope queries | 100 |

The standalone probe receipt at `results/quality/compiled-closure/receipt.json`
records **56 successful queries, 104 independently GHC-checked terms, and
successful projection evaluation**. Its `validation-status.txt`,
`candidates.tsv`, `query-metrics.tsv`, `ghc.txt`, and
`projection-diversity.txt` preserve the separate stages. The probe was compiled
with `-O1`; its executable SHA-256 is
`3bee140a0d864c7f4016722aaadc954363ddb2ab956311614d574e5193235604`.
The observation window of 12, output cap of four, 10,000-step/choice budget,
and 15-second timeout were unchanged.

`results/quality-cli-closure/results.json` records **14 exact compiler-replayed
outputs, ten invalid-option rejections, and successful settings/reset,
qualified-provider, reload, and module-scope checks**. That distinct executable
remained unchanged throughout the run and has SHA-256
`485bc35ea11cc4e6d5b9ba04009b4bb69386f3a932c38c53b6cd639ce3ce1982`.

The measured quality distinction is explicit:

| Fixture | Legacy first term | Structural first term | Interpretation |
| --- | --- | --- | --- |
| Exference provider | `expensive` | `cheap ()` under balanced, compact, and diverse | Provider cost 40 → 0; the balanced metric applied to both terms is 81 → 3, while size is 1 → 3. |
| Djinn provider | `cheap ()` | `cheap ()` | Existing preference preserved. |
| Haskell `nil`, both engines | Zero eliminations | Zero eliminations | Existing simple output preserved. |

The final Exference `nil` alternatives queries used a 10,000-step budget and
completed in approximately 0.10–0.17 seconds wall time. These are recorded
diagnostics, not first-result latency. The cause of their difference from
earlier runs has not been established; no particular source change is credited
with that timing difference, and no general speedup is claimed.

## Recorded Lean acceptance

Recorded E0 Lean quality acceptance belongs to Leant `5629936`, which includes
the corrected test assertion and reviewed compact goldens. Its production
code and E0 executable remain those introduced at `a970d1f`, with vendored
Djex `ae986bf5` and unchanged synthesis code `2954b6d2`. Its E0 executable
remained unchanged and has SHA-256
`e0b9c87cae0bc34d59c8d5a34a58fdd5005676913969a80d5503d7281081d025`.
The following receipts are in Leant's `test-church/quality-results/`,
separately from the Haskell reports above:

| Receipt | Confirmed result |
| --- | --- |
| `build-leant-06.log` | All 569 Leant unit tests passed in 389.71 seconds; the process exited zero and the E0 executable remained unchanged. |
| `matrix-final/results.json` | All 84 queries and 136 exact displayed terms passed live synthesis and independent kernel replay; all three fresh paired nil improvements passed. |
| `fixtures-repair/results.json` | All 90 compact queries passed live and kernel checks: 78 empty axiom inventories and 12 exact declared-premise inventories. All four pre-golden validations passed. |
| `church-djinn-final/results.json` and `church-exference-final/results.json` | Fresh 350/350 candidates per engine on unchanged E0; all 700 exact displayed terms passed independent kernel replay with empty axiom inventories. |
| `compact-comparison/results.json` | All four reviewed goldens match the preserved compact captures by offline comparison. The original receipt retains its three golden mismatches; no synthesis or kernel run was repeated for this comparison. |
| `ordinary-final/results.json` and `ordinary-review-audit.json` | All 26 ordinary fixtures completed, covering 175 synthesis commands. Review checked 99 exact changed-term kernel replays against query-specific axiom allowances; the original live exit 1 for 20 golden differences is preserved. |
| `synth-prove-review/review.json` | Separate proof-mode validation passed for six terms, two exact tactic applications, and the expected evaluation type error. |
| `ordinary-golden-application/application.json` and `composite-comparison/results.json` | Twenty reviewed ordinary goldens were updated; offline comparison of the preserved ordinary and compact captures matched all 30 goldens (265 synthesis commands). Neither synthesis nor kernel checking was rerun by the comparison. |

These historical E0 receipts precede the Leant-only accepted-exact-spelling
repair. It retains the first accepted candidate's original metadata and
authority, retries fresh alternatives within an already bounded group, and
neither refills the raw search window nor borrows a later duplicate's evidence.
Leant production commit `043a6a3d1562578f9aee8ad73ada4e02ddd4a52d` passed **all
578 unit tests serially in 533.23 seconds** and its executable build passed.
`dedup-build-acceptance.json`, `build-leant-dedup-02.log`, and
`build-executable-dedup-02.log` record those successful process exits and
executable SHA-256
`42c0c9c0a46a35302a04691a93fc68099c5e80bd91305e9a927ba9de9cec5cae`.
Fresh acceptance of that executable is recorded separately:

| Receipt | Confirmed result |
| --- | --- |
| `dedup-compatibility/results.json` and `dedup-independent-review.json` | All 30 fixtures/265 commands completed in 1,124.61 seconds with a 30-second synthesis timeout. All prior successes, first results, controls, and proof outputs were preserved. The sole drift removed one duplicate accepted spelling; no new spelling or remaining exact duplicate was found, and no retry at the earlier 600-second allowance was needed. |
| `dedup-gap-replay/results.json` | Both retained exact terms passed fresh kernel replay; their inventories contain only the declared `Gap.Token` and `Gap.polyGlobal` premises. |
| `dedup-final-comparison/results.json` | After the single reviewed Gap golden update, all 30/265 preserved captures matched offline. The original live exit 1 is retained; this comparison reran neither synthesis nor the kernel. |
| `matrix-dedup-complete.json` | Two disjoint live/kernel runs passed: `matrix-dedup` covers Djinn/Exference with 56 queries/87 terms, and `matrix-dedup-both` covers Both with 28/49. Total: 84 queries/136 exact terms, 112 empty and 24 declared-premise inventories, three paired Exference nil improvements, and three diversity proofs. |

All matrix type/term lists match E0, with unchanged settings: window 12,
display cap four, 10,000 Exference steps or Djinn choice points, and a
30-second synthesis timeout. Aggregation itself did not rerun synthesis or
the kernel. The [detailed Leant guide](https://github.com/VladimirReshetnikov/Leant/blob/main/test-church/quality.md)
keeps the complete receipt and review boundaries. The E0 700-term Church
corpus remains historical and was not rerun for this repair. Canonical
Djex code `2954b6d2` and its recorded 19-component acceptance are unchanged;
no new Haskell rerun is implied. The compact fixtures exercise
retained exact-vector reconstruction and structural combined-mode provider
staging under their original limits; the [policy guide](../docs/candidate-quality.md)
explains the reconstruction authority and unchanged resource bounds.

Each complete matrix covers seven examples, four policies, and three engine
selections. Every policy uses the same candidate window of 12, display cap of
four, 10,000 Exference steps or Djinn choice points, and a 30-second synthesis timeout.
The historical E0 matrix's unsplit source transcript has SHA-256
`96b6681ec583aa213df0d6a80d780eac17f36ba4e13827f98b1e8526a1851547`.

Kernel axiom inspection found empty inventories for all **112 closed terms**.
The other **24 terms** used only their explicitly declared provider premises;
they are accepted relative to those premises, not counted as closed proofs.
Each run's `QualityCandidates.lean` and `kernel-output.txt` preserve its exact
displayed terms and inventories. Across the complete matrix, three successful
functional-diversity proofs show that, for each engine selection, the diverse projection
outputs include both 11 and 29 when applied to those distinct inputs.

The paired nil comparison is a strict improvement in all three structural
profiles in both matrix generations. Exference legacy returns:

```lean
fun _ _ f x => match Sum.inr x with | .inl a => f a x | .inr b => b
```

Balanced, compact, and diverse each return:

```lean
fun _ _ _ x => x
```

Both before and after terms are independently kernel-accepted under the
original signature. The quality gate checks the direct last-argument selector
as well as removal of the explicit match; it does not infer success from a
smaller printed name or a changed structural-family label.

The ordinary review also preserves a counterexample to uniform improvement:
Djinn's first result for `synth-manual.txt` Q20 grew from two matches to three
for conjunction decidability. The exact new term passed kernel replay. No
final-score inversion or budget exhaustion was established; the transcript
has no truncation diagnostic, and membership of the smaller old term in the
new candidate pool is unknown. The measured nil and diversity gains therefore
do not imply that every first result improves.

The separate fresh E0 Church replay covers 315 total, 16 integer-provider,
and 19 explicit-default cases per engine. Every exact displayed term is
unchanged from its earlier corpus counterpart, so this is preservation
evidence. The runs use a one-candidate window, 4,096 Exference steps, and a
30-second synthesis timeout. Djinn retains `synth-budget off`, subject to the
shared deadline and intrinsic planning caps; startup, serialization, and
kernel replay are outside that synthesis timeout.

The earlier Leant `fb84b96` executable
`dab110ad2a7903ac4ef4883898d48532c00cc8c3b1b8d8748aac7744eedffb61`
passed 565 tests (`build-leant-04.log`), an 84-query/139-term matrix
(`matrix-accepted/results.json`), and all 700 Church terms
(`church-djinn/results.json` and `church-exference/results.json`). These
remain historical receipts for that executable. Its Church runs used one
candidate per query, 4,096 Exference steps, and a 30-second synthesis timeout;
Djinn retained its default unbounded choice-point budget, subject to the
shared deadline and intrinsic planning caps. The corpus source SHA-256 is
`782e4edaa5bf813e30e39ae02d52278ab0566315ebc521401a947b98c44cfd11`,
and the manifest SHA-256 is
`0c2954eeb36811ea065aba86187d09cd8761529de1acefd5111ccf088b189ed9`.
The [corpus guide](README.md) retains the universe/default policy and the
earlier Leant `4757569`/Djex `e2eb71e` rank-N and ordinary compatibility report.

The corresponding [Lean runner and guide](https://github.com/VladimirReshetnikov/Leant/blob/main/test-church/quality.md)
live in Leant's `test-church/` directory.
It preserves and kernel-replays every displayed alternative, checks explicit
axiom inventories, and uses the same projection distinction. Its explicit
Djinn budget avoids conflating `synth-steps` with Djinn choice points.
