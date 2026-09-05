# Focused candidate-quality acceptance

These probes compare `legacy`, `balanced`, `compact`, and `diverse` with the
same search settings. They complement the shared structural-scoring,
raw-observation-limit, scope, sharing, visible-type-argument, and typed-handle
unit tests. They do not replace the Church corpus, the rank-N scope corpus,
or the existing Length behavioral contracts.
The [policy guide](../docs/candidate-quality.md) defines the score and the
distinction between raw search work, observed candidates, and output quotas.
Haskell compiler, CLI, and live Lean quality acceptance are recorded below.
The fresh full Lean Church replay on the same balanced executable is recorded
separately from the quality matrix and from the historical corpus receipts.

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

Live Lean quality acceptance was run against Leant `fb84b96` with vendored
Djex `2954b6d2`. The fixed executable SHA-256 is
`dab110ad2a7903ac4ef4883898d48532c00cc8c3b1b8d8748aac7744eedffb61`.
The following receipts belong to Leant's `test-church/quality-results/`
directory, separately from the Haskell reports above:

| Receipt | Confirmed result |
| --- | --- |
| `build-leant-04.log` | All 565 synthesis tests passed in 392.05 seconds. |
| `focused-repair/results.json` | Six live queries, 14 exact kernel-accepted terms, and two fresh paired nil improvements passed. |
| `matrix-accepted/results.json` | All 84 queries and 139 exact displayed terms passed synthesis and independent kernel replay; all three fresh paired nil improvements passed. |
| `church-djinn/results.json` and `church-exference/results.json` | Each engine produced 350/350 candidates; all 700 exact displayed terms passed independent kernel replay with empty axiom inventories on the same unchanged balanced executable. |

The full matrix covers seven examples, four policies, and three engine
selections. Every policy uses the same candidate window of 12, display cap of
four, 10,000 Exference steps or Djinn choice points, and 30-second timeout.
The receipt records unchanged executable hashes and successful live and
kernel process exits. Its source-transcript SHA-256 is
`96b6681ec583aa213df0d6a80d780eac17f36ba4e13827f98b1e8526a1851547`.
The focused repair transcript has SHA-256
`e5b4e9b6cab117d1d6a3bdb3a7bf34062ab2c2f29e39ef4b0f48a66221992ece`.

Kernel axiom inspection found empty inventories for all **112 closed terms**.
The other **27 terms** used only their explicitly declared provider premises;
they are accepted relative to those premises, not counted as closed proofs.
`QualityCandidates.lean` and `kernel-output.txt` preserve the exact displayed
terms and their inventories. The replay module also contains three successful
functional-diversity proofs: for each engine selection, the diverse projection
outputs include both 11 and 29 when applied to those distinct inputs.

The paired nil comparison is a strict improvement in all three structural
profiles. In the same fresh matrix, Exference legacy returns:

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

The separate full Church replay used one candidate per query, 4,096 Exference
steps, and a 30-second synthesis timeout. Djinn retained its default unbounded
choice-point budget; `synth-steps` is Exference-only. Both engines' receipts
record 350 cases, 350 candidates, successful live and kernel exits, and the
same unchanged `dab110ad...` executable identified above. The shared canonical
source hash is
`782e4edaa5bf813e30e39ae02d52278ab0566315ebc521401a947b98c44cfd11`,
and the input manifest hash is
`0c2954eeb36811ea065aba86187d09cd8761529de1acefd5111ccf088b189ed9`.
The [corpus guide](README.md) describes the universe and explicit-default
policy and retains the historical 700-term and ordinary compatibility
receipts under their original executable identities.

The corresponding [Lean runner and guide](https://github.com/VladimirReshetnikov/Leant/blob/main/test-church/quality.md)
live in Leant's `test-church/` directory.
It preserves and kernel-replays every displayed alternative, checks explicit
axiom inventories, and uses the same projection distinction. Its explicit
Djinn budget avoids conflating `synth-steps` with Djinn choice points.
