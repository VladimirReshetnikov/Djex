# Synthesis with executable behavioral examples

The named form adds a Haskell Boolean condition to a type query:

```haskell
:synth f :: TYPE where HOST_BOOL
```

`f` names the candidate inside the condition. The type still determines which
implementations the engine may construct. The condition determines which
checked candidates may be reported as successes. A failed or inconclusive
condition must not consume the displayed-success limit. Search and evaluation
remain bounded; an exhausted search does not prove that the specification is
impossible.

For example, Church negation can be distinguished from the equally well-typed
identity and constant functions by its two Boolean observations:

```haskell
:load
:synth f :: (forall r. r -> r -> r) -> (forall r. r -> r -> r) where f (\yes _ -> yes) True False == False && f (\_ no -> no) True False == True
```

The bare `:load` establishes an empty source workspace with the real Prelude.
Use it for self-contained examples after default startup: the installed
declaration models describe synthesis providers and are not compilable Haskell
modules. For a project query, load the actual source modules instead.

This is execution of a host-language Boolean expression. It is separate from
the existing `:synth --where LENGTH_CLAUSE -- TYPE` form, which parses a bounded
Length contract and uses the sealed Length/Z3/replay pipeline. The latter does
not execute arbitrary Haskell. A finite collection of successful observations
is evidence about those inputs; it is not a proof of a function's behavior on
all inputs. A Haskell exception or divergence must not be reported as a
successful Boolean check.

See [Haskell behavioral runtime details](behavioral-constraints.md) for exact
workspace scope, worker lifecycle, deadlines, and observation accounting.

If GHC rejects a candidate's compatibility expression, the named Haskell path
can retry it using annotations from that same candidate's retained source
graph. Scoped forall signatures, argument types, and exact type applications
guide GHC elaboration. A missing graph or unsupported scope does not authorize
invented annotations. The retry checks the full requested signature and the
same predicate; an accepted retry displays exactly the expression checked.
It consumes no new candidate slot, and its extra compiler check is reported
separately. The first three compilation failures include bounded source samples
with the observation number, graph status, original expression, and retry
outcome. Runtime failures, false predicates, and timeouts are not retried.
The [priorities implementation report](reports/2026-09-07-synthesis-priorities-1-4.md)
tracks the current scope and remaining acceptance work.

## Six-operation corpus

The new corpus connects the existing Church type-acceptance suite to
value-sensitive behavioral checks. Its types come from
[`Church.hs`](examples/Church.hs), with the `map` wildcard resolved by the
existing manifest to `(a -> b)`. Live targets are rendered directly from that
manifest's expanded type trees. The source implementations do not enter the
synthesis environment. The adapters and reference comparisons are based on
[`Spec.hs`](examples/Spec.hs:123), whose differential tests already exercise
the source implementation; the new tests apply those observations to emitted
candidates instead.

| Operation | Original type | Finite observations per candidate |
| --- | --- | ---: |
| `not` | `Bool -> Bool` | 2 |
| `swap` | `Pair a b -> Pair b a` | 15 |
| `map` | `(a -> b) -> List a -> List b` | 200 |
| `append` | `List a -> List a -> List a` | 169 |
| `reverse` | `List a -> List a` | 40 |
| `filter` | `(a -> Bool) -> List a -> List a` | 200 |

Here `Bool`, `Pair`, and `List` are the rank-N Church encodings, rather than
the Prelude datatypes used to observe results. The unary list tests enumerate
all 40 lists of length zero through three over `[-1,0,1]`. Append tests all
pairs of the 13 lists of length at most two. Map tests four integer transforms
and a type-changing integer-to-Boolean transform. Filter tests constant true,
constant false, parity, negativity, and equality. Swap tests nine integer
pairs and six heterogeneous integer/Boolean pairs. These inputs expose order,
repetition, argument selection, element transformation, and predicate mistakes
that length-only conditions cannot distinguish.

All six operations are total. The separate 19 default-bearing Church cases
are outside this corpus. The predicates use local `let`-bound encoders and
decoders with explicit rank-N signatures, so the oracle helpers are not loaded
as named synthesis providers. No reference `not`, `swap`, `map`, `append`,
`reverse`, or `filter` implementation is available to search.

## Djinn's explicit alternative search

Djinn retains its ordinary `depth-first` default. Explicit alternative search
with `interleave` additionally explores reusable assumptions and later formula
plans. This combination is useful for behavioral conditions: the first
well-typed inhabitant may be an identity, projection, or constant that fails
the supplied examples. Sorting alone does not enable this extra search.

Each formula plan first follows the original LJT proof search. After its exact
first proof, a second stream enumerates beta-normal terms using the same
checked assumptions and exact atomic type identities. It can reuse a function
or a conversion bridge, introduce lambdas, and forward an existing function
after a partial application. This grammar applies to atomic and function
formulas; products, sums, and their eliminators continue through LJT. It does
not contain operation names, Church-specific construction rules, or reference
implementations. Quantified-type instantiations come from the existing checked
plan machinery.

Normal terms are explored by increasing number of variable or provider uses;
lambda introduction adds no cost to this size measure. An index selects only
heads whose exact remaining function type matches the current goal. A
conservative analysis can prove a finite maximum size and stop that plan's
normal-term stream after its final layer. Recursive or unresolved states keep
an unbounded stream: this extension is not an exhaustive decision procedure
for higher-rank or impredicative synthesis. Failed attempts, argument
partitions, the finite-bound analysis, and advancing a size layer all consume
the existing choice allowance.

Formula plans are admitted incrementally. Active formula plans give up their
turn after a raw proof or 64 observed choices. Streaming cycles among three
families: historical plans, exact-result specialization plans, and other
carrier plans. Each family retains its own incremental queue, so adding plans
in one family does not dilute the others' turn frequency. This also preserves
regular progress for function-valued carriers when the demanded result itself
has a simpler specialization. Empty families lend their turns to the others;
the first turn remains historical.
These are scheduling intervals, not additional search budgets.

Inside a focused carrier plan whose final result matches the demanded result,
the original LJT tail receives a 64-choice turn and the increasing-size
normal-form tail receives a 4,096-choice turn. Both hand over immediately after
a raw proof. The original
first proof is preserved, every choice is charged, and both continuations
remain available. Other plans and ordinary batch search keep the existing
equal 64-choice turns.

Small plans can group already-checked instantiation bridges that share an
exact result type, allowing different source schemes to cooperate without
adding unrelated instances. Every bridge keeps its own source type and
visible type arguments. Streaming also keeps an exact-result singleton bridge
live when multiple inputs have the same source scheme: a composition may need
to reuse that one specialization. The broader original plans remain available.

All emitted proofs still pass the existing independent checking and source
conversion. Rejected proofs and duplicates consume their original raw slots;
none of these paths refill a query's work allowance. Preserving the first LJT
proof of each plan does not promise the same first result across differently
scheduled plans. Named behavioral queries now consume Djinn's typed candidate
stream directly. A checked candidate reaches the predicate before the backend
collects its remaining pool, and `select first` leaves the continuation
unobserved after a success. The stream keeps deterministic discovery order;
ordinary unnamed queries retain their existing complete-batch ranking.

Each observation carries its own source association. Raw proofs rejected by
conversion and duplicates still spend the one search's raw-proof and choice
allowances; rejected predicates do not restart or refill it. Completion or a
late search error is observed only when the consumer reaches it. A successful
prefix does not claim that the rest of the search completed. Behavioral
`best` and `all` still inspect their bounded observation window before selecting
and presenting results.
For Djinn's singleton stream, `best-lookahead` counts candidate observations
without an improvement, rather than complete candidate pools.

## Reproduction and evidence

The later [streaming acceptance report](reports/2026-09-06-djinn-behavioral-streaming.md)
and [paired compact receipt](../test-church/receipts/behavior-streaming-final.json)
record the current streaming milestone. All 30 Haskell/Lean behavioral cells
passed at unchanged per-engine limits with independent exact-term replay,
alongside 943 affected unit tests and separate Lean quota, inconclusive, and
deadline-retention controls. Djinn's Haskell median first-visible result changed
from 54.581 to 1.978 seconds in the paired runs; the report preserves startup,
engine-specific, and single-run limitations. The earlier executable-specific
receipts below remain historical evidence.

Run against an already built executable; the runner does not build Djex:

```powershell
python test-church/behavior_probe.py --prepare-only --output test-church/results/behavior-prepared
python test-church/behavior_probe.py --djex PATH_TO_BUILT_DJEX_EXE --output test-church/results/behavior
```

The default matrix has **12 positive queries**, six for each Haskell engine,
plus one deliberately false predicate per engine. Each query uses its own
owned process and capture. `--engine` and `--operation` select explicit subsets.
The default `--window 256` sets both Djinn's raw proof-candidate limit and the
behavioral observation window (`quality-window`). Both engine work limits are explicitly
100,000, and selection is `first` under `balanced` ranking. Djinn's ordinary
`depth-first` strategy remains the default. The recorded Exference run passed
all six operations at these limits. Djinn passed all six with the wider
explicit Interleave calibration below. The separate process wall guard defaults to 300 seconds; independent
Boolean replay allows two seconds per assertion. Child processes belong to a
kill-on-close Windows Job, assigned before execution, or a dedicated POSIX
process group.

A larger frontier can be calibrated explicitly without changing the default:

```powershell
python test-church/behavior_probe.py --djex PATH_TO_BUILT_DJEX_EXE --window 4096 --djinn-strategy interleave --steps 100000 --budget 100000 --output test-church/results/behavior-window4096-interleave
```

This explicitly selects Djinn's `interleave` branch strategy and expands search
and observation allowances while retaining `select first`,
all finite assertions, isolated replay, and false-predicate controls. It does
not increase the step/choice budgets or time guards. The strategy is sent through
`:set djinn-strategy`, verified in the settings snapshot, and recorded in the
receipt; `--djinn-strategy depth-first` explicitly selects the ordinary order.
Strategy changes affect Djinn only, including when both Haskell engines are
selected for a run. Add
`--operation reverse --operation filter` for those two operations, or repeat
`--operation` with any of `not`, `swap`, `map`, `append`, `reverse`, and `filter`.
Omitting it selects all six. A pass at 4,096 is evidence at that configured
window, not evidence that the earlier 256-window attempt passed.

The completed six-operation Djinn run used a wider, explicit calibration:

```powershell
python test-church/behavior_probe.py --djex PATH_TO_BUILT_DJEX_EXE --engine djinn --window 65536 --djinn-strategy interleave --steps 100000 --budget 500000 --output test-church/results/behavior-djinn-six
```

This raises the raw/observation window to 65,536 and the shared Djinn choice
budget to 500,000. It retains the same predicates, full source types, balanced
ranking, isolated GHC replay, and deliberately false control. These are
caller-selected calibration limits, not new defaults or completeness bounds.
The [Djinn acceptance receipt](../test-church/receipts/behavior-djinn-common-result-first.json)
records the complete run, including independent replay. Use a smaller operation
subset when investigating a specific miss, while retaining its exact settings
and an independent receipt.

Every preparation or live attempt requires a fresh output directory; an
existing empty directory is also accepted. The runner rejects a nonempty path
before writing files, preserving earlier commands, captures, and receipts.
Use different paths for `--prepare-only`, live runs, and successive calibration
attempts. The receipt retains the exact window and selected matrix.

The runner preserves each exact displayed definition, compiles it at the full
polymorphic signature, and executes the finite condition again outside Djex.
Each candidate has its own module and imports neither other candidates nor
oracle controls. A driver imports only their exported check actions; it does
not add names to candidate scope. The live settings snapshot must also confirm
the requested backend and search limits before its result counts as coverage.
It also compiles six known witnesses that must pass and ten well-typed, total
wrong implementations that must fail. These controls appear **only in the
separate oracle-control module**. A further specialized swap control checks that the
observation distinguishes pair identity from swapping. It is explicitly
monomorphic: a total parametric inhabitant of the original heterogeneous
polymorphic swap type cannot serve as an incorrect swap control.

`results.json` separates live synthesis, compiler acceptance, and executable
observations. It records source/manifest/specification hashes, the executable
identity, exact commands, bounds, candidate definitions, and process receipts.
`--prepare-only` also emits `OracleControls.hs`; preparing an artifact does not
run or validate it. The corresponding Lean runner covers Djinn, Exference, and
Both, bringing the complete positive matrix to **30** cells across the two
host languages.

**Exference passed all six live Haskell queries** at window 256 and 100,000
steps under `balanced` ranking and `select first`. The
[current compact acceptance receipt](../test-church/receipts/behavior-exference-current-final.json)
retains every exact definition and full type, settings, per-query verdicts,
executable hash, and source/capture/replay hashes. This fresh run used the same
unchanged executable as the Djinn acceptance below, SHA-256
`d5d9f0112f300b4d33c0fbebdcf39a9d3aaf22db5b054c6411882c0c6651cefd`.
Its six exact definitions, full types, and observation counts match the
[earlier Exference milestone](../test-church/receipts/behavior-exference-grounded-first.json),
which remains available as historical evidence for its older executable.

| Operation | Candidates checked through success | Rejected by the predicate |
| --- | ---: | ---: |
| `not` | 5 | 4 |
| `swap` | 1 | 0 |
| `map` | 3 | 2 |
| `append` | 11 | 10 |
| `reverse` | 146 | 145 |
| `filter` | 30 | 29 |

Each query accepted one candidate with no predicate errors or timeouts. A
separate `forall a. a -> a where Prelude.False` query rejected all 256 observed
candidates and displayed no definition; the observation window was consumed
without refilling it. This control establishes rejection within that window,
not a proof that the type is uninhabited.

Independent GHC compilation and execution then passed **23 assertions**: one
complete finite condition for each of the six exact emitted definitions,
covering 626 observations in total, plus the 17 oracle controls described
above. Both compiler and execution exit codes were zero. This replay is
separate from the live worker's decisions and preserves each candidate's full
polymorphic type. The separate Lean oracle baseline passed 33 named
declaration/proof checks with empty axiom inventories; that baseline alone
does not establish live synthesis coverage.

**Djinn also passed all six live Haskell queries**, using explicit Interleave,
a 65,536 raw-proof/observation window, and 500,000 shared choices under balanced
ranking and `select first`. Its
[compact acceptance receipt](../test-church/receipts/behavior-djinn-common-result-first.json)
retains the six exact definitions, full types, settings, source and capture
hashes, and separate replay results. The unchanged executable SHA-256 was
`d5d9f0112f300b4d33c0fbebdcf39a9d3aaf22db5b054c6411882c0c6651cefd`.

| Operation | Candidates checked through success | Predicate false | Compilation errors rejected |
| --- | ---: | ---: | ---: |
| `not` | 6 | 5 | 0 |
| `swap` | 1 | 0 | 0 |
| `map` | 6 | 5 | 0 |
| `append` | 309 | 302 | 6 |
| `reverse` | 78 | 73 | 4 |
| `filter` | 103 | 101 | 1 |

Each query accepted exactly one candidate and reported no predicate timeout.
The 11 compilation errors were rejected before success; they are distinct
from false Boolean observations. All six accepted definitions subsequently
compiled at their complete original types in separate modules and passed
their finite conditions again. This second execution passed **23 assertions**:
the six candidate conditions, covering all 626 observations, plus the same
17 oracle controls. Compiler and execution exit codes were both zero.

The separate false-predicate query observed one candidate, rejected it, and
displayed no definition. It did not consume the entire 65,536-slot window;
the receipt preserves that actual count. Captured positive-query process
times ranged from about 14.5 to 53.7 seconds, including startup, bounded
candidate collection, compilation and predicate checks. They are measurements
of this run, not latency guarantees; `select first` does not avoid the internal
collection cost described above.

Together these receipts establish the **12 Haskell cells** of the behavioral
matrix on **one unchanged executable**, with each engine's explicit settings.
The separate [Lean acceptance receipt](https://github.com/VladimirReshetnikov/Leant/blob/main/test-church/receipts/behavior-lean-complete.json)
records the other **18 cells** across Djinn, Exference, and Both, including
independent kernel replay of each exact term and its finite assertion. Together
they complete the **30-cell behavioral corpus** at the recorded per-engine
limits. These receipts do not establish that every later build, strategy, or
well-typed input has been validated, and they remain distinct from aggregate
unit-suite results.
