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

## Reproduction and evidence

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
`depth-first` strategy remains the default. These are proposed
corpus limits pending live calibration, not a claim of current synthesis
success. The separate process wall guard defaults to 300 seconds; independent
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
Both, bringing the intended positive matrix to **30** cells across the two
host languages.

The independent oracle baseline has passed 17 Haskell assertions and the Lean
counterpart has passed all 33 named declaration/proof checks with empty axiom
inventories. **Live six-operation synthesis acceptance is pending.** This
baseline establishes that the oracles accept their witnesses and reject the
wrong controls; it does not establish that either engine can yet find all six
operations within the proposed bounds.
