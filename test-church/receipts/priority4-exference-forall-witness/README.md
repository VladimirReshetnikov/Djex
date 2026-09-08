# Exact forall-graph witness replay

These are the exact source artifacts emitted and checked by the independent
`maybeEither` graph probe. `Candidate.hs` retains the full original signature;
`Main.hs` evaluates the original seven-observation predicate with only its
candidate name rebound. The two predicate files preserve that transformation.
`graph.txt` and `compatibility.txt` are the associated checked graph and exact
compatibility projection. The receipt pins their raw bytes.

This is a supplied witness, not a live-search result. The corresponding live
query still checks 256 false candidates. See the
[receipt](../priority4-exference-forall-graphs.json) and
[engineering report](../../../docs/reports/2026-09-08-exference-forall-graphs.md)
for the separate acceptance boundaries and historical failures.

From the repository root, using GHC 9.12.4 and a fresh output directory:

```powershell
New-Item -ItemType Directory dist-newstyle/forall-witness-replay
ghc -v0 -O0 -fforce-recomp -i -itest-church/receipts/priority4-exference-forall-witness -outputdir dist-newstyle/forall-witness-replay test-church/receipts/priority4-exference-forall-witness/Main.hs -o dist-newstyle/forall-witness-replay/replay.exe
dist-newstyle/forall-witness-replay/replay.exe
```

The successful program prints `PASS exact-maybeEither-typed-graph`. Its
evaluation is independently bounded to two seconds. Compiling these saved
files repeats the compiler/predicate replay; it does not rerun the source
checker or search engine. The source-checker regressions are in the private
`exference-engine-tests` component.
