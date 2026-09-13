# Maintaining the Church behavior ledger

The [160-cell table](behavior-ledger.md) and [machine-readable index](behavior-ledger.json)
cover every extended and supplied-default operation in the canonical specifications,
across Haskell Djinn/Exference and Lean Djinn/Exference/Both. The first catalog indexes
40 cells with historical acceptance, four with attempts without indexed acceptance,
and 116 with no indexed evidence. These are historical index counts, not a current
pass rate. No indexed evidence does not mean a case was never attempted.

The [catalog](behavior-ledger-catalog.json) selects nine principal receipt collections:
the original Haskell batches, native-Int corrections, focused Exference acceptance,
and the latest published native Exference integration. It is not an exhaustive
archive of all past experiments. An older accepted run and a later failed attempt
can coexist in one cell's history. Collection order is not a latest-run claim.

Each observation has a repository, receipt hash, JSON pointer and replay pointer.
Archived receipts additionally pin the archive and the exact member bytes. Settings
and runtime hashes are retained; source manifests and oracle controls are linked by
validated pointers and value hashes. Source manifests identify the tested inputs
when the original receipt does not record an exact source commit. The generator
does not reinterpret a receipt's baseline commit as the revision of a dirty build.
False queries are indexed as control rows outside the operation count.

Historical acceptance requires a recorded passing candidate and successful replay.
The known Lean adapter checks the recorded axiom inventories: implementations and
numeric primitives are axiom-free; only the named `maybeEither` oracle proof may
use `propext`. The index does not rerun those compilers, rehash every transitive
artifact, or certify the historical harness. Its claim is that the pinned receipt
records the result, with enough references to inspect the original evidence.

From a Djex checkout, with a matching Leant checkout available:

```powershell
python -B test-church/behavior_ledger.py --djex-root . --leant-root C:/Leant --output test-church
python -B test-church/behavior_ledger.py --djex-root . --leant-root C:/Leant --output test-church --check
python -B -m unittest discover -s test-church -p test_behavior_ledger.py
```

The Leant wrapper is `test-church/build_behavior_ledger.py --djex-root C:/Djex`.
It requires a Djex checkout containing the new generator; the older accepted
`lib/Djex` pin does not yet include it. This tooling dependency does not promote
the production synthesis dependency. Both generated outputs should match exactly.

To index another completed run, add an explicitly reviewed collection to the
catalog, including pinned receipt/member hashes, operation group, language/engine,
row pointer, settings, source/runtime identities and controls. Regenerate both
outputs and run the checks. Never edit generated status cells by hand. Unknown
operations, contradictory engine attribution, duplicate observations, missing
metadata and changed receipt bytes are errors. A passed process or reference-only
control cannot stand in for a synthesized implementation.

The next behavior work starts with Haskell Djinn `maybeEither` and supplied-default
selectors/nonempty reductions, followed by extrema and native-Int indexing according
to diagnosed failures. Newly accepted cells need live synthesis, original limits,
actual False controls and exact independent replay. The full 160-cell goal remains
open; this index adds no synthesis acceptance by itself.
