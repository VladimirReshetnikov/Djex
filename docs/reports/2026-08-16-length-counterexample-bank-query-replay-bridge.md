# Length counterexample-bank query-replay bridge

Date: 2026-08-16

## Outcome

Djex now exposes nominal scalar and binary-product SMT-LIB operations which
associate bounded counterexample banks with exact query-owned replay. The
production implementation landed in
`86fc2184ee115e44fa2a4927877786d81402db60`; its frozen characterization
landed in `84f0b140180c6c7cf9ac5f8f5351a90b8e917205`.

One operation freshly reproduces a validated counterexample through a
same-scope current query before recording only its natural inputs. The other
replays one exact sample currently retained by a same-scope bank. Neither
operation treats the old receipt or stored sample as evidence. Every hit is a
fresh candidate-specific receipt minted by the current query.

This is an additive pure association bridge over the existing bank kernel and
query-owned input replay. It changes no checked-problem or query identity,
canonical SMT-LIB bytes, execution profile, protocol, live-session behavior,
or solver authority.

## Fresh recording

`recordLengthSMTLibQueryCounterexampleInBank` and
`recordLengthSpinePairSMTLibQueryCounterexampleInBank` implement the same
nominally separated sequence:

1. require the bank to match the current query's counterexample-bank scope;
2. admit and record one bounded replay attempt;
3. extract only the supplied receipt's inputs and replay them through the
   exact current query;
4. require a freshly reproduced counterexample; and
5. pass the fresh inputs and caller-supplied coarse origin to the ordinary
   bounded insertion kernel.

The old receipt supplies an input vector only. It may have come from another
candidate query sharing the same candidate-independent scope. A miss against
the current candidate is ordinary and records nothing. A hit returns the new
current-query receipt; the bank retains neither the old nor new receipt and
stores no verdict.

Insertion remains the sole owner of vector validation, strict retention,
input-only duplicate recognition, origin replacement, newest-first promotion,
tail eviction, and storage statistics. The bridge does not reimplement or
weaken those rules.

## Exact retained-sample replay

`replayLengthSMTLibCounterexampleBankSample` and its `SpinePair` counterpart
accept exactly one opaque sample and one bank. Their precedence is:

1. current-query scope match;
2. full opaque-sample membership in that bank;
3. replay-attempt admission; and
4. exact current-query replay of the retained natural inputs.

Membership is not an input-vector search: a bounded scan of the retained set
must find the supplied opaque sample itself. The primitive neither selects nor
automatically replays any other sample, and replay does not promote, reinsert,
relabel, or rewrite the sample. Its only bank-statistic effect after admission
is the replay-attempt charge. A hit is a fresh current-query receipt; a miss
remains `Nothing`.

## Authoritative successor state

Both families return the bank alongside their result. Scope mismatch,
membership mismatch, and attempt-cap refusal occur before replay admission and
return the unchanged input bank. Once an attempt has been admitted, every
replay rejection, ordinary miss, successful hit, or later insertion refusal
returns the charged successor. Real replay work is never erased because a
subsequent step failed.

For recording, the full precedence is scope, attempt admission, fresh replay,
counterexample reproduction, then insertion. For retained-sample replay it is
scope, membership, attempt admission, then fresh replay. Callers must thread
the returned bank on both success and failure.

## Authority exclusions

The bridge deliberately provides no:

- whole-bank traversal, sample choice, candidate iteration, or lane
  scheduling;
- automatic recording from decoded models, simplification, applicable-domain
  analysis, or live observations;
- SMT-LIB emission, solver execution, live-worker use, or solver-status trust;
- command/session bank owner, concurrency policy, persistence, or
  serialization; or
- Djex frontend or Leant runtime behavior.

A same-scope match permits an attempted fresh replay only. A charged attempt
is accounting, not proof. A retained sample remains data, not evidence, and a
fresh receipt retains only its existing exact current-query authority.

## Frozen implementation metrics

The production commit changed two files by 293 insertions and one deletion:

| File | Lines | SHA-256 |
| --- | ---: | --- |
| `synthesis/internal/Language/Haskell/Synthesis/Internal/Semantic/Length/SMTLib.hs` | 1,898 | `5f882bcbcf190b07a719dc84157a9262b00fadd1b2ed88d49f7635a6a727df39` |
| `synthesis/src/Language/Haskell/Synthesis/Semantic/Length/SMTLib.hs` | 108 | `73fb35d3e36d7b655f70582ab5349d81ca45f9611c602417db85e02e7293a995` |

The ordered newline-terminated `sha256sum` manifest of those two files is
`5af8e8b47e751716646805bc3187fef08bc8bb95814d8d8bbd7824c09fb65b93`.

The characterization commit changed three files by 851 insertions and one
deletion:

| File | Lines | `testCase` tokens | SHA-256 |
| --- | ---: | ---: | --- |
| `synthesis/test-length/Spec.hs` | 16,886 | 325 | `4dc0b184c6f256ddcf68e6291433438aa551987bfa42d0080af8f15982cba67a` |
| `test-api/FacadeSpec.hs` | 2,882 | 31 | `f719dd14fee94bcabf33971117fede997921a194a3df8b02160f9ec62bbbb667` |
| `test-api/Spec.hs` | 714 | 9 | `cc033e25ecb774272b958733969b5ddc371a47a675195c6724c12f590c41f66e` |

The ordered newline-terminated `sha256sum` manifest of those three files is
`c99a5575555aaa1df1b3f9c2b9a58db8543a78d3ffe6af30b812c14c19bbd9eb`.

## Validation evidence

Frozen validation passed:

- 8/8 focused counterexample-bank bridge cases;
- 393/393 Length cases;
- 38/38 API cases;
- all 16 repository suites under strict warning-as-error settings, totaling
  1,831 tests;
- clean `cabal check`; and
- clean `git diff --check` before documentation changes.

The focused cases pin current-query receipt association, foreign same-scope
receipt misses and hits, origin-sensitive exact membership, attempt charging,
strict refusal precedence, insertion failure, duplicate MRU behavior, and the
nominal binary-product counterparts.

## Documentation boundary

The current usage contract is in the
[library guide](../library-api.md#prepare-a-bounded-replay-input-bank), while
the authority model is in
[semantic foundations](../semantic-foundations.md#counterexample-bank-query-replay).
The earlier
[nominal bank report](2026-08-16-nominal-length-counterexample-bank-foundation.md)
remains a point-in-time description of the storage-only checkpoint immediately
before this bridge. This report describes the current experimental contract,
not a stability, persistence, versioning, or backward-compatibility promise.
