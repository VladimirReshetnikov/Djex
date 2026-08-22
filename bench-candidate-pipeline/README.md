# SelectBest candidate-pipeline screen

This directory contains the one-shot, fail-closed, end-to-end screen for the
behavioral `SelectBest` candidate pipeline. It is benchmark-only: it imports no
private production module and adds no production instrumentation. It does
include a separate benchmark-only diagnostic sampler calibration, whose rows
are explicitly non-release evidence. The corrected protocol emits screen, row,
process-tree, and decision schemas at v2. The v1 attempt described below
remains immutable HOLD evidence and is never reinterpreted as v2 evidence.

The comparison is frozen to:

- baseline commit `0716144502a7dcd8bfa8755f57fd9ced58bf3b83`, binary
  SHA-256 `9d8bf7d37ee13e7933bfef61cb44b85fee0fe4807f44cb1e58b29baa4d9316b0`,
  ELF Build ID `8bb56dc58402bba8b692ca0c4a8b52264a58034f`, and plan SHA-256
  `8d98e5dfaea42e20b76667bd50e1d63fe629beac29bb9b017e5cbf90da20ef2a`;
  its Git tree is `7f7a068b1a50d4ebe28a6567cdc91121f3d4e825` and Git-archive
  SHA-256 is
  `68cc2830b0dc87f109d03bf4c60b0722b5a3ea32cb2f8a3361e78a73048e42e9`;
- candidate commit `aff7a5e8d0fe81f50b05c5073ff05f77e1ab68ca`, binary
  SHA-256 `37b7e3c25cdba445244c7253fc9c78d9007c77c994d6858ab7842c612aca1dac`,
  ELF Build ID `bfec9cf7e0bb7099c692a37894c04677427ba0ef`, and plan SHA-256
  `5aade84865cb49c05eec811718be5d2d2e7aa1c3d9922fd106c78984246a49d6`;
  its Git tree is `c87265e59f4b2b2fd9814a7dd76e94c56b28ee4e` and Git-archive
  SHA-256 is
  `3188206125975e224660994aeee5c1d45cec40eaf53bcdee8509ef58f43d68c9`;
- Z3 `/tmp/djex-z3-4.8.12.5tlMuM/root/usr/bin/z3`, SHA-256
  `e555c27efbbbdd63b6cb6d54abb4a7aeabacba8184593bb917c4a7c16cb6056c`,
  and exact ordinary mode `0755`;
- Debian package
  `/tmp/djex-z3-4.8.12.5tlMuM/z3_4.8.12-1_amd64.deb`, SHA-256
  `6742d8addd8a39df4d48945ef2323179966595d4c9249f4e92f44d83ad3a2ab3`,
  package/version/architecture `z3`/`4.8.12-1`/`amd64`.

The runner does not build. The detached binaries were rechecked with the exact
invocation `cabal build exe:djex -O2 --ghc-options=-Werror -j1`; both returned
`Up to date` without changing the frozen plan or binary hashes. The recorded
local evidence is
`/tmp/djex-pipeline-screen-root-20260822.QUs8ub/o2-attestation-manifest.json`,
SHA-256
`158931c7fcc6dd3c40943b959be11479baae17a7a01df90dd3783e9fd8cc426d`.
The runner verifies that manifest and its two exact logs. It also verifies the
plan compiler/Cabal/platform fields and binds each supplied binary to the sole
local `exe:djex` plan output. This is the mechanical `-O2` attestation; the
plan's `opt` directory name alone is not treated as proof.

The two raw plan hashes are also required to be structurally equivalent after
worktree-path normalization. The runner recursively replaces the corresponding
absolute detached root in every JSON string (including keys) with the literal
`<WORKTREE>`, then serializes with
`json.dumps(object, sort_keys=True, indent=2) + "\n"` using the default ASCII
behavior. Each normalized plan must be exactly 72,323 bytes with SHA-256
`586af7ef87a6d144ef2fd6deadabbf215be03aeb989180696cf58dbe3f77da68`;
that shared identity is recorded in provenance and rechecked after the screen.

The runner also pins the complete `ldd` object-hash sets for both Djex
executables and Z3, plus the exact `/usr/bin` Git, strace, readelf, ldd, and
dpkg-deb tools and the GHC/Cabal driver and compiler payload identities. All are
recorded and rehashed after the screen. The executing interpreter is likewise
pinned to resolved path `/usr/bin/python3.10`, version `3.10.12`, and SHA-256
`7d51cd6b48b521277f5caa4610a82126e315fa2be4df069823a8b1eeb5bd4a86`;
the documented launcher is `/usr/bin/python3`, which resolves to that payload.

Before the expensive run, commit the harness and `djex.cabal` additions. The
runner requires its repository to be tracked-clean, verifies that every
protocol artifact is tracked, and records the protocol repository HEAD, tree,
and Git-archive hash. Untracked files are deliberately ignored, including a
caller-owned ReplayLedger. The runner records and rechecks its own source hash;
the committed repository identity provides the non-self-referential binding.
The workload templates and result schema additionally have embedded,
preregistered hashes.

Run the cheap static and deterministic checks first. In addition to pure
fixtures, self-check launches one isolated sealed-Z3 memfd session with task
children deliberately hidden, captures Z3 4.8.12's deterministic parsed
cmdline state, and checks one already-exited sealed-Z3 lifecycle. It does not
run Djex or either benchmark workload:

```sh
PYTHONDONTWRITEBYTECODE=1 /usr/bin/python3 -B \
  bench-candidate-pipeline/benchmark.py self-check
```

Then invoke the screen exactly once, after the harness commit, with a new
absolute evidence path:

```sh
PYTHONDONTWRITEBYTECODE=1 /usr/bin/python3 -B \
  bench-candidate-pipeline/benchmark.py run \
  --baseline-root /tmp/djex-pipeline-screen-root-20260822.QUs8ub/baseline \
  --baseline-binary /tmp/djex-pipeline-screen-root-20260822.QUs8ub/baseline/dist-newstyle/build/x86_64-linux/ghc-9.12.4/djex-2026.7.17/x/djex/opt/build/djex/djex \
  --baseline-binary-sha256 9d8bf7d37ee13e7933bfef61cb44b85fee0fe4807f44cb1e58b29baa4d9316b0 \
  --candidate-root /tmp/djex-pipeline-screen-root-20260822.QUs8ub/candidate \
  --candidate-binary /tmp/djex-pipeline-screen-root-20260822.QUs8ub/candidate/dist-newstyle/build/x86_64-linux/ghc-9.12.4/djex-2026.7.17/x/djex/opt/build/djex/djex \
  --candidate-binary-sha256 37b7e3c25cdba445244c7253fc9c78d9007c77c994d6858ab7842c612aca1dac \
  --output /absolute/new-evidence-directory
```

The apparent timing options are not protocol knobs: `--outer-timeout` must be
exactly `600` seconds and `--sample-interval-ms` must be exactly `1.0`.
Supplying any other values is HOLD before the screen can run.

The output path must not exist. A failed or interrupted invocation is immutable
HOLD evidence; never replace a sample or reuse its output directory. A HOLD
decision records the primary failure, run ID, completed-row count, and hashes
of the flushed partial results, provenance, and schedule when present. Raw
artifacts remain for audit. The run ID is the SHA-256 of the absolute evidence
path. Catchable `SIGHUP`, `SIGINT`, `SIGTERM`, and `SIGQUIT` queue a termination
request, terminate the active process group when one is active, allow the
signal-deferred cleanup and host-control finalization sequence to finish, and
then take the same HOLD path. At the outcome-commit boundary the runner blocks
those four signals, folds every queued or already-pending request into the
decision, atomically publishes `decision.json`, and intentionally keeps them
blocked through one-shot process exit. `SIGKILL` and host failure are
inherently outside that guarantee.

## Immutable attempt-1 instrumentation HOLD

Protocol v1 ran exactly once at
`/tmp/djex-candidate-pipeline-screen-one-shot-20260822-b9f0ab0c`. It completed
all 16 trace preflights and W1/A warmup, then stopped before appending W1/B
warmup with `HarnessFailure: observed 0 sealed solver images`. Thus it contains
17 completed rows and **zero measured rows**. Zero measured rows cannot support
a speed, KEEP, or revert decision; the production route remains release-HOLD
until a separately preregistered corrected protocol completes.

The failing invocation was baseline cell B (`jobs=1`, `-N2`), not the candidate
route. Its sampler recorded direct child PID 494770, start identity, and CPU,
but not a retained executable image. The 17 earlier rows, provenance, host
controls, cleanup, and trace vectors independently validated. The attempt-1
tree contains 315 files and 677,149,306 bytes. Hashing the GNU `sha256sum`
records for every absolute file path in NUL-sorted order gives SHA-256
`c93ed82e6ad7ecd0b70ebe536699b6bf4486fce688462bd816ed912022a94051`.
That path is never reused, completed rows are never promoted into v2, and a v2
run is new evidence rather than a replacement sample.

The observed v1 failure cause was its pristine-only mutable-cmdline gate. The
launch vector was the exact six arguments, but Z3 4.8.12 deterministically
parses each `key=value` setting in place by replacing `=` with NUL. A delayed
`/proc/PID/cmdline` observation therefore saw the exact stable nine-token
parsed state and v1 rejected it instead of retaining the image. Incomplete
Linux task-`children` hints, inspection delayed until after traversal, and
silently discarded procfs/signature telemetry were additional v1
instrumentation gaps. A zombie can retain stat/CPU identity after
`/proc/PID/exe` is unavailable, so overall pass coverage alone did not prove a
live inspection. None of these findings retroactively promotes attempt-1.

## Immutable sampler-calibration v2-01 mode HOLD

The first diagnostic v2 sampler calibration ran once at
`/tmp/djex-solver-sampler-calibration-20260822-7920655b-v2-01`. It stopped in
the first baseline W1/B invocation before appending a row. Its durable
`results.tsv` therefore contains only the header, the decision records zero
completed invocations and zero captures, and the row attestation records zero
rows. The decision is HOLD; this is neither replacement evidence nor
performance or release evidence.

The process-tree evidence isolates the harness-only error. For exact-target
PID 506321 the sampler observed Z3 4.8.12's exact parsed argv shape 212 times.
Target, open, regular-file, size, and live device/inode gates also succeeded
212 times. The sole consistency mismatch was `mode`, also exactly 212 times:
the image was `0755`, but v2-01 incorrectly required `0500`. The production
Linux REPL had selected the plain descriptor-bound launcher, which copies the
pinned Z3 source's ordinary rwx bits; fixed `0500` belongs to the separate
access-checked launch variants. No descriptor was captured, so exact-target
cardinality correctly forced HOLD. End identity attestation passed and there
were no finalization failures.

The immutable evidence contains 13 regular files totaling 40,648 bytes. Its
decision SHA-256 is
`e4546cc010a2b7353629160278ece9614acc2406b799cdad2dbd6cca3b1087be`;
the first process-tree SHA-256 is
`612d718522538966717f086c2764b8cee7268c6f2f35fdb62b9fa5882d8654e8`;
the header-only results SHA-256 is
`504ec031e2ae8882af356d4e8a02ae18e4557a19298c16b354a34e69cad9c2a4`;
and provenance SHA-256 is
`859b97ecec96402c43ee6b790b517164503e06a4e353f7e402834d7fdf345feb`.
Start and end identity files both have SHA-256
`ca33ebebad5c0da1b07bf7ce2291c88ce6ae58918421bff34865bc76a3a069a0`,
and their attestation has SHA-256
`46e2f74e1f42cc3906f735caa9b4142679e47f08ae297c2eaac8092607cc8f86`.
The failed capture kept the expensive full `/proc` scan active on every pass,
so its 11.379314 ms mean sampling interval also exceeded the 3 ms gate. That
is a consequence of never reaching post-capture cadence, not an independent
performance result. The v2-01 directory is never reused; any corrected
calibration is a new diagnostic invocation from separately frozen bytes.

## Immutable sampler-calibration v2-02 cadence HOLD

The source-mode repair was frozen and the second diagnostic calibration ran
once at
`/tmp/djex-solver-sampler-calibration-20260822-c4a76370-v2-02`. It durably
appended and fsynced 42 valid W1/B calibration rows. Invocation 43 retained the
exact sealed Z3 image (source-derived mode 493, or `0755`), passed the singleton
recognized/captured PID-and-start identity attestation, and completed cleanup,
but its 3,101,996 ns mean sampling interval exceeded the frozen 3,000,000 ns
gate. The invocation therefore threw before a row could be constructed or
appended. Its input, stdout, and stderr bytes equal those of the 42 completed
rows, but invocation 43 is **not** a fully validated workload row: cadence
failed before the later sealed-image row checks and transcript validation, and
its process-tree record has no return-code field. No exit-status or semantic
PASS is inferred for it.

Although those invocation-43 files are coherent and preserved, the immutable
v2-02 decision and provenance do not cryptographically anchor that unappended
attempt. That evidence-durability gap is an additional protocol HOLD; the
files are descriptive diagnostic evidence, not a retroactive row.

This was a harness-only cadence defect. The v2-02 loop used a relative
`stop_event.wait(interval)` after every sampling pass, so each pass's own work
was added to the nominal 1 ms interval. The corrected loop schedules pass
starts against fixed monotonic deadlines. If a pass overruns, all missed ticks
coalesce into at most one immediate catch-up; the pass after that catch-up is
scheduled one full interval from its finish, even if the catch-up also
overruns. This removes relative-delay drift without permitting an unbounded
back-to-back sampling loop. The 3 ms mean gate and every performance/resource
gate remain unchanged. The failed invocation now also motivates a durable,
canonical failure-attempt manifest: it hashes the command, input, transcript,
RTS statistics, process tree, residue, and any trace files without fabricating
a result row, and both HOLD provenance and decision records anchor its digest.

The immutable v2-02 tree contains 307 regular files totaling 672,508 bytes.
Its decision SHA-256 is
`2852a3d5325bf8483842770db2d304628078108eb0c5e40d74db5fd91ee18e07`;
provenance is
`10aacd6cd628a7aa2d4b1f434a13e502c02745afb791a2d862fe250f135ca64e`;
the 42-row results file is
`d79296795821e4db6863d0ed10d52f00cac815e4ab72da6483784e7f2c9a553d`;
and invocation 43's process tree is
`c2136e5edb64201db5ca67d18cc45881005fbbbf1fc83ff7269c6ee226dadf56`.
The equal start/end identity files hash to
`44c53f45a2f23c0bbcc247c0fd56d000539b1a95583d1618af5a5bd3d44fcf68`,
and their attestation hashes to
`7abc50295a1179188876c56e324183e0ab8d84d2d4738eb490eb4fbbc1dd0274`.
This is diagnostic HOLD evidence only, makes no performance inference, and is
never reused. A v2-03 calibration may use only fresh output after the repaired
source and synthetic checks receive an independent audit.

## Frozen workloads and calibration

Both templates explicitly select the Exference backend and `best` selection,
use cell-specific `jobs`, set queue capacity 2048, disable unused arguments,
residual constraints, and multi-constructor patterns, set constraint deferral
to zero, use no REPL timeout, and load one empty environment. The 600-second
outer timeout is only a harness safety bound.

W1 is frozen at 128 search steps:

```text
:exference --where length result > length arg4 + mod (length arg4) 4 -- a -> a -> a -> a -> [a] -> [a]
```

It performs 24 assessments, all 24 are accepted by UNSAT, and SelectBest
renders 24 candidates before the sole truncation warning.

W2 is frozen at 192 search steps. Its corrected predicate is specifically on
`snd result` and `arg4`:

```text
:exference --where length (snd result) > length arg4 + mod (length arg4) 3 -- a -> a -> a -> [a] -> [a] -> ([a], [a])
```

It performs 48 assessments: 24 are accepted by UNSAT and 24 produce SAT models
and are replay-refuted. SelectBest renders the best six candidates before the
sole truncation warning. Assessment acceptance and the rendered best-tier count
are intentionally separate invariants.

The preregistration is backed by pinned real-Z3 baseline traces:

- W1 solver trace `strace.480432`, SHA-256
  `b8034f2fad5442f9894f45690fcf36673d9c70f6a0c91f687641ad8ea9697c22`;
- W2 solver trace `strace.479715`, SHA-256
  `80a3036759f33b1052eabc6327caef9fad37009075521cda271c76a3a02e6be4`.

Detached candidate `jobs=2`/`N2` calibration produced the same normalized
vectors:

- W1 trace `strace.484218`, SHA-256
  `cb7d94f4763f1b2ee5059332a8cf46f3d63f6b8ad825df53899e42a3526fcffd`;
- W2 trace `strace.484285`, SHA-256
  `87ca7ab91fa751140289c84eee075b387f4238d301a7ba01932d7421f6e2c58c`.

The normalized W1 vector SHA-256 is
`532772f41a2cfe8a362a20530239875f091700d2350880878ed6dee07681d883`;
W2 is
`b0c1d2d38e5de857b2e58a6b4beafb639b0db03691218b12b70c5541b0b679dc`.
Two real-Z3 repetitions and the candidate `jobs=2`/`N2` checks had exact
observable parity: W1 stdout SHA-256
`d551f8afbd6543077595d715a44f9de805b65e6a2636fd7eca489d14a920f554`,
W2 stdout SHA-256
`3d6d190cead367a38eb7524573400ea892321660966028a76aada810243eae80`,
and all six stderr SHA-256
`fa2baab14653b34ca38319e6a9c5b7977722d5ffa06d0b587d0b06ade47d6a62`.
The screen itself still requires byte-identical exit/stdout/stderr across every
cell and phase; calibration does not waive that check.

## Trace and process validation

Every workload/cell gets an untimed trace preflight. `Popen(start_new_session)`
makes the subprocess root PID the frozen session ID. The v2 sampler merges
recursive task-children hints with a bounded top-level `/proc` TGID scan for
that exact SID. It inspects synchronously before starting its sampling thread,
scans every pass until the first solver image is retained, scans at a fixed
50 ms monotonic target afterward to detect an additional exact-target
identity, and forces a final scan. No scan may enumerate more than 65,536 TGIDs
or take more than 50 ms, and the maximum gap between full-scan starts is 100
ms; missing or sparse scan evidence is HOLD. Cheap known-lineage and
task-children passes retain the 1 ms target between those full audits.

For a candidate PID, capture reads stat-before with PID/start/SID/state, rejects
zombies and foreign sessions, requires the exact
`/memfd:djex-z3-main-image (deleted)` target, and opens `/proc/PID/exe` **before**
reading mutable cmdline or environment views. It then requires the held fstat,
one of two frozen whole-byte live cmdline layouts, empty environment,
stat-after, readlink-after, live-executable stat, and held-fstat-after to agree
on the same live non-zombie PID/start/SID and device/inode. `exec_exact` is the
pristine six-argv layout. `z3_4_8_12_parsed_exact` is the source-backed stable
layout after Z3 4.8.12's `src/shell/main.cpp` parser replaces each of the three
`=` bytes in `smtlib2_compliant=true`, `timeout=1000`, and `rlimit=100000`
with NUL, yielding nine tokens. The two intermediate prefix-mutation layouts
are retried but never accepted; every other layout is rejected. The untimed
strace preflight remains the launch-vector oracle and still requires the
pristine six arguments at successful `execveat`.

The image must be a regular file with the frozen solver size and exact mode
`0755`. This is not a permissive `0500|0755` gate. On Linux the REPL's
`:set length-z3` path selects `mkLengthSMTLibDescriptorBoundExecutionConfig`;
the plain descriptor-bound Haskell sealer forwards the opened source metadata
mode, and its native sealer applies `source_mode & 0777`. The pinned regular Z3
source is exactly `0755`, so the staged image must also be exactly `0755`.
Provenance records the numeric source and expected image modes plus hashes of
the REPL selection, Haskell sealer, and native sealer sources in both detached
revisions; start/end checks rederive the same identity. The live and synthetic
fixtures emulate this plain production launch. The fixed `0500` staging mode
used by the effective-ID and execve-check access-checked variants is explicitly
rejected here because those variants are not the benchmark treatment.

Every held-image mode is numeric in the image manifest, and every attempted
exact-target capture records numeric first, last, and counted mode observations
in per-PID telemetry, including rejected modes. The descriptor remains open
across process exit and is hashed only after the timed interval. Every later
live inspection of a captured PID also stats its executable and requires the
retained device/inode, preventing a same-spelling memfd re-exec from hiding
behind the link target. At finalization, the set of every observed exact-target
`(PID,start-time)` identity must equal the captured image identity set and have
cardinality exactly one. Thus a second recognized exact-target process remains
HOLD even if its cmdline never reaches an accepted shape; zero images and
multiple captured images are also HOLD.

Executable-descriptor ownership is transferred only under a fail-safe local
owner. A close-hook failure is reported even when an emergency raw close
confirms the fd is gone; a descriptor that cannot be confirmed closed remains
in an explicit retry registry and makes the run HOLD. Snapshot, discard, and
primary-failure paths therefore cannot silently lose the sole fd owner.

Every observed same-session PID has canonical categorized telemetry: first and
last observation times, discovery-source counts, state and SID counts, target
and cmdline-shape counts, gate-success counts, exact signature- and
consistency-mismatch categories, errno categories for each procfs stage, and
capture source/time counts. Unexpected cmdlines have a bounded first
observation containing byte length, SHA-256, NUL count, token lengths, and only
constrained printable arguments. Environment contents are never recorded.
Scan counts, endpoints, maximum duration/gap, TGID bounds, categorized
enumeration errors, and policy are recorded beside it.

`/proc/PID/stat` is parsed as bytes so arbitrary non-UTF-8 or `)` characters in
an unrelated process name cannot kill sampling. A malformed unknown-TGID race
in the global scan receives one immediate bounded retry without manufacturing
a per-PID identity. Only a retry that resolves the identity is allowed; an
unresolved malformed record and the same parse failure for a known
same-session PID are HOLD. Normal ENOENT/ESRCH disappearance races are
categorized. `solver_observation_sha256` binds that material, including the
recognized-target/captured-identity equality attestation, in each result row,
and the whole object remains covered by `process_tree_sha256`. Consequently a
future miss remains HOLD but identifies whether discovery, liveness, target,
signature, descriptor retention, or identity consistency failed.

For preflights, the exact sampled solver PID selects one `strace -ff -yy` file,
but every emitted strace file is scanned for successful exact-target sealed
execs. Across all files there must be exactly one successful
`execveat(AT_EMPTY_PATH)` for the sealed-main-image target, it must belong to
the captured solver PID, and it must carry the full six-argument vector and
zero environment variables, preceded by exactly one successful
`setpgid(0, 0)` in that PID. Failed executable access probes do not count. The
parser reconstructs only successful returned byte prefixes from that PID's
fd-0 pipe `read`/`readv` stream and fd-1 pipe `write`/`writev` stream, joining
unfinished/resumed calls and tolerating split or partial writes. Other
processes and file descriptors cannot contaminate the protocol.

Exact echo nonces frame the four-command opening capability handshake, every
assessment status, and every conditional get-value/model exchange. W1 requires
24 repetitions of its exact modulo-4 symbolic program followed by UNSAT. W2
requires this exact ordered case/status vector:

```text
6  input_1 + 3  UNSAT
6  input_0 + 3  SAT + exact two-input model
18 input_1 + 4  UNSAT
18 input_0 + 4  SAT + exact two-input model
```

All W2 modulo witnesses and right-hand sides use `input_1`, the symbolic
mapping for `arg4`. The full canonical preamble, declarations, ordered
assertions, checks, get-value commands, models, barriers, and response order
are validated; arbitrary `(check-sat)` traffic or capability probes cannot
satisfy the workload contract. The post-callback readiness check is a
non-writing process-state check, so there is no second capability handshake.

The 1 ms sampler records explicit parent lineage, sampled process-session CPU,
aggregate peak RSS, sample count, achieved intervals, and maximum pass
duration. Pass starts use fixed monotonic deadlines rather than sleeping one
full interval after each pass. Missed ticks coalesce into at most one immediate
catch-up; the successor of that catch-up waits a full interval from its finish,
which bounds back-to-back work under repeated overruns. The SID fallback
deliberately adds bounded top-level `/proc` scan overhead inside the timed
interval: every pass before capture and nominally every 50 ms after. Pre-freeze
diagnostic helpers characterized full-scan start cadence; they were not
benchmark samples and do not promote attempt-1. The
corrected protocol independently freezes a 50 ms maximum scan duration and
100 ms maximum start gap as fail-closed quality bounds. After capture, exact
parsed argv removes any need for sub-millisecond launch racing, while the full
SID audit exists to detect an unexpected additional exact-target identity.
This overhead is part of corrected v2 for every cell and is reported rather
than subtracted. Every run must cover at least 0.98 of its wall
interval inside the first-start through last-finish observation window; the
initial and terminal gaps, maximum interval, and maximum pass duration must
each be at most 50 times the configured target, while the mean interval must be
at most 3 times the target. Row validation also requires the complete reported
fixed-rate scheduling-policy object to equal the frozen policy for the rounded
target interval; a missing or drifted policy is HOLD. Sparse CPU/RSS sampling
is therefore HOLD rather than silently under-counted. Cleanup rejects any
observed descendant identity or
same-process-group survivor and every non-directory private node, including
FIFO, socket, device, and symlink nodes. A very fast detached double-fork that
is neither sampled nor left in the original process group is outside this
literal cleanup guarantee; the evidence claims observed recursive lineage plus
same-group cleanup, not a cgroup/subreaper guarantee.

If an invocation fails after its artifact directory exists, `Screen.execute`
does not invent a partial result row. It instead atomically writes and fsyncs a
canonical `failure-attempt-manifest.json` in that invocation directory. The
manifest identifies phase, workload, cell, position, optional sample/Williams
row, and hashes/sizes the command, input, stdout, stderr, RTS statistics,
process tree, residue, optional query vector, and every extant trace file.
Release and calibration HOLD provenance and decisions include the verified
manifest references and their canonical aggregate SHA-256.

After v2 source hashes and self-check are independently frozen, the following
optional helper performs 64 exact baseline W1/B (`jobs=1`, `-N2`) captures in a
fresh directory. It is diagnostic-only, emits `release_evidence: false`, has no
candidate or speed comparison, and cannot replace either attempt-1 or a release
screen. Run it only on an explicitly quiescent host; do not run it merely as
part of static validation:

```sh
PYTHONDONTWRITEBYTECODE=1 /usr/bin/python3 -B \
  bench-candidate-pipeline/benchmark.py calibrate-sampler \
  --baseline-root /tmp/djex-pipeline-screen-root-20260822.QUs8ub/baseline \
  --baseline-binary /tmp/djex-pipeline-screen-root-20260822.QUs8ub/baseline/dist-newstyle/build/x86_64-linux/ghc-9.12.4/djex-2026.7.17/x/djex/opt/build/djex/djex \
  --baseline-binary-sha256 9d8bf7d37ee13e7933bfef61cb44b85fee0fe4807f44cb1e58b29baa4d9316b0 \
  --iterations 64 \
  --output /absolute/new-diagnostic-calibration-directory
```

Calibration rows use the dedicated
`djex-solver-sampler-calibration-row/v1` schema and `calibration` phase. Each
completed row is appended, flushed, and fsynced to `results.tsv` before the
next invocation; a partial row set is preserved on HOLD. The decision is
derived only from that durable `Screen.rows` sequence. Before row 1 and after
closing the results file, calibration independently reattests the protocol
repository, baseline source/build plan/binary and libraries, Z3 image,
libraries and package, tools/interpreter/runner, result schema, README, workload
templates, and sampler constants. Every end identity must exactly equal its
start identity. The provenance and decision record the start/end/attestation
hashes and pass flag; any capture, row, close, signal, evidence-write, or
identity drift is diagnostic HOLD with primary-failure precedence. Calibration
PASS means only 64/64 valid sampler captures under this diagnostic protocol; it
still makes no performance or release decision.

Immediately before the first preflight, the runner writes and hashes a
cgroup-v2 host-control snapshot. It records the process scheduling affinity,
the exact `/proc/self/cgroup` membership, the sole cgroup-v2 mount identity,
and every directory from the membership leaf through the visible mount root.
For each ancestor it preserves directory identity and the presence and content
of `cpuset.cpus.effective`, `cpuset.mems.effective`, `cpu.max`, and the complete
parsed `cpu.stat`. The task affinity must contain at least two CPUs and must
equal the nearest inherited effective CPU cpuset; a narrower taskset is not an
unregistered benchmark treatment.

After the last run has cleaned up—or on any HOLD path after the start snapshot
exists—the outer signal-deferred finalizer writes the corresponding post
snapshot before comparing it. Membership, mount, affinity, ancestor topology,
control-file presence/content, and `cpu.stat` field presence must be unchanged.
Every ancestor exposing `nr_throttled` and `throttled_usec` must expose them as
a pair with exact zero deltas, and at least one ancestor must expose the pair.
This walks beyond the leaf: on the preregistered host the effective controls
are inherited from `/sys/fs/cgroup/user.slice`, including CPU set `0-5`, memory
node `0`, and `cpu.max` value `max 100000`. Pre/post `/proc/loadavg` and
CPU/memory/I/O pressure-stall data are preserved as diagnostics only, with no
post-hoc threshold. The start, start-attestation, end, and window-attestation
hashes are bound into provenance/evidence and `decision.json`.

The primary wall interval starts immediately before `Popen` and ends when
`communicate` establishes child exit and drained pipes. Sampler shutdown,
sealed-image hashing, descendant audits, artifact writes, residue traversal,
and cleanup occur after that endpoint. `+RTS -sFILE` records allocation outside
application stderr.

Every invocation gets fresh per-run `HOME`, `TMPDIR`, XDG roots, and work
directory. One stable screen-wide absolute module-environment path is freshly
created empty before each sequential invocation and removed and verified after
it. This keeps Djex's printed Source-workspace banner byte-identical without
normalizing application output. Exact input, command/environment, stdout,
stderr, RTS statistics, process-tree manifest, traces, and query vectors remain
in the evidence directory.

## Cells and scheduling

| Cell | Binary | jobs | RTS |
|---|---|---:|---:|
| A | baseline | 1 | N1 |
| B | baseline | 1 | N2 |
| C | baseline | 2 | N1 |
| D | baseline | 2 | N2 |
| E | candidate | 1 | N1 |
| F | candidate | 1 | N2 |
| G | candidate | 2 | N1 |
| H | candidate | 2 | N2 |

After preflights, every workload/cell receives one untimed warmup. The eight
measured samples are never replaced. W1 uses Williams rows 1 through 8; W2 uses
rows 2 through 8 and then row 1. Odd samples run W1 first and even samples run
W2 first.

```text
1  A B H C G D F E
2  B C A D H E G F
3  C D B E A F H G
4  D E C F B G A H
5  E F D G C H B A
6  F G E H D A C B
7  G H F A E B D C
8  H A G B F C E D
```

`result-schema.tsv` defines every raw results column. P95 is nearest-rank; with
eight samples it is the maximum. Medians and gates use unrounded values.

## Decision rule

For each workload, using median wall costs:

```text
pipeline N2       = F / H
pipeline N1       = E / G
canonical shipped = B / H
matched shipped   = D / H
diff in diff      = (F / H) / (B / D)
serial drift      = E / A, F / B
baseline jobs     = C / A, D / B
```

KEEP requires all of the following:

- geometric means of `F/H` and `B/H` across W1 and W2 are strictly greater
  than 1.10;
- each workload's `F/H`, `B/H`, `D/H`, and difference-in-differences is
  strictly greater than 1.0;
- N1 route-on median and P95 costs `G/E` are at most 1.05;
- serial drift and baseline job controls remain within `[0.95, 1.05]`;
- H P95 is at most 1.05 times both F and D P95;
- median allocation ratios `G/E`, `H/F`, `G/C`, and `H/D` are at most 1.10;
- the corresponding process-tree CPU and aggregate-RSS ratios are at most
  1.25;
- every provenance, semantics, trace, topology, and cleanup invariant passes.

A genuine result such as 1.165x clears the substantive threshold and is worth
keeping when the other gates pass. The separately reported 1.25x tier requires
both workloads independently to reach at least 1.25x for both pipeline and
canonical-shipped comparisons. Missing that stronger tier does not turn a
greater-than-1.10 KEEP into speculation.

Producer-only and assessment-only phase balance was considered as a workload-
design heuristic but was not implemented and is not a release gate. No private
phase instrumentation was added. The decision rests on unreplaced production
end-to-end `F/H` and `B/H > 1.10` results together with the preregistered
semantic, route, drift, tail, allocation, CPU, RSS, provenance, and cleanup
controls above.
