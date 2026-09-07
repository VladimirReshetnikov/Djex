# Checked one-layer recursive-data cases

This priority-2 checkpoint adds useful case analysis to Djinn's existing
bounded recursive introduction. It does not complete the separate recursor
stage or priorities 1, 3, and 4. Lean integration and kernel acceptance are
separate from the Haskell evidence below.

## Search and source authority

The additional proof-producing plans keep declared datatype applications
opaque in both the goal and loaded function premises. Positive foralls still
introduce scoped skolems, and aliases normalize through the checked inventory.
Exact source atoms determine a finite inventory of one-layer constructor
shapes; it is not recursively closed under their fields. This prevents an
expanding datatype such as `R (Maybe a)` from generating an unbounded inventory.

Negative positions can use continuation-form case views. Their erasure is an
identity on source functions, followed by independent checking of the actual
constructor case tree. Positive positions can use actual declared constructors
at the exact specialized types. Constructor names receive their role from the
sealed source inventory; raw proof values cannot become constructors merely
because their spellings are uppercase. Hidden constructors and record selectors
remain unavailable. A visible record constructor can now expose a field even
when its selector function is hidden.

An initial case plan uses existing values; the following plan adds constructor
choices. This keeps supplied defaults reachable before closed constructors
multiply branch choices. The plans apply only to sequents supplying a recursive
datatype at a negative position and never authorize negative logical evidence.
Pure constructor introduction retains its existing finite layer bounds. Historical nonrecursive
search and its refutation boundary remain in place.

For explicitly interleaved alternatives, tuple components and case branches
use the existing fair bind. Each dependent search retains its freshness state
and original choice charges. Depth-first product enumeration is unchanged.

## Acceptance evidence

The public-query fixture in `test-integration/Spec.hs` provides datatype
declarations, synthesizes candidates, requires complete source graphs, renders
each at its full signature, and independently compiles and executes it with
GHC. No reference implementation is supplied to search. Finite observations
cover:

- `null`, `headOr`, `tailOr`, and shallow tree inspection;
- list aliases and a recursive datatype with a unary tuple field;
- an `unconsOr` tuple result;
- two independently typed list inputs whose observations form a pair.

Seven scenarios use a 64-candidate window. The independent two-list composition
uses 256; it did not pass at 64. Both product scenarios request interleaving.
All use a 50,000-choice budget and the generated Haskell replay has a 30-second
wall limit. All eight passed in the combined fixture (1.25 seconds). These are
finite behavioral checks, not universal laws.

The full Djinn unit suite passed 132 tests (49.42 seconds), including existing
scope, recursive-expansion, conversion, and negative-evidence boundaries. The
public source-graph suite passed 59 tests (0.27 seconds), including exact loaded
providers and mutually recursive families. The facade suite passed 101 tests
(11.33 seconds). Builds use GHC 9.12.4 with `-Werror`.

Logs are retained under `dist-newstyle/`: `priority-data-composition-behavior.log`,
`priority-data-authority-djinn-tests.log`, `priority-data-source-graphs-final.log`,
and `priority-data-facade.log`. After updating the visible-record-constructor
expectation and the outdated introduction-only omission message, all 99 CLI
tests passed (70.21 seconds). All 43 private source-graph checks passed (0.01
seconds), and four property groups passed 200 trials each (0.05 seconds).
Those logs are `priority-data-cli-tests-final.log`,
`priority-data-private-graphs.log`, and `priority-data-properties.log`.

## Remaining gates

Leant's working integration of `3ce26cfd` now passes the eight Djinn-only
live queries and independent Lean replay of the exact displayed implementations
and finite observations, with sixteen empty axiom inventories and an actually
falsified control. Its receipt is
`dist-newstyle/recursive-acceptance/djinn-v1/results.json` in Leant.

Full integration remains open. The Exference/Both run fails on the unary tuple
payload, and Exference misses `tailOr` within its 100,000-step bound. That
failed run performed no independent kernel replays. Close those search/evidence
cases, require all eight scenarios in all three engine modes, and rerun the
full Leant boundary suite with the fake-Z3 helper configured. Preserve the
negative-occurrence activation gate and positive-only constructor limits.
The [current re-triage](2026-09-07-synthesis-retriage.md) records the revised
execution order and evidence boundaries.

Supplied folds/recursors and their termination
evidence remain a separate required delivery. This checkpoint does not establish
general recursive synthesis, induction, or completion of the broader Church
behavioral corpus.
