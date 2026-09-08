# Focused introduction reaches the complete extended Exference corpus

A fresh Haskell Exference run synthesizes and independently replays all
**13 extended Church operations**, including the previously missing
`maybeEither`. The original 256-candidate window, 100,000-step limit and
8,192-node queue limit are unchanged. A strict build, all **2,308 tests in
15 complete regression suites**, and the **350-signature corpus in each
Haskell engine** pass. The [receipt](../../test-church/receipts/focused-forall-search.json)
indexes a [compressed evidence archive](../../test-church/receipts/focused-forall-search.zip)
containing exact source snapshots, commands, captures, candidate replays and
the unsuccessful diagnostic experiments.

## What the search trace established

The preceding [checking correction](2026-09-08-parenthesized-forall-checking.md)
restored the original behavioral query and located a compatible unfinished
branch. The new queue observation follows that same branch through the actual
capacity boundary. It is never pruned in the observed prefix. At the final
snapshot, after 3,346 public batches, it remains at position 1,002 of 8,192,
with priority -60.28; the best retained node is -54.75 and the worst -73.15.
Both diagnostics preserve all 256 baseline candidate expressions, admission
steps and graph-availability results.

The branch has already applied the input eliminator, both nested Maybe
eliminators and both success continuations. It still needs two polymorphic
Either injections. The old worklist interleaves their construction, while
opening each forall temporarily exposes expensive parameter types before
introducing those parameters as local values. Queue capacity is not the cause
of this particular miss.

## General search change

Two changes keep introduction work together:

1. A new goal group precedes pending siblings when its goal types, local class
   assumptions and all visible local binding types contain no free flexible
   type variables. Otherwise it retains the existing deferred order, so
   sibling obligations can still determine shared type choices. This applies
   to lambda bodies and application dependencies. Provider substitutions are
   applied in their existing separate namespaces before testing the group.
2. A quantified function keeps the quantifier's existing heuristic estimate
   during `ContinueForallIntroduction`, until its arrow parameters enter the
   scope. The resulting body and actual local usage then receive their normal
   scores. Ordinary function goals retain their existing type complexity.

Neither change supplies a reference term, recognizes an operation name,
removes search alternatives, changes a step charge, or refills the behavioral
window. Scope, unification, dictionary and independent checking boundaries
remain in place. The exported type-complexity function is unchanged.

Completing determined goals first, by itself, still produced 256 false
candidates. The combined change finds a matching `maybeEither` implementation
in the original window: 255 false candidates and one true candidate, with no
errors or timeouts. The separately supplied positive and wrong-reference
controls pass, as does an actual 256-candidate False query. The unsuccessful
ordering-only experiment remains diagnostic evidence.

## Behavioral acceptance

The fresh full run accepts `foldr`, `foldl`, `length`, `fromMaybe`,
`maybeToList`, `listToMaybe`, `catMaybes`, `squashMaybe`, `isLeft`,
`maybeEither`, `either`, `numeralSuccessor` and `numeralAdd`. All 13 exact
displayed implementations compile and execute at their complete original
signatures. All 28 positive/wrong-reference controls and the live False query
pass. References and observation adapters remain outside synthesis inventories;
native Int length retains its two explicit numeric providers.

The new CLI regression uses an equivalent expanded signature with different
binder names, checks all seven original Int/Bool sum-of-options inputs, rejects
False, and replays the exact multiline definition in a separate GHC process.
It tests composition of both branches rather than requiring a particular
spelling of the implementation.

## Validation and remaining scope

The focused CLI regression passes, as does the complete 104-test CLI suite.
All 15 regression suites pass their complete inventories, totaling 2,308 tests
in 399.14 suite seconds. Strict builds use `-Werror`. Runtime sources,
executables and helpers remain unchanged during the recorded regression run.

The separate whole-signature run accepts all 350 cases in each Haskell engine
at the original 10,000-step and two-second query limits. GHC checks expanded
signatures, original alias forwarding and the 19 supplied-default wrappers.
Djinn also checks each candidate's graph root, erasure and absence of holes;
the Exference compatibility result receives GHC checking. These 700 type
inhabitation results are distinct from the 13 behavioral synthesis results.
The signature run preserves its own source/executable hashes.

The archive manifest records a SHA-256 and byte count for every artifact.
Instrumented historical snapshots are explicitly diagnostic; the production
search source contains no operation-specific tracing or reference matching.

This closes the 13-operation Haskell Exference extended matrix. The 19
explicit-default operations, Haskell Djinn's remaining behavioral coverage,
and the Lean Djinn/Exference/Both matrices retain their full requirements.
Leant's native acceptance still belongs to its Djex `4a4ed0fc` pin; it does not
validate this newer search change. The [roadmap](2026-09-07-synthesis-retriage.md)
and [completion register](2026-09-07-synthesis-priorities-1-4.md) remain open.
