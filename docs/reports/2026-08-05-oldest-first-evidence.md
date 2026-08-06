# Oldest-first antecedent evidence in LJT — 2026-08-05

## Scope

A downstream consumer (Leant's `:synth`, which folds constructor and
library premises into engine goals as antecedents) measured that in
premise-rich searches no candidate reaching for the goal's own
arguments appeared within the first 300 proofs enumerated: every atom
choice point reached for freshly derived compositions first, so closed
junk built from introduction premises alone saturated any candidate
window before a proof that consumed an argument was reached. Sorting
downstream cannot repair this — the interesting candidates are not in
the batch to sort.

## The change

`Djinn.Internal.LJT` stored all three evidence pools newest-first:

- `addAtom` prepended each derived proof of an atom;
- `insert` prepended new atomic-implication bucket entries;
- `addNestImp` prepended nested implications.

All three now append, so `choose`/`select` consult evidence in arrival
order: the least-derived proof of an atom (a goal argument, a named
premise) is tried before compositions built from it, and implications
are branched over in the order the caller supplied them rather than in
reverse. Search space, termination, and completeness are untouched;
only enumeration order — and therefore which proofs fall inside a
caller's candidate cutoff — moves.

## What was measured and rejected

Two candidate remedies failed the same measurement before this one was
adopted. The `Interleave` fair strategy still never surfaced an
argument-using proof in 300 candidates: the junk tree is bushy on both
sides of every choice point, so fair alternation starves nothing but
also promotes nothing. Deferring atomic-implication application
(indexing first, applying to in-scope atoms as the alternative) merely
moved the cascade to atom-arrival time. Arrival order of evidence was
the operative variable in every experiment; this change aligns the
engine's preference with it.

## Calibration fallout

`djinn/test-unit` pins a choice budget to prove that formula plans
share one global budget rather than receiving fresh ones. The new
enumeration reaches the appended plan's direct construction with less
fuel, so the pinned budget tightens from 150 to 60 to keep that
construction out of reach; the assertion's subject is unchanged.
`test-cli`'s `.djexrc` startup case fails before and after this change
on the measurement machine (environmental); no other suite moved.
