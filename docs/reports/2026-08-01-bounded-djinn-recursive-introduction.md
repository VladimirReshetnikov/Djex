# Bounded Djinn recursive-constructor introduction — 2026-08-01

## Scope and outcome

Commit `3a70f68e` lets checked Djinn sessions retain recursive datatype
declarations and use their visible constructors for finite introduction. The
change complements Exference's bounded recursive elimination: Djinn can build
the first constructor layer of a requested recursive result, but it does not
case-analyze a recursive input, recursively call the synthesized definition, or
derive an induction principle.

This is particularly useful at a rank-N or impredicative payload boundary. For
example, with an opaque `Seed` and

```haskell
data Rec a = Done a | Again (Rec a)
```

the query

```haskell
(forall x. (Seed -> x) -> x)
  -> Rec (forall y. (Seed -> y) -> y)
```

can produce `Done` or `\value -> Done value`. The quantified payload has no
closed total inhabitant while `Seed` is opaque, so this result demonstrates
that the supplied impredicative value is transported through the real
constructor rather than replaced by an independently synthesized value.
Exference finds and independently checks the corresponding constructor term as
well.

## Constructor roles, not axioms

Recursive constructors remain datatype constructors in Djinn's formula and
proof lowering. They are not installed as global value-declaration axioms.
That distinction preserves constructor injections in the proof term, lets the
ordinary lowering and checker validate them, and keeps presentation names such
as `Done`, `Again`, and `ToEven` instead of treating them as variable-like
globals.

The checked environment obtains recursive datatype heads from the shared
`PreparedInventoryExpansion`. Classification happens after exact synonym
expansion, so direct, mutual, and alias-hidden recursion agree across the
backends; a phantom alias which erases an apparent cycle remains nonrecursive.
Only SCCs wholly made of those authoritative datatype heads may enter the
bounded compiler. Recursive synonyms and mixed or unmarked definition cycles
are still rejected. The historical low-level compiler retains its strict
all-cycle rejection unless a checked caller explicitly supplies the recursive
datatype set.

## Polarized one-layer rule

The formula compiler uses the following conservative matrix:

| Occurrence | Formula view |
| --- | --- |
| First recursive datatype in a positive expansion path | Expose exactly one constructor layer. |
| Recursive field beneath that layer | Keep the complete alias-normalized application as one atom. |
| Any recursive occurrence in a negative position | Keep the complete application as one atom; do not add case elimination. |
| Exact-opaque plan | Keep the complete application as one atom. |

The path guard stops after the first recursive head, rather than after the first
occurrence of each distinct head. That detail also bounds mutual recursion: in
`Even = ToEven Odd` and `Odd = ToOdd Even`, an `Odd -> Even` query may produce
`ToEven`, but lowering cannot continue through `Odd` and then reopen `Even`.
Recursive fields remain opaque formula atoms below the first constructor.

The exact-opaque plan is a necessary complement to positive introduction. It
preserves exact transport such as `Rec a -> Rec a`, whose candidate remains
`\value -> value` even though the primary positive view exposes constructors.
Both unfolding and atomizing recursive structure mark a translation
incomplete. A proof from either view is still checked and sound, but an empty
search cannot be promoted to proof-backed non-inhabitation. Thus
`Rec Seed -> Seed`, a closed seedless `Rec Seed`, and cyclic-only recursive
families finish as undecided. A recursive SCC which the query never reaches
does not mark that query incomplete, so complete negative evidence for an
unrelated formula remains available.

## Shared REPL projection

The normal REPL keeps a recursive declaration when all of its constructors are
visible and reports:

```text
recursive datatype; constructors are introduction-only in Djinn
```

Its recursive identities come from the prepared Exference session's exact
deconstructor metadata, not from re-walking already shaped source declarations.
Canonical heads are translated through the prompt's type renaming before the
Djinn session is sealed. Alias-hidden recursion therefore receives the same
introduction and elimination boundaries as a direct occurrence.

Visibility remains authoritative. If any constructor is hidden, the datatype
is projected as an abstract type before the recursive-introduction rule can
apply; hidden names never enter search or output. A later reference or sealing
repair can make the same degradation, and the REPL reports only the surviving
abstract-type reason rather than a stale introduction-only message.

Recursive records are deliberately not fully eliminable in Djinn. A visible
record selector is therefore retained as an axiom under either
`djinn-axioms` policy, because it is the supported route to the field when
recursive structural elimination is unavailable. Hidden selectors remain
excluded. Selectors of nonrecursive, fully eliminable records remain omitted as
redundant and may be used only to present an already derived structural field
projection.

## Verification boundary

The regression suites cover direct, mutual, alias-hidden, seedless, and phantom
recursion; exact recursive identity; rank-N constructor transport; absence of
recursive elimination and false negative evidence; unrelated complete
refutations; higher-kinded recursive constructors; hidden-constructor safety;
alias-recursive record selectors; and agreement between Djinn and Exference.

The completed verification was:

- 78 Djinn unit tests passed;
- 476 Exference unit tests passed;
- 84 shared CLI tests passed;
- 56 facade integration tests passed; and
- `cabal test all -j1 --test-show-details=direct` passed for the complete Cabal
  test graph.
