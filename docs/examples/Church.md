# `Church.hs` — Algebraic Data Types as Pure Functions (Böhm–Berarducci Encoding in Haskell)

> A complete, annotated guide to `src/Church.hs`: a library that reimplements `Bool`,
> pairs, lists, `Maybe`, and `Either` — together with a large slice of `Data.List`,
> `Data.Maybe`, and `Data.Either` — **without using a single algebraic data type**. Every
> "value" is a rank-N polymorphic function equal to its own fold/eliminator. No constructors,
> no pattern matching, no `if`-`then`-`else`; only `λ` and application. The one concession is
> `Int` (for arithmetic) and `undefined`/`error` (for partial functions).

| | |
|---|---|
| **Source file** | [`src/Church.hs`](../src/Church.hs) (≈1,864 lines; Part I documents its foundations, sections 0–7) |
| **Part II** | [below](#part-ii--dictionaries-sorting-selection-search-and-numeric-scans) — sections 8–24 of the same file: dictionaries, four sorts, selection, `subsequences`/`permutations`, and the `<algorithm>`/`<map>`/`<numeric>` ports |
| **Tests** | [`test/Spec.hs`](../test/Spec.hs) — the Bool/Pair/List/Maybe/Either foundation cases |
| **Toolchain** | Cabal project, single GHC **9.12.4** (`cabal build`/`cabal test`); the pre-golf sources were observed to type-check on 9.8.1 / 9.10.3 / 9.12.4 with no flags — only 9.12.4 is validated for the current source (see [Appendix A](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes)) |
| **Status** | `cabal build` compiles clean; full suite **403/403 PASS** via `cabal test` on 9.12.4; see the workspace [`API-PARITY.md`](../../API-PARITY.md) for the Typelevel crosswalk |
| **Key extension** | `ImpredicativeTypes` — lets the `∀` live *inside* the type synonyms |

---

## The one idea

A data type is fully described by **what you can do with it** — its eliminator. Böhm and
Berarducci (1985) made this precise: in System F (polymorphic λ-calculus), every algebraic
data type is *isomorphic* to the type of its own fold, and the isomorphism is enforced for
free by parametricity. This file is that theorem, used as a programming technique. Replace
each constructor by a function ("here is how to handle me"), replace `case` by application
("apply the value to your handlers"), and let the universally-quantified answer type `∀e`
guarantee that nothing else is observable.

```haskell
type Bool       = ∀e. e -> e -> e                 -- = if-then-else
type Pair a b   = ∀e. (a -> b -> e) -> e          -- = uncurry
type List a     = ∀e. (a -> e -> e) -> e -> e     -- = foldr  (a list IS its own right fold)
type Maybe a    = ∀e. e -> (a -> e) -> e          -- = maybe
type Either a b = ∀e. (a -> e) -> (b -> e) -> e   -- = either
```

The `∀e.` is the whole trick. Because the *consumer* chooses the answer type `e`, a closed
inhabitant of `∀e. e -> e -> e` can only ever return one of its two arguments — so that type
has exactly two values, `true = const` and `false = const id`, and is genuinely `Bool`. The
free theorem is the soundness proof. A worked taste, every step β-reduction-verified:

```text
and true false
  = true false false           -- and b₁ b₂ = b₁ b₂ false
  = const false false          -- true = const
  = false                      ✓  (the Church Bool `false`)

fst (pair 10 20)
  = pair 10 20 const           -- fst p = p const
  = const 10 20  =  10         ✓  (test: pairTests, line 205)
```

Reading a Church program is mostly tracking *which eliminator is being applied to which
continuations*. The chapters below do exactly that, function by function.

---

## How to read this document

- **§1 Foundations** is the conceptual core: the Böhm–Berarducci correspondence, why rank-N
  and impredicative polymorphism are mandatory, the strict house rules, and the language
  extensions. Read it first.
- **§2–§7** are the catalogue: Booleans & Pairs; List constructors & access — including
  **`caseList`**, the one-step eliminator that packages the old "`uncons`, then destructure"
  idiom and now fronts most non-fold list functions; the everyday fold/map/filter toolkit
  (including the beautiful **`foldl`-as-`foldr` CPS trick**); the advanced
  accumulating/parallel/unfold combinators and `transpose`; `Maybe`; `Either`.
- **Appendix A** documents the GHC-version incompatibility and the minimal fixes that make
  the file compile on the current toolchain — including the precise root cause (deep
  subsumption + Quick Look impredicativity tightening).
- **Appendix B** collects the recurring encoding tricks; **Appendix C** indexes the
  functions of sections 0–7.
- **Part II** (chapters 8–15 and Appendix D, further down this document) continues into the
  algorithms half of the same file — sections 8–24: dictionaries, sorting, selection,
  search, and the `<algorithm>`/`<map>`/`<numeric>` ports.

Every β-reduction trace in this document was re-derived by hand from the source and
cross-checked against the expected values in `test/Spec.hs`; many were additionally
confirmed by running the compiled module.

---

## Table of contents

1. [Foundations: the encoding, the five types, the aliases, and the house rules](#1-foundations-the-encoding-the-five-types-the-aliases-and-the-house-rules)
2. [Church Booleans and Pairs](#2-church-booleans-and-pairs)
3. [List constructors and sublist access](#3-list-constructors-and-sublist-access)
4. [Folds, maps, filters, and the everyday list toolkit](#4-folds-maps-filters-and-the-everyday-list-toolkit)
5. [Accumulating maps, parallel N-ary combinators, unfolds, and transpose](#5-accumulating-maps-parallel-n-ary-combinators-unfolds-and-transpose)
6. [Church Maybe](#6-church-maybe)
7. [Church Either](#7-church-either)
- [Appendix A — GHC compatibility: why it broke after 9.8, and the fixes](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes)
- [Appendix B — Encoding tricks and design notes](#appendix-b--encoding-tricks-and-design-notes)
- [Appendix C — Function index](#appendix-c--function-index)
- [Part II — Dictionaries, Sorting, Selection, Search, and Numeric Scans](#part-ii--dictionaries-sorting-selection-search-and-numeric-scans) — chapters 8–15 and [Appendix D](#appendix-d--function-index-part-ii)

---

## 1. Foundations: the encoding, the five types, the aliases, and the house rules

This chapter covers the conceptual core of `Church.hs`: the encoding scheme it commits to, the five term-level types it builds everything from, the documentation-only aliases layered on top, the prerequisite `Int` comparisons, and the language extensions that make the whole thing type-check. Everything downstream in the module is an exercise in programming against the eliminators introduced here, so it pays to be precise about what these definitions *are*.

### The encoding: Böhm–Berarducci, not "Church numerals"

The folklore name is "Church encoding," but what this file actually implements is the **Böhm–Berarducci encoding**: the precise, type-directed version of the idea that any (covariant, strictly-positive) algebraic data type is isomorphic to the type of its own **fold / eliminator / non-dependent recursor**. Church's original 1930s encodings were untyped λ-terms; Böhm and Berarducci (1985) showed that in System F — polymorphic λ-calculus with `∀` — these encodings become *exact*: the encoded type and the ADT are isomorphic, and the isomorphism is mediated entirely by parametricity. Haskell with `RankNTypes`/`ImpredicativeTypes` is essentially a surface for System F, which is exactly why this file is possible at all.

The recipe is mechanical. Given an ADT

```haskell
data T = C₁ τ₁,₁ … τ₁,k₁ | … | Cₙ τₙ,₁ … τₙ,kₙ
```

its Böhm–Berarducci encoding is

```text
T  ≅  ∀e. (handler for C₁) -> … -> (handler for Cₙ) -> e
```

where the handler for a constructor `Cᵢ : τᵢ,₁ → … → τᵢ,kᵢ → T` is a function `τᵢ,₁ → … → τᵢ,kᵢ → e`, **with every recursive occurrence of `T` replaced by the answer type `e`**. A value of `T` *is* the function that, given one handler per constructor, dispatches to the right one with the right field values — i.e. it is its own case-analysis / fold. Construction becomes "supply yourself to the matching handler"; pattern matching becomes "apply the scrutinee to the handlers." There are no constructors and no `case`; there is only application.

The `∀e.` is the load-bearing part. Because `e` is universally quantified *inside* the type (rank-2 position), the **consumer** picks the answer type at each use site. That is precisely what makes the encoding an iso rather than a leaky representation: parametricity (Reynolds' abstraction theorem / Wadler's "Theorems for Free!") guarantees that a closed inhabitant of `∀e. … -> e` can do *nothing* with its arguments except apply the handlers it was given, in the structure dictated by the type. It cannot inspect, branch on, or fabricate an `e` out of thin air. So the only inhabitants of `∀e. e -> e -> e` are the two projections — exactly `True` and `False`, no more, no less. The free theorem *is* the soundness proof of the encoding. This is the parametricity argument the file's header comment gestures at when it says rank-N "preserves parametricity and prevents inspection of the encoding's internal representation."

### Why rank-N / impredicative polymorphism is mandatory

Two distinct demands stack here:

- **Rank-N (specifically rank-2):** the `∀e.` sits to the left of an arrow that is itself an argument. `not :: Bool -> Bool` expands to `(∀e. e->e->e) -> (∀e. e->e->e)`; the argument is a *polymorphic* value the body must be free to instantiate at whatever answer type it needs. Standard Hindley–Milner forbids `∀` in argument position; `RankNTypes` (here pulled in transitively via `ImpredicativeTypes`) lifts that restriction.
- **Impredicativity:** the file does not write these `∀`s out longhand at every call. It hides them behind *type synonyms* — `type Bool = ∀e. e -> e -> e` — and then instantiates polymorphic type variables *at those polytypes*. Writing `Maybe (a `Pair` List a)` instantiates `Maybe`'s argument at `a `Pair` List a`, which unfolds to `∀e. (a -> List a -> e) -> e` — a polytype shoved into a type-constructor argument position. That is an **impredicative** instantiation (a quantifier instantiated with a type that itself contains a quantifier), and only `ImpredicativeTypes` permits it. Without it the synonyms could not nest, and the whole layered design — `List (Either a b)`, `Maybe (a `Pair` List a)`, `Tuple (List a)` — would be rejected. GHC 9.2+'s Quick Look impredicativity inference is what makes this ergonomic enough to use without per-call annotation, modulo the occasional hint discussed below.

### The house rules

The header comment commits the module to a strict discipline, and it is worth internalizing because nearly every clever trick downstream is a consequence of one of these rules:

1. **No ADTs.** Not `Bool`, not `(,)`, not `[]`, not `Maybe`/`Either` — not even in local helpers or `let`-bindings. Every "data type" must be a rank-N function. This is enforced by the module's *positive* import list — `import Prelude (Int, otherwise, (<=), (+), (-), (*), succ, const, id, flip, (.), ($), undefined, error, liftA2, (<*>))` — which admits only the sixteen sanctioned primitives, so the Prelude's ADT vocabulary is simply never in scope and the module's own term-level definitions claim the names.
2. **No `if`-`then`-`else`.** Branching must go through the Church `Bool` itself: a `Bool` *is* its own `if` (`b then else`), so conditionals are written as `cond thenBranch elseBranch`. You will see `null xs nothing (…)` used directly as an if-expression — the Church Bool returned by `null` is applied to the two branches. (`ltInt` etc. and the guards inside them are the sanctioned boundary; see below.)
3. **Only `Int`.** The single permitted concrete primitive type, used for arithmetic and comparison in the prerequisites and in `length`/`replicate`. No other ground type leaks in.
4. **`undefined`/`error` are allowed,** because they introduce no ADT — they inhabit every type via ⊥. They are the encoding's way of expressing partial eliminators: `head` of an empty list, `fromJust` of `nothing`, the out-of-bounds arms of `at`.

### LANGUAGE pragmas and OPTIONS — what each one buys

```haskell
{-# LANGUAGE UnicodeSyntax, TypeOperators, ScopedTypeVariables, ImpredicativeTypes, PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fno-max-relevant-binds #-}
```

- **`UnicodeSyntax`** — lets `∀`, `∷`, and `→` stand in for `forall`, `::`, and `->`. Purely cosmetic, but it makes `type Bool = ∀e. e -> e -> e` read like the System F judgment it is. (The file mixes `∷` and `::` freely and uses `∀` throughout, but spells arrows as ASCII `->`; the test suite additionally uses `→`.)
- **`TypeOperators`** — enables the infix type constructor `` `Pair` `` and `` `Either` ``, so signatures read `a `Pair` b` and `List a `Pair` List b`. These are ordinary (prefix) type synonyms written infix via backticks; the extension is what makes a type-level name usable infix. Pays off enormously in readability: `s `Pair` List b` versus `Pair s (List b)`.
- **`ScopedTypeVariables`** — brings the `∀a.` in a top-level signature into scope over the body, so the visible type applications — `null @(List a)` in `transpose`, `scanr @a @(List a)` in `tails`, `foldr @(a -> List a)` in `composeFlatten` — can refer to the *same* `a` rather than a fresh one. Essential for the impredicative cases where GHC needs to be told which polytype an instantiation involves.
- **`ImpredicativeTypes`** — discussed above; the keystone. Lets the `∀`-bearing synonyms live inside other synonyms' arguments and be instantiated at polytypes.
- **`PartialTypeSignatures`** — permits `_` as a type wildcard, used to write "don't-care" slots: `fst ∷ a `Pair` _ -> a`, `left ∷ a -> Either a _`. The module uses this both to suppress irrelevant type detail and, crucially, to hand GHC's inference a partial skeleton it can complete when full impredicative inference would otherwise stall. A later compile-checked sweep pushed the idiom further: 44 fully-inferable higher-order *argument* types — the mapped function in `map ∷ _ -> List a -> List b`, the steps of `foldl`/`scanl`/`zipWith`, and kin — are now spelled `_` as well. The elaborated types are provably unchanged (the interface dump is identical before and after); Appendices C and D index every function at its full type.
- **`TypeApplications`** — used in exactly three places in these foundations sections (0–7): `null @(List a)` in `transpose`, `scanr @a @(List a)` in `tails`, and `foldr @(a -> List a)` in `composeFlatten` (the algorithm sections of Part II retain a further handful), each pinning an otherwise-unguessable polytype instantiation (without the application GHC cannot tell at which layer of a nested synonym an eliminator is being taken). An automated compile-checked sweep removed every type application and signature `forall` GHC does not require, so the survivors are *exactly* the ones inference needs — see Appendix A.
- **`-Wno-partial-type-signatures`** — silences the warning GHC emits for every `_` wildcard; with `PartialTypeSignatures` deliberately pervasive, the warnings would be pure noise.
- **`-fno-max-relevant-binds`** — removes the cap on how many candidate bindings GHC lists in a type error. In a module where every value has a deeply nested rank-N type, a truncated "relevant bindings" list is nearly useless; this makes the (inevitable) inference errors legible while developing.

### Section 0 — Prerequisites: `Int` comparisons returning a Church `Bool`

These five functions are the sanctioned bridge between the one allowed primitive (`Int`) and the encoding. Only `leInt` performs a *genuine* primitive comparison via a guard; the other four are derived from it algebraically — `geInt`/`gtInt` by `flip`, and `eqInt`/`ltInt` through the section-2 combinators `eqFromLE`/`ltFromLE` (below). All five return a **Church-encoded `Bool`** — `true` or `false`, the rank-N projections — not a Prelude `Bool`.

```haskell
ltInt :: Int -> Int -> Bool
ltInt = ltFromLE leInt

gtInt :: Int -> Int -> Bool
gtInt = flip ltInt
leInt :: Int -> Int -> Bool
leInt x y
  | x <= y    = true
  | otherwise = false
eqInt :: Int -> Int -> Bool
eqInt = eqFromLE leInt
geInt :: Int -> Int -> Bool
geInt = flip leInt
```

The guard `| x <= y` *is* an honest use of a Prelude comparison and a non-Church branch — but it is confined to this one primitive, which the house rules carve out explicitly as the `Int` exception. Everywhere else in the module, "is this `Int` smaller?" is asked through these functions, and the answer arrives as a `Bool` you can immediately use as an if-expression. The result type in each signature is the module's own `type Bool = ∀e. e -> e -> e`, not `Prelude.Bool` (which is simply never imported), so `leInt n 0 nil (x `cons` …)` in `replicate` typechecks by *applying* the returned Bool to its two branches — the no-`if` rule made operational. They are exercised transitively throughout: e.g. `replicate` (`leInt n 0`), `lexicographicLE`, and the sort predicates in the test suite all bottom out here.

### Section 1 — the five Church-encoded types

Each of the five is the Böhm–Berarducci eliminator of the corresponding Prelude ADT. Read every definition as "I am the fold: give me one handler per constructor and I will run myself."

```haskell
type Bool       = ∀e. e -> e -> e
type Pair a b   = ∀e. (a -> b -> e) -> e
type List a     = ∀e. (a -> e -> e) -> e -> e
type Maybe a    = ∀e. e -> (a -> e) -> e
type Either a b = ∀e. (a -> e) -> (b -> e) -> e
```

**`Bool = ∀e. e -> e -> e`.** The eliminator of `data Bool = False | True`, except the file orders the handlers `true`-then-`false` (the standard "if-then-else" order). Two nullary constructors → two `e` handlers → the answer is one of them. A `Bool` *is* `if`: `b thn els`. Inhabitants are exactly `const` (`true`, returns the first) and `const id` (`false`, returns the second). Used directly as a conditional everywhere the no-`if` rule bites.

**`Pair a b = ∀e. (a -> b -> e) -> e`.** The eliminator of `data Pair a b = MkPair a b`. One constructor with two fields → one handler `a -> b -> e` → apply it. A pair *is* `uncurry`-waiting-for-a-function: `p f = f x y`. This is exactly the λ`f → f a b` the header comment names. Note there is no projection primitive — `fst`/`snd` are recovered by passing `const`/`const id` as the handler. Test: `fst (pair 10 20) == 10`, `snd (pair 10 20) == 20`.

**`List a = ∀e. (a -> e -> e) -> e -> e`.** The eliminator of `data [a] = (:) a [a] | []`, with the recursive `[a]` field replaced by `e` in the cons-handler. **This is precisely the type of `foldr` with its arguments flipped** — `List a` *is its own right fold*. A list is the function that, given a cons-replacement `c :: a -> e -> e` and a nil-replacement `n :: e`, rebuilds itself with `c` for every `(:)` and `n` for `[]`. Hence `foldr f z xs = xs f z` is the identity-stripped fold (Section 5.3), and `nil = const id` (ignore `c`, return `n`), `cons x xs c = c x . xs c` (apply `c` to the head, then recurse). Every list operation in the module is "choose the right `c` and `n`."

**`Maybe a = ∀e. e -> (a -> e) -> e`.** The eliminator of `data Maybe a = Nothing | Just a`, handler order `nothing`-then-`just`. `nothing = const` (return the nothing-handler, drop the just-handler); `just x _ j = j x` (apply the just-handler to the payload). A `Maybe` *is* the `maybe` combinator awaiting its two cases. Test: `just 5 "nothing" show == "5"`, `nothing "nothing" show == "nothing"`.

**`Either a b = ∀e. (a -> e) -> (b -> e) -> e`.** The eliminator of `data Either a b = Left a | Right b`. Two unary constructors → two handlers → dispatch. `left x l _ = l x`, `right x _ r = r x`. An `Either` *is* the `either` combinator partially applied to the value. Test: `left 5 show (show . negate) == "5"`, `right 10 (show . negate) show == "10"`.

### Section 2 — aliases with designated semantics

These add no new representations; they are pure `type` synonyms whose *purpose* is documentation of intent that the type system cannot encode. They are the file's answer to "how do I express an invariant Haskell can't check?" — name it.

```haskell
type Tuple a  = List a
type Dict k v = List (k `Pair` v)
type Equal a  = a -> a -> Bool
type LE a     = a -> a -> Bool
type Matrix a = List (List a)
```

- **`Tuple a = List a`.** A homogeneous, fixed-length (assumed non-empty) tuple, represented as a `List` *used homogeneously*. The type system cannot enforce "all instances same length," so the name carries the contract. This is what lets `zipWithN`/`scanlN`/`mapAccumLN` be N-ary: they take a `Tuple (List a)` — a list-of-lists viewed as "k parallel lists" — and advance over the outer structure with `map head`/`map tail` (a recursion realized inside `transpose`, through which all three now route), treating each row as one position of an N-tuple. From the typechecker's view it is *identical* to `List`; the `scanlN` comment in the source spells this out explicitly.
- **`Dict k v = List (k `Pair` v)`.** An association list of Church pairs, with the documented contract that keys are unique under some equality and order is insignificant. Purely an alias — every `Dict` operation is a `List` operation.
- **`Equal a = a -> a -> Bool`** and **`LE a = a -> a -> Bool`.** *Structurally identical* synonyms (both `a -> a -> Bool`, returning a Church Bool), distinguished only by intent: `Equal` flags an equality test, `LE` flags an ordering test. The `LE` comment notes the standard trick that equality is derivable from `LE` (`a = b` iff `a ≤ b ∧ b ≤ a`), which is exactly how `lexicographicLE` decides element-equality without an `Equal`. Section 2 packages the trick as two top-level combinators: `eqFromLE le x y = le x y (le y x) false` (the conjunction, the second comparison consulted only when the first holds) and `ltFromLE le x y = not (le y x)` — the latter leaning on *totality*, so strict-less costs a single call. Section 0's `eqInt`/`ltInt` are their `leInt` instances, and Part II's `binarySearch`, `lexicographicalLess`, and `nextPermutation` consume them wholesale.
- **`Matrix a = List (List a)`.** A rectangular list-of-lists, rows as inner lists. Again alias-only; `transpose :: Matrix a -> Matrix a` is just a `List (List a) -> List (List a)` whose name and rectangularity precondition live in the synonym and comment. The `null @(List a)` type application inside `transpose` is needed precisely because, at the type level, `Matrix a` has dissolved back into `List (List a)` and GHC must be told which layer an eliminator targets.

The throughline: in this module, *types are documentation as much as machinery*. The five real encodings carry the computational content; the aliases carry the invariants the encodings are too coarse to express. Keeping that distinction in mind is the key to reading the rest of the file — whenever you see `Tuple`, `Dict`, or `Matrix`, mentally substitute `List`/`List (… `Pair` …)`/`List (List …)` and the code stops looking magical.

### A first worked reduction: `Bool` as `if`

To make the "data is its eliminator" slogan concrete, here is `and true false` unfolding by pure β-reduction, using `true = const`, `false = const id`, and `and b₁ b₂ = b₁ b₂ false`:

```text
and true false
  = true false false                 -- unfold `and b₁ b₂ = b₁ b₂ false`, b₁=true, b₂=false
  = const false false                -- unfold true = const
  = (\a _ -> a) false false          -- const
  = false                            -- keep first arg
```

`and true true` instead lands on `true true false = const true true = true`, and `and false _` is `false _ false = const id _ false = id false = false`. The truth table falls straight out of which projection each Bool is. This same "apply the value to its handlers" move is the entire operational story of the module; every later chapter is just richer handlers fed to richer eliminators.

---

## 2. Church Booleans and Pairs

A Church `Bool` is not a value you inspect — it *is* the conditional. The type

```haskell
type Bool = ∀e. e -> e -> e
```

says: give me two branches of any single type `e`, and I will pick one. By convention `true` returns its first argument, `false` its second, so the elimination form `b thenBranch elseBranch` is literally `if b then thenBranch else else…` collapsed into one application. This is why the module's house rule "no if-then-else" costs nothing: every `Bool` produced here is *used directly* as an if-then-else. You will see `null xs z (…)` (packaged once and for all as the `caseList` eliminator of §3), `leInt n 0 nil (…)`, `p x (…) (…)` everywhere — the boolean steers control flow by being applied to the two outcomes.

The `∀e.` living *inside* the synonym is the crux. Because of `ImpredicativeTypes`, a single `Bool` value can be eliminated at `e = List a` in one place and `e = Maybe b` in another. Without rank-N quantification the consumer could not choose the result type, and the boolean would degenerate into a monomorphic selector.

### `true` and `false`

```haskell
true ∷ Bool
true = const

false ∷ Bool
false = const id
```

`true = const` because `const t f = t` selects the first branch. For `false` we need a two-argument function returning the second: `false t f = f`. Point-free, that is `\t -> id`, i.e. `\t f -> f`, which is exactly `const id` (`const id t = id`, and `id f = f`). Both unify against `∀e. e -> e -> e`: `const :: a -> b -> a` specializes at `a ~ b ~ e`, and `const id :: b -> (e -> e)` specializes the codomain `e -> e` back to the shared `e`. The test suite pins the meaning through the round-trip helpers `toPreludeBool b = b True False` and `fromPreludeBool b = if b then true else false` (test/Spec.hs:133-137): `null nil` must yield `True` and `null xs` must yield `False` (test/Spec.hs:212-213).

### `not`

```haskell
not ∷ Bool -> Bool
not b = b false true
```

Eliminate `b` with the branches *swapped*: if `b` is `true` it selects its first argument `false`; if `b` is `false` it selects `true`. No new boolean is constructed — `not` is just `b` viewed through a flipped pair of branches.

```text
not true  = true false true   = (const) false true   = false
not false = false false true  = (const id) false true = id true = true
```

### `and`

```haskell
and ∷ Bool -> Bool -> Bool
and b = flip b false
```

Read as `if b₁ then b₂ else false`: the point-free `flip b false` slots the incoming `b₂` *between* `b` and `false`, so η-expanded the definition is `and b₁ b₂ = b₁ b₂ false` — the unfolding the traces here (and in "The one idea") use. Short-circuiting falls out for free: when `b₁` is `false`, `b₂` is never forced as a branch selector.

```text
and true false
  = true false false          -- b₁ = true selects first branch
  = const false false
  = false                     ✓  (and True False = False)
```

A second trace confirms the truth table at the identity element:

```text
and true true
  = true true false
  = const true false
  = true                      ✓
```

### `or`

```haskell
or ∷ Bool -> Bool -> Bool
or b₁ = b₁ true
```

Curried to the bone: `or b₁ b₂ = b₁ true b₂`, i.e. `if b₁ then true else b₂`. Dropping `b₂` from both sides leaves `or b₁ = b₁ true`, a partial application that *is* the two-branch eliminator waiting for its else-branch.

```text
or false true
  = false true true           -- b₁ = false
  = (const id) true true
  = id true
  = true                      ✓  (or False True = True)
```

### `xor`

```haskell
xor ∷ Bool -> Bool -> Bool
xor b = b not id
```

`if b₁ then (not b₂) else b₂`: when `b₁` holds we flip `b₂`, otherwise we pass it through. This is the cleanest two-branch reading of exclusive-or.

```text
xor true true
  = true (not true) true
  = const (not true) true
  = not true
  = true false true
  = const false true
  = false                     ✓  (True `xor` True = False)
```

### 4. Church-Encoded Pairs

The pair type

```haskell
type Pair a b = ∀e. (a -> b -> e) -> e
```

encodes `(a, b)` as a function that, given a two-argument continuation `k :: a -> b -> e`, hands it the two components and returns whatever `k` returns. A pair *is* a `uncurry`-ready value: to read it you supply a binary function and it applies that function to the contents. Construction and elimination are inverse: `pair x y` packages `x, y`; applying it to `k` runs `k x y`. As with `Bool`, the inner `∀e.` (under `ImpredicativeTypes`) lets the *same* pair be projected at different result types — `fst` instantiates `e ~ a`, `snd` instantiates `e ~ b`, `pairToList` instantiates `e ~ List (Either a b)`.

### `pair`

```haskell
pair ∷ a -> b -> a `Pair` b
pair x y f = f x y
```

The constructor. `pair x y` is the closure `\f -> f x y`; the eta-expanded third argument `f` is the consumer's continuation. The infix backtick form `a \`Pair\` b` is just `Pair a b` written to read like a product. The test suite exercises it via `toPreludeTuple t = t (,)` (test/Spec.hs:130-131) — feeding the real tuple constructor as the continuation reifies a Church pair back into `(a, b)`.

### `fst` and `snd`

```haskell
fst ∷ a `Pair` _ -> a
fst = uncurry const

snd ∷ _ `Pair` b -> b
snd = uncurry $ const id
```

Here `Bool` and `Pair` share their wiring beautifully: the projections feed the *boolean selectors* as continuations, with `uncurry` (below) doing the feeding — `uncurry k p = p k`, so `fst = uncurry const` applies the pair to `const = true`, which keeps the first component, and `snd = uncurry (const id)` applies it to `const id = false`, which keeps the second. The `_` in the signatures is a `PartialTypeSignatures` wildcard — the discarded component's type is irrelevant.

```text
fst (pair x y)
  = (pair x y) const
  = const x y                 -- pair x y k = k x y, with k = const
  = x                         ✓  (fst (pair 10 20) = 10, test/Spec.hs:205)
```

```text
snd (pair x y)
  = (pair x y) (const id)
  = (const id) x y
  = id y
  = y                         ✓  (snd (pair 10 20) = 20, test/Spec.hs:206)
```

### `swap`

```haskell
swap ∷ a `Pair` b -> b `Pair` a
swap = uncurry $ flip pair
```

Eliminate `p` with a continuation that re-packages the components in the opposite order — and that continuation is literally `flip pair`, the pair constructor with its two arguments exchanged. Note there is no field access in between — `swap` never "extracts then rebuilds"; it threads a single continuation that constructs the swapped pair from inside `p`'s application. Where `fst`/`snd` pass a *projection* (`const`/`const id`) that keeps one component, `swap` passes a flipped *constructor* that keeps both and permutes them.

```text
swap (pair x y)
  = (pair x y) (flip pair)
  = flip pair x y             -- pair x y k = k x y, with k = flip pair
  = y `pair` x                ✓  (flip f x y = f y x; swap (pair "hello" 42) = (42,"hello"), test/Spec.hs:355)
```

### `curry` and `uncurry`

```haskell
curry ∷ (a `Pair` b -> c) -> a -> b -> c
curry f x = f . pair x

uncurry ∷ _ -> a `Pair` b -> c
uncurry f p = p f
```

These are the classic isomorphism, transcribed for Church pairs. `curry` takes a function on packaged pairs and lets you call it on loose arguments by constructing the pair first. `uncurry` is even more transparent: a Church pair `p` is *already* "a thing that wants a binary function," so `uncurry f p = p f` simply hands `f` to the pair as its continuation. In other words, `uncurry` is the eliminator made explicit, and `p f ≡ f (fst p) (snd p)` by construction.

```text
uncurry f (pair x y)
  = (pair x y) f
  = f x y                     -- the defining property of a Church pair
```

### `pairToList`

```haskell
pairToList ∷ a `Pair` b -> List (Either a b)
pairToList = uncurry $ \x y -> left x `cons` singleton (right y)
```

Project the pair into a heterogeneous two-element list by tagging the first component `Left` and the second `Right` (the `Either` and `List` encodings are covered in their own sections; `left`/`right`/`cons`/`singleton` are their constructors). The continuation builds `[Left x, Right y]` as a Church list. This is the canonical way to make a `Pair a b` "homogeneous" so list machinery can traverse it — the result type instantiates the pair's `∀e.` at `List (Either a b)`.

```text
pairToList (pair x y)
  = (pair x y) (\x' y' -> left x' `cons` singleton (right y'))
  = left x `cons` singleton (right y)
  ≅ [Left x, Right y]
```

### `bimapPair`

```haskell
bimapPair :: (a -> c) -> _ -> a `Pair` b -> c `Pair` d
bimapPair f g = uncurry $ \x y -> f x `pair` g y
```

The `Bifunctor` `bimap` for Church pairs: eliminate `p`, apply `f` to the first component and `g` to the second, and re-pair the results. One continuation does map-both-and-rebuild in a single pass, with no intermediate extraction. The signature carries no explicit `forall` — the compile-checked sweep that stripped every unneeded annotation confirmed inference threads all four parameters through the rank-N pair argument unaided.

```text
bimapPair f g (pair x y)
  = (pair x y) (\x' y' -> f x' `pair` g y')
  = f x `pair` g y
```

### Why this all type-checks at rank-N

Every function above is *given* a Church value and *applies* it to a continuation. The polymorphism flows the right way: the caller of `pair` produces a `∀e.`-quantified value; each consumer (`fst`, `swap`, `pairToList`, …) instantiates that `∀e.` at exactly the result type it wants. Because the quantifier sits inside the `Pair`/`Bool` synonym rather than at the binding's top level, `ImpredicativeTypes` is doing real work — it permits instantiating these universally-quantified synonyms at other (possibly polymorphic) types and storing them in argument position. The pervasive use of the same eliminator at many different `e` is precisely what a hand-rolled Böhm–Berarducci encoding buys you.

---

## 3. List constructors and sublist access

Recall the central type synonym for this section:

```haskell
type List a = ∀e. (a -> e -> e) -> e -> e
```

A `List a` *is* its own right fold: it is a function that, given a cons-replacement `c :: a -> e -> e` and a nil-replacement `n :: e`, produces the `e` you get by replacing every `(:)` with `c` and the final `[]` with `n`. Because `e` is universally quantified *inside* the synonym (this is where `ImpredicativeTypes` earns its keep), the same list value can be folded into any result type, instantiating `e` differently at each call site. Every operation below is ultimately phrased as "choose what `c` and `n` to fold with."

A small but pervasive house rule: this module forbids ADTs (no `Bool`, `(,)`, `[]`), forbids `if`-`then`-`else`, and allows only `Int`, `undefined`, and `error` as escape hatches. So branching is always done by *applying* a Church `Bool`/`Maybe`/`List` to its alternatives, never by syntactic conditionals. Watch for this in `null`, `caseList`/`uncons`, and `any`/`all`.

### nil

```haskell
nil ∷ List a
nil = const id
```

`nil` is the empty fold: `nil c n` must ignore `c` and return `n`. Indeed `const id c n = id n = n`. Note the pointfree spelling `const id` is *exactly* the Church `false` (`false = const id`) — at the level of untyped terms, the empty list and the boolean `false` are the same λ-term `λx y. y`; only their intended types differ. This is the K-combinator applied to I: `const id = λc. id`, and then `id n = n`.

### cons

```haskell
cons ∷ a -> List a -> List a
cons x xs c = c x . xs c
```

This is the cleverest of the constructors and the one to internalize, because the whole module's fold-based style flows from it. Read it η-expanded:

```haskell
cons x xs c n = c x (xs c n)
```

Given the fold operations `c` and base `n`, the list `x : xs` folds by first folding the tail `xs c n :: e`, then combining the head with `c x (...)`. That is precisely the right-fold equation `foldr c n (x:xs) = c x (foldr c n xs)`. The pointfree `c x . xs c` reads as: "`xs c` is the partially-applied tail-fold of type `e -> e`; precompose it with `c x` (also `e -> e`)." So `cons x xs c` is itself a function `e -> e` awaiting the base `n`. The head is *injected into the fold*, not stored — there is nowhere to store it; the data is the fold.

It type-checks at rank-N because `c` is the very `(a -> e -> e)` that the result `List a` demands; `x :: a`, `xs c :: e -> e`, `c x :: e -> e`, composition closes the loop.

### snoc

```haskell
snoc ∷ List a -> a -> List a
snoc xs x c = xs c . c x
```

The mirror image of `cons`: append `x` at the end. η-expanded, `snoc xs x c n = xs c (c x n)`. The new element is folded into the base *first* (`c x n`), then the existing list folds on top. Note the perfect symmetry with `cons x xs c = c x . xs c`: `cons` puts `c x` on the *left* of the composition (head first), `snoc` puts it on the *right* (last element deepest in the fold). `snoc` is exercised by `listOpTests`: `snoc [1..5] 6` yields `[1,2,3,4,5,6]`.

### singleton

```haskell
singleton ∷ a -> List a
singleton x = ($ x)
```

The one-element list `[x]` should fold to `c x n`. Here `singleton x = ($ x)`, i.e. `singleton x c = c $ x = c x`, so `singleton x c n = c x n`. Tersely, `[x]` is "apply the cons-replacement to `x`." Equivalent to `cons x nil` but with the `nil`-fold optimized away. `listOpTests` checks `singleton 7 == [7]`.

### caseList — one-step case analysis, the workhorse eliminator

```haskell
caseList ∷ b -> (a -> List a -> b) -> List a -> b
caseList z f xs = null xs z $ f (head xs) $ tail xs
```

`caseList` is to lists what `Prelude.maybe` is to `Maybe`: a *one-step* case analysis. `caseList z f xs` is `z` when `xs` is `nil`, and `f (head xs) (tail xs)` otherwise. The handlers come first and the scrutinee last, so partial applications like `caseList nil (\x xs' -> …)` are themselves ready-made `List a -> b` functions — which is why so many definitions below are point-free in their list argument. There is still no pattern match: `null xs :: Bool` is *applied directly as the two-way branch*, selecting `z` (because `null` of an empty list returns `true = const`, which keeps its first argument) or the applied handler.

The subtlety is that `f (head xs) (tail xs)` is built *unconditionally* — but `head`'s `undefined` and `tail`'s one-step-delay machinery live inside thunks that only the non-empty branch ever forces. When `xs` is `nil`, the Church `Bool` returned by `null` discards its entire second argument unevaluated, so the empty case never touches `head`/`tail`. Laziness is what makes the definition safe; in a strict language `caseList` would have to delay the handler application explicitly.

One definition, many payoffs: `caseList` packages the module's former call-site idiom `uncons xs z (\p -> p $ \x xs' -> …)` — eliminate the `Maybe`, then eliminate the `Pair` — into a single combinator, and it now fronts most of the non-fold list functions here (`uncons` itself, `foldl1`, `scanl`, `zipWith`, `dropWhile`, `lexicographicLE`, `rotateLeft`, `at`, plus a dozen more in the algorithm sections of Part II). Wherever you previously read a nested `uncons`-and-destructure, read `caseList z f xs` instead.

### uncons

```haskell
uncons ∷ List a -> Maybe (a `Pair` List a)
uncons = caseList nothing (curry just)
```

`uncons` deconstructs a list into `Maybe (head, tail)` — with `caseList` in hand, a one-liner. The empty handler is `nothing`; the non-empty handler must send `x` and `xs'` to `just (x `pair` xs')`, and that is spelled point-free as `curry just`: since `curry f x = f . pair x`, we have `curry just x xs' = just (x `pair` xs')` — equivalently `(just .) . pair`, the `just` injection post-composed onto the two-argument `pair` constructor. The `Maybe` it returns is itself Church-encoded (`type Maybe a = ∀e. e -> (a -> e) -> e`), so a downstream consumer eliminates it by supplying a nothing-handler and a just-handler — exactly what the tests do.

The empty case is safe for the reason spelled out under `caseList`: the pair of `head`/`tail` thunks is built unconditionally inside the handler, but the `nothing` branch discards it unevaluated, so `head`'s `undefined` is never forced.

`unconsTests` pins this down: `uncons [1,2,3,4,5]` handled with `\p -> p (\x ys -> show (x, toList ys))` gives `"(1,[2,3,4,5])"`, and `uncons nil` gives `"nothing"`.

### unsnoc

```haskell
unsnoc ∷ List a -> Maybe (List a `Pair` a)
unsnoc xs =
  null xs
    nothing
    $ just $ init xs `pair` last xs
```

The dual of `uncons`: split off the *last* element, yielding `Maybe (init, last)`. Same `null xs`-as-branch idiom — written out raw rather than via `caseList`, because the pieces it pairs are `init xs`/`last xs`, not the head/tail that `caseList` hands its handler. It leans on `init` and `last` from §5.2 rather than re-deriving the split.

---

### 5.2 Sublist Access

### head

```haskell
head ∷ List a -> a
head = foldr const undefined
```

Fold the list with `c = const` and `n = undefined`. On a cons cell, `cons x rest` folds to `const x (rest-folded) = x` — `const` keeps the head and discards the entire folded tail, so the recursion on the tail is never forced. On `nil`, the fold returns the base `undefined`, so `head nil` throws (the house rules permit `undefined` precisely for these partial cases). The choice `c = const` is the whole trick: the right fold visits the head first and `const` short-circuits there.

#### Worked trace: `head (cons 1 (cons 2 nil))`

```text
head (cons 1 (cons 2 nil))
  = (cons 1 (cons 2 nil)) const undefined          -- def head, c=const n=undefined
  -- cons x xs c = c x . xs c, so cons x xs c n = c x (xs c n)
  = const 1 ((cons 2 nil) const undefined)         -- unfold outer cons at c=const
  = 1                                              -- const 1 _ = 1; tail fold discarded
```

The inner `(cons 2 nil) const undefined` is never evaluated; `const` drops it. Result `1`.

### null

```haskell
null ∷ List a -> Bool
null = foldr (const $ const false) true
```

Fold with `c = \_ _ -> false` and `n = true`. An empty list folds straight to the base `true`; any cons cell folds its head/tail through `\_ _ -> false`, immediately yielding `false` and discarding both arguments (so, again, the tail fold is never forced). The result is a genuine Church `Bool = ∀e. e -> e -> e`, ready to be used directly as a branch selector — which is exactly how `caseList` and `unsnoc` consume it.

#### Worked trace: `null nil`

```text
null nil
  = nil (\_ _ -> false) true        -- def null
  -- nil = const id, so nil c n = n
  = true
```

#### Worked trace: `null (cons 1 nil)`

```text
null (cons 1 nil)
  = (cons 1 nil) (\_ _ -> false) true                 -- def null, c=(\_ _ -> false) n=true
  -- cons x xs c n = c x (xs c n)
  = (\_ _ -> false) 1 (nil (\_ _ -> false) true)      -- unfold cons
  = false                                             -- (\_ _ -> false) ignores both args
```

`listTests` confirms both: `null [1,2,3]` is `False` and `null nil` is `True` (via `toPreludeBool`).

### tail

```haskell
tail ∷ List a -> List a
tail xs cons' nil' =
  xs
    (\x r skip -> skip (r false) (x `cons'` r false))
    (const nil')
    true
```

This is the subtle one. A right fold visits elements head-first, but `tail` must *drop* the head — a decision that depends on whether we are at the outermost element. The encoding solves this with a **one-step-delay via a Church `Bool` flag**: instead of folding to a `List b` directly, it folds to a function `Bool -> e` (call the flag `skip`), and only at the very end applies it to `true`.

Read the pieces. The fold's combining step is `\x r skip -> skip (r false) (x \`cons'\` r false)`, where:
- `x` is the current element, `r :: Bool -> e` is the folded *rest* (also awaiting a flag),
- `r false` is "the rest, built normally" (flag `false` means *don't* skip),
- `skip` is the incoming flag deciding what *this* position does.

When `skip = true` (only the outermost element gets this, from the final `... true`), `true (r false) (x \`cons'\` r false)` selects the *first* branch `r false` — i.e. emit the tail *without* `x`. When `skip = false` (every inner position, fed by the `r false` sub-calls), it selects the *second* branch `x \`cons'\` r false` — i.e. keep `x`. The base case `const nil'` ignores whatever flag it receives and yields `nil'`, so `tail nil = nil`.

It type-checks at rank-N because the fold's result type `e` is instantiated to `Bool -> e₀` (a function type) — legal precisely because `List`'s `∀e` is impredicative; you are folding into the polytype `Bool -> e₀`.

#### Worked trace: `tail [1,2]` (writing `[1,2] = cons 1 (cons 2 nil)`)

Let `f = \x r skip -> skip (r false) (x \`cons'\` r false)` and base `b = const nil'`. The fold of `[1,2]` is `f 1 (f 2 b)`.

```text
-- innermost: r2 = f 2 b
r2 = \skip -> skip (b false) (2 `cons'` b false)
   = \skip -> skip nil' (2 `cons'` nil')           -- b false = const nil' false = nil'

-- r1 = f 1 r2
r1 = \skip -> skip (r2 false) (1 `cons'` r2 false)
-- r2 false = false nil' (2 `cons'` nil') = (2 `cons'` nil')   -- false = const id picks 2nd arg
r1 = \skip -> skip (2 `cons'` nil') (1 `cons'` (2 `cons'` nil'))

-- finally apply the outermost flag true:
tail [1,2] cons' nil' = r1 true
   = true (2 `cons'` nil') (1 `cons'` (2 `cons'` nil'))
   = (2 `cons'` nil')                              -- true = const picks 1st arg
   = [2]
```

The outermost `true` discards the head `1` and keeps the already-built tail; every inner `r false` kept its element. `listFunctionTests` checks `tail [1..5] == [2,3,4,5]`.

### last

```haskell
last ∷ List a -> a
last = foldl (flip const) undefined
```

Left-fold keeping always the newest element: `\_ x -> x` discards the accumulator and returns the current element, so after consuming the whole list the accumulator holds the final element. The seed `undefined` is returned only for the empty list (so `last nil` throws). This relies on `foldl` from §5.3 (whose own "build a function then apply" CPS trick is documented there). `listFunctionTests`: `last [1..5] == 5`.

### init

```haskell
init ∷ List a -> List a
init = liftA2 (zipWith const) id tail
```

`init` (all elements but the last) is the classic one-liner: zip the list against its own tail and keep the left component of every pairing. (The `liftA2 (zipWith const) id tail` spelling is the *function applicative* at work — `liftA2 g u v xs = g (u xs) (v xs)` — so this is exactly `zipWith const xs (tail xs)` with `xs` distributed to both slots; the same reader-`liftA2` trick recurs in `unzip`, and in doubled `liftA2 (liftA2 pair)` form in `splitAt` and `span` below.) The insight is that `zipWith` (§5.3) truncates at the *shorter* input, and `tail xs` is exactly one element shorter than `xs` — so the zip performs `length xs - 1` steps, with `const` keeping the element from `xs` and discarding the one from `tail xs`, whose only job was to be the right length. The last element of `xs` never finds a partner and is silently dropped.

```text
init [1,2,3]
  = zipWith const [1,2,3] (tail [1,2,3])
  = zipWith const [1,2,3] [2,3]
  = const 1 2 : const 2 3 : []       -- zipWith stops when [2,3] runs dry
  = [1,2]
```

Laziness makes this cheaper than it looks: `const` never forces the element supplied by the tail, so `tail xs` is consumed only as a *length gauge* — its spine is walked, its elements untouched. `init nil` is safe by the same truncation: `tail nil = nil`, and `zipWith`'s first `caseList` hits its empty branch, so `init nil = nil` (where the Prelude's `init` would throw). `listFunctionTests`: `init [1..5] == [1,2,3,4]`.

### any

```haskell
any :: (a -> Bool) -> List a -> Bool
any p = foldr (or . p) false
```

Existential quantification via right fold, with `c = \x acc -> p x true acc` and `n = false`. Crucially `p x :: Bool`, used itself as the branch: `p x true acc` means "if `p x` holds, short-circuit to `true`; otherwise fall through to `acc`" (the folded rest). Note `acc` is the *second* argument to the Church `Bool` `p x`, so when `p x = true = const`, `true true acc = true` regardless of the rest — a genuine early exit (no further folding forced). When `p x = false`, `false true acc = acc`, continuing. Base `false` for the empty list. `any` is a workhorse for the N-ary parallel combinators (`zipWithN`, `scanlN`, `mapAccumLN`) where `any null lists` is the termination test.

### all

```haskell
all :: (a -> Bool) -> List a -> Bool
all p = foldr (and . p) true
```

Universal quantification, dual to `any`: `c = \x acc -> p x acc false`, `n = true`. Here `p x acc false` means "if `p x` holds, continue with `acc` (the rest); otherwise short-circuit to `false`." The branch arms are swapped relative to `any` (`acc` in the true-slot, `false` in the false-slot). Empty list folds to `true` (vacuous truth). Again the Church `Bool` returned by `p` is consumed positionally as the conditional, never with `if`.

#### Worked trace: `any (eqInt 2) [1,2]`

```text
any (eqInt 2) [1,2]
  = foldr (\x -> eqInt 2 x true) false (cons 1 (cons 2 nil))
  -- foldr c n xs = xs c n; let c x acc = eqInt 2 x true acc
  = c 1 (c 2 false)
  = eqInt 2 1 true (c 2 false)        -- eqInt 2 1 = false
  = false true (c 2 false) = c 2 false
  = eqInt 2 2 true false              -- eqInt 2 2 = true
  = true true false = true
```

`true` short-circuits the moment a match is found; had the match been first, the rest would never have been folded at all.

---

## 4. Folds, maps, filters, and the everyday list toolkit

Section 5.3 is where the encoding earns its keep. Recall the single fact that drives
everything here: a Church list **is** its own right fold,

```haskell
type List a = ∀e. (a -> e -> e) -> e -> e
```

so `xs cons' nil'` literally *runs* `xs`, substituting `cons'` for every `cons` node
and `nil'` for the terminal `nil`. Most functions below are nothing but a clever choice
of what to put in those two slots. The genuinely surprising one is `foldl`, which builds
a *function* in the `e` slot and applies it at the very end — the CPS trick that the rest
of the section leans on.

Throughout, remember the house rules this file obeys (stated in the header, lines 58–64 of the module comment):
no `Bool`, `(,)`, or `[]` even in helpers; no `if`-`then`-`else`; only `Int` is borrowed
from the Prelude as a primitive; `undefined`/`error` are allowed because they introduce no
ADT. Several functions exploit the first rule to elegant effect — a Church `Bool` is
*already* an if-then-else, so `null xs thenBranch elseBranch` needs no conditional syntax.

### `foldr` — the eliminator, undisguised

```haskell
foldr ∷ (a -> b -> b) -> b -> List a -> b
foldr f z xs = xs f z
```

`foldr f z xs` is just `xs f z`: hand the list its two consumers and let it fold itself.
The instantiation `e := b` is what makes it type-check — the list's `∀e` is specialized to
the fold's result type. This is `flip ($)` morally, but at rank-N: `foldr` *is* the
identity on the encoding, modulo argument order. Every other right-folding function in the
file is `foldr` with a particular `(f, z)`.

```text
foldr (+) 0 [1,2,3]
  = [1,2,3] (+) 0                       -- [1,2,3] = λc n. c 1 (c 2 (c 3 n))
  = (+) 1 ((+) 2 ((+) 3 0))
  = 1 + (2 + (3 + 0))  = 6
```

Test suite (`listFunctionTests`, line 243): `foldr (+) 0 xs` equals `Prelude'.foldr (+) 0 [1..5] = 15`.

### `foldl` — a right fold that builds a continuation, applied at the end

```haskell
foldl ∷ _ -> b -> List a -> b
foldl f z xs = xs (flip (.) . flip f) id z
```

This is the cleverest line in the section, so it is worth slowing down. A left fold
associates the *other* way, `f (f (f z a) b) c`, which a pure right fold cannot produce by
naive substitution. The standard resolution — known from `foldl = foldr (flip . ...) id`
in idiomatic Haskell — is **higher-order**: instead of folding to a value, fold to a
*function* `b -> b` (a continuation that, given the accumulator so far, finishes the job),
then apply that function to the seed `z` at the very end.

Look at the slots. We instantiate the list at `e := (b -> b)`:

- The `nil`/terminal slot is `id :: b -> b` — the empty continuation, "whatever the
  accumulator is, that's the answer."
- The `cons` slot is `flip (.) . flip f` — a point-free spelling that unfolds to
  `\x g acc -> g (f acc x)`: `flip f x = \acc -> f acc x` is the one-step update, and
  `flip (.)` pre-composes it onto the tail's continuation (`(flip (.) . flip f) x g =
  g . flip f x`). Here `x` is the head element, `g :: b -> b` is the continuation built
  from the *tail*, and `acc` is the accumulator that will arrive from the left. It steps
  the accumulator once (`f acc x`) and threads the result into `g`.

Because the right fold associates the continuation-builders from the right, the resulting
function composes so that the *leftmost* element runs *first* when the accumulator is
finally injected. The trailing `z` is that injection.

Trace `foldl (-) 0 [1,2,3]`, with `f = (-)` and `k = \x g acc -> g (f acc x)`. Write the
encoded list as `λc n. c 1 (c 2 (c 3 n))`:

```text
foldl (-) 0 [1,2,3]
  = [1,2,3] k id 0                              -- e := (Int -> Int); slots k and id
  = k 1 (k 2 (k 3 id)) 0

innermost first:
  k 3 id   = \acc -> id (acc - 3)            = \acc -> acc - 3
  k 2 g3   = \acc -> g3 (acc - 2)            = \acc -> (acc - 2) - 3
  k 1 g2   = \acc -> g2 (acc - 1)            = \acc -> ((acc - 1) - 2) - 3

apply to z = 0:
  = ((0 - 1) - 2) - 3
  = ((-1) - 2) - 3
  = (-3) - 3
  = -6
```

Note how the parenthesization came out left-associated — `((0-1)-2)-3` — exactly what a
strict left fold gives, even though we only ever *substituted into a right fold*. The magic
is that each `k` does not combine values; it **defers**, wrapping the tail's continuation
`g` around its own one-step update. The composition `g3 ∘ (subtract 2) ∘ ...` only collapses
to a number once `z` arrives. This is continuation-passing in its purest form: build the
whole pipeline as a `b -> b`, then run it once.

Why it type-checks at rank-N: the list is `∀e. (a -> e -> e) -> e -> e`; we instantiate
`e := (b -> b)`, a polymorphic-free function type, so `cons'` has type
`a -> (b -> b) -> (b -> b)` and `nil' = id :: b -> b`. `ImpredicativeTypes` is not strictly
needed *here* (the instantiation is at a monotype `b -> b`), but it is what lets `List a`
remain a `∀`-bearing synonym that we can feed monomorphic arguments to without unwrapping.

Behavior: ordinary strict-shaped left fold. Test suite (line 244):
`foldl (+) 0 xs = Prelude'.foldl (+) 0 [1..5] = 15`; and the `foldTests` block exercises
the non-commutative `foldl1 (-)` (line 542) whose value, `1-2-3-4-5 = -13`, depends on
precisely the left-association this trick reproduces.

### `foldl1` and `foldr1` — seedless folds, two different tricks

```haskell
foldl1 :: _ -> List a -> a
foldl1 = caseList (error "foldl1: empty list") . foldl

foldr1 :: _ -> List a -> a
foldr1 f xs = foldr (\x m -> just (m x (f x))) nothing xs
  (error "foldr1: empty list") id
```

`foldl1` peels one element and delegates, via `caseList` (§3): read the composition right —
`caseList (error …) . foldl` applied to `f` is `caseList (error …) (foldl f)` — so the
empty-list handler is the `error` and the non-empty handler is `foldl f` itself:
`foldl1 f xs = foldl f (head xs) (tail xs)`, seed the left fold with the head, fold the tail.

`foldr1` cannot peel from the left (its seed is the *last* element), so it plays a different
trick: fold the whole list into a `Maybe a` — `nothing` for empty, `just result` otherwise —
then eliminate that with `(error "foldr1: empty list")` and `id`. The step
`\x m -> just (m x (f x))` is the pretty part: `m` is the folded tail, a Church `Maybe`,
and `m x (f x)` eliminates it *with the current head as the nothing-branch* — if the tail
was empty, `x` is the last element and is the answer outright; otherwise the just-branch
`f x` combines `x` with the tail's result. Every step re-wraps in `just`, so only the empty
input ever reaches the outer `error`. No conditional syntax, no ADT; the `Maybe` *is* the
branch selector.

```text
foldr1 (-) [1,2,3,4,5]
  = 1 - (2 - (3 - (4 - 5)))
  = 1 - (2 - (3 - (-1)))
  = 1 - (2 - 4) = 1 - (-2) = 3
```

Test suite (`foldTests`, lines 536–547): `foldl1 (+)`=15, `foldr1 (+)`=15, `foldl1 (-)`=-13,
`foldr1 (-)`=3.

### `scanr` — fold that keeps every partial result

```haskell
scanr ∷ (a -> b -> b) -> b -> List a -> List b
scanr f q = foldr (\x p -> f x (head p) `cons` p) $ singleton q
```

`scanr f z` is `foldr step (singleton z)`: seed the accumulator with `[z]`, and at each step
prepend `f x (head p)`, where `head p` is the most recent partial sum. Because it is a right
fold, `head p` is always the running result for the suffix — so each new element extends the
result list by one on the left, giving the standard "one longer than input" scan.

```text
scanr (+) 0 [1,2,3]
  step 3 [0]       = (3+0):[0]       = [3,0]
  step 2 [3,0]     = (2+3):[3,0]     = [5,3,0]
  step 1 [5,3,0]   = (1+5):[5,3,0]   = [6,5,3,0]
```

Test (line 315): `scanr (+) 0 [1..5] = [15,14,12,9,5,0]`.

### `scanl` — left scan as a self-seeding recursive producer

```haskell
scanl ∷ _ -> b -> List a -> List b
scanl f q = cons q . caseList nil (scanl f . f q)
```

`scanl` cannot reuse the pure-`foldl` continuation trick directly, because it must *emit*
every intermediate accumulator, not just the final one. So it is written as an explicit
producer around `caseList` (§3). The first act is always to emit the current accumulator —
the `cons q .` prefix puts `q` in front of whatever follows; then `caseList` inspects the
input: empty means `nil` (the seed just emitted was the last output), otherwise the
recursion is simply `scanl` *itself* with the stepped accumulator — the handler
`\x xs' -> scanl f (f q x) xs'` eta-reduces to the point-free `scanl f . f q`. There is no
helper: the "emit the seed, then step" invariant is maintained by every recursive call
re-entering through the same `cons q .` front door, so the stepped accumulator of one call
is emitted as the leading seed of the next.

```text
scanl (+) 0 [1,2]
  = 0 : scanl (+) 1 [2]         -- caseList on [1,2]: x:=1 ; f 0 1 = 1
  = 0 : 1 : scanl (+) 3 []      -- caseList on [2]:   x:=2 ; f 1 2 = 3
  = 0 : 1 : 3 : []              -- caseList on [] → nil, after the last seed
  = [0,1,3]
```

Test (line 312): `scanl (+) 0 [1..5] = [0,1,3,6,10,15]`. `inits = scanl snoc nil`
reuses this directly: scanning `snoc` from `nil` accumulates ever-longer prefixes.

### `map` — post-compose into the `cons` slot

```haskell
map ∷ _ -> List a -> List b
map f xs c = xs $ c . f
```

A jewel of brevity. The output list, given its own consumer `c :: b -> e -> e`, runs the
input list with `c . f` in the cons slot — i.e. each element `x` is mapped to `f x` *just
before* it would have been consed. The `nil` slot is left implicit (eta): `xs (c . f)` still
expects the input's `nil`, which becomes the output's `nil`. No traversal is written; `map`
is a one-line rewriting of the fold's algebra.

```text
map (*2) [1,2] c
  = [1,2] (c . (*2))                 -- [1,2] = λc' n. c' 1 (c' 2 n)
  = (c . (*2)) 1 ((c . (*2)) 2 n)
  = c 2 (c 4 n)                       -- the encoded list [2,4]
```

Test (line 318): `map (*2) [1..5] = [2,4,6,8,10]`.

### `filter` — conditionally skip in the `cons` slot

```haskell
filter ∷ (a -> Bool) -> List a -> List a
filter p = concatMap $ \x -> p x (singleton x) nil
```

Same predicate-steering idea, routed through `concatMap`: each element is sent to a
zero-or-one-element list — the Church-`Bool` `p x`, a boolean `∀e. e -> e -> e`, is applied
to `singleton x` (keep) and `nil` (drop) and selects which — and the concatenation splices
the survivors together, with no conditional syntax anywhere. Because `concatMap` fuses
definitionally (see `concat`/`concatMap` below), the composite behaves exactly like a fold whose cons slot
consults `p`: applied to a consumer `c`, each kept element contributes `c x acc` and each
dropped one passes `acc` through — the reading the trace below uses.

```text
filter even [1,2] c
  = [1,2] (\x acc -> even x (c x acc) acc)
  = (\x acc-> even 1 (c 1 acc) acc) 1 ((\x acc-> even 2 (c 2 acc) acc) 2 n)
  = even 1 (c 1 (even 2 (c 2 n) n)) (even 2 (c 2 n) n)
  -- even 1 = false ⇒ take right branch; even 2 = true ⇒ take left
  = even 2 (c 2 n) n
  = c 2 n                              -- the encoded list [2]
```

Test (line 321): `filter even [1..5] = [2,4]`.

### `append` and `appendEither` — replay one list onto the other

```haskell
append ∷ List a -> List a -> List a
append xs = xs cons
```

The tersest definition in the file: fold `xs` with the *genuine* `cons` as the step — and
the base slot, by partial application, is simply the second list. `append xs ys = xs cons ys`
replays every element of `xs`, re-`cons`ing each onto the front of `ys`; when the replay
reaches `xs`'s terminal `nil`, the base `ys` is already sitting there. This is
`foldr cons ys xs`, the textbook append, with the fold written as bare application.

```text
append [1] [2]
  = [1] cons [2]                       -- fold [1] with step cons, base [2]
  = cons 1 (nil cons [2])              -- [1] = cons 1 nil replays its spine
  = cons 1 [2]                         -- encoded [1,2]
```

`appendEither` tags before appending: ``map left xs `append` map right ys`` wraps each
side's elements with the Church-`Either` constructors, then appends the two homogeneous
halves. Type `List a -> List b -> List (Either a b)`.

Test (line 245): `append [1..5] [6..10] = [1..10]`.

### `concat`, `concatMap`

```haskell
concat ∷ List (List a) -> List a
concat = foldr append nil

concatMap ∷ (a -> List b) -> List a -> List b
concatMap f = concat . map f
```

`concat` is `foldr append nil` — the obvious monoidal fold. `concatMap` is literally
`concat . map f`, but because every one of these lists *is* its own fold, the composition
β-reduces to the fused form `concatMap f xs c = xs (\x -> f x c)`: the output, given
consumer `c :: b -> e -> e`, folds the input with `\x -> f x c`. Read that slot's type: it
must be `a -> e -> e`; `f x :: List b = ∀e. (b -> e -> e) -> e -> e`, so `f x c :: e -> e`
is exactly an `a`-step that splices the entire sublist `f x` (already consumed by `c`) in
front of the running `e`. No intermediate list of lists is ever materialized — the inner
lists are inlined straight into the output fold. (`unfoldTree` uses
`concatMap (unfoldTree f) seeds` to flatten its branches, a nice reuse.)

```text
concatMap (\x -> [x,x*2]) [1,2] c
  = [1,2] (\x -> f x c)              where f x = [x, x*2]
  = (f 1 c) ((f 2 c) n)
  = c 1 (c 2 ( c 2 (c 4 n)))         -- encoded [1,2,2,4]
```

Test (line 246): `concatMap (\x -> [x,x*2]) [1..5] = [1,2,2,4,3,6,4,8,5,10]`; `concat` test
at line 555 gives `[1..7]`.

### `length` and `reverse` — counting and accumulating

```haskell
length ∷ List a -> Int
length = foldr (const succ) 0

reverse ∷ List a -> List a
reverse = foldl (flip cons) nil
```

`length` is a right fold whose step ignores the element and increments: `const succ` is
`\_ acc -> acc + 1`, so every cons node contributes one `succ` on top of the base `0` —
`Int` is the one permitted Prelude type, so `succ` is legal. `reverse` is the textbook
`foldl (flip cons) nil`, built from the *genuine* constructors: fold the input left-to-right,
prepending each element onto the accumulated list. Because a left fold processes elements
left-to-right while `flip cons` prepends, the first input element ends up deepest, i.e.
last — reversal. This is `foldl`-the-CPS-trick doing real work: the continuation built by
`foldl` is what re-associates the conses into reverse order.

```text
reverse [1,2,3]
  = foldl (flip cons) nil [1,2,3]
  = (flip cons) ((flip cons) ((flip cons) nil 1) 2) 3   -- left-assoc from foldl
  = cons 3 (cons 2 (cons 1 nil))                        -- encoded [3,2,1]
```

Tests: `length [1..5]=5` (line 324); `reverse [1..5]=[5,4,3,2,1]` (`reverseTests`, line 350).

### `takeWhile`, `dropWhile`

```haskell
takeWhile ∷ (a -> Bool) -> List a -> List a
takeWhile p = foldr (\x acc -> p x (x `cons` acc) nil) nil
```

A plain `foldr` — no encoding arguments, no η-expansion: the fold builds a genuine Church
list out of the real `cons` and `nil`. At each element, the Church-`Bool` `p x` chooses
between `x `cons` acc` (keep and continue) and `nil` (stop — discard the entire rest, since
`acc` is abandoned). Because the fold is right-associated, the moment one element fails,
its `nil` truncates everything to its right that was already built into `acc`. Clean and
conditional-free.

```text
takeWhile (<3) [1,2,3,4]
  -- innermost acc for 4: p 4 (…) nil = nil ; for 3: p 3 (…) nil = nil
  -- 2: p 2 (2 `cons` nil) nil = 2 `cons` nil   (since 2<3)
  -- 1: p 1 (1 `cons` (2 `cons` nil)) nil = 1 `cons` (2 `cons` nil)
  = 1 `cons` (2 `cons` nil)      -- encoded [1,2]
```

`dropWhile` cannot be a plain fold — once the predicate fails it must emit the *entire
remaining tail unchanged*, which a right fold's accumulator has already collapsed. So it
recurses via `caseList`: on `nil` return `nil`; while `p x` holds, recurse on the tail; the
first time it fails, return the scrutinee `xs` itself — the handler closes over the whole
list, so nothing needs re-`cons`ing. Type `(a -> Bool) -> List a -> List a`.
Tests: `takeWhile (<4) [1..5]=[1,2,3]` (line 252); `dropWhile (<3) [1..5]=[3,4,5]` (line 327).

### `at`, `take`, `drop`, `splitAt`, `takeLast`, `dropLast`

The Wolfram-inspired slicing additions are the finite-list counterparts of `Part`,
`Take`, and `Drop`, with Haskell indexing conventions where the name is Haskell-like:

```haskell
at :: Int -> List a -> a
at n =
  ltInt n 0
    (error "at: negative index")
    (caseList (error "at: index out of bounds") const . drop n)
take :: Int -> List a -> List a
take n xs = zipWith const xs $ replicate n undefined
drop :: Int -> List a -> List a
drop n = compose $ replicate n tail
splitAt :: Int -> List a -> List a `Pair` List a
splitAt = liftA2 (liftA2 pair) take drop
takeLast :: Int -> List a -> List a
takeLast n = reverse . take n . reverse
dropLast :: Int -> List a -> List a
dropLast n = reverse . drop n . reverse
```

`take` reuses `init`'s length-gauge trick against a synthetic ruler: zip the list with
`replicate n undefined` and keep the left of each pairing — `zipWith` truncates at the
shorter input, so at most `n` elements survive, and the `undefined`s are never forced
(`const` discards them). `drop` composes `n` copies of `tail`, `compose (replicate n tail)`.
Non-positive counts behave like their Prelude analogues because `replicate` then yields
`nil`: `take` returns `nil`, `drop` the whole list. `splitAt` is just the Church pair of
those two traversals, spelled with the same doubled reader-`liftA2` as `span` below.
`takeLast`/`dropLast` use the ordinary finite-list identity `reverse . take/drop n . reverse`,
matching Wolfram's negative-count `Take`/`Drop` use case without adding signed index syntax
to the core API. `at` is partial: a negative index errors immediately, and otherwise
`caseList (error …) const . drop n` drops `n` elements and takes the head of what remains —
`const` *is* the keep-the-head handler — erroring when the drop exhausted the list.

### `partition` and `span`

```haskell
partition ∷ (a -> Bool) -> List a -> List a `Pair` List a
partition p xs = filter p xs `pair` filter (not . p) xs
```

`partition` is its own specification: "the elements that satisfy `p`, paired with the
elements that don't" — spelled as exactly that, two `filter`s over the same list packaged
into a Church `Pair`. (`not` here is the module's Church-`Bool` negation from §2, so
`not . p` is the complementary predicate.) Behavior: `(satisfying, not-satisfying)`,
preserving order.

`span` is the contiguous cousin — the longest satisfying *prefix*, then the rest — and it
gets the same treatment:

```haskell
span :: (a -> Bool) -> List a -> List a `Pair` List a
span = liftA2 (liftA2 pair) takeWhile dropWhile
```

The doubled `liftA2` is the function (reader) applicative: `liftA2 g u v = \x -> g (u x) (v x)`,
so the outer `liftA2 (liftA2 pair)` distributes the predicate `p` to both traversals and the
inner `liftA2 pair` distributes the list `xs` — β-reducing to literally the defining equation
from the Haskell report, `span p xs == (takeWhile p xs, dropWhile p xs)`. Both this and
`splitAt` above pair up two complementary traversals in exactly this shape. Yes, each walks the
input twice where a fused fold could manage one pass — but the two passes are *independent
and lazy*. A Church `Pair` holds two unevaluated components, so a consumer that eliminates
the pair and forces only one side — `fst (span p xs)` is `pair t d const = t` — pays for
only that one traversal, which a fused accumulating fold could not offer. For an
educational library, trading a constant factor for a definition that *is* its own
correctness proof is the right bargain. (`span`'s previous single-pass CPS formulation was
also the file's worst GHC-9.12 trouble spot — see Appendix A; the two-pass form needs no
annotations at all.)

### Wolfram-style chunking: `partitionEvery`, `partitionStep`, `windows`

These are the list-of-blocks analogues of Wolfram Language `Partition`, distinct from
predicate `partition` above:

```haskell
partitionEvery :: Int -> List a -> List (List a)
partitionEvery n = partitionStep n n
partitionStep :: Int -> Int -> List a -> List (List a)
partitionStep n step xs =
  gtInt n 0
    (gtInt step 0
       (leInt n (length xs)
          (take n xs `cons` partitionStep n step (drop step xs))
          nil)
       nil)
    nil
windows :: Int -> List a -> List (List a)
windows = flip partitionStep 1
```

`partitionEvery n` is `partitionStep n n`: complete, non-overlapping blocks, discarding a
trailing short block. `partitionStep n step` starts a new complete block every `step`
elements. `windows n` is the sliding-window specialization `partitionStep n 1`.
The implementation nests three Church-`Bool` guards — `n` and `step` must both be positive,
and `leInt n (length xs)` checks that a complete block remains — then emits `take n xs`
and recurses on `drop step xs`.

### `intersperse`, `replicate`

```haskell
intersperse :: a -> List a -> List a
intersperse sep = tail . concatMap (cons sep . singleton)
```

Every element is expanded into the two-element block `[sep, x]` (`cons sep . singleton`),
`concatMap` splices the blocks, and the one unwanted *leading* `sep` is chopped off by
`tail`. No two-state recursion, no conditional — "separator *between* elements" is just
"separator *before* each element, minus the first," and the empty list is safe because
`tail nil = nil`. Test (line 402): `intersperse ',' "hello" = "h,e,l,l,o"`.
`riffle` is the Wolfram Language spelling of the same operation.

`rotateLeft` moves the head to the back in one `caseList`: the empty handler is the list
itself (rotating `nil` is `nil`) and the non-empty handler `flip snoc` sends `x`/`xs'` to
``xs' `snoc` x``. `rotateLeftN n` composes `n` copies of it — `compose (replicate n
rotateLeft)`, the same `compose`/`replicate` shape as `drop` — while `rotateRightN`
conjugates by `reverse`, and `rotateRight = rotateRightN 1`. Empty lists and non-positive
counts are left unchanged.

```haskell
replicate :: Int -> a -> List a
replicate n x = leInt n 0 nil (x `cons` replicate (n - 1) x)
```

`leInt n 0` is a Church-`Bool` (from the section-0 `Int` primitives) used as the loop guard
— `n <= 0` selects `nil`, otherwise cons `x` and recurse on `n-1`. Pure Church recursion
on a primitive `Int` counter, no `if`.

### `zip`, `unzip`

```haskell
zip ∷ List a -> List b -> List (a `Pair` b)
zip = zipWith pair

unzip ∷ List (a `Pair` b) -> List a `Pair` List b
unzip = liftA2 pair (map fst) (map snd)
```

`zip` is `zipWith pair` — combine corresponding elements with the Church-`Pair`
constructor, truncating at the shorter list (the `zipWith` recursion stops the moment either
`caseList` hits its empty branch). `unzip` is its inverse in the two-independent-passes
style of `span`: the reader-applicative `liftA2 pair (map fst) (map snd)` β-reduces to
``map fst ps `pair` map snd ps`` — project every pair twice, package the two projections.
As with `span`, a consumer that eliminates the pair and forces only one side pays for only
that traversal. Test (line 264): `zip [1..5] [6..10] = [(1,6),(2,7),(3,8),(4,9),(5,10)]`.

### `inits`, `tails`

```haskell
inits :: List a -> List (List a)
inits = scanl snoc nil

tails :: ∀a. List a -> List (List a)
tails = scanr @a @(List a) cons nil
```

A perfectly dual pair of scans. `inits` is delightfully `scanl snoc nil`: scanning `snoc`
from the empty list yields every prefix, shortest first. `tails` is the mirror image,
`scanr cons nil`: a right scan's partial results are exactly the folds of the suffixes, and
with `cons`/`nil` as the algebra each partial result *is* the suffix itself — longest first,
ending with the `[]` contributed by the scan's seed `singleton nil`. The `@a @(List a)`
visible type application is one of the foundations sections' three inference-required survivors (§1):
the scan folds a `List a` into a `List (List a)`, an impredicative instantiation Quick Look
cannot guess unaided. Behavior:
`inits [1,2] = [[],[1],[1,2]]`, `tails [1,2] = [[1,2],[2],[]]`.

### `lexicographicLE` — ordering as nested Church-`Bool` dispatch

```haskell
lexicographicLE :: LE a -> LE (List a)
lexicographicLE le xs ys =
  caseList true (\x xs' ->
    caseList false (\y ys' ->
      le x y
        (le y x (lexicographicLE le xs' ys') true)
        false) ys) xs
```

A textbook lexicographic comparison written purely with Church eliminators. `LE a` is the
documentation-flavored alias `a -> a -> Bool`. The logic: empty `xs` is `<=` anything
(`true`); a non-empty `xs` against empty `ys` is `false`; otherwise compare heads with
`le x y` (a Church-`Bool`), and on a tie (`le y x` also holds) recurse on the tails. The
three-way nesting of `le` results — each a `∀e. e -> e -> e` used as a selector — replaces
what would be `compare`/`if` in ordinary code, honoring the no-conditionals rule. This is
the comparator the sort routines of Part II ([§11](#11-four-sorting-algorithms)) feed to `sortBy`-style code.

### A note on `Tuple`/`Dict`/`Matrix` aliases

`Tuple a`, `Dict k v`, and `Matrix a` (lines 121–158) are **documentation-only synonyms of
`List`** (and `List` of pairs / `List` of `List`). The type system cannot enforce "all the
same length" (`Tuple`) or "rectangular" (`Matrix`) or "unique keys" (`Dict`); the aliases
exist solely to signal intent at call sites. The N-ary parallel operators
`zipWithN`/`scanlN`/`mapAccumLN` (lines 329–369) exploit this: they take a `Tuple (List a)`
— a homogeneous list of lists — and at each step compute `heads = map head lists` and
`tails = map tail lists`, advancing all lanes in lockstep, stopping when `any null lists`
(the recursion lives in `transpose`, to which all three delegate).
That `any null lists` guard is, once more, a Church-`Bool` used directly as the branch
selector. So the entire parallel-traversal family is built from this section's `map`, `head`,
`tail`, `any`, and `null` over a `List`-that-we-promise-to-treat-as-a-`Tuple`.

---

## 5. Accumulating maps, parallel N-ary combinators, unfolds, and transpose

This chapter covers the heavy machinery near the end of section 5.3: the
accumulating maps (`mapAccumR`/`mapAccumL` and their 2- and N-ary cousins),
the parallel-iteration family (`zipWith`/`zipWithN`, `scanl2`/`scanlN`,
`mapAccumL2`/`mapAccumLN`), the two unfolds (`unfoldr`/`unfoldTree`), the
endomorphism folds (`compose`/`composeFlatten`), the Cartesian-product family
(`cartesianWith`/`cartesian`/`cartesianN`), and `transpose`.

Throughout, recall the encodings these definitions manipulate:

- `List a = ∀e. (a -> e -> e) -> e -> e` — a list *is* its own right fold. A
  list value `xs`, applied to `cons'` and `nil'`, replays its spine. The two
  function arguments are conventionally named `cons'`/`nil'` precisely because
  they are the replacement constructors the eliminator dispatches to.
- `Pair a b = ∀e. (a -> b -> e) -> e` — a pair is a CPS'd "give me a
  continuation `\x y -> ...` and I'll feed you both fields".
- `Maybe a = ∀e. e -> (a -> e) -> e` — eliminator order is `nothing`-branch
  then `just`-branch; `m d f` reads "if `m` is nothing return `d`, else apply
  `f` to the contents".
- `Bool = ∀e. e -> e -> e` — `b t f` selects `t` on true, `f` on false. This is
  the house rule's only `if`: with `if`-then-`else` banned, every conditional in
  this chapter is `someChurchBool thenBranch elseBranch`. `null xs`, `any null
  lists`, and `leInt n 0` are all used directly in that selector position.

A recurring theme: because `List a` is literally the right-fold type, a function
that *produces* a list may be η-expanded to take the consumer's `cons'`/`nil'` as
extra arguments and thread them down the recursion — `tail` (§3) is the standing
example. After the terseness pass, though, most producers are spelled as plain
folds or `caseList` recursions that build genuine lists from the real `cons`/`nil`;
the fusion is the same either way, since applying such a list to `cons'`/`nil'`
β-reduces right back to the η-expanded form.

### `mapAccumR` — right-to-left accumulation by reversal

```haskell
mapAccumR ∷ (s -> a -> s `Pair` b) -> s -> List a -> s `Pair` List b
mapAccumR f s = bimapPair id reverse . mapAccumL f s . reverse
```

Right-to-left threading is bought with two `reverse`s around the left-to-right
worker: reverse the input so `mapAccumL` visits the original elements
rightmost-first, then patch up the result pair with `bimapPair id reverse` —
keep the final state (`id`), un-reverse the output list so it lines up with the
original input order. The state therefore flows right-to-left, exactly matching
`Data.List.mapAccumR`, while all the actual accumulation machinery lives in
`mapAccumL` just below.

```text
mapAccumR (\s x -> pair (s+x) (x*2)) 0 [1,2]
  = bimapPair id reverse (mapAccumL f 0 (reverse [1,2]))
  = bimapPair id reverse (mapAccumL f 0 [2,1])
  -- mapAccumL (its trace pattern below, with these numbers):
  --   step (pair 0 nil) 2:  f 0 2 = pair 2 4  → pair 2 [4]
  --   step (pair 2 [4]) 1:  f 2 1 = pair 3 2  → pair 3 [4,2]
  = bimapPair id reverse (pair 3 [4,2])
  = pair 3 [2,4]
```

**Behavior:** returns `(final state, mapped outputs)`, threading right-to-left. The test
suite (`listFunctionTests`, "mapAccumR") checks
`mapAccumR (\acc x -> pair (acc+x) (x*2)) 0 [1..5]` against
`Data.List.mapAccumR (\acc x -> (acc+x, x*2)) 0 [1..5]`, i.e.
**`(15, [2,4,6,8,10])`** — final accumulator 15 (the sum), each element doubled.

### `mapAccumL` — the left-to-right worker via `foldl` and `snoc`

```haskell
mapAccumL ∷ (s -> a -> s `Pair` b) -> s -> List a -> s `Pair` (List b)
mapAccumL f s = foldl
  (\acc x -> acc $ \s₁ bs -> bimapPair id (snoc bs) $ f s₁ x)
  $ s `pair` nil
```

This is the primitive of the pair: a `foldl` whose accumulator is a `Pair` of
"current state" and "results so far". The body is pure CPS pair-elimination — no
`fst`/`snd` projections appear. `acc` is a `Pair s (List b)`; applying it to the
continuation `\s₁ bs -> …` destructures it into the threaded state `s₁` and the
accumulated results `bs`. Then `f s₁ x` is itself a `Pair s b`, and
`bimapPair id (snoc bs)` rebuilds it in one stroke: keep the new state, append
the new output at the *back* of `bs`. The append is the detail worth dwelling
on: in a left fold we encounter the leftmost element first, and its output must
land first in the result, so we `snoc`, not `cons`. `snoc xs x c = xs c . c x`
does this in `O(n)` per step (`O(n²)` overall), the honest cost of building a
left-to-right list out of a right-fold representation without a reversal. It
type-checks at rank-N because `acc`, of type `Pair s (List b) = ∀e. (s -> List b
-> e) -> e`, is *instantiated at* `e := s `Pair` List b` when applied to the
step's continuation — a `∀`-under-a-synonym instantiated at another polytype,
which is what `ImpredicativeTypes` buys.

Note `mapAccumL` leans on `foldl`, itself one of the cleverest encodings in the
file: `foldl f z xs = xs (flip (.) . flip f) id z`, whose step unfolds to
`\x g acc -> g (f acc x)`. The list is folded not
into a value but into a *function* `acc -> result` (a difference/continuation
list), which is then applied to the seed `z`. Each element `x` extends the
function so that it first does `f acc x` and passes the result rightward to the
continuation `g`. This "build a function with `foldr`, then apply it once" trick
is the standard way to get a left fold out of a pure right-fold type, and every
`foldl`-based definition here (`mapAccumL`, `foldl1`, `reverse`, `last`,
`cartesianN`) inherits it.

```text
mapAccumL (\s x -> pair (s+x) (x*2)) 0 [1,2]
  = foldl step (pair 0 nil) [1,2]
  -- left fold: process 1 then 2
  step (pair 0 nil) 1
    -- s₁:=0, bs:=nil ; f 0 1 = pair 1 2 ; s₂:=1, b:=2
    = pair 1 (snoc nil 2) = pair 1 [2]
  step (pair 1 [2]) 2
    -- s₁:=1, bs:=[2] ; f 1 2 = pair 3 4 ; s₂:=3, b:=4
    = pair 3 (snoc [2] 4) = pair 3 [2,4]
```

**Behavior:** `(final state, outputs)` folding left. Test (`listFunctionTests`,
"mapAccumL") compares against `Data.List.mapAccumL (\acc x -> (acc+x, x*2)) 0
[1..5]` = **`(15, [2,4,6,8,10])`**. The accumulator value coincides with
`mapAccumR`'s here only because `+` is commutative and associative; the
machinery differs.

### `zipWith` — parallel two-list recursion via `caseList`

```haskell
zipWith ∷ _ -> List a₁ -> List a₂ -> List b
zipWith f xs ys = caseList nil (\x xs' ->
  caseList nil (\y ys' -> f x y `cons` zipWith f xs' ys') ys) xs
```

A doubly-nested `caseList` (§3), and a good place to appreciate its handlers-first
argument order: `caseList nil (…) xs` reads "if `xs` is empty, `nil`; otherwise
open it". The outer `caseList` opens `xs` into `x`/`xs'`; its handler immediately
opens `ys` the same way; the doubly-non-empty case combines heads with `f x y`,
`cons`es it on, and recurses on both tails. Either list running dry short-circuits
to `nil`, giving the shorter-of-two length. No `Maybe`, no `Pair`, no η-expansion —
the one-step eliminator does all the destructuring.

```text
zipWith (+) [1,2] [10,20,30]
  caseList on [1,2]:      non-empty → x:=1, xs':=[2]
  caseList on [10,20,30]: non-empty → y:=10, ys':=[20,30]
      = (1+10) `cons` zipWith (+) [2] [20,30]
      = 11 `cons` ( caseList on [2]:     x:=2, xs':=[]
                    caseList on [20,30]: y:=20, ys':=[30]
            = (2+20) `cons` zipWith (+) [] [30]
            = 22 `cons` ( caseList on []: empty → nil ) )
  = cons 11 (cons 22 nil)             -- i.e. [11,22]
```

**Behavior:** elementwise combination, length = shorter input. Test
(`listFunctionTests`, "zipWith (+)") compares `zipWith (+) [1..5] [6..10]`
against the Prelude — **`[7,9,11,13,15]`**. `zip = zipWith pair` reuses this
directly.

### The Tuple-of-Lists parallel-iteration pattern (`zipWithN`/`scanlN`/`mapAccumLN`)

`zipWith` zips a fixed *two* lists. To zip an arbitrary *N* lists at once we need
to iterate over a homogeneous collection of lists in lockstep. The file's device
is a `Tuple (List a)` — and crucially `Tuple a = List a` is a **documentation-only
alias**: the type system sees a plain list, but the name signals "all instances
are the same length and are read as the components of a tuple". (Likewise `Dict`,
`Matrix`, `Equal`, and `LE` are pure aliases.) So `Tuple (List a)` is "a list of
lists, read as a tuple-of-columns", and one *step* of N-ary iteration is:

1. **Stop test:** `any null tupleOfLists` — if *any* component list is empty, the
   parallel iteration ends. `any` is `foldr (\x -> p x true) false`; here it
   yields a Church `Bool` used immediately as the `if` selector (house rule: no
   `if`-then-`else`), picking the `nil`/base branch on true.
2. **Take a slice:** `heads = map head tupleOfLists` collects the head of every
   component into one homogeneous `Tuple a` (the current "row" across all lists).
3. **Advance:** `tails = map tail tupleOfLists` drops the head of every component,
   producing the smaller `Tuple (List a)` to recurse on.

`f heads` consumes the slice however the caller wants (sum it, multiply it, pair
it, fold it). This `any null` + `map head` / `map tail` triad is realized once —
inside `transpose` — and shared by `zipWithN`, `scanlN`, and `mapAccumLN`, which
are all thin compositions through it; they differ only in what they do with each
row and how/whether they thread an accumulator. This is the N-ary generalization
of `zipWith`'s double `caseList`: instead of matching a statically known number of
lists, it `map head`/`map tail`s across a runtime-sized tuple. It relies on `tail` of the empty list never being forced,
which is guaranteed because `any null` filters that case out before the slice is
taken (`head`/`tail` are partial: `head [] = undefined`).

A subtle correctness point on `tail`: it is the file's trickiest single
definition, a one-step-delay encoding —
`tail xs cons' nil' = xs (\x r skip -> skip (r false) (x `cons'` r false)) (const nil') true`
— that during the right fold carries a `skip`/keep flag so that the very first
element is dropped while every later element is kept. `map tail` therefore
genuinely re-tails each component on every N-ary step. That makes the N-ary
combinators quadratic in the lists' lengths, but the encoding stays pure.

### `zipWithN` — zip an N-tuple of lists

```haskell
zipWithN :: (Tuple a -> b) -> Tuple (List a) -> List b
zipWithN f = map f . transpose
```

Type `(Tuple a -> b) -> Tuple (List a) -> List b` — and the definition is just
`map f . transpose`: `transpose` runs the triad, turning the tuple-of-columns into
the list of rows (each row a `Tuple a`), and `map f` consumes one row at a time.
Unfolding `transpose`'s recursion and fusing the `map` gives the step-by-step
behavior: the `any null` Church-Bool selects `nil` (some column empty) or the
productive branch, which emits `f heads` and recurses on `tails`.

```text
zipWithN (foldr (+) 0) [[1,2],[2,4,6],[3,9,15,21]]
  any null [...]  → none empty → false → take productive branch
    heads = map head = [1,2,3]      tails = map tail = [[2],[4,6],[9,15,21]]
    = (foldr (+) 0 [1,2,3]) `cons` zipWithN f [[2],[4,6],[9,15,21]]
    = 6 `cons` ( any null → false
        heads = [2,4,9]   tails = [[],[6],[15,21]]
        = (2+4+9) `cons` zipWithN f [[],[6],[15,21]]
        = 15 `cons` ( any null [[],...] → TRUE → nil ) )
  = cons 6 (cons 15 nil)             -- [6,15]
```

**Behavior:** like `zipWith` but for any number of equal-or-unequal-length lists,
result length = shortest. Tests (`listFunctionTests`):
`zipWithN (foldr (+) 0) [[1,2,3],[2,4,6],[3,9,15]]` = **`[6,15,24]`**;
the ragged `[[1,2],[2,4,6],[3,9,15,21]]` = **`[6,15]`** (matching the trace);
a list containing `[]` gives **`[]`**; and the product variant
`zipWithN (foldr (*) 1) [[1,2,3],[2,4,6],[3,9,15]]` = **`[6,72,270]`**.

### `scanl2` — parallel two-list scan (zipWith ⋈ scanl)

```haskell
scanl2 ∷ _ -> b -> List a₁ -> List a₂ -> List b
scanl2 f q xs = scanl (uncurry . f) q . zip xs
```

`scanl2` is `scanl` lifted to two parallel lists — literally: `zip xs` pairs the
lists up (truncating at the shorter), and `scanl (uncurry . f) q` scans the
pair-list, `uncurry . f` opening each Church pair back into the two element
arguments `f` expects. The seed `q` is always emitted first (by `scanl`'s
`cons q .` front door), so the result is one longer than the shorter input — even
when both inputs are empty (then `zip` yields `nil`, `scanl`'s `caseList` stops,
and only the seed remains). Emitting and stepping remain the same act, one call
apart, exactly as in `scanl`.

```text
scanl2 (\acc x y -> acc + x + y) 0 [1,2,3] [4,5,6]
  = 0 `cons` scanl2 f 5  [2,3] [5,6]       -- x:=1, y:=4 ; f 0 1 4 = 5
  = 0 : 5  `cons` scanl2 f 12 [3] [6]      -- x:=2, y:=5 ; f 5 2 5 = 12
  = 0 : 5 : 12 `cons` scanl2 f 21 [] []    -- x:=3, y:=6 ; f 12 3 6 = 21
  = 0 : 5 : 12 : 21 `cons` nil             -- zip ran dry, after the last seed
  = [0,5,12,21]
```

**Behavior:** accumulating left scan over two lists. The dedicated
`scanl2Tests` block exercises it thoroughly:
equal-length `(+)` on `[1,2,3,4]`/`[4,5,6,7]` = **`[0,5,12,21,32]`**;
`(*)`-style on `[1,2,3,4]`/`[2,3,4,6]` = **`[1,2,12,144,3456]`**;
ragged inputs (either shorter) both = **`[0,5,12,21]`**;
any empty input = **`[0]`** (seed only);
non-commutative `acc - x - y` from seed 5 = **`[5,0,-3,-8]`**;
and `acc + x*y` = **`[0,4,13,23]`**.

### `scanlN` — N-ary parallel scan

```haskell
scanlN ∷ (b -> Tuple a -> b) -> b -> Tuple (List a) -> List b
scanlN f q = scanl f q . transpose
```

This is `scanl2` generalized: `transpose` runs the Tuple-of-Lists triad to
produce the row list, then plain `scanl` scans it. The emission discipline is
inherited: `q` is emitted unconditionally *before* any row is examined, so exactly
like `scanl`/`scanl2` the output is one longer than the productive recursion. The
accumulator update `f q heads` consumes a whole row at once, and the *new*
accumulator is what gets recursed with (and emitted on the next round as its own
seed). When `any null lists` fires inside `transpose`, the row stream ends and the
scan terminates after the last seed was already emitted.

```text
scanlN (\acc row -> acc + foldr (+) 0 row) 0 [[1,2],[10,20]]
  = 0 `cons` ( any null [[1,2],[10,20]] → false
      heads=[1,10], tails=[[2],[20]] ;  f 0 [1,10] = 0+11 = 11
      = scanlN f 11 [[2],[20]]
        = 11 `cons` ( any null → false
            heads=[2,20], tails=[[],[]] ; f 11 [2,20] = 11+22 = 33
            = scanlN f 33 [[],[]]
              = 33 `cons` ( any null [[],[]] → TRUE → nil ) ) )
  = [0,11,33]
```

**Behavior:** left scan threading an accumulator across an N-tuple of lists, one
longer than the shortest. Pinned directly by the `"scanlN"` case in the suite,
alongside the thorough `scanl2Tests` for its 2-ary sibling (above); it also
shares the verified triad with `zipWithN`.

### `mapAccumL2` and `mapAccumLN` — accumulating parallel maps returning a Pair

```haskell
mapAccumL2 ∷ (s -> a₁ -> a₂ -> s `Pair` b) -> s -> List a₁ -> List a₂ -> s `Pair` List b
mapAccumL2 f s xs = mapAccumL (uncurry . f) s . zip xs
```

Where `scanl2` only emits values, `mapAccumL2` also returns the final state, so
its result is a `Pair s (List b)` rather than a bare `List` — and it is built by
exactly the same two-step recipe as `scanl2`: `zip xs` pairs the inputs
(truncating at the shorter, which is what preserves the *current* state when
either list runs dry), and `mapAccumL (uncurry . f) s` threads the accumulator
down the pair-list, `uncurry` opening each zipped pair back into `f`'s two
element arguments. All the CPS pair-elimination happens inside `mapAccumL`
(above): the accumulator pair is opened with `acc $ \s₁ bs -> …`, the step result
is re-paired with `bimapPair`, and outputs are `snoc`ed so left-to-right order
falls out.

```text
mapAccumL2 (\s x y -> pair (s+x+y) (x*y)) 0 [1,2] [3,4]
  = mapAccumL (uncurry . f) 0 (zip [1,2] [3,4])
  = mapAccumL (uncurry . f) 0 [(1,3),(2,4)]
  step (pair 0 nil) (1,3):  f 0 1 3 = pair 4 3    → pair 4 [3]
  step (pair 4 [3]) (2,4):  f 4 2 4 = pair 10 8   → pair 10 [3,8]
  = pair 10 [3,8]
```

```haskell
mapAccumLN ∷ (s -> Tuple a -> s `Pair` b) -> s -> Tuple (List a) -> s `Pair` List b
mapAccumLN f s = mapAccumL f s . transpose
```

`mapAccumLN` is to `mapAccumL2` what `scanlN` is to `scanl2`: `transpose` runs
the Tuple-of-Lists triad (`any null` stop, `map head` slice, `map tail` advance)
to produce the row list, and the ordinary 1-ary `mapAccumL` threads the
accumulator down it, `f s heads` consuming one whole row per step. The definition
is only the plumbing between those two already-documented machines.

**Behavior:** parallel `mapAccumL` returning `(final state, outputs)`, length =
shortest. Both forms now have direct cases in the suite — `"mapAccumL2"` and
`"mapAccumLN truncates at the shortest input"` — alongside `mapAccumL`'s own
("mapAccumL" test → `(15,[2,4,6,8,10])`) and the shared triad under `zipWithN`.

### `unfoldr` — corecursive list construction from a seed

```haskell
unfoldr ∷ (b -> Maybe (a `Pair` b)) -> b -> List a
unfoldr f s = f s nil $ uncurry $ \x s' -> x `cons` unfoldr f s'
```

The dual of `foldr`: instead of consuming a list it produces one. `f s` is a
`Maybe (a `Pair` b)`; eliminating it, the `nothing`-branch is the genuine `nil`
(seed exhausted → end the list) and the `just`-branch is `uncurry $ \x s' -> …`:
`uncurry` opens the delivered `Pair a b` of "next element, next seed", the element
`x` is emitted via `cons`, and the recursion continues from the new seed `s'`.
This is precisely the standard `unfoldr`, with the Maybe and Pair both consumed by
their eliminators rather than pattern-matched.

```text
unfoldr (\n -> leInt n 0 nothing (just (pair n (n-1)))) 2
  f 2 = just (pair 2 1)
    → uncurry (\x s' -> x `cons` unfoldr f s') (pair 2 1)
    → 2 `cons` unfoldr f 1
      f 1 = just (pair 1 0)
      → 1 `cons` unfoldr f 0
        f 0 = leInt 0 0 → ... nothing  → nil
  = cons 2 (cons 1 nil)                -- [2,1]
```

`unfoldr` currently has no in-module clients (`init`, once written as an unfold
that stopped one element early, is now the `zipWith const xs (tail xs)` one-liner
of §3), but it remains the family's archetype: `replicate` and the `scanl`
family are instances of the same produce-and-recurse shape (`take` now rides on
`zipWith` instead), and `unfoldTree` below generalizes it from one successor seed
to a list of them.

**Behavior:** builds a list by iterating the seed until `nothing` — pinned by the
direct `"unfoldr"` countdown case in the suite, besides the producer functions
above, which share its driver pattern.

### `unfoldTree` — corecursion that branches

```haskell
unfoldTree ∷ (b -> a `Pair` List b) -> b -> List a
unfoldTree f s = f s $ \x seeds -> x `cons` concatMap (unfoldTree f) seeds
```

Where `unfoldr` produces *one* successor seed (or stops), `unfoldTree` produces a
*list* of successor seeds, so it walks a virtual rose tree and flattens it into a
list in preorder. `f s` is a `Pair a (List b)` (never a `Maybe` — every node
yields an element `x` plus zero-or-more child seeds; the empty `seeds` list is
how a leaf terminates). The continuation `\x seeds -> ...` emits `x`, then recurses
on each child seed and concatenates: `concatMap (unfoldTree f) seeds`. The
recursion builds with the genuine `cons`, and `concatMap`'s definitional fusion
lays the whole tree down into a single output fold. Termination is data-driven: a
node returning `pair x nil` is a leaf, and `concatMap g nil = nil`.

```text
-- f n = pair n [n*2, n*2+1] but stop (no children) once n > 2:
-- f 1 = pair 1 [2,3] ; f 2 = pair 2 [] ; f 3 = pair 3 []
unfoldTree f 1
  f 1 = pair 1 [2,3]
  = 1 `cons` concatMap (unfoldTree f) [2,3]
    unfoldTree f 2 = (f 2 = pair 2 []) → 2 `cons` concatMap _ [] = [2]
    unfoldTree f 3 = (f 3 = pair 3 []) → 3 `cons` concatMap _ [] = [3]
  = 1 `cons` ([2] ++ [3])
  = [1,2,3]
```

**Behavior:** preorder flattening of a seed-generated tree — pinned by the
`"unfoldTree preorder"` case in the suite; it is the corecursive sibling of
`unfoldr` and reuses the already-tested `concatMap`.

### `compose` and `composeFlatten` — folding endomorphisms / Kleisli arrows

```haskell
compose :: List (a -> a) -> a -> a
compose = foldr (.) id
```

A list of endomorphisms `a -> a` folded under ordinary function composition. Since
`List a = ∀e. (a -> e -> e) -> e -> e` *is* the right fold, instantiating
`e := (a -> a)`, applying the list to `cons' := (.)` and `nil' := id` literally
*is* `foldr (.) id xs`. The result applies the functions right-to-left:
`compose [f,g,h] x = f (g (h x))`. There is no recursion to write — the list's own
eliminator does the folding.

```text
compose [f,g] = [f,g] (.) id
  -- [f,g] replays as (.) f ((.) g id)
  = f . (g . id)
  = \x -> f (g x)
```

```haskell
composeFlatten :: ∀a. List (a -> List a) -> a -> List a
composeFlatten = foldr @(a -> List a) (\f g -> concatMap g . f) singleton
```

The list-monad (Kleisli) analogue of `compose`: each element is a function
`a -> List a` (a nondeterministic step), and `composeFlatten` chains them with
Kleisli composition `>=>`, seeding with `singleton` (= `return` for the list
monad, the Kleisli identity). `foldr step singleton` composes right-to-left, so
`composeFlatten [f,g] x = concatMap g (f x)` runs `f` first, then feeds every
result through `g`.

This step is the **GHC 9.12 patch site** flagged in Appendix A. It was first
rewritten from the original point-free `f x . flip g`-style formulation (which
needed `DeepSubsumption`) to an explicit `concatMap g (f x)`, and the terseness
pass has since compacted that to `\f g -> concatMap g . f` — still the same
left-to-right Kleisli chain, with the bind made concrete (`concatMap` *is* the
list `>>=` with arguments flipped) so the type checker never has to instantiate a
`∀` at a section. The `foldr @(a -> List a)` visible type application is another
of the foundations sections' three inference-required survivors: the fold's element type *is*
the polytype `a -> List a`, which Quick Look cannot conjure unaided.

```text
composeFlatten [f,g] = foldr step singleton [f,g] = step f (step g singleton)
  -- step f h = concatMap h . f, i.e. \x -> concatMap h (f x)
  let h = step g singleton = \y -> concatMap singleton (g y)   -- = g (since concatMap singleton = id)
  step f h = \x -> concatMap h (f x)
  -- at a concrete x:
composeFlatten [f,g] x = concatMap (\y -> concatMap singleton (g y)) (f x)
                       = concatMap g (f x)
```

**Behavior:** `compose` = `foldr (.) id`; `composeFlatten` = Kleisli/`>=>` fold
in the list monad. The latter has a dedicated `"composeFlatten"` case in the suite;
`compose` is exercised wholesale through `drop` and `rotateLeftN` (both
`compose`-of-`replicate` under the hood), and `concat`/`concatMap` on which
`composeFlatten` rests are tested too (`concatTests`: `concat [[1,2],[3,4,5],[6,7]]` =
**`[1..7]`**; `listFunctionTests` "concatMap": `concatMap (\x -> [x, x*2]) [1..5]`).

### `cartesianWith` / `cartesian` / `cartesianN` — Cartesian products

```haskell
cartesianWith :: _ -> List a -> List b -> List c
cartesianWith f xs ys = concatMap (flip map ys . f) xs

cartesian :: List a -> List b -> List (a `Pair` b)
cartesian = cartesianWith pair

cartesianN :: Tuple (List a) -> List (Tuple a)
cartesianN = foldl (cartesianWith snoc) (singleton nil)
```

`cartesianWith` is a `concatMap` of `map`s: for each `x`, the section
`flip map ys . f` builds `map (f x) ys` — the whole `x`-row of the product — and
`concatMap` splices the rows end to end. Thanks to the definitional fusion of
`concatMap` and `map` (§4), nothing intermediate is materialized: applied to a
consumer `c`, each row folds `ys` with `\y rest -> c (f x y) rest`, and each
row's terminal slot is filled by the next row's fold — the nested loops splice
into one output list, the products of later `x`s becoming the tail.

`cartesianN` builds the N-ary product by left-folding `cartesianWith snoc` over
the tuple of lists, starting from `singleton nil` — the list containing one empty
tuple. Each list in the tuple extends every partial tuple-so-far by one element
via `snoc` (append, to keep component order), exactly the standard "sequence in
the list monad" / `mapM (const id)` construction: `[[1,2],[3,4]]` becomes
`[[1,3],[1,4],[2,3],[2,4]]`. The seed `singleton nil` is the monoidal unit (one
way to choose nothing), and `snoc` grows each combination on the right as we move
through successive lists.

```text
cartesianWith pair [1,2] [10,20]
  = concatMap (\x -> map (pair x) [10,20]) [1,2]
  row x:=1 → [(1,10),(1,20)] ;  row x:=2 → [(2,10),(2,20)]
  = [ (1,10),(1,20),(2,10),(2,20) ]
```

**Behavior:** standard Cartesian products. All three now have direct cases in the
suite — `"cartesianWith ordering"`, `"cartesian"`, and `"cartesianN"` — besides
resting on the tested `snoc`/`singleton` (`listOpTests`).

### `transpose` — first column out, recurse on the rest

```haskell
transpose :: ∀a. Matrix a -> Matrix a
transpose xss =
  null @(List a) xss
    nil
    (any null xss
      nil
      (map head xss `cons` transpose (map tail xss)))
```

`Matrix a = List (List a)` (a documentation alias again). This is the classic
textbook recursion, and — the input being rectangular by contract — it needs
nothing more:

1. **Empty matrix** (`null @(List a) xss`, no rows at all) → `nil`.
2. **Rows exhausted** (`any null xss`) → `nil`. For a rectangular matrix, one
   empty row means *all* rows are empty: every column has been peeled off, so the
   transpose is complete.
3. **General case:** the first output row is the first input *column*,
   `map head xss`; the rest is the transpose of the matrix minus that column,
   `transpose (map tail xss)`.

Note the shape: this is exactly the Tuple-of-Lists triad from `zipWithN`/`scanlN`
— stop on `any null`, slice with `map head`, advance with `map tail` — reused on a
`Matrix` instead of a `Tuple (List a)`; on non-empty input, `transpose` behaves
like `zipWithN id`. The one case the triad alone cannot handle is the *empty
matrix*: `any null nil` is (vacuously) `false`, so without the leading `null`
guard an empty `xss` would take the productive branch and emit `map head nil =
nil` forever — an infinite list of empty rows. Guard (1) is what makes
`transpose nil = nil` instead of that infinite list. And as in the triad, the
partial `head` (`tail` is total — `tail nil = nil`) is safe because `any null`
has already ruled out an empty row before any slice is taken.

On the types: the branches instantiate `map`, `head`, and `any null` at the
element type `List a` — a polytype — and Quick Look copes unaided, exactly as it
does in `zipWithN`, because in each call the polytype is supplied directly by an
argument's own type. The single survivor of the old GHC 9.12 patch (Appendix A)
is the visible type application in `null @(List a) xss`: there the element type
would have to be *extracted by unifying* `null`'s type against `xss`, which
9.12's stricter impredicativity checking refuses to guess, so the `@(List a)`
pins it. It is one of exactly three `TypeApplications` uses in the foundations
sections (0–7) — the others are `scanr @a @(List a)` in `tails` and `foldr @(a -> List a)` in
`composeFlatten` — the full set of Part I survivors of the compile-checked sweep that
removed every annotation inference does not need.

```text
transpose [[1,2,3],[4,5,6]]
  null? no ; any null? no
  = map head xss `cons` transpose (map tail xss)
  = [1,4] `cons` transpose [[2,3],[5,6]]
      ⟶ [2,5] `cons` transpose [[3],[6]]
          ⟶ [3,6] `cons` transpose [[],[]]
              null [[],[]]? no (two rows) ; any null [[],[]] → true → nil
  = [[1,4],[2,5],[3,6]]
```

**Behavior:** rectangular-matrix transpose. Test (`matrixTests`, "transpose"):
`transpose [[1,2,3],[4,5,6]]` = **`[[1,4],[2,5],[3,6]]`**, matching the trace.

---

## 6. Church Maybe

Recall the encoding from §1:

```haskell
type Maybe a = ∀e. e -> (a -> e) -> e
```

A `Maybe a` *is* its own case analysis. It is a rank-N function that takes two continuations — a value `e` to use for the `Nothing` case, and a function `a -> e` to apply in the `Just` case — and returns an `e`. There is no tag to inspect: the value chooses for you. This is the two-way eliminator

```
m  default  (\x -> ...)
   ^^^^^^^^  ^^^^^^^^^^^
   Nothing   Just branch
   branch
```

Every consumer in this section is just a particular pair of continuations handed to `m`. The `∀e` lives *inside* the synonym, so a single `Maybe a` value can be eliminated at `Bool`, at `a`, at `List a`, at `Maybe (List a)` — each call site instantiates `e` independently. `ImpredicativeTypes` is what allows that quantifier to sit inside the synonym and to be instantiated at other polytypes (e.g. when `e` is itself chosen to be a `List a`, which is again a `∀`-type).

### Constructors: `just` and `nothing`

```haskell
just ∷ a -> Maybe a
just x _ j = j x

nothing ∷ Maybe a
nothing = const
```

`just x` ignores the `Nothing`-continuation (`_`) and feeds `x` to the `Just`-continuation `j`. `nothing` ignores the `Just`-continuation entirely and returns the `Nothing`-default; since `nothing n j = n`, it is exactly `const` (note `const = \n _ -> n`). These are the two introduction forms, and they are the η-long identities of the eliminator type: `just x` selects the second argument, `nothing` selects the first.

The test suite pins down the raw eliminator behavior directly (`test/Spec.hs`, `maybeTests`):

```text
just (5 ∷ Int) "nothing" show   ==  "5"
nothing "nothing" (show ∷ Int → String)  ==  "nothing"
```

i.e. `just 5` runs the `Just`-continuation `show` on `5`, while `nothing` returns the default string untouched.

### `isNothing`

```haskell
isNothing ∷ Maybe a -> Bool
isNothing = null . maybeToList
```

Two neighbors composed: `maybeToList` (below) reflects the `Maybe` into a zero-or-one-element list — `nothing ↦ nil`, `just x ↦ [x]` — and `null` asks the one question a length-0-or-1 list can answer. The returned `Bool` is meant to be used *directly* as an if-then-else, in keeping with the file's house rule that `if`/`then`/`else` is banned: a Church `Bool` selecting between two branches *is* the conditional.

Worked trace:

```text
isNothing nothing
  = null (maybeToList nothing)            -- unfold isNothing
  = null (nothing nil singleton)          -- unfold maybeToList
  = null nil                              -- nothing = const
  = true

isNothing (just x)
  = null (maybeToList (just x))           -- unfold isNothing
  = null (singleton x)                    -- just x _ j = j x, here j = singleton
  = false                                 -- a one-element list is not null
```

Tests (`maybeOpTests`):

```text
toPreludeBool (isNothing nothing)    == True
toPreludeBool (isNothing (just 5))   == False
```

### `fromJust` and `fromMaybe`

```haskell
fromJust ∷ Maybe a -> a
fromJust = fromMaybe undefined

fromMaybe ∷ a -> Maybe a -> a
fromMaybe x m = m x id
```

Both pick `e = a` and pass `id` as the `Just`-branch, so a `just`'s payload is returned verbatim. They differ only in the `Nothing`-branch: `fromJust` supplies `undefined` (the file permits `undefined`/`error`, which are ADT-free), so forcing the result of `fromJust nothing` throws; `fromMaybe` supplies a caller-given default `x`. Note that `undefined` is passed unforced — Church elimination is lazy in the unused branch, exactly as `Maybe` pattern matching would be.

Worked traces for `fromMaybe`:

```text
fromMaybe d nothing
  = nothing d id                          -- unfold fromMaybe (x := d, m := nothing)
  = const d id                            -- nothing = const
  = d                                     -- const d id = d

fromMaybe d (just x)
  = just x d id                           -- unfold fromMaybe
  = id x                                  -- just x _ j = j x, here j = id
  = x                                     -- id x = x
```

Tests (`maybeOpTests`):

```text
fromJust (just 42)        == 42
fromMaybe 99 nothing      == 99
fromMaybe 99 (just 42)    == 42
```

### `maybeToList`

```haskell
maybeToList ∷ Maybe a -> List a
maybeToList m = m nil singleton
```

The `Maybe` is eliminated with the two list *constructors* as its continuations: the `Nothing`-branch is the empty list `nil`, and the `Just`-branch is `singleton` itself — the `a -> List a` injection is exactly the right shape for the `a -> e` slot, with `e` instantiated at the polytype `List a` (an `ImpredicativeTypes` moment). No η-expansion is needed; the constructors are passed as ordinary values.

```text
maybeToList nothing
  = nothing nil singleton                 -- unfold
  = const nil singleton                   -- nothing = const
  = nil                                    -- empty list

maybeToList (just x)
  = just x nil singleton                  -- unfold
  = singleton x                            -- just x _ j = j x
```

Tests (`maybeOpTests`):

```text
toPreludeList (maybeToList (nothing ∷ Maybe Int))  == []
toPreludeList (maybeToList (just 42))              == [42]
```

### `listToMaybe`

```haskell
listToMaybe ∷ List a -> Maybe a
listToMaybe = foldr (const . just) nothing
```

This runs the *list's* eliminator (recall `List a = ∀e. (a -> e -> e) -> e -> e`, the right fold). The `cons`-step is `const . just` — unfolded, `\x _ -> just x`: it grabs the head `x`, discards the fold of the tail, and yields `just x`. The `nil`-case is `nothing`. So a fold that ordinarily threads through the whole list short-circuits to "first element or `Nothing`" purely by ignoring its second argument. Because the step ignores the accumulator, laziness means the tail is never forced — `head`-like behavior falls out of a `foldr`. Here `e` is instantiated to `Maybe a`, itself a polytype.

```text
listToMaybe nil
  = nil (\x _ -> just x) nothing
  = (const id) (\x _ -> just x) nothing   -- nil = const id
  = id nothing
  = nothing

listToMaybe (cons a as)
  = (cons a as) (\x _ -> just x) nothing
  = (\x _ -> just x) a (as (\x _ -> just x) nothing)   -- cons step
  = just a                                              -- second arg discarded
```

Tests (`maybeOpTests`):

```text
toPreludeBool (isNothing (listToMaybe nil))            == True
fromJust (listToMaybe (fromPreludeList [1,2,3,4,5]))   == 1
```

### `catMaybes`

```haskell
catMaybes ∷ List (Maybe a) -> List a
catMaybes = mapMaybe id
```

Two already-built pieces snap together, through `mapMaybe = concatMap . (maybeToList .)` — read the point-free tangle as `mapMaybe f = concatMap (maybeToList . f)`, so `catMaybes = mapMaybe id = concatMap maybeToList`. `maybeToList` (just above) sends each `Maybe a` to a zero-or-one-element list, and `concatMap` splices those mini-lists into one output: a `nothing` contributes `nil` and simply vanishes in the concatenation, a `just x` contributes the singleton. So `catMaybes` keeps exactly the `Just` values, in order — `mapMaybe id` specialized to extraction. And because `concatMap f xs c = xs (\x -> f x c)`, no intermediate list-of-lists is ever materialized: each `maybeToList m` receives the *outer* consumer `c` directly and folds straight into the result. Read monadically, `maybeToList` is the canonical monad morphism `Maybe ↝ List` and `catMaybes` is its pointwise application followed by the list monad's `join`.

### `squashMaybe`

```haskell
squashMaybe ∷ Maybe (Maybe a) -> Maybe a
squashMaybe = fromMaybe nothing
```

The monadic `join` for `Maybe`, in one line. Outer `Nothing` ↦ `nothing`; outer `Just inner` ↦ `id inner = inner`. Here `e = Maybe a`, so both branches return a `Maybe a` and the types line up: ``m :: Maybe (Maybe a)`` is `∀e. e -> (Maybe a -> e) -> e`, instantiated at `e = Maybe a`.

```text
squashMaybe nothing          = nothing nothing id = const nothing id = nothing
squashMaybe (just nothing)   = just nothing nothing id = id nothing = nothing
squashMaybe (just (just x))  = just (just x) nothing id = id (just x) = just x
```

### `sequence` — the clever one

```haskell
sequence ∷ List (Maybe a) -> Maybe (List a)
sequence ms e = ms (\m r k -> m e $ \x -> r $ k . cons x) ($ nil)
```

This turns a list of `Maybe`s into a `Maybe` of a list: `Just` of all the payloads if every element is `Just`, otherwise `Nothing`. The artistry is the choice of fold target. The result type `Maybe (List a) = ∀e. e -> (List a -> e) -> e` is η-expanded just far enough to expose the `Nothing`-default `e` — and then the list is folded not into a `Maybe` but into a **success-continuation transformer**: each fold state is a function `(List a -> e) -> e` that either calls its success continuation `k` on the list built so far, or bypasses it entirely and returns the shared `e`.

- The `nil` slot is `($ nil)` — "call the success continuation on the empty list": an empty list of `Maybe`s succeeds with `[]`.
- The step `\m r k -> m e $ \x -> r $ k . cons x` eliminates the current element `m` *with the outer `e` as its nothing-branch*: if `m` is `nothing`, the whole expression collapses to `e` on the spot — failure short-circuits through every enclosing layer, because every layer installed the *same* `e`. Otherwise the payload `x` is bound, and the rest of the fold `r` runs with the success continuation extended to `k . cons x` — "when the tail succeeds with `xs`, succeed with ``x `cons` xs``".

Because only the success continuation grows while the failure default is shared, no intermediate `Maybe` tag is ever materialized: the fold's value, handed the caller's `j`, is `j (x₁ `cons` … `cons` nil)` on all-success, and the untouched `e` the moment any element is `nothing`. (The definition is η-reduced past `j`: `sequence ms e` *is* the `(List a -> e) -> e` the fold builds.) This is monadic `mapM id` / applicative `sequenceA` for `Maybe`, encoded with nothing but continuations.

Worked trace on a two-element list (write `[m₁, m₂]` for ``m₁ `cons` (m₂ `cons` nil)``, and `step m r k = m e (\x -> r (k . cons x))`):

```text
sequence [m₁, m₂] e j
  = [m₁, m₂] step ($ nil) j                 -- fold at e-slot K := (List a -> e) -> e
  = step m₁ (step m₂ ($ nil)) j

Case m₁ = nothing:
  = nothing e (\x -> …) = const e (…) = e   -- short-circuit ⇒ the Nothing-default

Case m₁ = just a, m₂ = just b:
  = (just a) e (\x -> step m₂ ($ nil) (j . cons x))
  = step m₂ ($ nil) (j . cons a)            -- just a _ f = f a
  = (just b) e (\y -> ($ nil) ((j . cons a) . cons y))
  = ((j . cons a) . cons b) nil             -- just b _ f = f b, then ($ nil)
  = j (a `cons` (b `cons` nil))             -- ⇒ Just [a, b]
```

Eliminating the final `sequence [just a, just b]` at `(undefined, id)` (i.e. `fromJust`) yields ``a `cons` (b `cons` nil)``; the moment any element is `nothing`, the result is the `e` handed in — `nothing`-behavior — and `isNothing` returns `true`.

### Behavior summary

| function | `Nothing` branch | `Just x` branch | result type |
|---|---|---|---|
| `isNothing` | `true` | `false` | `Bool` |
| `fromJust` | `undefined` (throws) | `x` | `a` |
| `fromMaybe d` | `d` | `x` | `a` |
| `maybeToList` | `nil` | `x \`cons\` nil` | `List a` |
| `listToMaybe` | (on `nil`) `nothing` | (on `cons`) `just head` | `Maybe a` |
| `catMaybes` | drop | keep `x` | `List a` |
| `squashMaybe` | `nothing` | `x` (inner `Maybe`) | `Maybe a` |
| `sequence` | propagate `Nothing` | thread payload via CPS | `Maybe (List a)` |

---

## 7. Church Either

Where `Maybe a = ∀e. e -> (a -> e) -> e` carries one "failure" branch and one "success" branch, `Either` generalizes to two *value-carrying* branches:

```haskell
type Either a b = ∀e. (a -> e) -> (b -> e) -> e
```

An `Either a b` is precisely its own eliminator: a value that, given a left-handler `a -> e` and a right-handler `b -> e`, dispatches to exactly one of them and returns the common result type `e`. This is the textbook Böhm–Berarducci encoding of the binary sum `a + b` — the two injections are the two constructors, and the encoded value *is* `Prelude.either`'s caller waiting for its two function arguments. The rank-N `∀e.` (living inside the synonym thanks to `ImpredicativeTypes`) is what lets a single encoded `Either` be eliminated at any answer type: at `Bool` in `isLeft`, at `a` in `fromLeft`, at `Maybe (Either a b)` in `maybeEither`. Throughout this section, reading `e onLeft onRight` as "case-analyze `e`" is the right mental model — there is no pattern match anywhere because the value already contains the dispatch.

### 7.1 Injections: `left`, `right`

```haskell
left ∷ a -> Either a _
left x l _ = l x

right ∷ b -> Either _ b
right x _ r = r x
```

`left x` builds the encoded value `λl r. l x`: it ignores the right-handler and feeds `x` to the left-handler. `right x` is the mirror image, `λl r. r x`. The wildcards `_` in the signatures are `PartialTypeSignatures` standing for the unconstrained other component — `left x :: Either a b` for *any* `b`, exactly as `Left :: a -> Either a b` is polymorphic in `b`.

Both are the two halves of the introduction form; every other function in this section is some elimination applied on top of them.

```text
left x  l r
  = (λx l _. l x) x l r
  → l x            -- right-handler r discarded
```

The test suite pins these injections down directly. With `e1 = left (5 :: Int)` and `e2 = right (10 :: Int)`:

```haskell
assertEqual "left (applied)"  "5"  (e1 show (show . negate))
assertEqual "right (applied)" "10" (e2 (show . negate) show)
```

`e1 show (show . negate)` runs the left-handler `show` on `5`, never touching `show . negate`, giving `"5"`; symmetrically `e2 (show . negate) show` selects the right-handler.

### 7.2 Discriminators: `isLeft`, `isRight`

```haskell
isLeft ∷ Either _ _ -> Bool
isLeft = either (const true) (const false)

isRight ∷ Either _ _ -> Bool
isRight = not . isLeft
```

`isLeft` eliminates `e` at the answer type `Bool` (= `∀e. e -> e -> e`, see §3) via the general `either` of §7.4, discarding the carried value with `const` on each side and returning a Church boolean that records *which* branch fired: `true` from the left-handler, `false` from the right. `isRight` doesn't bother swapping the handlers — it is simply the negation `not . isLeft`. Note the elimination happens at a *polytype* (`Bool` is itself a `∀`), one of the places `ImpredicativeTypes` is load-bearing.

Worked trace for `isRight (right y)`:

```text
isRight (right y)
  = not (isLeft (right y))
  = not ((right y) (const true) (const false))   -- either f g e = e f g
  = not (((λx _ r. r x) y) (const true) (const false))
  → not ((const false) y)
  → not false
  → true
```

So `isRight (right y)` evaluates to the Church boolean `true = const`, ready to be used as a two-way selector with no `if`-then-`else` in sight (the file forbids `if`-then-`else`; a `Bool` *is* the conditional). The tests cross-check all four combinations:

```haskell
assertEqual "isLeft left"  Prelude'.True  (toPreludeBool $ isLeft e1)   -- e1 = left 5
assertEqual "isLeft right" Prelude'.False (toPreludeBool $ isLeft e2)   -- e2 = right 10
assertEqual "isRight left" Prelude'.False (toPreludeBool $ isRight e1)
assertEqual "isRight right" Prelude'.True (toPreludeBool $ isRight e2)
```

### 7.3 Projections: `fromLeft`, `fromRight`

```haskell
fromLeft ∷ Either a _ -> a
fromLeft e = e id undefined

fromRight ∷ Either _ b -> b
fromRight e = e undefined id
```

`fromLeft` eliminates at answer type `a`: the left-handler is `id` (return the carried value untouched), the right-handler is `undefined` *itself* — ⊥ inhabits the handler type `_ -> a` as readily as anything else, so a right value has no `a` to offer and forcing the result diverges. `error`/`undefined` are explicitly permitted by the house rules precisely because they introduce no ADT. `fromRight` is the dual.

```text
fromLeft (left x)
  = (left x) id undefined
  = ((λx l _. l x) x) id undefined
  → (λl _. l x) id undefined
  → id x
  → x
```

So `fromLeft (left x) = x`, and dually `fromRight (right y) = y`; applying either to the wrong constructor forces `undefined`. The tests exercise both the safe paths and the expected failure paths:

```haskell
assertEqual "fromLeft"  5  (fromLeft e1)   -- e1 = left  (5  :: Int)
assertEqual "fromRight" 10 (fromRight e2)  -- e2 = right (10 :: Int)
```

### 7.4 The general eliminator: `either`

```haskell
either ∷ (a -> c) -> _ -> Either a b -> c
either f g e = e f g
```

This is the encoding's reason for being: `either f g e` is *literally* `e f g`. Because the encoded value already is "the thing waiting for two continuations," the case-analysis combinator is the identity rearrangement of arguments. Every other function above (`isLeft`, `fromLeft`, …) is a specialization of this one with the handlers chosen and the answer type `c` instantiated.

```text
either f g (left x)
  = (left x) f g
  = ((λx l _. l x) x) f g
  → (λl _. l x) f g
  → f x
```

and symmetrically `either f g (right y) → g y`. The test suite uses heterogeneous handlers, confirming the two branches land in a common result type `Int`:

```haskell
e1 = left "error"; e2 = right 42
assertEqual "either (left)"  5  (either Prelude'.length (*2) e1)  -- length "error" = 5
assertEqual "either (right)" 84 (either (Prelude'.length :: String -> Int) (*2) e2)  -- 42*2
```

For `e1 = left "error"`: `either length (*2) e1 → length "error" = 5`. For `e2 = right 42`: `either length (*2) e2 → 42 * 2 = 84`.

### 7.5 The two natural transformations: `maybeEither`, `eitherMaybe`

These are the interesting pair — the two canonical ways to commute `Maybe` past `Either` (recall `Maybe a = ∀e. e -> (a -> e) -> e`, eliminated as `m onNothing onJust`).

```haskell
-- | Transforms Either (Maybe a) (Maybe b) to Maybe (Either a b)
maybeEither :: Either (Maybe a) (Maybe b) -> Maybe (Either a b)
maybeEither e = e
  (\ma -> ma nothing (just . left))
  (\mb -> mb nothing (just . right))
```

`maybeEither` eliminates the outer `Either` at answer type `Maybe (Either a b)`. The left-handler receives a `Maybe a` and eliminates *it*: a `Nothing` propagates to `nothing`, a `Just x` becomes `just (left x)`. The right-handler is the mirror, producing `just (right y)`. So it pushes an outer-sum decision inward and absorbs an absent payload into the outer `Maybe`. There are four input shapes; the encoding handles each by two nested eliminations:

```text
maybeEither (left (just x))
  = (left (just x)) (\ma -> ma nothing (just . left)) (\mb -> …)
  → (\ma -> ma nothing (just . left)) (just x)        -- left selects its handler
  → (just x) nothing (just . left)
  → (just . left) x                                   -- just selects its (a->e) handler
  → just (left x)

maybeEither (left nothing)
  → (\ma -> ma nothing (just . left)) nothing
  → nothing nothing (just . left)
  → nothing                                           -- Nothing selects the onNothing arg
```

Behavior summary: `left (just x) ↦ just (left x)`, `right (just y) ↦ just (right y)`, and both `left nothing` and `right nothing ↦ nothing`. Note this transformation is *lossy* on the empty cases — `left nothing` and `right nothing` collapse to the same `nothing`, so `maybeEither` is not injective (it cannot be: `Maybe(Either a b)` has one "empty" inhabitant while `Either(Maybe a)(Maybe b)` has two).

```haskell
-- | Transforms Maybe (Either a b) to Either (Maybe a) (Maybe b)
eitherMaybe :: Maybe (Either a b) -> Either (Maybe a) (Maybe b)
eitherMaybe m = m (left nothing) (either (left . just) (\y -> right (just y)))
```

`eitherMaybe` goes the other way and is *total and injective*. It eliminates the outer `Maybe` at answer type `Either (Maybe a) (Maybe b)`: a `Nothing` maps to the chosen canonical witness `left nothing`, and a `Just e` is handed to the just-branch — call it `onJust` for the traces below — which re-eliminates the inner `Either`, re-wrapping each carried value in `just` while preserving the side. That inline branch, `either (left . just) (\y -> right (just y))`, wears its history on its sleeve: the left half is point-free, but the compile-checked sweep confirmed the right half must stay η-expanded — spelling it `right . just` trips GHC 9.12's stricter impredicative instantiation. (Appendix A's original fix hoisted this branch into a named, signature-annotated `onJust` helper; a later simplification pass re-inlined it, and the η-expanded right half is what remains of the patch.)

```text
eitherMaybe (just (right y))
  = (just (right y)) (left nothing) onJust
  → onJust (right y)                                  -- Just selects onJust
  = (right y) (left . just) (\y -> right (just y))
  → (\y -> right (just y)) y                          -- right selects its handler
  → right (just y)

eitherMaybe nothing
  = nothing (left nothing) onJust
  → left nothing                                      -- Nothing selects the onNothing arg
```

Behavior: `nothing ↦ left nothing`, `just (left x) ↦ left (just x)`, `just (right y) ↦ right (just y)`. Composing the round trip `maybeEither . eitherMaybe` is the identity on `Maybe (Either a b)`, but `eitherMaybe . maybeEither` is *not* — it sends the distinct `right nothing` to `left nothing` — confirming `eitherMaybe` is a section and `maybeEither` a retraction of a non-isomorphism.

### 7.6 Filtering lists of `Either`: `lefts`, `rights`, `partitionEithers`

These three consume a `List (Either a b)` (recall `List a = ∀e. (a -> e -> e) -> e -> e`, its own right fold). `lefts` and `rights` are `concatMap` one-liners; `partitionEithers` packages both of them into a Church `Pair`.

```haskell
lefts ∷ List (Either a b) -> List a
lefts = concatMap (either singleton (const nil))

rights ∷ List (Either a b) -> List b
rights = concatMap (either (const nil) singleton)
```

Each element is sent to a zero-or-one-element list, and `concatMap` splices the results. In `lefts`, the mapped function `either singleton (const nil)` — the general eliminator of §7.4, partially applied — turns `left x` into the singleton `[x]` and `right y` into `nil`, which vanishes in the concatenation; `rights` swaps the two handlers. This is precisely the recipe `catMaybes = concatMap maybeToList` uses in §6, with `either … …` playing the role of the sum-to-list conversion. It also needs no inference help: `either singleton (const nil)` arrives with its `Either a b -> List a` type on its sleeve, so `concatMap`'s impredicative instantiation at the polytype element `Either a b` is *supplied* rather than guessed — the shape Quick Look has always accepted (see Appendix A; an earlier `foldr`-with-named-`step` formulation of this pair was one of the 9.12 patch sites). Worked trace on `lefts [left x, right y]` (applying the result to its own `c`/`n`):

```text
lefts (cons (left x) (cons (right y) nil)) c n
  -- concatMap f xs c = xs (\e -> f e c), with f = either singleton (const nil)
  = f (left x) c (f (right y) c n)                 -- the list replays its spine
  -- inner: f (right y) = (right y) singleton (const nil) = const nil y = nil ; nil c n = n
  = f (left x) c n
  -- f (left x) = (left x) singleton (const nil) = singleton x ; singleton x c n = c x n
  = c x n                                          -- the singleton list [x]
```

So `lefts` keeps only left payloads, `rights` only right payloads, each preserving order.

```haskell
partitionEithers ∷ List (Either a b) ->  List a `Pair` List b
partitionEithers = liftA2 pair lefts rights
```

`partitionEithers` is the sum-side sibling of `unzip`, in the same doubled-traversal style: the reader-applicative `liftA2 pair lefts rights` β-reduces to ``partitionEithers xs = lefts xs `pair` rights xs`` — the two filters just documented, packaged into a Church `Pair` (recall `Pair a b = ∀e. (a -> b -> e) -> e`). As with `span` and `unzip` (§4–§5), the two passes are independent and lazy: a consumer that eliminates the pair and forces only one side pays for only that traversal. Worked trace:

```text
partitionEithers (cons (left a) (cons (right b) nil))
  = lefts [left a, right b] `pair` rights [left a, right b]
  -- lefts keeps left payloads, rights keeps right payloads (traces above)
  = (a `cons` nil) `pair` (b `cons` nil)   -- ([a], [b])
```

The result is the pair `([a], [b])`, i.e. left payloads in left order on the left, right payloads on the right. (All three are pinned directly — the `"lefts"`, `"rights"`, and `"partitionEithers"` assertions in `test/Spec.hs` — atop `eitherTests`/`eitherOpTests`' coverage of the underlying `left`/`right`/`isLeft`/`isRight`/`fromLeft`/`fromRight`/`either` machinery.)

---

## Appendix A — GHC compatibility: why it broke after 9.8, and the fixes

`Church.hs` was originally developed against **GHC 9.8.1**. On the repository's current
toolchain (**GHC 9.12.4**) the *unmodified* file failed to type-check, with ~14 errors
clustered in a handful of definitions. This appendix records the investigation and the
minimal, behavior-preserving fixes that now make the whole project compile **and** pass its
test suite. The repository is now a **Cabal project pinned to a single GHC (9.12.4)** — the
everyday workflow is `cabal build` / `cabal test`. The patched sources were, at the time of
the migration, portable across 9.8.1, 9.10.3, and 9.12.4 with no compiler flags, as the
matrix below records; only 9.12.4 is validated for the current post-golf source.

### A.1 The compatibility matrix

Measured directly (`ghc -fno-code`), error counts for the *original* vs. *patched* sources — a historical snapshot taken before the later simplification and terseness passes; the current source is validated on 9.12.4 only:

| Source | GHC 9.8.1 | GHC 9.10.3 | GHC 9.12.4 |
|--------|:---------:|:----------:|:----------:|
| **`Church.hs` (original)** | ✅ 0 *(needs `-XDeepSubsumption`)* | ✗ broken under any flag | ✗ ~14 errors |
| **`Church/Algorithms.hs` (original)** | ✅ 0 *(needs `-XDeepSubsumption`)* | ✗ | ✗ ~31 errors |
| **`Church.hs` / `Church/Algorithms.hs` (patched)** | ✅ 0 | ✅ 0 | ✅ 0 |
| **HUnit test suite (current)** | — | — | ✅ 403/403 PASS |

Two facts fall out immediately. First, the original code never compiled on 9.8.1 with the
file's *stated* extensions alone: it relied on **`DeepSubsumption`** (supplied externally,
e.g. a cabal `default-extensions:` stanza), because that extension is not in the file's
`LANGUAGE` pragma. Second, there is **no flag** that rescues the original on 9.10.3 or 9.12.4
— `DeepSubsumption` fixes one definition while breaking three others, so a source change is
unavoidable. The patched version removes the `DeepSubsumption` dependency entirely and is
portable across all three releases.

### A.2 Root cause: two GHC changes, accumulating

Two independent tightenings of GHC's type system, landing in different releases, are
responsible.

**(1) Deep subsumption was removed from the default in GHC 9.0** (the simplified-subsumption
change of [GHC proposal #287](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.md)).
Eta-reduced, point-free code at rank-N stops type-checking unless the arrows line up exactly,
because GHC no longer silently coerces `σ → (∀a. τ)` to `∀a. σ → τ` under an arrow. In this
file the casualty is `composeFlatten`, whose body `f x . flip g` composes Church lists
(themselves `∀`-types) point-free. It needed `-XDeepSubsumption` even on 9.8.1.

**(2) Quick Look impredicativity got stricter across 9.10 and 9.12.** The Quick Look
algorithm ([Serrano, Hage, Peyton Jones, Vytiniotis, *A quick look at impredicativity*, ICFP
2020](https://doi.org/10.1145/3409006)) decides where a polymorphic type may be *guessed* as
the instantiation of a type variable. Post-9.8 it became progressively more conservative
about instantiating a function's type variable at a **polytype** that has to be *decomposed
out of an argument's type* rather than supplied directly. The signature error is
**`GHC-91028: Cannot instantiate unification variable … with a type involving polytypes`**.

The distinction that decides whether a given call survives is subtle but consistent:

- **Survives:** when the polytype is *handed over* by an argument's own signature.
  `concat = foldr append nil` instantiates `foldr`'s element variable at the polytype
  `List a`, but `append :: List a → List a → List a` *supplies* that type, so Quick Look
  succeeds.
- **Fails (9.12):** when the polytype must be *extracted by unifying* a generic function's
  type against an argument. `null' = null` with `null' :: List (List a) → Bool` asks GHC to
  set `null`'s element variable to `List a` purely by matching `List elt ~ List (List a)`,
  with nothing supplying `List a` directly. 9.8 did this; 9.12 refuses it — *even with an
  explicit `null :: List (List a) → Bool` annotation*. Only a **visible type application**
  `null @(List a)` forces it.

The same root cause explains both families of failures: point-free aliases that monomorphize
a generic list/dictionary function at a polytype-bearing element (`null`, `uncons`, `lookup`,
`foldr`, `filter`) and point-free compositions over such types (`transpose . reverse`).

### A.3 The fixes (Church.hs)

Six definitions changed. Five are **purely type-level** (annotations, named helpers,
eta-expansion, visible type application) and therefore behavior-preserving by construction;
one (`composeFlatten`) is a small structural rewrite whose equivalence was verified by
running both versions. All six were confirmed identical in behavior by compiling the original
(9.8.1 + `DeepSubsumption`) and the patched source and diffing their output, and by the full
86-case suite passing.

| Definition | Symptom | Fix | Kind |
|-----------|---------|-----|------|
| `span` | Quick Look couldn't instantiate the inner `Pair` eliminator through `$` | annotate the rank-2 binder `\(pr ∷ a `Pair` List a) -> …`; give the `go` helper a real `∀r.` result type; replace `$` with parentheses | type-level |
| `lefts`, `rights` | `foldr` instantiated at the polytype element `Either a b` via an inline lambda | move the inline lambda to a **named, signature-annotated `step`** helper (the idiom the rest of the file already uses, e.g. `catMaybes`) | type-level |
| `eitherMaybe` | result polytype `Either (Maybe a) (Maybe b)` left a left-slot unification variable that had to become `Maybe a` | a named `onJust` helper with a full signature pins both slots; `left . just` eta-expanded to `\x -> left (just x)` | type-level |
| `transpose` | point-free `null' = null` / `uncons' = uncons` over `List (List a)` (polytype element) | **visible type application** `null @(List a)`, `uncons @(List a)`; rank-2 binders annotated; `$` → parens | type-level |
| `composeFlatten` | `f x . flip g` needs `DeepSubsumption` (impredicative point-free composition) | rewrite as `concatMap g (f x)` via a named `step` — the same left-to-right Kleisli composition, with no deep subsumption | **structural (verified equivalent)** |

`TypeApplications` was added to the `LANGUAGE` pragma (at the time used only by `transpose`; the supersession note below lists today's three uses).

**Partially superseded by later simplification and terseness passes.** The table above
records the 9.8→9.12 migration as it happened; a subsequent cleanup rewrote several of the
patched definitions into simpler forms that sidestep the fragile inference outright, so
some of the named helpers no longer exist in the source. Specifically: `span` is now the
doubled reader-`liftA2` over `takeWhile`/`dropWhile`, `lefts`/`rights` are
`concatMap`/`either` one-liners, `catMaybes` (the table's exemplar of the named-`step`
idiom) became `concatMap maybeToList` and is today the further-condensed `mapMaybe id`,
and `transpose` uses the classic `map head`/`map tail` column recursion. A code-golf sweep
then removed, compile-checked, every type application and signature `forall` GHC does not
require; the survivors in the foundations sections (0–7) are exactly three — `null @(List a)` (`transpose`),
`scanr @a @(List a)` (`tails`), and `foldr @(a -> List a)` (`composeFlatten`). The same
sweep re-inlined `eitherMaybe`'s `onJust` helper (its η-expanded right branch
`\y -> right (just y)` is what remains of that patch), compacted `composeFlatten`'s step
to `\f g -> concatMap g . f`, and replaced the `import Prelude hiding (…)` blocklist with
a positive 16-name import.

`Church/Algorithms.hs` — at migration time a separate module, since merged into `Church.hs`
as its sections 8–24 — needed the **same** three patterns
(it was not part of the original documentation task but was fixed so the suite runs):
point-free monomorphizing aliases (`uncons'`/`lookup'`/`foldr'`)
became visible type applications — each helper given its own `∀` so its variables scope into
the `@…` — `insertOrUpdate` calls at polytype values were given explicit `@…`, `deleteKey`'s
`filter $ …` became `filter @(k `Pair` v) (…)` (later passes moved and then removed that
annotation — today `deleteKey` delegates via `keySelect` to `filterWithKey`, which needs
no `@` at all), and `turn`/`unturn` were eta-expanded with `reverse @(List a)`, which they
retain. The golf sweep pruned the annotations of these algorithm sections (now 8–24 of the
merged `Church.hs`) too; the applications still present there
(e.g. `map @(k `Pair` v)` in `keys`/`values`, `caseList @_ @(k `Pair` v)` in `insertWith`,
`foldr @_ @(List _ `Pair` List _)` in `mergesort`) are again exactly the ones inference
needs.

### A.4 Why these fixes, and not a flag

`DeepSubsumption` is necessary for the *original* `composeFlatten` but actively harmful to the
N-ary combinators (`zipWithN`, `scanlN`, `mapAccumLN`): turning it on flips those from
compiling to failing on 9.10/9.12. So no single global flag compiles the original file on the
current toolchain. The chosen fixes instead:

- rephrase the one deep-subsumption-dependent definition (`composeFlatten`) so the dependency
  disappears;
- replace point-free monomorphizing aliases with the explicit instantiation GHC now demands
  (`@…`) or with the named-`step` idiom the file already favors;
- add only one extension (`TypeApplications`), which is the language's sanctioned mechanism
  for exactly this kind of impredicative instantiation.

The result was strictly *more* portable than the original (no flags, compiling — as then
measured — on 9.8 through 9.12) while remaining faithful to the file's style and identical
in behavior.

### A.5 Reproducing the verification

```text
# build + type-check the library on the pinned GHC 9.12.4:
cabal build all            # 0 errors

# full HUnit suite via Cabal:
cabal test
# => Cases: 403  Tried: 403  Errors: 0  Failures: 0   PASS
```

The library and tests build and pass on the pinned **GHC 9.12.4** under Cabal. For
historical reference only, the pre-golf sources were also observed to type-check without
flags on GHC 9.8.1 and 9.10.3 (an observation that predates the terseness pass and has not
been re-run). Those releases are not supported; the repository's package metadata and
validation policy target only GHC 9.12.4.

---

## Appendix B — Encoding tricks and design notes

The recurring ideas that make this style work, collected for reference. Each is developed in
full in the chapter cited.

**1. A value *is* its eliminator; consumption is application.** Reading any definition reduces
to one question: *which eliminator is applied to which continuations?* `fst = uncurry const`
— i.e. `fst p = p const` — applies the pair `p` to the 2-argument continuation `const` (= keep
the first). `not b = b false true` applies the Bool `b` to its two branches, swapped. There is
never a `case`. (§1–§2)

**2. The same λ-term denotes different things at different types.** `const` is simultaneously
`true`, the pair-eliminator "first projection", and `nothing`-ish constants; `const id` is
`false`, `nil`, and the "second projection". This is not a coincidence — it is parametricity:
`∀e. e -> e -> e` and `∀e. (a -> e -> e) -> e -> e` share the "ignore one argument" inhabitants.
(§2, §3, §6)

**3. A Church `Bool` *is* an `if`-`then`-`else`.** With `if`-`then`-`else` banned by the house
rules, every branch is `cond then else`. The idiom appears most strikingly where a predicate's
result is applied directly: `null xs z (f (head xs) (tail xs))` — packaged once and for all as
the one-step eliminator `caseList` (§3) — `leInt n 0 nil (x `cons` …)` in `replicate`,
`p x (keep) (drop)` inside `filter`/`takeWhile`. The Bool is non-strict in the *unselected*
branch exactly as `if` is — which is also what makes `caseList`'s unconditional `head`/`tail`
thunks safe. (§1, §3, §4)

**4. The `foldl`-as-`foldr` CPS trick — the cleverest idea in the file.** A list is *only* a
right fold, yet `foldl` must associate left. The resolution: fold *to a function* and apply it
at the end. `foldl f z xs = xs (flip (.) . flip f) id z` — the step unfolds to
`\x g acc -> g (f acc x)` — builds, right-to-left, a chain of deferred continuations `g`, each
waiting for the accumulator; seeding with `id` and feeding `z` runs them left-to-right. This sets the answer type `e := b -> b` (an impredicative
instantiation). `reverse`, `last`, `foldl1`, `mapAccumL`, and `cartesianN` all ride on it. (§4)

**5. The one-step-delay `tail`.** `tail` cannot "drop the head" directly — a right fold sees
the head first. It threads a Church-`Bool` "skip flag" so that the cons step emits its element
only *after* the first step has passed, effectively delaying the stream by one and discarding
the head. Read the trace in §3 slowly; it is the subtlest single definition. (§3)

**6. `cons` prepends *inside* the fold.** `cons x xs c = c x . xs c`: given a consumer `c`, the
new list runs `c x` and then the old list's action, composed. `map f xs c = xs (c . f)` is the
same move — push the operation through the fold by composing on the consumer — and
`append xs = xs cons` is its degenerate limit: replay one list with the *genuine* `cons`,
landing on the other. (§3, §4)

**7. Partiality is `undefined`/`error`, the only ⊥ available.** `head xs = xs const undefined`
returns the head, or `undefined` on `nil`; `fromJust`, `foldl1`/`foldr1` on empty, and the
out-of-bounds arms of `at` use the same device. These are the encoding's partial
eliminators; they are total functions returning ⊥, not ADT-level errors. (§3, §4, §6)

**8. `Tuple`, `Dict`, `Matrix` are documentation-only.** All three are *type synonyms for
`List`* (or `List` of `Pair`/`List`). The type system cannot enforce "all the same length" for
`Tuple` or "rectangular" for `Matrix`; the aliases announce intent and let the N-ary
combinators share one implementation. (§1, §5)

**9. The parallel N-ary pattern.** `zipWithN`, `scanlN`, `mapAccumLN` all iterate a `Tuple` of
lists in lockstep by the same shape: stop when `any null` of the columns, else take `map head`
and recurse on `map tail`. It is `zipWith`/`scanl`/`mapAccumL` lifted from 2 lists to *n*. (§5)

**10. `composeFlatten` is Kleisli composition for the list monad.** `composeFlatten [f₁…fₙ] =
f₁ >=> f₂ >=> … >=> fₙ`, with `singleton` as `return`. Each step is `\f g -> concatMap g . f`
— bind in the nondeterminism monad. (§5; this is also the one definition rewritten for
GHC 9.12, see Appendix A.)

**11. The two natural transformations `Maybe ↔ Either`.** `maybeEither` and `eitherMaybe`
witness the (non-iso) maps between `Either (Maybe a) (Maybe b)` and `Maybe (Either a b)` — a
small but genuine bit of category theory expressed purely in eliminators. (§7)

**12. The impredicativity tax.** Nesting these synonyms (`List (Either a b)`, `Maybe (a `Pair`
List a)`, `Tuple (List a)`) instantiates type variables at polytypes. GHC's inference handles
*most* of it automatically, but a few spots need a hint — a binder annotation, a named helper,
or a visible type application. Where and why is the subject of [Appendix A](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes).

---

## Appendix C — Function index

Every top-level function in sections 0–7 of `Church.hs` — the foundations covered by Part I (chapters 1–7) — grouped by the file's own sections, with its type signature. In Church-encoded code the *type is the documentation*: a function's rank-N signature tells you exactly which eliminators it consumes and produces. Where the source spells an inferable higher-order argument as a `_` signature wildcard (the §1 sweep), the table gives the full elaborated type. Note that after the compile-checked annotation sweep only three signatures in these sections retain an explicit `∀` — `tails`, `composeFlatten`, and `transpose` — precisely the ones whose bodies use a visible type application that needs the quantified variable in scope. The algorithm sections 8–24 (sorts, `subsequences`/`permutations`, dictionary and matrix operations, the `<algorithm>`/`<map>`/`<numeric>` ports) are indexed in [Appendix D](#appendix-d--function-index-part-ii).

**0. Prerequisites**

| Function | Type |
|----------|------|
| `ltInt` | `Int -> Int -> Bool` |
| `gtInt` | `Int -> Int -> Bool` |
| `leInt` | `Int -> Int -> Bool` |
| `eqInt` | `Int -> Int -> Bool` |
| `geInt` | `Int -> Int -> Bool` |

**2. Aliases with Designated Semantics**

| Function | Type |
|----------|------|
| `eqFromLE` | `LE a -> Equal a` |
| `ltFromLE` | `LE a -> a -> a -> Bool` |

**3. Church-Encoded Booleans**

| Function | Type |
|----------|------|
| `true` | `Bool` |
| `false` | `Bool` |
| `not` | `Bool -> Bool` |
| `and` | `Bool -> Bool -> Bool` |
| `or` | `Bool -> Bool -> Bool` |
| `xor` | `Bool -> Bool -> Bool` |

**4. Church-Encoded Pairs**

| Function | Type |
|----------|------|
| `pair` | `a -> b -> a `Pair` b` |
| `fst` | `a `Pair` _ -> a` |
| `snd` | `_ `Pair` b -> b` |
| `swap` | `a `Pair` b -> b `Pair` a` |
| `curry` | `(a `Pair` b -> c) -> a -> b -> c` |
| `uncurry` | `(a -> b -> c) -> a `Pair` b -> c` |
| `pairToList` | `a `Pair` b -> List (Either a b)` |
| `bimapPair` | `(a -> c) -> (b -> d) -> a `Pair` b -> c `Pair` d` |

**5.1 Constructors and Basic Operations**

| Function | Type |
|----------|------|
| `nil` | `List a` |
| `cons` | `a -> List a -> List a` |
| `snoc` | `List a -> a -> List a` |
| `uncons` | `List a -> Maybe (a `Pair` List a)` |
| `unsnoc` | `List a -> Maybe (List a `Pair` a)` |
| `singleton` | `a -> List a` |
| `caseList` | `b -> (a -> List a -> b) -> List a -> b` |

**5.2 Sublist Access: head, tail, etc.**

| Function | Type |
|----------|------|
| `head` | `List a -> a` |
| `null` | `List a -> Bool` |
| `tail` | `List a -> List a` |
| `last` | `List a -> a` |
| `init` | `List a -> List a` |
| `at` | `Int -> List a -> a` |
| `any` | `(a -> Bool) -> List a -> Bool` |
| `all` | `(a -> Bool) -> List a -> Bool` |

**5.3 Folds, Scans, and General List Utilities**

| Function | Type |
|----------|------|
| `foldr` | `(a -> b -> b) -> b -> List a -> b` |
| `foldl` | `(b -> a -> b) -> b -> List a -> b` |
| `foldl1` | `(a -> a -> a) -> List a -> a` |
| `foldr1` | `(a -> a -> a) -> List a -> a` |
| `scanr` | `(a -> b -> b) -> b -> List a -> List b` |
| `mapAccumR` | `(s -> a -> s `Pair` b) -> s -> List a -> s `Pair` List b` |
| `scanl` | `(b -> a -> b) -> b -> List a -> List b` |
| `mapAccumL` | `(s -> a -> s `Pair` b) -> s -> List a -> s `Pair` (List b)` |
| `zipWith` | `(a₁ -> a₂ -> b) -> List a₁ -> List a₂ -> List b` |
| `zipWithN` | `(Tuple a -> b) -> Tuple (List a) -> List b` |
| `scanl2` | `(b -> a₁ -> a₂ -> b) -> b -> List a₁ -> List a₂ -> List b` |
| `scanlN` | `(b -> Tuple a -> b) -> b -> Tuple (List a) -> List b` |
| `mapAccumL2` | `(s -> a₁ -> a₂ -> s `Pair` b) -> s -> List a₁ -> List a₂ -> s `Pair` List b` |
| `mapAccumLN` | `(s -> Tuple a -> s `Pair` b) -> s -> Tuple (List a) -> s `Pair` List b` |
| `zip` | `List a -> List b -> List (a `Pair` b)` |
| `unzip` | `List (a `Pair` b) -> List a `Pair` List b` |
| `unfoldr` | `(b -> Maybe (a `Pair` b)) -> b -> List a` |
| `unfoldTree` | `(b -> a `Pair` List b) -> b -> List a` |
| `append` | `List a -> List a -> List a` |
| `appendEither` | `List a -> List b -> List (Either a b)` |
| `map` | `(a -> b) -> List a -> List b` |
| `concat` | `List (List a) -> List a` |
| `concatMap` | `(a -> List b) -> List a -> List b` |
| `filter` | `(a -> Bool) -> List a -> List a` |
| `length` | `List a -> Int` |
| `reverse` | `List a -> List a` |
| `take` | `Int -> List a -> List a` |
| `drop` | `Int -> List a -> List a` |
| `splitAt` | `Int -> List a -> List a `Pair` List a` |
| `takeLast` | `Int -> List a -> List a` |
| `dropLast` | `Int -> List a -> List a` |
| `takeWhile` | `(a -> Bool) -> List a -> List a` |
| `dropWhile` | `(a -> Bool) -> List a -> List a` |
| `partition` | `(a -> Bool) -> List a -> List a `Pair` List a` |
| `partitionEvery` | `Int -> List a -> List (List a)` |
| `partitionStep` | `Int -> Int -> List a -> List (List a)` |
| `windows` | `Int -> List a -> List (List a)` |
| `inits` | `List a -> List (List a)` |
| `tails` | `∀a. List a -> List (List a)` |
| `span` | `(a -> Bool) -> List a -> List a `Pair` List a` |
| `replicate` | `Int -> a -> List a` |
| `lexicographicLE` | `LE a -> LE (List a)` |
| `compose` | `List (a -> a) -> a -> a` |
| `composeFlatten` | `∀a. List (a -> List a) -> a -> List a` |
| `cartesianWith` | `(a -> b -> c) -> List a -> List b -> List c` |
| `cartesian` | `List a -> List b -> List (a `Pair` b)` |
| `cartesianN` | `Tuple (List a) -> List (Tuple a)` |
| `intersperse` | `a -> List a -> List a` |
| `riffle` | `a -> List a -> List a` |
| `rotateLeftN` | `Int -> List a -> List a` |
| `rotateRightN` | `Int -> List a -> List a` |
| `rotateLeft` | `List a -> List a` |
| `rotateRight` | `List a -> List a` |
| `transpose` | `∀a. Matrix a -> Matrix a` |

**6. Church-Encoded Maybe**

| Function | Type |
|----------|------|
| `just` | `a -> Maybe a` |
| `nothing` | `Maybe a` |
| `isNothing` | `Maybe a -> Bool` |
| `fromJust` | `Maybe a -> a` |
| `fromMaybe` | `a -> Maybe a -> a` |
| `maybeToList` | `Maybe a -> List a` |
| `listToMaybe` | `List a -> Maybe a` |
| `catMaybes` | `List (Maybe a) -> List a` |
| `mapMaybe` | `(a -> Maybe b) -> List a -> List b` |
| `squashMaybe` | `Maybe (Maybe a) -> Maybe a` |
| `sequence` | `List (Maybe a) -> Maybe (List a)` |

**7. Church-Encoded Either**

| Function | Type |
|----------|------|
| `left` | `a -> Either a _` |
| `right` | `b -> Either _ b` |
| `isLeft` | `Either _ _ -> Bool` |
| `isRight` | `Either _ _ -> Bool` |
| `fromLeft` | `Either a _ -> a` |
| `fromRight` | `Either _ b -> b` |
| `maybeEither` | `Either (Maybe a) (Maybe b) -> Maybe (Either a b)` |
| `eitherMaybe` | `Maybe (Either a b) -> Either (Maybe a) (Maybe b)` |
| `either` | `(a -> c) -> (b -> c) -> Either a b -> c` |
| `lefts` | `List (Either a b) -> List a` |
| `rights` | `List (Either a b) -> List b` |
| `partitionEithers` | `List (Either a b) -> List a `Pair` List b` |

---

# Part II — Dictionaries, Sorting, Selection, Search, and Numeric Scans

> The algorithms half of the library — sections 8–24 of the same
> [`src/Church.hs`](../src/Church.hs). Where
> [Part I](#the-one-idea) builds the five core types and the `Data.List`/`Maybe`/`Either`
> toolkit, this Part builds the *algorithms*: association-list **dictionaries**, **four
> comparison sorts** (merge, quick, heap, intro), two **selection** algorithms (nth-element /
> introselect), set-like operations, combinatorial generators (`subsequences`, `permutations`),
> projection-based combinators, `<numeric>` folds/scans, and the longest-common-substructure
> family — all still without a single algebraic data type,
> without `if`-`then`-`else`, and using only `Int` plus `undefined`/`error`.

| | |
|---|---|
| **Source** | sections 8–24 of [`src/Church.hs`](../src/Church.hs) (Part I documents sections 0–7) |
| **Builds on** | the encoding foundations of [Part I](#the-one-idea) — same file, same toolchain and suite status as Part I's header table |
| **Tests** | [`test/Spec.hs`](../test/Spec.hs) — the dict/sort/selection/subsequence/`<algorithm>` cases; [`API-PARITY.md`](../../API-PARITY.md) for the cross-project audit |
| **GHC-compat patches** | a few definitions here were patched for GHC 9.12; the full story is in [Appendix A](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes) |

---

## What Part II adds, and the vocabulary it assumes

This Part assumes the encoding is already understood — if `type List a = ∀e. (a -> e -> e)
-> e -> e` (a list *is* its own right fold) and "branching is a Church `Bool` applied to two
continuations" are not yet second nature, read **[Part I](#the-one-idea)** first. Here we
only note the synonyms these chapters lean on, all defined in section 2 of the source as
*documentation-only* aliases:

```haskell
type Dict k v = List (k `Pair` v)   -- association list: unordered, unique keys (under a supplied Equal)
type Matrix a = List (List a)       -- rectangular, row-major
type Equal a  = a -> a -> Bool      -- equality relation (returns a Church Bool)
type LE a     = a -> a -> Bool      -- ≤ relation (same shape as Equal; intent: ordering)
```

Two idioms recur throughout and are worth naming up front:

- **Relations are passed in, not assumed.** Every operation that needs equality or ordering
  takes an explicit `Equal`/`LE` argument (the encoding has no type classes to dispatch on).
  Many are further *generalized to a correspondence relation* `eq :: a -> b -> Bool` so they can
  relate two *different* element types (e.g. `lookup`, `isPrefixOf`, `elemBy`).
- **Control flow is the returned Church `Bool`.** A comparison `le x y` or `eq a b` returns a
  Church Boolean, which is then applied directly to the two branches: `le x y (thenBranch)
  (elseBranch)`. There is no `if`. Watch for this in every sort's partition step.

---

## How to read Part II

- **§8 Dictionaries** — the association-list operations (`lookup`, `insertOrUpdate`, `invert`,
  `merge`, `collapse`, relational `chain`, …), almost all expressed either as folds that
  consult/update an accumulator dictionary or as one-line instances of shared primitives
  (`insertWith`, `keySelect`).
- **§9 Matrix rotations & set-like ops** — `turn`/`unturn`, and `nubBy`/`unionBy`/`groupBy`/`pick`.
- **§10 Subsequences, permutations & search** — the combinatorial generators and the
  prefix/suffix/substring/subsequence predicates.
- **§11 Sorting** — the centerpiece: **mergesort, quicksort, heapsort, introsort**, each a
  complete comparison sort written in pure eliminators.
- **§12 Selection & LCS** — `nthElement`, the introselect `nthElement'`, and the
  longest-common-prefix/suffix/sublist family.
- **§13 C++ `<algorithm>` equivalents** — non-modifying searches, modifying operations,
  partition/sort predicates, binary search, sorted-range merge/set operations,
  min/max/comparison, and permutation operations.
- **§14 `<map>`-style dictionary operations** — the `std::map`/`Data.Map` relation surface:
  query and construction, insertion and update, filtering/combining/folding.
- **§15 Projection, map & numeric extensions** — `on`-based list operations, keyed `Data.Map`
  combinators, and the generic `<numeric>` reduction/scan family.
- **Appendix D** — a function index for Part II (sections 8–24 of the source).

Every reduction trace was re-derived by hand and cross-checked against `test/Spec.hs`.

---

## Table of contents

8. [Dictionary operations](#8-dictionary-operations)
9. [Matrix rotations and set-like list operations](#9-matrix-rotations-and-set-like-list-operations)
10. [Subsequences, permutations, and search/predicate combinators](#10-subsequences-permutations-and-searchpredicate-combinators)
11. [Four sorting algorithms](#11-four-sorting-algorithms)
12. [Selection algorithms and longest-common-substructure](#12-selection-algorithms-and-longest-common-substructure)
13. [C++ `<algorithm>` equivalents](#13-c-algorithm-equivalents)
14. [`<map>`-style dictionary operations](#14-map-style-dictionary-operations)
15. [Projection, keyed map, and numeric extensions](#15-projection-keyed-map-and-numeric-extensions)
- [Appendix D — Function index (Part II)](#appendix-d--function-index-part-ii)

---

## 8. Dictionary operations

A `Dict k v = List (k `Pair` v)` is an unordered association list whose keys are unique under a caller-supplied `Equal k` (see [Part I](#the-one-idea) for the `List`, `Pair`, `Maybe`, `Either`, and `Equal` encodings; this section assumes them). Nothing in the type enforces uniqueness — it is an invariant the operations *maintain*. Almost every function here is one of two shapes:

- **A fold over an accumulator dict**, where the step upserts the current `(k,v)` pair into the accumulator — inserting a fresh entry or combining with the matching one via `insertOrUpdate`/`upsertWith`/`insertWith`. This is the workhorse for `invert` and `merge` (via `fromListWith`); `mapMaybeValues` and `chain` are per-entry `Maybe`-eliminating maps over one dict, and `collapse` is a pure `concatMap` flatten.
- **A one-line instantiation of a deeper primitive.** `lookup` is itself a lazy `foldr` (the leftmost match wins because the winning arm discards the unforced tail), and `insertOrUpdate`/`upsertWith` are both instances of §14's `insertWith` — the *one* remaining direct structural recursion (a `caseList` one-step elimination plus `Pair` elimination) that the whole upsert family rests on. The key-filter family chains the same way: `deleteKey`/`keyTake`/`keyDrop` → `keySelect` → `filterWithKey` → `Church.filter`.

Two encoding facts drive all the control flow, neither using `if`-`then`-`else` or pattern matching:

1. **`lookup` returns a Church `Maybe`**, which *is* its own eliminator: `m nothingBranch justBranch`. Applying the result to two continuations *is* the case split. No `isNothing`/`fromJust` needed.
2. **Branching is a Church `Bool` applied to two arms.** `eq k key (thenArm) (elseArm)` selects an arm by virtue of `true = const`, `false = const id`. A pair `kv` is destructured by *applying* it to a binary continuation: `kv $ \k v -> …`, since `Pair a b = ∀e.(a->b->e)->e`.

One portability note up front: an automated, compile-checked sweep removed every `TypeApplication` and signature `∀` that GHC does not actually require, so the annotations that *survive* in this section are exactly the ones inference needs — e.g. `insertWith` (§14) pins its deconstructor inline as `caseList @_ @(k \`Pair\` v)`, and `invert` pins `upsertWith @b @(List a)` — each one letting GHC 9.12's stricter impredicative-instantiation pick the monomorphic instance the rank-N body needs; see **[Appendix A](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes)** for the full story. They are semantically identity-level annotations; ignore them when reading the algorithm. (Earlier revisions carried belt-and-braces helpers like `lookup'`/`foldr'` for the same purpose; the sweep proved them unnecessary and they are gone.)

### 8.1 `lookup` — the `Maybe`-returning search

```haskell
lookup :: (a -> c -> Bool) -> c -> Dict a b -> Maybe b
lookup eq key = listToMaybe . lookupAll eq key
```

The signature is deliberately *relaxed*: equality is a heterogeneous correspondence `a -> c -> Bool`, so the search key type `c` need not equal the stored key type `a`. The algorithm is a plain linear scan — but no explicit recursion needs to be written at all: it is the *multimap* query `lookupAll` (§14: `values . keySelect (flip eq key)` — filter the matching entries, project their values) capped with `listToMaybe` (`foldr (const . just) nothing`), which takes the head of the match list as a Church `Maybe`. Zero pattern matching; every decision is the per-entry `eq k key` Boolean consumed inside `filter`.

The **leftmost match still wins**, and it wins *lazily*: `filter` produces its survivors head-first, and `listToMaybe`'s `just`-arm discards the unforced rest — on a hit, no entry beyond the first match is ever examined. This is exactly the shape of `findIf` (§13) specialized to key-value pairs. (An earlier revision spelled the same scan as an explicit `uncons` recursion with a GHC-9.12 `uncons'` type-application helper; the pipeline absorbs both.)

**Trace** — `lookup eqI 2 [(1,"a"),(2,"b")]` with `eqI x y = x == y`:

```text
lookup eqI 2 [(1,"a"),(2,"b")]
  = listToMaybe (lookupAll eqI 2 [(1,"a"),(2,"b")])
  lookupAll: (1,"a"): eqI 1 2 = false → drop
             (2,"b"): eqI 2 2 = true  → keep   -- lazily: produced on demand
  = listToMaybe ["b", …]                        -- the … is never forced
= just "b"
```

`lookup eqI 9 [(1,"a")]` misses at every step, so the seed `nothing` bubbles all the way out. This `Maybe` result is exactly what the fold-based ops below switch on.

### 8.2 `insertOrUpdate` — the upsert (an `insertWith` instance)

```haskell
insertOrUpdate :: Equal a -> a -> b -> Dict a b -> Dict a b
insertOrUpdate = insertWith const
```

One line — and it is the line `insertWith`'s own documentation always promised. `insertWith f` (§14, where the full `caseList` recursion is quoted and traced) inserts `(key, value)` when the key is absent, and otherwise replaces the stored value `old` with `f value old`; instantiating `f = const` makes the replacement `const value old = value` — blindly overwrite with the new value. So the upsert is not a separate primitive but the `const` instance of the combine-on-collision primitive. The same is true of `upsertWith eq key new combine = insertWith (const combine) eq key new` — absent key ⇒ insert `new`, present key ⇒ `const combine new old = combine old` — so the whole upsert family is one recursion, instantiated twice. (An earlier revision duplicated `insertWith`'s twelve-line recursion here with the replace-arm hard-coded, and `upsertWith` made a `lookup` pass *followed by* an `insertOrUpdate` pass; the delegations say the same things in one traversal.)

Unfolding `insertWith const` for the traces below: linear recursion preserving the unique-key invariant. On the empty list, produce `singleton (key,value)`. Otherwise split off the head `kv`; if its key matches, *replace* it — `(key `pair` value) `cons` rest`, dropping the old `kv` and not recursing further (uniqueness guarantees at most one hit). If it does not match, *keep* `kv` and recurse into `rest`. Notice the replace-arm rebuilds with a fresh `pair key value` (so the value is overwritten even if the key representation differs), and the carry-on arm reuses the original `kv` verbatim.

**Trace** — update: `insertOrUpdate eqI 2 "v2" [(1,"a"),(2,"b"),(3,"c")]`:

```text
head (1,"a"): eqI 1 2 = false → keep (1,"a"), recurse on [(2,"b"),(3,"c")]
  head (2,"b"): eqI 2 2 = true → replace → (2,"v2") `cons` [(3,"c")]
                                          = [(2,"v2"),(3,"c")]
re-cons kept head:  (1,"a") `cons` [(2,"v2"),(3,"c")]
= [(1,"a"),(2,"v2"),(3,"c")]
```

**Trace** — insert new key: `insertOrUpdate eqI 4 "four" [(1,"one"),(2,"two"),(3,"three")]` walks all three mismatching heads and bottoms out at `nil`, yielding `singleton (4,"four")`, which the unwinding conses behind the kept heads ⇒ `[(1,"one"),(2,"two"),(3,"three"),(4,"four")]`.

The test suite `dictTests` (`test/Spec.hs:407`) pins all three behaviors exactly:

- insert new key (4,"four") ⇒ `[(1,"one"),(2,"two"),(3,"three"),(4,"four")]`
- update existing key 2 ⇒ `[(1,"one"),(2,"updated two"),(3,"three")]`
- insert into empty ⇒ `[(1,"one")]`

### 8.3 `invert` — group keys by value

```haskell
invert :: ∀a b. Equal b -> Dict a b -> Dict b (List a)
invert eq = foldrWithKey
  (\k v -> upsertWith @b @(List a) eq v (singleton k) (cons k)) nil
```

This is the canonical instance of the fold pattern. `foldrWithKey step nil` threads an accumulator dict `acc :: Dict b (List a)` (value ↦ list-of-keys). For each `(k,v)`, `step` is one `upsertWith` at the key `v` — a single traversal of the accumulator whose collision behavior splits exactly the old two ways:

- value unseen ⇒ insert a fresh group `(v, singleton k)`.
- value seen with group `oldKeys` ⇒ combine to `k `cons` oldKeys`, prepending `k` to the existing group.

Because `foldr` processes right-to-left, the *last* occurrence of a value seeds its group (so its key sits deepest), and earlier keys prepend — net effect, keys end up in original left-to-right order within each group, and groups appear in first-seen order.

**Trace** — `invert eqS [(1,"foo"),(2,"foo"),(3,"bar"),(4,"foo")]` (`foldr` ⇒ process rightmost first):

```text
acc₀ = []
(4,"foo"): upsertWith "foo": absent  → insert ("foo",[4])          → [("foo",[4])]
(3,"bar"): upsertWith "bar": absent  → insert ("bar",[3])          → [("foo",[4]),("bar",[3])]
(2,"foo"): upsertWith "foo": hit [4] → combine (cons 2) ⇒ [2,4]
                                         → [("foo",[2,4]),("bar",[3])]
(1,"foo"): upsertWith "foo": hit [2,4] → combine (cons 1) ⇒ [1,2,4]
                                         → [("foo",[1,2,4]),("bar",[3])]
= [("foo",[1,2,4]),("bar",[3])]
```

The `invert` test (`test/Spec.hs:431`) asserts exactly `[("foo",[1,2,4]),("bar",[3])]` on this input — confirming both the per-group key order and the first-seen group order described above.

### 8.4 `merge` — combine a list of dicts, gathering collisions

```haskell
merge :: Equal k -> List (Dict k v) -> Dict k (List v)
merge eq ds = fromListWith (flip append) eq (concatMap (mapValues singleton) ds)
```

A *doubly* folded version of the same idea, now deliberately left-to-right so key order is
the order of first occurrence across the input dictionaries. `concatMap (mapValues singleton)`
first flattens all the dicts into one long entry list with every value boxed as `[v]`; then
`fromListWith (flip append)` (§14's left fold of `insertWith`) either creates `k -> [v]`
or appends the new singleton to the existing value list (`flip append` applies as
`append old new`, so the stored list stays in front and the new value lands at the end). So `merge`
is "union of all dicts, with every key mapped to the list of all values it received."
Keys colliding across dicts (or within one) accumulate rather than overwrite — the
distinguishing choice versus a last-wins union.

**Trace** — `merge eqI [[(1,"a"),(2,"b")], [(1,"c")]]`:

```text
acc₀ = []
process [(1,"a"),(2,"b")]:
   (1,"a"): missing → [(1,["a"])]
   (2,"b"): missing → [(1,["a"]),(2,["b"])]
process [(1,"c")]:
   (1,"c"): present ["a"] → [(1,["a","c"]),(2,["b"])]
= [(1,["a","c"]),(2,["b"])]
```

### 8.5 `collapse` — flatten a dict-of-dicts into composite keys

```haskell
collapse :: ∀k₁ k₂ v. Equal k₁ -> Equal k₂ -> Dict k₁ (Dict k₂ v) -> Dict (k₁ `Pair` k₂) v
collapse _ _ = concatMap @(k₁ `Pair` Dict k₂ v) @((k₁ `Pair` k₂) `Pair` v)
  (\kv -> kv $ \k dict -> mapKeys (pair k) dict)
```

Also doubly structural, but *no* lookups — it is a pure `concatMap` flatten. Each outer entry `(k₁, innerDict)` is destructured by application (`kv $ \k dict -> …`) and replaced by `mapKeys (pair k) dict` — the inner dictionary with every key upgraded to the composite Church `Pair` `k₁ `pair` k₂` — and `concatMap` splices the rewritten inner dictionaries end to end. (The `eq₁`/`eq₂` parameters are part of the uniform dictionary-op signature; here the input invariant already guarantees distinct composite keys, so no dedup is performed. The `concatMap @… @…` pin fixes the impredicative entry types the rank-N context cannot.)

**Trace** — `collapse _ _ [(1, [(10,"a"),(11,"b")]), (2, [(20,"c")])]`:

```text
outer (1, [(10,"a"),(11,"b")]): mapKeys (pair 1) → [((1,10),"a"),((1,11),"b")]
outer (2, [(20,"c")]):          mapKeys (pair 2) → [((2,20),"c")]
concatMap splices the rewritten inner dicts in order:
= [((1,10),"a"),((1,11),"b"),((2,20),"c")]
```

### 8.6 `eitherDict` — tagged disjoint union of two dicts

```haskell
eitherDict :: Dict k₁ v -> Dict k₂ v -> Dict (Either k₁ k₂) v
eitherDict dict₁ dict₂ = append (mapKeys left dict₁) (mapKeys right dict₂)
```

The one op that is *not* a fold-with-lookup. It re-tags each key of `dict₁` with `left` and each key of `dict₂` with `right`, then `append`s — and each re-tagging is just `mapKeys` (itself `map (bimapPair f id)`: rewrite the first component of every entry, keep the second) instantiated at the key transforms `left`/`right`. (After the annotation sweep this needs no instantiation pins at all — `left` and `right` supply the key types themselves.) Because the two key universes are made provably disjoint by the `Either` tag, no collision handling is needed — uniqueness is automatic. Behaviorally: concatenation of `[(Left k, v)…]` and `[(Right k, v)…]`.

### 8.7 `chain` / `chainN` — relational composition of dicts

```haskell
chain :: Equal k₁ -> Equal k₂ -> Dict k₁ k₂ -> Dict k₂ v -> Dict k₁ v
chain _ eq₂ = flip (mapMaybeValues . flip (lookup eq₂))

chainN :: Equal a -> Dict a a -> List (Dict a a) -> Dict a a
chainN eq = foldr (chain eq eq)
```

`chain` composes two finite maps like relations: given `dict₁ : k₁ ↦ k₂` and `dict₂ : k₂ ↦ v`, produce `k₁ ↦ v` by `dict₂ ∘ dict₁`. Unflipped, the point-free body reads `chain _ eq₂ dict₁ dict₂ = mapMaybeValues (\k₂ -> lookup eq₂ k₂ dict₂) dict₁` — for each stored value `k₂` of `dict₁`, `lookup eq₂ k₂ dict₂` — and the returned `Maybe v` is the branch (`mapMaybeValues` eliminating it per entry):

- `nothing` arm: `k₂` absent from `dict₂` ⇒ drop the entry (composition is a *partial* map; unmatched links vanish).
- `just v` arm: keep the entry as `(k₁, v)`.

`chainN eq d ds = foldr (chain eq eq) d ds` folds `chain` across a whole list of dictionaries — repeated relational composition. Note the `foldr` seeds with `d` and the second list `ds`, so it composes right-to-left: `chainN eq d [m₁,m₂] = foldr (chain eq eq) d [m₁,m₂] = chain m₁ (chain m₂ d)`, i.e. it threads `d` as the innermost (final) lookup table.

**Trace** — `chain eqI eqS [(1,"x"),(2,"y")] [("x",100),("z",9)]`:

```text
(1,"x"): lookup "x" [("x",100),("z",9)] = just 100 → keep (1,100)
(2,"y"): lookup "y" …                   = nothing  → drop
= [(1,100)]
```

Entry `(2,"y")` is dropped because `"y"` has no image in `dict₂`; `(1,"x")` resolves to `100`.

### 8.8 `mapValues` / `mapMaybeValues` — value transforms

```haskell
mapValues :: (v₁ -> v₂) -> Dict k v₁ -> Dict k v₂
mapValues = map . bimapPair id

mapMaybeValues :: (v₁ -> Maybe v₂) -> Dict k v₁ -> Dict k v₂
mapMaybeValues = mapMaybeWithKey . const
```

`mapValues` is `fmap` over the values in a single `map`: `bimapPair id f` (Church's pair-bimap, `bimapPair f g p = p (\x y -> f x `pair` g y)`) rewrites the second component of every entry and passes the key through `id`. Its exact mirror is `mapKeys f = map (bimapPair f id)` — one combinator, two instantiations, covering both projections of the entry. `mapMaybeValues` cannot be a plain `map` (entries may *disappear*), so it routes through §14's `mapMaybeWithKey` (with the key ignored via `const`), itself one `mapMaybe` over the entries — a tidy use of the `Maybe`-as-eliminator idiom: `f v₁` returns a Church `Maybe v₂`, re-eliminated per entry as `f v₁ nothing (just . pair k)`. A `nothing` result simply *omits* the entry (it contributes `nil` to the splice); a `just v₂` keeps the key with the new value. No `catMaybes` pass needed — the filtering is fused into `mapMaybe`.

**Trace** — `mapMaybeValues f [(1,"a"),(2,""),(3,"c")]` with `f s = null s → nothing; just (length s)`:

```text
(1,"a"): f "a" = just 1  → contributes [(1,1)]
(2,""):  f ""  = nothing → contributes nil
(3,"c"): f "c" = just 1  → contributes [(3,1)]
mapMaybe splices the contributions in order:
= [(1,1),(3,1)]
```

### 8.9 `cartesianDict` — Cartesian product of two dicts

```haskell
cartesianDict :: ∀k₁ k₂ v₁ v₂. Dict k₁ v₁ -> Dict k₂ v₂ -> Dict (k₁ `Pair` k₂) (v₁ `Pair` v₂)
cartesianDict = cartesianWith @(k₁ `Pair` v₁) @(k₂ `Pair` v₂)
  @((k₁ `Pair` k₂) `Pair` (v₁ `Pair` v₂))
  (uncurry $ \k v -> bimapPair (pair k) (pair v))
```

Built directly on Church's `cartesianWith :: (a -> b -> c) -> List a -> List b -> List c` (`Church.hs`), which is just nested `foldr`/cons under the hood. For every pair of entries `(k₁,v₁)` from the first dict and `(k₂,v₂)` from the second, the pairing function *transposes the grouping*: keys collect into a composite key `(k₁,k₂)` and values into a composite value `(v₁,v₂)`. The result is a dict over composite keys; since input keys are unique per dict, every composite key `(k₁,k₂)` is distinct, so the uniqueness invariant holds with no dedup.

**Trace** — `cartesianDict [(1,"a")] [(10,True),(20,False)]`:

```text
for (1,"a") × (10,True):   ((1,10), ("a",True))
for (1,"a") × (20,False):  ((1,20), ("a",False))
= [((1,10),("a",True)), ((1,20),("a",False))]
```

### 8.10 `deleteKey` — remove an entry by key

```haskell
deleteKey :: Equal k -> k -> Dict k v -> Dict k v
deleteKey eq key = keySelect (not . flip eq key)
```

A one-liner over `keySelect`: keep every entry whose key is *not* `eq` to `key`. The predicate sees only the key — `keySelect p = filterWithKey (const . p)` supplies the value-ignoring adapter, and `filterWithKey = filter . uncurry` (§14) bottoms out in Church's `filter` at the entry type (after the type-application sweep, no instantiation hint is needed anywhere in this chain). `deleteKey` thereby heads a little delegation chain the whole key-filter family shares: `deleteKey`/`keyTake`/`keyDrop` → `keySelect` → `filterWithKey` → `filter` — one primitive, several predicates (`keyTake eq wanted` keeps keys `elemBy`-present in `wanted`, `keyDrop` negates that via `notElemBy`, and `keyComplement` feeds `keySelect` an over-the-dicts membership test). Removes the matching entry if present, otherwise returns the dict unchanged.

**Trace** — `deleteKey eqI 2 [(1,"a"),(2,"b"),(3,"c")]`:

```text
(1,"a"): not (eqI 1 2) = not false = true  → keep
(2,"b"): not (eqI 2 2) = not true  = false → drop
(3,"c"): not (eqI 3 2) = not false = true  → keep
= [(1,"a"),(3,"c")]
```

### Summary of the shared pattern

The recurring skeleton is: fold over entries, destructure each `kv` by *application* to a binary continuation, consult the accumulator (or a fixed dict) with `lookup`, whose `Maybe` result *is* the case analysis, and reconstruct via `insertOrUpdate`/`upsertWith` (collision-merging) or plain `cons` (no collisions possible). `lookup` is itself a filter pipeline, and `insertOrUpdate`/`upsertWith` are both instances of §14's `insertWith`, so the whole section bottoms out in `foldr`/`filter` plus a single `caseList` + `Pair`/`Bool` recursion. There is no `if`, no pattern match, and no ADT anywhere — every decision is a Church `Bool` or `Maybe` applied to its branches.

### Wolfram-style association conveniences

The dictionary section now includes the association operations that correspond directly to
Wolfram Language `Keys`, `Values`, `Lookup[..., default]`, membership/free-key predicates,
key filtering, key sorting, key mapping, schema union/intersection/complement, general
`Merge`, and association construction:

```haskell
lookupDefault :: (a -> c -> Bool) -> b -> c -> Dict a b -> b
lookupDefault eq def key = fromMaybe def . lookup eq key
keyExists :: (a -> c -> Bool) -> c -> Dict a b -> Bool
keyExists = member
keyMember :: (a -> c -> Bool) -> c -> Dict a b -> Bool
keyMember = keyExists
keyFree :: (a -> c -> Bool) -> c -> Dict a b -> Bool
keyFree = notMember
keys :: ∀k v. Dict k v -> List k
keys = map @(k `Pair` v) fst
values :: ∀k v. Dict k v -> List v
values = map @(k `Pair` v) snd
keyValueMap :: _ -> Dict k v -> List a
keyValueMap = map . uncurry
mapKeys :: (k₁ -> k₂) -> Dict k₁ v -> Dict k₂ v
mapKeys = map . flip bimapPair id
dictFromLists :: List k -> List v -> Dict k v
dictFromLists = zip
associationMap :: Equal k -> _ -> List k -> Dict k v
associationMap eq f = fromList eq . map (pair <*> f)
keyTake/keyTakeOrdered/keyDrop/keySelect
keySort/keySortBy
keyComplement/keyIntersection/keyUnion
mergeWith
```

Most preserve association-list order; the schema-producing functions (`keyTakeOrdered`,
`keyIntersection`, `keyUnion`) intentionally use the supplied or discovered key order.
`dictFromLists` follows `zipWith pair`, so it truncates to the shorter input list.
`associationMap` builds through `fromList` — a left fold of the overwriting upsert
`insertOrUpdate` — so duplicate keys keep their first slot while later values win. `keyUnion eq missing` pads absent keys with `missing key`, avoiding a dedicated
`Missing` ADT. `mergeWith eq f` is `mapValues f . merge eq`.

The membership predicates are aliases into the `<map>` query layer of chapter 14 — `keyExists = member` (with
`keyMember = keyExists`) and `keyFree = notMember` — and the key filters all chain onto one
primitive: `deleteKey`/`keyTake`/`keyDrop` are `keySelect` instances (§8.10), and
`keyComplement` is `keySelect` again, with the predicate
`flip (notElemBy (keyExists eq)) others` — "keep the keys no other dict
claims," `notElemBy` (an `any` under a `not`) collapsing the list of dicts to a single Church `Bool`.

---

## 9. Matrix rotations and set-like list operations

Part II represents a matrix as `Matrix a = List (List a)` (a documentation synonym from section 2; see [Part I §1](#1-foundations-the-encoding-the-five-types-the-aliases-and-the-house-rules)), a list of rows, required to be rectangular. The two rotations are defined purely as compositions of Part I's `transpose` and `reverse`, so all of their interesting content is borrowed — the only thing this section adds is the geometric framing and a `@(List a)` instantiation pin in each definition.

### `turn` — rotate 90° clockwise

```haskell
-- | 'turn' rotates a rectangular matrix by 90 degrees clockwise.
turn :: ∀ a. Matrix a -> Matrix a
turn = transpose . reverse @(List a)
```

Type: `Matrix a -> Matrix a`. A clockwise quarter turn is `transpose . reverse`: reverse the *order of rows* (flip the matrix top-to-bottom), then transpose. The composition order matters and is the standard linear-algebra identity for a clockwise rotation — `reverse` first flips rows, and `transpose` then carries the (now last) row into the first column. No Church-level control flow appears here at all: `reverse` and `transpose` are the only eliminators, both inherited from `Church`. The `@(List a)` type application pins `reverse`'s element type to a *row* (`List a`), not a scalar — necessary because `reverse :: ∀a. List a -> List a` is being instantiated at the row type and the surrounding rank-N context gives GHC nothing else to fix it by.

The definition is the point-free `turn = transpose . reverse @(List a)`, and the `@(List a)` pin is the *only* concession GHC 9.12 demands here: it is one of the survivors of the automated sweep that deleted every type application inference can do without. (An earlier revision also eta-expanded the composition to `turn xss = transpose (reverse @(List a) xss)`; the sweep proved the composition itself fine — only the instantiation needed pinning. Full story in Appendix A.)

#### Worked trace

Take the matrix used by `matrixTests`, `m = [[1,2,3],[4,5,6]]` (2 rows × 3 columns). A clockwise turn should give a 3×2 matrix whose first row is the *first column of `m` read bottom-to-top*, i.e. `[4,1]`.

```text
turn m
  = transpose (reverse [[1,2,3],[4,5,6]])
  -- reverse flips row order:
  = transpose [[4,5,6],[1,2,3]]
  -- transpose maps rows->columns:
  --   col 0 = [4,1], col 1 = [5,2], col 2 = [6,3]
  = [[4,1],[5,2],[6,3]]
```

Behavior: the top-left of the input (`1`) lands at the top-right of the output, as expected of a clockwise rotation. Pinned directly by the `"turn clockwise"` assertion in `test/Spec.hs`, which expects exactly `[[4,1],[5,2],[6,3]]` on this input; the components are covered besides — `transpose [[1,2,3],[4,5,6]] = [[1,4],[2,5],[3,6]]` is the `"transpose"` assertion (line 399), and `reverse` is covered in `reverseTests`.

### `unturn` — rotate 90° counterclockwise

```haskell
-- | 'unturn' rotates a rectangular matrix by 90 degrees counterclockwise.
unturn :: ∀ a. Matrix a -> Matrix a
unturn = reverse @(List a) . transpose
```

Type: `Matrix a -> Matrix a`. The mirror image: `reverse . transpose`. Transpose first, then reverse the resulting *row order*. `unturn` is a genuine inverse of `turn` on rectangular input: `unturn (turn m) = reverse (transpose (transpose (reverse m))) = reverse (reverse m) = m`, using `transpose . transpose = id` and `reverse . reverse = id`. Same `@(List a)` instantiation rationale as `turn` (Appendix A).

#### Worked trace

```text
unturn m
  = reverse (transpose [[1,2,3],[4,5,6]])
  -- transpose:
  = reverse [[1,4],[2,5],[3,6]]
  -- reverse flips row order:
  = [[3,6],[2,5],[1,4]]
```

Behavior: counterclockwise, so the top-*right* of the input (`3`) lands at the top-left of the output. Pinned directly by the `"unturn counterclockwise"` assertion in `test/Spec.hs`. Sanity check against `turn`: `turn m = [[4,1],[5,2],[6,3]]` and `unturn m = [[3,6],[2,5],[1,4]]` are reverses-and-transposes of one another, confirming `unturn = turn⁻¹`.

### 10. Set-like and grouping list operations

This section covers the `…By` family (dedup, difference, union, intersection, grouping) and the two `pick` selectors. The unifying theme is that every branch is a *Church `Bool` applied to two continuations* — there is no `if`/`then`/`else` and no pattern match. A predicate or equality test `eq x y :: Bool = ∀e. e -> e -> e` is itself the conditional: `(eq x y) thenBranch elseBranch` selects `thenBranch` when true and `elseBranch` when false. Structural recursion is driven by `caseList :: b -> (a -> List a -> b) -> List a -> b` — Church's `maybe`-style one-step eliminator, `caseList z f xs` being `z` on `nil` and `f (head xs) (tail xs)` otherwise (it replaced every former `uncons xs z (\p -> p $ \x xs' -> …)` call-site dance: handlers first, scrutinee last) — or by folding with `foldr`/`foldl`/`any`. See [Part I](#the-one-idea) for the encodings of `Bool`, `Maybe`, `Pair`, and `List`.

### `nubBy` — deduplicate under an equality

```haskell
nubBy :: (a -> a -> Bool) -> List a -> List a
nubBy eq = foldl (\acc x -> elemBy eq x acc acc (acc `snoc` x)) nil
```

Type: `Equal a -> List a -> List a`. `nubBy` removes later duplicates, keeping the first occurrence of each equivalence class. It is a single **left** fold: `step acc x` asks `elemBy eq x acc` — does `x` already match something already accumulated? `elemBy eq x acc :: Bool` (§10's `any . eq` membership test, below), applied to the two branches `acc` (drop `x`, it's a dup) and `acc `snoc` x` (keep `x`, appended at the end). The conditional *is* the `Bool`; `elemBy` collapses the accumulator to a `Bool`, which then chooses a branch.

Note the left-fold direction. Because `foldl` walks the input front-to-back and `snoc` appends, the accumulator at each step holds exactly the *earlier* survivors — so the membership test literally asks "has an equivalent element already been kept?", and the **first** occurrence in input order is the survivor, matching `Data.List.nubBy`'s convention. (An earlier revision was a right fold that tested each element against everything to its *right*; the left fold's accumulator makes the first-wins story direct.)

#### Worked trace

Let `eq` be Church `Int` equality and `xs = [1,2,1,3,2]`. Folding left, `step` is applied with `x` running `1,2,1,3,2` front-to-back:

```text
nubBy eq [1,2,1,3,2]
  step []      1      -> elemBy eq 1 []       = false -> [1]
  step [1]     2      -> elemBy eq 2 [1]      = false -> [1,2]
  step [1,2]   1      -> elemBy eq 1 [1,2]    = true  -> [1,2]     (1 dropped)
  step [1,2]   3      -> elemBy eq 3 [1,2]    = false -> [1,2,3]
  step [1,2,3] 2      -> elemBy eq 2 [1,2,3]  = true  -> [1,2,3]   (2 dropped)
  = [1,2,3]
```

Behavior: `[1,2,1,3,2] ↦ [1,2,3]`. Exercised via the `nubOn` (`test/Spec.hs:705`) and `deleteDuplicatesBy` (`test/Spec.hs:1057`) assertions, both of which delegate here.

### `deleteBy` — remove the first match

```haskell
deleteBy :: (a -> a -> Bool) -> a -> List a -> List a
deleteBy eq y xs = break (flip eq y) xs $ \pre rest -> pre `append` tail rest
```

Type: `Equal a -> a -> List a -> List a`. Removes the *first* element equal to `y`, leaving the rest untouched. No recursion is written at all: `break (flip eq y) xs` (§10's `span` of the negated predicate) splits `xs` into the longest prefix `pre` of non-matches and the remainder `rest`, which — if a match exists — starts with the first matching element. The returned Church `Pair` *is* a function awaiting a binary continuation, so `break … xs $ \pre rest -> …` is the destructuring; the result is `pre `append` tail rest` — everything before the first hit, then everything after it. The one delicious edge case is a *miss*: when nothing matches, `rest` is `nil`, and Church's total `tail nil = nil`, so the result is `pre `append` nil = xs` unchanged — the no-match contract falls out of `tail`'s totality with no branch anywhere.

#### Worked trace

`deleteBy eq 2 [1,2,3,2]`:

```text
break (flip eq 2) [1,2,3,2]
  = span (not . flip eq 2) [1,2,3,2]
  takeWhile: 1 kept (eq 1 2 = false), 2 stops   ⇒ pre  = [1]
  dropWhile: drops 1, stops at 2                ⇒ rest = [2,3,2]
pre `append` tail rest = [1] `append` [3,2]
  = [1,3,2]
```

Behavior: `[1,2,3,2] ↦ [1,3,2]` (only the first `2` removed). The miss and empty cases are pinned directly by the `"deleteBy absent and empty"` assertion in `test/Spec.hs`; the hit path is exercised wholesale through `deleteFirstsBy`/`isPermutation` and through `nextPermutation`'s `deleteBy (eqFromLE le) m` call.

### `deleteFirstsBy` — list difference

```haskell
deleteFirstsBy :: (a -> a -> Bool) -> List a -> List a -> List a
deleteFirstsBy eq = foldr (deleteBy eq)
```

Type: `Equal a -> List a -> List a -> List a` — the non-associative bag-difference `(\\)`. Reading the partial application: `deleteFirstsBy eq xs ys = foldr (deleteBy eq) xs ys`, i.e. fold `deleteBy eq` over `ys` with the seed `xs`. Each element `y` of `ys` deletes its first occurrence from the accumulator. Beautifully, the whole algorithm is *one fold whose step is itself a Church list eliminator* (`deleteBy`), with no extra control flow — all branching lives inside `deleteBy`.

#### Worked trace

`deleteFirstsBy eq [1,2,2,3] [2,3]` = `foldr (deleteBy eq) [1,2,2,3] [2,3]`:

```text
  deleteBy eq 2 (deleteBy eq 3 [1,2,2,3])
  inner: deleteBy eq 3 [1,2,2,3] = [1,2,2]      (first 3 removed)
  outer: deleteBy eq 2 [1,2,2]   = [1,2]        (first 2 removed)
  = [1,2]
```

Behavior: removes, for each element of the second list, one matching element of the first: `[1,2,2,3] \\ [2,3] = [1,2]`. No dedicated assertion; it is exercised through `isPermutation` (§13), whose bag subtraction is exactly this fold.

### `unionBy` — set union preserving order

```haskell
unionBy :: (a -> a -> Bool) -> List a -> List a -> List a
unionBy eq xs ys = append xs (complementBy eq ys xs)
```

Type: `Equal a -> List a -> List a -> List a`. The result is `xs` followed by every element of `ys` not already present in `xs` — and that clause is *literally* the delegation: `complementBy eq ys xs` (§10's order-preserving difference, itself one `filter`) keeps each `y` of `ys` exactly when `elemBy eq y xs` is false. Note the membership test is against the *original* `xs`, never against an accumulator, so this is faithful to `Data.List.unionBy` (which does not dedup `xs` internally nor among the new `ys` elements — only `ys`-vs-`xs` is filtered). `append xs (…)` then prepends the untouched `xs`.

#### Worked trace

`unionBy eq [1,2] [2,3,1,4]`:

```text
complementBy eq [2,3,1,4] [1,2] = filter (\y -> not (elemBy eq y [1,2])) [2,3,1,4]:
  2 -> elemBy eq 2 [1,2] = true  -> drop     (2 already in xs)
  3 -> elemBy eq 3 [1,2] = false -> keep
  1 -> elemBy eq 1 [1,2] = true  -> drop     (1 already in xs)
  4 -> elemBy eq 4 [1,2] = false -> keep
  = [3,4]
append [1,2] [3,4] = [1,2,3,4]
```

Behavior: `[1,2] ∪ [2,3,1,4] = [1,2,3,4]`. Pinned directly by the `"unionBy"` assertion in `test/Spec.hs`: `[1,2] ∪ [2,3,3] = [1,2,3,3]` — note the duplicate `3` inside `ys` survives, exactly the no-dedup-within-`ys` contract described above.

### `intersectBy` — set intersection

```haskell
intersectBy :: (a -> a -> Bool) -> List a -> List a -> List a
intersectBy eq = flip (filter . flip (elemBy eq))
```

Type: `Equal a -> List a -> List a -> List a`. Keeps each element of `xs` that has a match in `ys`. Unflipped, the point-free spelling reads `intersectBy eq xs ys = filter (\x -> elemBy eq x ys) xs` — one `filter` over `xs` whose predicate is the `ys`-membership test, the Church `Bool` `elemBy eq x ys` selecting keep versus drop inside `filter`. (The two `flip`s in the source merely rearrange the arguments so the whole thing is a composition; no new machinery.) Order and multiplicity follow `xs`: duplicates in `xs` are preserved if they match, matching `Data.List.intersectBy`.

#### Worked trace

`intersectBy eq [1,2,3,2] [2,4]`:

```text
filter (\x -> elemBy eq x [2,4]) [1,2,3,2]:
  1 -> elemBy eq 1 [2,4] = false -> drop
  2 -> elemBy eq 2 [2,4] = true  -> keep
  3 -> elemBy eq 3 [2,4] = false -> drop
  2 -> elemBy eq 2 [2,4] = true  -> keep
  = [2,2]
```

Behavior: `[1,2,3,2] ∩ [2,4] = [2,2]` (both `2`s kept, since both match). Pinned directly by the `"intersectBy"` assertion in `test/Spec.hs` (on `[1,2,2,3] ∩ [2,4] = [2,2]`), and cross-checked against `Data.List.intersectBy` in the exhaustive differential group.

### `groupBy` — runs of adjacent equivalents

```haskell
groupBy :: (a -> a -> Bool) -> List a -> List (List a)
groupBy eq = caseList nil $ \x xs' ->
  span (eq x) xs' $ \matching rest ->
    (x `cons` matching) `cons` groupBy eq rest
```

Type: `Equal a -> List a -> List (List a)`. `groupBy` partitions a list into maximal runs of adjacent elements that the predicate relates to the run's first element. The outer recursion is one `caseList`: empty ⇒ `nil`, no groups; otherwise the handler receives head `x` and tail `xs'`, and `span (eq x) xs'` splits `xs'` into the longest prefix `matching` that satisfies `eq x` and the `rest`. The first group is `x `cons` matching`; recurse on `rest`.

The interesting wrinkle is the call shape `span (eq x) xs' $ \matching rest -> …`, which *looks* like a bespoke CPS helper but is plain `Church.span`. `span` pairs `takeWhile` with `dropWhile` (`span p xs = takeWhile p xs `pair` dropWhile p xs`, spelled applicatively in the source as `liftA2 (liftA2 pair) takeWhile dropWhile`) and so returns a Church `Pair` — and a Church `Pair` *is* a function awaiting a binary continuation (`Pair a b = ∀e.(a->b->e)->e`), so applying the pair directly to `\matching rest -> …` is the destructuring. In this encoding, "returns a tuple" and "takes a two-argument continuation" are the *same interface*; the call site reads identically either way. (An earlier revision shadowed `Church.span` with a hand-rolled CPS `where`-helper of exactly this shape — the import already had it.)

#### Worked trace

`groupBy eq [1,1,2,3,3]` with `eq` = `Int` equality:

```text
caseList: x=1, xs'=[1,2,3,3]
  span (eq 1) [1,2,3,3]
    = takeWhile (eq 1) [1,2,3,3] `pair` dropWhile (eq 1) [1,2,3,3]
    = [1] `pair` [2,3,3]
  pair applied to continuation -> matching=[1], rest=[2,3,3]
  group = (1 : [1]) = [1,1]; recurse on [2,3,3]
    caseList: x=2, xs'=[3,3]
      span (eq 2) [3,3] = [] `pair` [3,3] -> matching=[], rest=[3,3]
      group = [2]; recurse on [3,3]
        caseList: x=3, xs'=[3]
          span (eq 3) [3] = [3] `pair` [] -> matching=[3], rest=[]
          group = [3,3]; recurse on [] -> nil
  = [[1,1],[2],[3,3]]
```

Behavior: `[1,1,2,3,3] ↦ [[1,1],[2],[3,3]]`. Exercised directly through its one-line clients: the `"groupOn"` assertion and `uniqueBy` (`= map head . groupBy eq`, §13) in `test/Spec.hs`.

### Wolfram-style grouping, counts, and positions

`groupBy` above groups adjacent runs. The Wolfram-style additions group by first occurrence
across the whole list and expose association-list summaries:

```haskell
gatherBy :: ∀a k. Equal k -> _ -> List a -> List (List a)
gatherBy eq keyOf = values @k @(List a) . merge eq . map (liftA2 singletonDict keyOf id)
gather :: Equal a -> List a -> List (List a)
gather = flip gatherBy id
countsBy :: Equal a -> List a -> Dict a Int
countsBy eq = fromListWith (+) eq . map (`pair` 1)
countsByKey :: Equal k -> _ -> List a -> Dict k Int
countsByKey eq keyOf = countsBy eq . map keyOf
tallyBy :: Equal a -> List a -> Dict a Int
tallyBy = countsBy
countDistinctBy :: Equal a -> List a -> Int
countDistinctBy eq = length . nubBy eq
positionIndexBy :: Equal a -> List a -> Dict a (List Int)
positionIndexBy eq xs = merge eq (zipWith singletonDict xs (scanl (const . succ) 1 xs))
```

`gatherBy` maps every element to a one-entry dictionary `singletonDict (keyOf x) x` and hands
the whole list of them to §8.4's `merge`, which gathers all values sharing a key into one bucket
in first-occurrence key order; the final `values` projection returns just the
groups. `countsBy` reaches for the sibling primitive, `fromListWith (+)` over `(x, 1)` pairs — the
same collision-combining fold with integer addition — and `countsByKey` is `countsBy` after a
`map keyOf`. `positionIndexBy` is deliberately one-based, matching Wolfram Language
`PositionIndex`; it `zipWith`s the elements against their `scanl`-generated positions as singleton
dictionaries and `merge`s those, so each key's bucket collects its positions in order.

### `pick` — choose from two lists by a list of Church Bools

```haskell
pick :: List Bool -> List a -> List b -> List (Either a b)
pick keys xs ys = pick' keys (map left xs) (map right ys)
```

Type: `List Bool -> List a -> List b -> List (Either a b)`. For each position in `keys`, emit the corresponding `xs` element tagged `Left` when the key is true, or the `ys` element tagged `Right` when false. The implementation is pure delegation: pre-tag *every* element of `xs` with `left` and every element of `ys` with `right` (two `map`s), which brings both lists to the common element type `Either a b` — and the homogeneous multiplexer `pick'` below does all the walking and choosing. Inside `pick'` the per-step chooser is `k x y`, so on the pre-tagged lists a true key selects the already-`Left`-tagged element and a false key the `Right`-tagged one. The tags that the old three-way recursion applied *at selection time* are now applied up front, and laziness means only the selected ones are ever forced. (After the type-application sweep, no instantiation pins are needed here at all — inference fixes `pick'`, `left`, and `right` from the delegation itself.) Termination and the length precondition are inherited from `pick'`: `keys` drives the recursion, and `xs`/`ys` must be at least as long as `keys`, or the named `error` fires.

#### Worked trace

`pick [true,false,true] [10,20,30] [100,200,300]`:

```text
= pick' [true,false,true] [left 10,left 20,left 30] [right 100,right 200,right 300]
key=true:  k=true,  k (left 10) (right 100) = left 10
key=false: k=false, k (left 20) (right 200) = right 200
key=true:  k=true,  k (left 30) (right 300) = left 30
keys exhausted -> nil
  = [Left 10, Right 200, Left 30]
```

Behavior: `[Left 10, Right 200, Left 30]` — `True` keys draw `Left`-tagged elements from `xs`, `False` keys draw `Right`-tagged from `ys`. Pinned directly by the `"pick preserves source tags"` assertion in `test/Spec.hs`.

### `pick'` — homogeneous specialization

```haskell
pick' :: List Bool -> List a -> List a -> List a
pick' keys xs ys =
  caseList @_ @Bool nil (\k ks ->
    caseList (error "pick: xs shorter than keys") (\x xs' ->
      caseList (error "pick: ys shorter than keys") (\y ys' ->
        k x y `cons` pick' ks xs' ys') ys) xs) keys
```

Type: `List Bool -> List a -> List a -> List a`. This is the primitive that `pick` above delegates to: three lists walked in lockstep via three nested `caseList` one-step eliminations, one key `k :: Bool` consumed per step, and the chooser is simply `k x y` — the Church `Bool` `k` selecting `x` (true) or `y` (false) directly. Two subtleties, both formerly duplicated in `pick`. First, the outermost elimination is `caseList @_ @Bool` — the one TypeApplication the sweep could *not* remove, because the element type here is the rank-N `Bool` synonym and inference has nothing else to fix it by (the same family of GHC-9.12 instantiation pins discussed in Appendix A; the inner two `caseList`s need no pin, since `xs`/`ys` determine their types). Second, `keys` drives termination: the recursion stops at `nil` exactly when keys runs out, while the `xs`/`ys` eliminations carry `error` continuations as their empty cases — so both must be at least as long as `keys`, or evaluation raises the named `error`. This is the pointwise multiplexer `zipWith3 (\b u v -> if b then u else v)` written without `if` and without ADTs — `b u v` does the whole job.

#### Worked trace

`pick' [true,false,true] [1,2,3] [4,5,6]`:

```text
key=true:  k=true,  k 1 4 = 1
key=false: k=false, k 2 5 = 5
key=true:  k=true,  k 3 6 = 3
  = [1,5,3]
```

Behavior: `[1,5,3]` — selects from `xs` where the key is `True`, from `ys` where `False`.

### Direct coverage

Nearly all of these operations now have direct HUnit assertions in `test/Spec.hs`
(`deleteFirstsBy` and `groupBy` are instead exercised through their one-line
clients `isPermutation`, `groupOn`, and `uniqueBy`), in addition to the
compositional and exhaustive differential checks of their shared primitives. The
suite also exercises the corresponding Typelevel-parity aliases.

---

## 10. Subsequences, permutations, and search/predicate combinators

This chapter covers the generative and predicate machinery in section 10 of `Church.hs`: the two *generators* `subsequences` and `permutations`, which build exponentially-sized result lists without any constructor but `cons`, and the *search/predicate family* (`isSubstring`, `isSubseq`, `isPrefixOf`, `break`, `elemBy`, `isSuffixOf`, `maximumBy`, `minimumBy`), each of which reduces a list to a Church `Bool` or to a chosen element by threading a relation `eq`/`le` through folds, `zip`/`takeWhile` pipelines, and `caseList` one-step elimination instead of `case`/`if`.

The recurring stylistic move worth naming up front is the **relax-equality-to-correspondence idiom**: where the Prelude would give `isPrefixOf :: Eq a => [a] -> [a] -> Bool`, every comparison-driven function here takes an explicit relation `eq :: a -> b -> Bool` rather than `a -> a -> Bool`. The two list arguments need not share an element type — `eq` is a heterogeneous *correspondence relation*, and "equality" is merely its most common instantiation. This is more than cosmetic: it means `isSubstring` can ask "does this `[Int]` pattern occur, under `near`, in this `[Double]` text", and the type checker enforces that the relation, the pattern, and the text agree pairwise. The same `eq` value is the control-flow driver — it returns a Church `Bool = ∀e. e -> e -> e`, so `eq x y branchTrue branchFalse` *is* the conditional. There is no `if`.

For the encoding foundations (`Bool`, `List a = ∀e.(a->e->e)->e->e` = foldr, `Pair`, `Maybe`, `caseList`, `null`, `head`/`tail`), see [Part I](#the-one-idea); this chapter assumes them and focuses on the algorithms layered on top.

### `subsequences` — the power set without ADTs

```haskell
subsequences ∷ List a -> List (List a)
subsequences = foldr
  (\x rest -> rest `append` map (cons x) rest) (singleton nil)
```

**Type and algorithm.** `subsequences :: List a -> List (List a)` enumerates every subset of the input as a sublist (preserving relative order), i.e. the power set. The classic equation is the recurrence

`subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)`

with `subsequences [] = [[]]`. Each element of the tail's power set appears twice: once unchanged (the subsets that omit `x`) and once with `x` prepended (the subsets that include it). Doubling the count at each step gives the expected `2^n` results.

**How the encoding expresses it.** There is no pattern match — there is not even a written recursion. A Church list *is* its own right fold, so the recurrence collapses to one `foldr` whose seed is `singleton nil` — a *list containing one element, the empty list*, i.e. the Church analogue of `[[]]` (not `[]`!). This base case is exactly why the empty-input test below expects `[[]]`. The step `\x rest -> rest `append` map (cons x) rest` receives in `rest` the already-folded power set of everything to the right — bound once as a lambda argument, so it is genuinely shared between its two uses (Haskell's call-by-need does the rest) — and realizes `rest ++ map (x:) rest`. The work is done entirely by `singleton`, `cons`, `append`, and `map` from `Church.hs`; after the type-application sweep, not a single annotation is needed (earlier revisions carried a scoped local signature, and before that a `PartialTypeSignatures` hole, to pin the impredicative fold type — the plain `foldr` needs neither).

**Worked trace** on `xs = "ab"` (writing list literals for the Church lists, and `[…]` for `List (List a)`; `foldr` nests the steps head-outermost, so evaluate inside-out):

```text
subsequences "ab"
  = step 'a' (step 'b' (singleton nil))
  step 'b' (singleton nil):
    rest = [""]                                  -- seed: one empty subsequence
    = rest ++ map ('b':) rest
    = [""] ++ ["b"]
    = ["","b"]
  step 'a' ["","b"]:
    rest = ["","b"]
    = rest ++ map ('a':) rest
    = ["","b"] ++ ["a","ab"]
    = ["","b","a","ab"]
```

**Behavior and test.** `subseqTests` (`test/Spec.hs:516`) pins exactly this. For `xs1 = "abc"` it asserts that the *sorted* result equals `Data.List.sort ["","a","b","ab","c","ac","bc","abc"]` — sorting is applied on both sides because the generation order (`rest` before the `hd`-prefixed copies) differs from `Data.List.subsequences`'s order, but the *set* of subsequences must match. For `xs2 = []` it asserts the result is `[[]]` (the singleton-empty list), confirming the `singleton nil` base case. Cross-ref: `test/Spec.hs:521–526`.

### `permutations` — all orderings via an interleave helper

```haskell
permutations ∷ ∀a. List a -> List (List a)
permutations = foldr
  (concatMap @(List a) @(List a) . interleave) (singleton nil)
  where
    interleave ∷ a -> List a -> List (List a)
    interleave x ys = zipWith
      ((. cons x) . append) (inits ys) (tails ys)
```

**Type and algorithm.** `permutations :: List a -> List (List a)` produces all `n!` orderings. The strategy is *insertion-based*: to permute `x:xs`, first permute `xs`, then for every permutation `ys` of `xs` insert `x` into every one of its `length ys + 1` gaps. `interleave x ys` does the insertion into one list `ys`, returning the list of all `ys` with `x` spliced in at each position.

**How the encoding expresses it.** Again no constructors or `case` — and again no written recursion at the top level: `foldr` with seed `singleton nil` = `[[]]` = "the empty list has exactly one permutation, itself", and step `concatMap . interleave`: for each permutation of the everything-to-the-right fold result, produce all of its insertions of `x`, then flatten. The gem is `interleave` itself, which is *also* not a recursion but a `zipWith`:

- `inits ys` and `tails ys` each enumerate `length ys + 1` lists, and zipped positionally they are exactly the splits of `ys`: `(pre, suf)` with `pre ++ suf = ys`.
- the zipping function `(. cons x) . append` applied to `pre` and `suf` is `append pre (cons x suf)` = `pre ++ x : suf` — the insertion of `x` into that gap.

So "insert into every gap" is literally "zip the inits against the tails and splice at the seam". This is the standard non-strict `permutations` recurrence rebuilt from `foldr`, `zipWith`, `inits`/`tails`, `append`, `cons`, `map`, `concatMap`, and `singleton` — no elimination beyond the folds themselves. (The `concatMap @(List a) @(List a)` pin survives the type-application sweep because the fold's element and result types are both the rank-N `List (List a)` and inference has nothing else to fix them by.)

**Worked trace** of `interleave 1 [2,3]`, then `permutations [1,2,3]` at the top level:

```text
interleave 1 [2,3]
  inits [2,3] = [[],[2],[2,3]]
  tails [2,3] = [[2,3],[3],[]]
  zipWith splice:
    splice []    [2,3] = [] ++ 1:[2,3] = [1,2,3]
    splice [2]   [3]   = [2] ++ 1:[3]  = [2,1,3]
    splice [2,3] []    = [2,3] ++ [1]  = [2,3,1]
  = [[1,2,3],[2,1,3],[2,3,1]]            -- 1 inserted into each gap of [2,3]

permutations [1,2,3]
  = concatMap (interleave 1) (permutations [2,3])
  permutations [2,3] = concatMap (interleave 2) (permutations [3])
                     = concatMap (interleave 2) [[3]]
                     = interleave 2 [3] = [[2,3],[3,2]]
  = concatMap (interleave 1) [[2,3],[3,2]]
  = interleave 1 [2,3] ++ interleave 1 [3,2]
  = [[1,2,3],[2,1,3],[2,3,1]] ++ [[1,3,2],[3,1,2],[3,2,1]]
  = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
```

**Behavior and test.** `subseqTests` asserts `Data.List.sort (Data.List.permutations [1,2,3])` equals the sorted Church result (`test/Spec.hs:527–529`). Sorting both sides normalizes away the order difference between this insertion order and the Prelude's; the `6 = 3!` permutations as a set must coincide, which the trace confirms.

### The search/predicate family

These are all "consume a list to a `Bool` (or to one element) under a relation". They are exercised directly by the whole-API regression group and exhaustively cross-checked against `Data.List` over small finite lists; the hand-reductions below additionally explain their behavior.

#### `isPrefixOf` — the workhorse

```haskell
isPrefixOf :: (a -> b -> Bool) -> List a -> List b -> Bool
isPrefixOf eq xs ys =
  eqInt (length xs) (length (keys (takeWhile (uncurry eq) (zip xs ys))))
```

**Algorithm.** `isPrefixOf eq xs ys` holds iff `xs` matches an initial segment of `ys` pairwise under `eq` — and the implementation states that as *counting*, not recursion. `zip xs ys` pairs the two lists positionally (truncating at the shorter one); `takeWhile (uncurry eq)` keeps the longest run of pairwise-matching pairs; `keys` (§14's `map fst`, wittily reading the pair list as a `Dict`) projects back the matched `xs`-elements; and `eqInt` asks whether *every* element of `xs` was matched — the matched-prefix length equals `length xs`. The relation is heterogeneous (`a -> b -> Bool`), so `xs :: List a` and `ys :: List b` may differ in element type.

**Encoding note.** Both failure modes fall out of the pipeline with no branch of their own. A head-on mismatch just makes `takeWhile` stop early, so the count comes up short; a too-short text is handled by `zip`'s truncation — with `ys` exhausted the pair list is already shorter than `xs`, and no `takeWhile` outcome can reach `length xs`. Empty `xs` is a prefix of anything: both lengths are `0`. The only Church `Bool`s in sight are the per-pair `eq x y` consumed inside `takeWhile` and the final `eqInt`; no `case`, no `if`, and — a first for this family — no explicit recursion at all. (An earlier revision was the textbook two-`uncons` recursion with a short-circuiting `and`; the pipeline form buys `isPrefixOf`'s many clients — `isSubstring`, `isSuffixOf`, `search`, `equalBy` — one shared, fusion-friendly shape.)

**Worked trace** of `isPrefixOf (==) "ab" "abc"`:

```text
isPrefixOf (==) "ab" "abc"
  zip "ab" "abc"                 = [('a','a'),('b','b')]      -- truncates at "ab"
  takeWhile (uncurry (==)) …     = [('a','a'),('b','b')]      -- both pairs match
  keys …                         = "ab"
  eqInt (length "ab") (length "ab") = eqInt 2 2 = true
```

And the mismatch case `isPrefixOf (==) "ax" "abc"`: `zip` gives `[('a','a'),('x','b')]`, `takeWhile` stops after one pair, and `eqInt 2 1 = false`.

#### `isSubstring` — `any isPrefixOf` over `tails`

```haskell
isSubstring :: (a -> b -> Bool) -> List a -> List b -> Bool
isSubstring eq xs = any (isPrefixOf eq xs) . tails
```

**Algorithm.** `xs` occurs as a *contiguous* substring of `ys` iff it is a prefix of some suffix of `ys`. `tails ys` (from `Church.hs:466`, which yields every suffix including `ys` itself and the final `nil`) enumerates the candidate starting points; `any (isPrefixOf eq xs)` (`Church.hs:276`, `any p = foldr (or . p) false`) returns `true` as soon as one suffix has `xs` as a prefix. `or`'s left operand deciding the whole disjunction makes `any` lazily short-circuiting: once a match is found the remaining suffixes are never tested.

**Worked trace** of `isSubstring (==) "bc" "abc"`:

```text
tails "abc" = ["abc","bc","c",""]
any (isPrefixOf (==) "bc") ["abc","bc","c",""]
  isPrefixOf (==) "bc" "abc" : matched prefix "" has length 0 ≠ 2 → false
  isPrefixOf (==) "bc" "bc"  : matched prefix "bc" has length 2 = 2 → true
  ⇒ any short-circuits to true
```

#### `isSubseq` — non-contiguous subsequence test

```haskell
isSubseq :: (a -> b -> Bool) -> List a -> List b -> Bool
isSubseq eq xs = caseList (null xs)
  (\y ys' -> caseList true (\x xs' -> isSubseq eq (eq x y xs' xs) ys') xs)
```

**Algorithm.** A greedy two-pointer scan, driven by a *single* recursion on the **text**. The outer `caseList` eliminates `ys`: text exhausted ⇒ the answer is `null xs` (success exactly when the pattern is exhausted too). Text head `y` in hand, the inner `caseList` eliminates `xs`: pattern empty ⇒ `true` immediately. With both heads available, the one-liner `isSubseq eq (eq x y xs' xs) ys'` does everything at once — the Church `Bool` `eq x y` selects the *pattern to carry forward*: `xs'` on a hit (the matched pattern head is consumed) or the untouched `xs` on a miss (keep looking for `x` further along); either way the text advances to `ys'`. The `Bool` choosing between two whole lists as data, rather than between two continuations, is the entire branching mechanism.

**Worked trace** of `isSubseq (==) "ac" "abc"`:

```text
isSubseq "ac" "abc":  y='a';  'a'=='a' = true  → carry "c",  text "bc"
isSubseq "c"  "bc" :  y='b';  'c'=='b' = false → carry "c",  text "c"
isSubseq "c"  "c"  :  y='c';  'c'=='c' = true  → carry "",   text ""
isSubseq ""   ""   :  text exhausted → null "" = true
```

So `"ac"` is recognized as a (non-contiguous) subsequence of `"abc"`. By contrast `isSubseq (==) "ca" "abc"` reduces to `false`, since after matching `c` the pattern head `a` cannot be found in the remaining text `""`.

#### `break` — `span (not . p)`

```haskell
break :: (a -> Bool) -> List a -> List a `Pair` List a
break p = span (not . p)
```

**Algorithm.** `break p` splits a list at the first element *satisfying* `p`: the first component is the longest prefix whose elements all *fail* `p`, the second is the rest (starting with the first element that satisfies `p`, if any). It is `span` of the negated predicate. `not` is the Church-`Bool` `not` (`Church.hs:170`), and `span` (`Church.hs:471`) is itself the two-pass pair `span p xs = takeWhile p xs `pair` dropWhile p xs` (spelled applicatively in the source) — the prefix and the remainder are computed by the `takeWhile`/`dropWhile` pair and packaged in a Church `Pair`. Composing `not . p` simply flips which branch of the inner Church `Bool` is taken.

**Worked trace** of `break (eqInt 3) [1,2,3,4]` (using `eqInt` from `Church.hs`):

```text
break (eqInt 3) [1,2,3,4] = span (not . eqInt 3) [1,2,3,4]
  takeWhile (not . eqInt 3) [1,2,3,4]:
    1: not (eqInt 3 1) = not false = true  → keep
    2: not (eqInt 3 2) = true              → keep
    3: not (eqInt 3 3) = not true = false  → stop      ⇒ [1,2]
  dropWhile (not . eqInt 3) [1,2,3,4]: drops 1, 2; stops at 3  ⇒ [3,4]
= [1,2] `pair` [3,4]
```

Result `([1,2], [3,4])`: the prefix before the first element equal to `3`, and the remainder.

#### `elemBy` — `any . eq`

```haskell
elemBy :: (a -> b -> Bool) -> a -> List b -> Bool
elemBy eq = any . eq
```

**Algorithm.** A point-free gem. `eq :: a -> b -> Bool`, so `eq x :: b -> Bool` is the predicate "corresponds to `x`", and `any (eq x) :: List b -> Bool` tests membership. Therefore `elemBy eq x = any (eq x)`, written `any . eq` because `(any . eq) x = any (eq x)`. Membership is just existential quantification of the correspondence relation over the list — and because it routes through `any`, it short-circuits on the first match.

**Worked trace** of `elemBy (==) 2 [1,2,3]`:

```text
elemBy (==) 2 [1,2,3] = any ((==) 2) [1,2,3]
  = foldr (\x -> (2 == x) true) false [1,2,3]
  2 == 1 = false → false (continue) ; 2 == 2 = true → true (stop)
= true
```

#### `find`, `findIndex`, `findIndices`, `elemIndex`

These are the term-level flat-list counterparts of Wolfram `FirstPosition`/`Position`,
using Haskell's zero-based index convention:

```haskell
find :: (a -> b -> Bool) -> b -> List a -> Maybe a
find eq = findIf . flip eq
findIndex :: (a -> Bool) -> List a -> Maybe Int
findIndex p = listToMaybe . findIndices p
findIndices :: (a -> Bool) -> List a -> List Int
findIndices p xs = keys (filterValues p (zip (scanl (const . succ) 0 xs) xs))
elemIndex :: (a -> b -> Bool) -> a -> List b -> Maybe Int
elemIndex eq = findIndex . eq
```

None is a written recursion — all four are delegation pipelines. `findIndices` does the real
work: `scanl (const . succ) 0 xs` manufactures the position list `0,1,2,…`, `zip` reads
positions-paired-with-elements as a `Dict Int a`, `filterValues p` keeps the entries whose
*element* satisfies `p`, and `keys` projects the surviving positions. `findIndex` is
`listToMaybe` of that (lazily, only the first survivor is ever forced); `find` is §13's
`findIf` at the predicate `flip eq v`; `elemIndex eq x` is `findIndex (eq x)`.

#### `isSuffixOf` — `isPrefixOf` on reversed lists

```haskell
isSuffixOf :: (a -> b -> Bool) -> List a -> List b -> Bool
isSuffixOf eq xs = isPrefixOf eq (reverse xs) . reverse
```

**Algorithm.** A suffix is a prefix read backwards. Reverse both arguments and reuse `isPrefixOf` under the same `eq` (note `eq`'s argument order is preserved: `reverse xs :: List a` stays on the left, so the heterogeneous relation still gets its `a` first). `reverse` is the tested `Church.hs:412` foldl-based reversal. This is the same reverse-and-reuse trick the `longestCommon*` family uses for suffixes.

**Worked trace** of `isSuffixOf (==) "bc" "abc"`:

```text
isSuffixOf (==) "bc" "abc"
= isPrefixOf (==) (reverse "bc") (reverse "abc")
= isPrefixOf (==) "cb" "cba"
  matched prefix of zip "cb" "cba" is "cb": length 2 = length "cb" → true
= true
```

#### `maximumBy` / `minimumBy` — selection via `foldl1`

```haskell
maximumBy :: LE a -> List a -> a
maximumBy le = foldl1 (maxBy le)

minimumBy :: LE a -> List a -> a
minimumBy le = foldl1 (flip (minBy le))
```

(Note the deliberately asymmetric argument order: `maximumBy`'s step applies `le max x`, whereas `minimumBy`'s applies `le x min`.)

**Algorithm.** `LE a = a -> a -> Bool` is the order relation. `foldl1` (`Church.hs:292`, itself now the one-liner `caseList (error "foldl1: empty list") . foldl`) folds a non-empty list left-associatively with no seed (it `error`s on `nil`), using the first element as the initial accumulator. The binary step is where the Church `Bool` returned by `le` selects the winner:

- `maximumBy`: step `\max x -> le max x x max`. Read `le max x` as a Church `Bool`; apply it to the two candidate results `x` and `max`. If `max ≤ x` it picks `x` (the new, larger element); otherwise it keeps `max`. So the running accumulator is always the maximum-so-far. Note that on ties (`max ≤ x` and `x ≤ max`) it picks `x`, the later element — the documented "exact tie order" choice.
- `minimumBy`: step `\min x -> le x min x min`. `le x min` applied to `x` then `min`: if `x ≤ min` pick `x`, else keep `min` — the running minimum, with ties resolving to the *later* element again.

There is no comparison operator and no `if`; the order relation *is* the two-armed selector, applied directly to the two values it chooses between.

**Worked trace** of `maximumBy leInt [3,1,2]` (with `leInt` from `Church.hs:87`):

```text
maximumBy leInt [3,1,2] = foldl1 step [3,1,2]      where step max x = leInt max x x max
  caseList: head 3, tail [1,2] → foldl step 3 [1,2]
  step 3 1 = leInt 3 1 1 3 = false 1 3 = 3   -- 3 ≤ 1 is false → keep 3
  step 3 2 = leInt 3 2 2 3 = false 2 3 = 3   -- 3 ≤ 2 is false → keep 3
= 3
```

And `minimumBy leInt [3,1,2]`:

```text
foldl1 step [3,1,2]      where step min x = leInt x min x min
  foldl step 3 [1,2]
  step 3 1 = leInt 1 3 1 3 = true 1 3 = 1    -- 1 ≤ 3 → pick 1
  step 1 2 = leInt 2 1 2 1 = false 2 1 = 1   -- 2 ≤ 1 is false → keep 1
= 1
```

Both `error` on the empty list (inherited from `foldl1`), matching the Prelude's partiality for `maximumBy`/`minimumBy`.

`minMaxBy` is the applicative pairing `liftA2 (liftA2 pair) minimumBy maximumBy` — two `foldl1` sweeps
whose results land in a Church `Pair` `(min,max)`; on `nil` the error now surfaces from `foldl1`
itself (`"foldl1: empty list"`), not from a bespoke `minMaxBy` guard.
`complementBy` is the order-preserving set-difference analogue of Wolfram `Complement`
under a correspondence relation, while `containsAllBy`/`containsAnyBy`/`containsNoneBy`/
`containsOnlyBy` mirror the corresponding Wolfram containment predicates.

### Where this sits

`maximumBy`/`minimumBy` and the `le`-driven selection idiom are the same control-flow pattern the four sorts and two selection algorithms use at scale: the order relation returns a Church `Bool`, and that `Bool` *is* the branch. `isPrefixOf` is the shared kernel of `isSubstring`, `isSuffixOf`, and (via `reverse`) the `longestCommon*` family. `subsequences`/`permutations` show that even exponential generators need no constructor beyond `cons`/`nil`, no flattening beyond `append`/`concatMap`, and no branching beyond the folds themselves — the entire combinatorial structure is carried by ordinary higher-order list operations from Part I.

GHC-9.12 portability note: the automated sweep that deleted every unrequired `TypeApplication` and signature `∀` left this chapter almost annotation-free — the survivors here are exactly the ones inference needs, namely `permutations`' `concatMap @(List a) @(List a)`, `gatherBy`'s `values @k @(List a)`, and `pick'`'s `caseList @_ @Bool`, each pinning a rank-N synonym the surrounding context cannot fix. (`subsequences`, once a `PartialTypeSignatures` puzzle and later a scoped-signature bearer, now needs nothing.) For the full 9.8→9.12 migration story see [Appendix A](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes).

---

## 11. Four sorting algorithms

This is the heart of Part II: four comparison sorts — mergesort, quicksort, heapsort, and the introsort hybrid — each `O(n log n)` (quicksort `O(n²)` worst-case), each written *without* a single `if`, `case`, or `[]`-pattern. The only ordering input is a Church-Boolean less-than-or-equal predicate

```haskell
type LE a = a -> a -> Bool          -- a -> a -> (∀e. e -> e -> e)
```

(from section 2, see Part I §1). Every comparison `le x y` is not a value you scrutinize — it *is* the branch selector: `le x y t f` reduces to `t` when `x ≤ y` and to `f` otherwise, because `true = const` and `false = const id` (Part I §2). So the entire control flow of these algorithms is a chain of saturated applications of `le` and of `caseList`. There is no decision procedure separate from the data; the encoded Bool dispatches directly.

Two encoding idioms recur throughout and are worth fixing before the traces:

- **Branch on emptiness** via `caseList :: b -> (a -> List a -> b) -> List a -> b` (Part I §3). `caseList onNil onCons xs` returns `onNil` if `xs` is empty, else applies `onCons` to the head and tail — the `maybe`-style one-step eliminator, the Church stand-in for `(h:t) ->` with handlers first and scrutinee last.
- **Filter by a Church predicate**: `filter p = concatMap (\x -> p x (singleton x) nil)` (`Church.filter`). When `p x` is `true` the element survives as a one-element list, when `false` it dissolves into `nil`; `concatMap` splices the verdicts together. Quicksort, introsort, and introselect all lean on this, through `partition`/`partition3`.

For the worked traces I'll use the test list `[5,3,1,4,2]` from `sortTests` where convenient, and the smaller `[3,1,2]` where a full reduction is illuminating. Throughout, `le = (≤)` on `Int`, and I write list values in ordinary bracket notation as shorthand for their fold encodings.

### 11.1 `mergesort` — split into halves, recursively sort, merge

```haskell
mergesort :: LE a -> List a -> List a
mergesort le xs = foldr @_ @(List _ `Pair` List _)
  (\x p -> p $ \l r -> (x `cons` r) `pair` l) (nil `pair` nil) xs $ \l r ->
    null r xs (mergeBy le (mergesort le l) (mergesort le r))
```

**Type & algorithm.** `mergesort :: LE a -> List a -> List a`. Classic top-down mergesort, now compressed to a single expression: the split *is* a `foldr`, the halves land in a Church `Pair`, and the module's own `mergeBy` (§17's `std::merge` equivalent, quoted and traced in chapter 13) does the merge phase.

- The merge phase is deliberately *not* local: an earlier revision carried a `merge` helper here that was line-for-line identical to `mergeBy le`, and the sort now just calls the shared one — which nowadays is itself an instance of §17's four-flag `setOp` engine. What matters here: `mergeBy` is the two-pointer merge, and it emits the **left** list's element whenever `le a b` holds — so it favors the left list on a tie. In a textbook mergesort with a contiguous (prefix/suffix) split this left-bias is exactly what makes the whole sort stable — but see the caveat under the split below: with the alternating split this implementation actually uses, the global sort is **not** stable.
- The split is the *alternating* deal, not a length-based bisection — and it is written as one `foldr` with the two-conses-and-a-swap step `\x p -> p $ \l r -> (x `cons` r) `pair` l`: each element is consed onto the *second* half, and the two halves then trade places. Since consecutive elements keep landing on alternating sides, index-even elements end up in the first half and index-odd in the second. This avoids needing `length`/`take`/`drop` and still yields halves whose sizes differ by at most one, guaranteeing the recursion shrinks. The `foldr @_ @(List _ `Pair` List _)` pin is a sweep survivor: the fold's result type is an impredicative `Pair` of rank-N lists, and nothing else in the expression fixes it. **Stability caveat:** because the deal interleaves elements rather than keeping contiguous blocks, equal keys can be permuted relative to their input order, so `mergesort` here is *not* a stable sort even though `mergeBy` itself favors the left list. (Empirically, `[(1,a),(2,b),(1,c),(2,d),(1,e)]` keyed on the first component sorts to `[(1,a),(1,e),(1,c),…]`, reordering the `1`s.)
- The crucial **base guard** is the `null r` test in the continuation `\l r -> null r xs (…)`. The split puts the halves' sizes within one of each other, so `r` is empty exactly when the input has fewer than two elements — and in that case the original `xs` is returned *unchanged*, no recursion. Only with ≥2 elements do both `mergesort` calls fire. Without this guard a singleton would split to `([x],[])`, "sort" the `[x]` half, and loop forever; one `null` is what makes the encoded recursion terminate.

**No branching primitives.** The split's `\p -> p $ \l r -> …` forms are `Pair` eliminations standing in for tuple pattern matches; the only decisions are `null r … …` (a Bool eliminator) and, inside `mergeBy`, `le a b (…) (…)`.

**Worked trace** — `mergesort le [3,1,2]` (taking `le = (≤)`):

```text
mergesort le [3,1,2]
  split = foldr step ([], []) [3,1,2]         -- step x (l,r) = (x:r, l)  ("cons right, swap")
    step 2 ([], [])     = ([2], [])
    step 1 ([2], [])    = ([1], [2])
    step 3 ([1], [2])   = ([3,2], [1])
  => l=[3,2], r=[1]
  null r = false  -> mergeBy le (mergesort le [3,2]) (mergesort le [1])
       mergesort le [1]:  split [1] = ([1],[]); null r = true -> [1] unchanged
       mergesort le [3,2]:
         split [3,2] = ([3],[2]); null r = false
         mergeBy le (mergesort le [3]) (mergesort le [2]) = mergeBy le [3] [2]
           le 3 2 = false  -> 2 leads: 2 `cons` mergeBy le [3] []
                                mergeBy le [3] [] : ys empty -> emit xs leftovers = [3]
                           => [2,3]
       mergeBy le [2,3] [1]:
         le 2 1 = false -> 1 leads: 1 `cons` mergeBy le [2,3] []
                            mergeBy le [2,3] [] = [2,3]
                        => [1,2,3]
=> [1,2,3]
```

**Behavior / test.** `mergesort le [5,3,1,4,2] = [1,2,3,4,5]`, asserted by `sortTests` (`test/Spec.hs:444,449`).

### 11.2 `quicksort` — 3-way partition via `quickStep`/`partition3`

```haskell
quicksort :: LE a -> List a -> List a
quicksort = liftA2 quickStep id quicksort
```

**Type & algorithm.** `quicksort :: LE a -> List a -> List a`. The definition is a tying of the recursive knot through two shared helpers. `quickStep le sort` performs *one* partitioning step — head is the pivot (`caseList`), and the private `partition3` splits the tail `rest` into three disjoint buckets — with the recursive occurrences abstracted out as the parameter `sort`. Then `quicksort = liftA2 quickStep id quicksort`, which in the function applicative (`liftA2 f g h x = f (g x) (h x)`) is exactly `quicksort le = quickStep le (quicksort le)`: plug quicksort itself back in as the recursion. (Introsort below reuses the *same* `quickStep` with a different `sort` — that is why the step is factored out.)

`partition3` synthesizes strict-less, equal, and strict-greater from a *single* `≤` using two `partition` passes (each of which is two `filter`s under the hood):

- `partition (flip le pivot) xs` splits off `greater` — the elements with `¬(x ≤ pivot)`, i.e. `x > pivot` — leaving `lower` (`x ≤ pivot`);
- `partition (le pivot) lower` then splits `lower` into `equal` (`pivot ≤ x` too — the antisymmetric closure) and `less` (`x ≤ pivot ∧ ¬(pivot ≤ x)`, i.e. `x < pivot`);
- the three buckets are passed to a continuation, CPS-style: `partition3 le pivot xs k = … k less equal greater`.

Each `partition` predicate returns a Church `Bool` that `filter` feeds its keep/drop slots.

**Why this is the elegant move.** Pulling out `equal` as its own bucket does two things at once. (1) **Termination:** every element of `rest` equal to the pivot lands in `equal`, *not* in a recursive call. The two recursive calls run on `less` and `greater`, both of which strictly exclude the pivot and all its duplicates, so each recursion is on a strictly smaller list — no infinite loop on `[2,2,2]`, and no quadratic blow-up from runs of equal keys. (2) **Equal-key grouping:** the result is `sort less ++ (pivot : equal) ++ sort greater`. Because `filter` preserves input order (each element's verdict is spliced in place by `concatMap`), `equal` keeps `rest`'s relative order, and the pivot is emitted just before them; the equal block is contiguous and order-preserving.

**No branching primitives.** `caseList nil …` for the empty base; all "comparisons" are `Bool`s consumed inside `filter`. The result assembly is two `append`s and a `cons` — pure structure-building, no conditionals.

**Worked trace** — `quicksort le [3,1,2]`:

```text
quicksort [3,1,2]                              -- = quickStep le (quicksort le) [3,1,2]
  caseList -> pivot=3, rest=[1,2]
  partition3: less    = [1,2]                  -- 1≤3∧¬(3≤1)=T ; 2≤3∧¬(3≤2)=T
              equal   = []
              greater = []                     -- ¬(1≤3)=F ; ¬(2≤3)=F
  => append (quicksort [1,2]) (append (cons 3 []) (quicksort []))
       quicksort [1,2]:
         pivot=1, rest=[2]
         less=[]; equal=[]; greater=[2]
         => append (quicksort []) (append [1] (quicksort [2]))
              quicksort [] = nil
              quicksort [2] = [2]   (pivot=2, rest=[], all buckets [])
         => append [] (append [1] [2]) = [1,2]
       quicksort [] = nil
  => append [1,2] (append [3] nil) = [1,2,3]
```

**Behavior / test.** `quicksort le [5,3,1,4,2] = [1,2,3,4,5]` (`test/Spec.hs:443,448`).

### 11.3 `heapsort` — fold singleton runs through `mergeBy`

```haskell
heapsort :: LE a -> List a -> List a
heapsort le = foldr (mergeBy le . singleton) nil
```

**Type & algorithm.** `heapsort :: LE a -> List a -> List a`. The name is historical (the docstring says as much); the compact ADT-free implementation is a **fold of merges**: each element becomes a one-element sorted run (`singleton x`), and `mergeBy le` merges that run into the already-sorted fold of everything to its right. One line, one direction, ascending output directly — no descending intermediate and no `reverse`/`flip` dance (an earlier revision built a descending "max-heap" list and drained it with `snoc`s; the plain fold of merges says the same thing in one combinator).

- The step `mergeBy le . singleton` applied to `x` and the sorted suffix `acc` is `mergeBy le [x] acc` — precisely *insertion* of `x` into a sorted list, expressed as a degenerate two-pointer merge: `mergeBy` emits elements of `acc` while they are strictly below `x` (its `right` arm), then `x` (its `left`/equal arms), then the rest of `acc`.
- The seed is `nil`, the empty sorted run. So the whole sort is `mergeBy le [x₁] (mergeBy le [x₂] (… nil))` — an insertion sort wearing merge clothing.
- **Stability:** `foldr` processes the rightmost element first, so each earlier element is merged *as the left list* into a sorted list of strictly later elements — and `mergeBy` favors the left list on ties. Earlier elements therefore land in front of their equal later fellows: input order of equal keys is preserved.

It is `O(n²)` in this list encoding (each `mergeBy le [x]` pass is linear), despite the historical name's `O(n log n)` connotation — the array-backed log-time sift has no analogue on a singly-linked Church list. Correctness is unaffected; only the complexity association is aspirational.

**No branching primitives.** All the control flow lives inside `mergeBy` (§17's `setOp` instance): `caseList` for empty/nonempty, `le x y (…) (…)` for who leads.

**Worked trace** — `heapsort le [3,1,2]`:

```text
heapsort le [3,1,2] = foldr (mergeBy le . singleton) nil [3,1,2]
  = mergeBy le [3] (mergeBy le [1] (mergeBy le [2] nil))
  mergeBy le [2] nil          = [2]              -- ys empty: xs leftovers
  mergeBy le [1] [2]:
    le 1 2 = T  -> 1 leads: 1 `cons` mergeBy le [] [2] = [1,2]
  mergeBy le [3] [1,2]:
    le 3 1 = F  -> 1 leads: 1 `cons` mergeBy le [3] [2]
      le 3 2 = F -> 2 leads: 2 `cons` mergeBy le [3] []
        ys empty -> [3]
    => [1,2,3]
=> [1,2,3]
```

**Behavior / test.** `heapsort le [5,3,1,4,2] = [1,2,3,4,5]` (`test/Spec.hs:445,450`).

### 11.4 `introsort` — depth-limited quicksort with heapsort fallback

```haskell
introsort :: ∀a. LE a -> List a -> List a
introsort le = go 16
  where
    go :: Int -> List a -> List a
    go depth xs = leInt (length xs) 16
      (quicksort le)
      (leInt depth 0 (heapsort le) (quickStep le (go (depth - 1)))) xs
```

**Type & algorithm.** `introsort :: LE a -> List a -> List a`. The classic Musser hybrid: quicksort's good average case, with a depth budget that switches to heapsort to cap the worst case. The decision logic is a small **two-level Church-Bool dispatch**, no `if` — and note the golfed shape: the nested `Bool`s select a *function* (`quicksort le`, `heapsort le`, or `quickStep le (go (depth-1))`), which is then applied to the trailing `xs`.

- `leInt (length xs) 16` and `leInt depth 0` are `leInt :: Int -> Int -> Bool` from `Church` — they return Church Booleans, so they are *applied* to the two continuations rather than tested.
- if `length xs ≤ 16`, finish with plain `quicksort` (small lists don't benefit from the hybrid, and quicksort's constant factors win).
- else if the budget is exhausted (`depth ≤ 0`), fall back to `heapsort`; otherwise take **one** partition step and recurse with `depth-1`.
- The one step is *literally* §11.2's shared `quickStep`, instantiated with `go (depth - 1)` as its recursion parameter — so both recursive calls go back through `go`, and the depth limit and the small-cutoff continue to apply at every level. This is exactly why `quickStep` abstracts its `sort` argument: `quicksort` plugs itself in, `introsort` plugs in the budgeted `go`. Note `equal` is *not* recursed into — same termination/stability guarantee as §11.2.

The initial budget is `16`, and the small-list cutoff is also `16` (real introsort would set the depth to `2⌊log₂ n⌋`; here it's a fixed constant, which is a simplification). For the 5-element `sortTests` input neither fallback can trip; tracing that control path shows why all four sorts agree:

**Worked trace** — `introsort le [5,3,1,4,2]` (length 5):

```text
go 16 [5,3,1,4,2]
  length = 5 ; leInt 5 16 = true
  -> quicksort le [5,3,1,4,2]          -- small branch taken immediately
     = [1,2,3,4,5]                      -- (by §11.2, pivot 5 -> rest all <5, etc.)
```

So for the 5-element test list introsort *is* quicksort. The other two paths are no longer test-dead: the `"introsort exercises the recursive large-list path"` case sorts the already-ascending `[1..100]`, whose length (`> 16`) forces the `quickStep` recursion — and because each ascending-input partition strips only the pivot (everything else lands in `greater`), the recursion burns through all 16 depth levels and lands in the `heapsort` fallback as well.

**No branching primitives.** Two nested Church-`Bool` applications replace what would be nested `if`; the partition reuses `quickStep`/`partition3` as in §11.2.

**Behavior / test.** `introsort le [5,3,1,4,2] = [1,2,3,4,5]` (`test/Spec.hs:446,451`). All four of `sortTests` assert the same `[1,2,3,4,5]`, which is the cross-check that the four independent encodings compute the same total order from the same `le`.

### 11.5 How the four relate

All four consume the *same* `LE a` and thread it through `caseList`/`filter`/`le`-dispatch; none uses `if`, `case`, or list patterns. Mergesort is worst-case `O(n log n)`; the merge-fold `heapsort` is `O(n²)` (its name is the historical artifact §11.3 explains); quicksort is the average-case workhorse whose 3-way split makes it robust to duplicates; introsort is the safety-net composition — quicksort until either the list is small (→ quicksort) or the recursion is too deep (→ heapsort). On stability the picture is the opposite of what the textbook intuition suggests: as implemented here, `heapsort` and `quicksort` happen to preserve the input order of equal keys (heapsort via `mergeBy`'s left-bias in its fold of singleton runs, quicksort via its order-preserving `equal` bucket), whereas `mergesort` is *not* stable — its alternating split interleaves equal elements before `mergeBy` ever sees them, so the left-bias inside `mergeBy` cannot recover their original order. The 3-way partition's `equal` bucket is the single most important design choice shared by quicksort, introsort, and the introselect `nthElement'` (below in the selection chapter): it is simultaneously what guarantees termination on repeated keys and what keeps equal elements grouped and in input order.

One GHC-9.12 note: after the automated type-application sweep, the only type application anywhere in the four sorts is `mergesort`'s `foldr @_ @(List _ `Pair` List _)` pin on its split fold (the impredicative pair-of-lists result type that nothing else fixes); everything else in this chapter is annotation-free. See Appendix A for the full story.

---

## 12. Selection algorithms and longest-common-substructure

This chapter covers the tail of Section 11 — the two *selection* procedures `nthElement` and `nthElement'` (full introselect/quickselect) — and the whole of Section 12, the longest-common-substructure family. Selection is the natural companion to the four sorts that precede it in Section 11: instead of producing a fully sorted list, it answers a *positional* query ("what is the element that would sit at index `n` once sorted?"). The LC\* family is a small, elegant demonstration of how `reverse` turns one algorithm (longest common *prefix*) into three.

Throughout, recall the eliminator vocabulary from Part I: a `List a = ∀e.(a->e->e)->e->e` is its own right fold; `caseList z f` is the one-step deconstructor (`z` on `nil`, else `f head tail`); a `Maybe b = ∀e.e->(b->e)->e` is applied as `m onNothing onJust`; a `Pair x y` is applied as `p (\x y -> ...)`; and a Church `Bool` *is* the if-then-else — `b thenBranch elseBranch`. Control flow here is entirely `le`/`eq`/`leInt`/`ltInt`/`geInt` (all returning Church `Bool`) selecting one of two continuations, never `if`. The `Int` comparison primitives `leInt`/`ltInt`/`geInt :: Int -> Int -> Bool` live in `Church.hs` (lines 81–96); they are the only place a host `Int` comparison leaks in, and they hand back a Church `Bool` so the rest of the dataflow stays encoding-pure.

### 11.x `nthElement` — selection by full `heapsort`

```haskell
nthElement :: LE a -> Int -> List a -> List a
nthElement = const . heapsort
```

Despite its selection-flavored name, this is the *deliberately simple* `O(n^2)` version: it ignores `n` entirely — the `const` swallows the index before it ever gets a name — and just **fully sorts** the list, then relies on the caller reading off index `n`. (`nthElement'` below is the genuinely selection-flavored one — quickselect — which originally shipped with two bugs, now fixed.) An earlier revision spelled the sort out longhand, with two local helpers `insert`/`sort` that turned out to be verbatim copies of `heapsort`'s internals; the definition now states outright what was always true: `nthElement` *is* `heapsort`, selection pretensions notwithstanding. Everything interesting about the computation — the fold that merges each element as a singleton run into the sorted suffix — is therefore §11.3's, documented and traced there.

#### Trace: `nthElement le 2 [3,1,2]` with `le = (<=)`

The call reduces immediately to `heapsort le [3,1,2]` — and §11.3's worked trace is on exactly this input, so it applies verbatim: singleton runs merged right-to-left.

```text
nthElement le 2 [3,1,2]
  = heapsort le [3,1,2]
  = mergeBy le [3] (mergeBy le [1] (mergeBy le [2] nil))     -- §11.3
  = mergeBy le [3] [1,2]
  = [1,2,3]
```

So `nthElement le 2 [3,1,2] = [1,2,3]`, and the element at index `2` is `3` — `heapsort`'s fold of merges doing exactly what it does in §11.3.

**Tests** (`test/Spec.hs`, `nthElementTests`, lines 454–485): empty stays empty; singleton stays singleton; `nthElement le 2 [5,3,1,4,2]` equals the fully sorted `[1,2,3,4,5]`; `nthElement le 4 [9,3,7,1,5,8,2,6,4]` equals `Data.List.sort [9,3,7,1,5,8,2,6,4]`; and the "finds median element" case confirms index 2 of the result is `3`. The suite checks the *whole* output list against the fully-sorted reference, which is consistent with `nthElement` being a complete sort. The `le` under test is `\x y -> fromPreludeBool (x <= y) :: LE Int`.

### 11.x `nthElement'` — introselect (quickselect)

```haskell
nthElement' :: ∀a. LE a -> Int -> List a -> List a
nthElement' le n xs = introselect 0 n xs
  where
    maxDepth = 2 * length xs
    -- 'introselect depth k ys' rearranges 'ys' so that index 'k' holds its k-th order
    -- statistic, with everything before <= it and everything after >= it. 'k' is the
    -- LOCAL index into 'ys' and is re-indexed on each recursion; every branch reassembles
    -- ALL of less ++ (pivot:equal) ++ greater so no elements are lost.
    introselect :: Int -> Int -> List a -> List a
    introselect depth k xs =
      geInt depth maxDepth
        (heapsort le xs)  -- Fall back to heapsort
        (caseList nil (\pivot rest ->
          partition3 le pivot rest $ \less equal greater ->
          let lessLen    = length less
              pivotBlock = pivot `cons` equal      -- the pivot and its duplicates
              pivotEnd   = lessLen + length pivotBlock
          in append (ltInt k lessLen (introselect (succ depth) k less) less)
                    (append pivotBlock
                            (ltInt k pivotEnd greater
                                   (introselect (succ depth) (k - pivotEnd) greater)))) xs)
```

This is the **quickselect** selection algorithm: rearrange so index `k` holds the `k`-th order statistic, with everything before it `≤` it and everything after `≥` it — *without* fully sorting (only the partition containing `k` is refined). Compared to the simple `nthElement` above, it does asymptotically less work because it discards-by-recursion only one side.

> **History (now fixed).** The originally-shipped `nthElement'` had two confirmed bugs — it was untested by the then-current suite (`nthElementTests` only exercises `nthElement`), so they shipped unnoticed. **(1)** The `less` branch returned `introselect … less` *alone*, dropping the pivot, the `equal` block, and `greater` — so the output was *shorter than the input* whenever the target fell in `less` (e.g. `nthElement' le 2 [5,4,3,2,1]` returned `[2,1,3]`, losing `4` and `5`). **(2)** The `greater` branch recursed with the *same* `n` rather than re-indexing, so it mis-selected on inputs like `[9,3,7,1,5,8,2,6,4]` at `n=3`. The fix above threads a **per-frame local index `k`** (initialised to `n`, decremented by `pivotEnd` when descending into `greater`) and **reassembles all three partitions in every branch** (`less ++ pivotBlock ++ greater`). It was checked exhaustively against the spec — all permutations of `[1..7]` for every `k`, plus duplicate-laden multisets, **41,960 cases, 0 failures** — and pinned by the new `nthElementPrimeTests` group in `test/Spec.hs`.

**Control structure, eliminator by eliminator.** `caseList nil (\pivot rest -> …) xs` extracts a pivot (the head) and `rest`; on `nil` the result is `nil`. The three-way partition is *literally* `quicksort`'s — the shared `partition3` (§11.2), two `partition` passes deriving `less` (strictly `< pivot`), `equal` (`x≤pivot ∧ pivot≤x`), and `greater` (strictly `> pivot`) from the single `≤` and handing all three to a continuation.

`filter p = concatMap (\x -> p x (singleton x) nil)` (Church.hs:406–407), underneath `partition`, makes each keep/drop decision a Church `Bool` choosing between a singleton and `nil` — again no `if`.

**The index arithmetic (now correct).** In the reassembled order `less ++ pivotBlock ++ greater` (with `pivotBlock = pivot : equal`, so `pivotEnd = lessLen + length pivotBlock`), `less` occupies indices `0 .. lessLen-1` and the pivot block occupies `lessLen .. pivotEnd-1`. The expression *always* rebuilds all three segments — one `append` spine, no per-case duplication — and the two `ltInt` tests merely decide which single segment gets *refined* rather than passed through untouched:

1. `ltInt k lessLen` (`k < lessLen`) — target is in `less`: the first segment becomes `introselect (succ depth) k less` (same `k`, it shares the leading index range); `pivotBlock` and `greater` ride along unrefined.
2. `ltInt k pivotEnd` false-side never fires for `lessLen ≤ k < pivotEnd` — target value *is* the pivot: both flanking segments pass through untouched, and the answer already sits in `pivotBlock`.
3. otherwise (`k ≥ pivotEnd`) — target is in `greater` at local index `k - pivotEnd`: the last segment becomes `introselect (succ depth) (k - pivotEnd) greater`.

`geInt`/`ltInt` compare host `Int`s but yield Church `Bool`s, applied immediately to two continuations — here selecting between a refined and an unrefined *list segment*, data rather than control.

**Note — the heapsort fallback is dead code.** This is *introselect* in name only: `geInt depth maxDepth` guards the `heapsort` fallback with `maxDepth = 2 · length(originalList)`, but `depth` increases by 1 per recursion on a *strictly smaller* sublist (the pivot is always removed), so `depth < length ≤ maxDepth` always holds and the fallback never fires. The effective algorithm is plain quickselect (worst-case `O(n²)`). This is a pre-existing quirk, not affected by the fix.

#### Trace (formerly wrong — `less`-branch element loss): `nthElement' le 2 [5,4,3,2,1]`, want index 2 (value `3`)

```text
maxDepth = 2*5 = 10
introselect 0 2 [5,4,3,2,1]: pivot=5, less=[4,3,2,1], equal=[], greater=[], lessLen=4, pivotEnd=5
  ltInt 2 4 = true  -> target in `less`; refine less, KEEP pivotBlock [5] and greater []:
    append (introselect 1 2 [4,3,2,1]) (append [5] [])
      introselect 1 2 [4,3,2,1]: pivot=4, less=[3,2,1], lessLen=3, pivotEnd=4
        ltInt 2 3 = true -> append (introselect 2 2 [3,2,1]) (append [4] [])
          introselect 2 2 [3,2,1]: pivot=3, less=[2,1], lessLen=2, pivotEnd=3
            ltInt 2 2 = false; ltInt 2 3 = true -> pivot block:
              append [2,1] (append [3] []) = [2,1,3]
          = [2,1,3]
        = append [2,1,3] [4] = [2,1,3,4]
      = [2,1,3,4]
    = append [2,1,3,4] [5] = [2,1,3,4,5]
```

Result `[2,1,3,4,5]` — all five elements present, index 2 is `3` = `sort!!2`, prefix `[2,1] ≤ 3`, suffix `[4,5] ≥ 3`. ✓ (The old code returned `[2,1,3]`.)

#### Trace (formerly wrong — stale index in `greater`): `nthElement' le 2 [1,2,3]`, want index 2 (value `3`)

```text
introselect 0 2 [1,2,3]: pivot=1, less=[], equal=[], greater=[2,3], lessLen=0, pivotEnd=1
  ltInt 2 0 = false; ltInt 2 1 = false -> target in `greater`; RE-INDEX k: 2 - 1 = 1
    append [] (append [1] (introselect 1 1 [2,3]))
      introselect 1 1 [2,3]: pivot=2, greater=[3], lessLen=0, pivotEnd=1
        ltInt 1 0=false; ltInt 1 1=false -> greater; RE-INDEX k: 1 - 1 = 0
          append [] (append [2] (introselect 2 0 [3]))
            introselect 2 0 [3]: pivot=3, lessLen=0, pivotEnd=1
              ltInt 0 0=false; ltInt 0 1=true -> pivot block: [3]
          = append [2] [3] = [2,3]
    = append [1] [2,3] = [1,2,3]
```

Result `[1,2,3]`, index 2 is `3`. ✓ The re-indexing `k: 2 → 1 → 0` is exactly what the old stale-`n` code lacked.

### 12. Longest Common Substructure

The whole section is built on one primitive — `longestCommonPrefix` — and a `reverse`-based duality. The companion `reverse :: List a -> List a` (Church.hs:412–413, `reverse = foldl (flip cons) nil`) is what lets a single prefix algorithm answer suffix and sublist questions. `Equal a = a -> a -> Bool` is the documentation-only synonym from Church.

#### `longestCommonPrefix`

```haskell
longestCommonPrefix :: Equal a -> List a -> List a -> List a
longestCommonPrefix eq xs = keys . takeWhile (uncurry eq) . zip xs
```

Type `Equal a -> List a -> List a -> List a`. The algorithm is the same `zip`/`takeWhile` pipeline as §10's `isPrefixOf`, stopped one stage earlier: `zip xs ys` walks the two lists in lockstep (either list ending ends the walk — that is `zip`'s truncation), `takeWhile (uncurry eq)` keeps the pairs while the Church `Bool` `eq x y` holds, and `keys` (§14's `map fst`) projects the `xs`-side of the surviving pairs. No `if`, no pattern match, no written recursion — three pipeline stages and one `eq` consumed inside `takeWhile`.

```text
longestCommonPrefix (==) [1,2,9] [1,2,8]
  zip [1,2,9] [1,2,8]        = [(1,1),(2,2),(9,8)]
  takeWhile (uncurry (==)) … = [(1,1),(2,2)]        -- eq 9 8 = false stops the walk
  keys …                     = [1,2]
```

So `longestCommonPrefix (==) [1,2,9] [1,2,8] = [1,2]`.

#### `longestCommonSuffix`

```haskell
longestCommonSuffix :: Equal a -> List a -> List a -> List a
longestCommonSuffix eq xs = reverse . longestCommonPrefix eq (reverse xs) . reverse
```

The duality in one line: a common *suffix* of `xs`,`ys` is a common *prefix* of their reverses, read backwards. Reverse both inputs, take the LCP, reverse the answer.

```text
longestCommonSuffix (==) [1,2,3] [9,2,3]
  reverse [1,2,3] = [3,2,1]; reverse [9,2,3] = [3,2,9]
  lcp [3,2,1] [3,2,9]: zip -> [(3,3),(2,2),(1,9)]; takeWhile keeps (3,3),(2,2); keys = [3,2]
  reverse [3,2] = [2,3]
```

So `longestCommonSuffix (==) [1,2,3] [9,2,3] = [2,3]`.

#### `longestCommonSublist`

```haskell
longestCommonSublist :: Equal a -> List a -> List a -> List a
longestCommonSublist eq = liftA2 (liftA2 append)
  (longestCommonPrefix eq) (longestCommonSuffix eq)
```

The name promises "longest common *contiguous* sublist," and the implementation is the "prefix+suffix trick" in its plainest form: the longest common *prefix* `append`ed to the longest common *suffix*. (An earlier revision obtained the prefix half by a double-reverse dance — `reverse $ longestCommonSuffix eq (reverse xs) (reverse ys)` — but since `longestCommonSuffix` is itself `reverse ∘ longestCommonPrefix ∘ (reverse × reverse)`, the expanded expression holds six reverses that cancel in three `reverse ∘ reverse` pairs — one around the result, one around each input — so it *is* `longestCommonPrefix eq xs ys`. The one-liner now says so.)

```text
longestCommonSublist (==) [1,2,3,9] [1,2,7,3,9]
  prefix = lcp [1,2,3,9] [1,2,7,3,9] = [1,2]    -- 1==1, 2==2, 3 vs 7 stop
  suffix = lcsuffix [1,2,3,9] [1,2,7,3,9]
         = reverse (lcp [9,3,2,1] [9,3,7,2,1])
         = reverse [9,3]                         -- 9==9, 3==3, 2 vs 7 stop
         = [3,9]
  result = [1,2] `append` [3,9] = [1,2,3,9]
```

**Caveat worth flagging.** This `prefix ++ suffix` formulation computes "(longest common prefix) followed by (longest common suffix)", which equals the true longest *contiguous* common sublist only when the maximal common run is anchored at a list end (or when prefix and suffix don't overlap). For two strings whose longest common substring sits strictly in the interior with mismatches on both sides — e.g. `xs=[1,5,5,2]`, `ys=[3,5,5,4]`, whose true longest common contiguous sublist is `[5,5]` — both LCP and LCS are empty and the function returns `nil`. So this is best read as a *prefix/suffix-anchored* common-sublist heuristic rather than a general longest-common-substring solver. I note it because it is exactly the kind of edge case a PL person will probe; the implementation as written is faithful to the "prefix+suffix trick" name but not to a full LCS.

### GHC 9.12 note

After the automated sweep, the module carries explicit `TypeApplications` only where GHC 9.12's tightened impredicative-instantiation rules genuinely need them — e.g. `caseList @_ @(k `Pair` v)` inside `insertWith`, `upsertWith @b @(List a)` inside `invert`, and `reverse @(List a)` inside `turn`/`unturn`; the functions in *this* chapter need none. See **[Appendix A](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes)** for the full migration story.

---

## 13. C++ `<algorithm>` equivalents

Sections 13-19 of `Church.hs` port the Haskell-meaningful functions of the C++ standard header `<algorithm>` into the Bohm-Berarducci encoding: non-modifying searches, modifying operations, partitioning/sorting predicates, binary search, merge and set operations on sorted ranges, min/max and comparison, and permutations. The C++ originals are catalogued in [`docs/reference/cpp/algorithm.md`](reference/cpp/algorithm.md). As everywhere in the module, ordering/equality relations are passed explicitly (no type classes), branching is a Church `Bool` applied to two arms (never `if`), and `Int` is the only concrete type. Every example below is pinned by the `algorithmTests` group in [`test/Spec.hs`](../test/Spec.hs) (73 HUnit cases; full suite 403/403 via `cabal test` on GHC 9.12.4).

**What is deliberately *not* ported, and why.** A number of `<algorithm>` operations have no
distinct meaning in a pure, immutable, list-shaped setting and are intentionally omitted: the
copy/move family (`copy`, `copy_n`, `copy_backward`, `move`, … — the identity, or `take`/`drop`
slices), `fill`/`fill_n` (that is `replicate`), `transform` (that is `map`/`zipWith`),
`generate`/`generate_n` (that is `unfoldr`/`iterate`), `for_each` (that is `map`), the in-place
positional `swap`/`iter_swap`/`swap_ranges`, the randomized `shuffle`/`sample`, and the
binary-heap primitives `make_heap`/`push_heap`/`pop_heap`/`sort_heap`/`is_heap`/`is_heap_until`
(an array-indexed heap does not map onto a Böhm–Berarducci list — and `heapsort` already provides
the only heap behaviour that does). The sorts (`sort`/`stable_sort`/`partial_sort`) are likewise
already covered by `mergesort`/`quicksort`/`heapsort`/`introsort` (Section 11) and, at the type
level, by `Sort`/`SortBy`.

---

### Non-modifying searches

This chapter documents **section 13** of `Church.hs` — Böhm–Berarducci renderings of the *non-modifying sequence operations* from C++ `<algorithm>` ([cppreference](https://en.cppreference.com/cpp/algorithm), see [`docs/reference/cpp/algorithm.md`](reference/cpp/algorithm.md)). As everywhere in this module there are no ADTs, no `if`-`then`-`else`, and no `case`: every decision is a Church `Bool = ∀e. e -> e -> e` or a Church `Maybe = ∀r. r -> (a -> r) -> r` *applied directly to its branches*. Two idioms from earlier chapters recur and are worth re-stating, because the whole section is built from them:

- **Relations are passed in and heterogeneous.** Where the STL takes a default `operator==` or a `BinaryPred`, every search here takes an explicit relation `eq :: a -> b -> Bool` (the encoding has no type classes). Generalizing to `a -> b -> Bool` rather than `a -> a -> Bool` lets the needle and haystack carry different element types; `find`/`count`/`mismatch`/`search`/`findEnd`/`findFirstOf`/`searchN` all exploit this.
- **`Maybe` *is* its own eliminator, `Bool` *is* its own conditional.** A predicate result `p x` is applied as `p x thenArm elseArm`; a `Maybe` result `m` is applied as `m nothingArm justArm`. There is nothing to pattern-match on — the constructor *is* the dispatcher. Most of these functions are therefore short *pipelines* of the chapter-3 vocabulary — `filter`, `listToMaybe`, `zip`, `tails`, `reverse`, `length` — with the per-element `Bool` consumed inside a fold several delegations down.

Throughout, `nothing`, `just`, `true`, `false`, `not`, `and`, `any`, `filter`, `zip`, `tails`, `listToMaybe`, `pair`, and `isPrefixOf` are the primitives of Part I and the earlier Part II chapters (see [Part I](#the-one-idea)); `Int` is the one concrete type the module admits, with `+`, `leInt` (`≤` on `Int`) used by the index-returning searches. All worked values below are the exact `assertEqual` expectations in the `algorithmTests` group of [`test/Spec.hs`](../test/Spec.hs) (lines 632–648).

#### `none` — `std::none_of`

```haskell
none :: (a -> Bool) -> List a -> Bool
none p = not . any p
```

The trivial De Morgan composite: `none p = not . any p`. `any p xs` collapses the list to a Church `Bool` (it is `foldr (or . p) false`, short-circuiting at the first satisfier), and `not` flips it. No recursion is written here — all the control flow lives inside the inherited `any`, and `not b = b false true` is itself just `Bool` elimination. The three `<algorithm>` predicates `all_of`/`any_of`/`none_of` correspond to the module's `all`/`any` (in `Church.hs`) plus this `none`.

**Worked examples.** `none (>10) [1,2,3] = True` (no element exceeds 10) and `none even [1,2,3] = False` (the `2` satisfies `even`) — `test/Spec.hs:632–633`.

#### `findIf` — `std::find_if`

```haskell
findIf :: (a -> Bool) -> List a -> Maybe a
findIf p = listToMaybe . filter p
```

The first element satisfying `p`, as a Church `Maybe`. It is a two-stage pipeline: `filter p` keeps the satisfiers (lazily — nothing past the first is ever demanded), and `listToMaybe` (= `foldr (const . just) nothing`) takes the head of that filtered list as a `Maybe`, whose `just`-arm discards the unforced rest. So the *leftmost* satisfier wins, and the first match short-circuits the (lazily-built) tail — the same behavior the old bespoke fold had, now for free from two inherited combinators.

**Worked example.** `findIf even [1,2,3,4] = just 2`. `filter even [1,2,3,4]` begins `[2,…`; `listToMaybe` seizes the `2` and never forces the `…` (which would be `[4]`). (`test/Spec.hs:634`; the no-match case `findIf (>9) [1,2,3] = nothing` — the filtered list is `nil`, so the seed `nothing` survives — is line 601.)

#### `findIfNot` — `std::find_if_not`

```haskell
findIfNot :: (a -> Bool) -> List a -> Maybe a
findIfNot p = findIf (not . p)
```

The first element *failing* `p`, defined by composing `findIf` with the negated predicate `\x -> not (p x)` (where `not b = b false true`). Nothing new structurally — the search and the `Maybe`-branching are entirely inherited from `findIf`; only the predicate is inverted.

**Worked example.** `findIfNot even [2,4,1,6] = just 1`: the predicate `\x -> not (even x)` first holds at `1`, after `2` and `4` are rejected (`test/Spec.hs:636`).

#### `find` — `std::find`

```haskell
find :: (a -> b -> Bool) -> b -> List a -> Maybe a
find eq = findIf . flip eq
```

The first element *equal under `eq`* to a target `v`, again a thin specialization of `findIf`: the unary predicate is `\x -> eq x v`, partially applying the heterogeneous correspondence `eq` to the fixed right operand `v`. The relaxed signature `(a -> b -> Bool) -> b -> …` lets the target type `b` differ from the element type `a` — the STL's `find` fixes both to the value type. All the `Maybe`/`Bool` machinery is `findIf`'s.

**Worked example.** `find (==) 3 [1,2,3,4] = just 3` — the scan rejects `1, 2`, matches at `3` (`test/Spec.hs:637`).

#### `findLast` — `std::ranges::find_last_if` (C++23)

```haskell
findLast :: (a -> Bool) -> List a -> Maybe a
findLast p = findIf p . reverse
```

The *last* element satisfying `p`. The body is the mirror of `findIf` by *reversal*: `reverse` the list, then take the first satisfier. The last match of `xs` is exactly the first match of `reverse xs` — same predicate-as-conditional, opposite scan direction, opposite winner. It corresponds to C++23's `ranges::find_last_if`.

**Worked example.** `findLast even [1,2,3,4,5] = just 4`. `reverse` gives `[5,4,3,2,1]`; `findIf even` rejects `5` and stops at `4` — yielding the *last* even element `4`, not the first (`test/Spec.hs:638`).

#### `countIf` — `std::count_if`

```haskell
countIf :: (a -> Bool) -> List a -> Int
countIf p = length . filter p
```

How many elements satisfy `p`, as an `Int` (the one concrete type the module permits). Another two-stage pipeline: `filter p` keeps the satisfiers, `length` (= `foldr (const succ) 0`) counts them. The per-element Church `Bool` is consumed inside `filter` — singleton-or-`nil` per element — and the "increment" is `length`'s `succ`; a branchless tally with no bespoke fold.

**Worked example.** `countIf even [1,2,3,4] = 2` — only `2` and `4` bump the counter (`test/Spec.hs:639`).

#### `count` — `std::count`

```haskell
count :: (a -> b -> Bool) -> b -> List a -> Int
count eq = countIf . flip eq
```

How many elements equal `v` under `eq`, exactly analogous to `find`-vs-`findIf`: `countIf` with the predicate `\x -> eq x v`. The heterogeneous `eq :: a -> b -> Bool` again decouples the target type `b` from the element type `a`. The counting and the `Bool`-driven increment are `countIf`'s.

**Worked example.** `count (==) 2 [2,1,2,3,2] = 3` — three of the five elements equal `2` (`test/Spec.hs:640`).

#### `mismatch` — `std::mismatch`

```haskell
mismatch :: (a -> b -> Bool) -> List a -> List b -> Maybe (a `Pair` b)
mismatch eq xs = findIf (not . uncurry eq) . zip xs
```

The first pair of corresponding elements that do **not** satisfy `eq`, walking the two lists in lockstep — as a pipeline: `zip xs ys` builds the lockstep pair list (truncating at the shorter range), and `findIf (not . uncurry eq)` returns the first pair whose components *disagree*, already packaged as a Church `Pair` inside the `Maybe`. Either list running out first simply ends the `zip`, so when the shorter range is a prefix-match of the longer, no pair violates `eq` and the result is `nothing` (matching `std::mismatch`'s "no mismatch within the common length").

**Worked examples.** `mismatch (==) [1,2,3] [1,9,3] = just (2,9)`: `zip` gives `[(1,1),(2,9),(3,3)]`, and `findIf` stops at `(2,9)` (`test/Spec.hs:641`). `mismatch (==) [1,2] [1,2,3] = nothing`: `zip` truncates to two agreeing pairs, `findIf` finds nothing (`test/Spec.hs:642`).

#### `adjacentFind` — `std::adjacent_find`

```haskell
adjacentFind :: (a -> a -> Bool) -> List a -> Maybe a
adjacentFind eq xs = findIf (uncurry eq) (zip xs $ tail xs)
  nothing (just . fst)
```

The first element equal (under `eq`) to its immediate successor. The adjacent windows are manufactured by the self-`zip` idiom: `zip xs (tail xs)` pairs each element with its successor (a list of length `< 2` yields no windows at all — `zip`'s truncation again). `findIf (uncurry eq)` locates the first window whose two components agree, and the trailing `nothing (just . fst)` eliminates that `Maybe (a `Pair` a)` down to `Maybe a`, projecting the *first* of the matching pair (as `std::adjacent_find` returns the iterator to the first). Note the homogeneous relation `a -> a -> Bool` here, since both operands come from the same list.

**Worked example.** `adjacentFind (==) [1,2,2,3] = just 2`: the window `(1,2)` differs, the next window `(2,2)` matches, returning the first `2` of that adjacent pair (`test/Spec.hs:643`).

#### `search` — `std::search`

```haskell
search :: (a -> b -> Bool) -> List a -> List b -> Maybe Int
search eq needle = findIndex (isPrefixOf eq needle) . tails
```

The start **index** of the first occurrence of `needle` inside `haystack` (under `eq`), or `nothing`. (The Haskell port returns an `Int` offset where `std::search` returns an iterator; an empty needle matches at `0`.) The candidate start positions are exactly the suffixes: `tails haystack` enumerates them in order, and `findIndex (isPrefixOf eq needle)` — §10's index search over that list of suffixes — returns the position of the first suffix that begins with the needle. Suffix number `i` starts at offset `i`, so the found index *is* the answer; the final `nil` in `tails` even makes the empty-needle-matches-at-the-end case come out right.

**Worked examples.** `search (==) [2,3] [1,2,3,4] = just 1`: `tails` gives `[[1,2,3,4],[2,3,4],[3,4],[4],[]]`; the prefix test fails at suffix 0 and succeeds at suffix 1 (`test/Spec.hs:644`). `search (==) [9,9] [1,2,3] = nothing`: every suffix fails the prefix test (`test/Spec.hs:645`).

#### `findEnd` — `std::find_end`

```haskell
findEnd :: (a -> b -> Bool) -> List a -> List b -> Maybe Int
findEnd eq needle = listToMaybe . reverse . findIndices (isPrefixOf eq needle) . tails
```

The start index of the **last** occurrence of `needle`. Where `search` takes the *first* matching suffix index, `findEnd` collects *all* of them — `findIndices (isPrefixOf eq needle) . tails` — then `reverse`s the index list and takes its head with `listToMaybe`: the last match, or `nothing` when the index list is empty. The "last winner" is thus obtained by list surgery on the complete match set rather than by any accumulator threading. This is `std::find_end`'s "iterator to the beginning of the last subrange".

**Worked example.** `findEnd (==) [1] [1,2,1,3,1] = just 4`: the singleton needle `[1]` matches at suffixes `0`, `2`, and `4`; `findIndices` yields `[0,2,4]`, `reverse` gives `[4,2,0]`, and `listToMaybe` seizes the `4` (`test/Spec.hs:646`).

#### `findFirstOf` — `std::find_first_of`

```haskell
findFirstOf :: (a -> b -> Bool) -> List a -> List b -> Maybe a
findFirstOf eq = flip (findIf . flip (elemBy eq))
```

The first element of `xs` that equals (under `eq`) *some* element of `set`. It composes two inherited combinators: the per-element membership test `\x -> any (\s -> eq x s) set` collapses "does `x` match anything in `set`?" to a Church `Bool` via `any`, and `findIf` then returns the first `x` for which that `Bool` is `true`. So the whole thing is `findIf` over a `set`-membership predicate — a doubly-nested but entirely branch-free search, with both the inner `any` and the outer `findIf` driven by `Bool`/`Maybe` elimination.

**Worked example.** `findFirstOf (==) [1,2,3] [9,8,2] = just 2`: `1` matches nothing in `{9,8,2}`, `2` matches the `2`, so `2` is returned (`test/Spec.hs:647`).

#### `searchN` — `std::search_n`

```haskell
searchN :: (a -> b -> Bool) -> Int -> b -> List a -> Maybe Int
searchN eq n = search (flip eq) . replicate n
```

The start index of the first run of `n` consecutive elements all equal (under `eq`) to `v` (`n <= 0` matches at `0`). The reduction is delicious: a run of `n` copies of `v` is just the needle `replicate n v` — so `searchN` *builds that needle and delegates to `search`*. The only wrinkle is orientation: `search`'s relation relates haystack elements to needle elements, and here the needle is made of `v :: b` while the haystack carries `a`, so the relation is flipped once (`search (flip eq)`). The `n <= 0` boundary needs no code at all — `replicate` of a non-positive count is `nil`, and an empty needle matches at `0`.

**Worked example.** `searchN (==) 2 7 [7,1,7,7,2] = just 2`: the needle is `replicate 2 7 = [7,7]`; `search` tests it against each suffix — `[7,1,…]` fails (`eq 1 7 = false` at the second element), `[1,…]` fails, `[7,7,2]` succeeds — so the run of two `7`s starts at index `2` (`test/Spec.hs:648`).

---

#### Modifying ops, partitioning and sorting predicates

These twelve functions are the `<algorithm>` modifying-sequence operations (sections 14–15 of `Church.hs`) — the index/structure transforms (`take`, `drop`, `rotate`), the predicate/equality *erasures* (`removeIf`, `remove`, `replaceIf`, `replace`, `uniqueBy`), and the two *partition/sort predicates* (`isPartitioned`/`partitionPoint`, `isSorted`/`isSortedUntil`). As everywhere in this module, order/equality come in as explicit `LE a`/`(a -> a -> Bool)` arguments (no type classes), every branch is a Church `Bool` applied to two continuations rather than `if`, and the only constructors are `cons`/`nil`/`pair`/`just`/`nothing` from `Church.hs`. The `Int`-valued arithmetic helper `leInt` (and `length` for the index-returning ones) is the lone non-Church machinery, and it too *returns* Church `Bool`s that select a branch.

All expected values below are pinned by the `algorithmTests` group in [`test/Spec.hs`](../test/Spec.hs) (lines 649–662), evaluated with `le x y = x <= y` and `eq x y = x == y` on `Int`.

##### `take` / `drop` — prefix and suffix by count

> `take` and `drop` live with the Wolfram-style list operations in section 5 of the
> source (alongside `splitAt`/`takeLast`/`dropLast`) and are documented in
> [Part I §4](#4-folds-maps-filters-and-the-everyday-list-toolkit); the definitions
> below are reproduced verbatim.

```haskell
take :: Int -> List a -> List a
take n xs = zipWith const xs $ replicate n undefined

drop :: Int -> List a -> List a
drop n = compose $ replicate n tail
```

These are `std::copy_n`/`std::ranges::take` and the dual `drop`: the first `n` elements, and everything after them, each clamped at the list length. Neither writes its own recursion — both are one-liners over `replicate` (whose `leInt n 0` base case is the only place the count is compared, returning a Church `Bool` that *is* the base/step split). `take n xs = zipWith const xs (replicate n undefined)` zips the list against an `n`-element ruler of `undefined`s: `zipWith` truncates at the shorter list — clamping for free — and `const` keeps the `xs`-side element, so the `undefined`s are never forced (laziness as load-bearing design). `drop n = compose (replicate n tail)` composes `n` copies of `tail` (`compose = foldr (.) id`, the endomorphism folder); each application peels one head, and Church's total `tail nil = nil` supplies the clamping.

Worked: `take 3 [1,2,3,4,5]` zips against `[⊥,⊥,⊥]` — three pairs, `const` picks left — giving **`[1,2,3]`**. `drop 2 [1,2,3,4,5]` is `tail (tail [1,2,3,4,5])` = **`[3,4,5]`**.

##### `rotate` — left rotation by `n`

```haskell
rotate :: Int -> List a -> List a
rotate = liftA2 (liftA2 append) drop take
```

`std::rotate` with the new "first" element at index `n`: it moves the first `n` elements to the back. The encoding is purely compositional — `append (drop n xs) (take n xs)` — reusing the two functions above and `Church.append`; the rotation point is expressed entirely as "the suffix `drop n xs`, then the prefix `take n xs`." No control flow of its own.

Worked: `rotate 2 [1,2,3,4,5] = append (drop 2 …) (take 2 …) = append [3,4,5] [1,2]` = **`[3,4,5,1,2]`**.

##### `removeIf` / `remove` — predicate- and value-driven deletion

```haskell
removeIf :: (a -> Bool) -> List a -> List a
removeIf p = filter (not . p)

remove :: (a -> b -> Bool) -> b -> List a -> List a
remove eq = removeIf . flip eq
```

`std::remove_if` and `std::remove` — but, unlike the C++ "stable-partition-then-erase" idiom, these actually *shrink* the list (there is no in-place end iterator to return). `removeIf` is one line over `Church.filter`: keep exactly the elements where `not (p x)` holds. The negated predicate `\x -> not (p x)` returns a Church `Bool` that `filter` consumes per element; nothing else is needed. `remove` then specializes the predicate to "equals `v` under `eq`" via the standard correspondence-relation generalization (`eq :: a -> b -> Bool`, so the sought value `v :: b` need not share the element type), reusing `removeIf` wholesale.

Worked: `removeIf even [1,2,3,4]` drops `2` and `4`, keeping the odds ⇒ **`[1,3]`**. `remove eq 2 [1,2,3,2]` deletes *every* element equal to `2` (not just the first — contrast `deleteBy`) ⇒ **`[1,3]`**.

##### `replaceIf` / `replace` — in-place substitution via `map`

```haskell
replaceIf :: (a -> Bool) -> a -> List a -> List a
replaceIf p new = map (\x -> p x new x)

replace :: (a -> a -> Bool) -> a -> a -> List a -> List a
replace eq = replaceIf . flip eq
```

`std::replace_if`/`std::replace`: overwrite every element satisfying the predicate (resp. equal to `old`) with `new`, keeping length and position. The whole substitution is hidden inside one `map`, and the per-element choice is *the Church `Bool` itself*: `p x new x` reads "apply the Bool `p x` to the two arms `new` and `x`," so a true predicate selects `new`, a false one keeps the original `x` — `true = const`, `false = const id` does the multiplexing with no `if`. `replace` just instantiates `p = \x -> eq x old`.

Worked: `replaceIf even 0 [1,2,3,4]` maps each element through `even x 0 x`, replacing the evens ⇒ **`[1,0,3,0]`**. `replace eq 2 9 [1,2,3,2]` rewrites both `2`s to `9` ⇒ **`[1,9,3,9]`**.

##### `uniqueBy` — collapse adjacent runs

```haskell
uniqueBy :: ∀a. (a -> a -> Bool) -> List a -> List a
uniqueBy eq = map @(List a) head . groupBy eq
```

`std::unique`: collapse each *consecutive* run of `eq`-equal elements to its first representative (non-adjacent equals survive — this is dedup-by-adjacency, not global `nub`). The two-stage pipeline reads as its own specification: `groupBy eq` (§10) splits the list into maximal adjacent runs related to each run's first element, and `map head` keeps exactly that first element of every run. All the anchor-tracking the old hand recursion did lives inside `groupBy`'s `span`; the `@(List a)` on `map` is a sweep survivor pinning the rank-N element type of the group list.

Worked: `uniqueBy eq [1,1,2,3,3,3,1]` groups to `[[1,1],[2],[3,3,3],[1]]`, whose heads are **`[1,2,3,1]`** — the trailing `1` survives because it is adjacent to a `3`, not to the earlier `1`s.

##### `isPartitioned` / `partitionPoint` — partition predicates

```haskell
isPartitioned :: (a -> Bool) -> List a -> Bool
isPartitioned p = none p . dropWhile p

partitionPoint :: (a -> Bool) -> List a -> Int
partitionPoint p = length . takeWhile p
```

`std::is_partitioned` and `std::partition_point`. `isPartitioned` checks the partition shape "all `p` then all `not p`" by a two-stage erasure: `dropWhile p xs` discards the leading `p`-block, and `none p` (= `not . any p`, from §13) asserts the remainder contains *no* further `p`-element. If any `p`-element appears after a non-`p` one, it survives the `dropWhile` and `none` reports `false`. `partitionPoint` returns the boundary index as `length (takeWhile p xs)` — the count of the leading `p`-prefix, i.e. the index of the first non-`p` element (`std::partition_point` assumes the input is already partitioned). Both reuse `Church`'s `dropWhile`/`takeWhile`/`length` and add no eliminators of their own.

Worked: `isPartitioned even [2,4,1,3]` ⇒ `dropWhile even = [1,3]`, `none even [1,3] = true` ⇒ **`True`**; `isPartitioned even [2,1,4]` ⇒ `dropWhile even = [1,4]`, but `4` is even so `none even [1,4] = false` ⇒ **`False`**. `partitionPoint even [2,4,1,3]` ⇒ `takeWhile even = [2,4]`, length **`2`**.

##### `isSorted` / `isSortedUntil` — sortedness predicates

```haskell
isSorted :: LE a -> List a -> Bool
isSorted le = all (uncurry le) . (zip <*> tail)

isSortedUntil :: LE a -> List a -> Int
isSortedUntil le xs = findIndex (not . uncurry le)
  (zip xs $ tail xs) (length xs) succ
```

`std::is_sorted` and `std::is_sorted_until`. `isSorted` is one line of reuse: a list is sorted iff *every adjacent pair satisfies `le`* — and the adjacent pairs are the self-`zip` `zip xs (tail xs)`, written applicatively as `(zip <*> tail)` (in the function applicative, `(zip <*> tail) xs = zip xs (tail xs)`). `all (uncurry le)` then demands `le` on every window. Empty and singleton lists have no windows, so `all` over the empty list gives `true`; and since `all` (an `and`-fold) short-circuits, `isSorted` still stops at the first out-of-order pair. `isSortedUntil` is the index-returning twin and is *also* recursion-free: `findIndex (not . uncurry le)` locates the first *violating* window in the same zipped list, and the resulting `Maybe Int` is eliminated with the arms `(length xs)` (no violation ⇒ the whole list is the sorted prefix) and `succ` (window `i` spans elements `i` and `i+1`, so the first element *breaking* sortedness sits at `i + 1` — equivalently the sorted prefix has length `i + 1`). An empty list has no windows and returns `length nil = 0`.

Worked: `isSorted le [1,2,2,3]` — windows `(1,2)`, `(2,2)`, `(2,3)`; `le` holds at each, so `all` gives **`True`**. `isSorted le [1,3,2]` — window `(1,3)` passes, window `(3,2)` fails (`3<=2` is false), `all` short-circuits to **`False`**. `isSortedUntil le [1,2,1,3]` — windows `(1,2),(2,1),(1,3)`; the first violation is window `1` (`2<=1` false), and `succ 1` = **`2`**.

---

#### Binary search, merge and set operations on sorted ranges

This subsection covers `Church.hs` §16 (binary search) and §17 (merge / set operations) — the `<algorithm>` family that *assumes its input ranges are already `le`-sorted*. Two design notes apply throughout. First, since the encoding has no random access, the four "binary search" queries are realized as **linear scans whose result still matches `std::lower_bound`/`std::upper_bound`/`std::binary_search`/`std::equal_range`** — the O(log n) iterator arithmetic of C++ is traded for an O(n) `countIf`/`any`, but the *return value* is identical on a sorted range. Second, the order relation `le :: LE a = a -> a -> Bool` is the only conditional primitive: every "is `x` less than `y`?", "are they equivalent?" decision is a Church `Bool` applied to two continuations, `le x y (thenArm) (elseArm)`, with strict-less and equivalence both *derived* from `le` alone. Because `le` is total, strict-less needs only **one** call: `x < y` is `not (le y x)` (the identity `ltFromLE` in `Church` packages); equivalence `x ≡ y` (the only equality these functions know) is the conjunction `le x y ∧ le y x` (`eqFromLE`). No `if`, no ADTs, no `Ord`.

##### §16 — Binary search (`lowerBound`, `upperBound`, `binarySearch`, `equalRange`)

```haskell
-- | 'lowerBound' is @std::lower_bound@: the index of the first element not less than
--   @v@ (equivalently, the count of elements strictly less than @v@).
lowerBound :: LE a -> a -> List a -> Int
lowerBound le v = countIf (not . le v)

-- | 'upperBound' is @std::upper_bound@: the index of the first element greater than
--   @v@ (equivalently, the count of elements not greater than @v@).
upperBound :: LE a -> a -> List a -> Int
upperBound le = countIf . flip le

-- | 'binarySearch' is @std::binary_search@: whether an element equivalent to @v@
--   (under @le@) is present.
binarySearch :: LE a -> a -> List a -> Bool
binarySearch le = any . eqFromLE le

-- | 'equalRange' is @std::equal_range@: the @('lowerBound', 'upperBound')@ pair.
equalRange :: LE a -> a -> List a -> Int `Pair` Int
equalRange le v = liftA2 pair (lowerBound le v) (upperBound le v)
```

These four exploit the standard identity that, on a sorted range, *a positional binary-search result is a count*. `lowerBound` (`std::lower_bound`) returns the index of the first element `≥ v`; on a sorted list that index equals the number of elements strictly `< v`, and by totality `x < v` is the *single* comparison `not (le v x)` — hence `countIf (not . le v)`, one `le` call per element where the pre-golf version spent two. `upperBound` (`std::upper_bound`) returns the first index `> v`, i.e. the count of elements `≤ v`, hence `countIf (flip le v)` — no `not` at all. The counting itself is `countIf`'s `length . filter p` pipeline; the comparison `le` is consumed as the `filter` predicate's Church Boolean. `binarySearch` (`std::binary_search`) does not need a position — only presence of an element *equivalent* to `v` — so it is `any (eqFromLE le v)`; and because `eqFromLE`'s conjunction `le a b ∧ le b a` is **symmetric** in its arguments, partially applying it to `v` on the left tests exactly the same relation the old `flip`ped spelling did — the symmetry is what let the golf drop the `flip`. `equalRange` (`std::equal_range`) is the pure pairing of the other two into a Church `Pair`; on a sorted range `[lowerBound, upperBound)` is precisely the half-open span of elements equivalent to `v`, and its width `upperBound − lowerBound` is the multiplicity of `v`.

**Worked example** (the `algorithmTests` cases on `[1,2,2,3]`): with `le x y = x ≤ y` and `v = 2`,

- `lowerBound le 2 [1,2,2,3]` counts elements `< 2`: only `1` qualifies ⇒ **`1`**.
- `upperBound le 2 [1,2,2,3]` counts elements `≤ 2`: `1,2,2` qualify ⇒ **`3`**.
- `equalRange le 2 [1,2,2,3]` = `(1, 3)` — the two `2`s occupy the half-open index range `[1,3)`, multiplicity `3 − 1 = 2`.
- `binarySearch le 3 [1,2,3,4]` finds `3` ≡ `3` ⇒ **`True`**; `binarySearch le 9 [1,2,3,4]` finds no equivalent ⇒ **`False`**.

##### §17 — Merge and set operations (`mergeBy`, `includes`, `setUnion`, `setIntersection`, `setDifference`, `setSymmetricDifference`)

These five are the classic *simultaneous linear scan of two sorted ranges* — and since "what each arm emits" is the *only* thing that ever differs between them, the scan itself is written **once**, as the private engine `setOp le left right eq both`, and every public operation is a row of four Church-`Boolean` flags. Inside `setOp`'s `go`, `caseList` on each list supplies the base cases, and the three-way head classification is two nested Church-`Bool` applications: the outer `le x y` separates "`x` not after `y`" from "`x` strictly after `y`", and *inside* the true-arm a second `le y x` distinguishes equivalence (`le y x` true ⇒ `x ≡ y`) from strict-less. This `le x y (le y x (≡-arm) (<-arm)) (>-arm)` idiom is the load-bearing pattern. The flags then read: `left` — emit `x` in the `<`-arm; `right` — emit `y` in the `>`-arm; `eq` — emit `x` on equivalence; `both` — on equivalence, consume *both* heads (versus only `x`). One lovely economy pays for the small flag count: for every `std::set_*` operation, "emit `x` when `x < y`" coincides with "emit the leftovers of `xs` when `ys` runs out", and likewise on the right — so `left`/`right` each serve *both* roles, in the base cases (`caseList (right ys nil) …`, `caseList (left xs nil) …` — the flag decides whether the surviving list or `nil` is returned) and in the strict arms. Each function below quotes its flag row.

```haskell
-- | 'mergeBy' is @std::merge@: merges two @le@-sorted lists into one sorted list,
--   preserving duplicates (stable: equal elements keep @xs@-before-@ys@ order).
mergeBy :: LE a -> List a -> List a -> List a
mergeBy le = setOp le true true true false
```

`mergeBy` (`std::merge`) is the flag row `true true true false`: it never drops anything, it interleaves. `left`/`right` both true means both strict arms emit and both leftovers survive (empty `xs` ⇒ the rest of `ys`, and vice versa). `eq = true` emits `x` on equivalence, and `both = false` then advances **only `xs`** — so the equal `y` stays in play and is emitted on a later round: duplicates are preserved, and equal elements come out `xs`-before-`ys`. That left-bias on ties is exactly what makes the merge **stable**, matching `std::merge`. It is also the merge phase of §11.1's `mergesort` and the insertion engine of §11.3's `heapsort`, which call it directly. Example: `mergeBy le [1,3,5] [2,4]` yields **`[1,2,3,4,5]`** (compare `1`≤`2`→`1`; `3`>`2`→`2`; `3`≤`4`→`3`; `5`>`4`→`4`; `ys` empty → emit leftovers `[5]`).

```haskell
-- | 'includes' is @std::includes@: whether sorted @xs@ contains sorted @ys@ as a
--   (multiset) subsequence.
includes :: LE a -> List a -> List a -> Bool
includes le xs ys = null (setDifference le ys xs)
```

`includes` (`std::includes`) tests multiset containment: is every element of sorted `ys` matched, with multiplicity, somewhere in sorted `xs`? Rather than running its own scan it asks `setDifference` (below): subtract `xs` from `ys` — each equivalence in `xs` cancels one occurrence in `ys` — and containment holds exactly when *nothing of `ys` survives*, i.e. `null (setDifference le ys xs)`. (Note the argument swap: the subtrahend is `xs`.) Laziness keeps this an honest scan: `null` only probes whether the difference list has a head, so the subtraction stops at the first surviving `ys`-element. Examples: `includes le [1,2,2,3] [2,3]` ⇒ **`True`** (both demanded elements cancel); `includes le [1,2,3] [2,2]` ⇒ **`False`** (only one `2` available for two demanded — one survives the difference).

```haskell
-- | 'setUnion' is @std::set_union@ on sorted ranges.
setUnion :: LE a -> List a -> List a -> List a
setUnion le = setOp le true true true true
```

`setUnion` (`std::set_union`) is `true true true true` — one flag away from `mergeBy`: on an equivalence it still emits `x`, but `both = true` now advances **both** (`xs'`, `ys'`), so a value present in both inputs appears once per matched pair. The `x < y` arm emits `x` and advances `xs`; the `x > y` arm emits `y` and advances `ys`; both leftovers survive. (As in `std::set_union`, if a value has multiplicity *m* in one input and *n* in the other, the result carries `max(m,n)` copies; with deduplicated inputs this is the ordinary set union.) Example: `setUnion le [1,2,3] [2,3,4]` ⇒ **`[1,2,3,4]`** (`1`<`2`→`1`; `2`≡`2`→`2`, both advance; `3`≡`3`→`3`, both advance; `xs` empty → leftovers `[4]`).

```haskell
-- | 'setIntersection' is @std::set_intersection@ on sorted ranges.
setIntersection :: LE a -> List a -> List a -> List a
setIntersection le = setOp le false false true true
```

`setIntersection` (`std::set_intersection`) is `false false true true` — keep **only** the equivalence case: `x ≡ y` emits `x` and advances both; the false `left`/`right` make `x < y` drop `x`, `x > y` drop `y`, *and* discard both leftovers (either list empty ⇒ `nil`, the same flags doing base-case duty). So an element survives exactly when it has a match remaining in the other range. Example: `setIntersection le [1,2,3] [2,3,4]` ⇒ **`[2,3]`** (`1`<`2`→drop `1`; `2`≡`2`→keep; `3`≡`3`→keep; `xs` exhausts, so the remaining `4` in `ys` finds nothing to match).

```haskell
-- | 'setDifference' is @std::set_difference@ on sorted ranges (@xs@ minus @ys@).
setDifference :: LE a -> List a -> List a -> List a
setDifference le = setOp le true false false true
```

`setDifference` (`std::set_difference`, "`xs` minus `ys`") is `true false false true`: it **drops** on equivalence (`eq = false`) and **keeps on strict-less** (`left = true`). The same `left = true` returns the rest of `xs` when `ys` empties — once the subtrahend is gone, everything left in `xs` survives — while `right = false` makes empty `xs` ⇒ `nil` and silently consumes `y` in the `>`-arm (this `y` removes nothing here). With both heads: `x ≡ y` emits nothing and advances both (the matched element is removed); `x < y` emits `x` (it has no equal in `ys`) and advances `xs`. Example: `setDifference le [1,2,3,4] [2,4]` ⇒ **`[1,3]`** (`1`<`2`→keep `1`; `2`≡`2`→drop; `3`<`4`→keep `3`; `4`≡`4`→drop).

```haskell
-- | 'setSymmetricDifference' is @std::set_symmetric_difference@ on sorted ranges.
setSymmetricDifference :: LE a -> List a -> List a -> List a
setSymmetricDifference le = setOp le true true false true
```

`setSymmetricDifference` (`std::set_symmetric_difference`) is `true true false true` — it emits an element exactly when it appears in *one* input but not the other, so it keeps both strict arms and drops only the equivalence arm. Its base cases match `setUnion`'s (both leftovers survive), because elements with no counterpart pass through untouched. With both heads: `x ≡ y` cancels (emit nothing, advance both); `x < y` emits `x` and advances `xs`; `x > y` emits `y` and advances `ys`. Example: `setSymmetricDifference le [1,2,3] [2,3,4]` ⇒ **`[1,4]`** (`1`<`2`→keep `1`; `2`≡`2`→cancel; `3`≡`3`→cancel; `xs` empty → leftovers `[4]`).

All ten functions are exercised by the `algorithmTests` group in [`test/Spec.hs`](../test/Spec.hs); the boldfaced values above are the exact `assertEqual` expectations there.

---

### Min/max, comparison and permutation operations

These are the Church-encoded analogues of the `<algorithm>` minimum/maximum group (`max`, `min`, `minmax`, `clamp`, `minmax_element`), the comparison group (`equal`, `lexicographical_compare`, `lexicographical_compare_three_way`), and the permutation group (`is_permutation`, `next_permutation`, `prev_permutation`). They live in **§18–§19** of the source. The unifying mechanic is the one that runs through the whole module: an order relation `le :: LE a = a -> a -> Bool` (or a heterogeneous equality `eq :: a -> b -> Bool`) returns a *Church `Bool`*, i.e. `∀e. e -> e -> e`, so `le x y t f` **is** the branch — `true = const`, `false = const id` pick `t` or `f`. There is no `if`, no `case`, no ADT; every decision is a `Bool`/`Maybe` applied to its arms, and every two-valued result (a `(min,max)` pair, an `Either`-free ordering) is a Church `Pair` or a bare `Int`. All expected values below are pinned by the `algorithmTests` group in [`test/Spec.hs`](../test/Spec.hs) (lines 675–699).

#### `maxBy` / `minBy` — `std::max` / `std::min` as pure `Bool`-selection

```haskell
maxBy :: LE a -> a -> a -> a
maxBy le x y = le x y y x

minBy :: LE a -> a -> a -> a
minBy le x y = le x y x y
```

There is no eliminator and no recursion here at all: the comparison `le x y :: Bool` is *itself* the chooser, applied to the two candidate results. For `maxBy`, `le x y` true (`x ≤ y`) selects `y`, false selects `x` — i.e. the form `(x ≤ y) ? y : x`. Because it consults `x ≤ y` rather than `x < y`, on a tie (`x ≤ y` holds) it returns the **second** argument `y`; this is exactly the source's stated "returns the second on a tie." (Note this differs from classic `std::max`, defined as `(a < b) ? b : a`, which returns the *first* argument when neither is greater.) `minBy` is the mirror: `le x y` true selects `x`, false selects `y`. These two are the atoms the rest of the section is built from.

**Examples.** `maxBy le 3 5 = 5` and `minBy le 3 5 = 3` (`le 3 5 = true`; the first picks the 2nd arm `5`, the second the 1st arm `3`).

#### `minmaxBy` — `std::minmax` returning a Church `Pair`

```haskell
minmaxBy :: LE a -> a -> a -> a `Pair` a
minmaxBy le = liftA2 (liftA2 pair) (minBy le) (maxBy le)
```

`std::minmax(a,b)` returns `{min,max}`; here that two-element bundle is a Church `Pair`, built by the *applicative pairing* of the two atoms: `liftA2 (liftA2 pair) (minBy le) (maxBy le)` runs `minBy le x y` and `maxBy le x y` on the same two arguments (the doubled `liftA2` threads them through both binary functions) and pairs the results. Two comparisons, but guaranteed consistency: on a tie `minBy` picks the *first* argument and `maxBy` the *second*, so the pair is `(x, y)` — each input accounted for exactly once, matching the source's tie contract.

**Example.** `minmaxBy le 5 3 = (3,5)`: `minBy le 5 3 = le 5 3 5 3 = 3` and `maxBy le 5 3 = le 5 3 3 5 = 5`; `pair` bundles them as `(3,5)`.

#### `clamp` — `std::clamp` as `max lo (min hi x)`

```haskell
clamp :: LE a -> a -> a -> a -> a
clamp le lo hi = maxBy le lo . minBy le hi
```

The textbook identity `clamp(x,lo,hi) = max(lo, min(hi, x))`, composed directly from the two atoms above — so it inherits their `Bool`-selection and adds no control flow of its own. `minBy le hi x` caps `x` at `hi` from above; `maxBy le lo (…)` lifts the result to at least `lo`. (As with `std::clamp`, the precondition `lo ≤ hi` is assumed, not checked.)

**Examples.** `clamp le 0 10 15 = 10` (above the ceiling: `min 10 15 = 10`, `max 0 10 = 10`); `clamp le 0 10 (-5) = 0` (below the floor: `min 10 (-5) = -5`, `max 0 (-5) = 0`); `clamp le 0 10 5 = 5` (in range, passes through both).

#### `minmaxElement` — `std::minmax_element` via two `foldl1` sweeps

```haskell
minmaxElement :: LE a -> List a -> a `Pair` a
minmaxElement le = liftA2 pair (foldl1 (minBy le)) (foldl1 (maxBy le))
```

Computes the `(minimum, maximum)` of a non-empty list as the applicative pairing of two seedless left folds: `foldl1 (minBy le)` sweeps for the minimum, `foldl1 (maxBy le)` for the maximum, and `liftA2 pair` runs both on the same list and bundles the results in a Church `Pair`. On `nil` the error is `foldl1`'s own (`"foldl1: empty list"` — empty input is undefined, as in C++). The tie contract falls out of the atoms: `minBy` keeps the *first* of equals, `maxBy` the *second*, so the pair is (first minimum, last maximum) — exactly `std::minmax_element`'s iterator convention. (An earlier revision threaded a `(lo,hi)` accumulator pair through one fold; two `foldl1`s of the existing atoms say the same thing with no pair plumbing.)

**Example.** `minmaxElement le [3,1,4,1,5] = (1,5)`: the min sweep runs `3→1→1→1→1`, the max sweep `3→3→4→4→5`; `pair` bundles `(1,5)`.

#### `equalBy` — `std::equal` (element-wise, length-sensitive)

```haskell
equalBy :: (a -> b -> Bool) -> List a -> List b -> Bool
equalBy eq xs ys = and (isPrefixOf eq xs ys) (isPrefixOf (flip eq) ys xs)
```

Heterogeneous pointwise equality under a correspondence `eq :: a -> b -> Bool`, returning `true` only when the lists agree element-by-element **and** have equal length — stated as *mutual prefixhood*: `xs` is a prefix of `ys` **and** `ys` is a prefix of `xs`. Two lists that are each other's prefixes must be pointwise-equal and equally long, so the definition is the `and` of two `isPrefixOf` calls (§10's counting pipeline), the second with the relation `flip`ped since the heterogeneous `eq` gets its `a` from `xs`. This is exactly the length-checking variant of `std::equal` (the four-iterator overload), not the "first range only" three-iterator one.

**Examples.** `equalBy eq [1,2,3] [1,2,3] = True` (each is a prefix of the other); `equalBy eq [1,2] [1,2,3] = False` (`[1,2]` *is* a prefix of `[1,2,3]`, but not vice versa — the second `isPrefixOf` counts only 2 matched pairs against `length [1,2,3] = 3`).

#### `lexicographicalLess` — `std::lexicographical_compare`

```haskell
lexicographicalLess :: LE a -> List a -> List a -> Bool
lexicographicalLess le = ltFromLE (lexicographicLE le)
```

Strict lexicographic "less than" under `le` — as a one-liner of pure algebra. `lexicographicLE le` (from `Church`) is a *total* less-than-or-equal on lists, and for any total `≤` the strict order is `x < y ⟺ ¬(y ≤ x)` — which is precisely what `ltFromLE` packages. So `lexicographicalLess le = ltFromLE (lexicographicLE le)`: one call to the list order with the arguments swapped, one `not`, no list traversal of its own. All the positional work — proper-prefix rule, first-strict-difference-decides — lives inside `lexicographicLE`; this function only applies the totality identity at the list level.

**Examples.** `lexicographicalLess le [1,2] [1,3] = True` (equal at `1`, then `2 < 3`); `lexicographicalLess le [1] [1,2] = True` (equal at `1`, then `xs` empties against non-empty `ys` ⇒ `true`); `lexicographicalLess le [1,2] [1,2] = False` (equal throughout, then `ys` empties ⇒ `false`).

#### `compareBy` — `std::lexicographical_compare_three_way`

```haskell
compareBy :: LE a -> List a -> List a -> Int
compareBy le xs ys = lexicographicLE le xs ys 0 1 - lexicographicLE le ys xs 0 1
```

The three-way version, returning `-1 | 0 | 1` for `xs < ys | xs ≡ ys | xs > ys`. Because the module admits no ADTs, a bare `Int` stands in for `std::strong_ordering` (`less`/`equal`/`greater`) — and the encoding is a little arithmetic gem: query the total list order **twice**, once each way, mapping each Church `Bool` to an `Int` (`lexicographicLE le xs ys 0 1` reads "0 if `xs ≤ ys`, else 1"), and *subtract*. `xs < ys` gives `0 − 1 = -1`; equivalence gives `0 − 0 = 0`; `xs > ys` gives `1 − 0 = 1`. The Church `Bool`s applied to `Int` arms *are* the case analysis, and ordinary subtraction fuses the two answers into the three-way verdict.

**Examples.** `compareBy le [1,2] [1,3] = -1` (equal at `1`, then `2 < 3`); `compareBy le [1,2] [1,2] = 0` (equal, then both empty); `compareBy le [2] [1] = 1` (`2 > 1` at the head).

#### `isPermutation` — `std::is_permutation` (multiset equality)

```haskell
isPermutation :: (a -> a -> Bool) -> List a -> List a -> Bool
isPermutation eq xs ys = and (eqInt (length xs) (length ys))
  (null (deleteFirstsBy eq ys xs))
```

True iff `ys` is a rearrangement of `xs` — same elements with the same multiplicities, under `eq`. The `and (eqInt (length xs) (length ys)) …` guard is the cheap length precheck `std::is_permutation` also performs. The work is a bag subtraction: `deleteFirstsBy eq ys xs` (§10's fold of `deleteBy`) removes from `ys` **one** matching occurrence for each element of `xs` — one-at-a-time deletion is what makes it multiset, not set, equality — and `null` demands nothing survives. With the length guard already ensuring `|xs| = |ys|`, an empty difference means every element of `xs` cancelled a distinct element of `ys`, i.e. a perfect matching.

**Examples.** `isPermutation eq [1,2,3] [3,2,1] = True` (each of `1,2,3` deletes its occurrence, the difference empties); `isPermutation eq [1,2,3] [1,2,4] = False` (lengths match, but `3` deletes nothing and the leftover `4` survives the difference).

#### `nextPermutation` — `std::next_permutation`

```haskell
nextPermutation :: LE a -> List a -> Maybe (List a)
nextPermutation le = caseList nothing $ \x xs ->
  nextPermutation le xs
    (findIf (ltFromLE le x) (quicksort le xs)
      nothing
      (\m -> just (m `cons` mergesort le (x `cons` deleteBy (eqFromLE le) m xs))))
    (just . cons x)
```

Produces the next-greater permutation in lexicographic order, or `nothing` if the input is already the maximal (fully descending) arrangement — exactly `std::next_permutation`'s contract, except the C++ in-place wrap-to-first-on-`false` is replaced by a `Maybe`. The recursion walks suffixes via `caseList` (empty ⇒ `nothing`): the `Maybe` returned by the recursive `nextPermutation le xs` on the tail *is* the branch. The just-arm `just . cons x` means "the tail already had a next permutation; keep `x` and prepend." The nothing-arm handles the pivot position — the tail is fully descending, so `x` itself must be replaced by its successor within the suffix. The old bespoke `findMinGreater` fold is now a one-liner of reuse: `findIf (ltFromLE le x) (quicksort le xs)` sorts the suffix ascending and takes the *first* element strictly greater than `x` — the first hit in ascending order *is* the minimum-greater, so no running-minimum bookkeeping is needed. (By totality, `ltFromLE le x m` is the single comparison `¬(le m x)`.) If no such element exists (`nothing`), the whole list from here is descending and `nothing` bubbles up; if some `m` exists, emit `m \`cons\` mergesort le (x \`cons\` deleteBy (eqFromLE le) m xs)` — place the successor, then sort the pivot together with the suffix-minus-one-`m` ascending, the canonical "swap pivot with successor, reverse the still-descending tail" specialized to a re-sort. Both derived relations are the named `Church` combinators now: `eqFromLE le` (for `deleteBy`'s one-occurrence removal) and `ltFromLE le` (strict).

**Examples.** `nextPermutation le [1,2,3] = Just [1,3,2]` (suffix `[2,3]` is ascending, so the deepest recursion finds its own next; net effect, swap the last ascending pair); `nextPermutation le [3,2,1] = Nothing` (fully descending — every suffix search comes up empty). The suite further checks that iterating `nextPermutation` from `[1,2,3]` enumerates all `Data.List.permutations [1,2,3]` in sorted agreement, and that the chain from `[1,2,3,4]` has length `24 = 4!` (`test/Spec.hs:695–699`).

#### `prevPermutation` — `std::prev_permutation` by relation duality

```haskell
prevPermutation :: LE a -> List a -> Maybe (List a)
prevPermutation le = nextPermutation (flip le)
```

A one-liner: the previous permutation under `le` is the *next* permutation under the **flipped** order `\a b -> le b a`. Reversing the comparison reverses lexicographic direction, so `nextPermutation` on the dual relation walks the sequence backwards — no separate descent logic needed. This is the cleanest possible statement of the `next`/`prev` symmetry, made trivial because the order is a first-class argument rather than a type-class instance.

**Example.** `prevPermutation le [2,1,3] = Just [1,3,2]` — under the flipped order, `[2,1,3]`'s "next" is `[1,3,2]`, i.e. the lexicographic predecessor.

---

## 14. `<map>`-style dictionary operations

Sections 20-22 of `Church.hs` extend the association-list dictionary of [chapter 8](#8-dictionary-operations) with the *relation* surface of C++ `std::map` / `std::unordered_map` and Haskell's `Data.Map`: queries, construction, keyed update, filtering, two-dictionary combinators, and folds. A `Dict k v = List (k `Pair` v)` is an unordered association list with unique keys under a passed `Equal k`. The container/hash machinery (buckets, node handles, key-ordered iteration) has no analogue on a linearly-scanned list and is intentionally absent. Every example is pinned by the `dictAlgoTests` group in [`test/Spec.hs`](../test/Spec.hs) (59 HUnit cases; full suite 403/403 via `cabal test` on GHC 9.12.4).

---

### Dictionary query and construction

This section documents the query/construction half of the `<map>`/`<unordered_map>` layer (section 20 of `Church.hs`). A `Dict k v = List (k `Pair` v)` is an association list whose keys are unique under a caller-supplied `Equal k`; these twelve functions read or build such a dict without mutating it. They mirror the *relation* surface of `std::map` / `std::unordered_map` (and `Data.Map`) — the container/hash/ordered-iteration machinery (buckets, node handles, key-ordered `lower_bound`, …) has no analogue on a linearly-scanned list and is intentionally absent. As everywhere in this module, key equality is **passed in explicitly** (there are no type classes to dispatch on), and several queries *relax* it to a heterogeneous correspondence `a -> c -> Bool`, so the probe key type `c` need not equal the stored key type `a`.

Two encoding facts drive the implementations, neither using `if`-`then`-`else`, ADTs, or pattern matching:

1. **The query group is a thin skin over `lookup`.** `member`, `notMember`, `findWithDefault`, and `atKey` all call `lookup eq key d`, whose result is a Church `Maybe b` — *itself its own eliminator* (`m nothingArm justArm`). `findWithDefault`/`atKey` apply that `Maybe` to two continuations (via `fromMaybe`), and `member`/`notMember` reflect it through `isJust`/`isNothing`; either way the present/absent case split is nothing but function application.
2. **The structural ops are generic `List`/`Pair` eliminators at the dict element type.** `keys`/`values`/`elems`/`sizeDict`/`nullDict` reuse `Church`'s `map`/`fst`/`snd`/`length`/`null`, and because a `Dict k v` element is the *polytype* `k `Pair` v` (a rank-N value, not a vanilla tuple), the call sites carry a TypeApplication `@(k `Pair` v)` so GHC 9.12's stricter impredicative instantiation can pin the generic list function's element type. These are semantically identity-level annotations — read past them — but they are why a generic list function can meet the polymorphic dict element.

The reference dictionaries used in the traces below come straight from `dictAlgoTests` (`test/Spec.hs:752`): `d12 = [(1,10),(2,20)]` and the multimap `dmm = [(1,10),(2,20),(1,30)]` (duplicate key `1`), with `eq x y = (x == y)`.

#### `member` — is the key present?

```haskell
-- | 'member' is C++20 @std::map::contains@: is @key@ present?
member :: (a -> c -> Bool) -> c -> Dict a b -> Bool
member eq key = isJust . lookup eq key
```

`member` is `std::map::contains` / `std::map::count`-as-bool (and `Data.Map.member`). It runs `lookup eq key d :: Maybe b` and asks `isJust` (§24's `not . isNothing`, which reflects the `Maybe` through `maybeToList`/`null`): a hit yields `true`, a miss `false`. The found value is never inspected, only presence. The heterogeneous `eq :: a -> c -> Bool` lets the probe `key :: c` differ in type from the stored keys.

**Trace** — `member eq 2 d12`:

```text
lookup eq 2 [(1,10),(2,20)]
  lookupAll keeps the matching entries: (1,10) dropped, (2,20) kept → values [20]
  listToMaybe [20] = just 20
isJust (just 20) = not (isNothing (just 20)) = not false = true
```

`member eq 9 d12` matches no entry, so `lookup` yields `nothing` and `isJust nothing = false`. The `dictAlgoTests` cases pin `member eq 2 d12 = True` and `member eq 9 d12 = False`.

#### `notMember` — key absent?

```haskell
-- | 'notMember' is the negation of 'member'.
notMember :: (a -> c -> Bool) -> c -> Dict a b -> Bool
notMember eq key = isNothing . lookup eq key
```

The pointwise negation of `member` (`Data.Map.notMember`; `!m.contains(k)` in C++). It is the same `lookup`, asked the complementary question: `isNothing` (`null . maybeToList`) reports `true` exactly when the `Maybe` is `nothing`. No new control flow — presence testing is again `Maybe` reflection, with the negation baked into which question is asked (`isJust` is itself `not . isNothing`).

**Trace** — `notMember eq 9 d12`:

```text
lookup eq 9 [(1,10),(2,20)] = nothing   (no key matches)
isNothing nothing = null (maybeToList nothing) = null nil = true
```

The test pins `notMember eq 9 d12 = True`.

#### `findWithDefault` — value or fallback

```haskell
-- | 'findWithDefault' is @std::map::at@ generalized with a fallback (@Data.Map.findWithDefault@).
findWithDefault :: b -> (a -> c -> Bool) -> c -> Dict a b -> b
findWithDefault = flip lookupDefault
```

This is `Data.Map.findWithDefault` — `std::map::at` made total by supplying a fallback `def` (morally `m.contains(k) ? m.at(k) : def`). Again the body is `lookup`-as-eliminator: the `Maybe b` is applied to `def` (the `nothing` arm, returned verbatim) and `id` (the `just` arm, a `b -> b` that returns the found value unchanged). The fallback type and the value type coincide at `b`, so both arms return a `b`.

**Trace** — hit `findWithDefault 0 eq 2 d12` and miss `findWithDefault 0 eq 9 d12`:

```text
hit:  lookup eq 2 d12 = just 20 ;  just 20 0 id = id 20 = 20
miss: lookup eq 9 d12 = nothing ;  nothing 0 id = 0
```

The tests pin `findWithDefault 0 eq 2 d12 = 20` and `findWithDefault 0 eq 9 d12 = 0`.

#### `atKey` — value or error

```haskell
-- | 'atKey' is @std::map::at@: the value at @key@, or 'error' if absent.
atKey :: (a -> c -> Bool) -> c -> Dict a b -> b
atKey eq = lookupDefault eq (error "atKey: key not found")
```

The partial accessor `std::map::at` / `Data.Map.!` — exactly `findWithDefault` with the default replaced by a bottoming `error`. The `nothing` arm is `error "atKey: key not found"` and the `just` arm is `id`. Because the module is allowed `error` (it involves no ADT), an absent key raises rather than returning a sentinel. The `error` thunk is only forced if the `nothing` arm is actually selected, i.e. only on a genuine miss.

**Trace** — `atKey eq 1 d12`:

```text
lookup eq 1 [(1,10),(2,20)] = just 10   (first entry matches)
just 10 (error "…") id = id 10 = 10     (error thunk never forced)
```

The test pins `atKey eq 1 d12 = 10`.

#### `keys` — the list of keys

```haskell
-- | 'keys' is @std::views::keys@: the list of keys.
keys :: ∀k v. Dict k v -> List k
keys = map @(k `Pair` v) fst
```

`keys` is `std::views::keys` / `Data.Map.keys`: project the first component out of every entry. It is just `Church`'s `map fst` over the association list, where `fst :: k `Pair` v -> k` destructures a Church `Pair` by applying it to `const`. The single TypeApplication `@(k `Pair` v)` fixes `map`'s domain to the polytype dict element (the codomain `k` then follows from `fst`) — the instantiation hint that lets the generic `map :: (a -> b) -> List a -> List b` meet the rank-N pair element; the sweep confirmed one application suffices. Order is preserved (no key-sorting — that is the `std::map` iteration order a list cannot provide), so the test sorts before comparing.

**Trace** — `keys d12`:

```text
map fst [(1,10),(2,20)] = [fst (1,10), fst (2,20)] = [1,2]
```

The test asserts `sort (keys d12) = [1,2]`.

#### `values` / `elems` — the list of values

```haskell
-- | 'values' (a.k.a. 'elems') is @std::views::values@: the list of values.
values :: ∀k v. Dict k v -> List v
values = map @(k `Pair` v) snd

elems :: Dict k v -> List v
elems = values
```

`values` is `std::views::values` / `Data.Map.elems`: the dual of `keys`, projecting the *second* component via `Church`'s `snd :: k `Pair` v -> v` (which applies the pair to `const id`). Same `map @(k `Pair` v)` instantiation idiom, now landing in `v`. `elems` is a bare synonym (`= values`), present because `Data.Map` exposes both names for the value list; they are definitionally identical, so the two tests below necessarily agree.

**Trace** — `values d12` (and `elems d12`):

```text
map snd [(1,10),(2,20)] = [snd (1,10), snd (2,20)] = [10,20]
```

The tests assert `sort (values d12) = [10,20]` and `sort (elems d12) = [10,20]`.

#### `sizeDict` — entry count

```haskell
-- | 'sizeDict' is @std::map::size@.
sizeDict :: ∀k v. Dict k v -> Int
sizeDict = length @(k `Pair` v)
```

`std::map::size` / `Data.Map.size` — the number of entries, i.e. the length of the underlying list. It delegates to `Church`'s `length` (`foldr (const succ) 0` — one `succ` per cell), with `@(k `Pair` v)` pinning the element type so `length :: List a -> Int` applies at the dict element. Note this counts *list cells*, which equals the key count only while the unique-key invariant holds; on a deliberately-built multimap it counts entries-with-duplicates (the `std::multimap::size` reading).

**Trace** — `sizeDict d12`:

```text
length [(1,10),(2,20)] = 0 + 1 + 1 = 2
```

The test pins `sizeDict d12 = 2`.

#### `nullDict` — emptiness test

```haskell
-- | 'nullDict' is @std::map::empty@.
nullDict :: ∀k v. Dict k v -> Bool
nullDict = null @(k `Pair` v)
```

`std::map::empty` / `Data.Map.null`: is the dict empty? It is `Church`'s `null` at the dict element type, and `null xs = xs (\_ _ -> false) true` is pure `List`-elimination — the list is applied to a cons-arm that ignores head/tail and yields `false`, and a nil-arm that yields `true`. So a non-empty dict reduces to `false` and `nil` to `true`, with no separate length computation.

**Trace** — `nullDict nil` and `nullDict d12`:

```text
nullDict nil  = nil (\_ _ -> false) true        = true
nullDict d12  = (1,10) `cons` …  →  cons-arm    = false
```

The tests pin `nullDict (nil :: Dict Int Int) = True` and `nullDict d12 = False`.

#### `singletonDict` — one-entry dictionary

```haskell
-- | 'singletonDict' builds a one-entry dictionary.
singletonDict :: k -> v -> Dict k v
singletonDict k = singleton . pair k
```

The construction counterpart: `std::map{{k, v}}` / `Data.Map.singleton`. It builds the Church pair `pair k v :: k `Pair` v` and wraps it in `Church`'s `singleton` (`singleton x = ($ x)`, the one-element list). Trivially key-unique. This is the only pure *constructor* in the query group — everything else reads an existing dict.

**Trace** — `singletonDict 7 8`:

```text
singleton (pair 7 8) = [(7,8)]
```

The test pins `singletonDict 7 8 = [(7,8)]` (compared as a sorted association list).

#### `lookupAll` — read the dict as a multimap

```haskell
-- | 'lookupAll' reads the dict as a *multimap*: every value stored under @key@.
lookupAll :: (a -> c -> Bool) -> c -> Dict a b -> List b
lookupAll eq key = values . keySelect (flip eq key)
```

Where `lookup` stops at the *first* match, `lookupAll` is the `std::multimap::equal_range` reading (`Data.Map` has no direct analogue since it forbids duplicate keys): it gathers *every* value stored under `key`. It is a two-stage pipeline of the section's own vocabulary: `keySelect (flip eq key)` keeps exactly the entries whose key matches (one Church `Bool` per entry, consumed inside the `keySelect → filterWithKey → filter` chain), and `values` projects out their second components. Filtering preserves entry order, so the surviving values come out in original left-to-right order. Prettily, `lookup` itself is now defined *on top of this*: `lookup eq key = listToMaybe . lookupAll eq key` — the first of all matches, lazily.

**Trace** — `lookupAll eq 1 dmm` over the duplicate-key multimap `[(1,10),(2,20),(1,30)]`:

```text
keySelect (flip eq 1):
  (1,10): eq 1 1 = true  → keep
  (2,20): eq 2 1 = false → drop
  (1,30): eq 1 1 = true  → keep
  = [(1,10),(1,30)]
values = [10,30]
```

The test asserts `sort (lookupAll eq 1 dmm) = [10,30]` — both values stored under key `1`.

#### `countKey` — multiplicity of a key

```haskell
-- | 'countKey' is @std::multimap::count@: how many entries are stored under @key@.
countKey :: (a -> c -> Bool) -> c -> Dict a b -> Int
countKey eq key = length . lookupAll eq key
```

`std::multimap::count` (for a unique-key dict this is `std::map::count`, i.e. `0` or `1`): how many entries are stored under `key`. It simply measures the multimap result — `length (lookupAll eq key d)` — so all the matching logic lives in `lookupAll` and `countKey` adds only a `Church.length`. On a genuine `std::map`-style dict it returns `0`/`1` and so doubles as a `member`-as-count; on the duplicate-key `dmm` it returns the true multiplicity.

**Trace** — `countKey eq 1 dmm`:

```text
lookupAll eq 1 dmm = [10,30]   (from the trace above)
length [10,30] = 2
```

The test pins `countKey eq 1 dmm = 2`.

#### Summary of the shared pattern

Two skeletons cover the whole group. The **query** functions (`member`, `notMember`, `findWithDefault`, `atKey`) are one-liners over `lookup`: `findWithDefault`/`atKey` apply its Church `Maybe` result to two continuations, `member`/`notMember` reflect it through `isJust`/`isNothing`, so the present/absent split is *just function application* — no `if`, no `Maybe` deconstruction. The **structural** functions (`keys`, `values`/`elems`, `sizeDict`, `nullDict`, `singletonDict`) are generic `List`/`Pair` eliminators (`map`/`fst`/`snd`/`length`/`null`/`singleton`) specialized to the polytype dict element via `@(k `Pair` v)`. The two **multimap** readers (`lookupAll`, `countKey`) drop the unique-key assumption: `lookupAll` is the `keySelect` filter chain projected through `values`, its per-entry Church `Bool` `eq k key` doing the keep-or-skip, and `countKey` just measures its length. No ADT, no pattern match, and no `if` appears anywhere — every decision is a Church `Bool` or `Maybe` applied to its branches.

---

### Insertion and update

Section 21 of `Church.hs` ports the *insertion / construction / update* surface of `std::map` / `std::unordered_map` (and `Data.Map`) onto the association-list `Dict k v = List (k `Pair` v)`. Recall the invariant from §8: a `Dict` is an unordered list of `(k,v)` pairs with **keys unique under a caller-supplied `Equal k`** — nothing in the type enforces it, the operations *maintain* it. Equality is always passed explicitly (the encoding has no type classes), and as everywhere in the module every decision is a Church `Bool = ∀e. e -> e -> e` or a Church `Maybe = ∀r. r -> (a -> r) -> r` *applied directly to its two arms* — there is no `if`, no `case`, and no ADT. A pair `kv` is destructured by *applying* it to a binary continuation `kv $ \k v -> …`, since `Pair a b = ∀e.(a->b->e)->e`.

Two structural shapes recur, both seen already in §8:

- **Direct `caseList` recursion preserving uniqueness.** `insertWith` walks the list with `caseList` + `Pair` elimination, and on a key hit *stops recursing* — the unique-key invariant guarantees at most one match. (`insert` does no recursion of its own; it is `insertWith (flip const)`, the keep-the-old-value instance.)
- **A fold or map rebuild.** `fromList`/`fromListWith` fold the input through `insertWith`; `adjust` and `mapWithKey` rebuild by mapping over the entries. `alter` is the odd one out: a single `lookup` whose `Maybe` result is fed to a user function `f`, then dispatched to delete-or-set.

Every worked value below is the exact `assertEqual`/`toAL` expectation in the `dictAlgoTests` group of [`test/Spec.hs`](../test/Spec.hs) (lines 782–793). Note `toAL` **sorts the result by key** before comparison, so the bracketed expected lists are key-sorted; the traces show the unsorted list the function actually returns. The shared fixture is `d12 = [(1,10),(2,20)]`.

#### `insert` — insert-if-absent (`std::map::insert` / `try_emplace`)

```haskell
insert :: Equal k -> k -> v -> Dict k v -> Dict k v
insert = insertWith (flip const)
```

`insert` is C++ `std::map::insert` / `try_emplace` and `Data.Map.insert` *for absent keys only*: it adds `(key,value)` **only if `key` is not already present**, and — crucially — **never overwrites** an existing value (contrast `insertOrUpdate` from §8, which is the upsert). And it is now the *third* one-line instance of `insertWith`, completing the family: the combiner `flip const` means "on a collision, replace the stored `old` with `flip const value old = old`" — i.e. keep the old value, discarding the new one. All scanning and control flow are `insertWith`'s (below); `insert` contributes only the combiner. On an absent key, `insertWith`'s recursion bottoms out at `nil` and appends the fresh entry at the **end**, which is harmless under the unordered-with-unique-keys model.

**Worked examples.** `insert (==) 3 30 [(1,10),(2,20)]`: both heads mismatch, the recursion bottoms out and appends ⇒ `[(1,10),(2,20),(3,30)]` (`test/Spec.hs:782`). `insert (==) 1 99 [(1,10),(2,20)]`: head `(1,10)` matches, so the entry is rebuilt as `(1, flip const 99 10) = (1,10)` — the stored value survives and the `99` is discarded ⇒ `[(1,10),(2,20)]` (`test/Spec.hs:783`).

#### `insertWith` — combine-on-collision (`Data.Map.insertWith`)

```haskell
insertWith :: ∀k v. _ -> Equal k -> k -> v -> Dict k v -> Dict k v
insertWith f eq key value =
  caseList @_ @(k `Pair` v)
    (singleton (key `pair` value))
    (\kv rest ->
      kv $ \k v ->
        eq k key
          ((key `pair` f value v) `cons` rest)
          (kv `cons` insertWith f eq key value rest))
```

`insertWith` is `Data.Map.insertWith`: insert `value` if `key` is absent, otherwise replace the stored value `old` with **`f value old`** (new-value first, stored-value second — order matters for non-commutative `f`). It is *the* upsert recursion of the whole module — the primitive the others are literal instances of: `insertOrUpdate = insertWith const` (overwrite, since `const value old = value`), `upsertWith eq key new combine = insertWith (const combine) eq key new` (combine with the old value), and `insert = insertWith (flip const)` (keep old). On the empty list, produce `singleton (key,value)`. Otherwise `caseList @_ @(k `Pair` v)` splits head `kv` from `rest`; the Church `Bool` `eq k key` chooses: match ⇒ rebuild as `(key, f value v) `cons` rest`, dropping the old `kv` and **not recursing** (uniqueness ⇒ at most one hit); mismatch ⇒ keep `kv` verbatim and recurse into `rest`. The `@_ @(k `Pair` v)` is the GHC-9.12 type application — it pins the generic `caseList` to the dict element type, which the rank-N body cannot otherwise fix; semantically it changes nothing (see **[Appendix A](#appendix-a--ghc-compatibility-why-it-broke-after-98-and-the-fixes)**).

**Worked examples.** `insertWith (+) (==) 1 5 [(1,10),(2,20)]`: head `(1,10)` matches, so replace with `(1, f 5 10) = (1, 5+10) = (1,15)` and splice in `rest = [(2,20)]` ⇒ `[(1,15),(2,20)]` (`test/Spec.hs:784`). `insertWith (+) (==) 3 5 [(1,10),(2,20)]`: both heads mismatch, recursion bottoms out at `nil` ⇒ `singleton (3,5)`, which the unwinding conses behind the kept heads ⇒ `[(1,10),(2,20),(3,5)]` (`test/Spec.hs:785`).

#### `fromList` — build a dict, last value wins (`Data.Map.fromList`)

```haskell
fromList :: Equal k -> List (k `Pair` v) -> Dict k v
fromList = fromListWith const
```

`fromList` is `Data.Map.fromList`: fold a raw list of pairs (possibly with duplicate keys) into a unique-keyed dict where, on a duplicate key, the **last** occurrence wins. The mechanism is `fromListWith const` — a `foldlWithKey` seeded with the empty dict whose step threads each `(k,v)` through `insertWith const eq k v acc`, i.e. §8's *overwriting* upsert `insertOrUpdate`. Because the fold is a **left** fold, later pairs are upserted after earlier ones, and the overwrite semantics means each repeat of a key clobbers the value left by the previous one — hence last-wins. (A right fold here would instead give first-wins; the fold direction is precisely what fixes the `Data.Map` convention.)

**Worked example.** `fromList (==) [(1,10),(2,20),(1,30)]` (`foldl`, left-to-right):

```text
acc₀ = []
(1,10): insertOrUpdate 1 10 []          → [(1,10)]
(2,20): insertOrUpdate 2 20 [(1,10)]    → [(1,10),(2,20)]
(1,30): insertOrUpdate 1 30 …           → overwrite key 1 → [(1,30),(2,20)]
= [(1,30),(2,20)]
```

`toAL` leaves this key-sorted as `[(1,30),(2,20)]` — the value `30` from the *last* `(1,_)` pair wins, not the `10` from the first (`test/Spec.hs:786`).

#### `fromListWith` — build a dict, folding duplicate-key values (`Data.Map.fromListWith`)

```haskell
fromListWith :: (v -> v -> v) -> Equal k -> List (k `Pair` v) -> Dict k v
fromListWith f eq = foldlWithKey (flip (flip . insertWith f eq)) nil
```

`fromListWith` is `Data.Map.fromListWith`: identical to `fromList` except colliding values are **combined with `f`** instead of overwritten — indeed it is the *general* form, `fromList` being its `const` instance. The body is a `foldlWithKey` of `insertWith f eq` (the `flip (flip . …)` merely rearranges the step's arguments into `foldlWithKey`'s accumulator-first shape) — so each fresh pair `(k,v)` either seeds the key or replaces the running value `old` with `f v old`. The left fold fixes the processing order, so for a key seen with values `v₁` then `v₂` the result is `f v₂ (… seed v₁ …)` — the later value is `f`'s first argument, matching `Data.Map.fromListWith`'s `f newValue oldValue` contract.

**Worked example.** `fromListWith (+) (==) [(1,10),(2,20),(1,30)]` (`foldl`):

```text
acc₀ = []
(1,10): insertWith (+) 1 10 []          → [(1,10)]
(2,20): insertWith (+) 2 20 [(1,10)]    → [(1,10),(2,20)]
(1,30): insertWith (+) 1 30 …           → replace key 1 with f 30 10 = 40
                                        → [(1,40),(2,20)]
= [(1,40),(2,20)]
```

Key `1` receives `10 + 30 = 40`; key `2` is untouched ⇒ `[(1,40),(2,20)]` (`test/Spec.hs:787`).

#### `adjust` — transform the value at one key (`Data.Map.adjust`)

```haskell
adjust :: Equal k -> _ -> k -> Dict k v -> Dict k v
adjust eq f key = mapWithKey (\k -> eq k key f id)
```

`adjust` is `Data.Map.adjust`: apply `f` to the value stored at `key` **if present**, leaving every other entry — and an absent key — unchanged. It is the keyed dual of §8's `mapValues` (which transforms *all* values); `adjust` transforms exactly the one whose key matches — and the definition is a single `mapWithKey` whose per-entry function is a jewel of Bool-as-data: `\k -> eq k key f id` uses the Church `Bool` `eq k key` to choose *which function* to apply to the value — `f` for the target key, `id` for everyone else. Because keys are unique, at most one entry receives `f`; if none matches, every entry passes through `id` and the dict is returned pointwise unchanged. (An earlier revision was a bespoke re-consing fold; selecting between `f` and `id` inside one `mapWithKey` says it in a line.)

**Worked examples.** `adjust (==) (+1) 1 [(1,10),(2,20)]`: entry `(1,10)` matches ⇒ `(1, 10+1) = (1,11)`; `(2,20)` does not ⇒ kept ⇒ `[(1,11),(2,20)]` (`test/Spec.hs:788`). `adjust (==) (+1) 9 [(1,10),(2,20)]`: key `9` matches nothing, both entries take the verbatim arm ⇒ `[(1,10),(2,20)]` unchanged (`test/Spec.hs:789`).

#### `alter` — the universal insert / update / delete combinator (`Data.Map.alter`)

```haskell
alter :: Equal k -> (Maybe v -> Maybe v) -> k -> Dict k v -> Dict k v
alter eq f key d =
  f (lookup eq key d)
    (deleteKey eq key d)
    (flip (insertOrUpdate eq key) d)
```

`alter` is `Data.Map.alter`, the most general single-key operation — it subsumes insert, update, and delete in one move. The user function `f :: Maybe v -> Maybe v` is shown the **current** value at `key` as a Church `Maybe` (`nothing` if absent, `just v` if present) and returns the **desired** value: a `nothing` result *deletes* the key, a `just v'` result *sets* it. The elegance is that `f`'s `Maybe` *result* needs no inspection — it is its own eliminator, applied directly to two arms: `f (lookup eq key d) (deleteBranch) (setBranch)`. The `nothing`-arm is `deleteKey eq key d` (§8's filter-by-key removal); the `just`-arm is `\v' -> insertOrUpdate eq key v' d` (§8's upsert with the produced value). So `alter` is two §8 primitives wired to the two branches of whatever `Maybe` the caller's `f` hands back — no `if`, no `case`, just `Maybe`-as-dispatcher twice over (once on `lookup`'s result feeding `f`, once on `f`'s result selecting delete-vs-set).

**Worked examples** (all on `d12 = [(1,10),(2,20)]`):

```text
alter (==) (\_ -> nothing) 1 d12
  lookup 1 d12 = just 10;  f _ = nothing  → delete branch → deleteKey 1 d12
  = [(2,20)]                                               (test/Spec.hs:790)

alter (==) (\_ -> just 99) 3 d12
  lookup 3 d12 = nothing;  f _ = just 99  → set branch → insertOrUpdate 3 99 d12
  = [(1,10),(2,20),(3,99)]                                (test/Spec.hs:711)

alter (==) (\m -> m (just 0) (\v -> just (v+1))) 1 d12
  lookup 1 d12 = just 10;  f (just 10) = just 11  → set branch → insertOrUpdate 1 11 d12
  = [(1,11),(2,20)]                                        (test/Spec.hs:792)
```

The third case shows `f` itself eliminating the incoming `Maybe`: `m (just 0) (\v -> just (v+1))` returns `just 0` when the key is absent (the `nothing`-arm) and `just (v+1)` when present — an "insert-default-or-increment". With key `1` present at `10`, it yields `just 11`, which the set-branch upserts. (`deleteKey`, `lookup`, and `insertOrUpdate` are all from §8.)

#### `mapWithKey` — map values with the key in scope (`Data.Map.mapWithKey`)

```haskell
mapWithKey :: _ -> Dict k v1 -> Dict k v2
mapWithKey f d = zip (keys d) (keyValueMap f d)
```

`mapWithKey` is `Data.Map.mapWithKey`: rewrite every value via `f`, but with the **key in scope** (`f :: k -> v1 -> v2`), keys themselves untouched. It is §8's `mapValues` generalized so the combining function also sees the key — and the implementation is a cheerful re-zip: `keyValueMap f d` (§8's `map . uncurry`) computes the transformed value `f k v` for every entry, `keys d` lists the untouched keys in the same order, and `zip` marries them back into a dictionary. No `bimapPair` could express a value transform that depends on the key, so the entry is taken apart (`uncurry` inside `keyValueMap`) and reassembled (`zip`) instead. Since keys are preserved verbatim and both lists share the dict's entry order, uniqueness is automatically maintained — no lookup or collision handling, a pure structure-preserving value transform.

**Worked example.** `mapWithKey (\k v -> k + v) [(1,10),(2,20)]`: entry `(1,10)` ↦ `(1, 1+10) = (1,11)`; `(2,20)` ↦ `(2, 2+20) = (2,22)` ⇒ `[(1,11),(2,22)]` (`test/Spec.hs:793`).

#### The shared skeleton

`insertWith` is the one `caseList`-recursion, halting on the unique key hit; `insert` and §8's `insertOrUpdate`/`upsertWith` are its `flip const`/`const`/`const combine` instances; `fromList`/`fromListWith` are **left** folds of it (the direction is what pins last-wins / `f newValue oldValue`); `adjust` is a `mapWithKey` whose per-entry `Bool` selects `f`-or-`id`, and `mapWithKey` itself is a `zip` of the untouched keys against the transformed values; and `alter` is a `lookup` whose `Maybe` is routed through the caller's `f` into delete-or-set. Every branch is a Church `Bool` or `Maybe` applied to its arms — no `if`, no `case`, no ADT — and the heavy lifting bottoms out in `lookup`, `insertWith`, and the `keySelect`-chained `deleteKey`.

---

### Filtering, combining and folding

This subsection covers the `<map>` operations that consume *and* combine whole dictionaries: the three `Data.Map`-style filters/partitions (`filterWithKey`, `filterValues`, `partitionDict`), the union/intersection/difference family with their `…With` value-combining variants, the two key-set restrictions (`restrictKeys`, `withoutKeys`), and the two key-aware folds (`foldrWithKey`, `foldlWithKey`) plus the `isSubmapOfBy` containment test. Every one of them lives in section 22 of `Church.hs` ("`<map>` equivalents: filtering, combining and folding").

They share the encoding vocabulary established in §8: a `Dict k v = List (k `Pair` v)` is an association list with keys unique under a passed `Equal k`; each `(k,v)` entry `kv` is destructured by *applying* it to a binary continuation `kv $ \k v -> …` (since `Pair a b = ∀e.(a->b->e)->e`); branching is a Church `Bool` applied to two arms, never `if`; and `lookup`/`member` return a Church `Maybe`/`Bool` that *is* its own eliminator. The value-combining pair (`unionWith`, `intersectionWith`) delegate to their keyed `…WithKey` forms, which consult the *other* dict via `insertWith` (a `foldrWithKey`) and `lookup` (a `mapMaybeWithKey`) respectively. Nearly everything else is a one-line *instantiation*: `union`/`intersection` fix the combiner to `const`; `unionsWith` is a `foldl` of `unionWith`; the filters, `difference`, and the key-set restrictions chain through §8's `keySelect`/`keyTake`/`keyDrop` and `filterWithKey` down to the generic `Church.filter` (and `partitionDict` down to `Church.partition`, `isSubmapOfBy` to `Church.all`) — pipelines that, after the type-application sweep, need no instantiation hints at all (inference fixes the dict element type from the delegations themselves); and the keyed folds are `uncurry` adapters over `Church.foldr`/`foldl`.

All expected values below are pinned by the `dictAlgoTests` group in `test/Spec.hs` (lines 715–821), whose fixtures are `d12 = [(1,10),(2,20)]`, integer-equality `eq`, and a `toAL` that sorts the result by key before comparison.

#### `filterWithKey` — keep entries by a key/value predicate

```haskell
filterWithKey :: (k -> v -> Bool) -> Dict k v -> Dict k v
filterWithKey = filter . uncurry
```

This is `Data.Map.filterWithKey` (and the `std::erase_if`-on-a-map filter, kept rather than removed). It is a one-liner over `Church.filter`: the predicate adapter is `uncurry p` — and Church's `uncurry f kv = kv f` is nothing but pair-elimination-by-application, so `uncurry p` *is* "destructure the entry and hand `p` the key and the value," producing the Church `Bool` that `filter` consumes to decide inclusion. No `if`, no pattern match — the returned `Bool` is the whole decision, and (a pleasant discovery of the sweep) the fully point-free `filter . uncurry` needs no type application: `uncurry` fixes the element type for `filter`. This one line is also the *bottom* of §8's key-filter chain: `deleteKey`/`keyTake`/`keyDrop` → `keySelect` → `filterWithKey` → `filter`.

**Example** (`test/Spec.hs:715`) — `filterWithKey (\k _ -> even k) [(1,10),(2,20)]`:

```text
(1,10): even 1 = false → drop
(2,20): even 2 = true  → keep
= [(2,20)]
```

#### `filterValues` — keep entries by value alone

```haskell
filterValues :: (v -> Bool) -> Dict k v -> Dict k v
filterValues p = filterWithKey (const p)
```

`Data.Map.filter` — the value-only specialization of `filterWithKey`. It simply discards the key in the predicate (`\_ v -> p v`) and delegates, so all of the encoding machinery is inherited: the value `v` is exposed by the same `kv $ \k v -> …` destructuring inside `filterWithKey`, and `p v :: Bool` selects keep/drop with no `if`.

**Example** (`test/Spec.hs:716`) — `filterValues (> 15) [(1,10),(2,20)]`:

```text
(1,10): 10 > 15 = false → drop
(2,20): 20 > 15 = true  → keep
= [(2,20)]
```

#### `partitionDict` — split a dictionary by a key/value predicate

```haskell
partitionDict :: (k -> v -> Bool) -> Dict k v -> Dict k v `Pair` Dict k v
partitionDict = partition . uncurry
```

`Data.Map.partitionWithKey`: the pair `(matches, non-matches)`. It is Church's list `partition` at the dict-element type, with the same `uncurry p` adapter as `filterWithKey` — and `Church.partition p xs = filter p xs `pair` filter (not . p) xs`, so the split is *two* filter passes packaged in a Church `Pair`: the `yes` half keeps the entries where `uncurry p` holds (it is literally `filterWithKey p`), the `no` half those where the negated predicate holds. Each `filter` preserves input order, so each half keeps the original left-to-right entry order; and because the result `Pair` holds two lazily-built lists, a consumer that eliminates only one component only ever pays for that pass.

**Example** (`test/Spec.hs:717`) — `partitionDict (\k _ -> even k) [(1,10),(2,20)]`:

```text
partition (uncurry p) [(1,10),(2,20)]
  yes = filter (uncurry p) [(1,10),(2,20)]
        (1,10): even 1 = false → drop ; (2,20): even 2 = true → keep    = [(2,20)]
  no  = filter (not . uncurry p) [(1,10),(2,20)]
        (1,10): keep ; (2,20): drop                                     = [(1,10)]
= ([(2,20)], [(1,10)])
```

#### `union` — left-biased union

```haskell
union :: Equal k -> Dict k v -> Dict k v -> Dict k v
union = unionWith const
```

The left-biased union: every key present in both `d1` and `d2` keeps its `d1` value. It is `Data.Map.union` (and the relation behind `std::map::merge`, which likewise does not overwrite existing keys). The implementation just specializes `unionWith` with the combiner `const` — `const l r = l`, always the left (`d1`) value — so all the work happens in `unionWith` below. Fully point-free: even `eq` is left for `unionWith` to collect.

**Example** (`test/Spec.hs:800`) — `union eq [(1,10),(2,20)] [(2,99),(3,30)]`:

```text
key 2 is in both → keep d1's value 20 (not 99)
key 1 only in d1 → 10;  key 3 only in d2 → 30
= [(1,10),(2,20),(3,30)]
```

#### `unionWith` — union combining shared-key values

```haskell
unionWith :: (v -> v -> v) -> Equal k -> Dict k v -> Dict k v -> Dict k v
unionWith = unionWithKey . const
```

`Data.Map.unionWith`: the genuine value-combining union (contrast `merge` from §8, which aggregates colliding values into *lists* rather than folding them with `f`). It delegates to the keyed `unionWithKey` with the key discarded (`. const`); there, the seed of the `foldrWithKey` is `d2`, and each entry of `d1` is folded in via `insertWith (f k) eq k` — which inserts `(k,v)` when the key is absent from the accumulator, and otherwise replaces the stored value `old` with `f v old` (new-from-`d1` as the left argument, existing-from-`d2` as the right). So `f` is applied as `f leftValue rightValue` exactly on the shared keys; `insertWith`'s own control flow is the usual `eq`-driven Church-`Bool` recursion, no `if`.

**Example** (`test/Spec.hs:802`) — `unionWith (+) eq [(1,1)] [(1,2),(2,3)]`:

```text
seed = d2 = [(1,2),(2,3)]
fold d1's (1,1): key 1 present → insertWith (+) replaces 2 with (1 + 2) = 3
= [(1,3),(2,3)]
```

#### `unionsWith` — fold `unionWith` over many dictionaries

```haskell
unionsWith :: (v -> v -> v) -> Equal k -> List (Dict k v) -> Dict k v
unionsWith f eq = foldl (unionWith f eq) nil
```

`Data.Map.unionsWith`: combine a whole list of dictionaries, resolving every key collision with `f`. It is *literally* a left fold of `unionWith f eq` over the list, seeded with the empty dict — the partially-applied `unionWith f eq :: Dict k v -> Dict k v -> Dict k v` already has exactly the shape `foldl` wants for its step, no wrapper needed. Because `foldl` threads the running result as the *left* argument, dictionaries earlier in the list contribute the left value to `f`, matching `Data.Map.unionsWith`'s left-to-right accumulation order.

**Example** (`test/Spec.hs:804`) — `unionsWith (+) eq [ [(1,1)], [(1,2),(2,3)] ]`:

```text
acc₀ = []
∪ [(1,1)]        → [(1,1)]
∪ [(1,2),(2,3)]  → key 1: (1 + 2) = 3; key 2 fresh: 3
= [(1,3),(2,3)]
```

#### `intersection` — keys in both, left values

```haskell
intersection :: Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1
intersection = intersectionWith const
```

`Data.Map.intersection`: keep only the keys present in *both* dicts, taking the value from `d1`. It specializes `intersectionWith` with the combiner `const` (here at type `v1 -> v2 -> v1`), discarding `d2`'s value — the exact mirror of `union = unionWith const` above. Note the result type is `Dict k v1` — the value universe is `d1`'s — which is why dropping `d2`'s value is type-correct.

**Example** (`test/Spec.hs:808`) — `intersection eq [(1,10),(2,20)] [(2,99),(3,30)]`:

```text
only key 2 is common → take d1's value 20
= [(2,20)]
```

#### `intersectionWith` — intersection combining values

```haskell
intersectionWith :: (v1 -> v2 -> v3) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v3
intersectionWith = intersectionWithKey . const
```

`Data.Map.intersectionWith`: keep the shared keys, combining the two values with `f v1 v2` into a possibly new value type `v3`. It delegates to the keyed `intersectionWithKey` with the key discarded (`. const`); there, a `mapMaybeWithKey` walks `d1`, and for each `(k,v1)`, `lookup eq k d2` returns a Church `Maybe v2` that *is* the case split — its `nothing` arm drops the entry (key absent from `d2`), its `just` arm is `just . f k v1` (present, emit the combined entry under the original key). No `isNothing`/`fromJust`, no `if`: applying the `Maybe` to two continuations does all the branching.

**Example** (`test/Spec.hs:806`) — `intersectionWith (+) eq [(1,10),(2,20)] [(2,2),(3,3)]`:

```text
(1,10): lookup 1 in d2 = nothing → drop
(2,20): lookup 2 in d2 = just 2  → (2, 20 + 2) = (2,22)
= [(2,22)]
```

#### `difference` — entries of `d1` whose keys are absent from `d2`

```haskell
difference :: Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1
difference eq d1 d2 = keySelect (flip (notMember eq) d2) d1
```

`Data.Map.difference` (`d1 \\ d2`): the many-key generalization of §8's `deleteKey` — and it is built from the very same brick. It is `keySelect` with the key predicate `notMember eq k d2` (§20's negated membership): keep exactly the entries of `d1` whose key is *absent* from `d2`. Values never enter the predicate (that is `keySelect`'s whole point), and the result carries `d1`'s value type `v1`. Under the hood the chain is §8's — `keySelect` → `filterWithKey` → `filter` — so the keep/drop decision is one Church `Bool` per entry, produced by `notMember` (itself `isNothing . lookup eq k`, the `lookup` result reflected through `isNothing`).

**Example** (`test/Spec.hs:810`) — `difference eq [(1,10),(2,20)] [(2,99),(3,30)]`:

```text
(1,10): notMember 1 d2 = true  → keep
(2,20): notMember 2 d2 = false → drop
= [(1,10)]
```

#### `restrictKeys` — keep only keys in a given set

```haskell
restrictKeys :: Equal k -> Dict k v -> List k -> Dict k v
restrictKeys = flip . keyTake
```

`Data.Map.restrictKeys`: keep exactly the entries whose key appears in the list `ks`. This is §8's Wolfram-flavored `keyTake` with the arguments reshuffled into the `Data.Map` convention (dict before key set) — the module's two dictionary layers share one implementation. `keyTake eq wanted = keySelect (\k -> elemBy eq k wanted)`, so each key's membership in `ks` is decided by `elemBy eq k ks` (the §10 list-membership predicate, itself `any . eq`), a Church `Bool` that `filter` — at the bottom of the `keySelect` chain — consumes to keep or drop the entry.

**Example** (`test/Spec.hs:812`) — `restrictKeys eq [(1,10),(2,20)] [2,9]`:

```text
(1,10): elemBy 1 [2,9] = false → drop
(2,20): elemBy 2 [2,9] = true  → keep
= [(2,20)]
```

#### `withoutKeys` — drop keys in a given set

```haskell
withoutKeys :: Equal k -> Dict k v -> List k -> Dict k v
withoutKeys = flip . keyDrop
```

`Data.Map.withoutKeys`: the complement of `restrictKeys` — drop every entry whose key is in `ks`. Again an argument-reshuffle of the §8 layer: `keyDrop eq unwanted = keySelect (\k -> not (elemBy eq k unwanted))` — the same membership test as `keyTake`, wrapped in Church `not` (`not b = b false true`, one more `Bool` elimination, not a new mechanism). So `restrictKeys` and `withoutKeys` differ by exactly one `not` in the predicate, three delegation layers down.

**Example** (`test/Spec.hs:814`) — `withoutKeys eq [(1,10),(2,20)] [2,9]`:

```text
(1,10): not (elemBy 1 [2,9]) = not false = true  → keep
(2,20): not (elemBy 2 [2,9]) = not true  = false → drop
= [(1,10)]
```

#### `foldrWithKey` — right fold with key access

```haskell
foldrWithKey :: (k -> v -> b -> b) -> b -> Dict k v -> b
foldrWithKey = foldr . uncurry
```

`Data.Map.foldrWithKey`: a right fold over the entries that hands the user function *both* the key and the value. It is the thinnest possible adapter over `Church.foldr`: the step is `uncurry f` — `uncurry g kv = kv g` feeds the entry's key and value straight into the curried `f`, whose *third* argument then lines up exactly with `foldr`'s accumulator slot. The seed and the dict are eta-reduced away. No control flow of its own — `f` is total and the recursion is `foldr`'s.

**Example** (`test/Spec.hs:816`) — `foldrWithKey (\k v acc -> k + v + acc) 0 [(1,10),(2,20)]`:

```text
= 1 + 10 + (2 + 20 + 0) = 11 + 22 = 33
```

#### `foldlWithKey` — left fold with key access

```haskell
foldlWithKey :: _ -> b -> Dict k v -> b
foldlWithKey = foldl . (uncurry .)
```

`Data.Map.foldlWithKey`: the left-fold counterpart, accumulator first. The mirror of `foldrWithKey`, with one asymmetry worth savoring: because `f` takes the accumulator *before* the key and value, a bare `uncurry f` does not fit — the adapter `(uncurry .)` first absorbs the accumulator and uncurries the remainder, `\acc -> uncurry (f acc) :: b -> (k `Pair` v) -> b`, which is exactly `foldl`'s step shape (and, post-sweep, the point-free spelling needs no type applications). For an associative-commutative `f` like summation the result coincides with `foldrWithKey`, which is exactly why the test pins both at `33`.

**Example** (`test/Spec.hs:817`) — `foldlWithKey (\acc k v -> acc + k + v) 0 [(1,10),(2,20)]`:

```text
= ((0 + 1 + 10) + 2 + 20) = 11 + 22 = 33
```

#### `isSubmapOfBy` — submap containment under a value relation

```haskell
isSubmapOfBy :: (v1 -> v2 -> Bool) -> Equal k -> Dict k v1 -> Dict k v2 -> Bool
isSubmapOfBy eqV eq d1 d2 =
  all (uncurry $ \k v1 -> lookup eq k d2 false $ eqV v1) d1
```

`Data.Map.isSubmapOfBy`: true iff every entry `(k, v1)` of `d1` occurs in `d2` under a key matched by `eq` and a value related by `eqV`. It uses the generic `Church.all` over `d1` (no instantiation pin needed — the `uncurry` adapter fixes the entry type): for each entry, `lookup eq k d2` yields a Church `Maybe v2` applied to two arms — `false` if `k` is absent from `d2` (so `all` short-circuits to a failing predicate), and `eqV v1` if present, comparing the two values. The conjunction across all entries is `all`'s job; the per-entry decision is `Maybe`-then-`Bool` elimination, no `if`. The relation is heterogeneous (`v1 -> v2 -> Bool`), so the two value universes need not coincide.

**Example** (`test/Spec.hs:818,820`) — with `d12 = [(1,10),(2,20)]` and `eqV = (==)`:

```text
isSubmapOfBy (==) eq [(1,10)] d12:  lookup 1 in d12 = just 10; 10 == 10 = true  → True
isSubmapOfBy (==) eq [(1,99)] d12:  lookup 1 in d12 = just 10; 99 == 10 = false → False
```

---

## 15. Projection, keyed map, and numeric extensions

The final source sections fill three practical gaps while keeping the module's terse
eliminator style.  First, `on f key x = f (key x) . key` lifts any binary relation through
a projection; `nubOn`, `groupOn`, `sortOn`, `minimumOn`, and `maximumOn` are then one-line
specializations of their existing `…By` counterparts. `sortOn` uses the stable three-way
`quicksort`, retaining the input order of equal-key elements. The projected key is not cached, so
these definitions deliberately favor algebraic economy over avoiding repeated projection.

Second, the dictionary layer gains the key-aware `Data.Map` family. `mapMaybeWithKey` is the
shared primitive: it applies a Church `Maybe`-valued transform and preserves the original key
only in the `just` arm. `update`/`updateWithKey` are restricted forms of `alter` that cannot
insert an absent key. `mapKeysWith` restores the dictionary uniqueness invariant after key
projection by delegating collisions to `fromListWith`. The keyed union/intersection/difference
variants expose the key to their combining function; their unkeyed forms delegate to them with
`const`. `unions`, `isSubmapOf`, and `disjoint` complete the common relation surface.

Finally, Section 23 mirrors the useful pure portion of C++ `<numeric>`:

| Family | Functions | Definition shape |
|---|---|---|
| reductions | `accumulate`, `reduce`, `transformReduce`, `transformReduce2` | `foldl`, `foldl1`, or `innerProduct` |
| products | `innerProduct` | `foldl add z . zipWith mul xs` |
| scans | `partialSum`, `inclusiveScan`, `exclusiveScan` | `scanl` with the seed retained or removed |
| transformed scans | `transformInclusiveScan`, `transformExclusiveScan` | scan after `map` |
| differences | `adjacentDifference` | first element, then `zipWith f current previous` |

`innerProduct` and `transformReduce2` stop at the shorter input, following `zipWith`.
`partialSum` and `adjacentDifference` return `nil` on an empty list. `reduce` is intentionally
partial on `nil`, exactly like the `foldl1` it aliases. All operators and seeds are explicit,
so the section needs no `Num` instance and introduces no concrete type beyond the module's
existing `Int` indexing helpers.

The 38 new assertions in `algorithmTests`, `numericAlgorithmTests`, and `dictAlgoTests` pin
projection behavior, fold direction, scan endpoints, truncation, key-collision order, keyed
combiner argument order, update/delete behavior, and disjoint/submap predicates.

---

## Appendix D — Function index (Part II)

Every public top-level function in sections 8–23 of `Church.hs` — the algorithms covered by Part II (chapters 8–15) — grouped by the file's own sections, with its type signature; section 24's parity aliases are summarized in the closing paragraph, and the comment-marked private helpers `partition3`, `quickStep`, and `setOp` are omitted. (The table writes `∀` quantifiers explicitly for readability; in the source, the automated sweep removed every quantifier GHC does not require, so most signatures leave the variables implicitly quantified and only the definitions whose bodies need `ScopedTypeVariables`/`TypeApplications` — e.g. `invert`, `permutations`, `insertWith`, `nthElement'` — keep an explicit `∀`. It likewise writes out in full the argument types the source leaves as `_` signature wildcards.) The core types and combinators these build on — sections 0–7 — are indexed in [Appendix C](#appendix-c--function-index).

**8. Dictionary Operations**

| Function | Type |
|----------|------|
| `lookup` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> Maybe b` |
| `lookupDefault` | `∀a b c. (a -> c -> Bool) -> b -> c -> Dict a b -> b` |
| `keyExists` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> Bool` |
| `keyMember` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> Bool` |
| `keyFree` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> Bool` |
| `keyValueMap` | `∀k v a. (k -> v -> a) -> Dict k v -> List a` |
| `mapKeys` | `∀k₁ k₂ v. (k₁ -> k₂) -> Dict k₁ v -> Dict k₂ v` |
| `dictFromLists` | `List k -> List v -> Dict k v` |
| `associationMap` | `∀k v. Equal k -> (k -> v) -> List k -> Dict k v` |
| `insertOrUpdate` | `∀a b. Equal a -> a -> b -> Dict a b -> Dict a b` |
| `invert` | `∀a b. Equal b -> Dict a b -> Dict b (List a)` |
| `merge` | `∀k v. Equal k -> List (Dict k v) -> Dict k (List v)` |
| `mergeWith` | `∀k v r. Equal k -> (List v -> r) -> List (Dict k v) -> Dict k r` |
| `collapse` | `∀k₁ k₂ v. Equal k₁ -> Equal k₂ -> Dict k₁ (Dict k₂ v) -> Dict (k₁ `Pair` k₂) v` |
| `eitherDict` | `∀k₁ k₂ v. Dict k₁ v -> Dict k₂ v -> Dict (Either k₁ k₂) v` |
| `chain` | `∀k₁ k₂ v. Equal k₁ -> Equal k₂ -> Dict k₁ k₂ -> Dict k₂ v -> Dict k₁ v` |
| `chainN` | `∀a. Equal a -> Dict a a -> List(Dict a a) -> Dict a a` |
| `mapValues` | `∀k v₁ v₂. (v₁ -> v₂) -> Dict k v₁ -> Dict k v₂` |
| `mapMaybeValues` | `∀k v₁ v₂. (v₁ -> Maybe v₂) -> Dict k v₁ -> Dict k v₂` |
| `cartesianDict` | `∀k₁ k₂ v₁ v₂. Dict k₁ v₁ -> Dict k₂ v₂ -> Dict (k₁ `Pair` k₂) (v₁ `Pair` v₂)` |
| `deleteKey` | `∀k v. Equal k -> k -> Dict k v -> Dict k v` |
| `keyTake` | `∀k v. Equal k -> List k -> Dict k v -> Dict k v` |
| `keyTakeOrdered` | `∀k v. Equal k -> List k -> Dict k v -> Dict k v` |
| `keyDrop` | `∀k v. Equal k -> List k -> Dict k v -> Dict k v` |
| `keySelect` | `∀k v. (k -> Bool) -> Dict k v -> Dict k v` |
| `upsertWith` | `∀k v. Equal k -> k -> v -> (v -> v) -> Dict k v -> Dict k v` |
| `keySortBy` | `∀k v a. LE a -> (k -> a) -> Dict k v -> Dict k v` |
| `keySort` | `∀k v. LE k -> Dict k v -> Dict k v` |
| `unionKeys` | `∀k v. Equal k -> List (Dict k v) -> List k` |
| `intersectionKeys` | `∀k v. Equal k -> List (Dict k v) -> List k` |
| `keyComplement` | `∀k v w. Equal k -> Dict k v -> List (Dict k w) -> Dict k v` |
| `keyIntersection` | `∀k v. Equal k -> List (Dict k v) -> List (Dict k v)` |
| `keyUnion` | `∀k v. Equal k -> (k -> v) -> List (Dict k v) -> List (Dict k v)` |

**9. Matrix Transformations**

| Function | Type |
|----------|------|
| `turn` | `∀ a. Matrix a -> Matrix a` |
| `unturn` | `∀ a. Matrix a -> Matrix a` |

**10. Additional List Combinators and Checks**

| Function | Type |
|----------|------|
| `subsequences` | `∀a. List a -> List (List a)` |
| `permutations` | `∀a. List a -> List (List a)` |
| `on` | `(b -> b -> c) -> (a -> b) -> a -> a -> c` |
| `nubBy` | `∀a. (a -> a -> Bool) -> List a -> List a` |
| `nubOn` | `∀a b. Equal b -> (a -> b) -> List a -> List a` |
| `deleteBy` | `(a -> a -> Bool) -> a -> List a -> List a` |
| `deleteFirstsBy` | `(a -> a -> Bool) -> List a -> List a -> List a` |
| `unionBy` | `∀a. (a -> a -> Bool) -> List a -> List a -> List a` |
| `intersectBy` | `∀a. (a -> a -> Bool) -> List a -> List a -> List a` |
| `groupBy` | `∀a. (a -> a -> Bool) -> List a -> List (List a)` |
| `groupOn` | `∀a b. Equal b -> (a -> b) -> List a -> List (List a)` |
| `gatherBy` | `∀a k. Equal k -> (a -> k) -> List a -> List (List a)` |
| `gather` | `∀a. Equal a -> List a -> List (List a)` |
| `countsBy` | `∀a. Equal a -> List a -> Dict a Int` |
| `countsByKey` | `∀a k. Equal k -> (a -> k) -> List a -> Dict k Int` |
| `tallyBy` | `∀a. Equal a -> List a -> Dict a Int` |
| `countDistinctBy` | `∀a. Equal a -> List a -> Int` |
| `positionIndexBy` | `∀a. Equal a -> List a -> Dict a (List Int)` |
| `pick` | `∀a b. List Bool -> List a -> List b -> List (Either a b)` |
| `pick'` | `∀a. List Bool -> List a -> List a -> List a` |
| `isSubstring` | `(a -> b -> Bool) -> List a -> List b -> Bool` |
| `isSubseq` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Bool` |
| `isPrefixOf` | `(a -> b -> Bool) -> List a -> List b -> Bool` |
| `break` | `∀a. (a -> Bool) -> List a -> List a `Pair` List a` |
| `elemBy` | `∀a b. (a -> b -> Bool) -> a -> List b -> Bool` |
| `findIndex` | `∀a. (a -> Bool) -> List a -> Maybe Int` |
| `findIndices` | `∀a. (a -> Bool) -> List a -> List Int` |
| `elemIndex` | `∀a b. (a -> b -> Bool) -> a -> List b -> Maybe Int` |
| `isSuffixOf` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Bool` |
| `maximumBy` | `∀a. LE a -> List a -> a` |
| `maximumOn` | `∀a b. LE b -> (a -> b) -> List a -> a` |
| `minimumBy` | `∀a. LE a -> List a -> a` |
| `minimumOn` | `∀a b. LE b -> (a -> b) -> List a -> a` |
| `minMaxBy` | `∀a. LE a -> List a -> a `Pair` a` |
| `deleteDuplicatesBy` | `∀a. Equal a -> List a -> List a` |
| `complementBy` | `∀a b. (a -> b -> Bool) -> List a -> List b -> List a` |
| `containsAllBy` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Bool` |
| `containsAnyBy` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Bool` |
| `containsNoneBy` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Bool` |
| `containsOnlyBy` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Bool` |

**11. Sorting**

| Function | Type |
|----------|------|
| `mergesort` | `∀a. LE a -> List a -> List a` |
| `sortOn` | `∀a b. LE b -> (a -> b) -> List a -> List a` |
| `quicksort` | `∀a. LE a -> List a -> List a` |
| `heapsort` | `∀a. LE a -> List a -> List a` |
| `introsort` | `∀a. LE a -> List a -> List a` |
| `nthElement` | `∀a. LE a -> Int -> List a -> List a` |
| `nthElement'` | `∀a. LE a -> Int -> List a -> List a` &nbsp;— quickselect (two bugs **fixed**; see §12) |

**12. Longest Common Substructure**

| Function | Type |
|----------|------|
| `longestCommonPrefix` | `∀a. Equal a -> List a -> List a -> List a` |
| `longestCommonSuffix` | `∀a. Equal a -> List a -> List a -> List a` |
| `longestCommonSublist` | `∀a. Equal a -> List a -> List a -> List a` |

**Sections 13-19: C++ `<algorithm>` equivalents** (see [chapter 13](#13-c-algorithm-equivalents))

| Function | Type |
|----------|------|
| `none` | `∀a. (a -> Bool) -> List a -> Bool` |
| `findIf` | `∀a. (a -> Bool) -> List a -> Maybe a` |
| `findIfNot` | `∀a. (a -> Bool) -> List a -> Maybe a` |
| `find` | `∀a b. (a -> b -> Bool) -> b -> List a -> Maybe a` |
| `findLast` | `∀a. (a -> Bool) -> List a -> Maybe a` |
| `countIf` | `∀a. (a -> Bool) -> List a -> Int` |
| `count` | `∀a b. (a -> b -> Bool) -> b -> List a -> Int` |
| `mismatch` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Maybe (a `Pair` b)` |
| `adjacentFind` | `∀a. (a -> a -> Bool) -> List a -> Maybe a` |
| `search` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Maybe Int` |
| `findEnd` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Maybe Int` |
| `findFirstOf` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Maybe a` |
| `searchN` | `∀a b. (a -> b -> Bool) -> Int -> b -> List a -> Maybe Int` |
| `take` | `∀a. Int -> List a -> List a` |
| `drop` | `∀a. Int -> List a -> List a` |
| `rotate` | `∀a. Int -> List a -> List a` |
| `removeIf` | `∀a. (a -> Bool) -> List a -> List a` |
| `remove` | `∀a b. (a -> b -> Bool) -> b -> List a -> List a` |
| `replaceIf` | `∀a. (a -> Bool) -> a -> List a -> List a` |
| `replace` | `∀a. (a -> a -> Bool) -> a -> a -> List a -> List a` |
| `uniqueBy` | `∀a. (a -> a -> Bool) -> List a -> List a` |
| `isPartitioned` | `∀a. (a -> Bool) -> List a -> Bool` |
| `partitionPoint` | `∀a. (a -> Bool) -> List a -> Int` |
| `isSorted` | `∀a. LE a -> List a -> Bool` |
| `isSortedUntil` | `∀a. LE a -> List a -> Int` |
| `lowerBound` | `∀a. LE a -> a -> List a -> Int` |
| `upperBound` | `∀a. LE a -> a -> List a -> Int` |
| `binarySearch` | `∀a. LE a -> a -> List a -> Bool` |
| `equalRange` | `∀a. LE a -> a -> List a -> Int `Pair` Int` |
| `mergeBy` | `∀a. LE a -> List a -> List a -> List a` |
| `includes` | `∀a. LE a -> List a -> List a -> Bool` |
| `setUnion` | `∀a. LE a -> List a -> List a -> List a` |
| `setIntersection` | `∀a. LE a -> List a -> List a -> List a` |
| `setDifference` | `∀a. LE a -> List a -> List a -> List a` |
| `setSymmetricDifference` | `∀a. LE a -> List a -> List a -> List a` |
| `maxBy` | `∀a. LE a -> a -> a -> a` &nbsp;— `std::max` |
| `minBy` | `∀a. LE a -> a -> a -> a` &nbsp;— `std::min` |
| `minmaxBy` | `∀a. LE a -> a -> a -> a `Pair` a` &nbsp;— `std::minmax` |
| `clamp` | `∀a. LE a -> a -> a -> a -> a` &nbsp;— `std::clamp` |
| `minmaxElement` | `∀a. LE a -> List a -> a `Pair` a` &nbsp;— `std::minmax_element` (errors on empty) |
| `equalBy` | `∀a b. (a -> b -> Bool) -> List a -> List b -> Bool` &nbsp;— `std::equal` |
| `lexicographicalLess` | `∀a. LE a -> List a -> List a -> Bool` &nbsp;— `std::lexicographical_compare` |
| `compareBy` | `∀a. LE a -> List a -> List a -> Int` &nbsp;— `std::lexicographical_compare_three_way` (`Int` for `strong_ordering`) |
| `isPermutation` | `∀a. (a -> a -> Bool) -> List a -> List a -> Bool` &nbsp;— `std::is_permutation` |
| `nextPermutation` | `∀a. LE a -> List a -> Maybe (List a)` &nbsp;— `std::next_permutation` |
| `prevPermutation` | `∀a. LE a -> List a -> Maybe (List a)` &nbsp;— `std::prev_permutation` |

**Sections 20-22: `<map>`/`<unordered_map>` dictionary operations** (see [chapter 14](#14-map-style-dictionary-operations))

| Function | Type |
|----------|------|
| `member` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> Bool` |
| `notMember` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> Bool` |
| `findWithDefault` | `∀a b c. b -> (a -> c -> Bool) -> c -> Dict a b -> b` |
| `atKey` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> b` |
| `keys` | `∀k v. Dict k v -> List k` |
| `values` | `∀k v. Dict k v -> List v` |
| `elems` | `∀k v. Dict k v -> List v` |
| `sizeDict` | `∀k v. Dict k v -> Int` |
| `nullDict` | `∀k v. Dict k v -> Bool` |
| `singletonDict` | `∀k v. k -> v -> Dict k v` |
| `lookupAll` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> List b` |
| `countKey` | `∀a b c. (a -> c -> Bool) -> c -> Dict a b -> Int` |
| `insert` | `∀k v. Equal k -> k -> v -> Dict k v -> Dict k v` &nbsp;— `std::map::insert` / `try_emplace` (insert-if-absent, no overwrite) |
| `insertWith` | `∀k v. (v -> v -> v) -> Equal k -> k -> v -> Dict k v -> Dict k v` &nbsp;— `Data.Map.insertWith` |
| `fromList` | `∀k v. Equal k -> List (k `Pair` v) -> Dict k v` &nbsp;— `Data.Map.fromList` (last value wins) |
| `fromListWith` | `∀k v. (v -> v -> v) -> Equal k -> List (k `Pair` v) -> Dict k v` &nbsp;— `Data.Map.fromListWith` |
| `adjust` | `∀k v. Equal k -> (v -> v) -> k -> Dict k v -> Dict k v` &nbsp;— `Data.Map.adjust` |
| `alter` | `∀k v. Equal k -> (Maybe v -> Maybe v) -> k -> Dict k v -> Dict k v` &nbsp;— `Data.Map.alter` (insert/update/delete) |
| `mapWithKey` | `∀k v1 v2. (k -> v1 -> v2) -> Dict k v1 -> Dict k v2` &nbsp;— `Data.Map.mapWithKey` |
| `mapMaybeWithKey` | `∀k v1 v2. (k -> v1 -> Maybe v2) -> Dict k v1 -> Dict k v2` |
| `mapKeysWith` | `∀k1 k2 v. (v -> v -> v) -> Equal k2 -> (k1 -> k2) -> Dict k1 v -> Dict k2 v` |
| `update` | `∀k v. Equal k -> (v -> Maybe v) -> k -> Dict k v -> Dict k v` |
| `updateWithKey` | `∀k v. Equal k -> (k -> v -> Maybe v) -> k -> Dict k v -> Dict k v` |
| `filterWithKey` | `∀k v. (k -> v -> Bool) -> Dict k v -> Dict k v` |
| `filterValues` | `∀k v. (v -> Bool) -> Dict k v -> Dict k v` |
| `partitionDict` | `∀k v. (k -> v -> Bool) -> Dict k v -> Dict k v `Pair` Dict k v` |
| `union` | `∀k v. Equal k -> Dict k v -> Dict k v -> Dict k v` |
| `unionWith` | `∀k v. (v -> v -> v) -> Equal k -> Dict k v -> Dict k v -> Dict k v` |
| `unionWithKey` | `∀k v. (k -> v -> v -> v) -> Equal k -> Dict k v -> Dict k v -> Dict k v` |
| `unionsWith` | `∀k v. (v -> v -> v) -> Equal k -> List (Dict k v) -> Dict k v` |
| `unions` | `∀k v. Equal k -> List (Dict k v) -> Dict k v` |
| `intersection` | `∀k v1 v2. Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1` |
| `intersectionWith` | `∀k v1 v2 v3. (v1 -> v2 -> v3) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v3` |
| `intersectionWithKey` | `∀k v1 v2 v3. (k -> v1 -> v2 -> v3) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v3` |
| `difference` | `∀k v1 v2. Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1` |
| `differenceWith` | `∀k v1 v2. (v1 -> v2 -> Maybe v1) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1` |
| `differenceWithKey` | `∀k v1 v2. (k -> v1 -> v2 -> Maybe v1) -> Equal k -> Dict k v1 -> Dict k v2 -> Dict k v1` |
| `restrictKeys` | `∀k v. Equal k -> Dict k v -> List k -> Dict k v` |
| `withoutKeys` | `∀k v. Equal k -> Dict k v -> List k -> Dict k v` |
| `foldrWithKey` | `∀k v b. (k -> v -> b -> b) -> b -> Dict k v -> b` |
| `foldlWithKey` | `∀k v b. (b -> k -> v -> b) -> b -> Dict k v -> b` |
| `isSubmapOfBy` | `∀k v1 v2. (v1 -> v2 -> Bool) -> Equal k -> Dict k v1 -> Dict k v2 -> Bool` |
| `isSubmapOf` | `∀k v. Equal v -> Equal k -> Dict k v -> Dict k v -> Bool` |
| `disjoint` | `∀k v1 v2. Equal k -> Dict k v1 -> Dict k v2 -> Bool` |

**23. `<numeric>` equivalents**

| Function | Type |
|----------|------|
| `accumulate` | `(b -> a -> b) -> b -> List a -> b` |
| `reduce` | `(a -> a -> a) -> List a -> a` |
| `innerProduct` | `∀a b c d. (c -> d -> c) -> (a -> b -> d) -> c -> List a -> List b -> c` |
| `partialSum` | `∀a. (a -> a -> a) -> List a -> List a` |
| `adjacentDifference` | `∀a. (a -> a -> a) -> List a -> List a` |
| `inclusiveScan` | `(a -> a -> a) -> List a -> List a` |
| `exclusiveScan` | `∀a. (a -> a -> a) -> a -> List a -> List a` |
| `transformReduce` | `∀a b. (b -> b -> b) -> (a -> b) -> b -> List a -> b` |
| `transformReduce2` | `∀a b c d. (c -> d -> c) -> (a -> b -> d) -> c -> List a -> List b -> c` |
| `transformInclusiveScan` | `∀a b. (b -> b -> b) -> (a -> b) -> List a -> List b` |
| `transformExclusiveScan` | `∀a b. (b -> b -> b) -> (a -> b) -> b -> List a -> List b` |

**24. Typelevel-parity conveniences**

This final source section supplies the portable operations that historically
existed only in `Typelevel`: finite `range`/`iterateN`/`cycleN`, `factorial`,
`isPalindrome`, `isRotation`, `isJust`, `dup`, `andList`/`orList`, `intercalate`,
one-shot `removeOnce`/`replaceOnce`, numeric aggregates, and conventional
unsuffixed spellings for list, comparison, and association operations. Equality
and ordering remain explicit Church-Boolean arguments. See the workspace
[`API-PARITY.md`](../../API-PARITY.md) for the full semantic crosswalk and the
small set of representation-specific exceptions.

