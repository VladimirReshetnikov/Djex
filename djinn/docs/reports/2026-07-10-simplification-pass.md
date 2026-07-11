# Djinn simplification and readability pass

- Date: 2026-07-10
- Baseline: `ad44d6d` (all three test suites green)
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0, Windows 11
- Companion report: [2026-07-10-code-review.md](2026-07-10-code-review.md)
  (the correctness review that preceded this pass)

## Executive summary

This pass re-read every module under `djinn/src/` with a different question
than the earlier correctness review: not "is it right?" but "is it as short,
deduplicated, and readable as it should be?" The result is a net reduction of
27 physical lines across `src/` *including* newly added clarifying comments,
with no behavioral change to valid inputs beyond three deliberate, small
fixes:

1. `:delete` of an undefined name now reports an error instead of silently
   succeeding.
2. `:delete` now parses qualified axiom names, so an axiom added as
   `Data.Function.id :: a -> a` is no longer undeletable.
3. The prefix function arrow `(->)` is lexed as one token, so the previously
   accepted `( - > )` (which GHC rejects) is now a parse error.

The dominant theme of the mechanical changes is **deduplication**: three
helper functions were maintained in two modules each, and two more re-derived
standard-library functions. Every change was validated against the full test
matrix (23 unit regressions, 600 property cases, 7 CLI subprocess scenarios),
plus manual REPL smoke tests of the touched paths.

## Scope and method

1. Read all eleven modules under `src/` (2,872 lines at the start), the Cabal
   description, the embedded help, and the prior review report.
2. Inventoried duplicated helpers, hand-rolled standard functions, dead
   tuple-threading, nesting that obscured control flow, and comments that were
   missing where the code is genuinely subtle.
3. Applied changes in two commits, running `cabal build all` (warning-clean
   under `-Wall -Wcompat`) and `cabal test all` after each.
4. Smoke-tested the interactive executable on the touched surfaces: prefix
   arrows, contexts, tuple projections, `+multi`, `cutoff`, qualified axiom
   addition/deletion, and unknown-name deletion.

Changes that could alter proof-search order, latency, or the accepted type
language were deliberately out of scope, with the one lexical exception noted
below. The open findings R-07 through R-12 from the companion report remain
open and are not restated in full here.

## Simplifications applied

### S-01 — `formulaSymbols` was defined twice

`LJT.hs` and `ProofEnv.hs` each carried an identical five-clause traversal
collecting every `Symbol` in a `Formula`. Both freshness mechanisms depend on
it agreeing with the `Formula` datatype, so a divergence (say, after adding a
constructor) could silently reintroduce the F-01/F-02 class of capture bugs in
whichever copy was forgotten. The function now lives next to the datatype in
`LJTFormula.hs`, is exported (and re-exported through `LJT`), and carries a
comment explaining *why* both proof variables and empty-type tags must be
reserved.

### S-02 — `schar`/`sstring`/`pParen` were defined twice

`Djinn.hs` and `HTypes.hs` each had private copies of the token-level `ReadP`
helpers (skip spaces, then match). They are now exported from `HIdentifier`,
which was already the shared lexical-rules module imported by both parsers.
Ten lines of duplication removed; one place to change tokenization.

### S-03 — hand-rolled standard functions

- `atIndex` (safe list indexing) was defined in both `LJT.hs` and
  `ProofCheck.hs`; both now use `Data.List.(!?)` (in `base` since 4.19).
- `joinWith` in `ProofCheck.hs` was `Data.List.intercalate`; replaced.
- `isPrefix` in `Djinn.hs` re-derived `isPrefixOf` with three length
  calculations; it is now `not (null p) && p `isPrefixOf` s` (the non-null
  guard is load-bearing: an empty prefix must not match every command).
- `freshMetas` in `ProofCheck.hs` was `mapM (const freshMeta) [1 .. count]`;
  now `replicateM count freshMeta`.
- `sortBy (\ (x,_) (y,_) -> compare x y)` in the query scorer is now
  `sortOn fst`.
- `many1 (satisfy (`elem` ['0'..'9']))` in the `cutoff` parser is now
  `munch1 isDigit`.
- Seven occurrences of `maybe x id $ lookup ...` across `HTypes.hs`,
  `ProofEnv.hs`, and `LJT.hs` are now `fromMaybe`.

### S-04 — monadic noise in pure-shaped code

`liftM`/`liftM2` chains in `LJT.hs` (`subst`, `copy`, `nf`) and the manual
bind-and-rebuild blocks in `ProofCheck.zonk` and `HCheck.ground` were
rewritten in applicative style (`Lam s <$> substitute body`,
`Product <$> mapM zonk elements`, `Sum <$> mapM (traverse zonk) alternatives`,
`KArrow <$> ground k1 <*> ground k2`). `zonk` shrank from 21 lines to 9 while
gaining a comment stating the invariant that makes the wildcard clause safe
(`Meta`, `EmptyType`, and `Atom` are leaves after pruning).

### S-05 — `ProofEnv.build` threaded dead accumulators

The internal-name generator returned a four-tuple whose last two components
(`finalUsed`, `finalNext`) were discarded at the only call site. The recursion
now returns just the two association lists it exists to build. Eleven lines
became six, and the shape now says what the function does.

### S-06 — the query result ladder was four cases deep

`Djinn.query` handled "proof check failed" and "rendering failed" with nested
`case ... of Left/Right` blocks whose error handling differed only in the
message prefix. Both steps now run in a single `Either` pipeline with a
`labeled` prefix combinator, and the success path sits one indentation level
below the search result instead of three. The comment on the pipeline states
the ordering constraint that forced this structure in the first place: every
candidate must be checked against the requested formula *before* display
names are restored.

### S-07 — twin lookup helpers in `HCheck`

`getVarHKind` and `getConHKind` differed only in their error string; they are
now one `lookupHKind` parameterized by the message, and the two call sites in
`iHKind` lost their redundant `do` wrappers.

### S-08 — miscellaneous readability

- `inIt` (named to dodge the `Prelude.init` clash) is now `welcome`, which is
  what it does.
- A `case v == v' of True -> ...` in `combPat` is now an `if`.
- `pHTVar`/`pHTCon` use `fmap` instead of `>>= return .`.
- The arrow separators in `pHType`/`pHKind` use the shared `sstring "->"`
  instead of an inline two-character sequence.
- New comments where the code is subtle rather than merely dense: the
  eta-reducer's simultaneous occurs-check, the kind-checker's
  unification-variable state, `redtop`'s fold-prove-apply-normalize shape,
  and the clause-scoring heuristic in `query`.

## Fixed flaws

### N-01 — `:delete` silently accepted undefined names

`:delete nosuch` filtered three lists, removed nothing, revalidated the
unchanged environment, and reported success. Typos in a deletion therefore
went unnoticed — particularly misleading in command files. `runCmd` now
checks membership across axioms, types, and classes first and reports
`Error: cannot delete nosuch: it is not defined`.

### N-02 — qualified axioms could be added but not deleted

`pAdd` accepts qualified external names (`Data.Function.id :: a -> a`), but
`pDel` parsed only unqualified identifiers, so such an axiom could never be
removed short of `:clear`. `pDel` now uses the same name grammar as `pAdd`
(plus plain constructor names for types and classes).

### N-03 — the prefix arrow tolerated interior white space

`(->)` was parsed as four independent space-skipping tokens, so `( - > )`
was accepted as the function-type constructor even though GHC rejects it,
while the infix arrow correctly required the two characters to be adjacent.
Both spellings now lex the arrow as one token. (Strictly a language change,
but only for inputs that were previously accepted by accident.)

Also fixed in passing: the verbose help described `cutoff=N` with the
garbled phrase "compute at most positive N solutions".

## Observations kept out of scope

These were noticed but deliberately not changed; none is a soundness issue.

- **O-01 — comment stripping is lexically naive.** `stripComments` treats
  every `--` in a command file as a comment start, so a (legal) operator such
  as `(--*)` cannot appear in a file even though the interactive parser
  accepts it. Real Haskell lexing treats `--*` as an operator. Fixing this
  means teaching the stripper operator lexing for a vanishingly rare case;
  the current behavior loses nothing but that corner.
- **O-02 — a bare `HTCon "[]"` displays as `([])`.** It cannot be produced by
  the parser (list types only arrive fully applied via `[a]`), so the
  non-round-tripping display is reachable only through the raw constructors
  already covered by finding R-10 in the companion report.
- **O-03 — `checkMethods` kind-checks all method types as one tuple**, so a
  kind error in a class declaration names no method. `Environment.checkClass`
  already reports per-method with context; unifying them would change several
  user-visible message formats and belongs with the R-08 class-model work.
- **O-04 — `Djinn.hs` remains the widest module** (parser, state, validation,
  orchestration, help). The companion report's suggestion of a pure command
  evaluator separated from the IO shell still stands (R-09 would fall out of
  it); it is a structural change, not a cleanup.
- **O-05 — the `P` monad's list-based backtracking** and the linear
  `AtomImps`/`NestImps` structures are unchanged, per the standing decision
  (R-07) that search-order changes need a benchmark corpus first.

## Change footprint

| Module | Change |
| --- | ---: |
| `Djinn.hs` | −5 lines, plus the two `:delete` fixes |
| `HCheck.hs` | −8 lines |
| `HIdentifier.hs` | +17 lines (now hosts the shared token helpers) |
| `HTypes.hs` | −11 lines |
| `LJT.hs` | −13 lines |
| `LJTFormula.hs` | +11 lines (now hosts `formulaSymbols`) |
| `ProofCheck.hs` | −21 lines |
| `ProofEnv.hs` | −10 lines |
| `Help.hs` | wording fix |

Net: −27 lines (2,872 → 2,845) with more comments than before. The two
helper-hosting modules grew so that four other modules could shrink and stop
diverging.

## Validation

```text
cabal build all        -- warning-clean under -Wall -Wcompat
cabal test all         -- djinn-tests 23/23, djinn-property-tests 3×200,
                       -- djinn-cli-tests 7/7
```

Manual REPL checks: `(->) a b -> a -> b`, `(Eq a) => a -> Bool`,
`((a,b),(c,d)) -> (b,c)`, `:set cutoff=5`, `:set +multi` with
`a -> a -> a` (both solutions, correct order), qualified axiom add/delete
round-trip, unknown-name deletion diagnostic, and EOF exit.
