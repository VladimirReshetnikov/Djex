# Djinn simplification and readability pass

- Date: 2026-07-10
- Baseline: `ad44d6d` (all three test suites green)
- Toolchain: GHC 9.12.4, Cabal 3.16.1.0, Windows 11
- Companion report: [2026-07-10-code-review.md](2026-07-10-code-review.md)
  (the correctness review that preceded this pass)

## Executive summary

This pass re-read every module under `djinn/src/` with a different question
than the earlier correctness review: not "is it right?" but "is it as short,
deduplicated, and readable as it should be?" The mechanical outcome is a net
reduction across `src/` *including* newly added clarifying comments, with the
dominant theme being **deduplication**: three helper functions were maintained
in two modules each, and two more re-derived standard-library functions.

Smoke-testing the pass against the built-in help's own examples then exposed
the most important finding of the day, a **completeness regression** that had
survived the earlier correctness review: after the nominal-`Empty` rework,
`redsucc` rejected every empty goal outright, so no theorem whose final goal
is an empty type could be proven. The flagship example from the built-in
help — the double negation of the law of excluded middle — was reported
"cannot be realized". The fix (N-01 below) restores upstream provability and
uncovered a follow-on defect in the independent proof checker (N-02), found
by QuickCheck within two full-matrix runs.

Five further deliberate fixes ride along: unary constructor fields no longer
render with singleton-tuple parentheses (`CD (c)` → `CD c`); `:delete` of an
undefined name reports an error instead of silently succeeding; `:delete`
accepts qualified axiom names, so a qualified axiom is no longer undeletable;
the prefix arrow `(->)` is lexed as one token (the accidentally accepted
`( - > )` is now a parse error); and the property suite's 200-case setting is
now a floor that `--quickcheck-tests` can raise.

Every change was validated against the full test matrix — now 27 unit
regressions, three properties at 200 cases (also exercised at 2,000), and 7
CLI subprocess scenarios — plus manual REPL smoke tests of the touched paths.

## Scope and method

1. Read all eleven modules under `src/` (2,872 lines at the start), the Cabal
   description, the embedded help, and the prior review report.
2. Inventoried duplicated helpers, hand-rolled standard functions, dead
   tuple-threading, nesting that obscured control flow, and comments that were
   missing where the code is genuinely subtle.
3. Applied changes in four commits, running `cabal build all` (warning-clean
   under `-Wall -Wcompat`) and `cabal test all` after each.
4. Smoke-tested the interactive executable against the verbose help's own
   worked examples plus the touched surfaces: prefix arrows, contexts, tuple
   projections, `+multi`, `cutoff`, qualified axiom addition/deletion, and
   unknown-name deletion. This step is what exposed N-01 and N-03.

Changes that could alter proof-search order or latency for already-working
queries were deliberately out of scope; N-01 restores provability the
calculus was always supposed to have, and the lexical fix N-06 only rejects
an accidentally accepted spelling. The open findings R-07 through R-12 from
the companion report remain open and are not restated in full here.

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

### N-01 — Critical: empty goals were unprovable (completeness regression)

Baseline reproduction, straight from the built-in verbose help's own example:

```text
Djinn> f ? Not (Not (Either x (Not x)))
-- f cannot be realized.
```

The double negation of the law of excluded middle is an intuitionistic
theorem, and upstream Djinn proves it. The regression came from the
nominal-`Empty` rework (R-03 in the companion report): upstream encoded
falsehood as `Disj []`, so an empty *goal* flowed through the disjunction
rule — introduce a fresh continuation atom, add one implication antecedent
per disjunct (none), and prove the atom, which is only reachable through
ex falso reasoning. The rework's `redsucc (Empty _) = mzero` kept empty
*antecedents* working but silently removed that goal-side path, so any
query whose final goal reduces to an empty type with contradictory
antecedents — the entire "theorem proving with `Not`" use case advertised
in the help — failed.

`redsucc` now routes an empty goal through the same fresh-atom encoding as
a zero-alternative disjunction. The proof is parametric in the fresh atom,
so it proves the empty goal as well, and the independent checker verifies
it against the actual `Empty` formula. The help's example now produces
exactly the output the help promises:

```text
f :: Not (Not (Either x (Not x)))
f a = void (a (Right (\ b -> a (Left b))))
```

Bare `Void`, the law of excluded middle itself, and nominal-empty identity
versus cross-empty elimination all still behave correctly. A unit
regression covers the theorem end to end (search plus independent check).

The lesson recorded for future passes: the prior review validated the
nominal-empty change against empty *hypotheses* (`EmptyA -> EmptyB`) but
never re-ran the help file's `Not`-based theorem examples, which are the
only stock queries whose *goal* is empty.

### N-02 — High: the proof checker rejected valid ex-falso chains

Found by QuickCheck immediately after N-01 was fixed, because N-01 made a
new class of proofs reachable. For

```text
Not x -> Not c -> Not (Either x c)    where x = (a -> a) -> (b)
```

the LJT proof feeds one `Ccases []` (ex falso) result into another. The
intermediate proof type is a genuinely free metavariable — any empty
instantiation types the term — but `solveConstraints` treated "no constraint
made progress" as failure and reported `unresolved proof constraints: 1`,
so the query printed "generated an invalid proof" for a correct proof.

When the solver stalls, an `EmptyEliminator` constraint whose input is
still an unsolved metavariable is now defaulted to `Sum []` and solving
resumes. This is safe: the variable is unconstrained elsewhere (otherwise
some constraint would have made progress), and a term that also injects
into the defaulted type still fails unification on the next pass. Stuck
`Injection` constraints remain hard errors. The discovered formula is
pinned as a deterministic unit regression, and the failing QuickCheck seed
(`SMGen 4298580404438746955 8158366780631436653`, case 74) replays green.

### N-03 — unary constructor fields rendered as singleton tuples

A unary constructor's field arrives at the proof-term converter as a 1-ary
split (its field list is translated as a one-element conjunction), and the
resulting pattern kept a one-element `HPTuple` wrapper:

```text
bindCD a b = case a of CD (c) -> ...     -- before
bindCD a b = case a of CD c -> ...       -- after
```

`(c)` is legal Haskell, but the expression side already collapsed singleton
tuples (`hETuple [e] = e`); the pattern side now mirrors it at the split
conversion. This also restores the exact output shown in the built-in
help's continuation-monad examples. Covered by a unit regression.

### N-04 — `:delete` silently accepted undefined names

`:delete nosuch` filtered three lists, removed nothing, revalidated the
unchanged environment, and reported success. Typos in a deletion therefore
went unnoticed — particularly misleading in command files. `runCmd` now
checks membership across axioms, types, and classes first and reports
`Error: cannot delete nosuch: it is not defined`.

### N-05 — qualified axioms could be added but not deleted

`pAdd` accepts qualified external names (`Data.Function.id :: a -> a`), but
`pDel` parsed only unqualified identifiers, so such an axiom could never be
removed short of `:clear`. `pDel` now uses the same name grammar as `pAdd`
(plus plain constructor names for types and classes).

### N-06 — the prefix arrow tolerated interior white space

`(->)` was parsed as four independent space-skipping tokens, so `( - > )`
was accepted as the function-type constructor even though GHC rejects it,
while the infix arrow correctly required the two characters to be adjacent.
Both spellings now lex the arrow as one token. (Strictly a language change,
but only for inputs that were previously accepted by accident.)

Also fixed in passing: the verbose help described `cutoff=N` with the
garbled phrase "compute at most positive N solutions", and the property
suite's `localOption (QuickCheckTests 200)` silently *capped* the case
count; it is now `adjustOption (max ...)`, a floor that
`--test-options='--quickcheck-tests=N'` can raise.

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
  (Superseded later the same day: see
  [2026-07-10-search-budget.md](2026-07-10-search-budget.md).)

## Change footprint

| Module | Change |
| --- | --- |
| `Djinn.hs` | −5 lines of ladder/duplication, plus the two `:delete` fixes |
| `HCheck.hs` | −8 lines (merged lookups, applicative `ground`) |
| `HIdentifier.hs` | +17 lines (now hosts the shared token helpers) |
| `HTypes.hs` | −11 lines, plus the singleton-pattern fix (N-03) |
| `LJT.hs` | −13 lines, plus the empty-goal fix (N-01) |
| `LJTFormula.hs` | +11 lines (now hosts `formulaSymbols`) |
| `ProofCheck.hs` | −21 lines, plus stuck-constraint defaulting (N-02) |
| `ProofEnv.hs` | −10 lines |
| `Help.hs` | wording fix |
| `test/Spec.hs` | two new regression groups (25 → 27 cases) |
| `test/PropertySpec.hs` | case-count cap became a floor |

The simplification portion alone was net −27 source lines with more comments
than before; the two helper-hosting modules grew so that four other modules
could shrink and stop diverging. The N-01/N-02 fixes then added back a
commented dozen.

## Validation

```text
cabal build all        -- warning-clean under -Wall -Wcompat
cabal test all         -- djinn-tests 27/27, djinn-property-tests 3×200,
                       -- djinn-cli-tests 7/7; five consecutive full-matrix
                       -- runs (fresh QuickCheck seeds each)
cabal test djinn-property-tests --test-options='--quickcheck-tests=2000'
                       -- 3×2000, 100% proof-generation rate
```

The property suite's proof-generation rate rose from ~99.5% to 100%: with
N-01 fixed, every formula the generator constructs as provable now actually
yields a proof.

Manual REPL checks: the full verbose-help example set (continuation monad
`returnCD`/`bindCD`/`callCCD`, state monad, double-negated excluded middle —
all outputs now match the help text exactly), `(->) a b -> a -> b`,
`(Eq a) => a -> Bool`, `((a,b),(c,d)) -> (b,c)`, `Not (Not (Not a)) -> Not a`,
De Morgan for `Either`, `EmptyA -> EmptyA` vs `EmptyA -> EmptyB`, bare `Void`
and `Either x (Not x)` still unprovable, `:set cutoff=5`, `:set +multi` with
`a -> a -> a` (both solutions, correct order), qualified axiom add/delete
round-trip, unknown-name deletion diagnostic, and EOF exit.
