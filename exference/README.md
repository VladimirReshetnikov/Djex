# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\ b -> fmap (\ g -> show (b g))`

[Djinn](https://hackage.haskell.org/package/djinn) is a well known tool that
does something similar; the main difference is that *Exference* supports a
larger subset of the haskell type system - most prominently type classes. This
comes at a cost, however: *Exference* makes no promise regarding termination.
Where *Djinn* tells you "there are no solutions", exference will keep trying,
sometimes stopping with "i could not find any solutions".

# Links

- **Documentation: [exference.pdf](https://github.com/lspitzner/exference-paper/raw/master/exference.pdf)** describes the implementation and properties;
- exferenceBot on freenode IRC #exference
    - play around without installing exference locally
    - reacts to `:exf` prefix, i.e. `:exf "Monad m => m (m a) -> m a"`
    - `/msg exferenceBot help`
    - uses the environment (i.e. known functions+typeclasses) at https://github.com/lspitzner/exference/tree/master/environment

# Building from source

The library and deterministic test suite build with GHC 9.12.4 and Cabal
3.16.1.0:

```text
cabal build all
cabal test all --test-show-details=direct
```

`exference-core` is a named, parser-independent library rooted at `src-core/`.
The unnamed `exference` library contains the `haskell-src-exts` frontend and
environment loader and re-exports the historical core API for compatibility.
This mirrors Djinn's library-first organization and lets future shared
frontends depend on the search engine without inheriting source-parser,
filesystem, or process dependencies.

The `exference` executable is a normal build target again. Its obsolete Hood,
search-tree, parallel-mode, and embedded manual-test machinery has been
removed; deterministic regressions live in `exference-tests`.

```text
cabal run exference -- --first "a -> a"
```

# Usage notes

There are certain types of queries where *Exference* will not be able to find
any / the right solution. Some common current limitations are:

- By default, searches **only for solutions where all input is used up**, e.g.
  `(a, b) -> a` will not find a solution (unless given `--allowunused` flag).
  Often this is the desired behaviour, consider queries such as
  `(a->b) -> [a] -> [b]` where a trivial solution would be `\_ _ -> []`.
  This also means that certain functions are not included in the environment,
  e.g. `length` or `mapM_`, as they "lose information";
- Type synonyms are expanded before search; cycles and unsaturated uses are
  rejected with diagnostics;
- Kinds are not checked, e.g. `Maybe -> Either`
  (which can be seen as both advantage and disadvantage, see report);
- The environment is composed by hand currently, and does only include parts
  of base plus a few other selected modules. Additions welcome!
- Pattern-matching on multiple-constructor data-types is not supported;
- See also the detailed feature description in the [exference.pdf](https://github.com/lspitzner/exference-paper/raw/master/exference.pdf) report.

## Experimental features

- Pattern-matching on multi-constructor data types can be enabled via
  `-c --patternMatchMC`, but reduces performance significantly for any
  non-trivial queries. Core algorithm needs re-write to optimize stuff
  sufficiently I fear.
- Rank-N positions are currently rejected conservatively. The historical
  implementation erased some quantifiers during unification, which was not a
  sound implementation of subsumption.

## Other known (technical) issues

- **Memory consumption is large** (even more so when profiling);
- The executable still owns environment loading and presentation policy. A
  reusable session layer should move those decisions below the CLI before the
  Djinn and Exference frontends are unified.

## Contributing

### environment

If you want to add new elements to the environment, be careful not to add
functions that
- are just synonyms of other functions (including cases such as `mapM` vs `forM`);
- lose information, e.g. `void :: Functor f => f a -> f ()`;

and avoid adding functions that
- are polymorphic in their return type (as they increase the search space
  for any query) - if really necessary, they can be added including an
  appropriate rating entry;
- are just more specific versions of existing functions.

## Trivia

* The author did not learn about the term "entailment" until after implementing
  the respective part of the algorithm.
* *Exference* was used at least once to implement some typed hole in its own
  source code.

## IRC

`#exference`
