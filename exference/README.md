# Exference

Exference is a Haskell tool for generating expressions from a type, e.g.

Input: `(Show b) => (a -> b) -> [a] -> [String]`

Output: `\b -> fmap (\g -> show (b g))`

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
3.16.1.0. Cabal is the maintained build path; the historical Stack file
targeted LTS 5.18 and dependencies that no longer exist in this tree, so it has
been removed rather than pretending to provide a second supported toolchain.

```text
cabal build all
cabal test all --test-show-details=direct
```

`exference-core` is a named, parser-independent library rooted at `src-core/`.
It is explicitly public and depends only on the shared synthesis vocabulary
plus its search data structures and transformer stack; it does not inherit
`haskell-src-exts`, filesystem/process libraries, or executable dependencies.
The unnamed `exference` library contains the `haskell-src-exts` frontend and
environment loader and preserves the historical core import paths.
This mirrors Djinn's library-first organization and lets future shared
frontends depend on the search engine without inheriting source-parser,
filesystem, or process dependencies.

Core names are validated, structural wrappers over `haskell-synthesis`: module
segments, ordinary identifiers and operators, list/cons/function constructors,
and boxed tuples can no longer be confused by rendered spelling.  The legacy
`QualifiedName(..)` constructor surface remains source-compatible, while new
code can use checked smart constructors.  Unqualified frontend lookup now
rejects ambiguous imported type names instead of silently choosing the first.

Class constraints are finite nominal values backed by
`Language.Haskell.Synthesis.Constraint`; they contain a validated class name
and arguments, never a recursively embedded declaration. Class declarations
and instances live in sealed strict maps built by `mkStaticClassEnv`, which
checks names, duplicate declarations/parameters, superclass variables and
cycles, referenced classes, and exact arities before superclass inflation.
Query and binding inputs likewise reject wrong arities for known classes while
retaining unknown classes as explicit external constraints.

`toSynthesisType` and `fromSynthesisType` adapt Exference's flexible and rigid
type IDs, applications, arrows, tuples, foralls, and constraints to the shared
source-type IR. The checked reverse conversion rejects rigid forall binders and
shared names outside Exference's representable subset rather than weakening
them during lowering.

`Language.Haskell.Exference.Core.Declaration` converts function bindings,
classes, instances, and deconstructor/data records to the shared declaration
IR; the HSE frontend uses the same boundary for type synonyms. Function
penalties and recursive-datatype flags survive as explicit metadata, while
lossy reverse conversions (such as dropping separately stored class methods)
are rejected.

Whole core environments also round-trip through the shared sealed declaration
inventory. `StaticClassEnv` retains explicit instance declarations separately
from its superclass-inflated lookup index, so adapters serialize source facts
rather than derived cache entries. The core-only adapter still rejects
frontend-only declarations such as type synonyms because its search
dictionary has no representation for them.

The HSE loader returns a parameterized `SourceEnvironment` record rather than
an anonymous five-tuple. Parsed functions, deconstructors, classes, datatype
names, and synonyms therefore remain one named inventory through rating and
CLI loading. `toSynthesisSourceEnvironment` seals that complete inventory in
the shared environment IR. Constructor signatures duplicated in Exference's
search-function list are represented only by their datatype declarations at
this boundary; list, unit, and tuple constructors have explicit intrinsic
datatype records, so `(:)` never masquerades as an ordinary value.
Class-environment construction likewise rejects repeated instance heads before
building its lookup index; each shipped primitive instance now has one owning
module instead of a second shadow declaration in `Data.hs`.
Sealing also runs the shared whole-inventory kind checker. Recursive datatypes
remain valid, recursive synonyms and ill-kinded signatures do not, and
unconstrained class parameters can generalize to support the shipped modern
poly-kinded `Typeable` vocabulary. The frontend selects the explicit open
inventory policy because loading a subset of modules deliberately retains
external type names after reporting them as warnings.

Exference's implicit instance variables become explicit binders at that shared
boundary. Reverse lowering accepts exactly the free flexible variables of the
instance context and head, preventing an unused or rigid shared binder from
silently changing meaning when returned to Exference's implicit form.

## Exference 1.7 migration

Version 1.7 intentionally breaks the old recursive class representation.
`HsConstraint` now matches `HsConstraint QualifiedName [HsType]`;
`HsInstance` stores prerequisites plus an `instance_head`; class collections
are strict `Map QualifiedName HsTypeClass` values; and `mkStaticClassEnv`
returns `Either ClassEnvError StaticClassEnv`. `StaticClassEnv` and
`QueryClassEnv` expose read-only accessors rather than updateable record fields.
The generic `Data` instances that depended on the recursive representation
were removed. Imports through `Language.Haskell.Exference` and the former core
module paths remain available, but callers constructing class values must
adopt the checked API.

Completed candidates are simplified inside the core and the exact transformed
tree is independently type-checked before it is returned.  Simplification is
environment-free and never invents globals such as `id` or `(.)`.  The typed
candidate is then erased into `Language.Haskell.Synthesis.Generated`, the same
scope-safe output tree and renderer used by Djinn.  That shared boundary
allocates names by variable identity, avoids binder/global capture, and applies
one qualification policy.  The `haskell-src-exts` converter remains only as a
compatibility frontend and consumes the shared allocator rather than owning a
second naming implementation.
`toGeneratedSearchBatch` projects status-bearing Exference chunks to the common
`SearchBatch` envelope with shared generated-expression candidates, while
retaining typed expressions through the compatibility API.

Symmetric unification keeps goal and provider variables tagged until the final
projection, so substitutions returned for either side are closed even when the
two inputs reuse numeric IDs.  The independent checker consumes every prenex
`forall` layer with the same rigid-ID order as search, and type rendering uses
one source-name map for quantifiers, constraints, and body occurrences.

The status-bearing search API is `findExpressionsWithStatsEither`. It retains
structured input failures and distinguishes a genuinely exhausted search space
from a step-limited search and one made incomplete by queue/depth pruning. The
`toSearchProgress` projection maps those compatibility statuses to
`Language.Haskell.Synthesis.Search`, retaining simultaneous queue and depth
pruning reasons and rejecting malformed hand-constructed status values. It
does not turn heuristic exhaustion into a logical uninhabitability claim. The
historical list-returning entry points remain compatibility adapters (including
their “invalid input means no elements” convention). Selection functions are
also available separately over chunk streams. They return `SearchSelection`,
which folds the last inspected status through the policy without retaining the
consumed trace. The executable therefore validates and runs search once, then
says when an empty result is conclusive versus when inhabitation remains
undecided.

The `exference` executable is a normal build target again. Its obsolete Hood,
search-tree, parallel-mode, and embedded manual-test machinery has been
removed; deterministic regressions live in `exference-tests` and the separate
`exference-cli-tests` subprocess suite.

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
  of base plus a few other selected modules. Its canonical 41-class,
  485-source-instance inventory is checked at load time and pinned by tests.
  The [normalization report](docs/reports/2026-07-11-environment-normalization.md)
  documents its naming and validation rules. Additions welcome!
- Pattern-matching on multiple-constructor data-types is not supported;
- See also the detailed feature description in the [exference.pdf](https://github.com/lspitzner/exference-paper/raw/master/exference.pdf) report.

## Experimental features

- Pattern-matching on multi-constructor data types can be enabled via
  `-c --patternMatchMC`, but reduces performance significantly for any
  non-trivial queries. Core algorithm needs re-write to optimize stuff
  sufficiently I fear.
- Chains of outer (prenex) `forall`s are supported. Rank-N positions are
  rejected conservatively; the historical implementation erased some nested
  quantifiers during unification, which was not a sound implementation of
  subsumption.

## Other known (technical) issues

- **Memory consumption is large** (even more so when profiling);
- The executable still owns environment loading. Search execution and result
  selection now have a reusable boundary, but a common Djinn/Exference session
  layer still needs to move environment policy below both CLIs.
- The detailed [Djinn/Exference integration audit](docs/reports/2026-07-11-djinn-integration-audit.md)
  records concrete correctness reproducers, shared-IR boundaries, and the
  staged migration order.

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
