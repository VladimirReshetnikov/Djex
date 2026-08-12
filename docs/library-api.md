# Djex library quick start

This guide uses the checked Djex APIs. Historical Djinn and Exference modules
remain importable for compatibility, but new integrations should build around a
sealed session, a checked request, and the shared result envelope.

## Build and install

Djex currently supports GHC 9.12.4. From the repository root:

```console
cabal build all
cabal test all -j1 --test-show-details=direct
```

The complete run is serial because its CLI suites invoke freshly linked Djex
executables as subprocesses. Focused library-only suites may run concurrently.

Run without installing:

```console
cabal run exe:djex
cabal run exe:djex -- djinn --render expression "a -> a"
cabal run exe:djex -- exference --select first "a -> a"
cabal run exe:djex -- download CABAL_TARGET
cabal run exe:djex -- install [--lib] CABAL_TARGET
```

Or install the three commands into Cabal's executable directory:

```console
cabal install exe:djex exe:djinn exe:exference
```

A downstream Cabal component needs one dependency:

```cabal
build-depends: djex
```

## Choose a backend

| Requirement | Djinn | Exference |
| --- | --- | --- |
| Terminating unbudgeted search for the supported logic | Yes | No |
| Proof-backed non-inhabitation result | Yes when formula translation is complete | No |
| Ranked heuristic candidates | No | Yes |
| Explicit prenex polymorphism | Yes at the checked request edge | Yes |
| Bounded rank-N rule | Positive introduction, including validated contexts under dictionary-independent semantics, through singleton, pairwise, triple, quadruple, and capped quintuple occurrence frontiers; historical context-free hypothesis instantiation at variable and guarded-quantified candidates; a positive-only query-correlated tail requiring a result-relevant quantified choice and an exact query-subtree result while excluding historical logical formulas; an established positive-only query-closed family adding closed, forall-free query subtrees while requiring at least one per tuple; separate per-use loaded-scheme instantiation at query/value-signature candidates; exact externally established provider-assignment vectors; and positive-only nominal transport through reachable parameterized datatype applications | Contextual quantified-goal introduction with lexical givens and escape-checked skolems; scoped-provider instantiation; closed visible instantiation selected by monomorphic instance heads or, for fully vacuous scoped and retained global providers, checked query monotypes and polytypes; exact externally established provider-assignment vectors; and guarded context-free shallow quantified-provider subsumption |
| Type-class participation | Validates contexts; synthesizes only dictionary-independent terms | Resolves givens, superclasses, and instances |
| Main controls | Candidate and choice-point limits | Step, queue, depth, constraint, and pattern controls |

Neither backend guesses the other's semantics. One-shot commands and checked
library calls select an engine explicitly; the shared REPL stores an explicit
active selection that can be `djinn`, `exference`, or `both`.

Both adapters represent an obligation as the same shared
`Constraint (Type variable)` value. `TypeClassConstraints` therefore means
that an engine accepts and checks that common syntax, not that the engines have
the same evidence semantics. Exference resolves nominal evidence. Djinn checks
class existence, arity, and kinds, then deliberately withholds class methods
from its propositional proof environment.

## Embed the shared terminal REPL

Import the interactive launcher separately from the main facade:

```haskell
import Language.Haskell.Djex.REPL
  ( ReplBackend (BothBackends)
  , ReplOptions (..)
  , defaultReplOptions
  , runRepl
  )
import System.Exit (ExitCode)

runProjectRepl :: IO ExitCode
runProjectRepl = runRepl defaultReplOptions
  { replInitialBackend = BothBackends
  , replEnvironmentPath = Just "./environment"
  , replAllowFix = False
  , replHistoryFile = Just "./.djex-history"
  , replIgnoreStartupFiles = False
  }
```

`ReplBackend` also provides `OneBackend DjinnBackend` and
`OneBackend ExferenceBackend`; import `Backend(..)` from
`Language.Haskell.Djex` when constructing those values. `defaultReplOptions`
starts on Djinn, loads the installed source workspace with the command-safe
no-fix policy, and keeps history only for the process lifetime.

`runRepl` returns an `ExitCode` and does not terminate the host application.
EOF and `:quit` are successful. Individual query, setting, shell, script,
package-command, and environment-load diagnostics are recoverable and leave
the loop running;
failure before a usable loop can be constructed returns a failure status. An
initial Exference load failure is recoverable because the independent standard
Djinn session is still usable.

This API embeds a terminal frontend, not an abstract protocol: it reads through
Haskeline, writes results and diagnostics to the process streams, can change
the process working directory, and runs trusted `.djexrc` startup commands
unless `replIgnoreStartupFiles` is `True`. Interactive commands can launch the
configured editor or a shell, evaluate code through real GHC, and invoke Cabal
through `:download`/`:install`. Package installation can execute
target-supplied build code and does not add compiled modules to Djex's
source-only inventory. Package children receive no REPL stdin or unrelated
inherited file descriptors. Use the checked adapters below when an editor,
service, or GUI needs to own input, output, authorization, or session
persistence. The complete interactive contract, including startup-file trust
checks, commands, settings, transactional reloads, prompt scope, both-mode
isolation, and the distinction from the historical Djinn REPL, is in the
[shared REPL guide](repl.md).

## Embed the complete command dispatcher

Applications that deliberately want the executable's whole interface can call
the exposed dispatcher without letting it terminate the host process:

```haskell
import Language.Haskell.Djex.CLI (runArguments)
import System.Exit (ExitCode)

runDjexCommand :: [String] -> IO ExitCode
runDjexCommand = runArguments
```

`runArguments` accepts the same argument vector as `djex`, writes to the
process streams, and returns the status that the executable would pass to
`exitWith`. It may start the REPL or a synthesis search, read source and
startup files, change the working directory, execute evaluated Haskell, or
launch editor, shell, and Cabal child processes. It is therefore convenient
for another command-line program, but it is not an isolation boundary. Use the
checked adapters below for a server or tool that needs explicit control over
input, output, authorization, and execution.

## Common lifecycle

Both checked adapters follow the same shape:

1. Build or load a neutral declaration environment and seal a backend session.
2. Parse or construct a `QueryRequest`, then cross the backend's smart
   constructor to obtain an opaque checked request.
3. Run the request against a session.
4. Inspect `resultEvidence` independently from `batchProgress`.
5. Select candidates with `selectQueryResults` and render them through the
   backend convenience renderer or the shared generated-code renderer.

Parsing and source loading return structured `Diagnostic` values. Use
`renderDiagnostic` for compiler-shaped text, but retain the structure when an
editor or service can present codes, spans, and context separately.

The checked construction paths also bound width-bearing list spines before a
complete traversal: known class arguments are observed only through the first
extra cell, and tuples only through the first unsupported arity. This matters
for programmatic callers because a cyclic lazy list is otherwise a valid input
value. Such input returns a structured arity/type failure instead of making
environment construction, kind inference, or a backend request hang.

Construction and result consumption have intentionally different evaluation
contracts. Finite declaration indexes and application spines are built
strictly, so sealing a large environment or lowering a wide generated term
does not leave a deferred fold behind. Result sequences remain lazy: taking
the first admissible candidate does not inspect later batches, and a checked
result does not force candidate payloads or tails merely to prove that its
batch is nonempty. Keep the returned Exference result list lazy when streaming,
but apply an explicit selection or fold when a global best candidate is
required.

## Supply provider-local instantiation evidence

A frontend whose source environment proves otherwise erased type choices can
use three payloads spanning two evidence models:

- `ProviderInstantiationCandidate` associates one type with an exact provider's
  independent candidate pool. A multiply quantified provider reconstructs
  bounded tuples from that pool, so this is appropriate only when every
  resulting combination is justified.
- `ProviderInstantiationAssignment` associates one complete ordered vector
  with an exact provider. It preserves the argument order and correlation
  established by one source-language proof and is consumed once, without
  Cartesian reconstruction. Its compatibility runner infers positional kinds
  from the retained provider body and defaults a vacuous binder to `Type`.
- `KindedProviderInstantiationAssignment` preserves the same complete vector
  while pairing every argument with the `GroundKind` attested by the frontend.
  This is the route for source kind facts which the kind-erased provider body,
  especially a vacuous binder, cannot reconstruct.

The original candidate API remains available unchanged:

```haskell
candidateEvidence =
  [ ProviderInstantiationCandidate
      { providerInstantiationCandidateProvider = providerName
      , providerInstantiationCandidateType = candidateType
      }
  ]

djinnCandidateResult =
  runDjinnQueryWithInstantiationCandidates
    djinnSession candidateEvidence djinnRequest
exferenceCandidateResults =
  runExferenceQueryWithInstantiationCandidates
    exferenceSession candidateEvidence exferenceRequest
```

Use an assignment when the frontend has established the complete binder
vector, for example one active instance head fixing both `a` and `b`:

```haskell
assignments =
  [ ProviderInstantiationAssignment
      { providerInstantiationAssignmentProvider = providerName
      , providerInstantiationAssignmentArguments =
          [firstArgumentType, secondArgumentType]
      }
  ]

djinnAssignmentResult =
  runDjinnQueryWithInstantiationAssignments
    djinnSession assignments djinnRequest
exferenceAssignmentResults =
  runExferenceQueryWithInstantiationAssignments
    exferenceSession assignments exferenceRequest
```

Use the parallel kinded payload when the source proof also retains each
leading binder's exact ground kind:

```haskell
kindedAssignments =
  [ KindedProviderInstantiationAssignment
      { kindedProviderInstantiationAssignmentProvider = providerName
      , kindedProviderInstantiationAssignmentArguments =
          [ ( FunctionKind ProperTypeKind ProperTypeKind
            , higherKindedConstructor
            )
          , (ProperTypeKind, closedImpredicativeType)
          ]
      }
  ]

djinnKindedAssignmentResult =
  runDjinnQueryWithKindedInstantiationAssignments
    djinnSession kindedAssignments djinnRequest
exferenceKindedAssignmentResults =
  runExferenceQueryWithKindedInstantiationAssignments
    exferenceSession kindedAssignments exferenceRequest
```

The payload's type variable must match the selected backend's neutral
type-variable namespace. All payload types and all four bounds are exported
by `Language.Haskell.Djex`:

- `maximumProviderInstantiationCandidates` is 32 scalar associations per call;
- `maximumProviderInstantiationAssignments` is 32 complete vectors per call;
- `maximumProviderInstantiationArguments` is six ordered arguments per
  vector; and
- `maximumProviderInstantiationKindNodes` is 129 constructors in each
  caller-supplied `GroundKind`.

Each checked runner observes an outer list through at most its first extra cell
before entering an element. Assignment runners likewise bound each argument
spine before entering an argument. Over-wide or cyclic caller-built lists
therefore fail finitely.

For each assignment that passes provider, scheme, context, and exact-arity
checks, kinded runners apply a productive node observer to all its supplied
kinds before that assignment's kind inference, same-provider kind-vector
equality, backend kind conversion, or paired-type forcing. The observer counts
at most 129 constructors; if work remains, it returns the sentinel 130 without
entering the pending constructor. Cyclic kinds and finite trees above the bound
therefore fail finitely. The shared 64-tuple constructor's right-associated
all-`Type` kind contains exactly `2 * 64 + 1 = 129` nodes and remains accepted.
This kind capacity does not change the six-argument limit on one
provider-assignment vector.

Construction is not certification. The caller remains responsible for the
source-language reason that a provider may be selected at the supplied types.
The runners are the representation and session trust boundary. Candidate
runners require the exact `Name` to denote an eligible loaded global, elaborate
each type in that sealed session's synonym and kind scope, and accept only a
closed, context-free proper type representable as a visible argument. This
legacy scalar Candidate contract remains proper-type-only.
Exference's scalar route narrows that shape to a ground monotype or a complete
forall-rooted type and uses it only for a context-free provider whose complete
leading prefix is vacuous.

Assignment runners additionally require an exact retained polymorphic scheme,
a context-free leading chain of arity one through six, and a vector whose
length equals that complete chain; contextual schemes are unsupported. The
legacy assignment runners infer each binder's ground kind from the exact
provider body, default a vacuous binder to `Type`, and synonym-elaborate the
argument in that position at the inferred kind. Their behavior is unchanged.

The kinded runners instead consume the caller's complete positional
`GroundKind` vector. They check the provider body at `Type` while sharing each
binder with its supplied kind obligation, so every non-vacuous occurrence must
agree with the assertion. A vacuous binder has no occurrence capable of proving
its source kind: the supplied ground kind is frontend-attested, though the
backend still elaborates the paired argument at exactly that kind. Complete
kind vectors for repeated assignments to one provider must agree before type
vectors are alpha-deduplicated. `GroundKind` is `Kind Void`, so an unresolved
kind variable cannot cross this boundary.

Every argument in either form must be lexically closed and representable as a
specified visible type argument. A contextual polytype is accepted: its nested
constraints remain inside the selected argument rather than becoming provider
obligations. Both backends substitute the complete vector and independently
check that the whole specialized body has kind `Type` in the sealed inventory.
Exference then rechecks the retained scheme's closure, provider context, arity,
and visible-argument shape at its private search boundary. The
kinded form supports a bare or partially applied higher-kinded constructor at a
vacuous position and can mix it with a closed impredicative `Type` argument.
Unlike its scalar route, either exact Exference route may also instantiate
binders which occur in the provider body because the complete correlated vector
is already known.

For Djinn, this higher-kinded route includes the canonical bare boxed pair
constructor `(,)`. Its saturated application remains the normal structural
`TupleType`, so existing pair synthesis and rendering are unchanged. The
shared-to-Djinn projection still rejects a wider boxed or any unboxed tuple
constructor left bare or partially applied, reporting
`PartialTupleConstructorUnsupported`; callers must not treat the pair case as
general partial-tuple support. Djinn unit/public regressions cover round-trip
rendering, both rejection cases, non-vacuous exact substitution, and a
contextual rank-N assignment. The facade integration regression exercises the
same `(,)` and `Either Natural` heads through Djinn and Exference and asks GHC to
check all generated definitions.

First occurrences are retained. Scalar types are alpha-deduplicated per
provider; assignments are alpha-deduplicated as whole ordered vectors per
provider. Kinded assignments first require one consistent complete kind vector
per provider; kinds are not discarded to make conflicting assertions compare
equal. Distinct type vectors with that same kind vector remain independent
choices. The public-runner regressions include two such vectors for one vacuous
provider at the genuinely higher-order kind `(Type -> Type) -> Type`, one a
bare constructor and one a partially applied constructor. In every case the
provider remains part of the key, so an alpha-identical scheme under another
name receives no evidence. Target-named Djinn specializations remain
diagnostic-only, and Exference's normal exact target exclusion keeps the
requested definition out of provider search.

For Djinn, a non-vacuous vector participates in the structural datatype
projection only when each relevant argument has faithful nested flow and its
corresponding declaration parameter is observable in constructor fields. The
vector is marked before substitution, covering phantom structure inside an
assigned type and later arguments to an assigned higher-kinded head. Recursive,
unknown, and fully applied empty datatypes retain compilation's complete opaque
identity. This occurrence-sensitive boundary keeps valid constructor
elimination that a whole-formula nominal comparison would discard, while
independently erased phantom positions remain nominal-only.

The original runners retain their exact historical behavior. Each engine has a
canonical typed path, and each legacy result is its lazy
`typedCandidateCompatibility` projection. Empty candidate and assignment calls
return the same result, ordering, diagnostics, and finite-budget observations:

```haskell
runDjinnTypedQuery session =
  runDjinnTypedQueryWithInstantiationCandidates session []
runDjinnQuery session =
  fmap (fmap typedCandidateCompatibility) . runDjinnTypedQuery session
runExferenceQuery session =
  runExferenceQueryWithInstantiationCandidates session []
```

Exference's typed candidate may contain its checked graph. Djinn currently
returns the explicit `DjinnTermGraphSourceTypingContextUnavailable` absence:
the retained LJT proof evidence is exact for its formula but predates the
source-typed proof transformations needed by a shared graph. Its public future
graph domain is already `DjinnTermGraphType = Type (Variable
DjinnTypeVariable)`, not the role-erased compatibility `DjinnType`. Thus the
typed API unifies result association without claiming behavioral
interoperability before the required source-variable role plan exists.

Nonempty evidence is additive, with engine- and payload-specific scheduling:

| Engine | Independent candidate pool | Exact ordered assignments |
| --- | --- | --- |
| Djinn | The historical plain structural, nominal, and query-local-instantiation plans remain first. The positive-only provider family reconstructs bounded tuples: at most six binders, 512 attempts, sixteen specializations per scheme, and 32 direct provider premises. | The same positive-only provider-plan position receives one direct premise per retained vector and never uses the tuple-attempt or per-scheme Cartesian window. It still carries query-local and loaded instantiation axioms for mixed proofs and runs before evidence-free loaded tails. Each proof is checked before lowering restores the exact provider and visible arguments. |
| Exference | Exact retained-global lookup alone receives the pool. After ordinary implicit use, its visible order is ground monomorphic instance heads, checked query-derived choices, then supplied scalar choices. Query-derived and supplied products retain separate 32-combination caps. Scoped values and sibling globals never consult the map. | Exact retained-global lookup consumes each vector once. After ordinary implicit use, its visible order is ground monomorphic instance heads, exact supplied assignments, then checked query-derived choices. No Cartesian product or vacuous-body restriction is used. Scoped values and sibling globals never consult the map. |

Both evidence models can make a visible choice such as
`provider @(forall a. a -> a)` available when a richer frontend has already
established the necessary fact. Only the exact-assignment model preserves a
multi-binder choice such as `[T1, T2]` without also authorizing `[T1, T1]` or
`[T2, T1]`. Neither model inspects the frontend's proof, infers an instance head,
invents a polytype, performs general higher-rank subsumption, or enables the
first-order unifiers to enter quantified bodies. A miss after the finite
engine-specific tails remains subject to each engine's existing incompleteness
and search-budget rules. See the original
[provider-local instantiation evidence report](reports/2026-08-05-provider-local-instantiation-evidence.md)
and the
[exact provider-instantiation assignment report](reports/2026-08-05-exact-provider-instantiation-assignments.md)
for the two trust boundaries and their regression coverage.

## Djinn example

This function uses the standard Djinn environment, parses the contextual Djinn
type grammar, runs one checked query, and renders every returned candidate:

```haskell
import Data.Bifunctor (first)
import Language.Haskell.Djex

djinnDefinitions :: String -> Either String [String]
djinnDefinitions source = do
  session <- first renderDiagnostic standardDjinnSession
  target <- first show $ mkIdentifier "result"
  request <- first renderDiagnostic $
    parseDjinnRequest
      session defaultQueryOptions target "<memory>" source
  result <- first renderDiagnostic $ runDjinnQuery session request
  traverse
    (first show . renderDjinnCandidateDefinition FullyQualified)
    (batchCandidates $ resultSearch result)
```

For example, `djinnDefinitions "(a, b) -> (b, a)"` returns a checked definition
for `result`. An empty candidate list must be interpreted together with
`resultEvidence` and `batchProgress`: Djinn may have proved the goal
uninhabited, found that only a target self-reference works, or stopped at a
budget.

The checked Djinn request edge accepts a leading `ForallType`/constraint
prefix as well as the textual contextual grammar. Explicit
`requestContexts` are inserted beneath those leading binders, whose variables
are then lowered capture-safely to Djinn's implicit monotype variables. The
selected session validates every combined constraint against its class table
and checks the goal and class arguments in one kind scope. Quantification below
that prefix is retained as a shared `TypeAtom`. Djinn's ordinary formula
projection treats it as one proposition and compares it by lexical
alpha-equivalence. The checked query worker also tries a polarized projection
that opens atoms in positive positions, including atoms with validated class
contexts, the historical exact opaque projection, two singleton occurrence
frontiers, two pairwise frontiers, two triple frontiers, two quadruple
frontiers, and two capped quintuple frontiers. Quintuple-opaque plans begin at
ten sites and quintuple-open plans at eleven. Selecting nested occurrences
also opens the union of their required enclosing chains. Prepared functions
cache the same views under distinct internal proof identities, allowing a
reusable source function to be used at different sound views in one term. The
historical fully-open, exact-opaque, and singleton prefix retains its order;
the deterministic pairwise through quintuple tail follows it. Quintuple
selection alternates stably from the two source-order edges and is capped at
512 views per orientation. The cap retains all 252 ten-site and all 462
eleven-site selections, making the family exhaustive through eleven
independent occurrences while limiting the new layer to 1,024 views on larger
inputs. A twelve-site proof requiring exactly six open and six opaque
occurrences remains outside the family. When a hypothesis-side
context-free chain of at most six binders exists, bounded instantiation axioms
can eliminate it completely at a candidate tuple drawn from
the sequent's variables, opened-forall skolems, premise scopes, and already
mentioned subtrees that are independent of enclosing binders and contain
quantification, including structural wrappers around quantified atoms.
Inferable axiom evidence lowers to the hypothesis expression itself. If a
selected binder is vacuous, proof conversion retains the shortest visible
prefix, including a specified closed quantified argument when the query
supplied one. One- through three-binder schemes retain their historical lexical
Cartesian order. Four-, five-, and six-binder schemes fairly interleave source-order
windows, repeated arguments,
sparse monotone selections, and the Cartesian tail without raising any search
cap.

A separate positive-only query-correlated family revisits only hypothesis-side
schemes embedded in the elaborated goal. It fairly schedules the same finite
variable and guarded-quantified vocabulary, using source occurrence order for
the variable candidates. A retained tuple must pair a quantified candidate with
a binder occurring free in the scheme body and specialize the complete body to
an alpha-equivalent subtree of the canonical elaborated query.

The builder starts with the exact active historical axioms for the structural
or nominal formula plan. A correlated axiom is excluded when its logical
formula is already present there or was retained earlier in this family, even
if its visible evidence differs. Before tuple enumeration, the builder seeds its
worklist with nested schemes exposed by those historical formulas; that initial
closure spends no eligible attempt. A later correlated candidate with an
already-seen formula is scanned the same way and spends one eligible attempt,
but neither case spends a per-scheme or family axiom allowance. The fair
producer prefilters at most 512 raw tuples for each scheme, while the builder
independently charges at most 512 eligible attempts across the family. At most
six binders, sixteen retained axioms per scheme, and 64 retained axioms per
structural or nominal family remain allowed.

The established query-closed positive-only family revisits only hypothesis-side
schemes embedded in the elaborated goal. Its candidate pool adds closed,
forall-free monotype subtrees already present in that goal to the historical
candidates. Fair tuple scheduling retains only tuples containing at least one
closed query candidate, so the historical prefix is not duplicated; mixed
tuples may still use variables, skolems, premise-scope spellings, or guarded
quantified subtrees. Each substituted body must kind-check, and a vacuous binder
may retain the shortest visible application required by the checked choice.

Prepared environments additionally retain each context-free loaded scheme
before leading binders are implicitized. An appended positive-only family can
instantiate those schemes at the historical variable and quantified
candidates plus closed, forall-free subtrees of the elaborated query and
synonym-expanded loaded value signatures. A candidate may be higher-kinded;
the complete substituted body must kind-check at `Type` in the exact prepared
environment before its axiom is compiled. Free signature variables are closed
implicitly, and each restored global occurrence can use an independent
instance. A non-target retained scheme makes bounded exhaustion inconclusive,
preventing the former false `ProvedUninhabitable` result.

Positive contexts do not become LJT premises: as at the prenex query
boundary, Djinn can
only construct a dictionary-independent body. Constrained hypothesis-side
schemes remain opaque. If the primary projection is incomplete, an empty search
carries no negative evidence.

The stable request worker retains datatype expansion as its primary formula
semantics. After query elaboration and synonym expansion, it computes a
backward slice from the goal to decide whether positive demand can reach a
datatype with one or more parameters. A second sealed compiler retains
parameterized saturated applications as complete alpha-aware nominal atoms and
supplies matching goal plans, cached loaded premises, and instantiation axioms.
Aliases therefore remain
transparent, while nullary datatypes retain their constructor formulas in both
views. Unrelated parameterized declarations neither activate the nominal
family nor perturb existing candidate prefixes.

Prepared value flows retain each whole result and project its positive function
results and tuple elements. A datatype declaration is only a field-projection
template: fields are exposed after a loaded value actually provides that owner,
and its declared parameters are specialized with the arguments of that exact
occurrence. Function parameters introduced along the query's positive result
path are available local values and seed the same projections without loaded
domains. Only explicit leading `forall` binders inside such a hypothesis become
specialization variables; free variables and enclosing query binders remain
rigid. Consequently a loaded `Holder`, a loaded tuple, and a local `Holder -> R`
premise can reveal a hidden consumer, while a declaration-only
`data Box a = Box a` cannot match an arbitrary rigid demand. Datatype heads are
visited at most once per projection path, bounding nested and future recursive
field traversal.

The complete historical structural no-axiom prefix runs before this focused
nominal family. Each nominal formula is paired with a plain proof plan and,
when present, an axiom-enabled proof plan. Historical structural instantiation
then completes before the appended loaded-scheme structural and, when needed,
nominal families. The established query-closed structural and optional nominal
plans retain their position after that prefix and contain no correlated axioms.
Pure query-correlated structural and optional nominal plans follow, carrying
historical, loaded, and provider premises but no query-closed axioms. Only when
both families contribute does a final combined structural and optional nominal
superset carry both sets, allowing mixed proofs to compose without moving or
duplicating either independent plan. Every plan consumes the same query-wide
candidate cutoff and choice-point budget. The nominal, loaded-scheme,
query-correlated, query-closed, and combined families are positive-only:
returned terms are checked against the exact formula, but an empty result never
contributes logical negative evidence. Thus ordinary
applications remain structural in the primary projection, while reachable
parameterized datatype applications have a complementary nominal transport
view for impredicative arguments.

For a declared `data D a = EmptyD | FullD a`, the checked request
`(forall a. D a) -> D (forall b. b -> b)` can return `\x -> x`. If the
environment also contains `finish :: D (forall b. b -> b) -> R`, the request
`(forall a. D a) -> R` can return `\x -> finish x`. Proof checking occurs
before inferable instantiation evidence is erased or a vacuous choice is
retained as a visible application. Every proof that consumes that evidence
takes the conservative no-eta conversion path, so cleanup cannot contract a
higher-rank application boundary exposed by erasure. GHC's
simplified subsumption rejects the bare `finish` at that expected type.
The same rule protects a selector application introduced by field-projection
normalization. Consumers compiling these signatures or candidates may need
`RankNTypes` and `ImpredicativeTypes`.

The checked request
`(forall a b. f a b) -> f (forall x. x -> x) (forall y. y -> y -> y)` can
return `\x -> x`. The two different guarded quantified arguments occur in the
request, and their complete specialization is exactly the requested result;
the rule does not construct either polytype or accept a merely related body.

Reachability also supports closed global composition. With
`token :: Token`, `poly :: Token -> (forall a. D a)`, and the `finish` signature
above in the prepared environment, a request for `R` can return
`finish (poly token)`. The backward slice matches loaded codomains against the
current demand, specializes matched variables, and adds the providers' domains
to the next demand frontier.

These examples describe candidates from the later nominal family. Set
`optionAlternatives = True` when an earlier structural constructor inhabitant
would otherwise end first-result search before that family is reached.

See the
[nominal parametric-data transport report](reports/2026-08-01-nominal-parametric-data-transport.md),
[loaded polymorphic values report](reports/2026-08-01-loaded-polymorphic-djinn-values.md),
[query-local closed-monotype report](reports/2026-08-09-query-local-closed-monotype-instantiation.md),
and
[query-correlated guarded-impredicative report](reports/2026-08-09-query-correlated-guarded-impredicative-instantiation.md)
for the proof-policy and regression boundaries.

Both stable adapters use the same `Language.Haskell.Synthesis.TypeAtom`
representation. A sealed atom retains normalized source syntax, a cached
alpha-normal key, and capture-avoiding free-variable substitution. Bound
variables are keyed by lexical scope and binder position; free identities are
not renamed away. The representation itself remains an inert transport/equality
feature. Each backend must opt into explicit typing rules: positive
introduction (with validated contexts ignored for proof power), bounded
context-free hypothesis instantiation, query-correlated guarded-impredicative
instantiation, query-closed monotype instantiation, and bounded
loaded-value instantiation in Djinn; or fresh
per-use provider instantiation, contextual quantified-goal introduction, and shallow subsumption
between context-free quantified schemes with no free flexible variables in
Exference. Neither backend implements general rank-N subsumption.

When ordinary Exference search exposes a nested `forall` as an active goal, it
can open the complete leading chain with branch-local fresh rigid constants and
synthesize the body. A layer's substituted context becomes lexical givens for
that descendant body, including binderless contextual layers. Each generated
obligation snapshots its own givens and is resolved independently under those
givens plus the root class environment; substitutions update both halves, and
instance prerequisites retain the same lexical scope. This permits local
methods and superclass entailment without allowing evidence to discharge an
unrelated sibling obligation. Every flexible variable alive before a layer
opens is barred from later containing that layer's rigids; the escape check
follows flexible-to-flexible substitution edges to a fixed point, so an
indirect assignment cannot smuggle out a skolem. The independent expression
checker repeats the same allocation, evidence scoping, and escape discipline at
expected-type boundaries. Exact forwarding and checked context-free shallow
subsumption keep their existing priority; non-exact contextual subsumption
remains outside the rule, and identifier or search-budget exhaustion loses
completeness rather than soundness.
Search-generated annotation skolems carry scope-owned provenance and are
compared up to an injective alpha-renaming because search and checking may
visit independent goals in different orders; environment, query, and
standalone caller-supplied rigids remain nominal. Any unresolved constraint
which still mentions a nested skolem is rejected at publication, while a
root-prenex residual remains a valid query obligation.

At an Exference scoped-value occurrence, exact alpha-aware quantified
forwarding is tried first, including exact constrained schemes. A non-exact
quantified request may then use the shallow rule only when both prenex schemes
have no direct context or free flexible variable. The matcher treats requested
binders as rigid and solves only provider binders; a solution may be a
monotype or, impredicatively, a quantified subtree that already occurs in the
requested scheme itself — no polytype is invented. Ambient rigid constants
remain nominal. Search
records the requested occurrence annotation without importing matcher state,
and independent expression checking classifies the occurrence again. A
non-quantified request instead freshly instantiates the complete leading
provider chain and turns its direct contexts into proof obligations.

Exference also tries one bounded visible construction for an instantiable
scoped or retained global provider. A direct provider constraint can match an
explicit instance head whose arguments are closed ground monotypes, with one
head determining the complete leading binder prefix. Independently, a
context-free provider with no free flexible variables whose complete leading
prefix is vacuous can select proper types already supplied by the checked
query. Its residual body may retain ambient rigids opened by that query. The
candidate pool contains ground monotypes and complete closed context-free
foralls observed below arrows or tuples. Search can therefore emit `provider @Int` or
`provider @(forall a0_0. a0_0 -> a0_0)`. The query route retains at most six
binders and 32 combinations; the instance-head route stays monotype-only. The
ordinary implicit per-use branch remains available and retains priority.

The shared generated-term API represents this with
`VisibleTypeApplication` and an abstract `VisibleTypeArgument`.
`inferredVisibleTypeArgument` renders as `@_`;
`specifiedVisibleTypeArgument` accepts a structurally valid, lexically closed
type and alpha-normalizes quantified binders. The complete value is available
through `visibleTypeArgumentClosedType`; `visibleTypeArgumentType` remains the
monotype-only compatibility projection. The independent Exference checker
first reconstructs each complete specified payload in a checker-local binder
namespace, validates its full type, and requires every nested constraint class
to exist in the sealed class environment. It then consumes one flexible
leading binder for each node and accepts either bounded form. Open arguments
such as `@a` and arbitrary caller-directed instantiation remain outside the API
invariant. Djinn's bounded axiom routes can also retain the node for vacuous
local or loaded binders; its historical `HExpr` projection rejects the shared
node instead of erasing it.

Candidate source containing such a node must be compiled with
`TypeApplications`. Its enclosing signature will commonly also need
`RankNTypes`; a quantified type argument generally needs `ImpredicativeTypes`,
and an ambiguous contextual provider such as
`forall a. C a => Token` may additionally need `AllowAmbiguousTypes`.

Validated contexts do not contribute methods to proof search. This is the
sound subset supported by Djinn's monomorphic propositional calculus: a query
such as `Eq a => a -> a` can synthesize an implementation that ignores its
dictionary, while a goal whose only implementation needs `(==)` remains
uninhabitable. Exference should be selected when synthesis must use class
evidence.

Use `mkDjinnSession` for a caller-built
`Environment DjinnTypeVariable Void ()`. Checked sessions are immutable. To
change declarations, retain or recover the neutral environment, build the
complete replacement with `mkEnvironment`, and seal a new session with
`mkDjinnSession`. This is the same lifecycle as Exference and keeps historical
Djinn `HType`, `HKind`, `Declaration`, and `Context` values out of the curated
API. The separate historical `djinn` executable retains its transactional
raw-declaration editor solely inside the compatibility frontend. The shared
`djex` REPL intentionally exposes read-only `:browse` and `:info` views,
non-evaluating term inspection through `:type`, and structural kind inspection
through `:kind`/`:kind!`, instead of that legacy mutation language.

`:type` is a private REPL frontend over the authoritative neutral inventory
retained by the Exference runtime. It derives callable signatures for ordinary
values, data constructors, and class methods, resolves them in the current
loaded module scope, and uses the prepared class environment to check residual
constraints. It does not consume the policy-filtered Exference search
dictionary and is not routed through the selected synthesis backend. Only the
shared qualification policy affects its presentation. The command performs
structural inference for the expression subset documented in the
[REPL guide](repl.md#inspecting-expression-types); it neither evaluates terms
nor infers types for loaded function bodies.

`:kind` is a sibling private REPL frontend over the same authoritative
inventory and current module scope. The parser admits type constructors,
synonyms, and class heads without forcing the result to `Type` first. Ordinary
types then use `inferTypeKind`, whose `InferredKind` residual `Natural`
variables are canonical presentation identities; the private REPL layer maps
the inventory's class-parameter kinds to a final `Constraint`. `:kind!`
normalizes synonyms through the session's exact `PreparedInventory` witness
and checks the kind on both sides. Library code needing that narrow
normalization rule can call
`normalizePreparedTypeSynonyms`: saturated aliases expand normally, while only
an undersaturated alias at the complete operational head beneath leading
context-free forall binders is retained. This operation does not perform kind
checking itself, so callers must validate the source before normalization, as
the REPL does. The complete interactive behavior and limits are in the
[REPL guide](repl.md#inspecting-type-kinds).

## Exference example without a parser

The parser-neutral adapter accepts the same shared `Environment` and `Type`
vocabulary. This example uses an empty environment and asks for the identity
function:

```haskell
import Data.Bifunctor (first)
import Language.Haskell.Djex

exferenceDefinitions :: Either String [String]
exferenceDefinitions = do
  environment <- first show $ mkEnvironment []
  session <- first renderDiagnostic $ mkExferenceSession environment
  name <- first show $ mkIdentifier "result"
  target <- first show $ mkDefinitionName name
  let variable = FlexibleVariable 0
      goal = FunctionType (TypeVariable variable) (TypeVariable variable)
      query = QueryRequest
        { requestTarget = target
        , requestGoal = goal
        , requestContexts = []
        , requestOptions = defaultExferenceOptions
        }
  request <- first renderDiagnostic $ mkExferenceRequest query
  results <- first renderDiagnostic $ runExferenceQuery session request
  let selected = selectQueryResults
        SelectFirst (const ()) (const True) results
  traverse
    (first show . renderExferenceCandidateDefinition FullyQualified)
    (selectionCandidates selected)
```

`runExferenceQuery` returns a lazy sequence of result batches. Selection is a
separate presentation policy, so a caller can take the first candidate, retain
all globally best candidates, use bounded lookahead, or stream every admissible
candidate without changing search semantics.

Exference first discharges an exact local given, including a variable-bearing
given such as `C a`, before deciding whether an obligation must be deferred. It
then follows superclass closure and explicit instance prerequisites. A session
rejects duplicate instance heads and any pair of explicit heads that can match
the same ground constraint; Exference has no overlap-selection policy, so
silently choosing one would make evidence depend on declaration order.

Class-environment construction also rejects any groundable instance
prerequisite that can grow its head by type-node count or by occurrences of a
head variable, including direct superclass prerequisites completed onto
explicit rules. For example,
`C [a] => C a` is rejected because resolving `C Int` would grow forever.
Shrinking rules and size-preserving cycles are accepted. Revisiting a ground
goal refutes that evidence branch during candidate search; final residual
checking retains the unresolved obligation rather than claiming evidence. A
prerequisite with a variable absent from the head likewise remains unresolved.
This makes nominal resolution terminate for accepted finite ground
constraints. Exference's surrounding ranked expression search still is not an
inhabitation decision procedure, so keep its step, queue, and depth controls
appropriate for the application.

## Loading an Exference source environment

Import the explicit source boundary in addition to the neutral adapter:

```haskell
import Language.Haskell.Djex.Exference
import Language.Haskell.Djex.Exference.HaskellSrc
```

Applications that compare engines should parse query text through the neutral
source module instead of invoking backend parsers twice:

```haskell
import Language.Haskell.Djex.HaskellSrc
```

`parseSourceType` accepts any checked shared `Inventory`; its scoped variant
also accepts the GHCi-style `ExferenceQueryScope` retained for compatibility.
Both return `ParsedSourceType`, whose semantic field is a shared
`Type (Variable Int)` and whose variable spellings and source location are
detached metadata. `mkExferenceRequestWithCheckedTargetFromParsed` seals the
Exference projection. Djinn clients can map variable identities and nominal
names structurally, then call `mkDjinnRequest`, exactly as the unified REPL
does.

`loadExferenceSession directory` parses an explicit directory's Haskell modules
and ratings, validates the complete shared inventory, and returns an
`ExferenceSessionLoadReport`. `loadDefaultExferenceSession` does the same for
Djex's installed environment; `defaultExferenceEnvironmentPath` exposes that
resolved path when an application needs to display or inspect it. The
policy-aware default loader is `loadDefaultExferenceSessionWithPolicy`.

Directory paths also discover `*.visibility` files. A manifest line
is `abstract|empty Module.Type ARITY PARAMETER_KIND...`; kinds use `Type` and
fully parenthesized arrows such as `(Type->Type)`. If a manifest is present it
is a complete, exact classification of the directory's constructorless
datatypes. Abstract entries retain their explicit checked kinds but have no
Exference eliminator. Empty entries remain concrete empty datatypes. Malformed,
duplicate, missing, unknown, inhabited, kind-invalid, or arity-mismatched
entries are fatal `EXF_TYPE_VISIBILITY` diagnostics. The explicit-file and
ordinary in-memory functions below do not discover or apply this sidecar and
therefore retain ordinary Haskell empty-datatype semantics. The unified REPL
captures manifests belonging to directory targets in the same immutable
snapshot as modules and ratings, then uses the opt-in snapshot boundary below.

Module-aware callers that already own discovery and dependency ordering can
use the explicit-file boundary:

```haskell
loadExferenceSessionFromFiles
  :: [FilePath] -- Haskell modules, in caller order
  -> [FilePath] -- rating files, in caller order
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromFilesWithPolicy
  :: ExferenceSessionPolicy
  -> [FilePath]
  -> [FilePath]
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromSources
  :: [(FilePath, String)] -- already-read modules, in caller order
  -> [(FilePath, String)] -- already-read ratings, in caller order
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromSourcesWithPolicy
  :: ExferenceSessionPolicy
  -> [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromSourcesWithTypeVisibility
  :: [(FilePath, String)] -- already-read modules
  -> [(FilePath, String)] -- already-read ratings
  -> [(FilePath, String)] -- already-read visibility manifests
  -> IO ExferenceSessionLoadReport

loadExferenceSessionFromSourcesWithTypeVisibilityWithPolicy
  :: ExferenceSessionPolicy
  -> [(FilePath, String)]
  -> [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO ExferenceSessionLoadReport
```

These functions run the same parse, inventory, rating, and sealing
pipeline as directory loading. The `FromSources` variants never reopen their
paths; paths are retained as parser filenames and diagnostic provenance. They
are appropriate when a caller already owns an immutable filesystem snapshot.
The explicitly named `WithTypeVisibility` variants apply their third snapshot
as one complete manifest; the older variants pass no manifest deliberately.
None of these functions discover dependencies: the caller owns the complete
ordered source closure. They do use the `import` declarations inside that
closure to elaborate types, classes, instances, constructors, and value or
method signatures. An empty module list is valid and retains Exference's
built-in syntax-level constructor inventory.

Callers that need the checked source projection before session policy and
sealing can use the corresponding lower-level boundary from
`Language.Haskell.Exference.EnvironmentParser`:

```haskell
parseModuleSources
  :: [(FilePath, String)]
  -> IO (LoadReport SourceEnvironment)

environmentFromSources
  :: [(FilePath, String)]
  -> [(FilePath, String)]
  -> IO (LoadReport CheckedSourceEnvironment)

environmentFromSourcesWithTypeVisibility
  :: [(FilePath, String)] -- modules
  -> [(FilePath, String)] -- ratings
  -> [(FilePath, String)] -- visibility manifests
  -> IO (LoadReport CheckedSourceEnvironment)
```

These functions have the same ordered snapshot and diagnostic-path contract;
the explicitly named visibility variant has the same opt-in manifest semantics
as the stable session facade.

Always inspect both report fields:

- `exferenceSessionLoadResult` contains either fatal diagnostics or the sealed
  session;
- `exferenceSessionLoadDiagnostics` contains warnings and informational
  diagnostics produced while loading and sealing.

Fatal source-loader phases use stable `EXF_*` codes. Read, parse, unsupported-
vocabulary, and source-aware extraction failures retain the source spans
available at those phases. A later neutral-inventory, sealing, or policy
failure uses its `DJEX_EXF_*` code and may be source-free because no single
token owns that whole-environment invariant.

The supplied closure must contain each logical module exactly once. Duplicate
explicit headers and multiple headerless `Main` modules fail before scope
construction with `DuplicateModuleDeclarations`; its
`EXF_MODULE_DUPLICATE` diagnostics point at each later source in caller order
and name the first declaration in context.

It must also be acyclic after `{-# SOURCE #-}` edges are removed. Ordinary
cycles fail as `CyclicModuleImports` with an `EXF_MODULE_CYCLE` diagnostic at
the import that closes the first stable cycle. This preflight prevents cyclic
re-export surfaces from oscillating or depending on an iteration cutoff;
adding an unrelated module cannot change the result.

The loader rejects unsupported source meaning before building a partial
inventory. The authoritative emitted `UnsupportedVocabularyForm` set includes
pattern-synonym signatures and XML page/hybrid modules as well as the
documented type-family, GADT, deriving, class, and instance limitations.
Ordinary term patterns and pattern-value bodies remain accepted; do not
interpret the pattern-synonym-signature restriction as a ban on ordinary
pattern matching. `ExplicitExportList` is retained as a source-compatible
constructor but is no longer emitted.

Type-operator fixity declarations are not retained in the neutral inventory.
Accordingly, a nested `TyInfix` tree without explicit grouping fails as
`UnparenthesizedTypeOperatorChain` at the complete chain span, even when the
operators have compatible default fixities or local declarations would select
one association. A single infix type application remains valid, as do
both `(A :<: B) :>: C` and `A :<: (B :>: C)`: the parentheses make the stored
tree independent of discarded fixity metadata.

All public, default, directory, file, and in-memory loaders apply the same
module-aware policy. A declaration sees its module's local nominal names and
only directly imported loaded names. `qualified`, `as`, positive import lists,
and `hiding` restrict both bare and qualified lookup. Named exports and
`module M` re-exports form the surface seen by downstream modules, while an
entity re-exported through another module retains its defining canonical name.
The `module M` surface is the identity intersection of unqualified scope and
scope through the written qualifier `M`; this includes self locals and
unqualified `as` imports, but not qualified-only imports. A loaded `Prelude`
contributes its implicit unqualified surface unless the module is `Prelude`,
imports it explicitly, or enables `NoImplicitPrelude` or
`RebindableSyntax`.

That policy is exact for modules present in the supplied closure: a
loaded-but-unimported declaration is out of scope even through its canonical
qualifier. Djex does not read package interfaces, so genuinely unknown names
remain external under Exference's open-inventory policy. A positive import list
provides enough information to assign finite canonical external names; an
unrestricted or `hiding` import of an unloaded module has no enumerable export
complement and cannot be verified as if its interface had been loaded. The
resolver nevertheless enforces every explicit `hiding` occurrence and keeps
each open or exact import route separate when qualifiers coincide.

This external-name policy applies only to ordinary, non-package imports.
Package-qualified imports fail during source-vocabulary validation, including
unused imports: the neutral nominal identity cannot retain a package key, and
must not make two same-named modules from different packages appear equal.

Loading deliberately retains exported and private declarations in the checked
inventory for kind checking, synonym expansion, recursion analysis, and class
validation. Export lists govern later imports rather than deleting facts; a
module-aware frontend may expose the full top level through `*MODULE`. The
result is still an inventory, not a prompt context.

With `loadExferenceSessionWithPolicy`, unknown exclusions are harmless no-ops.
This lets a reusable policy exclude an optional binding absent from a particular
environment. Rating overrides are stricter: every rating must be finite and
every overridden name must still be available to search after exclusions and
capability filtering, otherwise session sealing returns a fatal policy
diagnostic.

With a session, use
`parseExferenceRequest session options target sourceName sourceText` so type
names, class arities, synonyms, source spellings, and later diagnostic spans all
come from the same checked inventory.

Source imports have already governed declaration elaboration by this point.
An interactive source frontend separately constructs an
`ExferenceQueryScope` and calls `parseExferenceRequestInScope` (or the
checked-target counterpart). Its fields have intentionally narrow meanings:

- `exferenceQueryVisibleNames` is the exact set admitted for unqualified
  lookup;
- `exferenceQueryCurrentModule` gives one full-top-level module's local names
  precedence;
- `exferenceQueryModuleAliases` maps prompt qualifiers introduced by imports
  to canonical loaded modules; and
- `exferenceQueryQualifiedNames` optionally restricts each written qualifier
  to an exact canonical-name set, allowing aliases to enforce explicit import
  lists and `hiding`.

Canonical qualification still resolves names from the complete sealed
inventory when no written-qualifier restriction applies. An empty outer
`exferenceQueryQualifiedNames` list preserves the original permissive scoped
API behavior; a present qualifier paired with an empty name list admits
nothing through that spelling. Scope is therefore a visibility projection
over one checked inventory, not a second independently loaded environment. The
shared REPL also narrows Exference's searchable binding projection to the same
visible names while keeping the complete inventory for qualified type
elaboration. Changing this prompt scope does not re-elaborate declarations or
discover another source dependency.

The `djex` and `exference` executables use the same default-path operation.
Cabal's generated `Paths_djex` module remains private; applications do not need
to import a package-specific generated module. From a source checkout, the
bundled directory is `exference/environment`.

## Reading results correctly

`QueryEvidence` and `Progress` answer different questions:

- `ValidatedCandidates` means the same result batch contains at least one
  independently checked candidate.
- `ProvedUninhabitable` is a logical Djinn conclusion, not an empty-list alias.
- `RequiresTargetReference` means a safe nonrecursive definition was excluded.
- `NoEvidence` makes no logical claim.
- `Completed Finished` means the configured exploration ended normally.
- `Completed (Truncated reasons)` records resource limits such as step,
  candidate, choice-point, queue, depth, or identifier-space limits.
- `Continuing` means more batches may follow.

A truncated batch can still contain useful validated candidates. Conversely, a
finished Exference batch with no candidates is not a proof of non-inhabitation.
Frontends should preserve both dimensions.

## Rendering and residual constraints

The backend render helpers accept `Unqualified`, `QualifyIdentifiers`, or
`FullyQualified` and return the common `RenderError` type. Exference candidates
may retain residual class constraints; inspect
`candidateResidualConstraints` or call
`renderExferenceResidualConstraintsWithQualification` with the same policy used
for the candidate rather than presenting the generated term as obligation-free.
`renderExferenceResidualConstraints` retains the historical fully qualified
policy. Both helpers return
`Either ExferenceResidualRenderError [String]`: they validate each nominal
class and every complete shared type argument before sorting or deduplicating,
then use the candidate's validated type-variable hints. The
qualification-aware helper applies its policy to the class and to every
constructor nested in the constraint arguments.

The checked `Either` result intentionally replaces the earlier pure `[String]`
signature. `Candidate` has a public compatibility constructor, so a pure
renderer could otherwise present caller-forged malformed residuals as Haskell
source. Failures carry zero-based constraint and argument positions and the
shared structural error; validation reports the first class or argument error
in the candidate's original order. Existing callers can migrate by handling
the result alongside `renderExferenceCandidateExpression` or
`renderExferenceCandidateDefinition`.

For custom presentation, use `candidateOutput` to obtain the shared
`FunctionClause` and the operations in
`Language.Haskell.Synthesis.Generated`. Scope validation and collision-safe
local naming remain part of that shared rendering boundary. Visible type
applications preserve the selected qualification policy and parenthesize
compound closed type arguments, so a type application such as `Maybe Int`
renders as `@(Maybe Int)` rather than changing the Haskell parse. In particular,
`Candidate` keeps a public constructor for compatibility, so the stable
expression and definition helpers validate the complete clause on every call;
caller-forged free local identities and duplicate pattern-binder identities
produce `RenderError` instead of unchecked Haskell text. Djinn's helpers then
reject any nonempty residual list with `UnexpectedResidualConstraints`, since
every genuine Djinn result is closed. Generated-clause failures deliberately
take precedence if a caller forges both the output and residual fields.

For structural consumers, `expressionFullApplicationSpine` returns one
source-ordered list whose `TermArgument` and `VisibleTypeArgumentArgument`
constructors preserve mixed applications. The older
`expressionApplicationSpine` remains term-only. `rewriteExpressionBottomUp`
and its monadic `rewriteExpressionBottomUpM` variant visit expression children
left-to-right before their parent; patterns and checked visible type arguments
are retained, and callback-produced nodes are not traversed again.

## Import guidance

- Start with `Language.Haskell.Djex` for a compact application or an API tour.
- Prefer explicit `Language.Haskell.Djex.Djinn` and
  `Language.Haskell.Djex.Exference` imports in larger modules to make backend
  ownership obvious.
- Import `Language.Haskell.Synthesis.*` modules directly when defining reusable
  neutral infrastructure.
- Use historical `Djinn*` or `Language.Haskell.Exference*` modules only when
  maintaining a compatibility integration.

`Language.Haskell.Djex.Djinn`, not `Djinn.Core`, is the curated checked Djinn
facade. Import `Djinn.Core` only when a compatibility integration deliberately
needs its historical representation and operations.

See [the architecture guide](architecture.md) for the stability tiers,
[the shared REPL guide](repl.md) for interactive use, and
[the synthesis API map](../synthesis/README.md) for the neutral modules.
