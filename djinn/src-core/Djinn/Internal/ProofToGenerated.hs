-- | Lower checked LJT proofs directly into Djex's shared generated syntax.
--
-- Djinn historically translated proofs through a second Haskell-shaped tree
-- (@HExpr@ and @HPat@).  That tree remains available as a compatibility view,
-- but the live backend now lowers directly into the shared tree and delegates
-- generic lexical cleanup to that tree's owning module.
module Djinn.Internal.ProofToGenerated
  ( termToGeneratedExpression
  , termToGeneratedClause
  , termToGeneratedClauseWithoutEta
  , termToGeneratedClauseWithVisibleApplications
  ) where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Trans.State.Strict (evalState, state)
import Data.List ((\\))
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Djinn.Internal.LJTFormula
import Djinn.Internal.HIdentifier
  ( generatedGlobalName
  , renderProofSymbolName
  )
import Language.Haskell.Synthesis.Fresh (allocateFresh)
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as Name

type HSymbol = String
type Expression = Generated.Expression HSymbol

-- | Convert and simplify one proof expression without constructing Djinn's
-- historical recursive output tree.
termToGeneratedExpression :: Term -> Either String Expression
termToGeneratedExpression = termToGeneratedExpressionWithEta True

termToGeneratedExpressionWithEta :: Bool -> Term -> Either String Expression
termToGeneratedExpressionWithEta contractEta =
  termToGeneratedExpressionWithEvidence contractEta Map.empty

termToGeneratedExpressionWithEvidence
  :: Bool
  -> Map.Map Symbol [Generated.VisibleTypeArgument]
  -> Term
  -> Either String Expression
termToGeneratedExpressionWithEvidence contractEta visibleEvidence term = do
  validateTermMetadata term
  (expression, _) <- convert [] renamedTerm
  let simplifyBindings
        | contractEta = Generated.simplifyExpressionBy id
        | otherwise = Generated.simplifyExpressionWithoutEtaBy id
      simplified = niceNames $ simplifyBindings
        $ Generated.simplifyExpressionCases
        $ Generated.discardUnusedPatternBindingsBy id
        $ Generated.normalizeExpressionPatterns
        $ Generated.discardUnusedPatternBindingsBy id expression
      allowed = Set.fromList $ map unSymbol $ freeVars term
      escaped = freeValueSpellings simplified `Set.difference` allowed
  if Set.null escaped
    then do
      either (Left . show) Right $
        Generated.validateExpressionScope simplified
      either (Left . show) Right $
        Generated.validateExpressionSyntax simplified
      pure simplified
    else Left $ "proof conversion introduced unbound variable(s): "
      ++ unwords (Set.toAscList escaped)
 where
  renamedTerm = alphaRenameTerm term
  reservedNames = termNames renamedTerm

  convert _ (Var symbol)
      | Just typeArguments <- Map.lookup symbol visibleEvidence =
          pure
            ( Generated.lambdaExpression
                [Generated.Bind evidenceBinder]
                $ applyVisibleTypeArguments typeArguments
                $ Generated.Local evidenceBinder
            , []
            )
  convert enclosing (Var symbol) = do
    let spelling = unSymbol symbol
    expression <- if spelling `elem` enclosing
      then Right $ Generated.Local spelling
      else Generated.Global <$>
        generatedGlobalName Name.VariableLike "value" spelling
    pure (expression, [])
  convert enclosing lambdaTerm@Lam{} = do
    let (symbols, body) = termLambdaSpine lambdaTerm
        spellings = map unSymbol symbols
    (convertedBody, refinements) <-
      convert (reverse spellings ++ enclosing) body
    -- The old inside-out conversion diagnosed the innermost malformed
    -- refinement first. Preserve that order while constructing the complete
    -- generated binder group only once.
    patterns <- reverse <$> mapM
      (`convertVariable` refinements)
      (reverse spellings)
    pure (Generated.lambdaExpression patterns convertedBody, refinements)
  convert enclosing (Apply (Cinj (ConsDesc constructor arity) _) argument) = do
    (convertedArgument, refinements) <- convert enclosing argument
    (wrap, arguments) <- unpackTuple arity convertedArgument
    constructorName <- generatedGlobalName
      Name.ConstructorLike "constructor" constructor
    pure
      (wrap $ foldl' Generated.Apply
        (Generated.Global constructorName) arguments, refinements)
  convert enclosing (Apply function argument) =
    convertApplication enclosing function [argument]
  convert _ (Ctuple 0) = do
    unit <- generatedGlobalName Name.ConstructorLike "constructor" "()"
    pure (Generated.Global unit, [])
  convert _ unsupported =
    Left $ "unsupported proof term: " ++ show unsupported

  termLambdaSpine = collect []
   where
    collect symbols (Lam symbol body) = collect (symbol : symbols) body
    collect symbols body = (reverse symbols, body)

  unpackTuple 0 _ = Right (id, [])
  unpackTuple 1 argument = Right (id, [argument])
  unpackTuple arity (Generated.Tuple arguments)
    | length arguments == arity = Right (id, arguments)
  unpackTuple arity expression = Left $
    "constructor payload has shape " ++ show expression
      ++ ", expected a tuple of arity " ++ show arity

  unpackTuplePattern 0 _ = Right []
  unpackTuplePattern 1 pattern' = Right [pattern']
  unpackTuplePattern arity (Generated.TuplePattern patterns)
    | length patterns == arity = Right patterns
  unpackTuplePattern arity pattern' = Left $
    "constructor pattern has shape " ++ show pattern'
      ++ ", expected a tuple of arity " ++ show arity

  convertApplication enclosing (Apply function argument) arguments =
    convertApplication enclosing function (argument : arguments)
  convertApplication enclosing (Var symbol) (argument : arguments)
      | Just typeArguments <- Map.lookup symbol visibleEvidence = do
          (convertedArgument, argumentRefinements) <-
            convert enclosing argument
          convertedRest <- mapM (convert enclosing) arguments
          let (restExpressions, restRefinements) = unzip convertedRest
          pure
            ( foldl' Generated.Apply
                (applyVisibleTypeArguments typeArguments convertedArgument)
                restExpressions
            , argumentRefinements ++ concat restRefinements
            )
  convertApplication enclosing (Ctuple arity) arguments
    | length arguments == arity = do
        converted <- mapM (convert enclosing) arguments
        let (expressions, refinements) = unzip converted
        pure (tuple expressions, concat refinements)
  convertApplication _ (Ctuple arity) arguments = Left $
    "tuple constructor expects " ++ show arity ++ " arguments, got "
      ++ show (length arguments)
  convertApplication enclosing (Ccases constructors)
      (scrutinee : arguments)
    | length arguments >= alternativeCount = do
        let (handlers, rest) = splitAt alternativeCount arguments
        convertedAlternatives <- zipWithM
          (convertAlternative enclosing) handlers constructors
        (convertedScrutinee, scrutineeRefinements) <-
          convert enclosing scrutinee
        convertedRest <- mapM (convert enclosing) rest
        let (alternatives, alternativeRefinements) =
              unzip convertedAlternatives
            (restExpressions, restRefinements) = unzip convertedRest
        pure
          ( foldl' Generated.Apply
              (Generated.simplifyCaseExpression
                convertedScrutinee alternatives)
              restExpressions
          , scrutineeRefinements ++ concat alternativeRefinements
              ++ concat restRefinements
          )
   where
    alternativeCount = length constructors
  convertApplication _ (Ccases constructors) arguments = Left $
    "case eliminator expects a scrutinee and "
      ++ show (length constructors) ++ " alternatives, got "
      ++ show (length arguments) ++ " arguments"
  convertApplication enclosing (Csplit arity)
      (handler : scrutinee : arguments) = do
        (convertedHandler, handlerRefinements) <- convert enclosing handler
        (convertedScrutinee, scrutineeRefinements) <-
          convert enclosing scrutinee
        convertedArguments <- mapM (convert enclosing) arguments
        (patterns, body) <- unpackLambda arity convertedHandler
        let (argumentExpressions, argumentRefinements) =
              unzip convertedArguments
            tuplePattern [pattern'] = pattern'
            tuplePattern elements = Generated.TuplePattern elements
        case convertedScrutinee of
          Generated.Local variable
            | variable `elem` enclosing && null arguments ->
                pure
                  ( body
                  , (variable, tuplePattern patterns)
                      : handlerRefinements ++ scrutineeRefinements
                  )
          _ -> pure
            ( foldl' Generated.Apply
                (Generated.simplifyCaseExpression convertedScrutinee
                  [(tuplePattern patterns, body)])
                argumentExpressions
            , handlerRefinements ++ scrutineeRefinements
                ++ concat argumentRefinements
            )
  convertApplication _ (Csplit arity) arguments = Left $
    "tuple eliminator of arity " ++ show arity
      ++ " expects a handler and tuple, got "
      ++ show (length arguments) ++ " arguments"
  convertApplication enclosing function arguments = do
    converted <- mapM (convert enclosing) (function : arguments)
    let (expressions, refinements) = unzip converted
    pure (foldl1 Generated.Apply expressions, concat refinements)

  convertVariable spelling refinements =
    case [pattern' | (owner, pattern') <- refinements, owner == spelling] of
      [] -> Right $ Generated.Bind spelling
      [pattern'] -> Right $ Generated.As spelling pattern'
      pattern' : patterns -> do
        merged <- foldM mergePatterns pattern' patterns
        pure $ Generated.As spelling merged

  mergePatterns left right
    | left == right = Right left
  mergePatterns (Generated.Bind variable) pattern' =
    Right $ Generated.As variable pattern'
  mergePatterns pattern' (Generated.Bind variable) =
    Right $ Generated.As variable pattern'
  mergePatterns (Generated.As variable left)
      (Generated.As variable' right) = do
        merged <- mergePatterns left right
        pure $ if variable == variable'
          then Generated.As variable merged
          else Generated.As variable $ Generated.As variable' merged
  mergePatterns (Generated.As variable pattern') other =
    Generated.As variable <$> mergePatterns pattern' other
  mergePatterns other (Generated.As variable pattern') =
    Generated.As variable <$> mergePatterns other pattern'
  mergePatterns (Generated.TuplePattern left)
      (Generated.TuplePattern right)
    | length left == length right =
        Generated.TuplePattern <$> zipWithM mergePatterns left right
  mergePatterns left right = Left $
    "cannot merge incompatible patterns " ++ show left ++ " and "
      ++ show right

  convertAlternative enclosing (Lam variable body)
      (ConsDesc constructor arity) = do
        let spelling = unSymbol variable
        (convertedBody, refinements) <-
          convert (spelling : enclosing) body
        payloadPattern <- case
            [ pattern'
            | (owner, pattern') <- refinements
            , owner == spelling
            ] of
          [] -> Right Nothing
          pattern' : patterns ->
            Just <$> foldM mergePatterns pattern' patterns
        patterns <- case payloadPattern of
          Nothing -> Right $ replicate arity Generated.Wildcard
          Just pattern' -> unpackTuplePattern arity pattern'
        constructorName <- generatedGlobalName
          Name.ConstructorLike "pattern constructor" constructor
        let payloadIsUsed = spelling `Set.member`
              Generated.expressionFreeLocalIdentitiesBy id convertedBody
            branchRefinements = filter ((/= spelling) . fst) refinements
        if payloadIsUsed
          then do
            let (patterns', fields) = payloadValues spelling patterns
                reconstructed = tuple fields
            body' <- maybe
              (Left $ "cannot reconstruct case payload " ++ show spelling
                ++ " without capturing a nested binder")
              Right
              $ Generated.substituteExpressionLocalBy
                  id spelling reconstructed convertedBody
            pure
              ( (Generated.Constructor constructorName patterns', body')
              , branchRefinements
              )
          else pure
            ( (Generated.Constructor constructorName patterns, convertedBody)
            , branchRefinements
            )
  convertAlternative _ handler _ =
    Left $ "case alternative is not a lambda: " ++ show handler

  payloadValues owner = go reservedNames (1 :: Natural)
   where
    go _ _ [] = ([], [])
    go used next (pattern' : patterns) =
      let (pattern'', field, used', next') =
            patternValue used next pattern'
          (patterns', fields) = go used' next' patterns
      in (pattern'' : patterns', field : fields)

    patternValue used next pattern' = case pattern' of
      Generated.Bind variable ->
        (pattern', Generated.Local variable, used, next)
      Generated.As variable _ ->
        (pattern', Generated.Local variable, used, next)
      _ ->
        let (fresh, used', next') = freshField used next
        in ( Generated.As fresh pattern'
           , Generated.Local fresh
           , used'
           , next'
           )

    freshField used next = allocateFresh
      (\suffix -> (owner ++ "_field" ++ show suffix, suffix + 1))
      used next

  unpackLambda 0 expression = Right ([], expression)
  unpackLambda arity expression =
    if length used == arity
      then Right (used, Generated.lambdaExpression remaining body)
      else Left $
        "tuple handler has shape " ++ show expression ++ ", expected "
          ++ show arity ++ " lambda argument(s)"
   where
    (patterns, body) = Generated.expressionLambdaSpine expression
    (used, remaining) = splitAt arity patterns

  -- GHC can visibly instantiate a variable-headed application, but not an
  -- arbitrary expression whose inferred type happens to be polymorphic. In
  -- particular, @(case ... of ...) @T@ loses the branch signatures before
  -- visible application. Push the complete application into case and let
  -- bodies until it reaches an inferable head. A locally constructed value
  -- at any other head remains implicitly instantiated; it carries no erased
  -- loaded-provider constraint of its own.
  applyVisibleTypeArguments arguments expression =
    pushVisibleArguments expression arguments

  pushVisibleArguments expression arguments =
    pushAtHead applicationHead existing arguments
   where
    (applicationHead, existing) =
      Generated.expressionFullApplicationSpine expression

  pushAtHead applicationHead existing arguments = case applicationHead of
    Generated.Case scrutinee alternatives ->
      Generated.Case scrutinee
        [ (pattern', pushVisibleArguments
            (Generated.applyExpressionArguments branch existing) arguments)
        | (pattern', branch) <- alternatives
        ]
    Generated.Let pattern' binding body ->
      Generated.Let pattern' binding $
        pushVisibleArguments
          (Generated.applyExpressionArguments body existing) arguments
    Generated.Global{} -> appendVisible
      applicationHead existing arguments
    Generated.Local{} -> appendVisible
      applicationHead existing arguments
    _ -> Generated.applyExpressionArguments applicationHead existing

  appendVisible applicationHead existing arguments =
    Generated.applyExpressionArguments applicationHead $
      existing ++ map Generated.VisibleTypeArgumentArgument arguments

  evidenceBinder = selected
   where
    (selected, _, _) = allocateFresh
      (\suffix ->
        ( "$djinn$instantiated" ++ suffixSpelling suffix
        , suffix + 1
        ))
      reservedNames (0 :: Natural)

  suffixSpelling 0 = ""
  suffixSpelling suffix = show suffix

-- | Construct and validate the shared clause used by the stable Djinn core.
termToGeneratedClause
  :: Generated.DefinitionName
  -> Term
  -> Either String (Generated.FunctionClause HSymbol)
termToGeneratedClause target term = do
  expression <- termToGeneratedExpression term
  expressionToClause target expression

-- | Convert a proof while retaining every eta expansion. This is used only
-- when erased rank-N instantiation evidence exposes an eta redex: Haskell's
-- simplified subsumption can accept @\x -> finish x@ while rejecting the
-- propositionally eta-equivalent @finish@ at the higher-rank type.
termToGeneratedClauseWithoutEta
  :: Generated.DefinitionName
  -> Term
  -> Either String (Generated.FunctionClause HSymbol)
termToGeneratedClauseWithoutEta target term = do
  expression <- termToGeneratedExpressionWithEta False term
  expressionToClause target expression

-- | Convert a proof whose retained instantiation axioms carry explicit type
-- choices. Applied evidence becomes @provider @T@; a bare axiom becomes the
-- corresponding @\provider -> provider @T@ function. Eta contraction stays
-- disabled for the same rank-N reason as 'termToGeneratedClauseWithoutEta'.
termToGeneratedClauseWithVisibleApplications
  :: Map.Map Symbol [Generated.VisibleTypeArgument]
  -> Generated.DefinitionName
  -> Term
  -> Either String (Generated.FunctionClause HSymbol)
termToGeneratedClauseWithVisibleApplications evidence target term = do
  expression <- termToGeneratedExpressionWithEvidence False evidence term
  expressionToClause target expression

expressionToClause
  :: Generated.DefinitionName
  -> Expression
  -> Either String (Generated.FunctionClause HSymbol)
expressionToClause target expression = do
  let clause = Generated.functionClauseFromExpression target expression
  either (Left . show) Right $
    Generated.validateFunctionClauseScope clause
  either (Left . show) Right $
    Generated.validateFunctionClauseSyntax Generated.FullyQualified clause
  pure clause

unSymbol :: Symbol -> HSymbol
unSymbol = symbolSpelling

tuple :: [Expression] -> Expression
tuple [expression] = expression
tuple expressions = Generated.Tuple expressions

-- Names present before Haskell conversion. Extra binders introduced for
-- constructor fields must avoid this complete set.
termNames :: Term -> Set.Set HSymbol
termNames proofTerm = case proofTerm of
  Var symbol -> Set.singleton $ unSymbol symbol
  Lam symbol body -> Set.insert (unSymbol symbol) $ termNames body
  Apply function argument -> termNames function `Set.union` termNames argument
  Xsel _ _ expression -> termNames expression
  _ -> Set.empty

-- The shared traversal owns the grammar walk; only the "__djinn" spelling
-- policy and the reservation of the term's free names remain local.
alphaRenameTerm :: Term -> Term
alphaRenameTerm term = evalState
    (freshenTermBinders freshBinder term)
    (Set.fromList $ freeVars term, 1 :: Natural)
 where
  freshBinder = state $ \(used, next) ->
    let (fresh, used', next') = allocateFresh
          (\suffix -> (Symbol $ "__djinn" ++ show suffix, suffix + 1))
          used next
    in (fresh, (used', next'))

-- | Bake Djinn's historical @a@..@z@ then @x1@.. display names into the
-- stored candidate tree.
--
-- This is deliberately a pre-storage canonicalization, not a rendering
-- preference, and must not migrate to the shared render-time allocator:
--
-- * the spellings ARE the public stored-tree contract —
--   v'Language.Haskell.Synthesis.Candidate.candidateOutput', the
--   t'Djinn.Internal.Generated.HClause' compatibility view, and every
--   identity-preference render
--   site (REPL, CLI, @hPrClause@) observe them directly;
-- * candidate de-duplication in "Djinn.Core" is alpha-aware, but canonical
--   stored names still keep direct tree inspection and legacy projections
--   stable independently of which proof plan produced a clause;
-- * the collision set is intentionally qualification-independent
--   ('unqualifiedValueGlobals' rather than the renderer's
--   qualification-sensitive emitted set), so one candidate has one spelling
--   regardless of the render options later applied to it.
--
-- Exference makes the opposite choice — search-native locals stored, name
-- hints supplied at render time — because its locals are 'Int's with no
-- historical stored spelling. Both policies are correct for their contracts.
niceNames :: Expression -> Expression
niceNames expression = fmap rename expression
 where
  boundVariables = catMaybes $ Generated.expressionBindingSites expression
  preferred = map pure ['a' .. 'z']
    ++ ["x" ++ show index | index <- [1 :: Natural ..]]
  available = preferred \\ Set.toList (unqualifiedValueGlobals expression)
  substitutions = zip boundVariables available
  rename variable = fromMaybe variable $ lookup variable substitutions

unqualifiedValueGlobals :: Expression -> Set.Set HSymbol
unqualifiedValueGlobals expression = Set.fromList
  [ spelling
  | name <- Generated.expressionGlobals expression
  , Name.nameModule name == Nothing
  , Name.nameLexicalClass name == Name.VariableLike
  , Just spelling <- [Name.nameSpelling name]
  ]

freeValueSpellings :: Expression -> Set.Set HSymbol
freeValueSpellings expression = Set.fromList
  [ renderProofSymbolName name
  | name <- Generated.expressionGlobals expression
  , Name.nameLexicalClass name == Name.VariableLike
  ]
