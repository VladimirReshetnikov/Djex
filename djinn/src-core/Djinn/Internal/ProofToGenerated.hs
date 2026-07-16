-- | Lower checked LJT proofs directly into Djex's shared generated syntax.
--
-- Djinn historically translated proofs through a second Haskell-shaped tree
-- ('HExpr' and 'HPat').  That tree remains available as a compatibility view,
-- but the live backend now performs cleanup here so the checked shared tree is
-- its only generated-output authority.
module Djinn.Internal.ProofToGenerated
  ( termToGeneratedExpression
  , termToGeneratedClause
  ) where

import Control.Monad (foldM, zipWithM)
import Data.Foldable (toList)
import Data.List (find, transpose, (\\))
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Djinn.Internal.LJTFormula
import Djinn.Internal.HIdentifier (renderProofSymbolName)
import Language.Haskell.Synthesis.Fresh (allocateFresh)
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as Name

type HSymbol = String
type Pattern = Generated.Pattern HSymbol
type Expression = Generated.Expression HSymbol

-- | Convert and simplify one proof expression without constructing Djinn's
-- historical recursive output tree.
termToGeneratedExpression :: Term -> Either String Expression
termToGeneratedExpression term = do
  (expression, _) <- convert [] renamedTerm
  let simplified = niceNames $ Generated.simplifyExpressionBy id $
        removeUnusedVariables $ fixSillyAsPatterns $
          removeUnusedVariables expression
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

  convert enclosing (Var symbol) = do
    let spelling = unSymbol symbol
    expression <- if spelling `elem` enclosing
      then Right $ Generated.Local spelling
      else Generated.Global <$> generatedName "value" spelling
    pure (expression, [])
  convert enclosing (Lam symbol body) = do
    let spelling = unSymbol symbol
    (convertedBody, refinements) <- convert (spelling : enclosing) body
    pattern' <- convertVariable spelling refinements
    pure (lambda [pattern'] convertedBody, refinements)
  convert enclosing (Apply (Cinj (ConsDesc constructor arity) _) argument) = do
    (convertedArgument, refinements) <- convert enclosing argument
    (wrap, arguments) <- unpackTuple arity convertedArgument
    constructorName <- generatedName "constructor" constructor
    pure
      (wrap $ foldl Generated.Apply
        (Generated.Global constructorName) arguments, refinements)
  convert enclosing (Apply function argument) =
    convertApplication enclosing function [argument]
  convert _ (Ctuple 0) = do
    unit <- generatedName "constructor" "()"
    pure (Generated.Global unit, [])
  convert _ unsupported =
    Left $ "unsupported proof term: " ++ show unsupported

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
          ( foldl Generated.Apply
              (caseExpression convertedScrutinee alternatives)
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
            ( foldl Generated.Apply
                (caseExpression convertedScrutinee
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
        constructorName <- generatedName "pattern constructor" constructor
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
  unpackLambda arity (Generated.Lambda patterns body)
    | length patterns >= arity =
        let (used, remaining) = splitAt arity patterns
        in Right (used, lambda remaining body)
  unpackLambda arity expression = Left $
    "tuple handler has shape " ++ show expression ++ ", expected "
      ++ show arity ++ " lambda argument(s)"

-- | Construct and validate the shared clause used by the stable Djinn core.
termToGeneratedClause
  :: Generated.DefinitionName
  -> Term
  -> Either String (Generated.FunctionClause HSymbol)
termToGeneratedClause target term = do
  expression <- termToGeneratedExpression term
  let (patterns, body) = case expression of
        Generated.Lambda leadingPatterns leadingBody ->
          (leadingPatterns, leadingBody)
        _ -> ([], expression)
      clause = Generated.FunctionClause target patterns body
  either (Left . show) Right $
    Generated.validateFunctionClauseScope clause
  either (Left . show) Right $
    Generated.validateFunctionClauseSyntax Generated.FullyQualified clause
  pure clause

unSymbol :: Symbol -> HSymbol
unSymbol (Symbol spelling) = spelling

generatedName :: String -> HSymbol -> Either String Name.Name
generatedName description source = case Name.parseName source of
  Left nameError -> Left $ "invalid generated " ++ description ++ " "
    ++ show source ++ ": " ++ Name.renderNameError nameError
  Right name -> Right name

tuple :: [Expression] -> Expression
tuple [expression] = expression
tuple expressions = Generated.Tuple expressions

lambda :: [Pattern] -> Expression -> Expression
lambda [] expression = expression
lambda patterns (Generated.Lambda morePatterns body) =
  Generated.Lambda (patterns ++ morePatterns) body
lambda patterns expression = Generated.Lambda patterns expression

-- Names present before Haskell conversion. Extra binders introduced for
-- constructor fields must avoid this complete set.
termNames :: Term -> Set.Set HSymbol
termNames proofTerm = case proofTerm of
  Var symbol -> Set.singleton $ unSymbol symbol
  Lam symbol body -> Set.insert (unSymbol symbol) $ termNames body
  Apply function argument -> termNames function `Set.union` termNames argument
  Xsel _ _ expression -> termNames expression
  _ -> Set.empty

alphaRenameTerm :: Term -> Term
alphaRenameTerm term = renamed
 where
  (renamed, _, _) =
    rename [] (Set.fromList $ freeVars term) (1 :: Natural) term

  rename environment used next proofTerm = case proofTerm of
    Var symbol ->
      (Var $ fromMaybe symbol $ lookup symbol environment, used, next)
    Lam binder body ->
      let (fresh, used', next') = freshBinder used next
          (body', used'', next'') =
            rename ((binder, fresh) : environment) used' next' body
      in (Lam fresh body', used'', next'')
    Apply function argument ->
      let (function', used', next') =
            rename environment used next function
          (argument', used'', next'') =
            rename environment used' next' argument
      in (Apply function' argument', used'', next'')
    Xsel index arity expression ->
      let (expression', used', next') =
            rename environment used next expression
      in (Xsel index arity expression', used', next')
    _ -> (proofTerm, used, next)

  freshBinder used next = allocateFresh
    (\suffix -> (Symbol $ "__djinn" ++ show suffix, suffix + 1))
    used next

patternVariables :: [Pattern] -> Set.Set HSymbol
patternVariables = Set.fromList . concatMap toList

fixSillyAsPatterns :: Expression -> Expression
fixSillyAsPatterns = fixPatterns []
 where
  fixPatterns substitutions expression = case expression of
    Generated.Lambda patterns body ->
      let (patterns', renamings) = unzip $ map findSilly patterns
      in Generated.Lambda patterns' $
        fixPatterns (concat renamings ++ substitutions) body
    Generated.Apply function argument -> Generated.Apply
      (fixPatterns substitutions function)
      (fixPatterns substitutions argument)
    original@Generated.Global{} -> original
    original@(Generated.Local variable) ->
      maybe original Generated.Local $ lookup variable substitutions
    Generated.Tuple expressions ->
      Generated.Tuple $ map (fixPatterns substitutions) expressions
    original@Generated.Hole{} -> original
    Generated.Let pattern' binding body ->
      let (pattern'', renamings) = findSilly pattern'
      in Generated.Let pattern''
        (fixPatterns substitutions binding)
        (fixPatterns (renamings ++ substitutions) body)
    Generated.Case scrutinee alternatives ->
      collapseCase
        (fixPatterns substitutions scrutinee)
        (map (fixAlternative substitutions) alternatives)

  fixAlternative substitutions (pattern', expression) =
    let (pattern'', renamings) = findSilly pattern'
    in (pattern'', fixPatterns (renamings ++ substitutions) expression)

  findSilly original@Generated.Bind{} = (original, [])
  findSilly Generated.Wildcard = (Generated.Wildcard, [])
  findSilly (Generated.Constructor name arguments) =
    let (arguments', renamings) = unzip $ map findSilly arguments
    in (Generated.Constructor name arguments', concat renamings)
  findSilly (Generated.TuplePattern patterns) =
    let (patterns', renamings) = unzip $ map findSilly patterns
    in (Generated.TuplePattern patterns', concat renamings)
  findSilly (Generated.As variable pattern') = case findSilly pattern' of
    (Generated.Wildcard, renamings) ->
      (Generated.Bind variable, renamings)
    (converted@(Generated.Bind variable'), renamings) ->
      (converted, (variable, variable') : renamings)
    (converted, renamings) ->
      (Generated.As variable converted, renamings)

  collapseCase scrutinee alternatives = case alternatives of
    (pattern', expression) : rest
      | null (toList pattern')
      , all (sameUnboundExpression expression) rest -> expression
    _ -> Generated.Case scrutinee alternatives

  sameUnboundExpression expression (pattern', expression') =
    null (toList pattern') && alphaEquivalent expression expression'

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

caseExpression :: Expression -> [(Pattern, Expression)] -> Expression
caseExpression scrutinee [] = Generated.Case scrutinee []
caseExpression _ [(Generated.Constructor name [], expression)]
  | Name.nameSpecial name == Just (Name.TupleConstructor Name.Boxed 0) =
      expression
caseExpression scrutinee alternatives
  | all (uncurry patternEqualsExpression) alternatives = scrutinee
caseExpression scrutinee [(pattern', Generated.Lambda patterns body)] =
  lambda patterns $ caseExpression scrutinee [(pattern', body)]
caseExpression scrutinee
    alternatives@((_, firstExpression@Generated.Lambda{}) : rest)
  | commonCount > 0 =
      lambda (map bindingPattern canonicalNames) $
        caseExpression scrutinee convertedAlternatives
 where
  commonCount = foldr (min . lambdaBinderCount . snd)
    (lambdaBinderCount firstExpression) rest
  lambdaBinderCount (Generated.Lambda patterns _) =
    length $ takeWhile isVariablePattern patterns
  lambdaBinderCount _ = 0
  isVariablePattern Generated.Bind{} = True
  isVariablePattern Generated.Wildcard = True
  isVariablePattern _ = False
  convertedAlternatives =
    [ let (used, remaining) = splitAt commonCount patterns
          substitutions =
            [ (source, target)
            | (Generated.Bind source, target) <- zip used canonicalNames
            , source /= target
            ]
      in (constructorPattern, lambda remaining $
            substituteLocals substitutions expression)
    | (constructorPattern, Generated.Lambda patterns expression) <- alternatives
    ]
  binderColumns = transpose
    [take commonCount patterns | (_, Generated.Lambda patterns _) <- alternatives]
  canonicalNames = map canonicalName binderColumns
  canonicalName patterns = fromMaybe "_" $
    find (/= "_") [name | Generated.Bind name <- patterns]
caseExpression _ ((_, expression) : alternatives@(_ : _))
  | all (alphaEquivalent expression . snd) alternatives = expression
caseExpression scrutinee alternatives =
  Generated.Case scrutinee alternatives

bindingPattern :: HSymbol -> Pattern
bindingPattern "_" = Generated.Wildcard
bindingPattern variable = Generated.Bind variable

patternEqualsExpression :: Pattern -> Expression -> Bool
patternEqualsExpression (Generated.Bind variable)
    (Generated.Local variable') = variable == variable'
patternEqualsExpression (Generated.Constructor name patterns) expression =
  case expressionSpine expression of
    (Generated.Global name', arguments) ->
      name == name' && length patterns == length arguments
        && and (zipWith patternEqualsExpression patterns arguments)
    _ -> False
patternEqualsExpression (Generated.TuplePattern patterns)
    (Generated.Tuple expressions) =
  length patterns == length expressions
    && and (zipWith patternEqualsExpression patterns expressions)
patternEqualsExpression _ _ = False

expressionSpine :: Expression -> (Expression, [Expression])
expressionSpine = collect []
 where
  collect arguments (Generated.Apply function argument) =
    collect (argument : arguments) function
  collect arguments function = (function, arguments)

alphaEquivalent :: Expression -> Expression -> Bool
alphaEquivalent left right
  | left == right = True
alphaEquivalent (Generated.Lambda leftPatterns leftBody)
    (Generated.Lambda rightPatterns rightBody) =
  case matchPattern
      (Generated.TuplePattern leftPatterns)
      (Generated.TuplePattern rightPatterns) of
    Just substitutions ->
      alphaEquivalent (substituteLocals substitutions leftBody) rightBody
    Nothing -> False
alphaEquivalent (Generated.Apply leftFunction leftArgument)
    (Generated.Apply rightFunction rightArgument) =
  alphaEquivalent leftFunction rightFunction
    && alphaEquivalent leftArgument rightArgument
alphaEquivalent (Generated.Global leftName) (Generated.Global rightName) =
  leftName == rightName
alphaEquivalent (Generated.Local leftVariable)
    (Generated.Local rightVariable) = leftVariable == rightVariable
alphaEquivalent (Generated.Tuple leftExpressions)
    (Generated.Tuple rightExpressions) =
  length leftExpressions == length rightExpressions
    && and (zipWith alphaEquivalent leftExpressions rightExpressions)
alphaEquivalent (Generated.Case leftScrutinee leftAlternatives)
    (Generated.Case rightScrutinee rightAlternatives) =
  length leftAlternatives == length rightAlternatives
    && alphaEquivalent leftScrutinee rightScrutinee
    && and (zipWith alphaEquivalent
      [Generated.Lambda [pattern'] body
      | (pattern', body) <- leftAlternatives]
      [Generated.Lambda [pattern'] body
      | (pattern', body) <- rightAlternatives])
alphaEquivalent _ _ = False

matchPattern :: Pattern -> Pattern -> Maybe [(HSymbol, HSymbol)]
matchPattern (Generated.Bind left) (Generated.Bind right) =
  Just [(left, right)]
matchPattern Generated.Wildcard Generated.Wildcard = Just []
matchPattern (Generated.Constructor leftName leftArguments)
    (Generated.Constructor rightName rightArguments)
  | leftName == rightName
  , length leftArguments == length rightArguments =
      concat <$> zipWithM matchPattern leftArguments rightArguments
matchPattern (Generated.TuplePattern left)
    (Generated.TuplePattern right)
  | length left == length right = concat <$> zipWithM matchPattern left right
matchPattern (Generated.As leftVariable leftPattern)
    (Generated.As rightVariable rightPattern) = do
  substitutions <- matchPattern leftPattern rightPattern
  pure $ (leftVariable, rightVariable) : substitutions
matchPattern _ _ = Nothing

substituteLocals :: [(HSymbol, HSymbol)] -> Expression -> Expression
substituteLocals substitutions = fmap $ \variable ->
  fromMaybe variable $ lookup variable substitutions

removeUnusedVariables :: Expression -> Expression
removeUnusedVariables expression = fst $ removeExpression expression
 where
  removeExpression original = case original of
    Generated.Lambda patterns body ->
      let (body', freeInBody) = removeExpression body
          binders = patternVariables patterns
      in ( Generated.Lambda (map (removePattern freeInBody) patterns) body'
         , freeInBody `Set.difference` binders
         )
    Generated.Apply function argument ->
      let (function', freeInFunction) = removeExpression function
          (argument', freeInArgument) = removeExpression argument
      in ( Generated.Apply function' argument'
         , freeInFunction `Set.union` freeInArgument
         )
    Generated.Tuple expressions ->
      let (expressions', freeInElements) = unzip $ map removeExpression expressions
      in (Generated.Tuple expressions', Set.unions freeInElements)
    Generated.Case scrutinee alternatives ->
      let (scrutinee', freeInScrutinee) = removeExpression scrutinee
          (alternatives', freeInAlternatives) = unzip
            [ let (body', freeInBody) = removeExpression body
                  binders = patternVariables [pattern']
              in ( (removePattern freeInBody pattern', body')
                 , freeInBody `Set.difference` binders
                 )
            | (pattern', body) <- alternatives
            ]
      in case alternatives' of
        [(Generated.Wildcard, body)] ->
          (body, Set.unions freeInAlternatives)
        _ ->
          ( caseExpression scrutinee' alternatives'
          , freeInScrutinee `Set.union` Set.unions freeInAlternatives
          )
    Generated.Let pattern' binding body ->
      let (binding', freeInBinding) = removeExpression binding
          (body', freeInBody) = removeExpression body
          binders = patternVariables [pattern']
      in ( Generated.Let (removePattern freeInBody pattern') binding' body'
         , freeInBinding `Set.union` (freeInBody `Set.difference` binders)
         )
    Generated.Local variable -> (original, Set.singleton variable)
    Generated.Global{} -> (original, Set.empty)
    Generated.Hole{} -> (original, Set.empty)

  removePattern freeInBody pattern' = case pattern' of
    original@(Generated.Bind variable)
      | variable `Set.member` freeInBody -> original
      | otherwise -> Generated.Wildcard
    Generated.Wildcard -> Generated.Wildcard
    Generated.Constructor name arguments ->
      Generated.Constructor name $ map (removePattern freeInBody) arguments
    Generated.TuplePattern patterns ->
      let patterns' = map (removePattern freeInBody) patterns
      in if all (== Generated.Wildcard) patterns'
        then Generated.Wildcard
        else Generated.TuplePattern patterns'
    Generated.As variable nested
      | variable `Set.member` freeInBody ->
          Generated.As variable $ removePattern freeInBody nested
      | otherwise -> removePattern freeInBody nested
