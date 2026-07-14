-- | Djinn's proof-output cleanup tree and adapter to the shared renderer.
--
-- The historical Haskell-shaped nodes remain useful while translating and
-- simplifying LJT proofs.  This module owns their final erasure into
-- 'Language.Haskell.Synthesis.Generated', keeping source types and proof
-- machinery out of the common output boundary.
module Djinn.Internal.Generated
  ( HClause (..)
  , HPat (..)
  , HExpr (..)
  , hPrClause
  , renderGeneratedClause
  , toGeneratedClause
  , getBinderVars
  , getBinderVarsHE
  , getBinderVarsHP
  ) where

import qualified Data.Set as Set
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as SharedName

type HSymbol = String

data HClause = HClause HSymbol [HPat] HExpr
  deriving (Show, Eq)

data HPat
  = HPVar HSymbol
  | HPCon HSymbol
  | HPTuple [HPat]
  | HPAt HSymbol HPat
  | HPApply HPat HPat
  deriving (Show, Eq)

data HExpr
  = HELam [HPat] HExpr
  | HEApply HExpr HExpr
  | HECon HSymbol
  | HEVar HSymbol
  | HETuple [HExpr]
  | HECase HExpr [(HPat, HExpr)]
  deriving (Show, Eq)

hPrClause :: HClause -> Either String String
hPrClause clause = toGeneratedClause clause >>= renderGeneratedClause

renderGeneratedClause
  :: Generated.FunctionClause HSymbol
  -> Either String String
renderGeneratedClause generated = do
  either (Left . show) Right $
    Generated.validateFunctionClauseScope generated
  either (Left . show) Right $
    Generated.renderFunctionClause renderOptions generated
  where
    renderOptions = Generated.RenderOptions
      Generated.FullyQualified id []

-- | Erase Djinn's post-proof Haskell tree into the shared generated-code AST.
-- Bound variables become backend-owned local identities; every free value and
-- constructor becomes a validated structural global name.
toGeneratedClause
  :: HClause
  -> Either String (Generated.FunctionClause HSymbol)
toGeneratedClause (HClause functionName patterns expression) = do
  rawName <- generatedName "function" functionName
  convertedPatterns <- mapM convertPattern patterns
  let bound = Set.fromList $ filter (/= "_") $
        concatMap getBinderVarsHP patterns
  convertedExpression <- convertExpression bound expression
  -- Clause scope historically precedes definition-name syntax in Djinn's
  -- compatibility diagnostics.  A lambda has exactly the same binder scope
  -- as a clause, including for an empty pattern list, and lets us preserve
  -- that ordering before constructing the checked shared name.
  either (Left . show) Right $ Generated.validateExpressionScope
    $ Generated.Lambda convertedPatterns convertedExpression
  name <- either (Left . show) Right $ Generated.mkDefinitionName rawName
  let generated = Generated.FunctionClause
        name convertedPatterns convertedExpression
  either (Left . show) Right $
    Generated.validateFunctionClauseSyntax Generated.FullyQualified generated
  return generated

convertExpression
  :: Set.Set HSymbol
  -> HExpr
  -> Either String (Generated.Expression HSymbol)
convertExpression bound expression = case expression of
  HELam patterns body -> do
    convertedPatterns <- mapM convertPattern patterns
    let binders = Set.fromList $ filter (/= "_") $
          concatMap getBinderVarsHP patterns
    Generated.Lambda convertedPatterns <$>
      convertExpression (bound `Set.union` binders) body
  HEApply function argument -> Generated.Apply
    <$> convertExpression bound function
    <*> convertExpression bound argument
  HECon name -> Generated.Global <$> generatedName "constructor" name
  HEVar name
    | name `Set.member` bound -> Right $ Generated.Local name
    | otherwise -> Generated.Global <$> generatedName "value" name
  HETuple elements -> Generated.Tuple <$>
    mapM (convertExpression bound) elements
  HECase scrutinee alternatives -> Generated.Case
    <$> convertExpression bound scrutinee
    <*> mapM convertAlternative alternatives
    where
      convertAlternative (pattern, body) = do
        convertedPattern <- convertPattern pattern
        let binders = Set.fromList $ filter (/= "_") $
              getBinderVarsHP pattern
        convertedBody <- convertExpression
          (bound `Set.union` binders) body
        return (convertedPattern, convertedBody)

convertPattern :: HPat -> Either String (Generated.Pattern HSymbol)
convertPattern pattern = case pattern of
  HPVar "_" -> Right Generated.Wildcard
  HPVar variable -> Right $ Generated.Bind variable
  HPCon constructor -> Generated.Constructor
    <$> generatedName "pattern constructor" constructor <*> pure []
  HPTuple elements -> Generated.TuplePattern <$> mapM convertPattern elements
  HPAt variable nested -> Generated.As variable <$> convertPattern nested
  HPApply{} -> do
    let (headPattern, arguments) = patternSpine pattern
    case headPattern of
      HPCon constructor -> Generated.Constructor
        <$> generatedName "pattern constructor" constructor
        <*> mapM convertPattern arguments
      _ -> Left $ "generated pattern application has non-constructor head: "
        ++ show headPattern

patternSpine :: HPat -> (HPat, [HPat])
patternSpine = collect []
  where
    collect arguments (HPApply function argument) =
      collect (argument : arguments) function
    collect arguments function = (function, arguments)

generatedName :: String -> HSymbol -> Either String SharedName.Name
generatedName description source = case SharedName.parseName source of
  Left nameError -> Left $ "invalid generated " ++ description ++ " "
    ++ show source ++ ": " ++ SharedName.renderNameError nameError
  Right name -> Right name

getBinderVars :: HClause -> [HSymbol]
getBinderVars (HClause _ patterns expression) =
  concatMap getBinderVarsHP patterns ++ getBinderVarsHE expression

getBinderVarsHE :: HExpr -> [HSymbol]
getBinderVarsHE expression = case expression of
  HELam patterns body ->
    concatMap getBinderVarsHP patterns ++ getBinderVarsHE body
  HEApply function argument ->
    getBinderVarsHE function ++ getBinderVarsHE argument
  HECon{} -> []
  HEVar{} -> []
  HETuple elements -> concatMap getBinderVarsHE elements
  HECase scrutinee alternatives ->
    getBinderVarsHE scrutinee ++ concat
      [ getBinderVarsHP pattern ++ getBinderVarsHE body
      | (pattern, body) <- alternatives
      ]

getBinderVarsHP :: HPat -> [HSymbol]
getBinderVarsHP pattern = case pattern of
  HPVar name -> [name]
  HPCon{} -> []
  HPTuple elements -> concatMap getBinderVarsHP elements
  HPAt name nested -> name : getBinderVarsHP nested
  HPApply function argument ->
    getBinderVarsHP function ++ getBinderVarsHP argument
