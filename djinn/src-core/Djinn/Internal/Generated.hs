-- | Djinn's historical proof-output view and adapters to shared syntax.
--
-- The Haskell-shaped nodes remain constructible for source compatibility,
-- but live proof cleanup happens over 'Language.Haskell.Synthesis.Generated'.
-- This module validates caller-built legacy trees on the way in and projects
-- checked shared output back only when a compatibility caller requests it.
module Djinn.Internal.Generated
  ( HClause (..)
  , HPat (..)
  , HExpr (..)
  , hPrClause
  , renderGeneratedClause
  , deduplicateEtaEquivalentClauses
  , toGeneratedClause
  , toGeneratedClauseWithName
  , fromGeneratedExpression
  , fromGeneratedClause
  ) where

import qualified Data.Set as Set
import Djinn.Internal.GeneratedDeduplication
  ( deduplicateEtaEquivalentClausesOn )
import Djinn.Internal.HIdentifier
  ( generatedGlobalName
  , generatedName
  , renderProofSymbolName
  )
import qualified Language.Haskell.Synthesis.Generated as Generated
import qualified Language.Haskell.Synthesis.Name as SharedName

type HSymbol = String

-- | A legacy function clause: the defined name, its argument patterns, and
-- the right-hand side.  Values are validated by 'toGeneratedClause' before
-- any rendering.
data HClause = HClause HSymbol [HPat] HExpr
  deriving (Show, Eq)

-- | A legacy pattern.  @'HPVar' \"_\"@ is the wildcard; constructor
-- arguments are curried through 'HPApply' and must have an 'HPCon' head.
data HPat
  = HPVar HSymbol
  | HPCon HSymbol
  | HPTuple [HPat]
  | HPAt HSymbol HPat
  | HPApply HPat HPat
  deriving (Show, Eq)

-- | A legacy proof-output expression: lambdas, application, constructor and
-- variable references, tuples, and case.  An 'HEVar' names a bound local
-- when a surrounding pattern binds it and a global value otherwise.
data HExpr
  = HELam [HPat] HExpr
  | HEApply HExpr HExpr
  | HECon HSymbol
  | HEVar HSymbol
  | HETuple [HExpr]
  | HECase HExpr [(HPat, HExpr)]
  deriving (Show, Eq)

-- | Render a legacy clause as Haskell source by converting it with
-- 'toGeneratedClause' and then applying 'renderGeneratedClause'; either
-- step's failure is reported.
hPrClause :: HClause -> Either String String
hPrClause clause = toGeneratedClause clause >>= renderGeneratedClause

-- | Scope-check a shared clause and render it as a Haskell equation with
-- fully qualified global names.  This is the rendering policy of Djinn's
-- compatibility layer, e.g. the strings in 'Djinn.Core.Realized'.
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

-- | Remove alpha-equivalent clauses after comparing their fully eta-normal
-- denotations, while retaining the first checked spelling unchanged.
--
-- Function clauses and lambdas store multiple binders in one pattern group,
-- whereas the generic eta reducer contracts a unary lambda. Split every group
-- into a nested unary suffix in the private comparison key so @f@ and
-- @\x y -> f x y@ compare alike without contracting the surviving clause.
deduplicateEtaEquivalentClauses
  :: Ord local
  => [Generated.FunctionClause local]
  -> [Generated.FunctionClause local]
deduplicateEtaEquivalentClauses =
  deduplicateEtaEquivalentClausesOn id

-- | Erase Djinn's post-proof Haskell tree into the shared generated-code AST.
-- Bound variables become backend-owned local identities; every free value and
-- constructor becomes a validated structural global name.
toGeneratedClause
  :: HClause
  -> Either String (Generated.FunctionClause HSymbol)
toGeneratedClause (HClause functionName patterns expression) = do
  rawName <- generatedName "function" functionName
  toGeneratedClauseWith
    (either (Left . show) Right $ Generated.mkDefinitionName rawName)
    patterns
    expression

-- | Convert a clause while retaining an already checked definition identity.
-- The proof tree still supplies the textual name used internally, but the
-- stable request remains authoritative for generated output.
toGeneratedClauseWithName
  :: Generated.DefinitionName
  -> HClause
  -> Either String (Generated.FunctionClause HSymbol)
toGeneratedClauseWithName name (HClause _ patterns expression) =
  toGeneratedClauseWith (Right name) patterns expression

-- | Reconstruct Djinn's historical output view on demand.  The stable proof
-- path no longer stores this tree; unsupported shared forms fail explicitly
-- rather than pretending that Djinn's old grammar contained holes or lets.
fromGeneratedExpression
  :: Generated.Expression HSymbol
  -> Either String HExpr
fromGeneratedExpression expression = case expression of
  Generated.Local variable -> Right $ HEVar variable
  Generated.Global name -> Right $
    if SharedName.nameLexicalClass name == SharedName.ConstructorLike
      then HECon $ renderProofSymbolName name
      else HEVar $ renderProofSymbolName name
  Generated.Lambda patterns body -> HELam
    <$> mapM fromGeneratedPattern patterns
    <*> fromGeneratedExpression body
  Generated.Apply function argument -> HEApply
    <$> fromGeneratedExpression function
    <*> fromGeneratedExpression argument
  Generated.VisibleTypeApplication{} -> Left $
    "Djinn compatibility expressions cannot represent visible type applications"
  Generated.Tuple elements -> HETuple <$> mapM fromGeneratedExpression elements
  Generated.Hole{} -> Left $
    "Djinn compatibility expressions cannot represent generated holes"
  Generated.Let{} -> Left $
    "Djinn compatibility expressions cannot represent generated lets"
  Generated.Case scrutinee alternatives -> HECase
    <$> fromGeneratedExpression scrutinee
    <*> mapM fromAlternative alternatives
 where
  fromAlternative (pattern, body) = (,)
    <$> fromGeneratedPattern pattern
    <*> fromGeneratedExpression body

-- | Project a checked shared definition into the legacy clause record.
fromGeneratedClause
  :: Generated.FunctionClause HSymbol
  -> Either String HClause
fromGeneratedClause (Generated.FunctionClause name patterns body) = HClause
  (Generated.definitionSpelling name)
  <$> mapM fromGeneratedPattern patterns
  <*> fromGeneratedExpression body

fromGeneratedPattern
  :: Generated.Pattern HSymbol
  -> Either String HPat
fromGeneratedPattern pattern = case pattern of
  Generated.Bind variable -> Right $ HPVar variable
  Generated.Wildcard -> Right $ HPVar "_"
  Generated.Constructor name arguments -> do
    converted <- mapM fromGeneratedPattern arguments
    Right $ foldl' HPApply (HPCon $ renderProofSymbolName name) converted
  Generated.TuplePattern elements ->
    HPTuple <$> mapM fromGeneratedPattern elements
  Generated.As variable nested -> HPAt variable <$> fromGeneratedPattern nested

toGeneratedClauseWith
  :: Either String Generated.DefinitionName
  -> [HPat]
  -> HExpr
  -> Either String (Generated.FunctionClause HSymbol)
toGeneratedClauseWith checkedName patterns expression = do
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
  name <- checkedName
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
  HECon name -> Generated.Global <$>
    generatedGlobalName SharedName.ConstructorLike "constructor" name
  HEVar name
    | name `Set.member` bound -> Right $ Generated.Local name
    | otherwise -> Generated.Global <$>
        generatedGlobalName SharedName.VariableLike "value" name
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
    <$> generatedGlobalName SharedName.ConstructorLike
      "pattern constructor" constructor
    <*> pure []
  HPTuple elements -> Generated.TuplePattern <$> mapM convertPattern elements
  HPAt variable nested -> Generated.As variable <$> convertPattern nested
  HPApply{} -> do
    let (headPattern, arguments) = patternSpine pattern
    case headPattern of
      HPCon constructor -> Generated.Constructor
        <$> generatedGlobalName SharedName.ConstructorLike
          "pattern constructor" constructor
        <*> mapM convertPattern arguments
      _ -> Left $ "generated pattern application has non-constructor head: "
        ++ show headPattern

patternSpine :: HPat -> (HPat, [HPat])
patternSpine = collect []
  where
    collect arguments (HPApply function argument) =
      collect (argument : arguments) function
    collect arguments function = (function, arguments)

-- Binding-site observations over checked shared clauses live in
-- 'Language.Haskell.Synthesis.Generated'; only the legacy-pattern binder
-- collection below is still needed while converting a caller-built tree.
getBinderVarsHP :: HPat -> [HSymbol]
getBinderVarsHP pattern = case pattern of
  HPVar name -> [name]
  HPCon{} -> []
  HPTuple elements -> concatMap getBinderVarsHP elements
  HPAt name nested -> name : getBinderVarsHP nested
  HPApply function argument ->
    getBinderVarsHP function ++ getBinderVarsHP argument
