{-# LANGUAGE LambdaCase #-}

-- | Scoped, non-evaluating expression inference for the shared REPL.
--
-- This is intentionally a frontend over the authoritative neutral inventory,
-- not over Exference's policy-filtered search dictionary.  Consequently
-- @:type@ can inspect every loaded term, including declarations that synthesis
-- omits, while using the same import aliases and visibility rules as queries.
module Language.Haskell.Djex.REPL.Type
  ( InferredExpression (..)
  , inferExpressionType
  , renderInferredType
  ) where

import Control.Monad (foldM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , get
  , gets
  , modify'
  , runStateT
  )
import qualified Data.IntMap.Strict as IntMap
import Data.List (inits, intercalate, tails)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Language.Haskell.Exts.Extension
  ( Extension (EnableExtension)
  , KnownExtension
      ( BangPatterns
      , LambdaCase
      , PatternSignatures
      , ScopedTypeVariables
      , TupleSections
      )
  )
import qualified Language.Haskell.Exts.Parser as HSEParser
import Language.Haskell.Exts.Parser (ParseMode (extensions))
import qualified Language.Haskell.Exts.Pretty as HSEPretty
import qualified Language.Haskell.Exts.SrcLoc as HSELocation
import qualified Language.Haskell.Exts.Syntax as HSE

import Language.Haskell.Djex.Exference
  ( ExferenceSession
  , defaultExferenceOptions
  , exferenceRequestQuery
  )
import Language.Haskell.Djex.Exference.HaskellSrc
  ( ExferenceQueryScope (..)
  , parseExferenceRequestWithCheckedTargetInScope
  )
import qualified Language.Haskell.Djex.Exference.Internal.Session
  as ExferenceSession
import Language.Haskell.Djex.REPL.Command (TypeDefaulting (..))
import Language.Haskell.Djex.REPL.Scope
import Language.Haskell.Djex.Text (trim)
import Language.Haskell.Exference.Core.ConstraintSolver (isPossible)
import Language.Haskell.Exference.Core.Internal.VariableSupply
  ( freshSynthesisVariable )
import Language.Haskell.Exference.Core.Types
  ( HsType
  , QueryClassEnv
  , applySubsts
  , applySubstsChecked
  , constraintApplySubsts
  , defaultVariableName
  , mkQueryClassEnv
  , qClassEnv_env
  , qClassEnv_inflatedConstraints
  )
import Language.Haskell.Exference.Core.Unify (unifyShared)
import Language.Haskell.Exference.EnvironmentParser
  ( haskellSrcExtsParseMode )
import qualified Language.Haskell.Synthesis.Collection as SharedCollection
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  , sourceTextLocation
  , withCode
  , withContext
  , withSourceLocation
  )
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Name
  ( Boxity (..)
  , Name
  , NameError
  , ModuleName
  , SpecialName (..)
  , listName
  , mkIdentifier
  , mkModuleName
  , mkOperator
  , mkQualifiedIdentifier
  , mkQualifiedOperator
  , maximumTupleArity
  , nameSpecial
  , parseName
  , renderCanonical
  , renderNameError
  , specialName
  , tupleName
  )
import Language.Haskell.Synthesis.Qualification
  ( Qualification (..) )
import Language.Haskell.Synthesis.Query
  ( QueryRequest (requestGoal) )
import qualified Language.Haskell.Synthesis.Type as SharedType
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , Variable (..)
  )
import Language.Haskell.Synthesis.TypeRender
  ( renderConstraintWithQualification
  , renderTypeWithQualification
  )

-- | One successful @:type@ result before the REPL chooses qualification.
data InferredExpression = InferredExpression
  { inferredExpressionSource :: String
  , inferredExpressionConstraints :: [Constraint HsType]
  , inferredExpressionType :: HsType
  }
  deriving (Eq, Show)

data TypeContext = TypeContext
  { contextSession :: ExferenceSession
  , contextScope :: ReplScope
  , contextTarget :: DefinitionName
  , contextSourceName :: FilePath
  , contextSourceText :: String
  , contextTermSchemes :: Map Name HsType
  , contextTermNames :: Set Name
  , contextClasses :: QueryClassEnv
  }

data InferState = InferState
  { inferNextVariable :: !(Maybe Int)
  , inferSubstitutions :: !(IntMap.IntMap HsType)
  , inferConstraints :: [Constraint HsType]
  }

type Locals = Map String HsType
type Infer = StateT InferState (Either Diagnostic)

-- | Parse and infer one expression in the exact current module context.
-- Nothing is evaluated, and synthesis settings other than qualification do
-- not affect the result.
inferExpressionType
  :: ExferenceSession
  -> ReplScope
  -> DefinitionName
  -> TypeDefaulting
  -> FilePath
  -> String
  -> Either Diagnostic InferredExpression
inferExpressionType session scope target defaulting sourceName source = do
  context <- prepareContext session scope target sourceName source
  expression <- parseExpression context
  case (defaulting, bareGlobal expression) of
    (PreserveTypeVariables, Just name) -> do
      scheme <- lookupTermScheme context name
      displayScheme context scheme
    _ -> fst <$> runStateT
      (inferExpression context Map.empty expression
        >>= finalizeInference context defaulting)
      InferState
        { inferNextVariable = Just 1
        , inferSubstitutions = IntMap.empty
        , inferConstraints = []
        }

prepareContext
  :: ExferenceSession
  -> ReplScope
  -> DefinitionName
  -> FilePath
  -> String
  -> Either Diagnostic TypeContext
prepareContext session scope target sourceName source = do
  pure TypeContext
    { contextSession = session
    , contextScope = scope
    , contextTarget = target
    , contextSourceName = sourceName
    , contextSourceText = source
    , contextTermSchemes = schemes
    , contextTermNames = Map.keysSet schemes
    , contextClasses = ExferenceSession.sessionInspectionClasses session
    }
 where
  schemes = ExferenceSession.sessionInspectionTermSchemes session

parseExpression
  :: TypeContext
  -> Either Diagnostic (HSE.Exp HSELocation.SrcSpanInfo)
parseExpression context = case HSEParser.parseExpWithMode
    (expressionParseMode $ contextSourceName context)
    (contextSourceText context) of
  HSEParser.ParseOk expression -> Right expression
  HSEParser.ParseFailed location message -> Left
    $ typeDiagnostic context "DJEX_REPL_TYPE_PARSE"
        "cannot parse Haskell expression"
    $ show location ++ ": " ++ message

expressionParseMode :: FilePath -> ParseMode
expressionParseMode sourceName = base
  { extensions = map EnableExtension supported ++ extensions base }
 where
  base = haskellSrcExtsParseMode sourceName
  -- These extensions only admit AST forms implemented below; they do not
  -- broaden the loaded declaration language or imply GHC evaluation support.
  supported =
    [ BangPatterns
    , LambdaCase
    , PatternSignatures
    , ScopedTypeVariables
    , TupleSections
    ]

bareGlobal
  :: HSE.Exp location
  -> Maybe (HSE.QName location)
bareGlobal expression = case expression of
  HSE.Var _ name
    | not $ expressionHole name -> Just name
  HSE.Con _ name -> Just name
  HSE.Paren _ nested -> bareGlobal nested
  _ -> Nothing

displayScheme
  :: TypeContext
  -> HsType
  -> Either Diagnostic InferredExpression
displayScheme context scheme = normalizeResult context constraints body
 where
  (_, constraints, body) = SharedType.splitLeadingForalls scheme

inferExpression
  :: TypeContext
  -> Locals
  -> HSE.Exp HSELocation.SrcSpanInfo
  -> Infer HsType
inferExpression context locals expression = case expression of
  HSE.Var _ name -> inferName context locals name
  HSE.Con _ name -> instantiateTerm context name
  HSE.Lit _ literal -> inferLiteral context literal
  HSE.InfixApp _ left operator right -> do
    when (unsafeInfixOperand left || unsafeInfixOperand right)
      $ unsupported context
          "unparenthesized infix chains (loaded fixities are not retained)"
    operatorType <- instantiateTerm context $ operatorName operator
    leftType <- inferExpression context locals left
    rightType <- inferExpression context locals right
    applyTypes context operatorType [leftType, rightType]
  HSE.App _ function argument -> do
    functionType <- inferExpression context locals function
    argumentType <- inferExpression context locals argument
    applyTypes context functionType [argumentType]
  HSE.NegApp _ nested -> do
    nestedType <- inferExpression context locals nested
    addUnaryConstraint context "Prelude.Num" nestedType
    pure nestedType
  HSE.Lambda _ patterns body -> do
    parameterTypes <- mapM (const freshType) patterns
    (_, scoped) <- foldM
      (\(bound, environment) (pattern, expected) ->
        inferPattern context bound environment pattern expected)
      (Set.empty, locals)
      $ zip patterns parameterTypes
    result <- inferExpression context scoped body
    pure $ SharedType.functionType parameterTypes result
  HSE.Let{} -> unsupported context
    "let expressions (polymorphic local declarations are not implemented)"
  HSE.If _ condition trueBranch falseBranch -> do
    conditionType <- inferExpression context locals condition
    booleanType <- knownType context "Data.Bool.Bool"
    unifyTypes context conditionType booleanType
    trueType <- inferExpression context locals trueBranch
    falseType <- inferExpression context locals falseBranch
    unifyTypes context trueType falseType
    zonk trueType
  HSE.MultiIf{} -> unsupported context "multi-way if expressions"
  HSE.Case _ scrutinee alternatives ->
    inferCase context locals scrutinee alternatives
  HSE.Do{} -> unsupported context "do notation"
  HSE.MDo{} -> unsupported context "recursive do notation"
  HSE.Tuple _ boxity elements -> do
    let convertedBoxity = convertBoxity boxity
    ensureTupleArity context convertedBoxity $ length elements
    TupleType convertedBoxity
      <$> mapM (inferExpression context locals) elements
  HSE.UnboxedSum{} -> unsupported context "unboxed sums"
  HSE.TupleSection _ boxity elements -> do
    let convertedBoxity = convertBoxity boxity
    ensureTupleArity context convertedBoxity $ length elements
    elementTypes <- mapM (maybe freshType $ inferExpression context locals)
      elements
    let missing =
          [ elementType
          | (Nothing, elementType) <- zip elements elementTypes
          ]
    pure $ SharedType.functionType missing
      $ TupleType convertedBoxity elementTypes
  HSE.List _ elements -> do
    elementType <- freshType
    mapM_ (inferAndUnify context locals elementType) elements
    pure $ listType elementType
  HSE.ParArray{} -> unsupported context "parallel arrays"
  HSE.Paren _ nested -> inferExpression context locals nested
  HSE.LeftSection _ left operator -> do
    operatorType <- instantiateTerm context $ operatorName operator
    leftType <- inferExpression context locals left
    rightType <- freshType
    resultType <- applyTypes context operatorType [leftType, rightType]
    pure $ FunctionType rightType resultType
  HSE.RightSection _ operator right -> do
    operatorType <- instantiateTerm context $ operatorName operator
    leftType <- freshType
    rightType <- inferExpression context locals right
    resultType <- applyTypes context operatorType [leftType, rightType]
    pure $ FunctionType leftType resultType
  HSE.RecConstr{} -> unsupported context "record construction"
  HSE.RecUpdate{} -> unsupported context "record updates"
  HSE.EnumFrom _ first -> inferEnumeration context locals [first]
  HSE.EnumFromTo _ first final ->
    inferEnumeration context locals [first, final]
  HSE.EnumFromThen _ first next ->
    inferEnumeration context locals [first, next]
  HSE.EnumFromThenTo _ first next final ->
    inferEnumeration context locals [first, next, final]
  HSE.ParArrayFromTo{} -> unsupported context "parallel array enumeration"
  HSE.ParArrayFromThenTo{} ->
    unsupported context "parallel array enumeration"
  HSE.ListComp{} -> unsupported context "list comprehensions"
  HSE.ParComp{} -> unsupported context "parallel comprehensions"
  HSE.ParArrayComp{} -> unsupported context "parallel array comprehensions"
  HSE.ExpTypeSig _ nested signature -> do
    inferred <- inferExpression context locals nested
    annotated <- parseGroundAnnotation context signature
    unifyTypes context inferred annotated
    zonk annotated
  HSE.OverloadedLabel{} -> unsupported context "overloaded labels"
  HSE.IPVar{} -> unsupported context "implicit parameters"
  HSE.VarQuote{} -> unsupported context "quoted names"
  HSE.TypQuote{} -> unsupported context "quoted types"
  HSE.BracketExp{} -> unsupported context "Template Haskell brackets"
  HSE.SpliceExp{} -> unsupported context "Template Haskell splices"
  HSE.QuasiQuote{} -> unsupported context "quasiquotes"
  HSE.TypeApp{} -> unsupported context "visible type application"
  HSE.XTag{} -> unsupported context "XML expressions"
  HSE.XETag{} -> unsupported context "XML expressions"
  HSE.XPcdata{} -> unsupported context "XML expressions"
  HSE.XExpTag{} -> unsupported context "XML expressions"
  HSE.XChildTag{} -> unsupported context "XML expressions"
  HSE.CorePragma _ _ nested -> inferExpression context locals nested
  HSE.SCCPragma _ _ nested -> inferExpression context locals nested
  HSE.GenPragma _ _ _ _ nested -> inferExpression context locals nested
  HSE.Proc{} -> unsupported context "arrow notation"
  HSE.LeftArrApp{} -> unsupported context "arrow notation"
  HSE.RightArrApp{} -> unsupported context "arrow notation"
  HSE.LeftArrHighApp{} -> unsupported context "arrow notation"
  HSE.RightArrHighApp{} -> unsupported context "arrow notation"
  HSE.ArrOp{} -> unsupported context "arrow notation"
  HSE.LCase _ alternatives -> do
    argumentType <- freshType
    resultType <- inferAlternatives context locals argumentType alternatives
    pure $ FunctionType argumentType resultType

inferName
  :: TypeContext
  -> Locals
  -> HSE.QName HSELocation.SrcSpanInfo
  -> Infer HsType
inferName context locals name = case localIdentifier name of
  _ | expressionHole name -> typeFailure context "DJEX_REPL_TYPE_HOLE"
    "typed holes are not accepted by :type" "_"
  Just local | Just localType <- Map.lookup local locals -> pure localType
  _ -> instantiateTerm context name

inferLiteral
  :: TypeContext
  -> HSE.Literal HSELocation.SrcSpanInfo
  -> Infer HsType
inferLiteral context literal = case literal of
  HSE.Char{} -> knownType context "Data.Char.Char"
  HSE.String{} -> listType <$> knownType context "Data.Char.Char"
  HSE.Int{} -> do
    result <- freshType
    addUnaryConstraint context "Prelude.Num" result
    pure result
  HSE.Frac{} -> do
    result <- freshType
    addUnaryConstraint context "Prelude.Fractional" result
    pure result
  HSE.PrimInt{} -> unsupported context "primitive integer literals"
  HSE.PrimWord{} -> unsupported context "primitive word literals"
  HSE.PrimFloat{} -> unsupported context "primitive float literals"
  HSE.PrimDouble{} -> unsupported context "primitive double literals"
  HSE.PrimChar{} -> unsupported context "primitive character literals"
  HSE.PrimString{} -> unsupported context "primitive string literals"

inferPattern
  :: TypeContext
  -> Set String
  -> Locals
  -> HSE.Pat HSELocation.SrcSpanInfo
  -> HsType
  -> Infer (Set String, Locals)
inferPattern context bound locals pattern expected = case pattern of
  HSE.PVar _ name -> bindPatternName context bound locals name expected
  HSE.PLit _ _ literal -> do
    when (numericLiteral literal)
      $ addUnaryConstraint context "Data.Eq.Eq" expected
    literalType <- inferLiteral context literal
    unifyTypes context expected literalType
    pure (bound, locals)
  HSE.PNPlusK{} -> unsupported context "n+k patterns"
  HSE.PInfixApp _ left constructor right ->
    if unparenthesizedPatternInfix left || unparenthesizedPatternInfix right
      then unsupported context
        "unparenthesized infix-pattern chains (loaded fixities are not retained)"
      else inferConstructorPattern context bound locals constructor [left, right]
        expected
  HSE.PApp _ constructor patterns ->
    inferConstructorPattern context bound locals constructor patterns expected
  HSE.PTuple _ boxity patterns -> do
    let convertedBoxity = convertBoxity boxity
    ensureTupleArity context convertedBoxity $ length patterns
    elementTypes <- mapM (const freshType) patterns
    unifyTypes context expected $ TupleType convertedBoxity elementTypes
    foldM
      (\(seen, environment) (nested, nestedType) ->
        inferPattern context seen environment nested nestedType)
      (bound, locals) $ zip patterns elementTypes
  HSE.PUnboxedSum{} -> unsupported context "unboxed-sum patterns"
  HSE.PList _ patterns -> do
    elementType <- freshType
    unifyTypes context expected $ listType elementType
    foldM
      (\(seen, environment) nested ->
        inferPattern context seen environment nested elementType)
      (bound, locals) patterns
  HSE.PParen _ nested -> inferPattern context bound locals nested expected
  HSE.PRec{} -> unsupported context "record patterns"
  HSE.PAsPat _ name nested -> do
    (bound', locals') <- bindPatternName context bound locals name expected
    inferPattern context bound' locals' nested expected
  HSE.PWildCard{} -> pure (bound, locals)
  HSE.PIrrPat _ nested -> inferPattern context bound locals nested expected
  HSE.PatTypeSig _ nested signature -> do
    annotated <- parseGroundAnnotation context signature
    unifyTypes context expected annotated
    inferPattern context bound locals nested annotated
  HSE.PViewPat{} -> unsupported context "view patterns"
  HSE.PRPat{} -> unsupported context "regular patterns"
  HSE.PXTag{} -> unsupported context "XML patterns"
  HSE.PXETag{} -> unsupported context "XML patterns"
  HSE.PXPcdata{} -> unsupported context "XML patterns"
  HSE.PXPatTag{} -> unsupported context "XML patterns"
  HSE.PXRPats{} -> unsupported context "XML patterns"
  HSE.PSplice{} -> unsupported context "Template Haskell pattern splices"
  HSE.PQuasiQuote{} -> unsupported context "pattern quasiquotes"
  HSE.PBangPat _ nested -> inferPattern context bound locals nested expected

bindPatternName
  :: TypeContext
  -> Set String
  -> Locals
  -> HSE.Name location
  -> HsType
  -> Infer (Set String, Locals)
bindPatternName context bound locals name expected =
  case ordinaryNameSpelling name of
    Just spelling
      | spelling `Set.member` bound -> typeFailure context
          "DJEX_REPL_TYPE_PATTERN" "duplicate variable in pattern" spelling
      | otherwise -> pure
          (Set.insert spelling bound, Map.insert spelling expected locals)
    Nothing -> typeFailure context "DJEX_REPL_TYPE_PATTERN"
      "invalid pattern variable" $ HSEPretty.prettyPrint name

inferConstructorPattern
  :: TypeContext
  -> Set String
  -> Locals
  -> HSE.QName HSELocation.SrcSpanInfo
  -> [HSE.Pat HSELocation.SrcSpanInfo]
  -> HsType
  -> Infer (Set String, Locals)
inferConstructorPattern context bound locals constructor patterns expected = do
  constructorType <- instantiateTerm context constructor
  let (fields, result) = SharedType.functionSpine constructorType
  unless (length fields == length patterns) $ typeFailure context
    "DJEX_REPL_TYPE_PATTERN" "constructor pattern has the wrong arity"
    (HSEPretty.prettyPrint constructor ++ " expects " ++ show (length fields)
      ++ " fields but received " ++ show (length patterns))
  unifyTypes context expected result
  foldM
    (\(seen, environment) (nested, fieldType) ->
      inferPattern context seen environment nested fieldType)
    (bound, locals) $ zip patterns fields

inferCase
  :: TypeContext
  -> Locals
  -> HSE.Exp HSELocation.SrcSpanInfo
  -> [HSE.Alt HSELocation.SrcSpanInfo]
  -> Infer HsType
inferCase context locals scrutinee alternatives = do
  scrutineeType <- inferExpression context locals scrutinee
  inferAlternatives context locals scrutineeType alternatives

inferAlternatives
  :: TypeContext
  -> Locals
  -> HsType
  -> [HSE.Alt HSELocation.SrcSpanInfo]
  -> Infer HsType
inferAlternatives context locals scrutineeType alternatives = do
  when (null alternatives) $ unsupported context "empty case expressions"
  resultType <- freshType
  mapM_ (inferAlternative resultType) alternatives
  zonk resultType
 where
  inferAlternative resultType (HSE.Alt _ pattern rhs bindings) = do
    when (hasBindings bindings) $ unsupported context
      "where declarations on case alternatives"
    (_, scoped) <- inferPattern context Set.empty locals pattern scrutineeType
    branchType <- case rhs of
      HSE.UnGuardedRhs _ expression -> inferExpression context scoped expression
      HSE.GuardedRhss{} -> unsupported context "guarded case alternatives"
    unifyTypes context resultType branchType
  hasBindings Nothing = False
  hasBindings (Just _) = True

inferEnumeration
  :: TypeContext
  -> Locals
  -> [HSE.Exp HSELocation.SrcSpanInfo]
  -> Infer HsType
inferEnumeration context locals arguments = do
  elementType <- freshType
  -- Enumeration syntax is wired to Enum in ordinary Haskell.  Inferring it
  -- directly avoids making the result depend on whether the environment chose
  -- to spell or export the individual enumFrom family of methods.
  addUnaryConstraint context "Prelude.Enum" elementType
  mapM_ (inferAndUnify context locals elementType) arguments
  pure $ listType elementType

inferAndUnify
  :: TypeContext
  -> Locals
  -> HsType
  -> HSE.Exp HSELocation.SrcSpanInfo
  -> Infer ()
inferAndUnify context locals expected expression =
  inferExpression context locals expression >>= unifyTypes context expected

applyTypes :: TypeContext -> HsType -> [HsType] -> Infer HsType
applyTypes context = foldM applyOne
 where
  applyOne functionType argumentType = do
    resultType <- freshType
    unifyTypes context functionType $ FunctionType argumentType resultType
    zonk resultType

instantiateTerm
  :: TypeContext
  -> HSE.QName location
  -> Infer HsType
instantiateTerm context name = lift (lookupTermScheme context name)
  >>= instantiateScheme

lookupTermScheme
  :: TypeContext
  -> HSE.QName location
  -> Either Diagnostic HsType
lookupTermScheme context source = do
  name <- firstName context $ convertQName source
  case intrinsicScheme name of
    Just scheme -> Right scheme
    Nothing -> do
      resolved <- scopeResolvedTerm context name
      maybe
        (Left $ typeDiagnostic context "DJEX_REPL_TYPE_SCOPE"
          "term has no loaded signature" $ show resolved)
        Right $ Map.lookup resolved $ contextTermSchemes context

scopeResolvedTerm :: TypeContext -> Name -> Either Diagnostic Name
scopeResolvedTerm context name = case resolveScopeNameAmong
    ValueScope (contextTermNames context) (contextScope context) name of
  Left failure -> Left $ typeDiagnostic context "DJEX_REPL_TYPE_SCOPE"
    "expression name is not available" failure
  Right resolved -> Right resolved

instantiateScheme :: HsType -> Infer HsType
instantiateScheme scheme = do
  let (binders, constraints, body) = SharedType.splitLeadingForalls scheme
      variables = Set.toAscList $ Set.unions
        [ Set.fromList binders
        , SharedType.freeVariables body
        , foldMap (foldMap SharedType.freeVariables) constraints
        ]
      flexible = Set.toAscList $ Set.fromList
        [ identifier
        | FlexibleVariable identifier <- variables
        ]
  replacements <- mapM (const freshType) flexible
  let substitutions = IntMap.fromList $ zip flexible replacements
      instantiateType = snd . applySubsts substitutions
  modify' $ \state -> state
    { inferConstraints = inferConstraints state ++ map
        (snd . constraintApplySubsts substitutions) constraints
    }
  pure $ instantiateType body

freshType :: Infer HsType
freshType = do
  state <- get
  case inferNextVariable state of
    Nothing -> lift $ Left $ contextualDiagnostic Error
      "DJEX_REPL_TYPE_SUPPLY" "type-variable supply is exhausted"
      "cannot allocate another variable"
    Just identifier -> do
      modify' $ \current -> current
        { inferNextVariable = if identifier == maxBound
            then Nothing
            else Just $ identifier + 1
        }
      pure $ TypeVariable $ FlexibleVariable identifier

unifyTypes :: TypeContext -> HsType -> HsType -> Infer ()
unifyTypes context rawLeft rawRight = do
  left <- zonk rawLeft
  right <- zonk rawRight
  case SharedType.firstForallType left of
    Just quantified -> unsupportedType quantified
    Nothing -> pure ()
  case SharedType.firstForallType right of
    Just quantified -> unsupportedType quantified
    Nothing -> pure ()
  case infiniteEquation left right of
    Just (variable, typeExpression) -> typeFailure context
      "DJEX_REPL_TYPE_INFINITE" "expression requires an infinite type"
      (defaultVariableName (FlexibleVariable variable) ++ " occurs in "
        ++ renderRawType typeExpression)
    Nothing -> case unifyShared left right of
      Just substitutions -> installSubstitutions substitutions
      Nothing -> case
          ( ExferenceSession.elaborateSessionGoal
              (contextSession context) left
          , ExferenceSession.elaborateSessionGoal
              (contextSession context) right
          ) of
        (Right expandedLeft, Right expandedRight) ->
          case unifyShared expandedLeft expandedRight of
            Just substitutions -> installSubstitutions substitutions
            Nothing -> mismatch left right
        _ -> mismatch left right
 where
  unsupportedType quantified = typeFailure context
    "DJEX_REPL_TYPE_RANK" "higher-rank application is not supported"
    $ renderRawType quantified
  mismatch left right = typeFailure context "DJEX_REPL_TYPE_MISMATCH"
    "expression types do not match"
    $ renderRawType left ++ " versus " ++ renderRawType right

infiniteEquation :: HsType -> HsType -> Maybe (Int, HsType)
infiniteEquation left right = case (left, right) of
  (TypeVariable (FlexibleVariable variable), typeExpression)
    | TypeVariable (FlexibleVariable variable) /= typeExpression
    , FlexibleVariable variable `Set.member`
        SharedType.freeVariables typeExpression -> Just (variable, typeExpression)
  (typeExpression, TypeVariable (FlexibleVariable variable))
    | TypeVariable (FlexibleVariable variable) /= typeExpression
    , FlexibleVariable variable `Set.member`
        SharedType.freeVariables typeExpression -> Just (variable, typeExpression)
  _ -> Nothing

installSubstitutions :: IntMap.IntMap HsType -> Infer ()
installSubstitutions incoming = modify' $ \state ->
  let applyIncoming = snd . applySubsts incoming
      existing = IntMap.map applyIncoming $ inferSubstitutions state
  in state
    { inferSubstitutions = incoming `IntMap.union` existing }

zonk :: HsType -> Infer HsType
zonk typeExpression = do
  substitutions <- gets inferSubstitutions
  let applied = SharedType.canonicalizeType
        $ snd $ applySubsts substitutions typeExpression
  if applied == typeExpression then pure applied else zonk applied

zonkConstraint :: Constraint HsType -> Infer (Constraint HsType)
zonkConstraint constraint = do
  substitutions <- gets inferSubstitutions
  pure $ fmap SharedType.canonicalizeType
    $ snd $ constraintApplySubsts substitutions constraint

finalizeInference
  :: TypeContext
  -> TypeDefaulting
  -> HsType
  -> Infer InferredExpression
finalizeInference context defaulting inferred = do
  result <- zonk inferred
  constraints <- gets inferConstraints >>= mapM zonkConstraint
  applyDefaulting context defaulting result $ distinct constraints
  finalResult <- zonk result
  finalConstraints <- gets inferConstraints >>= mapM zonkConstraint
  let canonicalConstraints = distinct finalConstraints
  residual <- case isPossible (contextClasses context) canonicalConstraints of
    Nothing -> typeFailure context "DJEX_REPL_TYPE_CONSTRAINT"
      "expression has an unsatisfied ground constraint"
      $ intercalate ", " $ map renderRawConstraint canonicalConstraints
    Just remaining -> pure remaining
  let simplified = minimizeConstraints context $ distinct residual
  ensureUnambiguous context finalResult simplified
  lift $ normalizeResult context simplified finalResult

ensureUnambiguous
  :: TypeContext
  -> HsType
  -> [Constraint HsType]
  -> Infer ()
ensureUnambiguous context result constraints = unless (Set.null ambiguous)
  $ typeFailure context "DJEX_REPL_TYPE_AMBIGUOUS"
      "expression has unresolved ambiguous constraints"
      $ intercalate ", " $ map defaultVariableName $ Set.toAscList ambiguous
 where
  resultVariables = SharedType.freeVariables result
  constraintVariables = foldMap (foldMap SharedType.freeVariables) constraints
  ambiguous = constraintVariables `Set.difference` resultVariables

-- Drop constraints already supplied transitively by another retained class
-- obligation.  The solver intentionally preserves variable constraints, so
-- principal REPL types need this small presentation-level entailment pass.
minimizeConstraints
  :: TypeContext
  -> [Constraint HsType]
  -> [Constraint HsType]
minimizeConstraints context constraints =
  [ constraint
  | (prefix, constraint : suffix) <- splits constraints
  , let assumptions = mkQueryClassEnv classes $ prefix ++ suffix
  , constraint `Set.notMember` qClassEnv_inflatedConstraints assumptions
  ]
 where
  classes = qClassEnv_env $ contextClasses context
  splits values = zip (inits values) (tails values)

applyDefaulting
  :: TypeContext
  -> TypeDefaulting
  -> HsType
  -> [Constraint HsType]
  -> Infer ()
applyDefaulting context mode result constraints = mapM_ defaultVariable
  candidates
 where
  resultVariables = SharedType.freeVariables result
  constraintVariables = Set.toAscList
    $ foldMap (foldMap SharedType.freeVariables) constraints
  candidates = distinct
    [ variable
    | variable@(FlexibleVariable _) <- constraintVariables
    , mode == DefaultTypeVariables || variable `Set.notMember` resultVariables
    , defaultable variable
    ]
  relevant variable = filter
    (Set.member variable . foldMap SharedType.freeVariables)
    constraints
  defaultable variable = case relevant variable of
    [] -> False
    obligations -> any numericConstraint obligations
      && all (allowedConstraint variable) obligations
  numericConstraint = (`Set.member` numericClasses)
    . renderCanonical . constraintClass
  allowedConstraint variable constraint =
    constraintArguments constraint == [TypeVariable variable]
      && renderCanonical (constraintClass constraint)
          `Set.member` defaultingClasses
  defaultVariable (FlexibleVariable identifier) = do
    let obligations = relevant $ FlexibleVariable identifier
    replacement <- firstSatisfying identifier obligations defaultTypes
    case replacement of
      Nothing -> pure ()
      Just selected -> installSubstitutions
        $ IntMap.singleton identifier selected
  defaultVariable RigidVariable{} = pure ()
  firstSatisfying _ _ [] = pure Nothing
  firstSatisfying identifier obligations (source : remaining) = do
    candidate <- knownType context source
    let substitutions = IntMap.singleton identifier candidate
        instantiate = snd . constraintApplySubsts substitutions
    if isPossible (contextClasses context) (map instantiate obligations)
        == Just []
      then pure $ Just candidate
      else firstSatisfying identifier obligations remaining

defaultTypes :: [String]
defaultTypes = ["Prelude.Integer", "Prelude.Double"]

numericClasses, defaultingClasses :: Set String
numericClasses = Set.fromList
  [ "Prelude.Num"
  , "Prelude.Real"
  , "Prelude.Integral"
  , "Prelude.Fractional"
  , "Prelude.Floating"
  , "Prelude.RealFrac"
  , "Prelude.RealFloat"
  ]
defaultingClasses = numericClasses `Set.union` Set.fromList
  [ "Data.Eq.Eq"
  , "Data.Ord.Ord"
  , "Text.Show.Show"
  , "Text.Read.Read"
  , "Prelude.Enum"
  , "Prelude.Bounded"
  ]

normalizeResult
  :: TypeContext
  -> [Constraint HsType]
  -> HsType
  -> Either Diagnostic InferredExpression
normalizeResult context constraints result = case applySubstsChecked renaming
    $ ForallType [] constraints result of
  Left failure -> Left $ typeDiagnostic context "DJEX_REPL_TYPE_NORMALIZE"
    "cannot normalize inferred type variables" $ show failure
  Right (_, substituted) -> case normalizeBinders substituted of
    Left failure -> Left $ typeDiagnostic context "DJEX_REPL_TYPE_NORMALIZE"
      "cannot normalize inferred type binders" $ show failure
    Right (ForallType [] renamedConstraints renamedResult, _) -> Right
      InferredExpression
        { inferredExpressionSource = trim $ contextSourceText context
        , inferredExpressionConstraints = renamedConstraints
        , inferredExpressionType = renamedResult
        }
    Right _ -> invalidEnvelope
 where
  observed = SharedType.freeVariablesInFirstOccurrenceOrder result
    ++ concatMap constraintVariables constraints
  identifiers = distinct
    [ identifier
    | FlexibleVariable identifier <- observed
    ]
  renaming = IntMap.fromList
    [ (source, TypeVariable $ FlexibleVariable destination)
    | (source, destination) <- zip identifiers [1 ..]
    ]
  constraintVariables = concatMap
    SharedType.freeVariablesInFirstOccurrenceOrder . constraintArguments
  -- ID zero renders as the legacy @v0@ fallback. Reserving it keeps freshly
  -- alpha-renamed binders in the same readable @a@, @b@, ... namespace as the
  -- free result variables normalized above.
  normalizeBinders = SharedType.uniquifyTypeBinders rejectBinder
    freshSynthesisVariable $ Set.singleton $ FlexibleVariable 0
  rejectBinder :: Variable Int -> Maybe ()
  rejectBinder _ = Nothing
  invalidEnvelope = Left $ typeDiagnostic context
    "DJEX_REPL_TYPE_NORMALIZE" "cannot normalize inferred type variables"
    "capture-avoiding substitution changed the result envelope"

renderInferredType :: Qualification -> InferredExpression -> String
renderInferredType qualification result = contextPrefix
  ++ renderTypeWithQualification qualification defaultVariableName
      (inferredExpressionType result)
 where
  renderedConstraints = map
    (renderConstraintWithQualification qualification defaultVariableName)
    $ inferredExpressionConstraints result
  contextPrefix = case renderedConstraints of
    [] -> ""
    [constraint] -> constraint ++ " => "
    constraints -> "(" ++ intercalate ", " constraints ++ ") => "

parseGroundAnnotation
  :: TypeContext
  -> HSE.Type HSELocation.SrcSpanInfo
  -> Infer HsType
parseGroundAnnotation context signature = do
  scheme <- lift $ parseAnnotationScheme context signature
  let (binders, constraints, body) = SharedType.splitLeadingForalls scheme
      variables = Set.unions
        $ SharedType.freeVariables body
        : map (foldMap SharedType.freeVariables) constraints
  unless (null binders && null constraints && Set.null variables
      && not (SharedType.containsForall body))
    $ typeFailure context "DJEX_REPL_TYPE_ANNOTATION_UNSUPPORTED"
        "polymorphic type annotations are not supported"
        $ HSEPretty.prettyPrint signature
  case ExferenceSession.elaborateSessionGoal (contextSession context) body of
    Left failure -> typeFailure context "DJEX_REPL_TYPE_ANNOTATION"
      "invalid ground type annotation" $ show failure
    Right _ -> pure ()
  pure body

parseAnnotationScheme
  :: TypeContext
  -> HSE.Type HSELocation.SrcSpanInfo
  -> Either Diagnostic HsType
parseAnnotationScheme context signature = do
  request <- firstAnnotation $ parseExferenceRequestWithCheckedTargetInScope
    (contextSession context)
    defaultExferenceOptions
    (contextTarget context)
    (queryScope $ contextScope context)
    (contextSourceName context)
    (HSEPretty.prettyPrint signature)
  pure $ requestGoal $ exferenceRequestQuery request
 where
  firstAnnotation = either
    (Left . withContext "while checking an expression type signature"
      . withCode "DJEX_REPL_TYPE_ANNOTATION")
    Right

queryScope :: ReplScope -> ExferenceQueryScope
queryScope scope = ExferenceQueryScope
  { exferenceQueryCurrentModule = scopeCurrentModule scope
  , exferenceQueryVisibleNames = scopeUnqualifiedTypeNames scope
  , exferenceQueryModuleAliases = scopeQualifierAliases scope
  , exferenceQueryQualifiedNames = scopeQualifiedTypeNames scope
  }

addUnaryConstraint :: TypeContext -> String -> HsType -> Infer ()
addUnaryConstraint context classSource argument = do
  className <- liftName context $ parseName classSource
  modify' $ \state -> state
    { inferConstraints = inferConstraints state
        ++ [Constraint className [argument]] }

knownType :: TypeContext -> String -> Infer HsType
knownType context source = TypeConstructor
  <$> liftName context (parseName source)

liftName :: TypeContext -> Either NameError Name -> Infer Name
liftName context = either
  (typeFailure context "DJEX_REPL_TYPE_INTERNAL"
    "invalid built-in type-inference name" . renderNameError)
  pure

firstName
  :: TypeContext
  -> Either NameError Name
  -> Either Diagnostic Name
firstName context = either
  (Left . typeDiagnostic context "DJEX_REPL_TYPE_NAME"
    "invalid expression name" . renderNameError)
  Right

convertQName :: HSE.QName location -> Either NameError Name
convertQName source = case source of
  HSE.UnQual _ name -> convertOrdinaryName Nothing name
  HSE.Qual _ (HSE.ModuleName _ moduleSource) name -> do
    qualifier <- mkModuleName moduleSource
    convertOrdinaryName (Just qualifier) name
  HSE.Special _ special -> convertSpecialName special

convertOrdinaryName
  :: Maybe ModuleName
  -> HSE.Name location
  -> Either NameError Name
convertOrdinaryName qualifier source = case (qualifier, source) of
  (Nothing, HSE.Ident _ spelling) -> mkIdentifier spelling
  (Nothing, HSE.Symbol _ spelling) -> mkOperator spelling
  (Just moduleName, HSE.Ident _ spelling) ->
    mkQualifiedIdentifier moduleName spelling
  (Just moduleName, HSE.Symbol _ spelling) ->
    mkQualifiedOperator moduleName spelling

convertSpecialName :: HSE.SpecialCon location -> Either NameError Name
convertSpecialName special = case special of
  HSE.UnitCon _ -> tupleName Boxed 0
  HSE.ListCon _ -> Right listName
  HSE.FunCon _ -> specialName FunctionConstructor
  HSE.TupleCon _ boxity arity -> tupleName (convertBoxity boxity) arity
  HSE.Cons _ -> specialName ConsConstructor
  HSE.UnboxedSingleCon _ -> tupleName Unboxed 1
  HSE.ExprHole _ -> mkIdentifier "_"

convertBoxity :: HSE.Boxed -> Boxity
convertBoxity HSE.Boxed = Boxed
convertBoxity HSE.Unboxed = Unboxed

operatorName :: HSE.QOp location -> HSE.QName location
operatorName operator = case operator of
  HSE.QVarOp _ name -> name
  HSE.QConOp _ name -> name

localIdentifier :: HSE.QName location -> Maybe String
localIdentifier name = case name of
  HSE.UnQual _ ordinary -> ordinaryNameSpelling ordinary
  _ -> Nothing

ordinaryNameSpelling :: HSE.Name location -> Maybe String
ordinaryNameSpelling name = case name of
  HSE.Ident _ spelling -> Just spelling
  HSE.Symbol _ spelling -> Just spelling

expressionHole :: HSE.QName location -> Bool
expressionHole name = case name of
  HSE.UnQual _ (HSE.Ident _ "_") -> True
  HSE.Special _ HSE.ExprHole{} -> True
  _ -> False

intrinsicScheme :: Name -> Maybe HsType
intrinsicScheme name = case nameSpecial name of
  Just ListConstructor -> Just $ quantified [1]
    $ listType $ typeVariable 1
  Just ConsConstructor -> Just $ quantified [1]
    $ SharedType.functionType
        [typeVariable 1, listType $ typeVariable 1]
        (listType $ typeVariable 1)
  Just (TupleConstructor boxity arity) ->
    let identifiers = [1 .. arity]
        variables = map typeVariable identifiers
    in Just $ quantified identifiers
      $ SharedType.functionType variables $ TupleType boxity variables
  _ -> Nothing
 where
  typeVariable = TypeVariable . FlexibleVariable
  quantified identifiers body =
    ForallType (map FlexibleVariable identifiers) [] body

listType :: HsType -> HsType
listType = TypeApplication $ TypeConstructor listName

numericLiteral :: HSE.Literal location -> Bool
numericLiteral literal = case literal of
  HSE.Int{} -> True
  HSE.Frac{} -> True
  _ -> False

unsafeInfixOperand :: HSE.Exp location -> Bool
unsafeInfixOperand expression = case expression of
  HSE.InfixApp{} -> True
  HSE.NegApp{} -> True
  _ -> False

unparenthesizedPatternInfix :: HSE.Pat location -> Bool
unparenthesizedPatternInfix patternExpression = case patternExpression of
  HSE.PInfixApp{} -> True
  _ -> False

ensureTupleArity :: TypeContext -> Boxity -> Int -> Infer ()
ensureTupleArity context boxity arity
  | arity <= maximumTupleArity && (boxity == Unboxed || arity /= 1) = pure ()
  | otherwise = typeFailure context "DJEX_REPL_TYPE_TUPLE"
      "unsupported tuple arity"
      $ show boxity ++ " tuple of arity " ++ show arity
        ++ "; maximum supported arity is " ++ show maximumTupleArity

distinct :: Ord value => [value] -> [value]
distinct = SharedCollection.distinctOn id

unsupported :: TypeContext -> String -> Infer value
unsupported context feature = typeFailure context
  "DJEX_REPL_TYPE_UNSUPPORTED" "unsupported Haskell expression form" feature

typeFailure :: TypeContext -> String -> String -> String -> Infer value
typeFailure context code message detail = lift $ Left
  $ typeDiagnostic context code message detail

typeDiagnostic :: TypeContext -> String -> String -> String -> Diagnostic
typeDiagnostic context code message detail =
  withSourceLocation
    (sourceTextLocation (contextSourceName context) $ contextSourceText context)
  $ contextualDiagnostic Error code message detail

renderRawType :: HsType -> String
renderRawType = renderTypeWithQualification
  FullyQualified defaultVariableName

renderRawConstraint :: Constraint HsType -> String
renderRawConstraint = renderConstraintWithQualification
  FullyQualified defaultVariableName
