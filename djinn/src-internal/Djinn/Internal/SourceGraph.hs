{-# LANGUAGE FlexibleContexts #-}

-- | Independent source typing of the exact clause retained with a checked
-- proof and its lowering history. Logical formula atoms are never consulted
-- as source types. Every global and constructor is looked up in the original
-- prepared inventory, and every inferred instantiation is recorded explicitly.
module Djinn.Internal.SourceGraph
  ( SourceGraphError(..)
  , checkSourceClauseGraph
  ) where

import Control.Monad (foldM, unless, when, zipWithM, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT, evalStateT, get, gets, modify, runStateT )
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)

import Djinn.Internal.SourceTypingContext
import Djinn.Internal.Environment (preparedEnvironmentInventory)
import Djinn.Internal.SourceGraphKinds
  ( validateSourceGraphKinds, inferSourceGraphMetavariableKinds )
import qualified Language.Haskell.Synthesis.Declaration as D
import qualified Language.Haskell.Synthesis.Environment as E
import qualified Language.Haskell.Synthesis.Generated as G
import qualified Language.Haskell.Synthesis.Inventory as I
import qualified Language.Haskell.Synthesis.Kind as K
import qualified Language.Haskell.Synthesis.KindInference as KI
import Language.Haskell.Synthesis.Name (Name, Boxity(Boxed))
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

type Variable = T.Variable String
type Type = T.Type Variable
type Locals = Map.Map String Type

data SourceGraphError
  = SourceGraphTypingFailure String
  | SourceGraphSealingFailure (Q.TermGraphError Type String)
  | SourceGraphProjectionMismatch
  deriving (Eq, Ord, Show)

data CheckState = CheckState
  { checkKey :: Natural
  , checkNext :: Natural
  , checkFuel :: Int
  , checkLevel :: Int
  , checkReserved :: Set.Set Variable
  , checkMetas :: Map.Map Variable Int
  , checkRigids :: Map.Map Variable Int
  , checkSubstitutions :: Map.Map Variable Type
  , checkNodes :: Map.Map Q.TermNodeId (Q.TermNode Type String)
  , checkGlobals :: Map.Map Name Type
  , checkConstructors :: Map.Map Name Type
  , checkEmptyTypes :: Set.Set Name
  }

type Check = StateT CheckState (Either SourceGraphError)

-- Kept package-private for adversarial checker tests. Public graph construction
-- always consumes the whole retained proof/source/clause association in Core.
checkSourceClauseGraph
  :: Natural -> SourceTypingContext -> G.FunctionClause String
  -> Either SourceGraphError (Q.TermGraph Type String)
checkSourceClauseGraph key context clause = do
  rawGlobals <- first SourceGraphTypingFailure $ sourceTypingTermSchemes context
  let globals = Map.map (fmap T.FlexibleVariable) rawGlobals
      declarations = E.environmentDeclarations $ I.inventoryEnvironment $
        preparedEnvironmentInventory $ sourceTypingPreparedEnvironment context
      constructorNames = Set.fromList
        [ D.constructorName constructor
        | D.DataTypeDeclaration _ _ _ declaredConstructors <- declarations
        , constructor <- declaredConstructors
        ]
      constructors = Map.restrictKeys globals constructorNames
      emptyTypes = Set.fromList
        [name | D.DataTypeDeclaration _ name _ [] <- declarations]
      goal = fmap T.FlexibleVariable $ sourceTypingGoal context
      reserved = Set.unions
        [ T.freeVariables ty `Set.union` Set.fromList (T.typeBinderVariables ty)
        | ty <- goal : Map.elems globals
        ]
      initial = CheckState key 0 100000 0 reserved Map.empty Map.empty
        Map.empty Map.empty globals constructors emptyTypes
  first (SourceGraphTypingFailure . show) $ G.validateFunctionClauseScope clause
  (root, state) <- runStateT
    (checkExpression Map.empty (G.functionClauseExpression clause) goal) initial
  (provisional, observedState) <- runStateT
    (mapM (resolveNodeWith zonk) $ Map.toAscList $ checkNodes state) state
  unresolvedKinds <- first (SourceGraphTypingFailure . ("before source type completion: " ++)) $
    inferSourceGraphMetavariableKinds context
      (Map.keys $ checkMetas state `Map.difference` checkSubstitutions state)
      $ Q.TermGraphSource root provisional
  let representatives = closedKindRepresentatives $ I.inventoryKindAssumptions $
        preparedEnvironmentInventory $ sourceTypingPreparedEnvironment context
  (_, completedState) <- runStateT (mapM_ (complete representatives) unresolvedKinds) observedState
  nodes <- evalStateT
    (mapM (resolveNodeWith resolveEvidenceType) $ Map.toAscList $ checkNodes completedState)
    completedState
  let structure = Q.sharedTypeStructure
        { Q.forallTypeStructure = Just Q.sharedForallTypeStructure
        , Q.constructorPatternFieldTypes = constructorFields constructors
        }
      source = Q.TermGraphSource root nodes
  first (SourceGraphTypingFailure . ("completed source graph: " ++)) $
    validateSourceGraphKinds context source
  graph <- first SourceGraphSealingFailure $ Q.sealTermGraph structure
    Q.defaultTermGraphLimits source
  unless (Q.eraseTermGraphToFunctionClause (G.clauseName clause) graph == clause) $
    Left SourceGraphProjectionMismatch
  pure graph
 where
  complete representatives (variable, kind) = case Map.lookup kind representatives of
    Nothing -> failCheck $ "no admitted closed source type for unresolved kind: " ++ show kind
    Just selected -> do
      let limits = Q.defaultTermGraphLimits
      either (failCheck . show) pure $ Q.observeTypeWithin Q.sharedTypeStructure
        (Q.maximumTermGraphTypeNodes limits) (Q.maximumTermGraphCollectionWidth limits) selected
      bindMeta variable selected

-- Type choices only: completing a vacuous instantiation introduces no term
-- provider. Start with unit and the exact admitted constructor kinds, then
-- close under partial application. Every added key is a result-kind subterm
-- of that finite inventory, so this deterministic census reaches a fixed point.
closedKindRepresentatives :: KI.KindAssumptions -> Map.Map KI.GroundKind Type
closedKindRepresentatives assumptions = close initial
 where
  initial = foldl add (Map.singleton K.ProperTypeKind $ T.TupleType Boxed [])
    [(kind, T.TypeConstructor name) | (name, kind) <- Map.toAscList $ KI.typeConstructorKinds assumptions]
  add representatives (kind, ty) = Map.insertWith (\_ original -> original) kind ty representatives
  close representatives =
    let applications =
          [ (result, T.TypeApplication function argument)
          | (K.FunctionKind domain result, function) <- Map.toAscList representatives
          , Just argument <- [Map.lookup domain representatives]
          ]
        extended = foldl add representatives applications
    in if Map.size extended == Map.size representatives then extended else close extended

failCheck :: String -> Check a
failCheck = lift . Left . SourceGraphTypingFailure

tick :: Check ()
tick = do
  fuel <- gets checkFuel
  when (fuel <= 0) $ failCheck "source typing work limit exceeded"
  modify $ \s -> s { checkFuel = fuel - 1 }

freshCoordinate :: Check Natural
freshCoordinate = do
  tick
  s <- get
  let n = checkNext s
      k = checkKey s
      coordinate = (k + n) * (k + n + 1) `div` 2 + n
  modify $ \state -> state { checkNext = n + 1 }
  pure coordinate

freshVariable :: Bool -> Check Type
freshVariable rigid = do
  coordinate <- freshCoordinate
  let variable = (if rigid then T.RigidVariable else T.FlexibleVariable)
        ("$djinn$source$" ++ show coordinate)
  used <- gets checkReserved
  if variable `Set.member` used then freshVariable rigid else do
    level <- gets checkLevel
    modify $ \s -> s
      { checkReserved = Set.insert variable $ checkReserved s
      , checkMetas = if rigid then checkMetas s else Map.insert variable level $ checkMetas s
      , checkRigids = if rigid then Map.insert variable level $ checkRigids s else checkRigids s
      }
    pure $ T.TypeVariable variable

freshOccurrence :: Check Q.OccurrenceId
freshOccurrence = Q.occurrenceId <$> freshCoordinate

emit :: Type -> Q.TermNodeForm Type String -> Check Q.TermNodeId
emit ty form = do
  count <- gets $ Map.size . checkNodes
  when (count >= Q.maximumTermGraphNodes Q.defaultTermGraphLimits) $
    failCheck "source graph node limit exceeded"
  node <- Q.termNodeId <$> freshCoordinate
  modify $ \s -> s { checkNodes = Map.insert node (Q.TermNode ty form) $ checkNodes s }
  pure node

nodeType :: Q.TermNodeId -> Check Type
nodeType node = do
  found <- gets $ Map.lookup node . checkNodes
  maybe (failCheck "missing source typing node") (zonk . Q.termNodeType) found

freshBinder :: T.FreshVariableAllocator Variable
freshBinder reserved variable = Just $ choose $ fmap (++ "'") variable
 where
  choose candidate
    | candidate `Set.member` reserved = choose $ fmap (++ "'") candidate
    | otherwise = candidate

substitute :: Map.Map Variable Type -> Type -> Either SourceGraphError Type
substitute substitutions = first (SourceGraphTypingFailure . show)
  . T.substituteTypeVariables freshBinder Set.empty substitutions

zonk :: Type -> Check Type
zonk ty = do
  tick
  substitutions <- gets checkSubstitutions
  let relevant = Map.restrictKeys substitutions $ T.freeVariables ty
  resolved <- mapM zonk relevant
  either liftFailure (pure . eraseEmptyForalls . T.canonicalizeType) $ substitute resolved ty
 where
  liftFailure = lift . Left

-- Empty, context-free wrappers have no binder or dictionary to introduce.
-- Keep every nonempty binder/context layer and all nested type structure.
eraseEmptyForalls :: Type -> Type
eraseEmptyForalls ty = case ty of
  T.ForallType [] [] body -> eraseEmptyForalls body
  T.ForallType binders constraints body -> T.ForallType binders
    (map (fmap eraseEmptyForalls) constraints) $ eraseEmptyForalls body
  T.TypeApplication function argument -> T.TypeApplication
    (eraseEmptyForalls function) $ eraseEmptyForalls argument
  T.FunctionType domain result -> T.FunctionType
    (eraseEmptyForalls domain) $ eraseEmptyForalls result
  T.TupleType box fields -> T.TupleType box $ map eraseEmptyForalls fields
  other -> other

bindMeta :: Variable -> Type -> Check ()
bindMeta variable ty = do
  resolved <- zonk ty
  unless (resolved == T.TypeVariable variable) $ do
    when (variable `Set.member` T.freeVariables resolved) $
      failCheck "occurs check failed in source typing"
    s <- get
    level <- maybe (failCheck "attempted to solve a source rigid identity") pure
      $ Map.lookup variable $ checkMetas s
    let free = T.freeVariables resolved
    when (any (> level) $ Map.elems $ Map.restrictKeys (checkRigids s) free) $
      failCheck "source type argument escapes its forall scope"
    modify $ \state -> state
      { checkSubstitutions = Map.insert variable resolved $ checkSubstitutions state
      , checkMetas = Map.mapWithKey
          (\key depth -> if key `Set.member` free then min level depth else depth)
          $ checkMetas state
      }

unify :: Type -> Type -> Check ()
unify left right = do
  tick
  l <- zonk left
  r <- zonk right
  if A.alphaEquivalentTypes l r then pure () else do
    metas <- gets checkMetas
    case (l, r) of
      (T.TypeVariable variable, _) | Map.member variable metas -> bindMeta variable r
      (_, T.TypeVariable variable) | Map.member variable metas -> bindMeta variable l
      (T.FunctionType a b, T.FunctionType c d) -> unify a c >> unify b d
      (T.TypeApplication a b, T.TypeApplication c d) -> unify a c >> unify b d
      (T.TupleType box xs, T.TupleType box' ys)
        | box == box', length xs == length ys -> zipWithM_ unify xs ys
      (T.ForallType{}, T.ForallType{}) -> underLevel $ do
        rigid <- freshVariable True
        openedLeft <- consumeForall l rigid
        openedRight <- consumeForall r rigid
        unify openedLeft openedRight
      _ -> failCheck $ "source type mismatch: " ++ show l ++ " /= " ++ show r

underLevel :: Check a -> Check a
underLevel action = do
  previous <- gets checkLevel
  modify $ \s -> s { checkLevel = previous + 1 }
  result <- action
  modify $ \s -> s { checkLevel = previous }
  pure result

consumeForall :: Type -> Type -> Check Type
consumeForall source selected = case source of
  T.ForallType (binder : rest) [] body ->
    either (lift . Left) (pure . T.canonicalizeType) $ substitute
      (Map.singleton binder selected)
      (if null rest then body else T.ForallType rest [] body)
  T.ForallType _ (_ : _) _ -> failCheck "source forall requires dictionary evidence"
  _ -> failCheck "source type application does not consume a forall binder"

instantiate :: Q.TermNodeId -> Check Q.TermNodeId
instantiate node = do
  ty <- nodeType node
  case ty of
    T.ForallType{} -> do
      selected <- freshVariable False
      result <- consumeForall ty selected
      occurrence <- freshOccurrence
      instantiated <- emit result $ Q.TypedImplicitTypeApplication occurrence node
        $ Q.ImplicitTypeApplicationWitness ty selected result
      instantiate instantiated
    _ -> pure node

functionParts :: Type -> Check (Type, Type)
functionParts source = do
  ty <- zonk source
  case ty of
    T.FunctionType domain result -> pure (domain, result)
    _ -> do
      domain <- freshVariable False
      result <- freshVariable False
      unify ty $ T.FunctionType domain result
      pure (domain, result)

checkExpression :: Locals -> G.Expression String -> Type -> Check Q.TermNodeId
checkExpression locals expression expected = do
  tick
  ty <- zonk expected
  case ty of
    T.ForallType{} -> underLevel $ do
      rigid <- freshVariable True
      opened <- consumeForall ty rigid
      body <- checkExpression locals expression opened
      occurrence <- freshOccurrence
      emit ty $ Q.TypedForallIntroduction occurrence body
        $ Q.ForallIntroductionWitness ty rigid opened
    _ -> case expression of
      G.Apply{} -> checkApplication locals expression $ Just ty
      G.VisibleTypeApplication{} -> checkApplication locals expression $ Just ty
      G.Lambda patterns body -> do
        (typedPatterns, bodyType, bodyLocals, remaining) <- checkParameters locals ty patterns
        checkedBody <- checkExpression bodyLocals (G.lambdaExpression remaining body) bodyType
        emit ty $ Q.TypedLambda typedPatterns checkedBody
      G.Tuple elements -> do
        fields <- case ty of
          T.TupleType Boxed fields | length fields == length elements -> pure fields
          _ -> do
            fields <- mapM (const $ freshVariable False) elements
            unify ty $ T.TupleType Boxed fields
            pure fields
        checked <- zipWithM (checkExpression locals) elements fields
        emit ty $ Q.TypedTuple checked
      G.Let pattern' value body -> do
        checkedValue <- inferLetValue locals ty pattern' value
        valueType <- nodeType checkedValue
        (typedPattern, bodyLocals) <- checkPattern locals pattern' valueType
        checkedBody <- checkExpression bodyLocals body ty
        emit ty $ Q.TypedLet typedPattern checkedValue checkedBody
      G.Case scrutinee alternatives -> do
        checkedScrutinee <- inferExpression locals scrutinee >>= instantiate
        scrutineeType <- nodeType checkedScrutinee
        when (null alternatives) $ requireEmptyType scrutineeType
        checkedAlternatives <- mapM (checkAlternative locals scrutineeType ty) alternatives
        emit ty $ Q.TypedCase checkedScrutinee checkedAlternatives
      G.Hole{} -> failCheck "source candidate contains a hole"
      _ -> do
        inferred <- inferExpression locals expression >>= instantiate
        actual <- nodeType inferred
        unify actual ty
        pure inferred

-- Generalize only identities created by this checker and unconstrained by
-- the surrounding environment or result. Recheck the exact value under the
-- resulting scheme so every generalized variable has a lexical rigid opening
-- in the graph; the speculative inference tree is never retained as evidence.
inferLetValue :: Locals -> Type -> G.Pattern String -> G.Expression String
  -> Check Q.TermNodeId
inferLetValue locals expected pattern' value = do
  previousNodes <- gets checkNodes
  checked <- underLevel $ inferExpression locals value
  inferred <- nodeType checked
  surrounding <- mapM zonk $ expected : Map.elems locals
  metas <- gets checkMetas
  let eligible = T.freeVariables inferred `Set.intersection` Map.keysSet metas
        `Set.difference` Set.unions (map T.freeVariables surrounding)
  case pattern' of
    G.Bind{} | not (Set.null eligible) -> do
      modify $ \s -> s { checkNodes = previousNodes }
      checkExpression locals value $ T.ForallType (Set.toAscList eligible) [] inferred
    _ -> pure checked

requireEmptyType :: Type -> Check ()
requireEmptyType ty = do
  resolved <- zonk ty
  emptyTypes <- gets checkEmptyTypes
  let nominalHead (T.TypeApplication function _) = nominalHead function
      nominalHead (T.TypeConstructor name) = Just name
      nominalHead _ = Nothing
  unless (maybe False (`Set.member` emptyTypes) $ nominalHead resolved) $
    failCheck "empty case requires an exact empty datatype declaration"

checkParameters
  :: Locals -> Type -> [G.Pattern String]
  -> Check ([Q.TypedPattern Type String], Type, Locals, [G.Pattern String])
checkParameters locals ty [] = pure ([], ty, locals, [])
checkParameters locals ty (pattern' : rest) = do
  tick
  resolved <- zonk ty
  case resolved of
    T.ForallType{} -> pure ([], resolved, locals, pattern' : rest)
    _ -> do
      (domain, result) <- functionParts resolved
      (typedPattern, extended) <- checkPattern locals pattern' domain
      (patterns, finalType, finalLocals, remaining) <- checkParameters extended result rest
      pure (typedPattern : patterns, finalType, finalLocals, remaining)

checkAlternative
  :: Locals -> Type -> Type -> (G.Pattern String, G.Expression String)
  -> Check (Q.TypedPattern Type String, Q.TermNodeId)
checkAlternative locals scrutineeType expected (pattern', branch) = do
  (typedPattern, branchLocals) <- checkPattern locals pattern' scrutineeType
  body <- checkExpression branchLocals branch expected
  pure (typedPattern, body)

inferExpression :: Locals -> G.Expression String -> Check Q.TermNodeId
inferExpression locals expression = do
  tick
  case expression of
    G.Local local -> do
      ty <- maybe (failCheck $ "unbound source local: " ++ local) pure $ Map.lookup local locals
      occurrence <- freshOccurrence
      emit ty $ Q.TypedLocal occurrence local
    G.Global name -> do
      ty <- gets (Map.lookup name . checkGlobals) >>= maybe
        (failCheck $ "global absent from exact source inventory: " ++ show name) pure
      occurrence <- freshOccurrence
      emit ty $ Q.TypedGlobal occurrence name
    G.Apply{} -> checkApplication locals expression Nothing
    G.VisibleTypeApplication{} -> checkApplication locals expression Nothing
    _ -> freshVariable False >>= checkExpression locals expression

data ApplicationStep
  = ValueStep Type Type (G.Expression String)
  | ImplicitStep Type Type Type
  | VisibleStep Type Type Type G.VisibleTypeArgument

-- Infer the complete mixed application telescope before checking any value
-- argument. Expected nominal result arguments can therefore determine every
-- correlated impredicative selection, including an early argument followed
-- by later term and forall applications. No independent Cartesian guessing
-- or monomorphic default is introduced by this check.
checkApplication
  :: Locals -> G.Expression String -> Maybe Type -> Check Q.TermNodeId
checkApplication locals expression expected = do
  let (headExpression, arguments) = G.expressionFullApplicationSpine expression
  headNode <- inferExpression locals headExpression
  source <- nodeType headNode
  (steps, result) <- planArguments source arguments
  (finalSteps, finalType) <- case expected of
    Nothing -> pure ([], result)
    Just wanted -> do
      (tailSteps, instantiated) <- planImplicit result
      unify instantiated wanted
      pure (tailSteps, instantiated)
  _ <- zonk finalType
  foldM buildStep headNode $ steps ++ finalSteps
 where
  planArguments source [] = pure ([], source)
  planArguments source (G.TermArgument argument : rest) = do
    (prefix, monotype) <- planImplicit source
    (domain, result) <- functionParts monotype
    (tailSteps, finalType) <- planArguments result rest
    pure (prefix ++ ValueStep domain result argument : tailSteps, finalType)
  planArguments source (G.VisibleTypeArgumentArgument argument : rest) = do
    resolved <- zonk source
    selected <- selectedVisibleType argument
    result <- consumeForall resolved selected
    (tailSteps, finalType) <- planArguments result rest
    pure (VisibleStep resolved selected result argument : tailSteps, finalType)

  planImplicit source = do
    resolved <- zonk source
    case resolved of
      T.ForallType{} -> do
        selected <- freshVariable False
        result <- consumeForall resolved selected
        (rest, finalType) <- planImplicit result
        pure (ImplicitStep resolved selected result : rest, finalType)
      _ -> pure ([], resolved)

  buildStep function step = case step of
    ValueStep domain result argument -> do
      checkedArgument <- checkExpression locals argument domain
      emit result $ Q.TypedApply function checkedArgument $ Q.ApplicationWitness domain result
    ImplicitStep source selected result -> do
      occurrence <- freshOccurrence
      emit result $ Q.TypedImplicitTypeApplication occurrence function
        $ Q.ImplicitTypeApplicationWitness source selected result
    VisibleStep source selected result argument -> do
      occurrence <- freshOccurrence
      emit result $ Q.TypedVisibleTypeApplication occurrence function argument
        $ Q.TypeApplicationWitness source selected result Nothing

selectedVisibleType :: G.VisibleTypeArgument -> Check Type
selectedVisibleType argument = case G.visibleTypeArgumentPatternType argument of
  Nothing -> freshVariable False
  Just source -> visibleType source
 where
  visibleVariable variable = T.FlexibleVariable $
    "$djinn$visible$" ++ G.closedVisibleTypeVariableSpelling variable
  visibleType ty = case ty of
    T.TypeVariable Nothing -> freshVariable False
    T.TypeVariable (Just variable) -> pure $ T.TypeVariable $ visibleVariable variable
    T.TypeConstructor name -> pure $ T.TypeConstructor name
    T.TypeApplication f x -> T.TypeApplication <$> visibleType f <*> visibleType x
    T.FunctionType a b -> T.FunctionType <$> visibleType a <*> visibleType b
    T.TupleType box fields -> T.TupleType box <$> mapM visibleType fields
    T.ForallType binders [] body -> do
      variables <- mapM (maybe (failCheck "inferred forall binder in visible argument")
        (pure . visibleVariable)) binders
      T.ForallType variables [] <$> visibleType body
    T.ForallType _ (_ : _) _ -> failCheck "visible source argument contains constraints"

checkPattern :: Locals -> G.Pattern String -> Type
  -> Check (Q.TypedPattern Type String, Locals)
checkPattern locals pattern' expected = do
  tick
  occurrence <- freshOccurrence
  (form, extended) <- case pattern' of
    G.Bind local -> pure (Q.TypedBind local, Map.insert local expected locals)
    G.Wildcard -> pure (Q.TypedWildcard, locals)
    G.As local nested -> do
      (checked, extended) <- checkPattern (Map.insert local expected locals) nested expected
      pure (Q.TypedAs local checked, extended)
    G.TuplePattern patterns -> do
      fields <- mapM (const $ freshVariable False) patterns
      unify expected $ T.TupleType Boxed fields
      (checked, extended) <- checkPatternFields locals patterns fields
      pure (Q.TypedTuplePattern checked, extended)
    G.Constructor name patterns -> do
      source <- gets (Map.lookup name . checkConstructors) >>= maybe
        (failCheck $ "constructor absent from exact source inventory: " ++ show name) pure
      -- Pattern instantiation is source checking only, not an emitted term
      -- application: the shared pattern checker independently replays fields
      -- against the exact nominal result after solving these selections.
      instantiated <- instantiateType source
      let (fields, result) = T.functionSpine instantiated
      unless (length fields == length patterns) $ failCheck "source constructor pattern arity mismatch"
      unify expected result
      (checked, extended) <- checkPatternFields locals patterns fields
      pure (Q.TypedConstructor name checked, extended)
  pure (Q.TypedPattern occurrence expected form, extended)

instantiateType :: Type -> Check Type
instantiateType source = do
  resolved <- zonk source
  case resolved of
    T.ForallType{} -> freshVariable False >>= consumeForall resolved >>= instantiateType
    _ -> pure resolved

checkPatternFields :: Locals -> [G.Pattern String] -> [Type]
  -> Check ([Q.TypedPattern Type String], Locals)
checkPatternFields locals patterns types = do
  unless (length patterns == length types) $ failCheck "source pattern field arity mismatch"
  foldM step ([], locals) (zip patterns types)
 where
  step (checked, scope) (pattern', ty) = do
    (next, extended) <- checkPattern scope pattern' ty
    pure (checked ++ [next], extended)

-- This callback never infers a nominal identity from a graph annotation. It
-- matches the exact constructor's retained result template, then substitutes
-- those owner arguments in that same constructor's fields.
constructorFields :: Map.Map Name Type -> Name -> Type -> Maybe [Type]
constructorFields globals name actual = do
  source <- Map.lookup name globals
  let (binders, constraints, body) = T.splitLeadingForalls source
      (fields, result) = T.functionSpine body
  unless (null constraints) Nothing
  selections <- matchTemplate (Set.fromList binders) Map.empty result actual
  either (const Nothing) Just $ mapM (substitute selections) fields

matchTemplate :: Set.Set Variable -> Map.Map Variable Type -> Type -> Type
  -> Maybe (Map.Map Variable Type)
matchTemplate variables selected template actual = case template of
  T.TypeVariable variable | variable `Set.member` variables ->
    case Map.lookup variable selected of
      Nothing -> Just $ Map.insert variable actual selected
      Just previous | A.alphaEquivalentTypes previous actual -> Just selected
      _ -> Nothing
  T.TypeApplication f x -> case actual of
    T.TypeApplication g y -> matchTemplate variables selected f g
      >>= \next -> matchTemplate variables next x y
    _ -> Nothing
  _ | A.alphaEquivalentTypes template actual -> Just selected
    | otherwise -> Nothing

resolvePatternWith :: (Type -> Check Type) -> Q.TypedPattern Type String
  -> Check (Q.TypedPattern Type String)
resolvePatternWith resolve (Q.TypedPattern occurrence ty form) = do
  resolved <- resolve ty
  form' <- case form of
    Q.TypedBind local -> pure $ Q.TypedBind local
    Q.TypedWildcard -> pure Q.TypedWildcard
    Q.TypedAs local nested -> Q.TypedAs local <$> resolvePatternWith resolve nested
    Q.TypedTuplePattern fields -> Q.TypedTuplePattern <$> mapM (resolvePatternWith resolve) fields
    Q.TypedConstructor name fields -> Q.TypedConstructor name <$> mapM (resolvePatternWith resolve) fields
  pure $ Q.TypedPattern occurrence resolved form'

resolveNodeWith :: (Type -> Check Type) -> (Q.TermNodeId, Q.TermNode Type String)
  -> Check (Q.TermNodeId, Q.TermNode Type String)
resolveNodeWith resolve (identity, Q.TermNode ty form) = do
  resolved <- resolve ty
  form' <- case form of
    Q.TypedLambda patterns body -> Q.TypedLambda <$> mapM (resolvePatternWith resolve) patterns <*> pure body
    Q.TypedApply f x (Q.ApplicationWitness domain result) ->
      Q.TypedApply f x <$> (Q.ApplicationWitness <$> resolve domain <*> resolve result)
    Q.TypedVisibleTypeApplication occurrence child argument witness ->
      Q.TypedVisibleTypeApplication occurrence child argument <$> (Q.TypeApplicationWitness
        <$> resolve (Q.typeApplicationSource witness) <*> resolve (Q.typeApplicationSelected witness)
        <*> resolve (Q.typeApplicationResult witness) <*> pure (Q.typeApplicationCertificate witness))
    Q.TypedForallIntroduction occurrence child witness ->
      Q.TypedForallIntroduction occurrence child <$> (Q.ForallIntroductionWitness
        <$> resolve (Q.forallIntroductionSource witness) <*> resolve (Q.forallIntroductionVariable witness)
        <*> resolve (Q.forallIntroductionBody witness))
    Q.TypedImplicitTypeApplication occurrence child witness ->
      Q.TypedImplicitTypeApplication occurrence child <$> (Q.ImplicitTypeApplicationWitness
        <$> resolve (Q.implicitTypeApplicationSource witness) <*> resolve (Q.implicitTypeApplicationSelected witness)
        <*> resolve (Q.implicitTypeApplicationResult witness))
    Q.TypedLet pattern' value body -> Q.TypedLet <$> resolvePatternWith resolve pattern' <*> pure value <*> pure body
    Q.TypedCase scrutinee alternatives -> Q.TypedCase scrutinee <$> mapM
      (\(pattern', body) -> (,) <$> resolvePatternWith resolve pattern' <*> pure body) alternatives
    other -> pure other
  pure (identity, Q.TermNode resolved form')

resolveEvidenceType :: Type -> Check Type
resolveEvidenceType ty = do
  resolved <- zonk ty
  metas <- gets checkMetas
  unless (Set.null $ T.freeVariables resolved `Set.intersection` Map.keysSet metas) $
    failCheck $ "source graph retains an unresolved inference variable: " ++ show resolved
  pure resolved
