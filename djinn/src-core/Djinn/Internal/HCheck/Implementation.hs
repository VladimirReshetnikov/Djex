--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
-- | Implementation of Djinn's compatibility kind checker and the raw-tree
-- worker used by sealed sessions. The historical exposed module deliberately
-- re-exports only its original raw checking surface.
module Djinn.Internal.HCheck.Implementation(
    AbstractTypeDefinitionError(..), normalizeAbstractTypeDefinitionsWith,
    PreparedKindCheck, prepareKindEnvironment,
    htCheckEnv, htCheckType, htCheckTypeKind, htCheckTypesKinds,
    htCheckTypePrepared, htCheckTypesKindsWith,
    htInferClassKinds, htInferClassKindsPrepared
    ) where
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Count (naturalLength)
import qualified Language.Haskell.Synthesis.KindInference as SharedInference
import qualified Language.Haskell.Synthesis.Name as SharedName
import qualified Language.Haskell.Synthesis.Type as SharedType

import Djinn.Internal.HTypes
import Djinn.Internal.Type (toSynthesisType)

-- | Structural failures unique to the historical raw type-definition tuple.
-- An abstract declaration redundantly stores its owner name inside 'HType',
-- and, unlike data types and synonyms, has no parameter list of its own.
data AbstractTypeDefinitionError
    = AbstractTypeDefinitionNameMismatch HSymbol HSymbol
    | AbstractTypeDefinitionHasParameters HSymbol [HSymbol]
    deriving (Eq, Show)

-- | Validate the redundant representation of abstract type definitions and
-- choose how their cached outer kind is refreshed.  Shared-environment
-- conversion installs the embedded declared kind immediately.  The legacy
-- checker keeps its polymorphic cache only until its ordinary whole-table
-- inference replaces every cached kind below.
normalizeAbstractTypeDefinitionsWith
    :: (cachedKind -> HKind -> cachedKind)
    -> [(HSymbol, ([HSymbol], HType, cachedKind))]
    -> Either AbstractTypeDefinitionError
        [(HSymbol, ([HSymbol], HType, cachedKind))]
normalizeAbstractTypeDefinitionsWith refreshKind = mapM normalize
  where
    normalize definition@(outerName, (parameters, body, cachedKind)) =
        case body of
            HTAbstract embeddedName embeddedKind
                | outerName /= embeddedName -> Left $
                    AbstractTypeDefinitionNameMismatch
                        outerName embeddedName
                | not (null parameters) -> Left $
                    AbstractTypeDefinitionHasParameters
                        outerName parameters
                | otherwise -> Right
                    ( outerName
                    , ( []
                      , HTAbstract embeddedName embeddedKind
                      , refreshKind cachedKind embeddedKind
                      )
                    )
            _ -> Right definition

renderAbstractTypeDefinitionError :: AbstractTypeDefinitionError -> String
renderAbstractTypeDefinitionError failure = case failure of
    AbstractTypeDefinitionNameMismatch outerName embeddedName ->
        "Abstract type definition " ++ show outerName ++
        " embeds the conflicting name " ++ show embeddedName
    AbstractTypeDefinitionHasParameters name parameters ->
        "Abstract type definition " ++ show name ++
        " cannot declare parameters: " ++ show parameters

-- | The transient checker used by Djinn's standalone raw compatibility
-- operations and editable-environment validation. It retains only legacy
-- synonym spellings/arities and already-ground shared assumptions, never the
-- declarations from which either was derived, and is not retained by sealed
-- sessions.
data PreparedKindCheck = PreparedKindCheck
    [(HSymbol, Natural)]
    SharedInference.KindAssumptions

-- | Prepare a compatibility kind-checking scope from Djinn declarations.
-- Whole-environment assumption conversion happens exactly once here.
prepareKindCheck
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> Either String PreparedKindCheck
prepareKindCheck definitions = do
    normalized <- first renderAbstractTypeDefinitionError $
        normalizeAbstractTypeDefinitionsWith
            (\_ embeddedKind -> embeddedKind) definitions
    assumptions <- synthesisAssumptions normalized
    return $ prepareKindCheckWithArities
        (synonymArities normalized) assumptions

-- Force the derived list while the raw source declarations are transient. The
-- checker retains names and arities, never deferred access to complete type
-- definitions.
prepareKindCheckWithArities
    :: [(HSymbol, Natural)]
    -> SharedInference.KindAssumptions
    -> PreparedKindCheck
prepareKindCheckWithArities arities assumptions =
    forceSynonymArities arities `seq` PreparedKindCheck arities assumptions

forceSynonymArities :: [(HSymbol, Natural)] -> ()
forceSynonymArities [] = ()
forceSynonymArities ((name, arity) : rest) =
    name `seq` arity `seq` forceSynonymArities rest

htCheckType :: [(HSymbol, ([HSymbol], HType, HKind))] -> HType -> Either String ()
htCheckType its = htCheckTypeKind its KStar

htCheckTypePrepared :: PreparedKindCheck -> HType -> Either String ()
htCheckTypePrepared prepared = htCheckTypeKindPrepared prepared KStar

-- Check that a type is well-kinded and has the given (ground) kind.  Free
-- type variables receive fresh kinds, so a variable argument fits any
-- expected kind while a mis-kinded application is still rejected.
htCheckTypeKind :: [(HSymbol, ([HSymbol], HType, HKind))]
                -> HKind -> HType -> Either String ()
htCheckTypeKind its expected t = htCheckTypesKinds its [(expected, t)]

htCheckTypeKindPrepared :: PreparedKindCheck
                        -> HKind -> HType -> Either String ()
htCheckTypeKindPrepared prepared expected t =
    htCheckTypesKindsPrepared prepared [(expected, t)]

-- Check several types in one kind-inference scope.  This matters whenever
-- free variables are shared between types: checking each type separately
-- could incorrectly assign the same variable a different kind each time.
htCheckTypesKinds :: [(HSymbol, ([HSymbol], HType, HKind))]
                  -> [(HKind, HType)] -> Either String ()
htCheckTypesKinds its expectedTypes = do
    prepared <- prepareKindCheck its
    htCheckTypesKindsPrepared prepared expectedTypes

htCheckTypesKindsPrepared :: PreparedKindCheck
                          -> [(HKind, HType)] -> Either String ()
htCheckTypesKindsPrepared
        (PreparedKindCheck preparedSynonymArities assumptions)
        = htCheckTypesKindsWith
            (checkSynonymApplicationWithArities preparedSynonymArities)
            assumptions

-- | Check a raw compatibility batch with caller-supplied synonym facts and
-- kind assumptions. Sealed sessions supply their exact witness; the transient
-- compatibility checker supplies its private arity cache. Saturation still
-- traverses the original 'HType' tree before any structural conversion,
-- preserving the historical first-failure order.
htCheckTypesKindsWith
    :: (HSymbol -> Natural -> Either String ())
    -> SharedInference.KindAssumptions
    -> [(HKind, HType)]
    -> Either String ()
htCheckTypesKindsWith checkSynonymApplication assumptions expectedTypes = do
    mapM_ (checkSynonymSaturationWith checkSynonymApplication . snd)
        expectedTypes
    obligations <- mapM convertObligation expectedTypes
    checkSynthesisObligations assumptions obligations
  where
    convertObligation (expected, typeExpression) = (,)
        <$> checkedGroundHKind expected
        <*> first show (toSynthesisType typeExpression)

checkSynthesisObligations
    :: SharedInference.KindAssumptions
    -> [(SharedInference.GroundKind, SharedType.Type HSymbol)]
    -> Either String ()
checkSynthesisObligations assumptions =
    first show . SharedInference.checkTypesKinds assumptions

-- Infer the kind of every class parameter from the class's method types,
-- as Haskell98 does.  Parameters left unconstrained (including every
-- parameter of a method-less class) default to *.  Only class parameters
-- share kind variables across methods: every other variable is implicitly
-- quantified by its individual method signature.
htInferClassKinds :: [(HSymbol, ([HSymbol], HType, HKind))]
                  -> [HSymbol] -> [HType]
                  -> Either String [(HSymbol, HKind)]
htInferClassKinds its params methodTypes = do
    prepared <- prepareKindCheck its
    htInferClassKindsPrepared prepared params methodTypes

htInferClassKindsPrepared :: PreparedKindCheck
                          -> [HSymbol] -> [HType]
                          -> Either String [(HSymbol, HKind)]
htInferClassKindsPrepared
        (PreparedKindCheck preparedSynonymArities assumptions)
        params methodTypes = do
    mapM_ (checkSynonymSaturationWith
        $ checkSynonymApplicationWithArities preparedSynonymArities) methodTypes
    convertedMethods <- mapM (first show . toSynthesisType) methodTypes
    inferred <- first show $ SharedInference.inferSharedVariableKinds
        assumptions params convertedMethods
    return [(parameter, fromGroundHKind kind) |
        (parameter, kind) <- inferred]

synthesisAssumptions
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> Either String SharedInference.KindAssumptions
synthesisAssumptions definitions = do
    constructors <- mapM convert definitions
    return SharedInference.emptyKindAssumptions
        { SharedInference.typeConstructorKinds = Map.fromList constructors }
  where
    convert (sourceName, (_, _, kind)) = (,)
        <$> first show (SharedName.parseName sourceName)
        <*> checkedGroundHKind kind

htCheckEnv :: [(HSymbol, ([HSymbol], HType, a))]
           -> Either String [(HSymbol, ([HSymbol], HType, HKind))]
htCheckEnv = fmap fst . prepareKindEnvironment

-- | Infer a complete type environment and retain the immutable information
-- needed to check every dependent declaration.  The inferred constructor
-- map is already exactly the assumptions a later check would reconstruct;
-- returning it here also lets the whole validation batch share one synonym
-- arity table.
prepareKindEnvironment
    :: [(HSymbol, ([HSymbol], HType, a))]
    -> Either String
        ( [(HSymbol, ([HSymbol], HType, HKind))]
        , PreparedKindCheck
        )
prepareKindEnvironment its = do
    normalized <- first renderAbstractTypeDefinitionError $
        normalizeAbstractTypeDefinitionsWith const its
    let preparedSynonymArities = synonymArities normalized
    mapM_ (checkSynonymSaturationWith
        (checkSynonymApplicationWithArities preparedSynonymArities) .
        declarationBody) normalized
    declarations <- mapM toKindDeclaration normalized
    inferred <- first show $
        SharedInference.inferAcyclicTypeConstructorKinds declarations
    checked <- mapM (attachKind inferred) normalized
    let assumptions = SharedInference.emptyKindAssumptions
            { SharedInference.typeConstructorKinds = inferred }
    return (checked, prepareKindCheckWithArities
        preparedSynonymArities assumptions)
  where
    declarationBody (_, (_, body, _)) = body

    toKindDeclaration (sourceName, (parameters, body, _)) = do
        name <- first show $ SharedName.parseName sourceName
        case body of
            HTAbstract _ kind -> SharedInference.DeclaredTypeKind name
                <$> checkedGroundHKind kind
            HTUnion constructors -> SharedInference.InferredTypeKind
                name parameters <$> mapM (first show . toSynthesisType)
                  [field | (_, fields) <- constructors, field <- fields]
            _ -> SharedInference.InferredTypeKind name parameters . (: [])
                <$> first show (toSynthesisType body)

    attachKind inferred (sourceName, (parameters, body, _)) = do
        name <- first show $ SharedName.parseName sourceName
        case Map.lookup name inferred of
            Nothing -> Left $
                "internal kind inference error for " ++ sourceName
            Just kind -> Right
                (sourceName, (parameters, body, fromGroundHKind kind))

-- Type synonyms are unlike data and abstract constructors: Haskell requires
-- every occurrence to supply at least the synonym's complete parameter list.
-- Kind checking alone cannot detect a partial use when it appears in a
-- compatible higher-kinded position, so validate minimum saturation from the
-- declaration shape. Excess arguments remain the kind checker's concern; in
-- Djinn's Haskell 98 subset every synonym result has kind @Type@, so they are
-- rejected there as an ill-kinded application rather than misreported as an
-- unsaturated synonym.
synonymArities
    :: [(HSymbol, ([HSymbol], HType, a))]
    -> [(HSymbol, Natural)]
synonymArities definitions =
    [(name, naturalLength parameters)
    | (name, (parameters, body, _)) <- definitions
    , isSynonymBody body]
  where
    isSynonymBody (HTUnion _) = False
    isSynonymBody (HTAbstract _ _) = False
    isSynonymBody _ = True

checkSynonymApplicationWithArities
    :: [(HSymbol, Natural)]
    -> HSymbol
    -> Natural
    -> Either String ()
checkSynonymApplicationWithArities arities name supplied =
    case lookup name arities of
        Just expected | supplied < expected -> Left $
            "Type synonym " ++ name ++ " expects at least " ++
            show expected ++ " argument(s), but got " ++ show supplied
        _ -> Right ()

checkSynonymSaturationWith
    :: (HSymbol -> Natural -> Either String ())
    -> HType
    -> Either String ()
checkSynonymSaturationWith checkApplication = checkType
  where
    checkType application@(HTApp _ _) = do
        let (headType, arguments) = splitApplication application
        checkHead headType (naturalLength arguments)
        case headType of
            HTCon _ -> return ()
            _ -> checkType headType
        mapM_ checkType arguments
    checkType (HTCon name) = checkName name 0
    checkType (HTTuple types) = mapM_ checkType types
    checkType (HTArrow argument result) =
        checkType argument >> checkType result
    checkType (HTForall _ constraints body) =
        mapM_ (mapM_ checkType) constraints >> checkType body
    checkType (HTUnion constructors) =
        mapM_ (mapM_ checkType . snd) constructors
    checkType (HTVar _) = return ()
    checkType (HTAbstract _ _) = return ()

    splitApplication = collect []
      where
        collect arguments (HTApp function argument) =
            collect (argument : arguments) function
        collect arguments headType = (headType, arguments)

    checkHead (HTCon name) supplied = checkName name supplied
    checkHead _ _ = Right ()

    checkName = checkApplication
