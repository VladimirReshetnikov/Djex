--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Djinn.Internal.HCheck(
    htCheckEnv, htCheckType, htCheckTypeKind, htCheckTypesKinds,
    htInferClassKinds
    ) where
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Void (absurd)

import qualified Language.Haskell.Synthesis.Kind as SharedKind
import qualified Language.Haskell.Synthesis.KindInference as SharedInference
import qualified Language.Haskell.Synthesis.Name as SharedName

import Djinn.Internal.HTypes
import Djinn.Internal.Type (toSynthesisType)

htCheckType :: [(HSymbol, ([HSymbol], HType, HKind))] -> HType -> Either String ()
htCheckType its = htCheckTypeKind its KStar

-- Check that a type is well-kinded and has the given (ground) kind.  Free
-- type variables receive fresh kinds, so a variable argument fits any
-- expected kind while a mis-kinded application is still rejected.
htCheckTypeKind :: [(HSymbol, ([HSymbol], HType, HKind))]
                -> HKind -> HType -> Either String ()
htCheckTypeKind its expected t = htCheckTypesKinds its [(expected, t)]

-- Check several types in one kind-inference scope.  This matters whenever
-- free variables are shared between types: checking each type separately
-- could incorrectly assign the same variable a different kind each time.
htCheckTypesKinds :: [(HSymbol, ([HSymbol], HType, HKind))]
                  -> [(HKind, HType)] -> Either String ()
htCheckTypesKinds its expectedTypes = do
    mapM_ (checkSynonymSaturation its . snd) expectedTypes
    assumptions <- synthesisAssumptions its
    obligations <- mapM convertObligation expectedTypes
    first show $ SharedInference.checkTypesKinds assumptions obligations
  where
    convertObligation (expected, typeExpression) = (,)
        <$> toGroundKind expected
        <*> first show (toSynthesisType typeExpression)

-- Infer the kind of every class parameter from the class's method types,
-- as Haskell98 does.  Parameters left unconstrained (including every
-- parameter of a method-less class) default to *.  Only class parameters
-- share kind variables across methods: every other variable is implicitly
-- quantified by its individual method signature.
htInferClassKinds :: [(HSymbol, ([HSymbol], HType, HKind))]
                  -> [HSymbol] -> [HType]
                  -> Either String [(HSymbol, HKind)]
htInferClassKinds its params methodTypes = do
    mapM_ (checkSynonymSaturation its) methodTypes
    assumptions <- synthesisAssumptions its
    convertedMethods <- mapM (first show . toSynthesisType) methodTypes
    inferred <- first show $ SharedInference.inferSharedVariableKinds
        assumptions params convertedMethods
    return [(parameter, fromGroundKind kind) |
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
        <*> toGroundKind kind

toGroundKind :: HKind -> Either String SharedInference.GroundKind
toGroundKind kind = case kind of
    KStar -> Right SharedKind.ProperTypeKind
    KArrow parameter result -> SharedKind.FunctionKind
        <$> toGroundKind parameter <*> toGroundKind result
    KVar variable -> Left $
        "kind contains an unsolved variable: " ++ show variable

fromGroundKind :: SharedInference.GroundKind -> HKind
fromGroundKind kind = case kind of
    SharedKind.ProperTypeKind -> KStar
    SharedKind.FunctionKind parameter result -> KArrow
        (fromGroundKind parameter) (fromGroundKind result)
    SharedKind.KindVariable impossible -> absurd impossible

htCheckEnv :: [(HSymbol, ([HSymbol], HType, a))]
           -> Either String [(HSymbol, ([HSymbol], HType, HKind))]
htCheckEnv its = do
    mapM_ (checkSynonymSaturation its . declarationBody) its
    declarations <- mapM toKindDeclaration its
    inferred <- first show $
        SharedInference.inferAcyclicTypeConstructorKinds declarations
    mapM (attachKind inferred) its
  where
    declarationBody (_, (_, body, _)) = body

    toKindDeclaration (sourceName, (parameters, body, _)) = do
        name <- first show $ SharedName.parseName sourceName
        case body of
            HTAbstract _ kind -> SharedInference.DeclaredTypeKind name
                <$> toGroundKind kind
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
                (sourceName, (parameters, body, fromGroundKind kind))

-- Type synonyms are unlike data and abstract constructors: Haskell requires
-- every occurrence to supply the synonym's complete parameter list.  Kind
-- checking alone cannot detect a partial use when it appears in a compatible
-- higher-kinded position, so validate saturation from the declaration shape.
checkSynonymSaturation :: [(HSymbol, ([HSymbol], HType, a))]
                       -> HType -> Either String ()
checkSynonymSaturation definitions = checkType
  where
    synonymArities =
        [(name, length parameters)
        | (name, (parameters, body, _)) <- definitions
        , isSynonymBody body]

    isSynonymBody (HTUnion _) = False
    isSynonymBody (HTAbstract _ _) = False
    isSynonymBody _ = True

    checkType application@(HTApp _ _) = do
        let (headType, arguments) = splitApplication application
        checkHead headType (length arguments)
        case headType of
            HTCon _ -> return ()
            _ -> checkType headType
        mapM_ checkType arguments
    checkType (HTCon name) = checkName name 0
    checkType (HTTuple types) = mapM_ checkType types
    checkType (HTArrow argument result) =
        checkType argument >> checkType result
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

    checkName name supplied =
        case lookup name synonymArities of
            Just expected | supplied /= expected -> Left $
                "Type synonym " ++ name ++ " expects " ++ show expected ++
                " argument(s), but got " ++ show supplied
            _ -> Right ()
