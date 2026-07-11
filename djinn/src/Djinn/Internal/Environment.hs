--
-- Transactional validation of declarations stored by a frontend, plus the
-- declaration-shape checks shared by the CLI and the library facade.
--
module Djinn.Internal.Environment (
    TypeDefinition, Axiom, ClassDefinition,
    validateEnvironment,
    replace, requireDistinct, requireUnusedName,
    checkConstructors, checkMethodNames
    ) where

import Data.List (nub)

import Djinn.Internal.HCheck (htCheckEnv, htCheckType, htInferClassKinds)
import Djinn.Internal.HTypes

type TypeDefinition = (HSymbol, ([HSymbol], HType, HKind))
type Axiom = (HSymbol, HType)
type ClassDefinition = (HSymbol, ([(HSymbol, HKind)], [Axiom]))

-- Rebuild inferred kinds first, then check every declaration that depends
-- on them.  Class parameter kinds are re-inferred against the rebuilt type
-- graph, so they cannot go stale when a mentioned type changes.  Callers
-- update their state only with the returned environment, making deletion
-- and replacement atomic even when validation fails midway.
validateEnvironment ::
    [TypeDefinition] -> [Axiom] -> [ClassDefinition] ->
    Either String ([TypeDefinition], [ClassDefinition])
validateEnvironment definitions axioms classes = do
    checked <- withContext "type environment" $ htCheckEnv definitions
    mapM_ (checkAxiom checked) axioms
    refreshed <- mapM (checkClass checked) classes
    return (checked, refreshed)

checkAxiom :: [TypeDefinition] -> Axiom -> Either String ()
checkAxiom definitions (name, axiomType) =
    withContext ("axiom " ++ name) $ htCheckType definitions axiomType

checkClass :: [TypeDefinition] -> ClassDefinition
           -> Either String ClassDefinition
checkClass definitions (className, (params, methods)) = do
    mapM_ checkMethod methods
    kinds <- withContext ("class " ++ className) $
        htInferClassKinds definitions (map fst params) (map snd methods)
    return (className, (kinds, methods))
  where
    checkMethod (methodName, methodType) =
        withContext
            ("method " ++ methodName ++ " of class " ++ className) $
            htCheckType definitions methodType

withContext :: String -> Either String a -> Either String a
withContext description result =
    case result of
        Left message -> Left $ description ++ ": " ++ message
        Right value -> Right value

-- Add or overwrite one binding in an association list.
replace :: HSymbol -> (HSymbol, a) -> [(HSymbol, a)] -> [(HSymbol, a)]
replace name binding = (binding :) . filter ((/= name) . fst)

requireDistinct :: String -> [HSymbol] -> Either String ()
requireDistinct what names =
    case [name | name <- nub names, length (filter (== name) names) > 1] of
        [] -> Right ()
        name:_ -> Left $ "Duplicate " ++ what ++ ": " ++ name

requireUnusedName :: String -> HSymbol -> [(HSymbol, a)] -> Either String ()
requireUnusedName existingKind name definitions
    | name `elem` map fst definitions =
        Left $ name ++ " is already defined as a " ++ existingKind
    | otherwise = Right ()

-- A data declaration may not reuse a constructor name, either within
-- itself or from another stored data type.
checkConstructors :: HSymbol -> HType -> [TypeDefinition]
                  -> Either String ()
checkConstructors owner (HTUnion constructors) definitions = do
    requireDistinct "data constructor" names
    case [(constructor, typeName)
            | (typeName, (_, HTUnion existing, _)) <- definitions
            , typeName /= owner
            , (constructor, _) <- existing
            , constructor `elem` names] of
        [] -> Right ()
        (constructor, typeName):_ -> Left $
            "Data constructor " ++ constructor ++
            " is already defined by " ++ typeName
  where names = map fst constructors
checkConstructors _ _ _ = Right ()

-- A class may not reuse a method name owned by another class.
checkMethodNames :: HSymbol -> [Axiom] -> [ClassDefinition]
                 -> Either String ()
checkMethodNames owner methods definitions =
    case [(method, className)
            | (className, (_, existing)) <- definitions
            , className /= owner
            , (method, _) <- existing
            , method `elem` names] of
        [] -> Right ()
        (method, className):_ -> Left $
            "Method " ++ prHSymbolOp method ++
            " is already defined by class " ++ className
  where names = map fst methods
