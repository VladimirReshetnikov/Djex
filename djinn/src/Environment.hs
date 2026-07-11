--
-- Transactional validation of declarations stored by the frontend.
--
module Environment (validateEnvironment) where

import HCheck (htCheckEnv, htCheckType, htInferClassKinds)
import HTypes

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
