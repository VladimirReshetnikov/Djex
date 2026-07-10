--
-- Transactional validation of declarations stored by the frontend.
--
module Environment (validateEnvironment) where

import HCheck (htCheckEnv, htCheckType)
import HTypes

type TypeDefinition = (HSymbol, ([HSymbol], HType, HKind))
type Axiom = (HSymbol, HType)
type ClassDefinition = (HSymbol, ([HSymbol], [Axiom]))

-- Rebuild inferred kinds first, then check every declaration that depends on
-- them.  Callers update their state only with the returned environment, making
-- deletion and replacement atomic even when validation fails midway.
validateEnvironment ::
    [TypeDefinition] -> [Axiom] -> [ClassDefinition] ->
    Either String [TypeDefinition]
validateEnvironment definitions axioms classes = do
    checked <- withContext "type environment" $ htCheckEnv definitions
    mapM_ (checkAxiom checked) axioms
    mapM_ (checkClass checked) classes
    return checked

checkAxiom :: [TypeDefinition] -> Axiom -> Either String ()
checkAxiom definitions (name, axiomType) =
    withContext ("axiom " ++ name) $ htCheckType definitions axiomType

checkClass :: [TypeDefinition] -> ClassDefinition -> Either String ()
checkClass definitions (className, (_, methods)) =
    mapM_ checkMethod methods
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
