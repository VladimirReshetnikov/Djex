{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-independent Haskell source types.
--
-- Search engines may retain richer internal types, but parsers and validated
-- declaration environments can meet at this representation. Declaration
-- bodies are intentionally absent: synonyms, data constructors, and opaque
-- types belong in a separate declaration layer rather than masquerading as
-- ordinary type expressions.
module Language.Haskell.Synthesis.Type
  ( Variable (..)
  , Type (..)
  , TypeError (..)
  , canonicalizeType
  , validateType
  , freeVariables
  , typeConstructors
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Constraint
import Language.Haskell.Synthesis.Name

-- | Flexible inference variables and rigid skolems share an identity domain
-- without being unifiable by accident. Backends without this distinction can
-- use their identity type directly as the parameter of 'Type'.
data Variable identity
  = FlexibleVariable identity
  | RigidVariable identity
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData identity => NFData (Variable identity)

data Type variable
  = TypeVariable variable
  | TypeConstructor Name
  | TypeApplication (Type variable) (Type variable)
  | FunctionType (Type variable) (Type variable)
  | TupleType Boxity [Type variable]
  | ForallType
      [variable]
      [Constraint (Type variable)]
      (Type variable)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (Type variable)

data TypeError variable
  = InvalidTypeConstructor Name
  | InvalidTupleTypeArity Boxity Int
  | DuplicateForallVariable variable
  | InvalidTypeConstraint ConstraintError
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData variable => NFData (TypeError variable)

-- | Give saturated function and tuple constructors one structural form.
-- Partial and over-applied constructors remain ordinary applications so kind
-- checking can diagnose them with the surrounding declaration environment.
canonicalizeType :: Type variable -> Type variable
canonicalizeType source = case source of
  TypeVariable{} -> source
  TypeConstructor name
    | nameSpecial name == Just (TupleConstructor Boxed 0) ->
        TupleType Boxed []
    | otherwise -> source
  TypeApplication{} ->
    let (headType, arguments) = applicationSpine source
        canonicalHead = canonicalizeType headType
        canonicalArguments = map canonicalizeType arguments
    in rebuildApplication canonicalHead canonicalArguments
  FunctionType parameter result ->
    FunctionType (canonicalizeType parameter) (canonicalizeType result)
  TupleType boxity elements ->
    TupleType boxity $ map canonicalizeType elements
  ForallType variables constraints body -> ForallType variables
    (map (fmap canonicalizeType) constraints)
    (canonicalizeType body)

applicationSpine :: Type variable -> (Type variable, [Type variable])
applicationSpine = collect []
  where
    collect arguments (TypeApplication function argument) =
      collect (argument : arguments) function
    collect arguments function = (function, arguments)

rebuildApplication :: Type variable -> [Type variable] -> Type variable
rebuildApplication headType arguments = case headType of
  TypeConstructor name
    | nameSpecial name == Just FunctionConstructor
    , [parameter, result] <- arguments -> FunctionType parameter result
    | Just (TupleConstructor boxity arity) <- nameSpecial name
    , arity == length arguments -> TupleType boxity arguments
  _ -> foldl TypeApplication headType arguments

validateType :: Ord variable => Type variable -> Either (TypeError variable) ()
validateType source = validate $ canonicalizeType source
  where
    validate typeExpression = case typeExpression of
      TypeVariable{} -> Right ()
      TypeConstructor name
        | validTypeConstructor name -> Right ()
        | otherwise -> Left $ InvalidTypeConstructor name
      TypeApplication function argument ->
        validate function >> validate argument
      FunctionType parameter result ->
        validate parameter >> validate result
      TupleType boxity elements -> do
        unless (validTupleArity boxity $ length elements) $
          Left $ InvalidTupleTypeArity boxity $ length elements
        mapM_ validate elements
      ForallType variables constraints body -> do
        case firstDuplicate variables of
          Just variable -> Left $ DuplicateForallVariable variable
          Nothing -> Right ()
        mapM_ validateForallConstraint constraints
        validate body

    validateForallConstraint constraint = do
      either (Left . InvalidTypeConstraint) Right $
        validateConstraint constraint
      mapM_ validate $ constraintArguments constraint

    validTypeConstructor name =
      nameLexicalClass name == ConstructorLike &&
        nameSpecial name /= Just ConsConstructor

validTupleArity :: Boxity -> Int -> Bool
validTupleArity Boxed arity = arity == 0 || arity >= 2
validTupleArity Unboxed arity = arity >= 1

firstDuplicate :: Ord value => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : remaining)
      | value `Set.member` seen = Just value
      | otherwise = go (Set.insert value seen) remaining

freeVariables :: Ord variable => Type variable -> Set variable
freeVariables typeExpression = case typeExpression of
  TypeVariable variable -> Set.singleton variable
  TypeConstructor{} -> Set.empty
  TypeApplication function argument ->
    freeVariables function `Set.union` freeVariables argument
  FunctionType parameter result ->
    freeVariables parameter `Set.union` freeVariables result
  TupleType _ elements -> Set.unions $ map freeVariables elements
  ForallType variables constraints body ->
    (Set.unions
      (freeVariables body :
        [ freeVariables argument
        | constraint <- constraints
        , argument <- constraintArguments constraint
        ])) `Set.difference` Set.fromList variables

-- | Collect nominal constructor references from a type. Structural function
-- and tuple forms have intrinsic kinds and therefore contribute only the
-- constructors mentioned by their elements.
typeConstructors :: Type variable -> Set Name
typeConstructors typeExpression = case typeExpression of
  TypeVariable{} -> Set.empty
  TypeConstructor name -> Set.singleton name
  TypeApplication function argument ->
    typeConstructors function `Set.union` typeConstructors argument
  FunctionType parameter result ->
    typeConstructors parameter `Set.union` typeConstructors result
  TupleType _ elements -> Set.unions $ map typeConstructors elements
  ForallType _ constraints body -> Set.unions
    (typeConstructors body :
      [ typeConstructors argument
      | constraint <- constraints
      , argument <- constraintArguments constraint
      ])
