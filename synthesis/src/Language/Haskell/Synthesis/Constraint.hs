{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-independent class-constraint syntax.
--
-- A constraint contains only nominal class identity and backend-owned type
-- arguments.  It deliberately does not embed a class declaration, instance
-- environment, arity, or resolution policy.  This keeps superclass and
-- instance graphs finite values: graph edges mention a 'Name', while each
-- backend resolves that name through its own validated environment.
module Language.Haskell.Synthesis.Constraint
  ( Constraint (..)
  , ConstraintError (..)
  , constraintArity
  , showsConstraintWith
  , validateConstraintClassName
  , validateConstraint
  ) where

import Control.DeepSeq (NFData (rnf))
import Language.Haskell.Synthesis.Name
  ( LexicalClass (ConstructorLike)
  , Name
  , nameLexicalClass
  , nameSpecial
  , renderPrefix
  )

-- | A class name applied to zero or more type arguments.
--
-- The type parameter lets Djinn and Exference retain their current type
-- representations while sharing constraint identity and traversal.
data Constraint ty = Constraint
  { constraintClass :: !Name
  , constraintArguments :: [ty]
  }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

-- Render the familiar Haskell surface form.  Argument precedence ensures an
-- application-shaped backend type supplies its own parentheses.
instance Show ty => Show (Constraint ty) where
  showsPrec = showsConstraintWith $ showsPrec 11

instance NFData ty => NFData (Constraint ty) where
  rnf (Constraint className arguments) =
    rnf className `seq` rnf arguments

-- | Lexically malformed class identity.  Arity and declaration lookup remain
-- environment concerns, but every backend can agree that a class occupies the
-- ordinary constructor namespace rather than a value or built-in namespace.
data ConstraintError
  = InvalidConstraintClass Name
  deriving (Eq, Ord, Show)

instance NFData ConstraintError where
  rnf (InvalidConstraintClass name) = rnf name

-- | Number of supplied class arguments.  Whether that arity is valid belongs
-- to the declaration environment and may differ for unknown-class policies.
constraintArity :: Constraint ty -> Int
constraintArity = length . constraintArguments

-- | Render a constraint once its type layer has supplied the rendering of an
-- argument position. This is the single structural renderer used by both the
-- generic 'Show' instance and the shared source-type renderer; backend type
-- precedence therefore remains a policy of the callback rather than a second
-- constraint traversal.
showsConstraintWith
  :: (ty -> ShowS)
  -> Int
  -> Constraint ty
  -> ShowS
showsConstraintWith showArgument precedence
    (Constraint className arguments) =
  showParen (precedence > 0 && not (null arguments)) $
    showString (renderPrefix className) . foldr renderArgument id arguments
 where
  renderArgument argument rest =
    showChar ' ' . showArgument argument . rest

-- | Validate a nominal class name without claiming that it is declared.
-- Qualified constructor identifiers and constructor operators are accepted;
-- structural names such as tuples, lists, and function arrows are not classes.
validateConstraintClassName :: Name -> Either ConstraintError ()
validateConstraintClassName name
  | nameLexicalClass name == ConstructorLike
  , nameSpecial name == Nothing = Right ()
  | otherwise = Left $ InvalidConstraintClass name

-- | Validate the backend-independent part of a constraint.  Type arguments
-- deliberately stay opaque here and are checked by their owning type layer.
validateConstraint :: Constraint ty -> Either ConstraintError ()
validateConstraint = validateConstraintClassName . constraintClass
