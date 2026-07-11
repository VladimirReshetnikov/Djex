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
  , constraintArity
  ) where

import Control.DeepSeq (NFData (rnf))
import Language.Haskell.Synthesis.Name (Name)

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
  showsPrec precedence (Constraint className arguments) =
    showParen (precedence > 0 && not (null arguments)) $
      shows className . foldr showArgument id arguments
    where
      showArgument argument rest =
        showChar ' ' . showsPrec 11 argument . rest

instance NFData ty => NFData (Constraint ty) where
  rnf (Constraint className arguments) =
    rnf className `seq` rnf arguments

-- | Number of supplied class arguments.  Whether that arity is valid belongs
-- to the declaration environment and may differ for unknown-class policies.
constraintArity :: Constraint ty -> Int
constraintArity = length . constraintArguments
