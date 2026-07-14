{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | Private effects and allocation policy for one Exference search step.
--
-- A truncated nondeterministic branch must remain observable even though it
-- has no successor 'SearchNode'.  Putting 'ExceptT' outside the historical
-- list nondeterminism gives exactly that shape: successful and truncated
-- sibling branches coexist in the lazy @[Either BranchTruncation a]@ stream.
module Language.Haskell.Exference.Core.Internal.SearchControl
  ( BranchTruncation (..)
  , SearchBranches
  , runSearchBranches
  , chooseBranches
  , maybeBranch
  , truncateBranch
  , SearchAllocators (..)
  , defaultSearchAllocators
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)

import Language.Haskell.Exference.Core.Internal.FlexibleIds
  ( FlexibleIdSupply
  , FlexibleRenaming
  , allocateNamespace
  )
import qualified Language.Haskell.Exference.Core.Internal.Scope as Scope
import Language.Haskell.Exference.Core.Types (TVarId)

-- | A representational resource limit that discarded one search branch.
data BranchTruncation
  = BranchIdentifierSpaceExhausted
  deriving (Eq, Show)

-- ExceptT supplies the desired bind semantics, but its standard Alternative
-- instance treats errors as recoverable and is left-biased.  Search needs the
-- historical list union instead: both successful and truncated siblings must
-- survive.  Keep that one distinction explicit in this small wrapper.
newtype SearchBranches a = SearchBranches (ExceptT BranchTruncation [] a)
  deriving (Functor, Applicative, Monad)

instance Alternative SearchBranches where
  empty = SearchBranches $ ExceptT []
  SearchBranches left <|> SearchBranches right = SearchBranches $ ExceptT
    $ runExceptT left ++ runExceptT right

instance MonadPlus SearchBranches

runSearchBranches :: SearchBranches a -> [Either BranchTruncation a]
runSearchBranches (SearchBranches branches) = runExceptT branches

chooseBranches :: [a] -> SearchBranches a
chooseBranches = SearchBranches . lift

maybeBranch :: Maybe a -> SearchBranches a
maybeBranch = maybe empty pure

truncateBranch :: BranchTruncation -> SearchBranches a
truncateBranch = SearchBranches . throwE

-- | Every finite namespace used dynamically by proof search.
--
-- Keeping the functions injectable makes genuine exhaustion testable with a
-- tiny deterministic capacity.  Production uses the complete 'Int' domain.
data SearchAllocators = SearchAllocators
  { searchAllocateTermIdentifier
      :: TVarId
      -> Maybe (TVarId, TVarId)
      -- ^ Allocate the current term identifier and return the next counter.
  , searchAllocateFlexibleNamespace
      :: [TVarId]
      -> FlexibleIdSupply
      -> Maybe (FlexibleRenaming, FlexibleIdSupply)
  , searchAddScope
      :: forall binding.
         Scope.ScopeId
      -> Scope.Scopes binding
      -> Either
          Scope.ScopeInvariantError
          (Scope.ScopeId, Scope.Scopes binding)
  }

defaultSearchAllocators :: SearchAllocators
defaultSearchAllocators = SearchAllocators
  { searchAllocateTermIdentifier = allocateTermIdentifier
  , searchAllocateFlexibleNamespace = allocateNamespace
  , searchAddScope = Scope.addScope
  }

-- Term identifiers start at one because the root hole owns zero.  Sequential
-- allocation deliberately crosses maxBound into the unused negative half of
-- the Int domain; returning to zero is the first actual collision.
allocateTermIdentifier :: TVarId -> Maybe (TVarId, TVarId)
allocateTermIdentifier next
  | next == 0 = Nothing
  | otherwise = Just (next, next + 1)
