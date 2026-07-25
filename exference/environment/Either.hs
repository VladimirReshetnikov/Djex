module Data.Either where

import qualified Control.Applicative (Applicative)
import qualified Control.Monad (Monad)
import qualified Data.Eq (Eq)
import qualified Data.Foldable (Foldable)
import qualified Data.Functor (Functor)
import qualified Data.Ord (Ord)
import qualified Data.Traversable (Traversable)
import qualified GHC.Generics (Generic, Generic1)
-- Instance-only SOURCE imports keep the curated module graph acyclic.
import {-# SOURCE #-} qualified Text.Read (Read)
import qualified Text.Show (Show)


data Either a b = Left a
                | Right b

-- replacable by pattern-matching; causes larger search-space
-- either :: (a->c) -> (b->c) -> Either a b -> c
partitionEithers :: [Either a b] -> ([a], [b]) 

instance Control.Monad.Monad (Either e)
instance Data.Functor.Functor (Either a)
instance Control.Applicative.Applicative (Either e)
instance Data.Foldable.Foldable (Either a)
instance Data.Traversable.Traversable (Either a)
instance GHC.Generics.Generic1 (Either a)
instance (Data.Eq.Eq a, Data.Eq.Eq b) => Data.Eq.Eq (Either a b)
instance (Data.Ord.Ord a, Data.Ord.Ord b) => Data.Ord.Ord (Either a b)
instance (Text.Read.Read a, Text.Read.Read b) => Text.Read.Read (Either a b)
instance (Text.Show.Show a, Text.Show.Show b) => Text.Show.Show (Either a b)
instance GHC.Generics.Generic (Either a b)
