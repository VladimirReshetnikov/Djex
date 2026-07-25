module Data.Maybe where

import qualified Control.Applicative (Alternative, Applicative)
import qualified Control.Monad (Monad, MonadPlus)
-- Instance-only SOURCE imports keep the curated module graph acyclic.
import {-# SOURCE #-} qualified Data.Data (Data)
import qualified Data.Eq (Eq)
import qualified Data.Foldable (Foldable)
import qualified Data.Functor (Functor)
import qualified Data.Monoid (Monoid)
import qualified Data.Ord (Ord)
import qualified Data.Traversable (Traversable)
import qualified GHC.Generics (Generic, Generic1)
import {-# SOURCE #-} qualified Text.Read (Read)
import qualified Text.Show (Show)


data Maybe a = Just a
             | Nothing

maybe :: b -> (a -> b) -> Maybe a -> b
fromMaybe :: a -> Maybe a -> a
catMaybes :: [Maybe a] -> [a]
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
maybeToList :: Maybe a -> [a]

instance Control.Monad.Monad Maybe
instance Data.Functor.Functor Maybe
instance Control.Applicative.Applicative Maybe
instance Data.Foldable.Foldable Maybe
instance Data.Traversable.Traversable Maybe
instance GHC.Generics.Generic1 Maybe
instance Control.Monad.MonadPlus Maybe
instance Control.Applicative.Alternative Maybe
instance Data.Eq.Eq a => Data.Eq.Eq (Maybe a)
instance Data.Data.Data a => Data.Data.Data (Maybe a)
instance Data.Ord.Ord a => Data.Ord.Ord (Maybe a)
instance Text.Read.Read a => Text.Read.Read (Maybe a)
instance Text.Show.Show a => Text.Show.Show (Maybe a)
instance GHC.Generics.Generic (Maybe a)
instance Data.Monoid.Monoid a => Data.Monoid.Monoid (Maybe a)
