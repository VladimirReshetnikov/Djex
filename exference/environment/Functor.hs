module Data.Functor where

-- Instance-only SOURCE imports keep the curated module graph acyclic.
import {-# SOURCE #-} qualified Control.Applicative (WrappedArrow, WrappedMonad)
import Control.Arrow (Arrow, ArrowMonad)
import qualified Control.Concurrent.STM (STM)
import qualified Control.Exception (Handler)
import {-# SOURCE #-} qualified Control.Monad (Monad)
import qualified Control.Monad.ST (ST)
import qualified Data.Functor.Const (Const)
import qualified System.Console.GetOpt (ArgDescr, ArgOrder, OptDescr)
import qualified Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadPrec (ReadPrec)


class Functor f where
  fmap :: (a->b) -> f a -> f b
  -- (<$) :: a -> f b -> f a

instance Functor []
instance Functor Text.ParserCombinators.ReadP.ReadP
instance Functor Text.ParserCombinators.ReadPrec.ReadPrec
instance Functor Control.Concurrent.STM.STM
instance Functor Control.Exception.Handler
instance Functor System.Console.GetOpt.ArgDescr
instance Functor System.Console.GetOpt.OptDescr
instance Functor System.Console.GetOpt.ArgOrder
-- instance Functor ((->) r)
instance Functor ((,) a)
instance Functor (Control.Monad.ST.ST s)
-- instance Functor (Proxy *)
instance Arrow a => Functor (Control.Arrow.ArrowMonad a)
instance Control.Monad.Monad m => Functor (Control.Applicative.WrappedMonad m)
instance Functor (Data.Functor.Const.Const m)
instance Arrow a => Functor (Control.Applicative.WrappedArrow a b)
