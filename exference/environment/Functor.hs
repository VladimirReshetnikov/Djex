module Data.Functor where



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
