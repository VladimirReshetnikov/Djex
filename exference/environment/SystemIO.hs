module System.IO where

import qualified Control.Applicative (Applicative)
import qualified Control.Monad (Monad)
import qualified Data.Functor (Functor)
import qualified Data.String (String)


data IO a

instance Data.Functor.Functor IO
instance Control.Applicative.Applicative IO
instance Control.Monad.Monad IO

type FilePath = Data.String.String
