module Data.Void where

-- Instance-only SOURCE imports keep the curated module graph acyclic.
import {-# SOURCE #-} qualified Data.Data (Data)
import qualified Data.Eq (Eq)
import Data.Functor (Functor)
import qualified Data.Ix (Ix)
import qualified Data.Ord (Ord)
import qualified GHC.Generics (Generic)
import {-# SOURCE #-} qualified Text.Read (Read)
import qualified Text.Show (Show)


data Void

instance Data.Eq.Eq Void  
instance Data.Data.Data Void  
instance Data.Ord.Ord Void   
instance Text.Read.Read Void 
instance Text.Show.Show Void  
instance Data.Ix.Ix Void  
instance GHC.Generics.Generic Void   
-- instance Exception Void   
-- instance Hashable Void  
-- instance Semigroup Void   
-- instance Typeable * Void  

absurd :: Void -> a

vacuous :: Functor f => f Void -> f a

type Not x = x -> Void
