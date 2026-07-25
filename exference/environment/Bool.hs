module Data.Bool where

-- Instance-only SOURCE imports keep the curated module graph acyclic.
import {-# SOURCE #-} qualified Data.Bits (Bits, FiniteBits)
import {-# SOURCE #-} qualified Data.Data (Data)
import {-# SOURCE #-} qualified Data.Eq (Eq)
import {-# SOURCE #-} qualified Data.Ix (Ix)
import {-# SOURCE #-} qualified Data.Ord (Ord)
import qualified Foreign.Storable (Storable)
import qualified GHC.Generics (Generic)
import {-# SOURCE #-} qualified Text.Read (Read)
import {-# SOURCE #-} qualified Text.Show (Show)


data Bool = True
          | False

(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool

bool :: a -> a -> Bool -> a 

instance Prelude.Bounded Bool
instance Prelude.Enum Bool
instance Data.Eq.Eq Bool
instance Data.Data.Data Bool
instance Data.Ord.Ord Bool
instance Text.Read.Read Bool
instance Text.Show.Show Bool
instance Data.Ix.Ix Bool
instance GHC.Generics.Generic Bool
instance Data.Bits.FiniteBits Bool
instance Data.Bits.Bits Bool
instance Foreign.Storable.Storable Bool
