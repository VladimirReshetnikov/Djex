module Data.Char where

-- Instance-only SOURCE imports keep the curated module graph acyclic.
import {-# SOURCE #-} qualified Data.Data (Data)
import qualified Data.Eq (Eq)
import qualified Data.Int (Int)
import qualified Data.Ix (Ix)
import qualified Data.Ord (Ord)
import qualified Foreign.Storable (Storable)
import qualified GHC.Generics (Generic)
import qualified Text.Printf (PrintfArg)
import {-# SOURCE #-} qualified Text.Read (Read)
import {-# SOURCE #-} qualified Text.Show (Show)


data Char


instance Prelude.Bounded Char
instance Prelude.Enum Char
instance Data.Eq.Eq Char
instance Data.Data.Data Char
instance Data.Ord.Ord Char
instance Text.Read.Read Char
instance Text.Show.Show Char
instance Data.Ix.Ix Char
instance GHC.Generics.Generic Char
instance Foreign.Storable.Storable Char
instance Text.Printf.PrintfArg Char   

ord :: Char -> Data.Int.Int
chr :: Data.Int.Int -> Char
