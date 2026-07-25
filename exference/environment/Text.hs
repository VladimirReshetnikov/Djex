module Data.Text where

import qualified Data.Data (Data)
import qualified Data.Eq (Eq)
import qualified Data.Monoid (Monoid)
import qualified Data.Ord (Ord)
import qualified Data.String (String)
import qualified Text.Read (Read)
import qualified Text.Show (Show)


data Text

pack :: Data.String.String -> Text
unpack :: Text -> Data.String.String



-- instance IsList Text
instance Data.Eq.Eq Text
instance Data.Data.Data Text
instance Data.Ord.Ord Text
instance Text.Read.Read Text
instance Text.Show.Show Text
-- instance IsString Text
instance Data.Monoid.Monoid Text
-- instance Binary Text
