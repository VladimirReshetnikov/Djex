module Text.Read where

import qualified Data.Either (Either)
import qualified Data.Eq (Eq)
import qualified Data.Maybe (Maybe)
import qualified Data.String (String)
import qualified Text.Show (Show)


data ReadS
data Lexeme

instance Data.Eq.Eq Lexeme
instance Read Lexeme
instance Text.Show.Show Lexeme

class Read a where

readEither :: Read a => Data.String.String -> Data.Either.Either Data.String.String a 

readMaybe :: Read a => Data.String.String -> Data.Maybe.Maybe a 
