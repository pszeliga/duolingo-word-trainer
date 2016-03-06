{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Json where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Control.Applicative    ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Data.Map
import Prelude hiding (Word)
import Data.Monoid ((<>))


data Word = Word {
   inSpanish :: String,
   inEnglish :: [String],
   correctAttempts :: Int,
   wrongAttempts :: Int
} deriving (Show, Generic, Eq)

instance FromJSON Word where

instance ToJSON Word where
    toEncoding = genericToEncoding defaultOptions
--     toEncoding (Word sp en cr wr) =
--        pairs ("sp" .= sp <> "en" .= show en <> "cr" .= cr <> "wr" .= wr)
data Words = Words { vocabulary    :: [Wordd]
                   } deriving (Show)
data Wordd = Wordd {
                  word       :: String,
                  gender     :: Text
                   } deriving (Show)

instance FromJSON Words where
  parseJSON (Object v) =
    Words <$>
    (v .: "vocab_overview")
instance FromJSON Wordd where
  parseJSON (Object v) =
    Wordd<$>
    (v .: "word_string")     <*>
    (fromMaybe "X" <$> v .: "gender")

data Transl = Transl { trans :: Map String [String]} deriving (Show)

instance FromJSON Transl where
    parseJSON val = Transl <$> parseJSON val


