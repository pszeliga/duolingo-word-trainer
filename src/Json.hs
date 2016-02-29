{-# LANGUAGE OverloadedStrings #-}
module Json where

import Data.Text (Text)
import Data.Aeson
import Control.Applicative    ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Data.Map


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
--    (v .: "normalized_string")     <*>
    (v .: "word_string")     <*>
    (fromMaybe "X" <$> v .: "gender")

data Transl = Transl { trans :: Map String [String]} deriving (Show)

instance FromJSON Transl where
    parseJSON val = Transl <$> parseJSON val


