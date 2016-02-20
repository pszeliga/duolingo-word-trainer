{-# LANGUAGE OverloadedStrings #-}
module DuolingoData where

import Data.Aeson             ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))

data DuolingoData = DuolingoData { languageData    :: [LanguageData]
                   } deriving (Show)

data Inner = Inner { getA        :: String
                    , getB       :: Integer
                    } deriving (Show)

instance FromJSON DuolingoData where
  parseJSON (Object v) =
    DuolingoData <$>
    (v .: "language_data")

instance FromJSON Inner where
  parseJSON (Object v) =
    Inner <$>
    (v .: "a")                  <*>
    (v .: "b")