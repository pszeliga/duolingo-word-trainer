{-# LANGUAGE OverloadedStrings #-}
module Json where


import Data.Aeson             ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS

data Paste = Paste { getLines    :: [Integer]
                   , getURL      :: Inner
                   } deriving (Show)

data Inner = Inner { getA        :: String
                    , getB       :: Integer
                    } deriving (Show)

instance FromJSON Paste where
  parseJSON (Object v) =
    Paste <$>
    (v .: "lines")                  <*>
    (v .: "url")

instance FromJSON Inner where
  parseJSON (Object v) =
    Inner <$>
    (v .: "a")                  <*>
    (v .: "b")