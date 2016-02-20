{-# LANGUAGE OverloadedStrings #-}
module Duolingo where

import Network.HTTP
import Network.Stream
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)
import Data.Aeson
import Data.Vector (head)
import Control.Applicative    ((<$>), (<*>))
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  loginResponse <- simpleHTTP (postRequestWithBody duolingoURL contentType credentialsJson)

  let (Right response) = loginResponse
      (Just authHeader) = findHeader HdrSetCookie response

  wordsResponse <- simpleHTTP $ setHeaders (getRequest wordsUrl) [(Header HdrCookie authHeader)]
  wordsJson  <- getResponseBody wordsResponse
--  print wresponse
  let wordsPacked = BS.pack wordsJson
  let (Just words) = decode wordsPacked :: Maybe Words

--  print $  Data.Vector.head $ vocabulary words
--  print  $ vocabulary words
  mapM_ putStrLn $ map word (vocabulary words)

  where duolingoURL = "http://www.duolingo.com/login"
        contentType = "application/x-www-form-urlencoded"
        credentialsJson = "password=xxxxx&login=boneash"
        userUrl = "http://www.duolingo.com//users/boneash"
        wordsUrl = "http://www.duolingo.com//vocabulary/overview"


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















--  userDataResponse <- simpleHTTP $ setHeaders (getRequest userUrl) [(Header HdrCookie authHeader)]
--
--  response  <- getResponseBody userDataResponse
--
--  let responsePacked = BS.pack response
--  let (Just languages) = decode responsePacked :: Maybe DuolingoData
--
--  let skillArray =  skills $ es $ languageData languages
--  print $ wordss $ Data.Vector.head skillArray
--  print $ wordss $ head skillArray
--  print $ length $ concatMap wordss skillArray
--data DuolingoData = DuolingoData { languageData    :: LanguageData
--                   } deriving (Show)
--
--data LanguageData = LanguageData { es        :: Skills
--                    } deriving (Show)
--
--data Skills = Skills { skills        :: [Skill]
--                    } deriving (Show)
--
--data Skill = Skill { wordss        :: [String]
--                    } deriving (Show)
--instance FromJSON DuolingoData where
--  parseJSON (Object v) =
--    DuolingoData <$>
--    (v .: "language_data")
--
--instance FromJSON LanguageData where
--  parseJSON (Object v) =
--    LanguageData <$>
--    (v .: "es")
--
--instance FromJSON Skills where
--  parseJSON (Object v) =
--    Skills <$>
--    (v .: "skills")
--
--instance FromJSON Skill  where
--  parseJSON (Object v) =
--    Skill <$>
--    (v .: "words")