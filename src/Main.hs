{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP
import Network.Stream
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)
import Data.Aeson
import Control.Applicative    ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Json
import Network.URI
import Network.BufferType
import Prelude hiding (putStr, Word)
import Data.ByteString.Char8 (putStrLn)
import Data.ByteString.UTF8 (fromString)
import Data.Map (Map)
import qualified Data.Map as Map
import Connector
import FileData

--main :: IO ()
main = do
  headers <- getAuthHeaders credentialsJson
  wordsJson <- getUserWords headers

  let wordsPacked = BS.pack wordsJson
      (Just words) = decode wordsPacked :: Maybe Words
      toTranslateAll = map ( convert . word) (vocabulary words)

  wordsFromFile <- loadWordsFromFile userProgressFilePath
  let untranslated = getUntranslated (map inSpanish wordsFromFile) toTranslateAll
  translated <- translateWords untranslated headers
  let translatedWords = map toWord translated
  -- mapM_ print (translatedWords)

  saveWordsToFile "progress3.txt" translatedWords

  wds <- loadWordsFromFile "progress3.txt"
  mapM_ print wds
  print "koniec"

  where credentialsJson = "password=xxx&login=boneash"
        userProgressFilePath = "progress.txt"
        ct = 155
        at = 5

toWord :: (String, [String]) -> Word
-- toWord (spanish, english) = Word spanish english 0 0
toWord (spanish, english) = Word (convert spanish) (filterForbidden english) 0 0

filterForbidden :: [String] -> [String]
filterForbidden = filter noForbidden
            where noForbidden word = ('\8230' `notElem` word) && ('\8776' `notElem` word) && ('\233' `notElem` word)

getUntranslated :: [String] -> [String] -> [String]
getUntranslated wordsFromFile = filter (`notElem` wordsFromFile)

convert :: String -> String
convert = concatMap repl
    where
        repl '\225' = "\\u00e1"
        repl '\233' = "\\u00e9"
        repl '\237' = "\\u00ed"
        repl '\241' = "\\u00f1"
        repl '\243' = "\\u00f3"
        repl '\250' = "\\u00fa"
        repl '\252' = "\\u00fc"
        repl chr = [chr]
