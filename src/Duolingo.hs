{-# LANGUAGE OverloadedStrings #-}
module Duolingo where

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
import Data.List.Split
import Connector
import FileData

--main :: IO ()
main = do
  headers <- getAuthHeaders credentialsJson
  wordsJson <- getUserWords headers

  let wordsPacked = BS.pack wordsJson
      (Just words) = decode wordsPacked :: Maybe Words
      toTranslateAll = map ( convert . word) (vocabulary words)
      toTranslateChunked = chunksOf 20 toTranslateAll

  wordsFromFile <- loadWordsFromFile userProgressFilePath
  print toTranslateAll
  let untranslated = getUntranslated (map inSpanish wordsFromFile) toTranslateAll
  print untranslated
--  tr <- getTranslations (head toTranslateChunked) headers
--  return toTranslate

  where credentialsJson = "password=xxx&login=boneash"
        userProgressFilePath = "progress.txt"
--3. wez te obecne ktorych nie ma w pliku i przetlumacz
--4. polacz z pliku z przetlumaczonymi

getUntranslated :: [String] -> [String] -> [String]
getUntranslated wordsFromFile = filter (\x ->  not $ x `elem` wordsFromFile)

convert :: String -> String
convert = concatMap repl
    where
        repl '\225' = "\\u00e1"
        repl '\233' = "\\u00e9"
        repl '\237' = "\\u00ed"
        repl '\241' = "\\u00f1"
        repl '\243' = "\\u00f3"
        repl chr = [chr]

test = do
        let t = "{\"word_string\":\"rat\\u00f3n\", \"gender\":null}"
            d = fromString t
        print d
        print( decodeStrict d :: Maybe Wordd)
