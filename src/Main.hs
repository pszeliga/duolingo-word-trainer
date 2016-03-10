{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP
import Network.Stream
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)
import Data.Aeson
import Data.List (find)
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
import Ask

--main :: IO ()
main = do
  headers <- getAuthHeaders credentialsJson
  wordsJson <- getUserWords headers
  -- print wordsJson
  let wordsPacked = BS.pack wordsJson
      (Just words) = decode wordsPacked :: Maybe Words
      toTranslateAll = map ( convert . word) (vocabulary words)
  -- let wordsFromFile = []
  wordsFromFile <- loadWordsFromFile userProgressFilePath
  let untranslated = getUntranslated (map inSpanish wordsFromFile) toTranslateAll
  translated <- translateWords untranslated headers
  let translatedWords = map (\x -> toWord x (vocabulary words)) translated
  -- mapM_ print (translatedWords)

  -- saveWordsToFile "progress3.txt" translatedWords

  -- wordsFromFile <- loadWordsFromFile "progress3.txt"
      -- wordsFromFile = []
  let wordsToPractice = wordsFromFile ++ translatedWords

  toPersist <- keepAsking wordsToPractice
  saveWordsToFile "progress3.txt" toPersist
  -- mapM_ print wordsFromFile
  print "koniec"

  where credentialsJson = "password=xxx&login=boneash"
        userProgressFilePath = "progress3.txt"

toWord :: (String, [String]) -> [Wordd] -> Word
toWord (spanish, english) genderReference = Word (convert spanish) (filterForbidden english) (genderr spanish genderReference) 0 0
            where genderr spanish genderReference = let (Just wrd) = find (\x -> word x == spanish) genderReference
                                                    in gender wrd

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
