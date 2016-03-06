{-# LANGUAGE OverloadedStrings #-}
module Duolingo where

import Network.HTTP
import Network.Stream
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)
import Data.Aeson
import Control.Applicative    ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Json
import Network.URI
import Network.BufferType
import Data.String.Unicode
import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStrLn)
import Data.ByteString.UTF8 (fromString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split

main :: IO ()
main = do
  loginResponse <- simpleHTTP (postRequestWithBody duolingoURL contentType credentialsJson)

  let (Right response) = loginResponse
      (Just authHeader) = findHeader HdrSetCookie response
      headers = [(Header HdrCookie authHeader)]

  wordsResponse <- simpleHTTP $ setHeaders (getRequest wordsUrl) headers
  wordsJson  <- getResponseBody wordsResponse
  let wordsPacked = BS.pack wordsJson
      (Just words) = decode wordsPacked :: Maybe Words

      toTranslateAll = map ( convert . word) (vocabulary words)
      toTranslateChunked = chunksOf 20 toTranslateAll
  tr <- getTranslations (head toTranslateChunked) headers
--  tran <- getTranslations toTranslate headers
--  print $  tran
  print tr

  where duolingoURL = "http://www.duolingo.com/login"
        contentType = "application/x-www-form-urlencoded"
        credentialsJson = "password=xxx&login=boneash"
        userUrl = "http://www.duolingo.com//users/boneash"
        wordsUrl = "http://www.duolingo.com//vocabulary/overview"
        translateUrl = "http://www.d2.duolingo.com/api/1/dictionary/hints/es/en?tokens="


getTranslations :: [String] -> [Header] -> IO (Map String [String])
getTranslations words authHeaders = do
     let translateTokens = "[" ++ (intercalate "," $ map (\w -> "\"" ++ w ++ "\"") words) ++ "]"
         translateURI = URI { uriScheme = "http:",
                                    uriAuthority = Just (URIAuth "" "d2.duolingo.com" ""),
                                    uriPath      = "/api/1/dictionary/hints/es/en?tokens=" ++ translateTokens,
                                    uriQuery = "",
                                    uriFragment  = ""}
     translateResponse <-  simpleHTTP $ setHeaders (mkRequest GET translateURI :: Request String) authHeaders
     translations <- getResponseBody translateResponse
     let translationsPacked = BS.pack translations
         (Just translationsMapAeson) = decode translationsPacked :: Maybe Transl
         translationsMap = trans translationsMapAeson
     return translationsMap

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

--
--  print wordsPacked
--  print $ toTranslate \\u00e1 \\u00e9
--  mapM_ (Data.ByteString.Char8.putStrLn . Data.ByteString.UTF8.fromString) toTranslate
