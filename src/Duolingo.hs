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

main :: IO ()
main = do
  loginResponse <- simpleHTTP (postRequestWithBody duolingoURL contentType credentialsJson)

  let (Right response) = loginResponse
      (Just authHeader) = findHeader HdrSetCookie response
      headers = [(Header HdrCookie authHeader)]

  wordsResponse <- simpleHTTP $ setHeaders (getRequest wordsUrl) headers
  wordsJson  <- getResponseBody wordsResponse
--  print wresponse
  let wordsPacked = BS.pack wordsJson
      (Just words) = decode wordsPacked :: Maybe Words
      toTranslate = take 10 $ map ( convert . word) (vocabulary words)
      translateTokens = "[" ++ (intercalate "," $ map (\w -> "\"" ++ w ++ "\"") toTranslate) ++ "]"
      translateURI = URI { uriScheme = "http:",
                           uriAuthority = Just (URIAuth "" "d2.duolingo.com" ""),
                           uriPath      = "/api/1/dictionary/hints/es/en?tokens=" ++ translateTokens,
                           uriQuery = "",
                           uriFragment  = ""}
--
--  print wordsPacked
--  print $ toTranslate \\u00e1 \\u00e9
  mapM_ (Data.ByteString.Char8.putStrLn . Data.ByteString.UTF8.fromString) toTranslate
  translateResponse <-  simpleHTTP $ setHeaders (mkRequest GET translateURI :: Request String) headers
  translations <- getResponseBody translateResponse

  let translationsPacked = BS.pack translations
      (Just translationsMapAeson) = decode translationsPacked :: Maybe Transl
      translationsMap = trans translationsMapAeson

  print translationsMap

  where duolingoURL = "http://www.duolingo.com/login"
        contentType = "application/x-www-form-urlencoded"
        credentialsJson = "password=xxx&login=boneash"
        userUrl = "http://www.duolingo.com//users/boneash"
        wordsUrl = "http://www.duolingo.com//vocabulary/overview"
        translateUrl = "http://www.d2.duolingo.com/api/1/dictionary/hints/es/en?tokens="



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

