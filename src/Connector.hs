module Connector where

import Network.URI
import Network.BufferType
import Network.HTTP
import Network.Stream
import Data.Map (Map)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate)
import Data.Aeson
import Json
import Prelude hiding (Word)


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


getAuthHeaders :: String -> IO [Header]
getAuthHeaders credentialsJson = do
    loginResponse <- simpleHTTP (postRequestWithBody duolingoURL contentType credentialsJson)
    let (Right response) = loginResponse
        (Just authHeader) = findHeader HdrSetCookie response
    return [(Header HdrCookie authHeader)]
    where duolingoURL = "http://www.duolingo.com/login"
          contentType = "application/x-www-form-urlencoded"


getUserWords :: [Header] -> IO (String)
getUserWords headers = do
    wordsResponse <- simpleHTTP $ setHeaders (getRequest wordsUrl) headers
    getResponseBody wordsResponse
    where wordsUrl = "http://www.duolingo.com//vocabulary/overview"