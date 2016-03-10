module Ask where

import Prelude hiding (Word)
import Control.Applicative (liftA2)
import Json

test :: IO ()
test = do
    let words = [Word "que" ["la", "ja"] "M" 3 6, Word "major" ["ver", "son"] "M" 0 6]
    toPersist <- keepAsking words
    print toPersist


keepAsking :: [Word] -> IO [Word]
keepAsking [] = return []
keepAsking words = do
    let (currentWord, wordsWhichLeft) = (head words, tail words)
    userTranslation <- askTranslation currentWord
    case userTranslation of
        Just translation -> liftA2 (:) scoredWord (keepAsking wordsWhichLeft)
             where scoredWord = scoreWord' currentWord translation
        Nothing -> pure words

askTranslation :: Word -> IO (Maybe String)
askTranslation word = do
    putStrLn $ "Translate: " ++ genderr word ++ " "  ++ show (inEnglish word)
    response <- getLine
    if not $ null response
        then return $ Just response else return Nothing

scoreWord :: Word -> String -> Word
scoreWord word answer | inSpanish word == answer = word { correctAttempts = (+1) (correctAttempts word) }
                      | otherwise = word { wrongAttempts = (+1) (wrongAttempts word) }

scoreWord' :: Word -> String -> IO Word
scoreWord' word answer | inSpanish word == answer = do
                                          print "OK"
                                          return word { correctAttempts = (+1) (correctAttempts word) }
                       | otherwise = do
                               print $ "Uncorrect, should be: " ++ inSpanish word
                               return word { wrongAttempts = (+1) (wrongAttempts word) }


--saveToFile :: [Word] -> IO ()
--saveToFile words =
