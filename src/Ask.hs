module Ask where

import Prelude hiding (Word)

main :: IO ()
main = do
    let words = [Word "que" ["la", "ja"] 3 6, Word "major" ["ver", "son"] 0 6]
    toPersist <- keepAsking words
    print toPersist


keepAsking :: [Word] -> IO [Word]
keepAsking [] = return []
keepAsking words = do
    let (currentWord, wordsWhichLeft) = (head words, tail words)
    userTranslation <- askTranslation $ inEnglish currentWord
    case userTranslation of
        Just translation -> fmap (++ [scoredWord]) (keepAsking wordsWhichLeft)
             where scoredWord = scoreWord currentWord translation
        Nothing -> return words

askTranslation :: [String] -> IO (Maybe String)
askTranslation words = do
    putStrLn $ "Translate: " ++ ( show words )
    response <- getLine
    if not $ null response
        then return $ Just response
    else return Nothing

scoreWord :: Word -> String -> Word
scoreWord word answer | inSpanish word == answer = word { correctAttempts = (+1) (correctAttempts word) }
                      | otherwise = word { wrongAttempts = (+1) (wrongAttempts word) }

data Word = Word {
   inSpanish :: String,
   inEnglish :: [String],
   correctAttempts :: Int,
   wrongAttempts :: Int
} deriving (Show)