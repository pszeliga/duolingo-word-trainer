module FileData where
import Json
import Prelude hiding (Word)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Aeson

main =   saveWordsToFile "progress3.txt" [Word "df" [] 1 3]

parseFile :: FilePath -> IO [Word]
parseFile fileName = do
    fileContent <- readFile fileName
    let words = linesToWords $ lines fileContent
    return $ linesToWords $ lines fileContent

saveWordsToFile :: FilePath -> [Word] -> IO ()
saveWordsToFile fileName words = do
    writeFile fileName (head $ wordsToLines words)

linesToWords :: [String] -> [Word]
linesToWords = map load
    where load line = let (Just word) = decode $ BS.pack line :: Maybe Word
                      in word

wordsToLines :: [Word] -> [String]
wordsToLines = map $ BS.unpack . encode