module FileData where
import Json
import Prelude hiding (Word)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate)
import Data.Aeson

loadWordsFromFile :: FilePath -> IO [Word]
loadWordsFromFile fileName = do
    fmap (linesToWords . lines) (readFile fileName)

saveWordsToFile :: FilePath -> [Word] -> IO ()
saveWordsToFile fileName words = do
    writeFile fileName (intercalate "\n" (wordsToLines words))

linesToWords :: [String] -> [Word]
linesToWords = map load
    where load line = let (Just word) = decode $ BS.pack line :: Maybe Word
                      in word

wordsToLines :: [Word] -> [String]
wordsToLines = map $ BS.unpack . encode

