import Data.List
import Data.Bifunctor
import System.IO

process :: Integral a => String -> a
process = foldl inner 0
    where
        inner :: Integral a => a -> Char -> a
        inner = ((flip rem 256 . (*17)) .) . (. (fromIntegral . fromEnum)) . (+)

calc :: Integral a => String -> a
calc = sum . fmap process . unfoldr split
    where
        split :: String -> Maybe (String, String)
        split "" = Nothing
        split s = Just $ bimap id (drop 1) $ break (==',') s

main = do
    text <- hGetContents stdin
    print $ calc text
