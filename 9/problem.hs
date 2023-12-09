import Data.List
import System.IO

process :: Integral a => [a] -> a
process ns 
    | all (==0) ns = 0
    | otherwise = last ns + process (unfoldr diffs ns)
    where
        diffs (a : ns@(b : _)) = Just ((b-a), ns)
        diffs (_ : []) = Nothing

calc :: Integral a => Read a => String -> a
calc = sum . fmap (process . fmap read . words) . lines

main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
