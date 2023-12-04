import Data.List
import Data.Bifunctor
import Data.Tuple
import Control.Monad
import System.IO

process :: Integral a => String -> a
process = exp . genericLength . uncurry intersect . join bimap words . bimap id (drop 1) . break (=='|') . drop 1 . snd . break (==':')
    where
        exp 0 = 0
        exp n = 2 ^ (n-1)

calc :: Integral a => String -> a
calc = sum . fmap process . lines

main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
