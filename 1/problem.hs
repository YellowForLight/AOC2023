import System.IO
import Data.Char
import Data.List

recover :: Integral a => Read a => String -> a
recover s = read [head $ dropWhile (not . isDigit) s, last $ dropWhileEnd (not . isDigit) s]

process :: Integral a => Read a => String -> a
process s = sum $ recover <$> lines s

main = do
    input <- hGetContents stdin
    hPrint stdout $ process input
