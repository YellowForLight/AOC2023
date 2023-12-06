import Data.List
import Data.Tuple
import System.IO

process :: Integral a => [(a, a)] -> [a]
process = fmap $ uncurry inner
    where
        inner :: Integral a => a -> a -> a
        inner b c 
            | b `rem` 2 == 0 = -1 + 2 * ceiling (sqrt (fromIntegral $ b^2 - 4*c) / 2)
            | b `rem` 2 == 1 = -2 + 2 * ceiling (0.5 + sqrt (fromIntegral $ b^2 - 4*c) / 2)

calc :: Integral a => Read a => String -> a
calc = product . process . uncurry zip . toTup . fmap (drop 1 . fmap read . words) . lines
    where toTup (a:b:[]) = (a, b)

main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
