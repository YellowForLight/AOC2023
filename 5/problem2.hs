import Data.List
import Data.Bifunctor
import Data.Maybe
import Data.Tuple
import Control.Monad
import System.IO

parse :: Integral a => [a] -> (a, a, a)
parse [a, b, c] = (b, b+c-1 , a)
        
parseSeeds :: Integral a => [a] -> [(a, a)]
parseSeeds = group2
    where
        group2 [] = []
        group2 (a:b:c) = (a, a+b-1 ): group2 c

process :: Integral a => ([(a, a)], [[(a, a, a)]]) -> [a]
process = fmap fst . uncurry (foldl inner)
    where
        inner :: Integral a => [(a, a)] -> [(a, a, a)] -> [(a, a)]
        inner as f = as >>= other f
        other :: Integral a => [(a, a, a)] -> (a, a) -> [(a, a)]
        other [] (mi, ma) = [(mi, ma)]
        other f@((low, high, bot) : fs) (mi, ma)
            | high < mi = other fs (mi, ma)
            | low > ma = [(mi, ma)]
            | low > mi = (mi, low-1) : other f (low, ma)
            | high >= ma = [(mi - low + bot, ma - low + bot)]
            | otherwise = (mi - low + bot, high - low + bot) : other fs (high+1, ma)


calc :: Integral a => Read a => String -> a
calc = minimum . process . bimap (parseSeeds . fmap read . words . drop 1 . snd . break (==':') . head) (fmap (sortOn (\(a, _, _) -> a)) . fmap2 (parse . fmap read . words) . unfoldr split . drop 1) . break (=="") . lines
    where
        split :: [String] -> Maybe ([String], [String])
        split [] = Nothing
        split ss = Just $ join bimap (drop 1) $ break (=="") ss
        fmap2 f = fmap (fmap f)

main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
