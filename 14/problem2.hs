import Prelude hiding (cycle)
import Data.List hiding (cycle)
import Data.Maybe
import System.IO

tilt :: [Char] -> [Char]
tilt as = reverse . fst $ foldl inner ([], 0) $ zip [0..] as
    where
        inner :: Integral a => ([Char], a) -> (a, Char) -> ([Char], a)
        inner (a, b) (_, '.') = (a, b)
        inner (a, b) (_, 'O') = ('O':a, b+1)
        inner (a, b) (i, '#') = ('#' : genericReplicate (i-b) '.' ++ a, i+1)

cycle :: [[Char]] -> [[Char]]
cycle = fmap reverse . fmap tilt . reverse . transpose . reverse . fmap tilt . transpose . reverse . fmap tilt . transpose . fmap tilt . transpose

cycleUntil :: Integral a => Show a => a -> [[[Char]]] -> [[Char]] -> [[Char]]
cycleUntil 0 css cs = cs
cycleUntil n css cs
    | cs == next = next
    | cs `elem` css = let index = fromJust (cs `elemIndex` css) in css !! (index - (fromIntegral (n) `rem` (index+1)))
    | otherwise = cycleUntil (n-1) (cs : css) next
    where next = cycle cs

insertEdge :: [[Char]] -> [[Char]]
insertEdge as = (replicate (2+length (head as)) '#' :) $ (++ [replicate (2+length (head as)) '#']) $ fmap (('#':) . (++"#")) as

getLoad :: Integral a => [Char] -> a
getLoad = sum . fmap fst . filter ((=='O') . snd) . zip [0..] . reverse

calc :: Integral a => String -> a
calc = sum . fmap getLoad . transpose . cycleUntil 1000000000 [] . insertEdge . lines

main = do
    text <- hGetContents stdin
    print $ calc text
