import Data.List
import System.IO

expand :: [String] -> [String]
expand = foldr inner []
    where
        inner bs as
            | all (=='.') bs = bs : bs : as
            | otherwise = bs : as

expandAll = transpose . expand . transpose . expand

number :: Integral a => [String] -> [(a, a)]
number = (>>= uncurry inner) . filter (not . null) . zip [0..] . fmap (fmap fst . filter ((=='#') . snd)) . fmap (zip [0..])
    where
        inner :: a -> [a] -> [(a, a)]
        inner = fmap . (,)

distances :: Integral a => [(a, a)] -> [a]
distances = (pure dist <*>) >>= (<*>)
    where
        dist (a, b) (c, d) = abs (c - a) + abs (d - b)

calc :: Integral a => String -> a
calc = (flip quot 2) . sum . distances . number . expandAll . lines

main = do
    text <- hGetContents stdin
    print $ calc text
