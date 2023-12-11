import Data.List
import Data.Bifunctor
import System.IO

expand :: Integral a => (a -> (a, a) -> (a, a)) -> [[(a, a)]] -> [[(a, a)]]
expand f = reverse . snd . foldl inner (0, [])
    where
        ff g (a, b) 
            | b == 0 = (a, b)
            | otherwise = g (a, b)
        inner (n, as) s
            | all ((==0) . snd) s = (n+999999, s : as)
            | otherwise = (n, fmap (ff (f n)) s : as)

expandAll :: Integral a => [[(a, a)]] -> [[(a, a)]]
expandAll = transpose . expand (bimap id . (+)) . transpose . expand (flip bimap id . (+))

number :: Integral a => [String] -> [[(a, a)]]
number = fmap (uncurry inner) . zip [1..] . fmap (fmap ffst . zip [1..])
    where
        ffst :: Integral a => (a, Char) -> a
        ffst (a, b)
            | b == '#' = a
            | otherwise = 0
        inner :: a -> [a] -> [(a, a)]
        inner = fmap . (,)

distances :: Integral a => [(a, a)] -> [a]
distances = (pure dist <*>) >>= (<*>)
    where
        dist (a, b) (c, d) = abs (c - a) + abs (d - b)

calc :: Integral a => String -> a
calc = (flip quot 2) . sum . distances . filter (not . (==0) . snd) . concat . expandAll . number . lines

main = do
    text <- hGetContents stdin
    print $ calc text
