import Data.List
import Data.Bifunctor
import System.IO

process :: Integral a => Read a => [[(String, a)]] -> String -> [[(String, a)]]
process bs ss = ls ++ new : rs
    where
        (l, a) = break (pure (||) <*> (=='=') <*> (=='-')) ss
        b = hash l
        (ls, c, rs) = (genericTake b bs, head $ genericDrop b bs, genericDrop (b+1) bs)
        rem (d : ds)
            | fst d == l = ds
            | otherwise = d : rem ds
        rem [] = []
        add n (d : ds)
            | fst d == l = (l, n) : ds
            | otherwise = d : add n ds
        add n [] = [(l, n)]
        new = case a of
            "-" -> rem c
            ('=' : d : []) -> add (read [d]) c

fancySum :: Integral a => [[(String, a)]] -> a
fancySum = sum . fmap (uncurry (*)) . zip [1..] . fmap (sum . fmap (uncurry (*)) . zip [1..] . fmap snd)

hash :: Integral a => String -> a
hash = foldl inner 0
    where
        inner :: Integral a => a -> Char -> a
        inner = ((flip rem 256 . (*17)) .) . (. (fromIntegral . fromEnum)) . (+)

calc :: Integral a => Read a => String -> a
calc = fancySum . foldl process (replicate 256 []) . unfoldr split
    where
        split :: String -> Maybe (String, String)
        split "" = Nothing
        split s = Just $ bimap id (drop 1) $ break (==',') s

main = do
    text <- hGetContents stdin
    print $ calc text
