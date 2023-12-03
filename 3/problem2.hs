import Data.List
import Data.Char
import Data.Bifunctor
import Data.Tuple
import Data.Traversable
import Control.Applicative
import Control.Monad
import System.IO

fmap2 f a = fmap (fmap f) a

filterGears :: Integral a => Eq a => [[Char]] -> [[a]]
filterGears = inner 1
    where
        inner :: Integral a => a -> [[Char]] -> [[a]]
        inner a (s : ss) = ns : inner b ss
            where (b, ns) = within a s
        inner _ [] = []
        within :: Integral a => a -> [Char] -> (a, [a])
        within n ('*':s) = bimap id (n:) $ within (n+1) s
        within n (_:s) = bimap id (0:) $ within n s
        within n "" = (n, [])

filterDigit :: Char -> Char
filterDigit c
    | isDigit c = c
    | otherwise = ' '

combine3 :: Integral a => [a] -> [a]
combine3 = foldr (:) [] . take 3

surround :: Integral a => [a] -> [[a]]
surround = init . init . fmap combine3 . tails . (0:)

hyperSurround :: Integral a => [[a]] -> [[[a]]]
hyperSurround = fmap2 (filter (/=0)) . getZipList . traverse (fmap (concat . getZipList) . traverse (ZipList . surround) . traverse ZipList) . getZipList . traverse (ZipList . surround)

type Map a b = [(a, [b])]
add :: Eq a => a -> b -> Map a b -> Map a b
add a b [] = [(a, [b])]
add a b ((c, bs) : ms) 
    | a == c = (a, b : bs) : ms
    | otherwise = (c, bs) : add a b ms

adds :: Eq a => [a] -> b -> Map a b -> Map a b
adds (a : as) b = adds as b . add a b
adds [] b = id

addd :: Eq a => Map a b -> Map a b -> Map a b
addd = foldl inner
    where
        inner :: Eq a => Map a b -> (a, [b]) -> Map a b
        inner m (a, bs) = foldl (flip $ add a) m bs

parse :: Integral a => Read a => Eq a => [(Char, [a])] -> Map a a
parse = fmap (bimap id (fmap read)) . thrd . foldr inner ("", [], [])
    where
        thrd (a, b, c) = c
        inner :: Integral a => (Char, [a]) -> (String, [a], Map a String) -> (String, [a], Map a String) 
        inner (' ', _) (s, t, xs)
            | null t = ("", [], xs)
            | otherwise = ("", [], adds t s xs)
        inner (c, a) (s, t, xs) = (c : s, t ++ a, xs)

calc :: Integral a=> Read a => String -> a
calc = sum . fmap (foldl (*) 1) . filter ((==2) . length) . fmap nub . fmap snd . foldl addd [] . fmap parse . fancyZip . bimap (fmap2 filterDigit) (hyperSurround . filterGears) . join (,) . map ('.':) . lines 
    where
        fancyZip :: ([[a]], [[b]]) -> [[(a, b)]]
        fancyZip = fmap partZip . partZip
        partZip :: ([a], [b]) -> [(a, b)]
        partZip = uncurry zip


main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
