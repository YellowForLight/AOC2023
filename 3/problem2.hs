import Data.List
import Data.Char
import Data.Bifunctor
import Data.Tuple
import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
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

add :: Ord a => Ord b => a -> b -> Map a (Set b) -> Map a (Set b)
add a b m 
    | M.null m = M.singleton a (S.singleton b)
    | otherwise = M.insertWith S.union a (S.singleton b) m

adds :: Ord a => Ord b => [a] -> b -> Map a (Set b) -> Map a (Set b)
adds = flip $ flip . foldr . flip add

parse :: Integral a => Read a => Ord a => [(Char, [a])] -> Map a (Set a)
parse = fmap (S.map read) . thrd . foldr inner ("", [], M.empty)
    where
        thrd (a, b, c) = c
        inner :: Integral a => (Char, [a]) -> (String, [a], Map a (Set String)) -> (String, [a], Map a (Set String)) 
        inner (' ', _) (s, t, xs)
            | null t = ("", [], xs)
            | otherwise = ("", [], adds t s xs)
        inner (c, a) (s, t, xs) = (c : s, t ++ a, xs)

calc :: Integral a=> Read a => String -> a
calc = sum . fmap product . filter ((==2) . length) . M.elems . M.unionsWith S.union . fmap parse . fancyZip . bimap (fmap2 filterDigit) (hyperSurround . filterGears) . join (,) . map ('.':) . lines 
    where
        fancyZip :: ([[a]], [[b]]) -> [[(a, b)]]
        fancyZip = fmap partZip . partZip
        partZip :: ([a], [b]) -> [(a, b)]
        partZip = uncurry zip


main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
