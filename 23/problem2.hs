import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.Array.IArray
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import System.IO

data Graph a b = Graph {nodes :: Set a, connections :: Map a [(b, a)]} deriving (Show, Eq, Ord, Read)
emp :: Graph a b
emp = Graph S.empty M.empty
comb :: Ord a => Ord b => Graph a b -> Graph a b -> Graph a b
comb (Graph n1 c1) (Graph n2 c2) = Graph (S.union n1 n2) (M.unionWith (++) c1 c2)
sing :: Ord a => a -> Graph a b
sing a = Graph (S.singleton a) (M.empty)
insN :: Ord a => a -> Graph a b -> Graph a b
insN n (Graph ns cs) = Graph (S.insert n ns) cs
insC :: Ord a => Ord b => (b, a, a) -> Graph a b -> Graph a b
insC (w, f, t) (Graph ns cs) = Graph ns (M.insertWith (++) f [(w, t)] cs)
insU :: Ord a => Ord b => (b, a, a) -> Graph a b -> Graph a b
insU (w, a, b) = insC (w, a, b) . insC (w, b, a)

left :: Ix a => Integral a => IArray t b => t (a, a) b -> (a, a) -> Maybe (a, a)
left a (x, y)
    | x == fst (fst $ bounds a) = Nothing
    | otherwise = Just (x-1, y)

right :: Ix a => Integral a => IArray t b => t (a, a) b -> (a, a) -> Maybe (a, a)
right a (x, y)
    | x == fst (snd $ bounds a) = Nothing
    | otherwise = Just (x+1, y)

up :: Ix a => Integral a => IArray t b => t (a, a) b -> (a, a) -> Maybe (a, a)
up a (x, y)
    | y == snd (fst $ bounds a) = Nothing
    | otherwise = Just (x, y-1)

down :: Ix a => Integral a => IArray t b => t (a, a) b -> (a, a) -> Maybe (a, a)
down a (x, y)
    | y == snd (snd $ bounds a) = Nothing
    | otherwise = Just (x, y+1)

toArray :: Ix a => Integral a => [[b]] -> Array (a, a) b
toArray l = listArray ((0, 0), (x-1, y-1)) $ concat $ transpose l
    where
        x = genericLength $ head l
        y = genericLength l

process :: Ix a => Integral a => IArray t Char => Set (a, a) -> t (a, a) Char -> [(a, a)] -> Graph (a, a) a
process s a [] = emp
process s a (p : ps)
    | snd p == snd (snd $ bounds a) = comb (sing p) (process (S.insert p s) a ps)
    | otherwise = uncurry finish $ foldr other (sing p, ps) $ filter (not . flip S.member s . snd) $ fmap (findNode False (S.singleton p)) $ filter ((/= '#') . (a !)) $ mapMaybe ($ p) [left a, right a, up a, down a]
    where 
        finish g ns = comb g (process (S.insert p s) a ns)
        other (w, n) (g, ns) = (insU (w, p, n) g, n : ns)
        findNode b s' p' 
            | (a ! p') /= '.' && b = (2, n)
            | snd p' == snd (snd $ bounds a) = (1, p')
            | p' == (1, 0) = (1, p')
            | otherwise = bimap (+1) id $ findNode True (S.insert p' s') n
            where
                n = head $ filter (not . flip S.member s') $ filter ((/= '#') . (a !)) $ mapMaybe ($ p') [left a, right a, up a, down a]

longest :: Integral a => Set (a, a) -> (a, a) -> (a, a) -> Graph (a, a) a -> Maybe a
longest s p b g
    | snd p == snd b = Just 0
    | S.member p s = Nothing
    | otherwise = maximum' $ mapMaybe (\(w, n) -> fmap (+w) $ longest (S.insert p s) n b g) ((connections g) M.! p)
    where
        maximum' xs
            | null xs = Nothing
            | otherwise = Just $ maximum xs

calc :: Ix a => Integral a => String -> Maybe a
calc = (pure (longest S.empty (1, 0)) <*> snd . bounds <*> flip (process S.empty) [(1, 0)]) . toArray . lines

main = do
    text <- hGetContents stdin
    print $ calc text
