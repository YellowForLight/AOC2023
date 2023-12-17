import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.Array.IArray
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as P
import Data.Set (Set)
import qualified Data.Set as S
import System.IO

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
getH :: Ix a => Integral a => IArray t b => t (a, a) b -> (a, a) -> a
getH a (x, y) = (tx - x) + (ty - y)
    where
        (tx, ty) = snd $ bounds a
toArray :: Ix a => Integral a => [[b]] -> Array (a, a) b
toArray l = listArray ((0, 0), (x-1, y-1)) $ concat $ transpose l
    where
        x = genericLength $ head l
        y = genericLength l

data Direction = Left | Right | Up | Down deriving (Ord, Eq, Show)
opposite :: Direction -> Direction
opposite = fromJust . flip lookup [(Left, Right), (Right, Left), (Up, Down), (Down, Up)]
direction :: Ix a => Integral a => IArray t b => Direction -> t (a, a) b -> (a, a) -> Maybe (a, a)
direction = fromJust . flip lookup [(Left, left), (Right, right), (Up, up), (Down, down)]
adjacent :: Direction -> [Direction]
adjacent = fromJust . flip lookup [(Left, [Up, Down]), (Right, [Up, Down]), (Up, [Left, Right]), (Down, [Left, Right])]

findPath :: forall a t. Ix a => Integral a => IArray t a => t (a, a) a -> MinPQueue a (a, a, Direction, (a, a)) -> [(a, a)]
findPath a = step (getH a (0, 0)) S.empty
    where
        step :: a -> Set (a, Direction, (a, a)) -> MinPQueue a (a, a, Direction, (a, a)) -> [(a, a)]
        step close s q 
            | x == fst (snd $ bounds a) && y == snd (snd $ bounds a) && forward >= 4 = return (0, heat)
            | x == fst (snd $ bounds a) && y == snd (snd $ bounds a) = step close s next
            | getH a p < close = (getH a p, heat) : step (getH a p) s q
            | forward == 10 = uncurry (step close) (foldr combine (s, next) (mapMaybe (inner 1) (adjacent d0)))
            | forward < 4 && not (forward == 0) = uncurry (step close) (foldr combine (s, next) (mapMaybe (inner (forward+1)) [d0]))
            | otherwise = uncurry (step close) (foldr combine (s, next) (mapMaybe (inner (forward+1)) [d0] ++ mapMaybe (inner 1) (adjacent d0)))
            where
                ((_, (heat, forward, d0, p@(x, y))), next) = P.deleteFindMin q
                combine :: Ord d => Ord b => (d, (b, c)) -> (Set d, MinPQueue b c) -> (Set d, MinPQueue b c)
                combine (a, (b, c)) (s, p) = (S.insert a s, P.insert b c p) 
                inner :: a -> Direction -> Maybe ((a, Direction, (a, a)), (a, (a, a, Direction, (a, a))))
                inner f d = (direction d a p) >>= (other f d) 
                other :: a -> Direction -> (a, a) -> Maybe ((a, Direction, (a, a)), (a, (a, a, Direction, (a, a))))
                other f d p2 
                    | (f, d, p2) `elem` s = Nothing
                    | otherwise = Just ((f, d, p2), (heat + (a ! p2) + getH a p2, (heat + (a ! p2), f, d, p2)))

calc :: Ix a => Integral a => Read a => String -> [(a, a)]
calc = (flip findPath $ P.fromList $ pure $ (0,) $ (0, 0, Right, (0, 0))) . fmap (read . pure) . toArray . lines

main = do
    text <- hGetContents stdin
    traverse print $ calc text
