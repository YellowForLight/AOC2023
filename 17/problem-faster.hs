import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as P
import Data.Set (Set)
import qualified Data.Set as S
import System.IO

data Zipper a = Zipper {backwards :: [a], forwards :: [a]} deriving (Show, Eq)
instance Functor Zipper where
    fmap f (Zipper ls rs) = Zipper (fmap f ls) (fmap f rs)
instance Foldable Zipper where
    foldMap f (Zipper ls rs) = (foldMap f ls) <> (foldMap f rs)
instance Traversable Zipper where
    traverse f (Zipper ls rs) = pure Zipper <*> (traverse f ls) <*> (traverse f rs)
type Zipperr a = Zipper (Zipper a)
right' :: Zipper a -> Maybe (Zipper a)
right' (Zipper _ (_ : [])) = Nothing
right' (Zipper ls (r : rs)) = Just $ Zipper (r : ls) rs
left' :: Zipper a -> Maybe (Zipper a)
left' (Zipper [] _) = Nothing
left' (Zipper (l : ls) rs) = Just $ Zipper ls (l : rs)
right :: Zipperr a -> Maybe (Zipperr a)
right = traverse right'
left :: Zipperr a -> Maybe (Zipperr a)
left = traverse left'
up :: Zipperr a -> Maybe (Zipperr a)
up = left'
down :: Zipperr a -> Maybe (Zipperr a)
down = right'
zipify :: [a] -> Maybe (Zipper a)
zipify [] = Nothing
zipify l = Just $ Zipper [] l
zippify :: [[a]] -> Maybe (Zipperr a)
zippify = (>>= zipify) . traverse zipify
position :: Integral a => Zipperr b -> (a, a)
position (Zipper us ((Zipper ls _) : _)) = (genericLength ls, genericLength us)
getC :: Zipperr a -> a
getC (Zipper _ ((Zipper _ (c : _)) : _)) = c
getH :: Integral a => Zipperr b -> a
getH (Zipper _ ((Zipper _ (_ : rs)) : ds)) = genericLength ds + genericLength rs

data Direction = Left | Right | Up | Down deriving (Ord, Eq, Show)
opposite :: Direction -> Direction
opposite = fromJust . flip lookup [(Left, Right), (Right, Left), (Up, Down), (Down, Up)]
direction :: Direction -> Zipperr a -> Maybe (Zipperr a)
direction = fromJust . flip lookup [(Left, left), (Right, right), (Up, up), (Down, down)]
adjacent :: Direction -> [Direction]
adjacent = fromJust . flip lookup [(Left, [Up, Down]), (Right, [Up, Down]), (Up, [Left, Right]), (Down, [Left, Right])]

findPath :: Integral a => a -> Set (Direction, a, (a, a)) -> MinPQueue a (a, a, Direction, Zipperr a) -> (a, a)
findPath h s p 
    | rs == [] && ds == [] = (h, heat)
    | forward == 3 = uncurry (findPath h) (foldr combine (s, next) (mapMaybe (inner 1) (adjacent d0)))
    | otherwise = uncurry (findPath h) (foldr combine (s, next) (mapMaybe (inner (forward+1)) [d0] ++ mapMaybe (inner 1) (adjacent d0)))
    where
        ((_, (heat, forward, d0, z@(Zipper us ((Zipper ls (c : rs)) : ds)))), next) = P.deleteFindMin p
        combine :: Ord a => Ord b => (a, (b, c)) -> (Set a, MinPQueue b c) -> (Set a, MinPQueue b c)
        combine (a, (b, c)) (s, p) = (S.insert a s, P.insert b c p) 
        inner f d = (direction d z) >>= (other f d) 
        other f d z2 
            | (d, f, position z2) `elem` s = Nothing
            | otherwise = Just ((d, f, position z2), (heat + getC z2 + (getH z2 * h), (heat + getC z2, f, d, z2)))

calc :: Integral a => Read a => String -> [(a, a)]
calc = (pure (flip findPath S.empty) <*> [9,8..1] <*>) . pure . P.fromList . pure . (0,) . (0, 0, Right,) . fmap (fmap (read . pure)) . fromJust . zippify . lines

main = do
    text <- hGetContents stdin
    traverse print (calc text)
