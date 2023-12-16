import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
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

data Direction = Left | Right | Up | Down deriving (Show, Eq, Ord)
opposite :: Direction -> Direction
opposite = fromJust . flip lookup [(Left, Right), (Right, Left), (Up, Down), (Down, Up)]
direction :: Direction -> Zipperr a -> Maybe (Zipperr a)
direction = fromJust . flip lookup [(Left, left), (Right, right), (Up, up), (Down, down)]
mirrorA :: Direction -> Direction
mirrorA = fromJust . flip lookup [(Left, Up), (Up, Left), (Right, Down), (Down, Right)]
mirrorB :: Direction -> Direction
mirrorB = fromJust . flip lookup [(Left, Down), (Down, Left), (Right, Up), (Up, Right)]

move :: Integral a => Show a => Set ((a, a), Direction) -> [(Direction, Maybe (Zipperr Char))] -> Set (a, a)
move v [] = S.empty
move v ((_, Nothing): n) = move v n
move v ((p, (Just z@(Zipper us ((Zipper ls (c : rs)) : ds)))) : n)
    | (pos, p) `S.member` v = move v n
    | (p == Left || p == Right) && c == '|' = S.insert pos (move v2 ((Up, up z) : (Down, down z) : n))
    | (p == Up || p == Down) && c == '-' = S.insert pos (move v2 ((Left, left z) : (Right, right z) : n))
    | c == '\\' = S.insert pos $ move v2 ((mirrorA p, direction (mirrorA p) z) : n)
    | c == '/' = S.insert pos $ move v2 ((mirrorB p, direction (mirrorB p) z) : n)
    | otherwise = S.insert pos $ move v2 ((p, direction p z) : n)
    where 
        pos = position z
        v2 = S.insert (pos, p) v
calc :: String -> Int
calc = S.size . move S.empty . pure . (Right,) . zippify . lines

main = do
    text <- hGetContents stdin
    print $ calc text
