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
lenDown :: Integral a => Zipperr b -> a
lenDown (Zipper _ ((Zipper _ _) : ds)) = genericLength ds
start :: Zipperr Char -> Zipperr Char
start z
    | getC z == '.' = z
    | otherwise = start (fromJust $ right z)
getC :: Zipperr a -> a
getC (Zipper _ ((Zipper _ (c : _)) : _)) = c

directions :: Char -> [Zipperr a -> Maybe (Zipperr a)]
directions = fromJust . flip lookup [('>', [right]), ('<', [left]), ('^', [up]), ('v', [down]), ('.', [left, right, up, down])]

process :: Integral a => Set (a, a) -> Zipperr Char -> [a]
process s z
    | getC z == '#' = []
    | getC z == '.' && lenDown z == 0 = [0]
    | position z `S.member` s = []
    | otherwise = fmap (+1) (mapMaybe ($ z) (directions $ getC z) >>= process (S.insert (position z) s))

calc :: Integral a => String -> a
calc = maximum . process S.empty . start . fromJust . zippify . lines

main = do
    text <- hGetContents stdin
    print $ calc text
