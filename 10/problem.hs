import Prelude hiding (Left, Right)
import Data.List
import Data.Maybe
import System.IO

data Zipper a = Zipper {backwards :: [a], forwards :: [a]} deriving (Show, Eq)
instance Functor Zipper where
    fmap f (Zipper ls rs) = Zipper (fmap f ls) (fmap f rs)
type Zipperr a = Zipper (Zipper a)
right' :: Zipper a -> Zipper a
right' (Zipper ls (r : rs)) = Zipper (r : ls) rs
left' :: Zipper a -> Zipper a
left' (Zipper (l : ls) rs) = Zipper ls (l : rs)
right :: Zipperr a -> Zipperr a
right = fmap right'
left :: Zipperr a -> Zipperr a
left = fmap left'
up :: Zipperr a -> Zipperr a
up = left'
down :: Zipperr a -> Zipperr a
down = right'
zipify :: [a] -> Zipper a
zipify = Zipper []
zippify :: [[a]] -> Zipperr a
zippify = zipify . fmap zipify
startRow :: Zipperr Char -> Zipperr Char
startRow z@(Zipper us (c : ds))
    | elem 'S' (forwards c) = z
    | otherwise = startRow (down z)
startColumn :: Zipperr Char -> Zipperr Char
startColumn z@(Zipper us ((Zipper ls (c : rs)) : ds))
    | c == 'S' = z
    | otherwise = startColumn (right z)
start :: Zipperr Char -> Zipperr Char
start = startColumn . startRow
data Direction = Left | Right | Up | Down deriving (Show, Eq)
direction :: Direction -> Zipperr a -> Zipperr a
direction = fromJust . flip lookup [(Left, left), (Right, right), (Up, up), (Down, down)]
opposite :: Direction -> Direction
opposite = fromJust . flip lookup [(Left, Right), (Right, Left), (Up, Down), (Down, Up)]
toDirections :: Char -> [Direction]
toDirections = fromJust . flip lookup [('.', []), ('|', [Up, Down]), ('-', [Left, Right]), ('J', [Left, Up]), ('L', [Right, Up]), ('F', [Right, Down]), ('7', [Left, Down])]
fromList :: Eq a => a -> [a] -> a
fromList prev (a : b : [])
    | a == prev = b
    | b == prev = a

process :: Integral a => Direction -> Zipperr Char -> a
process prev z@(Zipper us ((Zipper ls (c : rs)) : ds))
    | c == 'S' = 1
    | otherwise = 1 + process next (direction next z)
    where next = fromList (opposite prev) (toDirections c)

calc :: Integral a => String -> a
calc = flip quot 2 . process Down . down . start . zippify . lines

main = do
    text <- hGetContents stdin
    print $ calc text
