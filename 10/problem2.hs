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
replace :: a -> Zipperr a -> Zipperr a
replace x (Zipper us ((Zipper ls (_ : rs)) : ds)) = Zipper us ((Zipper ls (x : rs)) : ds)
zipify :: [a] -> Zipper a
zipify = Zipper []
zippify :: [[a]] -> Zipperr a
zippify = zipify . fmap zipify
unzipify :: Zipper a -> [a]
unzipify (Zipper [] rs) = rs
unzipify z = unzipify (left' z)
unzippify :: Zipperr a -> [[a]]
unzippify = unzipify . fmap unzipify
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

final :: Direction -> Char
final = fromJust . flip lookup [(Down, 'B'), (Left, 'f'), (Right, '6')]
process :: Direction -> Zipperr Char -> Zipperr Char
process prev z@(Zipper us ((Zipper ls (c : rs)) : ds))
    | c == 'S' = replace (final prev) z
    | otherwise = process next (direction next (replace letter z))
    where 
        next = fromList (opposite prev) (toDirections c)
        letter = fromJust $ lookup c [('|', 'B'), ('F', 'f'), ('L', 'l'), ('7', '6'), ('J', 'j'), ('-', 'N')]

countIn :: Integral a => [[Char]] -> a
countIn = sum . fmap inner
    where
        inner = thrd . foldl sumIn (False, Nothing, 0)
        sumIn (b, f, n) 'B' = (not b, f, n)
        sumIn (b, Nothing, n) 'f' = (b, Just True, n)
        sumIn (b, Just True, n) 'j' = (not b, Nothing, n)
        sumIn (b, Just True, n) '6' = (b, Nothing, n)
        sumIn (b, Nothing, n) 'l' = (b, Just False, n)
        sumIn (b, Just False, n) '6' = (not b, Nothing, n)
        sumIn (b, Just False, n) 'j' = (b, Nothing, n)
        sumIn (b, Just u, n) 'N' = (b, Just u, n)
        sumIn (True, Nothing, n) _ = (True, Nothing, n+1)
        sumIn (False, Nothing, n) _ = (False, Nothing, n)
        thrd (_, _, a) = a

calc :: Integral a => String -> a
calc = countIn . unzippify . process Down . down . start . zippify . lines

main = do
    text <- hGetContents stdin
    print $ calc text
