import Data.List
import Data.Containers.ListUtils
import Data.Maybe
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import System.IO

data Zipper a = Zipper {backwards :: [a], forwards :: [a], square :: Int} deriving (Show, Eq)
instance Functor Zipper where
    fmap f (Zipper ls rs s) = Zipper (fmap f ls) (fmap f rs) s
instance Foldable Zipper where
    foldMap f (Zipper ls rs s) = (foldMap f ls) <> (foldMap f rs)
instance Traversable Zipper where
    traverse f (Zipper ls rs s) = pure Zipper <*> (traverse f ls) <*> (traverse f rs) <*> (pure s)
type Zipperr a = Zipper (Zipper a)
right' :: Zipper a -> Zipper a
right' (Zipper ls (r : []) s) = Zipper [] (reverse ls ++ [r]) (s+1)
right' (Zipper ls (r : rs) s) = Zipper (r : ls) rs s
left' :: Zipper a -> Zipper a
left' (Zipper [] rs s) = Zipper (reverse $ init rs) [last rs] (s - 1)
left' (Zipper (l : ls) rs s) = Zipper ls (l : rs) s
len :: Integral a => Zipper b -> a
len (Zipper ls rs _) = genericLength ls + genericLength rs
right :: Zipperr a -> Zipperr a
right = fmap right'
left :: Zipperr a -> Zipperr a
left = fmap left'
up :: Zipperr a -> Zipperr a
up = left'
down :: Zipperr a -> Zipperr a
down = right'
zipify :: [a] -> Zipper a
zipify l = Zipper [] l 0
zippify :: [[a]] -> Zipperr a
zippify = zipify . fmap zipify
position :: Zipperr b -> (Int, Int, (Int, Int))
position (Zipper us ((Zipper ls _ s1) : _) s2) = (length ls, length us, (s1, s2))
getc :: Zipper a -> a
getc (Zipper _ (c : _) _) = c
getC :: Zipperr a -> a
getC = getc . getc

step :: Zipperr Char -> [Zipperr Char]
step z = filter ((/='#') . getC) $ fmap ($ z) [right, left, up, down]
process :: Integral a => a -> Zipperr Char -> a
process n z = inner n z
    where
        zs = pure z
        l = len z
        (q, r) = n `quotRem` l
        (w, n1) = next r zs
        (x, n2) = next l n1
        (y, _) = next l n2
        a = ((y - x) - (x - w)) `div` 2
        b = (x - w) - a
        c = w
        inner n z = q*q*a + q*b + c
        next 0 zs = (genericLength zs, zs)
        next n zs = next (n-1) (nubOrdOn position (zs >>= step))

startRow :: Zipperr Char -> Zipperr Char
startRow z
    | elem 'S' (forwards (getc z)) = z
    | otherwise = startRow (down z)
startColumn :: Zipperr Char -> Zipperr Char
startColumn z
    | getC z == 'S' = z
    | otherwise = startColumn (right z)
start :: Zipperr Char -> Zipperr Char
start = startColumn . startRow

calc :: Integral a => String -> a
calc = process 26501365 . start . zippify . lines

main = do
    text <- hGetContents stdin
    print $ calc text
