import Data.List
import Data.Containers.ListUtils
import Data.Maybe
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

step :: Zipperr Char -> [Zipperr Char]
step z = filter ((/='#') . getC) $ mapMaybe ($ z) [right, left, up, down]
process :: Integral a => a -> [Zipperr Char] -> a
process 0 zs = genericLength zs
process n zs = process (n-1) (nubOrdOn position (zs >>= step))

startRow :: Zipperr Char -> Zipperr Char
startRow z@(Zipper us (c : ds))
    | elem 'S' (forwards c) = z
    | otherwise = startRow (fromJust $ down z)
startColumn :: Zipperr Char -> Zipperr Char
startColumn z@(Zipper us ((Zipper ls (c : rs)) : ds))
    | c == 'S' = z
    | otherwise = startColumn (fromJust $ right z)
start :: Zipperr Char -> Zipperr Char
start = startColumn . startRow

calc :: Integral a => String -> a
calc = process 64 . pure . start . fromJust . zippify . lines

main = do
    text <- hGetContents stdin
    print $ calc text
