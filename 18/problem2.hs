import Numeric
import Data.List
import Data.Maybe
import System.IO

data Direction = L | R | U | D | N deriving (Show, Eq, Ord, Read)
dtt :: Integral a => Direction -> (a, a)
dtt = fromJust . flip lookup [(L, (1, 0)), (R, (-1, 0)), (U, (0, -1)), (D, (0, 1)), (N, (0, 0))]
mulS :: Integral a => a -> (a, a) -> (a, a)
mulS a (x, y) = (a*x, a*y)
addT :: Integral a => (a, a) -> (a, a) -> (a, a)
addT (x1, y1) (x2, y2) = (x1+x2, y1+y2)
lToT :: Integral a => Read a => [String] -> (Direction, a, String)
lToT (a : b : c : []) = (read a, read b, c)
dir :: Char -> Direction
dir = ([R, D, L, U]!!) . (+ (-48)) . fromEnum
readCol :: Integral a => (b, c, String) -> (Direction, a, String)
readCol (_, _, ('(' : '#' : a)) = (dir (last text), fst $ head $ readHex (init text), text)
    where text = init a
isClockwise :: Direction -> Direction -> Bool
isClockwise a b
    | a == N || b == N = True
    | a == R && b == D = True
    | a == D && b == L = True
    | a == L && b == U = True
    | a == U && b == R = True
    | otherwise = False

calc :: Integral a => Read a => (Direction -> Direction -> Bool) -> String -> a
calc f = flip div 2 . abs . other . foldr inner (0, (N, True), (0, 0)) . fmap (readCol . lToT . words) . lines
    where
        other :: Integral a => (a, (Direction, Bool), (a, a)) -> a
        other (s, _, p) = s + step p (0,0)
        inner :: Integral a => (Direction, a, b) -> (a, (Direction, Bool), (a, a)) -> (a, (Direction, Bool), (a, a))
        inner (d, r, _) (s, (prev, o), p)
            | f prev d && o = (s + step p p' + step p' p2', (d, True), p2')
            | f prev d = (s + step p p2, (d, True), p2) 
            | o = (s + step p p2, (d, False), p2)
            | otherwise = (s + step p p'' + step p'' p2'', (d, False), p2'')
            where 
                p' = addT p (dtt prev)
                p'' = addT p (mulS (-1) (dtt prev))
                p2 = addT p (mulS r (dtt d))
                p2' = addT p' (mulS r (dtt d))
                p2'' = addT p'' (mulS r (dtt d))
        step :: Integral a => (a, a) -> (a, a) -> a
        step (x1, y1) (x2, y2) = (y1 + y2) * (x1 - x2)

main = do
    text <- hGetContents stdin
    print $ calc isClockwise text
    print $ calc (flip isClockwise) text
