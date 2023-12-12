import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Bifunctor
import System.IO

process :: Integral a => String -> [a] -> a
process aa bb = mem M.! (False, group aa, bb)
    where
        mem = M.fromList [((bool, as, bs), inner bool as bs) | bool <- [True, False], aas <- fmap group (tails aa), as <- [aas, "" : aas], bbs <- tails bb, bs <- precedingList bbs]
        inner True [] _ = 0
        inner False [] (b : bs)
            | b == 0 = mem M.! (False, [], bs)
            | otherwise = 0
        inner False [] [] = 1
        inner _ (a@('#' : _) : as) (b : bs)
            | b == 0 = 0
            | genericLength a > b = 0
            | genericLength a == b = mem M.! (False, as, (0 : bs))
            | genericLength a < b = mem M.! (True, as, (b-genericLength a : bs))
        inner _ (('#' : _) : _) [] = 0
        inner True (('.' : _) : _) _ = 0
        inner False (('.' : _) : as) (b : bs)
            | b == 0 = mem M.! (False, as, bs)
            | otherwise = mem M.! (False, as, (b : bs))
        inner _ (('.' : _) : as) [] = mem M.! (False, as, [])
        inner True (a@('?' : _) : as) (b : bs)
            | genericLength a > b = mem M.! (False, ((genericDrop b a) : as), (0 : bs))
            | genericLength a == b = mem M.! (False, as, (0 : bs))
            | genericLength a < b = mem M.! (True, as, (b-genericLength a : bs))
        inner False (('?' : as) : aas) (b : bs)
            | b == 0 = mem M.! (False, (as : aas), bs)
            | b == 1 = mem M.! (False, (as : aas), (b : bs)) + mem M.! (False, (as : aas), (0 : bs))
            | otherwise = mem M.! (False, (as : aas), (b : bs)) + mem M.! (True, (as : aas), (b-1 : bs))
        inner _ (('?' : _) : as) [] = mem M.! (False, as, [])
        inner c ("" : as) bs = mem M.! (c, as, bs)
        preceding :: Integral a => a -> [a]
        preceding 0 = [0]
        preceding a = a : preceding (a-1)
        precedingList :: Integral a => [a] -> [[a]]
        precedingList (a:as) = (:as) <$> preceding a
        precedingList [] = [[]]

calc :: Integral a => Read a => String -> [a]
calc = fmap (uncurry process . bimap (concat . intersperse "?" . replicate 5) (unfoldr inner . concat . intersperse "," . replicate 5 . drop 1) . break (==' ')) . lines
    where
        inner :: Integral a => Read a => String -> Maybe (a, String)
        inner "" = Nothing
        inner s = Just $ bimap read (drop 1) $ break (==',') s

main = do
    text <- hGetContents stdin
    let solLines = calc text
    traverse print solLines
    print $ sum solLines
