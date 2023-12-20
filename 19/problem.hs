import Prelude hiding (LT, GT)
import Data.Char
import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import System.IO

data Property = X | M | A | S deriving (Show, Eq, Read, Ord)
data Comparison = GT | LT deriving (Show, Eq, Read, Ord)
data Rule a = Condition {prop :: Property, comp :: Comparison, num :: a, dest :: String} | Default {dest :: String} deriving (Show, Eq, Read, Ord)
data Part a = Part {x :: a, m :: a, a :: a, s :: a} deriving (Show, Eq, Read, Ord)
getProp :: Property -> Part a -> a
getProp = fromJust . flip lookup [(X, x), (M, m), (A, a), (S, s)]
toProp :: Char -> Property
toProp = fromJust . flip lookup [('x', X), ('m', M), ('a', A), ('s', S)]
getComp :: Ord a => Comparison -> a -> a -> Bool
getComp = fromJust . flip lookup [(GT, (>)), (LT, (<))]
toComp :: Char -> Comparison
toComp = fromJust . flip lookup [('<', LT), ('>', GT)]
fulfills :: Integral a => Part a -> Rule a -> Bool
fulfills p (Condition pr c n _) = getComp c (getProp pr p) n
sumPart :: Integral a => Part a -> a
sumPart (Part x m a s) = x + m + a + s
split :: Eq a => a -> [a] -> [[a]]
split x =  unfoldr (inner x)
    where
        inner _ [] = Nothing
        inner x xs = Just $ bimap id (drop 1) (break (==x) xs)
toPart :: [a] -> Part a
toPart (x : m : a : s : []) = Part x m a s
readRule :: Integral a => Read a => String -> (String, [Rule a])
readRule = bimap id (fmap inner . split ',' . drop 1) . break (=='{')
    where
        inner :: Read a => String -> Rule a
        inner s
            | isSuffixOf "}" s = Default (init s)
            | otherwise = parseCondition (break (==':') s)
        parseCondition :: Read a => (String, String) -> Rule a
        parseCondition (p : c : n, ':' : d) = Condition (toProp p) (toComp c) (read n) d

process :: Integral a => Map String [Rule a] -> String -> Part a -> Bool
process m s p = inner m (fromJust $ M.lookup s m) p
    where
        goto :: Integral a => Map String [Rule a] -> Part a -> String -> Bool
        goto _ _ "R" = False
        goto _ _ "A" = True
        goto m p s = process m s p
        inner :: Integral a => Map String [Rule a] -> [Rule a] -> Part a -> Bool
        inner m (Default s : _) p = goto m p s
        inner m (r@(Condition _ _ _ s) : n) p
            | fulfills p r = goto m p s
            | otherwise = inner m n p

calc :: Integral a => Read a => String -> a
calc = uncurry inner . bimap ( M.fromList . fmap readRule) (fmap (toPart . fmap (read . filter isDigit) . split ',') . drop 1) . break (=="") . lines
    where
        inner :: Integral a => Map String [Rule a] -> [Part a] -> a
        inner m = sum . fmap (sumPart) . filter (process m "in")

main = do
    text <- hGetContents stdin
    print $ calc text
