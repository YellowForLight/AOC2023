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
data Part a = Part {x :: (a, a), m :: (a, a), a :: (a, a), s :: (a, a)} deriving (Show, Eq, Read, Ord)
getProp :: Property -> Part a -> (a,a)
getProp = fromJust . flip lookup [(X, x), (M, m), (A, a), (S, s)]
withProp :: Property -> (a, a) -> Part a -> Part a
withProp X x (Part _ m a s) = Part x m a s
withProp M m (Part x _ a s) = Part x m a s
withProp A a (Part x m _ s) = Part x m a s
withProp S s (Part x m a _) = Part x m a s
toProp :: Char -> Property
toProp = fromJust . flip lookup [('x', X), ('m', M), ('a', A), ('s', S)]
getComp :: Ord a => Comparison -> a -> a -> Bool
getComp = fromJust . flip lookup [(GT, (>)), (LT, (<))]
toComp :: Char -> Comparison
toComp = fromJust . flip lookup [('<', LT), ('>', GT)]
sumRange :: Integral a => Part a -> a
sumRange (Part x m a s) = num x * num m * num a * num s
    where
        num :: Integral a => (a, a) -> a
        num (a, b) = 1 + b - a
getRange :: Integral a => Part a -> Rule a -> (Maybe (Part a), Maybe (Part a))
getRange p (Default _) = (Just p, Nothing)
getRange p r@(Condition pr c n d) = inner (getProp pr p)
    where
        comp :: Ord a => a -> a -> Bool
        comp = getComp c
        inner (low, high)
            | comp low n && comp high n = (Just p, Nothing)
            | comp low n = (Just (withProp pr (low, n-1) p), Just (withProp pr (n, high) p))
            | comp high n = (Just (withProp pr (n+1, high) p), Just (withProp pr (low, n) p))
            | otherwise = (Nothing, Just p)
split :: Eq a => a -> [a] -> [[a]]
split x =  unfoldr (inner x)
    where
        inner _ [] = Nothing
        inner x xs = Just $ bimap id (drop 1) (break (==x) xs)
readRule :: Integral a => Read a => String -> (String, [Rule a])
readRule = bimap id (fmap inner . split ',' . drop 1) . break (=='{')
    where
        inner :: Read a => String -> Rule a
        inner s
            | isSuffixOf "}" s = Default (init s)
            | otherwise = parseCondition (break (==':') s)
        parseCondition :: Read a => (String, String) -> Rule a
        parseCondition (p : c : n, ':' : d) = Condition (toProp p) (toComp c) (read n) d

process :: Integral a => Map String [Rule a] -> String -> Part a -> [(Part a, Bool)]
process m s p = inner m (fromJust $ M.lookup s m) (Just p)
    where
        goto :: Integral a => Map String [Rule a] -> Maybe (Part a) -> String -> [(Part a, Bool)]
        goto _ Nothing _ = []
        goto _ (Just p) "R" = pure (p, False)
        goto _ (Just p) "A" = pure (p, True)
        goto m (Just p) s = process m s p
        inner :: Integral a => Map String [Rule a] -> [Rule a] -> Maybe (Part a) -> [(Part a, Bool)]
        inner _ _ Nothing = []
        inner m (Default s : _) p = goto m p s
        inner m (r@(Condition _ _ _ s) : n) (Just p) = other (getRange p r)
            where
                other (p1, p2) = goto m p1 s ++ inner m n p2

calc :: Integral a => Read a => String -> a
calc = inner . M.fromList . fmap readRule . fst . break (=="") . lines
    where
        inner :: Integral a => Map String [Rule a] -> a
        inner m = sum $ fmap (sumRange . fst) $ filter snd $ process m "in" (Part (1,4000) (1,4000) (1,4000) (1,4000))

main = do
    text <- hGetContents stdin
    return ()
    print $ calc text
