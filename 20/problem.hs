import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S
import System.IO

data Module = FlipFlop {statef :: Bool, outputf :: [String]} | Conjunction {statec :: Map String Bool, outputc :: [String]} | Testing | Broadcast {output :: [String]} deriving (Show, Eq, Ord, Read)

process :: Integral a => (a, a) -> Map String Module -> Seq (String, String, Bool) -> (a, a, Map String Module)
process (n1, n2) m S.Empty = (n1, n2, m)
process (a1, a2) m (q@(s, p, b) :<| n) = sendSignal (m M.! s)
    where
        nextSum = (a1 + fromIntegral (fromEnum b), a2 + fromIntegral (fromEnum (not b)))
        sendSignal (Broadcast out) = process nextSum m (n >< S.fromList (fmap (,s,b) out))
        sendSignal Testing = process nextSum m n
        sendSignal (FlipFlop st out)
            | b = process nextSum m n
            | otherwise = process nextSum (M.insert s (FlipFlop (not st) out) m) (n >< S.fromList (fmap (,s,not st) out))
        sendSignal (Conjunction st out) = sendConjunction (Conjunction (M.insert p b st) out)
        sendConjunction c@(Conjunction st out)
            | and st = process nextSum (M.insert s c m) (n >< S.fromList (fmap (,s,False) out))
            | otherwise = process nextSum (M.insert s c m) (n >< S.fromList (fmap (,s,True) out))

initM :: Map String Module -> Map String Module
initM = inner (S.singleton ("broadcaster", "abc"))
    where
        inner (S.Empty) m = m
        inner ((c, p) :<| n) m = other (m M.!? c)
            where
                other Nothing = inner n (M.insert c Testing m)
                other (Just (Broadcast out)) = inner (n >< S.fromList (fmap (,c) out)) m
                other (Just Testing) = inner n m
                other (Just (FlipFlop False out)) = inner n m
                other (Just (FlipFlop True out)) = inner (n >< S.fromList (fmap (,c) out)) (M.insert c (FlipFlop False out) m)
                other (Just (Conjunction st out))
                    | M.member p st = inner n m
                    | M.null st = inner (n >< S.fromList (fmap (,c) out)) (M.insert c (Conjunction (M.insert p False st) out) m)
                    | otherwise = inner n (M.insert c (Conjunction (M.insert p False st) out) m)

parse :: String -> Map String Module
parse = foldMap parseLine . lines
    where
        parseLine :: String -> Map String Module
        parseLine s
            | "broadcaster" `isPrefixOf` s = M.singleton "broadcaster" (Broadcast out)
            | "%" `isPrefixOf` s = M.singleton name (FlipFlop True out)
            | "&" `isPrefixOf` s = M.singleton name (Conjunction M.empty out)
            where
                (name, out) = bimap (drop 1 . init) (words . filter (/=',') . drop 3) $ break (=='-') s

addT :: Integral a => (a, a) -> (a, a) -> (a, a)
addT (a, b) (c, d) = (a + c, b + d)
mulT :: Integral a => (a, a) -> a
mulT (a, b) = a * b
mulS :: Integral a => a -> (a, a) -> (a, a)
mulS a (b, c) = (a*b, a*c)


pushButton :: Integral a => Map String Module -> a
pushButton = inner [] []
    where
        inner :: Integral a => [(a, a)] -> [Map String Module] -> Map String Module -> a
        inner ns ms m 
            | genericLength ms == 1000 = mulT $ foldr addT (0, 0) ns
            | m `elem` ms = other m ms (fmap (bimap fromIntegral fromIntegral) ns)
            | otherwise = inner ((n1, n2) : ns) (m : ms) m2
            where
                (n1, n2, m2) = process (0, 0) m (S.singleton ("broadcaster", "", False))
        other m ms ns = fromIntegral $ mulT z
            where
                a = fromJust (elemIndex m ms) + 1
                b = length ms - a
                c = 1000 - b
                (d, e) = c `quotRem` a 
                (f, g) = splitAt a ns
                (h, i) = splitAt (a-e) f
                j = mulS d (foldr addT (0, 0) h)
                k = mulS (d+1) (foldr addT (0, 0) i)
                l = foldr addT (0, 0) g
                z = addT l (addT j k)

calc :: Integral a => String -> a
calc = pushButton . initM . parse

main = do
    text <- hGetContents stdin
    print $ calc text
