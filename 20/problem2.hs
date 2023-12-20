import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S
import Control.Monad
import System.IO

-- Could be a lot cleaner as there are a lot of thing from part 1 that I could have deleted.

data Module = FlipFlop {statef :: Bool, output :: [String]} | Conjunction {statec :: Map String Bool, fired :: Bool, output :: [String]} | Testing | Rx {n :: Int} | Broadcast {output :: [String]} deriving (Show, Eq, Ord, Read)

process :: Integral a => Map String Module -> String -> a
process m = inner . (m M.!) 
    where
        isC :: String -> Bool
        isC = other . (m M.!)
            where
                other (FlipFlop _ _) = False
                other _ = True
        inner :: Integral a => Module -> a
        inner (FlipFlop _ out)
            | all isC out = 1
            | any isC out = 1 + 2 * (sum $ fmap (process m) out)
            | otherwise = 2 * (sum $ fmap (process m) out)
        inner _ = 0

initM :: Map String Module -> Map String Module
initM = inner (S.singleton ("broadcaster", "abc"))
    where
        inner (S.Empty) m = m
        inner ((c, p) :<| n) m = other (m M.!? c)
            where
                other Nothing = inner n (M.insert c Testing m)
                other (Just (Rx _)) = inner n m
                other (Just (Broadcast out)) = inner (n >< S.fromList (fmap (,c) out)) m
                other (Just Testing) = inner n m
                other (Just (FlipFlop False out)) = inner n m
                other (Just (FlipFlop True out)) = inner (n >< S.fromList (fmap (,c) out)) (M.insert c (FlipFlop False out) m)
                other (Just (Conjunction st f out))
                    | M.member p st = inner n m
                    | M.null st = inner (n >< S.fromList (fmap (,c) out)) (M.insert c (Conjunction (M.insert p False st) False out) m)
                    | otherwise = inner n (M.insert c (Conjunction (M.insert p False st) False out) m)

parse :: String -> Map String Module
parse = foldMap parseLine . lines
    where
        parseLine :: String -> Map String Module
        parseLine s
            | "broadcaster" `isPrefixOf` s = M.singleton "broadcaster" (Broadcast out)
            | "%" `isPrefixOf` s = M.singleton name (FlipFlop True out)
            | "&" `isPrefixOf` s = M.singleton name (Conjunction M.empty False out)
            where
                (name, out) = bimap (drop 1 . init) (words . filter (/=',') . drop 3) $ break (=='-') s

addT :: Integral a => (a, a) -> (a, a) -> (a, a)
addT (a, b) (c, d) = (a + c, b + d)
mulT :: Integral a => (a, a) -> a
mulT (a, b) = a * b
mulS :: Integral a => a -> (a, a) -> (a, a)
mulS a (b, c) = (a*b, a*c)
thrd :: (a, b, c) -> c
thrd (a, b, c) = c

calc :: Integral a => String -> a
calc = foldr lcm 1 . join (traverse (flip process) . output . (M.! "broadcaster")) . parse

main = do
    text <- hGetContents stdin
    print $ calc text
