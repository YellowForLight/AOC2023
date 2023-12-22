import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.Containers.ListUtils
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import System.IO

thrd :: (a, b, c) -> c
thrd (a, b, c) = c

data Direction = X | Y | Z deriving (Eq, Ord, Show, Read)
data Block a = Block {getX :: a, getY :: a, getZ :: a, dir :: Direction, size :: a} | Floor deriving (Eq, Ord, Show, Read)
getDir :: Integral a => Direction -> (a, a, a)
getDir = fromJust . flip lookup [(X, (1, 0, 0)), (Y, (0, 1, 0)), (Z, (0, 0, 1))]
mulS :: Integral a => a -> (a, a, a) -> (a, a, a)
mulS s (x, y, z) = (s*x, s*y, s*z)
addT :: Integral a => (a, a, a) -> (a, a, a) -> (a, a, a)
addT (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)
getRange :: Integral a => Block a -> [(a, a, a)]
getRange (Block x y z d s) = fmap (addT (x, y, z) . flip mulS (getDir d)) [0..s]

parse :: Integral a => Read a => String -> Block a
parse = g . join bimap (f . bimap read (bimap read (read . drop 1) . break (==',') . drop 1) . break (==',')) . bimap id (drop 1) . break (=='~')
    where
        f :: (a, (b, c)) -> (a, b, c)
        f (a, (b, c)) = (a, b, c)
        g :: Integral a => ((a, a, a), (a, a, a)) -> Block a
        g ((x1, y1, z1), (x2, y2, z2)) 
            | x1 < x2 = Block x1 y1 z1 X (x2-x1)
            | x2 < x1 = Block x2 y1 z1 X (x1-x2)
            | y1 < y2 = Block x1 y1 z1 Y (y2-y1)
            | y2 < y1 = Block x1 y2 x1 Y (y1-y2)
            | z1 < z2 = Block x1 y1 z1 Z (z2-z1)
            | z2 < z1 = Block x1 y1 z2 Z (z1-z2)
            | otherwise = Block x1 y1 z1 X 0

dropBlocks :: Integral a => Block a -> (Map (a,a,a) (Block a), Map (Block a) [Block a]) -> (Map (a,a,a) (Block a), Map (Block a) [Block a])
dropBlocks b (m1, m2) 
    | any (flip M.member m1 . addT (0, 0, -1)) (getRange b) = (foldr (flip M.insert b) m1 (getRange b), M.insert b (getBelow b) m2)
    | any ((==1) . thrd) (getRange b) = (foldr (flip M.insert b) m1 (getRange b), M.insert b [Floor] m2)
    | otherwise = dropBlocks (dropping b) (m1, m2)
    where
        getBelow = nubOrd . mapMaybe (flip M.lookup m1 . addT (0, 0, -1)) . getRange
        dropping (Block x y z d s) = Block x y (z-1) d s

safe :: Ord b => Integral a => Map (Block b) [Block b] -> a
safe m = sum $ fmap (wouldFall . S.singleton) (M.keys m)    
    where
        wouldFall bs
            | all (flip S.member bs) unstable = genericLength unstable
            | otherwise = wouldFall (S.union bs (S.fromList unstable))
            where
                unstable = M.keys $ M.filter (all (flip S.member bs)) m

calc :: Integral a => String -> a
calc = safe . snd . foldr dropBlocks (M.empty, M.empty) . sortOn ((* (-1)) . getZ) . fmap parse . lines

main = do
    text <- hGetContents stdin
    print $ calc text
