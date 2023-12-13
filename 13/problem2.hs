import Data.List
import Data.Bifunctor
import System.IO

data Zipper a = Zipper [a] [a]
forward :: Zipper a -> Maybe (Zipper a)
forward (Zipper _ []) = Nothing
forward (Zipper _ (r:[])) = Nothing
forward (Zipper ls (r : rs)) = Just $ Zipper (r : ls) rs
numAbove :: Integral a => Zipper b -> a
numAbove (Zipper ls _) = genericLength ls
isReflect :: Eq b => Zipper [b] -> Bool
isReflect (Zipper ls rs) = sum (fmap (sum . fmap fromEnum) $ zipWith (zipWith (/=)) ls rs) == 1
zipify :: [a] -> Zipper a
zipify = Zipper []
reset :: Zipper a -> [a]
reset (Zipper ls rs) = reverse ls ++ rs

process :: Integral a => Eq b => Zipper [b] -> a
process zipper = inner (forward zipper)
    where
        inner (Just z)
            | isReflect z = 100 * numAbove z
            | otherwise = process z
        inner Nothing = flip quot 100 $ process (zipify $ transpose $ reset zipper)

calc :: Integral a => String -> a
calc = sum . fmap (process . zipify) . unfoldr split . lines
    where
        split :: [String] -> Maybe ([String], [String])
        split [] = Nothing
        split s = Just $ bimap id (drop 1) (break (=="") s) 

main = do
    text <- hGetContents stdin
    print $ calc text
