import Data.List
import Data.Bifunctor
import Data.Maybe
import Data.Tuple
import Control.Monad
import System.IO

parse :: Integral a => [a] -> [a -> Maybe a]
parse [a, b, c] = [inner]
    where
        inner d
            | (d-b)<c && (d-b)>0 = Just $ d-b+a
            | otherwise = Nothing
        
process :: Integral a => ([a], [a -> Maybe a]) -> [a]
process = uncurry (foldl inner)
    where
        inner :: Integral a => [a] -> (a -> Maybe a) -> [a]
        inner as f = fmap (\a -> fromMaybe a $ f a) as

calc :: Integral a => Read a => String -> a
calc = minimum . process . bimap (fmap read . words . drop 1 . snd . break (==':') . head) (fmap (foldr concatMaybe (\_ -> Nothing)) . fmap2 (foldr concatMaybe (\_ -> Nothing) . parse . fmap read . words) . unfoldr split . drop 1) . break (=="") . lines
    where
        split :: [String] -> Maybe ([String], [String])
        split [] = Nothing
        split ss = Just $ join bimap (drop 1) $ break (=="") ss
        concatMaybe :: (a->Maybe b) -> (a -> Maybe b) -> a -> Maybe b
        concatMaybe f g a = case (f a) of
            Just b -> Just b
            Nothing -> g a
        fmap2 f = fmap (fmap f)

main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
