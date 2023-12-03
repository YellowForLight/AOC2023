import Data.List
import Data.Char
import Data.Bifunctor
import Data.Tuple
import Data.Traversable
import Control.Applicative
import Control.Monad
import System.IO

filterSymbol :: Char -> Bool
filterSymbol c 
    | isDigit c = False
    | c == '.' = False
    | otherwise = True

filterDigit :: Char -> Char
filterDigit c
    | isDigit c = c
    | otherwise = ' '

combine3 :: [Bool] -> Bool
combine3 = foldl (||) False . take 3

surround :: [Bool] -> [Bool]
surround = init . init . fmap combine3 . tails . (False:)

hyperSurround :: [[Bool]] -> [[Bool]]
hyperSurround = getZipList . traverse (ZipList . surround) . getZipList . traverse (ZipList . surround)

parse :: Integral a => Read a => [(Char, Bool)] -> [a]
parse = fmap read . dropWhile (=="") . fmap fst . foldr inner [("", False)]
    where
        inner :: (Char, Bool) -> [(String, Bool)] -> [(String, Bool)]
        inner (' ', _) ((s, t) : xs)
            | t = ("", False) : (s, True) : xs
            | otherwise = ("", False) : xs
        inner (c, a) ((s, t) : xs) = (c : s, t || a) : xs

calc :: Integral a => Read a => String -> a
calc = sum . fmap sum . fmap parse . fancyZip . bimap (fmap2 filterDigit) (hyperSurround . fmap2 filterSymbol) . join (,) . map ('.':) . lines 
    where
        fmap2 f a = fmap (fmap f) a
        fancyZip :: ([[a]], [[b]]) -> [[(a, b)]]
        fancyZip = fmap partZip . partZip
        partZip :: ([a], [b]) -> [(a, b)]
        partZip = uncurry zip


main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
