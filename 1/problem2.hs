{-# LANGUAGE ViewPatterns #-}
import System.IO
import Data.Char
import Data.List
import Data.Maybe

prefix :: Eq a => [a] -> [a] -> Maybe [a]
prefix = stripPrefix

recover :: Integral a => Read a => Show a => String -> a
recover = (pure (+) <*> (*10) . head <*> last) . digits
    where
        getDigit :: Integral a => Read a => String -> Maybe a
        getDigit (prefix "nine" -> Just _) = Just 9
        getDigit (prefix "eight" -> Just _) = Just 8
        getDigit (prefix "seven" -> Just _) = Just 7
        getDigit (prefix "six" -> Just _) = Just 6
        getDigit (prefix "five" -> Just _) = Just 5
        getDigit (prefix "four" -> Just _) = Just 4
        getDigit (prefix "three" -> Just _) = Just 3
        getDigit (prefix "two" -> Just _) = Just 2
        getDigit (prefix "one" -> Just _) = Just 1
        getDigit (x : _)
            | isDigit x = Just $ read [x]
            | otherwise = Nothing
        getDigit [] = Nothing
        digits :: Integral a => Read a => String -> [a]
        digits = mapMaybe getDigit . tails

process :: Integral a => Show a => Read a => String -> a
process s = sum $ recover <$> lines s

main = do
    input <- hGetContents stdin
    hPrint stdout $ process input
