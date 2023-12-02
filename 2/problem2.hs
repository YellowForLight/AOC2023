{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.Bifunctor
import Data.Char
import Control.Monad
import Control.Applicative
import System.IO

parse :: Integral a => Read a => String -> ZipList a
parse = fmap maximum . traverse (sumFold . fmap (join process) . split ',') . split ';'
    where
        split :: Char -> String -> [String]
        split _ [] = []
        split c s = let (st, en) = break (==c) s
                    in st : split c (drop 1 en)
        readStr :: Read a => String -> a
        readStr = read . filter (isDigit)
        process :: Read a => Integral a => String -> String -> ZipList a
        process (isSuffixOf "red" -> True) s = ZipList [readStr s,0,0]
        process (isSuffixOf "green" -> True) s = ZipList [0, readStr s,0]
        process (isSuffixOf "blue" -> True) s = ZipList [0, 0, readStr s]
        sumFold :: Integral a => [ZipList a] -> ZipList a
        sumFold = foldl (\a b -> pure (+) <*> a <*> b) $ pure (0)


ids :: Integral a => Read a => String -> a
ids  = product . getZipList . parse . drop 1 . snd . break (==':')
    where
        getId :: Read a => Integral a => String -> a
        getId (stripPrefix "Game " -> Just s) = read s

calc :: Integral a => Read a => String -> a
calc = sum . fmap ids . lines

main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
