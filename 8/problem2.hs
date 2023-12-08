import Data.List
import Data.Bifunctor
import Data.Maybe
import Control.Monad
import System.IO

parse :: String -> (String, (String, String))
parse = bimap (take 3) inner . break (=='=')
    where
        inner :: String -> (String, String)
        inner = pure (,) <*> take 3 . drop 3 <*> take 3 . drop 8

-- I know that technically I also have to check that the distance from the node ending in Z to itself is the same, it goes through no other nodes in between and this distance is divisible by the period of the directions or do something more compliciated if these are false, but this seems to work anyway. I looked through the first few elements the filter returned earlier, saw that they were periodic and assumed that was meant to be part of the question
process :: Integral a => Show a => String -> a
process = foldl lcm 1 . fmap (head . fmap fst . filter (isSuffixOf "Z" . snd) . zip [0..]) . uncurry (flip moveAll) . bimap (concat . repeat . head) (fmap parse . drop 1) . break (=="") . lines
    where
        moveAll :: [(String, (String, String))] -> [Char] -> [[String]]
        moveAll = join $ (sequence .) . traverse (flip . move) . filter ("A" `isSuffixOf`) . fmap fst
        move :: String -> [Char] -> [(String, (String, String))] -> [String]
        move a ('L' : bs) c = a : move (fst $ fromJust $ lookup a c) bs c
        move a ('R' : bs) c = a : move (snd $ fromJust $ lookup a c) bs c

main = do
    text <- hGetContents stdin
    hPrint stdout $ process text
