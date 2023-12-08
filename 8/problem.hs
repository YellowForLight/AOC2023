import Data.List
import Data.Bifunctor
import Data.Maybe
import System.IO

parse :: String -> (String, (String, String))
parse = bimap (take 3) inner . break (=='=')
    where
        inner :: String -> (String, String)
        inner = pure (,) <*> take 3 . drop 3 <*> take 3 . drop 8

process :: Integral a => String -> a
process = uncurry (move "AAA") . bimap (concat . repeat . head) (fmap parse . drop 1) . break (=="") . lines
    where
        move :: Integral a => String -> [Char] -> [(String, (String, String))] -> a
        move "ZZZ" _ _ = 0
        move a ('L' : bs) c = 1 + move (fst $ fromJust $ lookup a c) bs c
        move a ('R' : bs) c = 1 + move (snd $ fromJust $ lookup a c) bs c

main = do
    text <- hGetContents stdin
    hPrint stdout $ process text
