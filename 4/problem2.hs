import Data.List
import Data.Bifunctor
import Data.Tuple
import Control.Monad
import System.IO

process :: Integral a => String -> a
process = genericLength . uncurry intersect . join bimap words . bimap id (drop 1) . break (=='|') . drop 1 . snd . break (==':')

calc :: Integral a => String -> a
calc = fst . foldl fancy (0, []) . fmap process . lines
    where
        fancy :: Integral a => (a, [a]) -> a -> (a, [a])
        fancy (a, bs) n = (n*c+1 + a, filter (/=0) $ genericReplicate c n ++ ((+(-1)) <$> bs))
            where c = 1 + genericLength bs

main = do
    text <- hGetContents stdin
    hPrint stdout $ calc text
