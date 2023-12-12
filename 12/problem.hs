import Data.List
import Data.Maybe
import Data.Bifunctor
import System.IO

process :: Integral a => String -> [a] -> a
process = (genericLength .) . (.(==)) . flip filter . fmap (fmap genericLength . filter head . group) . traverse (fromJust . flip lookup [('.', [False]), ('#', [True]), ('?', [False, True])])

calc :: Integral a => Read a => String -> a
calc = sum . fmap (uncurry process . bimap id (unfoldr inner . drop 1) . break (==' ')) . lines
    where
        inner :: Integral a => Read a => String -> Maybe (a, String)
        inner "" = Nothing
        inner s = Just $ bimap read (drop 1) $ break (==',') s

main = do
    text <- hGetContents stdin
    print $ calc text
