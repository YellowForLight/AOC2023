import Data.List
import System.IO

count :: Integral a => [Char] -> a
count as = fst $ foldl inner (0, genericLength as) $ zip [0..] as
    where
        inner :: Integral a => (a, a) -> (a, Char) -> (a, a)
        inner (a, b) (_, '.') = (a, b)
        inner (a, b) (_, 'O') = (a+b, b-1)
        inner (a, _) (i, '#') = (a, genericLength as - i - 1)

calc :: Integral a => Show a => String -> a
calc = sum . fmap count . transpose . lines

main = do
    text <- hGetContents stdin
    print $ calc text
