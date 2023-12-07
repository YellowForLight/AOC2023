import Data.List
import Data.Maybe
import Data.Ord
import Data.Bifunctor
import System.IO

checkCards :: String -> String -> Ordering
checkCards = inner
    where 
        inner a b = fromMaybe (compare a b) (isOrdered $ typeCompare a b)
        isOrdered :: Ordering -> Maybe Ordering
        isOrdered EQ = Nothing
        isOrdered a = Just a
        typeCompare :: String -> String -> Ordering
        typeCompare = ( . count) . compare . count
        count :: String -> [Int]
        count s = (pure (:) <*> (+ length (filter (=='1') s)) . head <*> tail) . (++[0]) . sortOn Down . fmap length . group . sort . filter (/='1') $ s

rename :: Char -> Char
rename 'T' = 'A'
rename 'J' = '1'
rename 'Q' = 'C'
rename 'K' = 'D'
rename 'A' = 'E'
rename a = a

process :: Integral a => Read a=> String -> a
process = sum . zipWith (*) [1..] . fmap snd . sortBy ((. fst) . checkCards . fst) . fmap (bimap (fmap rename) (read . drop 1) . break (==' ')) . lines

main = do
    text <- hGetContents stdin
    hPrint stdout $ process text
