import Data.Char
import Data.List

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1 ..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]
