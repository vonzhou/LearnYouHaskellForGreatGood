import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub 

numWords :: String -> [(String, Int)]
numWords = map (\ws -> (head ws, length ws)) . group . sort . words 

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack =any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode offset msg = encode (negate offset) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1 ..]


firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> k == key) $ xs

findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey2 key [] = Nothing
findKey2 key ((k,v):xs) 
  | key == k = Just v
  | otherwise = findKey2 key xs



findKey3 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey3 key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs


