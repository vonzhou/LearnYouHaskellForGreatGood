
negativeNums :: Num a => [a] -> [a]
negativeNums xs = map (\x -> negate (abs x)) xs

negativeNums2 :: Num a => [a] -> [a]
negativeNums2 xs = map (negate .abs) xs
