
sum' :: Num a => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'2 :: Num a => [a] -> a
sum'2 = foldl (+) 0 

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

map'2 :: (a -> b) -> [a] -> [b]
map'2 f xs = foldl (\acc x -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'2 :: [a] -> [a]
reverse'2 = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x) 

and' :: [Bool] -> Bool 
and' xs = foldr (&&) True xs

sqrtNums :: Int
sqrtNums = length(takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
