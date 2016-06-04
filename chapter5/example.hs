multiThree :: Int -> Int -> Int -> Int 
multiThree x y z =  x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred2 :: Int -> Ordering
compareWithHundred2 = compare 100 

divideByTen :: Floating a => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool 
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where g x y = f y x

flip'2 :: (a -> b -> c) -> b -> a -> c
flip'2 f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallOrEqual = filter (<=x) xs
      larger = filter (>x) xs
  in quicksort smallOrEqual ++ [x] ++ quicksort larger 

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3820 == 0

--Collatz Sequence 
chain :: Integer -> [Integer]
chain 1 = [1]
chain x 
  | even x = x : chain (x `div` 2)
  | odd x = x : chain (x * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

sum' :: Num a => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'2 :: Num a => [a] -> a
sum'2 = foldl (+) 0 
