

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!!"
lucky x = "Sorry, you are out of luck, pall!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n - 1))


charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x
               ++ " and " ++ show y


firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "Your are underweight, you emo , you!"
  | bmi <= 25.0 = "You are supposed normal, Pfft , I bet you ugly"
  | bmi <= 30.0 = "You are fat, lose some weight, fatty!"
  | otherwise = "You are a whale , congratulations!"


bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
  | weight / height ^ 2 <= 18.5 = "Your are underweight, you emo , you!"
  | weight / height ^ 2 <= 25.0 = "You are supposed normal, Pfft , I bet you ugly"
  | weight / height ^ 2 <= 30.0 = "You are fat, lose some weight, fatty!"
  | otherwise = "You are a whale , congratulations!"


max' :: Ord a => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

myCompare :: Ord a => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a < b =  LT
  | otherwise = GT


bmiTell3 :: Double -> Double -> String
bmiTell3 weight height
  | bmiT <= 18.5 = "Your are underweight, you emo , you!"
  | bmiT <= 25.0 = "You are supposed normal, Pfft , I bet you ugly"
  | bmiT <= 30.0 = "You are fat, lose some weight, fatty!"
  | otherwise = "You are a whale , congratulations!"
  where bmiT = weight / height ^ 2

niceGreeting :: String
niceGreeting =  "Hello1 So very nice to meet you !"
badGreeting = " Oh Pfft It 's you'"
greet :: String -> String
greet "Juan" = niceGreeting  ++ "Juan!"
greet "Vonzhou" = niceGreeting  ++ "Juan!"
greet name =  badGreeting ++ name

initials :: String -> String -> String 
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname 
        (l:_) = lastname 


calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height  = weight / height ^ 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

cylinder :: Double -> Double -> Double 
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea 

headList :: [a] -> a
headList [] =  error "No head element for empty list"
headList (x:_) = x

headList2 :: [a] -> a
headList2 xs =  case xs of [] -> error "No head element for empty list"
                           (x:_) -> x



describeList :: [a] -> String 
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "singleton."
                                               xs -> "a longer list."

describeList2 :: [a] -> String 
describeList2 ls = "The list is " ++ what ls
  where what [] = "empty"
        what [x] = "singleton"
        what xs = "a longer list"


