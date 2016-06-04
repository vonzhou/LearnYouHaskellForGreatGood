import Control.Monad

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1,2]
  ch <- "ab"
  return (n, ch)

sevensOnly :: [Int]
sevensOnly = do 
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  return x
