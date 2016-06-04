import Data.List
import Control.Monad

solveRPN :: String -> Double 
solveRPN = head . foldl foldingFunction [] . words

foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x:y:xs) "*" = (y * x) : xs
foldingFunction (x:y:xs) "+" = (x + y) : xs
foldingFunction (x:y:xs) "-" = (y - x) : xs
foldingFunction (x:y:xs) "/" = (y / x) : xs
foldingFunction (x:y:xs) "^" = (y ** x) : xs
foldingFunction (x:xs) "ln" = (log x) : xs
foldingFunction xs "sum" = [sum xs]
foldingFunction stack numberString = read numberString : stack 

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x:y:xs) "*" = return ((y * x) : xs)
foldingFunction' (x:y:xs) "+" = return ((x + y) : xs)
foldingFunction' (x:y:xs) "-" = return ((y - x) : xs)
foldingFunction' (x:y:xs) "/" = return ((y / x) : xs)
foldingFunction' (x:y:xs) "^" = return ((y ** x) : xs)
foldingFunction' stack numberString = liftM (:stack) (readMaybe numberString) 


solveRPN' :: String -> Maybe Double 
solveRPN' st = do
  [result] <- foldM foldingFunction' [] (words st)
  return result
