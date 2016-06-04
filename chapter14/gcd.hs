import Control.Monad.Writer

gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)


gcd'' ::  Int -> Int -> Writer [String] Int
gcd'' a b 
  | b == 0 = do
            tell ["Finished with " ++ show a]
            return a
  | otherwise = do
            tell [show a ++ " mod " ++ show b ++ "=" ++ show (a `mod` b)]
            gcd'' b (a `mod` b)


gcdReverse ::  Int -> Int -> Writer [String] Int
gcdReverse a b 
  | b == 0 = do
            tell ["Finished with " ++ show a]
            return a
  | otherwise = do
            result <- gcdReverse b (a `mod` b)
            tell [show a ++ " mod " ++ show b ++ "=" ++ show (a `mod` b)]
            return result
