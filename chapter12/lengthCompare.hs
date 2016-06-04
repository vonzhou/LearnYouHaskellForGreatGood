import Data.Monoid

-- 看元音字符的个数
lengthCompare :: String -> String -> Ordering
lengthCompare   x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend` 
                      (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")


