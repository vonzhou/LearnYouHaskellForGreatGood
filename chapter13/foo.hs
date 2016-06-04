foo :: Maybe String
foo = Just 3 >>= (\x ->
  Just "!" >>= (\y -> 
  Just (show x ++ y)))
