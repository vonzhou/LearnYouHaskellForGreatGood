
main = do
  a <- (++) <$> getLine <*> getLine 
  putStrLn $ "The two lines concatenated turned out to be:" ++ a

