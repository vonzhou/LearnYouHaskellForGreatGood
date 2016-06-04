main = do 
  line <- fmap reverse getLine 
  putStrLn $ "You said " ++ line ++ " backwards"
