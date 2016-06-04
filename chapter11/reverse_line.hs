

main = do 
  line <- getLine 
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards"


