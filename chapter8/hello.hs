main = do
  foo <- putStrLn "Hello ,what is your name ?"
  name <-  getLine 
  putStrLn ("Hey " ++ name ++ ", you rock!!")

