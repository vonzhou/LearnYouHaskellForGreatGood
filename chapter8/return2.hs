
main =  do 
  a <- return "helllo"
  b <- return "vonzhou"
  putStrLn $ a ++ " " ++ b
