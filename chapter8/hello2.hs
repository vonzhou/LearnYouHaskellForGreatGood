import Data.Char

main = do 
  putStrLn "What is your first name?"
  first <- getLine 
  putStrLn "What is your last name? "
  last <- getLine 
  let bigFirst = map toUpper first 
      bigLast = map toUpper last 
  putStrLn $ "Hey, " ++ bigFirst ++  " "
          ++ bigLast ++ ",how are you?"


