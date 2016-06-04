import Control.Monad 
import Data.Char

main =  forever $ do 
  l <- getLine 
  putStrLn $ map toUpper l
