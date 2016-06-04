--P 205
--
import System.Environment
import Data.List


main = do 
  args <- getArgs 
  progName <- getProgName
  putStrLn "The args are:"
  mapM putStrLn args
  putStrLn "The proramme name is:"
  putStrLn progName
