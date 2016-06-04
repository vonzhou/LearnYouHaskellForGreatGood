import System.IO
import Data.Char

main = do 
  contents <- readFile "girlfriends.txt"
  writeFile "girlfriendscaps.txt" (map toUpper contents)

