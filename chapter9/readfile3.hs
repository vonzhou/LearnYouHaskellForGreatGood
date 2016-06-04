import System.IO
main= do
  contents <-   readFile "girlfriends.txt" 
  putStr contents 
