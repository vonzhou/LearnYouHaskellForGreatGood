import System.IO
main= do
  handle <- openFile "girlfriends.txt" ReadMode
  contents <-   hGetContents handle 
  putStr contents 
  hClose handle 
