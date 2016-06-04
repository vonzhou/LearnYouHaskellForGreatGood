import System.IO
main= do
  withFile "girlfriends.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents )
