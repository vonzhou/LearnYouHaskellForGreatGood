import Control.Monad

main = do 
  colors <- forM [1,2,3,4] (\a -> do
            putStrLn $ "what color do you associate with this number " ++ show a ++ "?"
            color <- getLine
            return color 
            )
  putStrLn "The colors you associate with them are:"
  --mapM putStrLn colors 
  print colors

