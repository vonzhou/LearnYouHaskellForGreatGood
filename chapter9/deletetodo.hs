import System.IO
import Data.List    -- delete 
import System.Directory   -- removeFile 


main = do 
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ "-" ++ line)
                              [0..] todoTasks 
  putStrLn "These are your TODO items:"
  mapM_ putStrLn numberedTasks 
  putStrLn "which one do you want to delete?"
  numberString <- getLine 
  let number = read numberString 
      newTodoTasks = unlines $ delete (todoTasks !! number) todoTasks 
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newTodoTasks 
  hClose tempHandle 
  removeFile "todo.txt"
  renameFile tempName "todo.txt"
