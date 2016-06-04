import System.IO

main = do
  todoItem <- getLine 
  appendFile "todo.txt" (todoItem ++ "\n")
