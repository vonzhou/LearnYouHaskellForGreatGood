import System.IO
import System.Directory
import System.Environment
import Data.List

main = do
  (command:argList) <- getArgs
  dispatch command argList 

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove 
dispatch command = doesntExist command 

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command usage : todo add [file name ] [to do item]"

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ "-" ++ line) 
                              [0..] todoTasks  
  putStr $ unlines numberedTasks 

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName 
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ "-" ++ line)
                              [0..] todoTasks 
  let number = read numberString 
      newTodoTasks = unlines $ delete (todoTasks !! number) todoTasks 
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newTodoTasks 
  hClose tempHandle 
  removeFile "todo.txt"
  renameFile tempName "todo.txt"


-- TODO: bump
-- get the spec line, delte that line and reconstruct a file

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command does not exist."
