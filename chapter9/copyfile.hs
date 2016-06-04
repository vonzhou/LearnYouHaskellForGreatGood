import System.Environment
import System.IO
import Control.Exception
import System.Directory
import qualified Data.ByteString.Lazy as B


main = do
  (filename1:filename2:_) <- getArgs
  copy filename1 filename2

copy source dest = do
  contents <- B.readFile source
  bracketOnError
    (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle 
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      B.hPutStr tempHandle contents 
      hClose tempHandle 
      renameFile tempName dest)


