import System.Random
import Control.Monad(when)

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 do you think ?"
  numberString <- getLine 
  when (not $ null numberString) $ do
    let number = read numberString 
    if randNumber == number 
      then putStrLn "Your are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    newStdGen
    main 
