import Control.Monad

type KnightPos = (Int, Int)

moveKnigt :: KnightPos -> [KnightPos]
moveKnigt (c,r) = do
  (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1), (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')


moveKnigt' :: KnightPos -> [KnightPos]
moveKnigt' (c,r) = filter onBoard
  [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1), (c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)]
  where onBoard (c', r') = c' `elem` [1 .. 8] && r' `elem` [1 .. 8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnigt start 
  second <- moveKnigt first 
  moveKnigt second 

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

