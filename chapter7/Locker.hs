import qualified Data.Map as Map

--data Either a b = Left a | Right b deriving (Eq, Ord, Show, Read)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of 
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " does not exist!"
  Just (state, code) -> if state /= Taken 
                        then Right code
                        else Left $ "Locker "  ++ show lockerNumber ++ " is already taken!"

locker :: LockerMap 
locker = Map.fromList
  [
  (100, (Taken, "ABC")),
  (101, (Free, "DBDD")),
  (102, (Free, "SFE777")),
  (103, (Taken, "332NN"))
  ]
