import Data.Map as Map
import Data.Char

phoneBookList :: [(String,String)]
phoneBookList = 
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("penny", "123-4444")
   ]
phoneBook :: Map.Map String  String
phoneBook = Map.fromList $ phoneBookList

string2digits ::String -> [Int]
string2digits = Prelude.map digitToInt . Prelude.filter isDigit 


intBook :: Map.Map String [Int]
intBook = Map.map string2digits phoneBook

phoneBookToMap :: (Ord k) => [(k,String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ "," ++ number2 

-- First convert to list with one element, and then Cons lists if key is same
phoneBookToMap2 :: (Ord k) => [(k,v)] -> Map.Map k [v]
phoneBookToMap2 xs = Map.fromListWith (++) $ Prelude.map (\(k,v) -> (k,[v])) xs

