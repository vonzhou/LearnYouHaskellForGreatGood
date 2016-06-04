-- first name, last name, age, height, phoneNumber, flavor
data Person1 = Person1 String String Int Float String String deriving (Show)


firstName' :: Person1 -> String
firstName' (Person1 firstname _ _ _ _ _) = firstname

data Person = Person {firstName::String,
                      lastName:: String,
                      age::Int,
                      height::Float,
                      phoneNumber::String,
                      flavor::String 
                      } deriving (Show, Eq)

vonzhou = Person {firstName = "von", lastName = "zhou", age = 88}
luyna = Person {firstName = "yu", lastName = "nan", age = 33}
nuan = Person {firstName = "yu", lastName = "nan", age = 33}






