data Person = Person {firstName::String,
                      lastName:: String,
                      age::Int
                      } deriving (Show, Eq, Read)

vonzhou = Person {firstName = "von", lastName = "zhou", age = 88}
luyna = Person {firstName = "yu", lastName = "nan", age = 33}
nuan = Person {firstName = "yu", lastName = "nan", age = 33}

mysteryDude = "Person {firstName = \"Vonzhou\"" ++
              ",lastName = \"zhou\"" ++
              ",age = 23}"




