
myAction :: IO String
myAction = (++) <$> getLine <*> getLine 


