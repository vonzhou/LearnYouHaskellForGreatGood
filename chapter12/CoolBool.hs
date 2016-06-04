newtype CoolBool = CoolBool {getCoolBool :: Bool}
-- data CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String 
helloMe (CoolBool _) = "hello"


