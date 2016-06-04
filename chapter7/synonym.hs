type PhoneNumber = String
type Name = String

type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number pbook = (name, number) `elem` pbook 

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

