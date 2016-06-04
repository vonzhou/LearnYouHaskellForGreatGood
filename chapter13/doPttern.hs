justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hvonzhou"
  return x

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

