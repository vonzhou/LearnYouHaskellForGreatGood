doubleMe x = x + x

doubleUs x y = x *2 + y * 2

doubleSmallNumber x = if x > 100
                      then x
                      else x * 2


doubleSmallNumber' x = (if x > 100
                      then x
                      else x * 2) + 1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]


length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z']]

--let rightTriangles = [(a,b,c) | c <- [1 .. 10], a <- [1 .. c], b <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2, a+b+c == 24]
