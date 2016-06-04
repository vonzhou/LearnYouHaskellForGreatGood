type Birds = Int
type Pole = (Birds, Birds)


landLeft :: Birds -> Pole -> Pole 
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole 
landRight n (left, right) = (left, right + n)

x -: f = f x

