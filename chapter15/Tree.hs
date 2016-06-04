
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
  Node 'P'
  (Node 'O'
    (Node 'L'
      (Node 'N' Empty Empty)
      (Node 'T' Empty Empty)
    )
    (
    Node 'Y'
      (Node 'N' Empty Empty)
      (Node 'T' Empty Empty)
    )
  )
  (
  Node 'L'
    (Node 'W'
      (Node 'C' Empty Empty)
      (Node 'R' Empty Empty)
    )
    (Node 'A'
      (Node 'A' Empty Empty)
      (Node 'C' Empty Empty)
    )
  )

data Direction = L | R deriving (Show)
type Directions = [Direction]

-- Change the M node to P
changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree Char -> Char
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

type Breadcrumbs = [Direction]

goLeft :: (Tree a , Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)


goRight :: (Tree a , Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ l r, bs) = (r, R:bs)

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)









