
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

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
goLeft (Empty,_) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)
goUp (t, []) = Nothing

type Zipper a = (Tree a , Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty , bs)

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t, [])
topMost z = topMost (goUp z)


