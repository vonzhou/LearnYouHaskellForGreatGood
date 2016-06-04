data Vector a = Vector a a a deriving (Show)

vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: Num a => Vector a -> Vector a ->  a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j * m + k *n


vmulti :: Num a => Vector a -> a -> Vector a
(Vector i j k) `vmulti` m  = Vector (i*m) (j*m) (k*m)
