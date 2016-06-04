# Learn you a haskell for great good!

## 1. 预备

## 2. 相信类型

## 3. 函数的语法

* 使用了类型变量的函数称为多态函数

head源码实现：

```haskell
-- | Extract the first element of a list, which must be non-empty.
head                    :: [a] -> a
head (x:_)              =  x
head []                 =  badHead
{-# NOINLINE [1] head #-}

badHead :: a
badHead = errorEmptyList "head"
```

## 4. 你好递归

## 5. 高阶函数

## 6. Module

* 学会使用已有的模块，如Data.List, Data.Map, Data.Char
* 实现自己的模块
* qualified 导入


## 7.构造我们自己的类型和类型类

* 值构造器 value constructor, 如 Person String String
* 类型构造器 type constructor, 如 Maybe a，使用类型作为参数，产生新的类型，如果 Maybe Int , Maybe String
* 空列表的类型为[a]，可以作为任意类型的列表

```haskell
*Main> :t []
[] :: [t]
*Main> :t [1,2]
[1,2] :: Num t => [t]
```

* 如果一个类型的行为如果和容器相似，那么可以考虑使用类型参数
* 不要在data声明中添加类约束，在函数声明中具体
* 把类型类理解为接口，定义了行为规范
* 有多个值构造器的类型，定义在前面的值比较小，如 False < True
* 对于两个来自同一个值构造器的值，看根据所含的字段比较大小
* type关键字定义类型别名，区别于data
* 列表concat实现：

```haskell
(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}    -- We want the RULE to fire first.
                             -- It's recursive, so won't inline anyway,
                             -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
```

* Data.Set, Data.Map 使用平衡二叉树实现
* class定义新的类型类，instance将类型转为某类型类的实例
* 类型类子类化，增加约束

```haskell
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
```

* 看一个typeclass有哪些实例，在ghci中 :info TypeClass

```haskell
Main> :info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

* Functor 定义,把f理解为容器

```haskell
class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const
```

* 列表是Functor的实例

```haskell
-- The list type
instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map
-----------------------
map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [1] map #-}    -- We want the RULE to fire first.
                            -- It's recursive, so won't inline anyway,
                            -- but saying so is more explicit
map _ []     = []
map f (x:xs) = f x : map f xs
```

* Maybe也是函子 functor

```haskell
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)
```

* Either a 也是！！

```haskell
data  Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show, Typeable)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)
```

* Map k 是一个Functor的实现

```haskell
instance Functor (Map k) where
  fmap f m  = map f m
```

* 类型具有自己的标签kind，使用:k 查看，使用:t 查看的是值的类型

## 8. IO

* <- 绑定IO action的结果
* do代码块最后一个action不能绑定名字

```haskell
getLine         :: IO String
getLine         =  hGetLine stdin

-- | The 'getContents' operation returns all user input as a single string,
-- which is read lazily as it is needed
-- (same as 'hGetContents' 'stdin').

getContents     :: IO String
getContents     =  hGetContents stdin
```

## 9. 继续IO

* getContents 惰性IO
* 从输入中取字符串，用一个函数做处理，然后把结果输出的模式很常见，所以有个专门函数interact

```haskell
interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)
```

* IO访问模式

```haskell
data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Enum, Read, Show)
```

* withFile的实现

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile name mode = bracket (openFile name mode) hClose
```

* getContents, hGetContents, putStrLn, hPutStrLn等等类似的函数，h前缀表示handle，处理特定的文件，没有h表示处理标准输入输出
* getLine获取的line末尾没有换行 
* pack将字节列表转化为Lazy，变得不那么惰性

```haskell
-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'.
pack :: [Word8] -> ByteString
pack = packBytes

-- | /O(n)/ Converts a 'ByteString' to a '[Word8]'.
unpack :: ByteString -> [Word8]
unpack = unpackBytes
```

## 10. 函数式地解决问题

* RPN
* 最短路径

## 11. Applicative

* 如果把IO操作结果赋予一个变量，唯一的目的是对其应用一个函数，并把结果赋给另一个变量，那么考虑使用fmap吧
* intersperse的实现在 Data.OldList中

```haskell
-- | The 'intersperse' function takes an element and a list and
-- \`intersperses\' that element between the elements of the list.
-- For example,
--
-- > intersperse ',' "abcde" == "a,b,c,d,e"

intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs


-- Not exported:
-- We want to make every element in the 'intersperse'd list available
-- as soon as possible to avoid space leaks. Experiments suggested that
-- a separate top-level helper is more efficient than a local worker.
prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs
```

* 函数函子的定义,其实就是组合，在GHC.Base中而不是书中所说 Control.Monad.Instances

```haskell
instance Functor ((->) r) where
    fmap = (.)
```

* 类型类Applicative的定义，接受两个applicative的参数，生成一个applicative的结果，第一个Applicative中是一个函数

```haskell
class Functor f => Applicative f where
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b
```

* Maybe applicative的定义：

```haskell
instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing
```

* 考虑到 pure f <*> x 等于 fmap f x, 所以有了 <$>函数（Data/Functor.hs）

```haskell
infixl 4 <$>

-- | An infix synonym for 'fmap'.
--
-- ==== __Examples__
--
-- Convert from a @'Maybe' 'Int'@ to a @'Maybe' 'String'@ using 'show':
--
-- >>> show <$> Nothing
-- Nothing
-- >>> show <$> Just 3
-- Just "3"
--
-- Convert from an @'Either' 'Int' 'Int'@ to an @'Either' 'Int'@
-- 'String' using 'show':
--
-- >>> show <$> Left 17
-- Left 17
-- >>> show <$> Right 17
-- Right "17"
--
-- Double each element of a list:
--
-- >>> (*2) <$> [1,2,3]
-- [2,4,6]
--
-- Apply 'even' to the second element of a pair:
--
-- >>> even <$> (2,2)
-- (2,True)
--
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

* 列表[]也是Applicative的实例

```haskell
-- See Note: [List comprehensions and inlining]
instance Applicative [] where
    {-# INLINE pure #-}
    pure x    = [x]
    {-# INLINE (<*>) #-}
    fs <*> xs = [f x | f <- fs, x <- xs]
    {-# INLINE (*>) #-}
    xs *> ys  = [y | _ <- xs, y <- ys]
```

* IO也是Applicative

```haskell
instance Applicative IO where
    pure = return
    (<*>) = ap

ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
```

* 函数是Applicative

```haskell
instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)
```

* ZipList也是

```haskell
-- | Lists, but with an 'Applicative' functor based on zipping, so that
--
-- @f '<$>' 'ZipList' xs1 '<*>' ... '<*>' 'ZipList' xsn = 'ZipList' (zipWithn f xs1 ... xsn)@
--
newtype ZipList a = ZipList { getZipList :: [a] }
                  deriving (Show, Eq, Ord, Read, Functor, Generic, Generic1)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
```

* liftA2

```
-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a <*> b
```


* Monoid的定义

```haskell
-- | The class of monoids (types with an associative binary operation that
-- has an identity).  Instances should satisfy the following laws:
--
--  * @mappend mempty x = x@
--
--  * @mappend x mempty = x@
--
--  * @mappend x (mappend y z) = mappend (mappend x y) z@
--
--  * @mconcat = 'foldr' mappend mempty@
--
-- The method names refer to the monoid of lists under concatenation,
-- but there are many other instances.
--
-- Some types can be viewed as a monoid in more than one way,
-- e.g. both addition and multiplication on numbers.
-- In such cases we often define @newtype@s and make those instances
-- of 'Monoid', e.g. 'Sum' and 'Product'.

class Monoid a where
        mempty  :: a
        -- ^ Identity of 'mappend'
        mappend :: a -> a -> a
        -- ^ An associative operation
        mconcat :: [a] -> a

        -- ^ Fold a list using the monoid.
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.

        mconcat = foldr mappend mempty
```

* 列表是Monoid,给出了mconcat实现，使用默认的foldr mappend mempty [[1,2],[3,4]] 也可以work， 这里mconcat 等于  concat

```haskell
instance Monoid [a] where
        {-# INLINE mempty #-}
        mempty  = []
        {-# INLINE mappend #-}
        mappend = (++)
        {-# INLINE mconcat #-}
        mconcat xss = [x | xs <- xss, x <- xs]
```

* Product,数乘(Data.Monoid模块中)

```haskell
-- | Monoid under multiplication.
newtype Product a = Product { getProduct :: a }
        deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

instance Num a => Monoid (Product a) where
        mempty = Product 1
        mappend = coerce ((*) :: a -> a -> a)
--        Product x `mappend` Product y = Product (x * y)
```

* Sum，数和

```haskell
-- | Monoid under addition.
newtype Sum a = Sum { getSum :: a }
        deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

instance Num a => Monoid (Sum a) where
        mempty = Sum 0
        mappend = coerce ((+) :: a -> a -> a)
--        Sum x `mappend` Sum y = Sum (x + y)
```

* Bool有两种方式成为Monoid

```haskell
-- | Boolean monoid under conjunction ('&&').
newtype All = All { getAll :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded, Generic)

instance Monoid All where
        mempty = All True
        All x `mappend` All y = All (x && y)

-- | Boolean monoid under disjunction ('||').
newtype Any = Any { getAny :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded, Generic)

instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)
```

* Ordering,如果相等才继续进行比较, 使得我们可以轻松的根据很多准则比较东西，有用

```haskell
-- lexicographical ordering
instance Monoid Ordering where
        mempty         = EQ
        LT `mappend` _ = LT
        EQ `mappend` y = y
        GT `mappend` _ = GT
```

* Maybe a作为Monoid，First a, Last a的作用是mconcat时保留第一个或最后一个非Nothing值

```haskell
-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\" Since
-- there is no \"Semigroup\" typeclass providing just 'mappend', we
-- use 'Monoid' instead.
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
  
-- | Maybe monoid returning the leftmost non-Nothing value.
--
-- @'First' a@ is isomorphic to @'Alt' 'Maybe' a@, but precedes it
-- historically.
newtype First a = First { getFirst :: Maybe a }
        deriving (Eq, Ord, Read, Show, Generic, Generic1,
                  Functor, Applicative, Monad)

instance Monoid (First a) where
        mempty = First Nothing
        First Nothing `mappend` r = r
        l `mappend` _             = l

-- | Maybe monoid returning the rightmost non-Nothing value.
--
-- @'Last' a@ is isomorphic to @'Dual' ('First' a)@, and thus to
-- @'Dual' ('Alt' 'Maybe' a)@
newtype Last a = Last { getLast :: Maybe a }
        deriving (Eq, Ord, Read, Show, Generic, Generic1,
                  Functor, Applicative, Monad)

instance Monoid (Last a) where
        mempty = Last Nothing
        l `mappend` Last Nothing = l
        _ `mappend` r  
```

* Foldable typeclass,可以折叠的数据结构

```haskell
-- | Data structures that can be folded.
--
-- For example, given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Foldable Tree where
-- >    foldMap f Empty = mempty
-- >    foldMap f (Leaf x) = f x
-- >    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
--
-- This is suitable even for abstract types, as the monoid is assumed
-- to satisfy the monoid laws.  Alternatively, one could define @foldr@:
--
-- > instance Foldable Tree where
-- >    foldr f z Empty = z
-- >    foldr f z (Leaf x) = f x z
-- >    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
--
-- @Foldable@ instances are expected to satisfy the following laws:
--
-- > foldr f z t = appEndo (foldMap (Endo . f) t ) z
--
-- > foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
--
-- > fold = foldMap id
--
-- @sum@, @product@, @maximum@, and @minimum@ should all be essentially
-- equivalent to @foldMap@ forms, such as
--
-- > sum = getSum . foldMap Sum
--
-- but may be less defined.
--
-- If the type is also a 'Functor' instance, it should satisfy
--
-- > foldMap f = fold . fmap f
--
-- which implies that
--
-- > foldMap f . fmap g = foldMap (f . g)

class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    -- | Combine the elements of a structure using a monoid.
    fold :: Monoid m => t m -> m
    fold = foldMap id

    -- | Map each element of the structure to a monoid,
    -- and combine the results.
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f = foldr (mappend . f) mempty

    -- | Right-associative fold of a structure.
    --
    -- @'foldr' f z = 'Prelude.foldr' f z . 'toList'@
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo #. f) t) z

    -- | Right-associative fold of a structure,
    -- but with strict application of the operator.
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr' f z0 xs = foldl f' id xs z0
      where f' k x z = k $! f x z

    -- | Left-associative fold of a structure.
    --
    -- @'foldl' f z = 'Prelude.foldl' f z . 'toList'@
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    -- There's no point mucking around with coercions here,
    -- because flip forces us to build a new function anyway.

    -- | Left-associative fold of a structure.
    -- but with strict application of the operator.
    --
    -- @'foldl' f z = 'List.foldl'' f z . 'toList'@
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldl' f z0 xs = foldr f' id xs z0
      where f' x k z = k $! f z x

    -- | A variant of 'foldr' that has no base case,
    -- and thus may only be applied to non-empty structures.
    --
    -- @'foldr1' f = 'Prelude.foldr1' f . 'toList'@
    foldr1 :: (a -> a -> a) -> t a -> a
    foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x m = Just (case m of
                         Nothing -> x
                         Just y  -> f x y)

    -- | A variant of 'foldl' that has no base case,
    -- and thus may only be applied to non-empty structures.
    --
    -- @'foldl1' f = 'Prelude.foldl1' f . 'toList'@
    foldl1 :: (a -> a -> a) -> t a -> a
    foldl1 f xs = fromMaybe (error "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf m y = Just (case m of
                         Nothing -> y
                         Just x  -> f x y)

    -- | List of elements of a structure, from left to right.
    toList :: t a -> [a]
    {-# INLINE toList #-}
    toList t = build (\ c n -> foldr c n t)

    -- | Test whether the structure is empty. The default implementation is
    -- optimized for structures that are similar to cons-lists, because there
    -- is no general way to do better.
    null :: t a -> Bool
    null = foldr (\_ _ -> False) True

    -- | Returns the size/length of a finite structure as an 'Int'.  The
    -- default implementation is optimized for structures that are similar to
    -- cons-lists, because there is no general way to do better.
    length :: t a -> Int
    length = foldl' (\c _ -> c+1) 0

    -- | Does the element occur in the structure?
    elem :: Eq a => a -> t a -> Bool
    elem = any . (==)

    -- | The largest element of a non-empty structure.
    maximum :: forall a . Ord a => t a -> a
    maximum = fromMaybe (error "maximum: empty structure") .
       getMax . foldMap (Max #. (Just :: a -> Maybe a))

    -- | The least element of a non-empty structure.
    minimum :: forall a . Ord a => t a -> a
    minimum = fromMaybe (error "minimum: empty structure") .
       getMin . foldMap (Min #. (Just :: a -> Maybe a))

    -- | The 'sum' function computes the sum of the numbers of a structure.
    sum :: Num a => t a -> a
    sum = getSum #. foldMap Sum

    -- | The 'product' function computes the product of the numbers of a
    -- structure.
    product :: Num a => t a -> a
    product = getProduct #. foldMap Product
```

* Monad

```haskell
class Applicative m => Monad m where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
    {-# INLINE (>>) #-}

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    return      = pure

    -- | Fail with a message.  This operation is not part of the
    -- mathematical definition of a monad, but is invoked on pattern-match
    -- failure in a @do@ expression.
    fail        :: String -> m a
    fail s      = error s

```

* Maybe作为Monad实例类

```haskell
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (>>) = (*>)

    return              = Just
    fail _              = Nothing
```

* 列表作为Monad的实例, (*>)接口在Applicative中

```haskell
instance Monad []  where
    {-# INLINE (>>=) #-}
    xs >>= f             = [y | x <- xs, y <- f x]
    {-# INLINE (>>) #-}
    (>>) = (*>)
    {-# INLINE return #-}
    return x            = [x]
    {-# INLINE fail #-}
    fail _              = []
```


* 结果是Monad的函数组合

```haskell
infixr 1 <=<, >=>

-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)
```


## 14. 再多一些Monad

* Writer
* Reader
* 函数类型 (->) r 是Functor，Applicative， Monad

```haskell
instance Functor ((->) r) where
    fmap = (.)

instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)

instance Monad ((->) r) where
    return = const
    f >>= k = \ r -> k (f r) r
```

* state定义,参数是一个函数，返回一个Monad

```haskell
-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
```

* Module ‘Control.Monad.Error’ is deprecated:Use Control.Monad.Except instead
* liftM 和 fmap 本质一样

```haskell
-- | Promote a function to a monad.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }
```

* ap函数类似于 <*>

```haskell
ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
```


* join的实现

```haskell
join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id
```

* filterM

```haskell
filterM          :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p        = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) (p x)) (pure [])
```

* foldM

```haskell
foldM          :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
{-# INLINEABLE foldM #-}
{-# SPECIALISE foldM :: (a -> b -> IO a) -> a -> [b] -> IO a #-}
{-# SPECIALISE foldM :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a #-}
foldM          = foldlM

-- associating to the right, i.e. from right to left.
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = foldl f' return xs z0
  where f' k x z = f x z >>= k

-- | Monadic fold over the elements of a structure,
-- associating to the left, i.e. from left to right.
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr f' return xs z0
  where f' x k z = f z x >>= k
```

* <=< 定义

```haskell
infixr 1 <=<, >=>

-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped.
--
-- Note how this operator resembles function composition @('.')@:
--
-- > (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
-- > (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)
```

* Prob.hs 问题：No instance for (Applicative Prob)


## 15. zipper

* 在数据结构中移动焦点，使用zipper保存辅助信息

## 后记
很久之前看英文版，读至后面三分之一处就放下了，而且理解不了。最近把中文版从头到尾阅读了一遍，部分仍然没有理解，接下来需要在实践中不断体会,对于有些概念要看Wikipedia。

2016.6.3 HUST

