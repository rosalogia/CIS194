import Test.QuickCheck
import Data.List
import Control.Monad

data Stream a = Cons a (Stream a)

data Supply s a = S (Stream s -> (a, Stream s))

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

-- Stream Functions

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show s = init (show (take 20 (streamToList s))) ++ "…"

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f x = Cons x (streamIterate f (f x))

nats :: Stream Integer
nats = streamIterate (+1) 0

-- Supply Functions

get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S (\xs -> (x, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S t) = S go
  where go xs = let (a, xs') = t xs
                in  (f a, xs')

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S t1) (S t2) = S go
  where go xs = let (a, xs')  = t1 xs
                    (b, xs'') = t2 xs'
                in  (f a b, xs'')

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S t1) k = S go
  where go xs = let (a, xs')  = t1 xs
                    (S t2)    = k a
                    (b, xs'') = t2 xs'
                in  (b, xs'')

runSupply :: Stream s -> Supply s a -> a
runSupply s (S t) = fst (t s)

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply

-- Tree functions

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node t1 t2) = Node <$> go t1 <*> go t2
    go (Leaf _) = Leaf <$> get

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTree

genTree :: Arbitrary a => Gen (Tree a)
genTree = sized $ \size -> do
    val <- arbitrary
    frequency [ (1, return (Leaf val))
                , (size, do x <- resize (size - 1) genTree
                            y <- resize (size - 1) genTree
                            return (Node (x) (y)) )]

size :: Tree a -> Int
size (Leaf _) = 1
size (Node (t1) (t2)) = (size t1) + (size t2)

treeToList :: Tree a -> [a]
treeToList (Leaf x) = [x]
treeToList (Node (t1) (t2)) = (treeToList t1) ++ (treeToList t2)

-- Properties to test

prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList t = (size t) == (length . treeToList $ t)

prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = (size t) == (size . labelTree $ t)

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = (length . treeToList . labelTree $ t) == (size . labelTree $ t)

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = (treeToList . labelTree . labelTree $ t) == (treeToList . labelTree $ t)
