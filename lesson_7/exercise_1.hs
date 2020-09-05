fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (\a b -> a + b) fibs2 (tail fibs2))

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show s = disp20 0 "[" s
        where
            disp20 :: Show a => Integer -> String -> Stream a -> String
            disp20 n acc (Cons x (stream))
                | n == 20   = acc ++ "...]"
                | otherwise = (disp20 (n+1) (acc ++ (show x) ++ ", ") stream)

streamToList :: Stream a -> [a]
streamToList (Cons element (remaining)) = element : (streamToList remaining)

streamRepeat :: a -> Stream a
streamRepeat x = (Cons x (streamRepeat x))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x (xs)) = (Cons (f x) (streamMap f xs))

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = (Cons seed (streamIterate f (f seed)))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x (s1)) s2 = (Cons x (streamInterleave s2 s1))

nats :: Stream Integer
nats = streamIterate (+ 1) 0

rulerF :: Integer -> Integer
rulerF n = highestDivisibleExp 0 n
    where
        highestDivisibleExp :: Integer -> Integer -> Integer
        highestDivisibleExp counter num
            | num `mod` (2^counter) == 0    = highestDivisibleExp (counter+1) num
            | otherwise                     = counter-1

constructRuler :: Integer -> Stream Integer
constructRuler n = streamInterleave (streamRepeat n) (constructRuler (n+1))

ruler :: Stream Integer
ruler = constructRuler 0

data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons x (xs)) -> x, xs)

pureSupply :: a -> Supply s a
pureSupply item = S (\xs -> item, xs)

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S g) = S ((\x, xs -> (f x), xs) . g)

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f Sa Sb = mapToSupplies f . chainSupplies Sa Sb
    where
        chainSupplies :: Supply s a -> Supply s b -> (a, (b, Stream s))
        chainSupplies (S f1) (S f2) = (\a, stream -> a, f2 stream) . f1

        mapToSupplies :: (a -> b -> c) -> (a, (b, Stream s)) -> Supply s c
        mapToSupplies mapF (a, (b, stream)) = S (\s -> (mapF a b), stream)

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S g) f = (\a, _ -> f a) . g

-- g: Stream s -> (a, Stream s)
-- f: a -> Stream s -> (b, Stream s)

-- (Stream s -> (a, Stream s))
-- (a, Stream s) -> 
-- 
-- (Stream s -> (b, Stream s))

runSupply :: Stream s -> Supply s a -> a
runSupply s (S g) = unit $ g s
    where unit (x, xs) = x

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (label t)
    where
        label :: Tree a -> Supply s (Tree s)
        label (Leaf _) = (get nats) `bindSupply` (\num -> (Leaf num), 
            where
                top :: (s, Stream s) -> s
                top (x, _) = x
        label (Leaf _) (Leaf _) = 









