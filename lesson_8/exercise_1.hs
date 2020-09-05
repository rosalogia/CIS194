data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (f (g [b]))

instance Functor (ComplicatedA a b) where
    fmap f (Con1 x y) = (Con1 x (f y))
    fmap f (Con2 [])  = Con2 []
    fmap f (Con2 [x]) = handleMaybe x
    fmap f (Con2 x:xs) = ((handleMaybe x) : (fmap f (Con2 xs)))
        where
            handleMaybe :: Maybe (a -> b) -> ComplicatedA a b
            handleMaybe Nothing = Con2 []
            handleMaybe Just g = Con2 [f . g]

instance Functor (ComplicatedB f g a b) where
    fmap fm (Con3 (fn x)) = (Con3 (fm . fn x))
    fmap fm (Con4 (gn x)) = (Con4 (fm . gm x))
    fmap fm (Con5 (gn (gm bs))) - 
