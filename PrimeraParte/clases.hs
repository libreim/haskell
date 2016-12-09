{- Clases de tipos -}

-- :t (==) ?

{-
class  Eq a  where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    -- Sólo hace falta definir uno, el otro se define por defecto:
    x /= y =  not (x == y)
    x == y =  not (x /= y)
-}
-- ¿Qué tipos no se pueden comparar por igualdad?

data Nat = Z | S Nat

instance Eq Nat where
  Z   == Z   = True
  S n == S m = n == m
  _   ==  _  = False


data Tree a = Empty | Node a (Tree a) (Tree a)

-- Los árboles de tipos comparables por igualdad son comparables por igualdad
instance (Eq a) => Eq (Tree a) where
  Empty        == Empty           = True
  Node a t1 t2 == Node a' t1' t2' = a == a' && t1 == t1' && t2 == t2'

{-
:t (<) ?
:t show ?
:t read ?
-}

{- Clases numéricas -}

-- :t a ?
-- t (+) ?
a = 42

sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

-- :t (/)
-- :t div

{- Los funtores -}

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

-- Leyes
-- fmap id         == id
-- fmap f . fmap g == fmap (f . g)

instance Functor Tree where
  fmap f Empty          = Empty
  fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2)
  

