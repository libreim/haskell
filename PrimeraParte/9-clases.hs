{- No podemos demostrar que las instancias cumplen las propiedades-}

{-
Clase de los monoides
Sujeto a las propiedades:

  · x <+> (y <+> z) = (x <+> y) <+> z
  · x <+> neutro = neutro <+> x = x
-}

class Monoide a where
  (<+>)  :: a -> a -> a
  neutro :: a

instance Monoide [a] where
  xs <+> ys = xs <> ys
  neutro    = []

instance Monoide Int where
  x <+> y = x + y
  neutro  = 0


reduce :: (Monoide a) => [a] -> a
reduce = foldr (<+>) neutro


data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Eq, Show)


{-
Clase de los funtores.
Sujeto a las propiedades:

  · fmap id == id
  · fmap f . fmap g == fmap (f . g)
-}
class Funtor f where
  fmap' :: (a -> b) -> f a -> f b


instance Funtor [] where
  fmap' f []     = []
  fmap' f (x:xs) = f x : fmap' f xs


instance Funtor Tree where
  fmap' f Empty = Empty
  fmap' f (Node a tLeft tRight) = Node (f a) (fmap' f tLeft) (fmap' f tRight)
