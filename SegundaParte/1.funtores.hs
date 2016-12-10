import Data.List

-- ¡En Haskell, se pueden tratar listas infinitas!
-- Eso es gracias a la evaluación perezosa
naturals :: [Integer]
naturals = [1..]

-- Pero no es lo único que se puede hacer con las listas.
-- Podemos mapear sobre ellas. Un contenedor sobre el que se
-- puede mapear forma una estructura algebraica que se llama
-- funtor.
--
-- >>> map (*2) it
-- [2,4,6,8,10,12,14,16,18,20]
-- >>> map toLower "HOLA!"
-- "hola!"
--
-- Aquí calculamos los números pares, con una función con
-- aplicación parcial
evenlist :: [Integer]
evenlist = map (*2) naturals

-- Y aquí los impares, con una función lambda
oddlist :: [Integer]
oddlist = map (\n -> 2*n - 1) naturals

-- Como una generalización de map, existe 'fmap', que no sólo
-- funciona sobre listas. Podemos mapear sobre Maybe o sobre pares,
-- o incluso sobre IO, por ejemplo:
pair20 :: (Int,Int)
pair20 = fmap (+2) (2,4)

just3 :: Maybe Int
just3 = Just 3

just6 :: Maybe Int
just6 = fmap (*2) just3

getInt :: IO Int
getInt = fmap read getLine

getPowerOf2 :: IO Int
getPowerOf2 = fmap (2^) getInt

-- Además, <$> es un equivalente de fmap que es útil para escribirlo
-- como operador infijo.
getWords :: IO [String]
getWords = words <$> getLine



-- Podemos convertir a nuestros árboles binarios en funtores y usarlos
-- con las funciones desde fmap
data Tree a = Node a (Tree a) (Tree a) | Nil
  deriving (Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

aTree :: Tree Int
aTree = Node 1 (Node 3 (Node 2 Nil Nil) Nil) (Node 4 (Node 5 Nil Nil) (Node 6 Nil Nil))
