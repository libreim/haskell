{- Listas -}

l1 :: [Int]
l1 = [1,2,3]

l2 = l1 ++ [4,5] -- Concatenar

l3 = 0 : l2 -- Anteponer un elemento
-- l3 == 0:(1:(2:(3:(4:(5:[])))))

-- :t l4 ?
l4 = []

{- Funciones sobre listas -}

head' :: [a] -> a
head' []     = error "Lista vacía"
head' (x:xs) = x

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

repeat' :: a -> [a]
repeat' a = a : repeat' a

elem' :: a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) = (y == x) || (elem' y xs)


{- Funciones de orden superior -}

-- map f [a1, a2, ..., an] = [f a1, f a2, ..., f an]
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map f xs

suma1 :: [Int] -> [Int]
suma1 = map (+1)

exclama :: [String] -> [String]
exclama = map (++"!")

-- foldr (⊕) z [a1,...,an] = a1 ⊕ (a2 ⊕ (... ⊕ (an ⊕ z)))
foldr' :: (a -> b -> b) -> b -> [a] -> b

sum' :: [Int] -> Int
sum' = foldr' (+) 0

concat' :: [[a]] -> [a]
concat' = foldr' (++) []

{- Definición de tipos -}

-- data Bool = False | True
-- data Int = -9223372036854775808 | ... | -1 | 0 | 1 | ... | 9223372036854775807


-- Tipos enumerados
data Forma = Triangulo | Cuadrado | Circulo

esPoligono :: Forma -> Bool
esPoligono Triangulo = True
esPoligono Cuadrado  = True
esPoligono Circulo   = False


-- Tipos con campos
data Persona = Persona String Int

getEdad :: Persona -> Int
getEdad (Persona nombre edad) = edad

mismaEdad :: Persona -> Persona -> Bool
mismaEdad (Persona n1 e1) (Persona n2 e2) = e1 == e2


-- Varios constructores de datos
data Color = RGB Double Double Double |  HSV Double Double Double

-- Tipos recursivos
data Nat = Z | S Nat -- Naturales (Peano)

suma :: Nat -> Nat -> Nat
suma   Z   n = n
suma (S n) m = S (suma n m)

toInt :: Nat -> Int
toInt   Z   = 0
toInt (S n) = 1 + toInt n

{- Constructores de tipos -}

-- (Sintaxis no legal)
-- data [a] = [] | a : [a]

data Tree a = Empty | Node a (Tree a) (Tree a)

refl Empty = Empty
refl (Node a t1 t2) = Node a (refl t2) (refl t1)


