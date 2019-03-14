l1 :: [Int]
l1 = [1,2,3]

l2 :: [Int]
l2 = l1 <> [4,5] -- Concatenar

l3 :: [Int]
l3 = 0 : l2 -- Anteponer un elemento
-- l3 == 0:(1:(2:(3:(4:(5:[])))))

-- :t l4 ?
l4 = []

{- Funciones sobre listas -}

head' :: [a] -> a
-- Primer elemento
head' []     = error "Lista vacía"
head' (x:xs) = _head

length' :: [a] -> Int
-- Longitud
-- ¿Por qué es ineficiente?
length' []     = 0
length' (x:xs) = 1 + length' xs

repeat' :: a -> [a]
repeat' a = a : repeat' a

(!!!) :: [a] -> Int -> a
[]     !!! _ = error "Índice demasiado grande"
(x:_)  !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)
