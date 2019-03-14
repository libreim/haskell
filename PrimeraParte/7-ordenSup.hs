flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


ifThenElse' :: Bool -> a -> a -> a
ifThenElse' True  = _ifTrue
ifThenElse' False = _ifFalse


-- map f [a1, a2, ..., an] =
-- [f a1, f a2, ..., f an]
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map f xs

suma1 :: [Int] -> [Int]
suma1 = map (+1)

exclama :: [String] -> [String]
exclama = map (<>"!")


-- foldr (⊕) z [a1,...,an] =
-- a1 ⊕ (a2 ⊕ (... ⊕ (an ⊕ z)))
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' g z []     = z
foldr' g z (x:xs) = x `g` foldr' g z xs

sum' :: [Int] -> Int
sum' = foldr (+) 0

concat' :: [[a]] -> [a]
concat' = foldr (<>) []

--- Hueco en el tipo
filter' :: _ -> [a] -> [a]
filter' p   []   = []
filter' p (x:xs) = if p x then x:ys else ys
  where ys = filter' p xs
