-- foldr (⊕) z [a1,...,an] =
-- a1 ⊕ (a2 ⊕ (... ⊕ (an ⊕ z)))
-- catamorphism

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr _gMap _zMap

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr _gFilter _zFilter

length' :: [a] -> Int
length' = foldr _gLength _zLength

-- Y más! Ver ejercicios
