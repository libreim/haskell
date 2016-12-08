{- Declaraciones de tipo -}
a :: Int
a = 42
-- a = 5

f :: Int -> Int
f x = x + 2

-- :t b
b = f a + f a

-- Podemos especificar o no el tipo de nand
nand :: Bool -> Bool -> Bool
nand x y = not (x && y)



{- Variables de tipo -}

-- :t identidad ?
-- ∀t ∈ Tipos. t → t
identidad x = x

-- El tipo de x puede ser distinto del de y (a vs. b)
ignoraY :: a -> b -> a
ignoraY x y = x

{- Reconocimiento de patrones -}
niega :: Bool -> Bool
niega True  = False
niega False = True

esCero :: Int -> Bool
esCero 0 = True
esCero x = False

factorial :: Int -> Int
-- El orden importa!!
factorial 0 = 1
factorial n = n * factorial (n-1)



{- Currificación -}

suma5 :: Int -> Int
suma5 = (5+)

siempre3 :: a -> Int
siempre3 = ignoraY 3

siempre8 :: a -> Int
siempre8 = suma5 . siempre3 -- . es la función composición
