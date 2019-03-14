
{- Definición de tipos -}

-- data Bool = False | True
-- data Int =
-- -9223372036854775808|...|-1|0|1|...|9223372036854775807


-- Tipos enumerados
data Forma = Triangulo | Cuadrado | Circulo -- :t Triangulo ?

esPoligono :: Forma -> Bool
esPoligono Triangulo = True
esPoligono Cuadrado  = True
esPoligono Circulo   = False


-- Tipos con campos :t P ?
data Persona = P String Int

-- Obtener la edad de una persona
getEdad :: Persona -> Int
getEdad (P _ edad) = edad

-- Tienen la misma edad?
mismaEdad :: Persona -> Persona -> Bool
mismaEdad (P _ e1) (P _ e2) = e1 == e2



-- Varios constructores de datos
data Color = RGB Double Double Double
          |  HSV Double Double Double



-- Tipos recursivos
data Nat = Z | S Nat
  deriving (Eq,Show)

suma :: Nat -> Nat -> Nat
-- suma de naturales
suma   Z   n = n
suma (S n) m = S (suma n m)

toInt :: Nat -> Int
-- Pasar a entero
toInt   Z   = 0
toInt (S n) = 1 + toInt n


data Quizas a = Nada | Algo a
  deriving (Eq,Show)

mapList :: (a -> b) -> Quizas a -> Quizas b
mapList f Nada     = Nada
mapList f (Algo x) = Algo (f x)


data Pareja a b = Pareja a b
  deriving (Eq, Show)
