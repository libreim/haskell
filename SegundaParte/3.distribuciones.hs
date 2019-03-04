{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-
En este archivo vamos a usar mónadas para definir distribuciones
discretas de probabilidad y aplicar operaciones algebraicas sobre ellas.
-}
import Control.Monad.State

-- Generación aleatoria
-- Para generar números pseudoaleatorios usaremos LCGs. La idea es tener
-- un dado que nos dé una distribución de probabilidad uniforme dada una
-- semilla y nos devuelva el resultado de la tirada y una nueva semilla
-- aleatoria. Buscamos que un dado de seis caras sea, por ejemplo:
--
--   dice 6 :: Seed -> (Int, Seed)
--
-- Si quisiéramos tirar dos dados, tendríamos que tomar la semilla resultante
-- del primer lanzamiento y pasarla al segundo; algo así:
--
--   let (a,newseed) = dice 6 seed
--   let (b,_)       = dice 6 newseed
--   print [a,b]
--
-- Pero esto se hace demasiado complejo. La semilla, en el fondo, es un
-- estado, así que podemos modelarla con la mónada State. Cada lanzamiento
-- será de la forma:
--
--   State Seed a   ===   Seed -> (a, Seed)
--
-- Luego podemos llamar a la distribución: Distribution a = State Seed a, y
-- trabajar con ella usando las funciones normales de mónadas.
type Seed = Int
type Distribution = State Seed




-- Nuestra primera distribución es un dado de "n" lados que usa internamente un
-- generador de números aleatorios.
dice :: Int -> Distribution Int
dice n = state (\s -> (s `mod` n + 1, 16807*s `mod` 2147483647))

-- Una moneda es un dado de dos caras
coin :: Distribution Int
coin = dice 2

-- Estas funciones pueden ser llamadas con la mónada estado, dada una
-- semilla inicial, devuelven el resultado y la nueva semilla:
--
-- λ> runState (dice 6) 1
-- (2,16807)
-- λ> runState (dice 6) 16807
-- (2,282475249)
--
-- El usar composición con mónadas nos ahorraba controlar los errores
-- en el primer caso, aquí nos ahorra controlar el cambio de semilla,
-- por ejemplo: para lanzar dos dados y hacer que la semilla se pase
-- internamente.
twodices' :: Distribution Int
twodices' = do
  a <- dice 6
  b <- dice 6
  return (a+b)

(⊕) :: Distribution Int -> Distribution Int -> Distribution Int
(⊕) = liftM2 (+)
(⊗) :: Distribution Int -> Distribution Int -> Distribution Int
(⊗) = liftM2 (*)

twodices :: Distribution Int
twodices = dice 6 ⊕ dice 6

-- Igual que hago esto, podría hacer:
--
--   foldr (⊕) (return 0) [dice 6,dice 6,dice 6]
--   foldr (⊕) (return 0) (replicate 10 (dice 6))
--
-- Que da un resultado que se aproxima a una distribución normal.

-- Ahora, desde ella, podemos crear otras distribuciones. La distribución de
-- bernoulli sería la de una moneda trucada donde una cara tiene probabilidad
-- p y la otra tiene probabilidad (1-p).
bernoulli :: Double -> Distribution Int
bernoulli p = do
  sample <- dice 1000000
  if (fromIntegral sample / 1000000.0 < p)
    then return 1
    else return 0

-- La distribución binomial es la suma de k distribuciones de Bernoulli
binomial :: Int -> Double -> Distribution Int
binomial k p = sum <$> replicateM k (bernoulli p)

-- La distribución constante y otra forma de escribir la distribución
-- binomial, de manera algebraica.
constant :: Int -> Distribution Int
constant n = return n

binomial' :: Int -> Double -> Distribution Int
binomial' k p = foldr (⊕) (constant 0) (replicate k (bernoulli p))




-- Muestra la distribución. Los detalles de implementación no son interesantes.
-- Hemos usado  TypeSynonymInstances para simplificar el proceso de sobrecargar
-- la instancia de Show y poder dibujar directamente por la pantalla las
-- demostraciones.
instance Show (State Seed Int) where
  show = showdist

showdist :: Distribution Int -> String
showdist dist = unlines $ map counter [minimum samples..maximum samples]
  where samples = fst $ runState (replicateM 50000 dist) 1
        counter n = show n <> ":\t " <> replicate ((count n samples) `div` (3000 `div` range)) '#'
        range = maximum samples - minimum samples + 1

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)


main :: IO ()
main = return ()
