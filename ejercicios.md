
0. Define las siguientes funciones:

  - `not  :: Bool -> Bool`
  - `(&&) :: Bool -> Bool -> Bool`
  - `max  :: Ord a => a -> a -> a`
  - `even :: Int -> Bool`
  - `head :: [a] -> a`
  - `(.)  :: (b -> c) -> (a -> b) -> (a -> c)` (composición de funciones)
  - `takeWhile :: (a -> Bool) -> [a] -> [a]`

1. Define una función para encontrar el último elemento de una lista, y otra para
encontrar el penúltimo.

2. Define una función `rango :: Int -> Int -> [Int]` de tal forma que `rango a b`
sea el rango entre a y b.

3. Define una función `mcd :: Int -> Int -> Int` que calcule el máximo común divisor
entre dos enteros.

4. Define una función que diga si una lista es sublista de otra.

5. La función `zip :: [a] -> [b] -> [(a,b)]` crea parejas de las dos listas pasadas
como argumento:

  -  Define `zip`.
  - ¿Qué hace `zip [0..]`?
  - Generalizar la definición a `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`,
  que empareja los elementos de las listas usando la función pasada
  (`zipWith (+) [1,2] [2,3,4] == [1+2, 2+3]`)
  - Define `zip` utilizando `zipWith`

6. Una sentencia condicional en Haskell se escribe:
  ```haskell
  if {-cond-} then {-caso de que sí -} else {-caso de que no -}
  ```
  Define una función `ifThenElse` que implemente una sentencia condicional.
  ¿Cuál es su tipo?

7. Define la función `until :: (a -> Bool) -> (a -> a) -> a -> a`, que aplica
una función `(a -> a)` sobre un elemento inicial `a` hasta que no se cumpla un
predicado `(a -> Bool)`.

8. La función `$ :: (a -> b) -> a -> b` se define `f $ x = f x`. Es el operador
infijo con menor precedencia, de tal forma que puede utilizarse para evitar
paréntesis. También tiene otros usos:
  
  - ¿Para qué puede utilizarse `map ($0)`?
  - ¿Y `zipWith ($)`?

9. Define la función `curry :: ((a, b) -> c) -> a -> b -> c`, que *currifica* una
función de dos argumentos.

10. Según los axiomas de Peano, un número natural puede ser 0 o un sucesor de
otro número natural. Define el tipo `Nat` que define los números naturales de
esta manera. Implementa:

  - Las funciones `suma, producto :: Nat -> Nat -> Nat`.
  - Las funciones `par, impar :: Nat -> Bool`.
  - Una función `toInt :: Nat -> Int` que convierta un natural en un entero.

11. Sea `g :: [a] -> b` una función tal que `g [] = v` y `g (x:xs) = f x (g xs)`.
Define `g` utilizando `foldr`. Aplica éste patrón para definir las siguientes
funciones:

  - `concat :: [[a]] -> [a]`
  - `maximum :: [a] -> a`
  - `length :: Num t => [a] -> t`
  - `++ :: [a] -> [a] -> [a]`
  - `map :: (a -> b) -> [a] -> [b]`
  - `filter :: (a -> Bool) -> [a] -> [a]`

12. ¿Cual es el tipo de `(.) . (.)`? ¿Para qué puede utilizarse?
