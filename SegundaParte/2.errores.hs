{-
Intentamos obtener raíces cuadradas en los reales usando el método de Newton.
Y parar cuando estemos suficientemente cerca (más cerca que un ε dado).
-}

-- Aquí imprimiremos los números en coma flotante con sólo dos decimales.
-- Nada importante, sólo detalles para hacer que el resto se vea bonito.
-- Incluimos además una función división que toma enteros y devuelve flotantes.
import Control.Monad
import Text.Printf
printd :: Double -> IO ()
printd = putStrLn . printf "%.3f"

(//) :: Int -> Int -> Double
(//) a b = (fromIntegral a) / (fromIntegral b)


-- Implementamos el concepto de límite en una lista infinita buscando hasta
-- encontrar dos términos que se diferencien en menos distancia que épsilon.
--
-- >>> armonic = map (1 //) [1..] 
-- >>> printd $ limit 0.0000000001 armonic
-- 0.000
--
-- >>> powerseq = map ((1//) . (2^)) [1..]
-- >>> powerseries = scanl (+) 0 powerseq
-- >>> printd $ limit 0.000000001 powerseries
-- 1.000
limit :: Double -> [Double] -> Double
limit epsilon (x:y:xs)
  | abs (x-y) < epsilon = y
  | otherwise           = limit epsilon (y:xs)

newtonaprox :: Double -> Double -> Double
newtonaprox n x = (x + n/x)/2

sqroot :: Double -> Double
sqroot 0 = 0
sqroot x = limit 0.003 (iterate (newtonaprox x) x)



-- Usamos la función para resolver una ecuación de segundo grado.
-- Podemos definir una estructura de datos `QPol` para el polinomio y una
-- función que lo resuelva obteniendo sus dos raíces:
data QPol = QPol Double Double Double
instance Show QPol where
  show (QPol a b c) = show a ++ "x² + " ++ show b ++ "x + " ++ show c

solve :: QPol -> (Double,Double)
solve (QPol a b c) = (sol1,sol2)
    where sol1 = ( (-b) + sqroot(b*b-4*c*a) )/(2*a)
          sol2 = ( (-b) - sqroot(b*b-4*c*a) )/(2*a)


-- Y podemos comprobar que funciona como esperamos:
--
-- >>> pol = QPol 1 (-5) 6
-- >>> putStrLn $ "Las soluciones de " ++ show pol ++ " son " ++ solve pol
-- Las soluciones de 1.0x² + -5.0x + 6.0 son (3.0,2.0)
--
-- ¡Excepto en los casos en los que el discriminante es no positivo!
-- El método está dando convergencia sólo cuando tratamos de sacar
-- la raíz cuadrada de un número que es positivo, pero no funciona
-- cuando sacamos la raíz de un número que no es positivo.
--
-- La primera solución es hacer que devuelva Maybe en el caso de error.
sqroot' :: Double -> Maybe Double
sqroot' x
  | x < 0     = Nothing
  | x == 0    = Just 0.0
  | otherwise = Just ( limit 0.003 (iterate (newtonaprox x) x) )



-- Esto lo soluciona, pero nos crea un problema mayor. La función `solve`
-- está usando la raíz cuadrada y se espera de ella que devuelva un número,
-- no un posible error.
--
-- Tenemos varias soluciones:
--  Solución 1. Implementar una suma especial.
(.+.) :: Maybe Float -> Maybe Float -> Maybe Float
(.+.) Nothing _ = Nothing
(.+.) _ Nothing = Nothing
(.+.) (Just a) (Just b) = Just (a + b)

-- Solución 2. Implementar una función especial sobre las habituales.
-- El resto de funciones se implementan desde ella.
errorAware :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
errorAware op Nothing _ = Nothing
errorAware op _ Nothing = Nothing
errorAware op (Just a) (Just b) = Just (op a b)

(+.), (*.) :: Maybe Double -> Maybe Double -> Maybe Double
(+.) = errorAware (+)
(*.) = errorAware (*)

-- Solución 3. ¡Usar mónadas!
-- Las mónadas son una estructura algebraica que modela la composición
-- de funciones que devuelven un contexto alrededor del valor o que
-- comparten un cierto estado común.
-- Cualquier contenedor (Maybe a, [a],...) puede ser una mónada si tiene
-- un método que permita ese tipo de composición.
--
-- Maybe es una mónada y list es una mónada
{-

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- Return de la mónada Maybe
return x = Just x

-- Bind de la mónada Maybe
(Just x) >>= k = k x
Nothing  >>= _ = Nothing

-- Return de la mónada List
return x = [x]

-- Bind de la mónada List
xs >>= f = [y | x <- xs, y <- f x]

-}
-- Entonces las mónadas nos dan una forma de componer funciones con estado
-- y de paso nos regalan multitud de funciones ya implementadas.
{-

sqroot' (sqroot' 3)              -- ¡Error de tipos!
sqroot' 3 >>= sqroot'            -- Usando mónadas
Just   3 >>= sqroot' >>= sqroot' -- Usando Just
return 3 >>= sqroot' >>= sqroot' -- Equivalente a lo anterior

-}
-- La solución para implementar solve de nuevo es:
solve'' :: QPol -> Maybe (Double,Double)
solve'' (QPol a b c) = sqroot' (b*b-4*c*a) >>= (\d -> return (sol1 d, sol2 d))
  where
    sol1 d = ( (-b) + d )/(2*a)
    sol2 d = ( (-b) - d )/(2*a)


-- Esto funciona muy bien pero es difícil de escribir, porque tenemos
-- que pasar el discriminante como argumento a una función lambda que
-- nos calcula los dos usos del discriminante.
-- Las mónadas nos dan una notación do, que es más fácil de usar para
-- estos casos:
solve2 :: QPol -> Maybe (Double,Double)
solve2 (QPol a b c) = do
  -- Esta es una forma de escribir la composición de sqroot y el return
  -- Aquí guardamos el resultado de la primera función para usarlo luego
  discr <- sqroot' (b*b - 4*c*a)

  -- Aquí componemos con la segunda función, que ya usa el resultado
  -- de la primera función.
  return (((-b) + discr)/(2*a), ((-b) - discr)/(2*a))

-- Más claramente
solve3 :: QPol -> Maybe (Double,Double)
solve3 (QPol a b c) = do
  discr <- sqroot' (b*b - 4*c*a)
  return (((-b) + discr)/(2*a), ((-b) - discr)/(2*a))


-- Nótese que esta es una composición que tiene en cuenta la estructura
-- de la mónada. Si la primera sqroot produce un error, esta se propaga
-- por composición a todas las funciones.


-- Otro ejemplo final, vamos a calcular una función como:
--   f(x) = sqrt(sqrt(x)) + sqrt(x) + x
-- Para eso vamos a usar 'return' para incluir x en la mónada
-- y vamos a usar 'liftM2' para que (+) trabaje en la mónada.
(+:) :: Maybe Double -> Maybe Double -> Maybe Double
(+:) = liftM2 (+)

f :: Double -> Maybe Double
f x = (sqroot' x >>= sqroot') +: (sqroot' x) +: (return x)


-- Leyes de las mónadas.
-- En haskell no es necesario, pero cuando definimos una mónada,
-- es una buena idea obligarla a que cumpla determinadas proposiciones
-- relacionadas con el hecho de que debe comportarse como una buena
-- composición.
{-
return a >>= f     ===    f a
m >>= return       ===    m
(m >>= f) >>= g    ===    m >>= (\x -> f x >>= g)
-}
