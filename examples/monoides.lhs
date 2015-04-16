---
title: Monoides
author: Pablo Baeyens, Mario Román
---

La clase `Monoid`
----------------

Las clases de tipos permiten generalizar el comportamiento de un conjunto de tipos
a partir de una interfaz común: la clase `Eq` incluye los tipos que permiten comparar
por igualdad, la clase `Ord` aquellos que admiten una relación de orden...

La clase [`Monoid`](http://hackage.haskell.org/package/base-4.7.0.2/docs/Data-Monoid.html)
permite agrupar aquellos tipos `T` que sean un monoide; es decir, que tengan una
operación binaria `<> :: T -> T -> T` tal que:

  1. Es **asociativa**: `(a <> b) <> c == a <> (b <> c)`.
     *Ejemplo*: La suma, aunque no la resta: $(3 - 2) - 1 \neq  3 - (2 - 1)$.
  2. Existe un **elemento neutro**: `a <> mempty == a == mempty <> a`.
     *Ejemplo*: $0$ con respecto de la suma. La exponenciación no tiene.

El ejemplo más sencillo de monoide es el tipo `()` con `a <> b = ()`.
Para empezar a utilizarlos, importamos el módulo que los define
y otros módulos que usaremos luego:

\begin{code}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
import Data.Monoid
import Control.Monad.Writer
import Data.Tree
\end{code}

Además, este módulo define la función `mconcat :: [m] -> m` que se define:

~~~haskell
mconcat = foldr (<>) mempty
~~~

Esta nos permite reducir listas de monoides.

Algunos ejemplos de monoides son:

Listas: [a]
-----------

En este caso, `<> = (++)` y `mempty = []`. Podemos comprobarlo con algunos ejemplos:

~~~haskell
ghci> [1,2] ++ [3,4] ++ [5,6]
[1,2,3,4,5,6]
ghci> [1,2] <> [3,4] <> [5,6]
[1,2,3,4,5,6]
ghci> [1,2,3,4] <> mempty
[1,2,3,4]
~~~

`Sum` y `Product`
-----------------

Los tipos numéricos permiten dos operaciones binarias con las que forman un monoide:
la suma y el producto. Para diferenciarlos se definen, dado un tipo numérico `a`,
tipos que los envuelven:

- `Sum`:     `Sum a <> Sum b = Sum (a + b)` y `mempty = Sum 0`.
- `Product`: `Product a <> Product b = Product (a * b)` y `mempty = Product 1`.

Las funciones `getSum` y `getProduct` permiten extraer el número del tipo. De esta
forma podemos definir:

\begin{code}
sumar, producto :: Num a => [a] -> a
sumar     = getSum     . mconcat . map Sum
producto = getProduct . mconcat . map Product
\end{code}

Estas son equivalentes a las funciones `sum` y `product` incluidas en el `Prelude`.

`Endo`
------

`Endo a` envuelve endomorfismos sobre un tipo `a` (funciones `a -> a`) y define un
monoide:

 - `Endo f <> Endo g = Endo (f . g)`:  la composición.
 - `mempty = Endo id`:  la función identidad, `id x = x`.

Extraemos la función con `appEndo :: Endo a -> (a -> a)`.
Utilizando este monoide podemos componer una lista de funciones fácilmente:

\begin{code}
fs :: Num a => [Endo a]
fs = [Endo (*3), Endo (+2), Endo negate, Endo (37-)]

g :: Num a => a -> a
g = appEndo (mconcat fs)
\end{code}

De esta forma forma tenemos `g x = (negate (37 - x) + 2) * 3`.

Kleisi
------

De forma análoga a `Endo`, podemos definir un monoide sobre las
funciones `a -> m a` con `m` una mónada.

La composición es `(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c`, y
`mempty` es simplemente `return`:

\begin{code}
instance Monad m => Monoid (a -> m a) where
    mempty  = return
    mappend = (>=>)
\end{code}

Crear un monoide
------------------
Podemos definir una función binaria sobre nuestros árboles y hacerlos un monoide.
La operación será introducir ordenadamente todos los elementos de un árbol en el otro.

\begin{code}
--instance (Ord a) => Monoid (Tree a) where
--   at <> bt =
\end{code}

La mónada Writer
----------------

La mónada `Writer` nos permite guardar un registro de las acciones que realizamos.
 **¿Cómo se define esta mónada?**:

- En primer lugar necesitamos un **monoide**, que será el tipo de nuestro registro.
- `return` introduce el elemento en su mínimo contexto. De esta forma, el mínimo
  registro es `mempty`: `return x = Writer (x, mempty)`
- Para definir el *bind* necesitamos una función que extraiga el tipo: `runWriter`
~~~Haskell
        x >>= f =
          let (a, m)  = runWriter x
              (b, m') = runWriter (f a)
          in Writer (b, m <> m')
~~~

De esta forma unimos ambos registros.

Veamos un ejemplo. Podemos introducir algo en la mónada `Writer` utilizando la
función `writer`:

\begin{code}
suma n x  = writer (x + n, ["Suma " ++ show n])
mult n x  = writer (x * n, ["Multiplica por " ++ show n])
resta n x = writer (x - n, ["Resta " ++ show n])
\end{code}

Estas funciones realizan una operación sobre un número y guardan en el registro
la operación realizada.

Utilizando el monoide que creamos antes componemos una serie de estas funciones:

\begin{code}
h = mconcat [suma 3, suma 2, mult 4, resta 24]
\end{code}

Y para probar nuestra función, creamos un programa sencillo:

\begin{code}
main = do
       putStrLn "Introduzca un número: "
       n <- readLn :: IO Int
       let (resultado, registro) = runWriter (h n)
       putStrLn $ "El resultado es " ++ show resultado
       putStrLn $ "\nLas operaciones realizadas han sido:\n" ++ unlines registro
\end{code}
