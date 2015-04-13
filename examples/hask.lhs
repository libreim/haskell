---
title: La categoría Hask
author: Pablo Baeyens, Mario Román
---

Los tipos como categoría
----------------

La categoría **Hask** es la que toma a los tipos de Haskell como objetos y a las
funciones de un tipo a otro como los morfismos entre ellos. Podemos comprobar
que cumplen la definición de categoría:

```Haskell
-- La composición de funciones es asociativa
f . (g . h) === (f . g) . h 
-- Hay un morfismo identidad para cualquier objeto
id . f === f === f . id
``` 

Para usarla tendremos que excluir algunos casos extremos, como funciones
definibles que no terminan nunca o la instancia `undefined`. 
Estos detalles pueden consultarse en la wiki de Haskell. [^hask-wiki]

[^hask-wiki]: Hask. [Haskell wiki](https://wiki.haskell.org/Hask).

\begin{code}
{-# OPTIONS_GHC -XEmptyDataDecls -XEmptyCase #-}
\end{code}


Objetos inicial y final
-------------------

El tipo **final** de la categoría es aquel para el que existe una única
función desde cualquier otro tipo. Será isomorfo a `()`.

\begin{code}
unit :: a -> ()
unit _ = ()
\end{code}

Esta función existe desde cualquier tipo, y no puede existir ninguna otra
porque sólo tenemos una forma de construir una instancia de `()`.


El tipo **inicial** de la categoría es aquel para el que existe una única
función hacia cualquier otro tipo. Será isomorfo a `Void`, el tipo vacío
que definimos sin constructores; nunca habrá forma de construirlo.

\begin{code}
data Void

absurd :: Void -> a
absurd v = case v of {}
\end{code}

Nótese que no hay que definir nada más para la función `absurd`. Hemos usado 
`pattern matching` contra todos los constructores de `Void`. Que es la única función
posible es obvio por esto mismo.


Productos y coproductos
------------------

El **producto** de dos tipos lo generamos con el constructor de tipos
`(,)`. Cualesquiera dos tipos tienen un producto en esta categoría. Las
proyecciones serán `fst` y `snd`.

\begin{code}
fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y
\end{code}

Y otro tipo con morfismos hacia ambos podrá descomponerse a través del
producto.

\begin{code}
(&&&) :: (c -> a) -> (c -> b) -> (c -> (a,b))
(&&&) f g = (\x -> (f x, g x))
\end{code}


El **coproducto** de dos tipos lo generamos con el constructor de tipos
`Either`. Cualesquiera dos tipos tienen un coproducto en esta categoría.
Las coproyecciones serán `Left` y `Right`.

\begin{code}
data Either a b = Left a | Right b
\end{code}

Y otro tipo con morfismos desde ambos podrá descomponerlos a través del
coproducto.

\begin{code}
either :: (a -> c) -> (b -> c) -> (Either a b -> c)
either f g (Left  x) = f x
either f g (Right y) = g y
\end{code}
