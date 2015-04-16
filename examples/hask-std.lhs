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
import Prelude()
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
data Void = Void Void

absurd :: Void -> a
absurd (Void a) = absurd a 
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


Cartesianamente cerrada
------------------

La categoría **Hask** es cerrada respecto a la estructura de monoide que tiene
el producto categórico de tipos. Podemos ver que:

* `((),a)` es isomorfo a `a`
* `(a,(b,c))` es isomorfo a `((a,b),c)`

Teniendo la estructura de monoide con el producto.

\begin{code}
prod_unit :: ((),a) -> a
prod_unit ((),x) = x

prod_assoc :: (a,(b,c)) -> ((a,b),c)
prod_assoc (x,(y,z)) = ((x,y),z)
\end{code}


Funtores
------------------

Los constructores de tipos de la clase `Functor` son endofuntores de esta 
categoría. Un functor `f` se aplica sobre los tipos como `f a` y sobre los 
morfismos con `fmap g`.

Las leyes de los functores deben cumplirse al definirlos:

``` Haskell
 fmap id == id
 fmap (f . g) == fmap f . fmap g
```


Lema de Yoneda
--------------------

La formulación en teoría de categorías del lema de Yoneda nos dice que hay una
correspondencia biyectiva entre transformaciones naturales desde el funtor
$Hom(A,-)$ a $F$ y $F(A)$, para cualquier funtor $F$ que vaya a la categoría
**Set**:

$$ Nat(Hom(A,-),F) \cong F(A) $$

Este resultado, en tipos de Haskell se traduce en que equivalen los dos tipos
siguientes:

```Haskell
 (a -> b) -> f b  ~~  f a
```

Donde `a` es algún tipo pero `b` es una variable arbitraria de tipo. De manera
más exacta, debe escribirse como:

```Haskell
 forall b . (a -> b) -> f b ~~ f a
```

Pueden encontrarse en las referencias una demostración y una explicación en mayor
detalle. [^bartosz-yoneda]

[^bartosz-yoneda]: Understanding Yoneda. [Bartosz Milewski](http://bartoszmilewski.com/2013/05/15/understanding-yoneda/).
