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


Objetos inicial y final
-------------------

El tipo **final** de la categoría es aquel para el que existe una única
función desde cualquier otro tipo. Será isomorfo a `()`.

\begin{code}
unit :: a -> ()
unit _ = ()
\end{code}

Esta función existe desde cualquier tipo y no puede existir ninguna otra,
porque sólo tenemos una forma de construir una instancia de `()`.


El tipo **inicial** de la categoría es aquel para el que existe una única
función hacia cualquier otro tipo. Será isomorfo a `Void`, el tipo vacío
que definimos sin constructores.

\begin{code}
data Void

absurd :: Void -> a
\end{code}

Nótese que no hay que definir nada para la función `absurd`. Hemos usado 
`pattern matching` contra todos los constructores de `Void`, es decir,
ninguno. Que es la única posible es obvio por esto mismo.
