---
title: Árboles binarios
author: Pablo Baeyens, Mario Román
---

Definición
----------------

Los árboles binarios se definen muy fácilmente por recursividad. Por un lado
tenemos el árbol vacío, que es el caso base, y por otro tenemos que un árbol
consiste en un nodo del que surgen dos árboles binarios a su vez, un **subárbol
izquierdo** y un **subárbol derecho**.

\begin{code}
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving Show
\end{code}

Lo hacemos polimórfico dependiendo de la variable de tipo `a`.


Funciones básicas
------------------

Con estos árboles, las funciones como los órdenes o la altura se escriben por
recursividad. Por ejemplo, la *altura* de un árbol es uno más de la altura de su
mayor subárbol.

\begin{code}
height :: (Integral b) => Tree a -> b
height Empty = 0
height (Node x lft rgt) = 1 + max (height lft) (height rgt)
\end{code}

El *preorden*, *inorden* y *postorden* surgen también directamente con
recursividad. El caso trivial es la lista vacía, y en otro caso, sólo hay que
colocar el nodo y reordenar los órdenes de los subárboles.

\begin{code}
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node x lft rgt) = preorder lft <> [x] <> preorder rgt
\end{code}


Inserción ordenada
-----------------

Vamos a usar los árboles para implementar el algoritmo de ordenación
`treesort`. Para ello debemos empezar creando árboles binarios ordenados, lo que
hacemos insertando un elemento ordenadamente sobre el árbol. En el caso vacío,
creamos un árbol de un elemento.

\begin{code}
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = Node x Empty Empty
\end{code}

En el caso general, lo comparamos con
el elemento del nodo y lo insertamos en el árbol derecho o izquierdo según el
resultado de la comparación.

\begin{code}
insert (Node y lf rg) x
  | x <= y    = Node y (insert lf x) rg
  | otherwise = Node y lf (insert rg x)
\end{code}

Ahora insertamos una lista completa en un árbol usando `foldl`. Vamos insertando
cada elemento sobre el árbol vacío.

\begin{code}
toTree :: (Ord a) => [a] -> Tree a
toTree = foldl insert Empty
\end{code}

Finalmente, el algoritmo de ordenación consiste en pasar la lista a árbol
binario y volver a pasarlo a una lista de nuevo.

\begin{code}
treesort :: (Ord a) => [a] -> [a]
treesort = preorder . foldl insert Empty
\end{code}


Completando los árboles
------------------

Vamos hacer a los árboles instancias de la clase `Eq`.
Podemos delegar la tarea en el compilador incluyendo `deriving Eq` en la
definición, pero vamos a escribirlo nosotros mismos.

\begin{code}
instance (Eq a) => Eq (Tree a) where
    Empty          == Empty          = True
    (Node x xl xr) == Empty          = False
    (Node x xl xr) == (Node y yl yr) = and [x==y, xl==yl, xr==yr]
\end{code}

Inmediatamente podemos usar `(/=)` en árboles.
