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
height Node x lft rgt = 1 + (max (height lft) (height rgt))
\end{code}
