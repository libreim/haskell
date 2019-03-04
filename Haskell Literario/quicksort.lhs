---
title: Quicksort
author: Pablo Baeyens, Mario Román
---

Implementación
----------------

El algoritmo Quicksort toma el primer elemento de la lista, coloca a su izquierda
los menores que él y a su derecha los mayores, y vuelve a aplicarse a ambos
lados.

Lo definimos recursivamente. La lista vacía está ya ordenada:

\begin{code}
qsort [] = []
\end{code}

Dada una lista no vacía, tomamos el elemento inicial y colocamos a un lado y
otro de él las listas de menores y mayores, respectivamente. Ordenamos esas
listas.

\begin{code}
qsort (x:xs) = qsort [y | y<-xs, y<=x]
            <> [x]
            <> qsort [y | y<-xs, y>x]
\end{code}


Ejemplo
---------------
Este programa ordena una lista de números:
\begin{code}
main :: IO ()
main = do putStrLn "Introduzca números a ordenar separados por espacios: "
          contents <- getLine
          let numbers = map read $ words contents :: [Integer]
          print $ qsort numbers
\end{code}
