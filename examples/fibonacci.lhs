---
title: Fibonacci. Listas infinitas
author: Pablo Baeyens, Mario Román
---

Listas infinitas
----------------

Gracias a la evaluación perezosa de Haskell podemos definir listas infinitas
sin que haya problema. Recursivamente, podemos usar funciones con *punto fijo*:

\begin{code}
naturals = 1 : map (+1) naturals
\end{code}

**¿Por qué funciona esta definición?** Cuando le pedimos el primer elemento de
la lista, obviamente nos dará `1`; si le pedimos el segundo, sólo necesitará
conocer el primero para calcularlo. Podemos usar `naturals` en su propia
definición porque nunca los llegamos a necesitar todos.

En Haskell, puede usarse directamente `[1..]` para notar esa lista infinita.

Además podemos seguir manipulando listas infinitas para crear otras listas
infinitas. Si `zipWith (+)` funciona con listas finitas uniéndolas mediante el
operador:

~~~Haskell
   zipWith (+) [1,2,3] [1,2,3] == [2,4,6]
~~~

Funcionará también con listas infinitas:

\begin{code}
evenNumbers = zipWith (+) naturals naturals
\end{code}


Sucesión de Fibonacci
-------------------

La forma obvia de calcular la sucesión de Fibonacci es mediante recursividad
sobre los naturales.

\begin{code}
fib' 0 = 1
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)
\end{code}

Es, sin embargo, muy poco eficiente. Hay que volver a calcular todas las sumas
cada vez que se calcula un término. Sería más útil tener una lista infinita
de los términos de la sucesión, y tomar el que necesitáramos cada vez con el
operador `!!`. [^hwiki-fib]

\begin{code}
fib n = fibs !! n
\end{code}

[^hwiki-fib]: The Fibonacci Sequence. [Haskell wiki](https://wiki.haskell.org/The_Fibonacci_sequence).

La definición de Fibonacci usará recursión de punto fijo,
ocupará ahora una línea y tendrá eficiencia lineal.

\begin{code}
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
\end{code}

**¿Por qué funciona esta definición?** Lo que estamos haciendo es sumarla contra
sí misma desplazada una posición; la suma es algo así:

~~~haskell
       1 : 1 : 2 : 3 : 5 : 8 : ...
       +   1 : 1 : 2 : 3 : 5 : ...
      --------------------------------
       1 : 2 : 3 : 5 : 8 : 13 ....
~~~
