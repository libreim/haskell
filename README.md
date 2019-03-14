# haskell

Repositorio de recursos de libreim sobre Haskell.

## Cuaderno en IHaskell

[Introducción a Haskell interactiva](https://github.com/libreim/haskell/blob/master/PrimeraParte/PrimeraParte.ipynb) que describe desde cero el sistema de tipos, cómo definir y utilizar funciones, tipos y constructores de tipos y funtores. Una versión actualizada del código original utilizado en la charla está disponible [aquí](https://github.com/libreim/haskell/tree/master/PrimeraParte).

Contiene ejercicios sobre los temas introducidos que pueden realizarse en el mismo cuaderno si **se ejecuta con [IHaskell](https://github.com/gibiansky/IHaskell)**.

También se incluyen [algunos ejercicios](https://github.com/libreim/haskell/blob/master/ejercicios.md) adicionales.


## Introducción a Haskell

Los primeros apuntes que creamos están revisados y [disponibles en pdf](https://github.com/libreim/haskell/blob/master/apuntes/introHaskell.pdf). Son una introducción corta que describe desde cero el sistema de tipos, los constructores de tipos más habituales (listas y tuplas) y cómo definir y utilizar funciones y tipos.

También está disponible [el código fuente](https://github.com/libreim/haskell/tree/master/apuntes) en LaTeX.

## Funtores y Mónadas

Ejemplos autocontenidos de funtores y uso de mónadas. Requieren los conocimientos de las introducciones desde cero.

* [Funtores](https://github.com/libreim/haskell/blob/master/SegundaParte/1.funtores.hs)
* [Manejo de errores con `Maybe`](https://github.com/libreim/haskell/blob/master/SegundaParte/2.errores.hs)
* [Distribuciones de probabilidad con `State`](https://github.com/libreim/haskell/blob/master/SegundaParte/3.distribuciones.hs)
* [Ejemplo de parser con `Parsec`](https://github.com/libreim/haskell/blob/master/SegundaParte/4.parsers.hs)

## Haskell en la OSL

Como parte de una reunión de usuarios de Github en Granada dimos una charla en la [OSL](http://osl.ugr.es). Incluimos aquí [la presentación con notas](https://github.com/libreim/haskell/blob/master/CharlaOSL.pdf).

### Ejemplos en Haskell literario

Ejemplos autocontenidos realizados en [Haskell literario](https://wiki.haskell.org/Literate_programming). Pueden compilarse a pdf o ejecutarse con ghc mediante el Makefile que incluimos. Requieren conocer los contenidos descritos en las introducciones desde cero:

* [Quicksort](https://github.com/libreim/haskell/blob/master/Haskell%20Literario/quicksort.lhs)
* [Árboles binarios](https://github.com/libreim/haskell/blob/master/Haskell%20Literario/bintree.lhs)
* [Sucesión de Fibonacci](https://github.com/libreim/haskell/blob/master/Haskell%20Literario/fibonacci.lhs)
* [Monoides](https://github.com/libreim/haskell/blob/master/Haskell%20Literario/monoides.lhs)
* [La categoría Hask](https://github.com/libreim/haskell/blob/master/Haskell%20Literario/hask.lhs)

## Posts

En el [blog](https://libreim.github.io/blog/) de libreim hay disponibles varios posts que expanden distintos temas de Haskell, relacionados con la teoría de tipos y la teoría de categorías:

- [Teoría de tipos](https://libreim.github.io/blog/2016/01/08/teoria-de-tipos/) una lista de lectura sobre teoría de tipos. Incluye:
    - [Álgebra de tipos](https://libreim.github.io/blog/2015/03/24/algebra-tipos). Construcción de un álgebra sobre los tipos de Haskell
    - [Inducción estructural](https://libreim.github.io/blog/2015/03/14/induccion-estructural/). Generalización de la inducción para trabajar sobre tipos
    - [Isomorfismo de Curry-Howard](https://libreim.github.io/blog/2014/12/04/curry-howard/). Exposición del isomorfismo de Curry-Howard en Haskell y su utilización en Coq
- [Mónadas](https://libreim.github.io/blog/2016/12/21/monadas/). Motivación de las mónadas y visión categórica. Esta última parte requiere conocer conceptos de [teoría de categorías](https://libreim.github.io/blog/2014/10/04/intro-categorias).

- Más recursos en nuestra [página de recursos](https://libreim.github.io/recursos/)
