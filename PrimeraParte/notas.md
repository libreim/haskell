# Intro

- Haskell platform: `haskell.org/platform`
- Mientras se instala usad `ghc.io`
- `ghci` sirve como debugger y como consola interactiva (al estilo e `irb`). Se sale con `:q`
- Estoy siguiendo más o menos los apuntes de `github.com/libreim/introHaskell`.

# ¿Qué es la programación funcional?

- Lenguaje de programación funcional puro, perezoso
- Se basa en definiciones, no asignaciones
- Las funciones no tienen efectos secundarios.
- Los programas se basan en sustituir iguales por iguales

Algunos ejemplos para probar en la consola.

```haskell
5 + 6 * 4
"Hello" ++ " World !"
True && False
not True
3/4 -- La división NO es entera
5 == 2+3
```

Para llamar a funciones ponemos el nombre y luego los argumentos separados por espacios:

```haskell
succ 33 -- El sucesor de 33
succ 'a' -- El sucesor de 'a'
max 3 4
max 3 4 - 2
max 3 (4 - 2)
(+) 5 6
```
- Para definir cosas usamos los comandos básicos son:
  - `:l`, cargar un archivo
  - `:r`, recargar

- En el ejemplo podemos optimizar `f a` y calcularlo sólo una vez. En un programa imperativo no podríamos hacer esta optimización (podría contactar con el Mundo Real, tocar variables globales, generar un número aleatorio...). De esta forma podemos **razonar algebraicamente y paralelizar fácilmente**.

# Los tipos. Ejemplos y primeras definiciones

- El sistema de tipos es fuerte, estático e inferido (salvo alguna cosa). En los ejemplos anteriores no hemos escrito los tipos, porque el compilador y el intérprete son capaces de inferirlos

```haskell
False == 0 -- Los tipos son estáticos
:t True -- Los tipos empiezan con Mayúscula
:t not -- Las funciones en cambio siempre empiezan en minúscula
:t (&&) -- Hay que poner paréntesis para mirar el tipo
```
- En nuestro ejemplo el compilador es capaz de saber el tipo de `b` a partir de los de `a` y `f`
- ¿Qué tipo tiene la función identidad? Dado que no imponemos ninguna restricción sobre la variable que tomamos como argumento, utilizamos una **variable de tipo**. Se escriben en minúscula y con nombres cortos.

- Reconocimiento de patrones. Los patrones se leen en orden y se intenta encajar en cada patrón hasta que se encuentra uno en el que encaja. Podemos poner variables en los patrones. Los patrones deben ser exhaustivos.

- La currificación es la identificación entre dos funciones.
- Composición de funciones

# Los constructores de tipos. Las listas. ¿Cómo se construye un tipo? Los naturales

- Un constructor de tipos es una función sobre tipos
- Las listas. Tamaño no fijo, tipo homogéneo. String es [Char]
- Se construyen con `:` y `[]`: `[1 ,2 ,3] == 1:2:3:[]`
  - ¿Cuál es el tipo de `[]` y `:`?
- Definir funciones básicas (head, length, repeat, ++, elem, !!)
 - Dejar algunas como ejercicio
- Funciones de orden superior (directamente ir con map, filter y foldr)
  - Con filter pueden hacer una versión sencilla de quicksort
  - Con foldr puede definirse concat, sum, maximum, minimum, product...
    - Expresar foldr como:
     `foldr (×) z [a1,...,an] = a1 × (a2 × (... × (an × z)))`
  - foldr tiene más poder expresivo y puede definir filter y map (ejercicio)

# Definición de tipos
  - Tipos simples
    - Bool, un tipo que sea una enumeración. Int (minBound, maxBound)
  - Tipos con campos
    - RGB
  - Tipos recursivos (Nat). Proponer ejercicios con Nat.
  - Las listas. Definición con sintaxis no legal. Árboles binarios

# Las clases de tipos. ¿Qué clases de tipos hay? ¿Cómo funcionan?

- Clases de tipos más frecuentes (Num, Ord, Show, Read, Floating, Eq..)
- Creación de una instancia de tipo (con `Eq` por ejemplo)
- Los funtores si da tiempo?
-
