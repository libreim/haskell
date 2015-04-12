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



