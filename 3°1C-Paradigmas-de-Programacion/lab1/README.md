---
title: Laboratorio de Funcional
author: Alvarez Ernesto, Frattini Nicolas, Gomez Ignacio
---
La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [x] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [x] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [x] Definición de funciones (esquemas) para la manipulación de dibujos.
- [x] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [x] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [x] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [x] Módulo `Dibujos/Grilla.hs`.
- [x] Módulo `Dibujos/Escher.hs`.
- [x] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [x] Tests para `Dibujo.hs`.
- [x] Tests para `Pred.hs`.

# 2. Experiencia
En este laboratorio logramos aprender como implementar un DSL (Domain Specific Language) para dar una abstraccion para crear distintas imagenes con gloss.

# 3. Preguntas
Al responder tranformar cada pregunta en una subsección para que sea más fácil de leer.

1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.

Primero hay que aclarar que en haskell solo se permite un modulo por archivo. El modulo basico es el de Dibujo ya que con este construimos los dibujos.
El módulo de FloatingPic nos permite usar vectores para crear y modificar las figuras que se van a representar en pantalla luego ayudandonos con operaciones simples
como "zero" y "half" por ejemplo.

El módulo Interp tiene algunas funciones bien detalladas en el paper de Henderson que ayudandose con las operaciones de FloatingPic realizan cambios en las figuras
como rotarlas cierta cantidad de grados, encimarlas, apilarlas,etc.

El módulo Pred utiliza un tipo de predicado booleano con el que luego las funciones en el definidas pueden probar si la figura que se pasa como argumento cumple o no
predicados simples.

Luego en Pred.hs se importa el Modulo de Dibujos ya que se usa el tipo dibujo en las definiciones de las funciones.

2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?

Las figuras basicas no estan definidas en la definicion del lenguaje para darle la libertad al desarrolador que utilice el modulo `Dibujo.hs` de elegir la figura basica que quiera. De esta forma, la persona que utilice `Dibujo.hs` puede elegir usar `Dibujo (Int, Int)` como en `Grilla.hs`, `Dibujo Bool` como en `Escher.hs` o incluso definir un tipo de datos propio como en `Feo.hs`.

3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

Utilizar la funcion fold en vez de hacer pattern-matching me permite no tener que escribir el mismo codigo para recorrer recursivamente
un dibujo. Entonces, esto hace el codigo mas simple para leer y tambien me permite modularizar en caso que quiera hacer un cambio en la
forma que recorro los dibujo o incluso si tengo que agregar una funcion constructura, me ahorro tener que agregarla en cada funcion que
haga el mismo huso de pattern-matching. 

4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

Los predicados definidos en Pred.hs ayudan a verificar predicados entre las figuras, mientras que los test se aseguran de que estos funcionen correctamente

# 4. Extras
Completar si hacen algo.