---
title: Laboratorio de Programación Orientada a Objetos
author: Alvarez Ernesto, Frattini Nicolas, Gomez Ignacio
---

El enunciado del laboratorio se encuentra en [este link](https://docs.google.com/document/d/1wLhuEOjhdLwgZ4rlW0AftgKD4QIPPx37Dzs--P1gIU4/edit#heading=h.xe9t6iq9fo58).

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [x] Java 17 instalado. Deben poder compilar con `make` y correr con `make run` para obtener el mensaje de ayuda del programa.

## 1.1. Interfaz de usuario
- [x] Estructurar opciones
- [x] Construir el objeto de clase `Config`

## 1.2. FeedParser
- [x] `class Article`
    - [x] Atributos
    - [x] Constructor
    - [x] Método `print`
    - [x] _Accessors_
- [x] `parseXML`

## 1.3. Entidades nombradas
- [x] Pensar estructura y validarla con el docente
- [x] Implementarla
- [x] Extracción
    - [x] Implementación de heurísticas
- [x] Clasificación
    - [x] Por tópicos
    - [x] Por categorías
- Estadísticas
    - [x] Por tópicos
    - [x] Por categorías
    - [x] Impresión de estadísticas

## 1.4 Limpieza de código
- [x] Pasar un formateador de código
- [x] Revisar TODOs

# 2. Experiencia
En este laboratorio aprendimos a utilizar Java para implementar un lector automatico de feeds, detectar entidades nombradas y generar ciertas estadisticas relacionadas a estas entidades. 

# 3. Preguntas
1. Explicar brevemente la estructura de datos elegida para las entidades nombradas.
Para representar a las entidades nombradas se creo una clase abstracta `NamedEntity` que tiene los atributos `name` que es la string que refiere a la entidad nombrada y `topics` que la lista de topicos que tiene la entidad nombrada. Dentro de esta clase definimos dos enum `Category` y `Topic` los cuales tienen como elementos a las categorias y topicos respectivamente provistos en la consigna. Luego, para cada categoria se creo una clase publica que hereda de `NamedEntity` ya que cada categoria tiene al menos un atributo unico. Entonces, si una entidad nombrada es de la categoria, por ejemplo, `Person` se crea un objeto de clase `Person` con sus respectivos atributos. Se decidio implementar de esta forma ya que esto permite que cada entidad nombrada, ademas tener atributos en comun, puedan tener sus propios atributos aunque en la implementacion de este laboratorio no se utilizan. A la clase `NamedEntity` se la definio como abstracta para que no se creen objetos de esta ya que solo sirve como clase madre de las otras entidades nombradas especificas (`Person`, `Location`, etc.). Finalmente, para las funciones que trabajan sobre estas entidades nombradas se creo la clase `NamedEntityProcessor` que junta todas estas funciones.
2. Explicar brevemente cómo se implementaron las heurísticas de extracción.
Las heuristicas implementadas fueron `CapitalizedVowelsHeuristics` y `CapitalizedConsonantsHeuristics`, ambas usando pattern matching con regex.  La primera elije aquellas palabras que empiezan con una mayuscula vocal, mientras que la segunda elije palabras que empiezan con una mayuscula consonante.
# 4. Extras
Completar si hacen algo.