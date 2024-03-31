#ifndef ESTRUCTURAGRAFO24_H
#define ESTRUCTURAGRAFO24_H


#include <stdio.h>
#include <stdlib.h>

typedef unsigned int u32;
typedef u32 color;

typedef struct s_vecinos_ {
    u32 *vecinos;
    u32 pos;
} *vecinos;

typedef struct s_GrafoSt_ {
    u32 cv; // cantidad de vertices
    u32 cl; // cantidad de lados
    u32 *grados; // array dinamico con el grados de cada vertice  
    color *colores; // array dinamico con el color de cada vertice
    u32 delta; 
    vecinos* vecinos; // array donde en vecinos[i] hay un puntero que apunta a las estructura vecinos que contiene un array dinamico y
    // el campo pos que dice el indice del primer lugar libre
} GrafoSt;

// Todos los arrays definidos en el struct estan pensados con la idea de que el indice del array corresponde con el valor del vertice,
// es decir, al vertice 0 esta en el indice 0, etc. 
#endif