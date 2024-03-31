#ifndef ESTRUCTURAGRAFO24_H
#define ESTRUCTURAGRAFO24_H


#include <stdio.h>
#include <stdlib.h>

typedef unsigned int u32;
typedef u32 color;

typedef struct node {
    u32 vertice;
    struct node* next;
} *node;

typedef struct s_GrafoSt_ {
    u32 cv; // cantidad de vertices
    u32 cl; // cantidad de lados
    u32 *grados; // array dinamico con el grados de cada vertice  
    color *colores; // array dinamico con el color de cada vertice
    u32 delta; 
    struct node** vecinos; // lista enlazada donde en vecinos[i] hay un puntero que apunta al primer vecino del vertice i
} GrafoSt;
// Todos los arrays definidos en el struct estan pensados con la idea de que el indice del array corresponde con el valor del vertice,
// es decir, al vertice 0 esta en el indice 0, etc. 
#endif