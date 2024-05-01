#include <stdio.h>
#include <stdlib.h>
#include "APIG24.h"
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <strings.h>
#include <string.h>

// compilacion: gcc -Wall -Wextra -O3 -std=c99
// compilacion para grafos mas chicos: gcc -Wall -Wextra -O3 -std=c99 -DNDEBUG -fsanitize=address,undefined
// valgrind: valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --verbose

typedef struct s_tuple_ {
    u32 v1;
    u32 v2;
} * tuple;

Grafo ConstruirGrafo() { 
    // inicializo
    Grafo grafo = NULL;

    grafo = calloc(1,sizeof(struct s_GrafoSt_));
    assert(grafo != NULL);

    u32 v,w;
    // char c;
    // leo la primera linea con info sobre el grafo
    u32 line_count = 0;

    while (scanf(" p edge %u %u\n", &grafo->cv, &grafo->cl) != 2) {
        while (getchar() != '\n');
    }
    grafo->grados = calloc(grafo->cv, sizeof(u32));
    assert(grafo->grados != NULL);

    grafo->colores = calloc(grafo->cv, sizeof(color));
    assert(grafo->colores != NULL);

    grafo->vecinos = calloc(grafo->cv, sizeof(u32 *));
    assert(grafo->vecinos != NULL);
    grafo->delta = 0;

    tuple *temp = calloc(grafo->cl, sizeof(tuple));
    // O(m)
    while (fscanf(stdin, "e %u %u\n", &v, &w) != EOF) {
        tuple tuple = calloc(1, sizeof(struct s_tuple_));
        assert(tuple != NULL);
        tuple->v1 = v;
        tuple->v2 = w;
        temp[line_count] = tuple;

        line_count++;

        // incremento el grado de v y w
        grafo->grados[v]++;
        grafo->grados[w]++;

        // busco el maximo grado, osea, delta
        if (grafo->grados[v] > grafo->delta) {
            grafo->delta = grafo->grados[v];
        }
        if (grafo->grados[w] > grafo->delta) {
            grafo->delta = grafo->grados[w];
        }
        
        if (line_count >= grafo->cl) {
            break;
        }
    }
    // O(n)
    for (u32 v = 0; v < grafo->cv; v++) {
        grafo->vecinos[v] = calloc(grafo->grados[v], sizeof(struct s_vecinos_));
        assert(grafo->vecinos[v] != NULL);
        grafo->vecinos[v]->vecinos = calloc(grafo->grados[v], sizeof(u32));
        grafo->vecinos[v]->pos = 0;
        assert(grafo->vecinos[v]->vecinos != NULL);
    }
    // O(m)
    for (u32 i = 0; i < grafo->cl; i++) {
        u32 v1 = temp[i]->v1;
        u32 v2 = temp[i]->v2;
        
        // // agrego a w como vecino de v y viceversa
        grafo->vecinos[v1]->vecinos[grafo->vecinos[v1]->pos] = v2;
        grafo->vecinos[v1]->pos++;
        
        grafo->vecinos[v2]->vecinos[grafo->vecinos[v2]->pos] = v1;
        grafo->vecinos[v2]->pos++;
        free(temp[i]);
    }
    free(temp);
    
    // complejidad: O(n+m)
    return grafo;
}

void DestruirGrafo(Grafo G) {
    free(G->grados);
    free(G->colores);

    for (u32 i = 0; i < G->cv; i++) {
        free(G->vecinos[i]->vecinos);
        free(G->vecinos[i]);
    }

    free(G->vecinos);
    free(G);
}

// 4. Funciones para extraer informacion de datos del grafo

u32 NumeroDeVertices(Grafo G) {
    return G->cv;
}

u32 NumeroDeLados(Grafo G) {
    return G->cl;
}

u32 Delta(Grafo G) {
    return G->delta;
}

// 5. Funciones para extraer informacion de los vertices

u32 Grado(u32 i, Grafo G) {
    if (i < G->cv) {
        return G->grados[i];
    }
    else {
        return 0;
    }
}

color Color(u32 i, Grafo G) {
    if (i < G->cv) {
        return G->colores[i];
    }
    else {
        return pow(2,32) - 1;
    }
}

u32 Vecino(u32 j, u32 i, Grafo G) {
    if ((i >= G->cv) || (i < G->cv && j >= G->grados[i])) {
        return pow(2,32) - 1;
    }
    else {
        if (i < G->cv && j < G->grados[i]) {
            return G->vecinos[i]->vecinos[j];
        }
    }
    return -1;
}

// 6. Funciones para asignar colores

void AsignarColor(color x, u32 i, Grafo G) {
    if (i < G->cv) {
        G->colores[i] = x;
    }
}

void ExtraerColores(Grafo G, color* Color) {
    for (u32 v = 0; v < G->cv; v++) {
        Color[v] = G->colores[v];
    }
}

void ImportarColores(color* Color, Grafo G) {
    for (u32 v = 0; v < G->cv; v++) {
        G->colores[v] = Color[v];
    }
}
