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


#define MAX_LINE_LENGTH 200

Grafo ConstruirGrafo() { 
    // inicializo
    Grafo grafo = NULL;

    grafo = calloc(1,sizeof(struct s_GrafoSt_));
    assert(grafo != NULL);

    // leo la primera linea con info sobre el grafo
    char linea[MAX_LINE_LENGTH];
    u32 line_count = 0;
    
    while (fgets(linea, sizeof(linea), stdin) != NULL) {
        if (linea[0] == 'p') {
            sscanf(linea, "p edge %u %u", &grafo->cv, &grafo->cl);

            grafo->grados = calloc(grafo->cv, sizeof(u32));
            assert(grafo->grados != NULL);

            grafo->colores = calloc(grafo->cv, sizeof(color));
            assert(grafo->colores != NULL);

            grafo->vecinos = calloc(grafo->cv, sizeof(u32 *));
            assert(grafo->vecinos != NULL);
            grafo->delta = 0;
            
        } 
        else if (linea[0] == 'e') {
            u32 v,w;
            sscanf(linea, "e %u %u", &v, &w);
            // printf("vertices: %u %u\n", v, w);
            line_count++;
            if (line_count > grafo->cl) {
                break;
            }
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
        }
        else if (linea[0] == 'c') {
            continue;
        }
        else {
            return NULL;
        }
    }

    rewind(stdin);
    while (fgets(linea, sizeof(linea), stdin) != NULL) {
        if (linea[0] == 'e') {
            u32 v,w;
            sscanf(linea, "e %u %u", &v, &w);
            // printf("vertices: %u %u\n", v, w);
            line_count++;
            if (line_count > grafo->cl) {
                break;
            }
        if (grafo->vecinos[v] == NULL) {
                // printf("vertice: %u\n", v);
                grafo->vecinos[v] = calloc(grafo->delta, sizeof(u32));
                assert(grafo->vecinos[v] != NULL);
        }
        if (grafo->vecinos[w] == NULL) {
            grafo->vecinos[w] = calloc(grafo->delta, sizeof(u32));
            assert(grafo->vecinos[w] != NULL);
        } 
        // agrego a w como vecino de v y viceversa
        grafo->vecinos[v][grafo->grados[v]-1] = w;
        grafo->vecinos[w][grafo->grados[w]-1] = v;       
        }
    }        
    return grafo;
}

void DestruirGrafo(Grafo G) {
    free(G->grados);
    free(G->colores);

    for (u32 i = 0; i < G->cv; i++) {
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
            return G->vecinos[i][j];
        }
    }
    return -1;
}

int main(void) {
    Grafo G1 = ConstruirGrafo();
    assert(G1 != NULL);

    printf("El grafo 1 tiene %u vertices\n", NumeroDeVertices(G1));
    printf("El grafo 1 tiene %u lados\n", NumeroDeLados(G1));
    printf("El valor de delta es: %u\n", Delta(G1));

    DestruirGrafo(G1);
}

// 6. Funciones para asignar colores

void AsignarColor(color x, u32 i, Grafo G) {
    if (i < G->cv) {
        G->colores[i] = x;
    }
}

void ExtraerColor(Grafo G, color* Color) {
    for (u32 v = 0; v < G->cv; v++) {
        Color[v] = G->colores[v];
    }
}

void ImportarColores(color* Color, Grafo G) {
    for (u32 v = 0; v < G->cv; v++) {
        G->colores[v] = Color[v];
    }
}