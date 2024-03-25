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
            
            for (u32 i = 0; i < grafo->cv; i++) {
                grafo->vecinos[i] = calloc(grafo->cv, sizeof(u32));
                assert(grafo->vecinos[i] != NULL);
                grafo->colores[i] = 0;
            }
        }
        else if (linea[0] == 'e') {
            u32 v,w;
            sscanf(linea, "e %u %u", &v, &w);
            line_count++;
            if (line_count > grafo->cl) {
                break;
            }
            // incremento el grado de v y w
            grafo->grados[v]++;
            grafo->grados[w]++;
            
            // agrego a w como vecino de v y viceversa
            grafo->vecinos[v][grafo->grados[v]-1] = w;
            grafo->vecinos[w][grafo->grados[w]-1] = v;
            

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
    // if (grafo->cl != line_count) {
    //     return NULL;
    // }
    
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
    Grafo G = ConstruirGrafo();
    assert(G != NULL);

    printf("El grafo tiene %u vertices\n", NumeroDeVertices(G));
    printf("El grafo tiene %u lados\n", NumeroDeLados(G));

/*     for (u32 v = 0; v < G->cv; v++) {
        printf("Los vecinos de %u son: ", v);
        for (u32 n = 0; n < G->grados[v]; n++) {
            // printf(" %u ", G->vecinos[v][n]);
            printf(" %u ", Vecino(n, v, G));
        }
        printf("\n");
    } */
    printf("El valor de delta es: %u\n", Delta(G));
    
    DestruirGrafo(G);
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