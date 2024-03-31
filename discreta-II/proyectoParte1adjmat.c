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

Grafo ConstruirGrafo() { 
    // inicializo
    Grafo grafo = NULL;

    grafo = calloc(1,sizeof(struct s_GrafoSt_));
    assert(grafo != NULL);

    u32 v,w;
    // leo la primera linea con info sobre el grafo
    u32 line_count = 0;
    
    if (fscanf(stdin, "p edge %u %u\n", &grafo->cv, &grafo->cl) == 2) {
        grafo->grados = calloc(grafo->cv, sizeof(u32));
        if (grafo->grados == NULL) {
            fprintf(stderr, "Error alocando memoria para el campo grados\n");
            free(grafo);
            return NULL;
        }

        grafo->colores = calloc(grafo->cv, sizeof(color));
        if (grafo->colores == NULL) {
            fprintf(stderr, "Error alocando memoria para el campo colores\n");
            free(grafo->grados);
            free(grafo);
            return NULL;
        }

        grafo->delta = 0;
    } 
    else if (getchar() != 'c') {
        return NULL;
    }

    //aloco espacio para la matriz
    grafo->vecinos = calloc(grafo->cv, sizeof(bool *));
    if (grafo->vecinos == NULL) {
        fprintf(stderr, "Error alocando memoria para el campo vecinos\n");
        free(grafo->grados);
        free(grafo->colores);
        free(grafo);
        return NULL;
    }

    for (u32 i = 0; i < grafo->cv; i++) {
        grafo->vecinos[i] = calloc(grafo->cv, sizeof(bool));
        if (grafo->vecinos[i] == NULL) {
            fprintf(stderr, "Error alocando memoria para el campo vecinos[%u]\n", i);
            DestruirGrafo(grafo);
            return NULL;
        }
    }
    printf("todo bien\n");
    while (fscanf(stdin, "e %u %u\n", &v, &w) == 2) {
        line_count++;

        // incremento el grado de v y w
        grafo->grados[v]++;
        grafo->grados[w]++;

        // agrego a w como vecino de v y viceversa
        if (!grafo->vecinos[v][w]) {
            grafo->vecinos[v][w] = true;    
        }

        if (!grafo->vecinos[w][v]) {
            grafo->vecinos[w][v] = true;    
        }

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


    return grafo;
}

void DestruirGrafo(Grafo G) {
    assert(G != NULL);
    free(G->grados);
    free(G->colores);

    for (u32 i = 0; i < G->cv; i++) {
        if (G->vecinos[i] != NULL) {
            free(G->vecinos[i]);    
        }
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
        printf("j: %u\n", j);
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

    

    // for (u32 i = 0; i < G1->cv; i++) {
    //     printf("Los vecinos de %u son: ", i);
    //     for (u32 j = 0; j < G1->grados[i]; j++) {
    //         printf(" %u ", Vecino(j,i,G1));
    //     }
    //     printf("\n");
    // }
    DestruirGrafo(G1);
    // Grafo G2 = ConstruirGrafo();
    // assert(G2 != NULL);

    // printf("El grafo 2 tiene %u vertices\n", NumeroDeVertices(G2));
    // printf("El grafo 2 tiene %u lados\n", NumeroDeLados(G2));
    // printf("El valor de delta es: %u\n", Delta(G2));
    
    // DestruirGrafo(G2);

    // Grafo G3 = ConstruirGrafo();
    // assert(G3 != NULL);

    // printf("El grafo 3 tiene %u vertices\n", NumeroDeVertices(G3));
    // printf("El grafo 3 tiene %u lados\n", NumeroDeLados(G3));
    // printf("El valor de delta es: %u\n", Delta(G3));
    
    // DestruirGrafo(G3);

    // Grafo G4 = ConstruirGrafo();
    // assert(G4 != NULL);

    // printf("El grafo 4 tiene %u vertices\n", NumeroDeVertices(G4));
    // printf("El grafo 4 tiene %u lados\n", NumeroDeLados(G4));
    // printf("El valor de delta es: %u\n", Delta(G4));
    
    // DestruirGrafo(G4);
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