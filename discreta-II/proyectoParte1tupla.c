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
    // leo la primera linea con info sobre el grafo
    u32 line_count = 0;
    
    if (fscanf(stdin, "p edge %u %u\n", &grafo->cv, &grafo->cl) == 2) {
        grafo->grados = calloc(grafo->cv, sizeof(u32));
        assert(grafo->grados != NULL);

        grafo->colores = calloc(grafo->cv, sizeof(color));
        assert(grafo->colores != NULL);

        grafo->vecinos = calloc(grafo->cv, sizeof(u32 *));
        assert(grafo->vecinos != NULL);
        grafo->delta = 0;
    } 
    else if (getchar() != 'c') {
        return NULL;
    } 

    tuple *temp = calloc(grafo->cl, sizeof(tuple));
    while (fscanf(stdin, "e %u %u\n", &v, &w) == 2) {
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

    printf("hasta aca bien\n");
    printf("tam de temp: %lu\n", grafo->cl*sizeof(tuple));
    for (u32 v = 0; v < grafo->cv; v++) {
        grafo->vecinos[v] = calloc(grafo->delta, sizeof(u32));
        assert(grafo->vecinos[v] != NULL);
    }

    printf("seguimos bien (?\n");
    
    int c = 0;
    for (u32 i = 0; i < grafo->cl; i++) {
        u32 v1 = temp[i]->v1;
        u32 v2 = temp[i]->v2;
        // printf("v1: %u, v2: %u\n", v1, v2);
        // aloco espacio solo para aquellos vertices que tengan vecinos
        // if (grafo->vecinos[v1] == NULL) {
        //     c++;
        //     grafo->vecinos[v1] = calloc(grafo->delta, sizeof(u32));
        //     assert(grafo->vecinos[v1] != NULL);
        // }
        
        // if (grafo->vecinos[v2] == NULL) {
        //     c++;
        //     grafo->vecinos[v2] = calloc(grafo->delta, sizeof(u32));
        //     assert(grafo->vecinos[v2] != NULL);
        // } 
        
        // // agrego a w como vecino de v y viceversa
        grafo->vecinos[v1][grafo->grados[v1]-1] = v2;
        grafo->vecinos[v2][grafo->grados[v2]-1] = v1;

        free(temp[i]);
    }
    printf("listo c=%d\n", c);
    free(temp);
    
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