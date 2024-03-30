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


node CrearNodo(u32 v) {
    node new_node = calloc(1, sizeof(struct node));
    new_node->vertice = v;
    new_node->next = NULL;
    return new_node;
}


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

        grafo->vecinos = calloc(grafo->cv, sizeof(struct node*));
        assert(grafo->vecinos != NULL);
        grafo->delta = 0;
    } 
    else if (getchar() != 'c') {
        return NULL;
    } 

    // leo los lados y averiguo delta
    while (fscanf(stdin, "e %u %u\n", &v, &w) == 2) {
        line_count++;
        
        // agrego a w como vecino de v y viceversa
        node dest = CrearNodo(w);
        dest->next = grafo->vecinos[v];
        grafo->vecinos[v] = dest;
        
        node source = CrearNodo(v);
        source->next = grafo->vecinos[w];
        grafo->vecinos[w] = source;

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
    
    return grafo;

}

void DestruirGrafo(Grafo G) {
    free(G->grados);
    free(G->colores);

    for (u32 i = 0; i < G->cv; i++) {
        node current = G->vecinos[i];
        node next = NULL;
        while (current != NULL) {
            next = current->next;
            free(current);
            current = next;
        }
        // free(G->vecinos[i]);
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
            // recorro la lista j veces hasta llegar al j-ésimo vertice y != NULL
            node curr_node = G->vecinos[i];
            u32 node_count = 0;
            while (curr_node != NULL && node_count < j) {
                curr_node = curr_node->next;
                node_count++;
            }
            if (curr_node != NULL) {
                return curr_node->vertice;
            }
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
    //     // printf("Los vecinos de %u son: ", i);
    //     for (u32 j = 0; j < G1->grados[i]; j++) {
    //         u32 vecino = Vecino(j,i,G1);
    //         // printf(" %u ", vecino); 
    //     }
    //     // printf("\n");
    // }
    
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