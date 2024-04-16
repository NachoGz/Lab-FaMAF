#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <malloc.h>

#include "API2024Parte2.h"
#include "APIG24.h"
#include "proyectoParte1.c"


// compilacion: gcc -Wall -Wextra -O3 -std=c99
// compilacion para grafos mas chicos: gcc -Wall -Wextra -O3 -std=c99 -DNDEBUG -fsanitize=address,undefined
// valgrind: valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --verbose

u32 Greedy(Grafo G, u32* Orden) {
    color cant_colores = 1;
    color col;
    color *colores_vecinos;
            bool used;

    // u32 delta = Delta(G);
    for (unsigned int i=0; i < NumeroDeVertices(G)-1; i++) {
        if (Orden[i] > Orden[i+1]) {
            return (pow(2,32) - 1);
        }
    }
    
    for (unsigned int i=0; i < NumeroDeVertices(G); i++) {
        col = 1;
        u32 grado = Grado(Orden[i], G);
        colores_vecinos = calloc(grado+1, sizeof(color));

        // obtengo los colores de los vecinos
        for (u32 vecino=0; vecino < grado; vecino++) {
            color color_vecino = Color(Vecino(vecino, Orden[i], G), G);
            colores_vecinos[vecino] = color_vecino; 
        }
        color curr_color = 1;
        for (curr_color=1; curr_color <= cant_colores; curr_color++) {
            used = false;
            for (u32 vecino=0; vecino < grado; vecino++) {            
                if (colores_vecinos[vecino] == curr_color) {
                    used = true;
                    break;
                }
            }
            // si encontre un color libre, corto
            if (!used) {
                break;
            }
        }
        
        // le asigno a col el ultimo color que se chequeo
        col = curr_color;

        // si estan todos los colores usados, "creo" uno nuevo
        if (used == 1) {
            col++;            
        }
        
        AsignarColor(col, Orden[i], G);
        if (col > cant_colores) {
            cant_colores = col;
        }
        free(colores_vecinos);
    }

    return cant_colores;
}

int main(void) {
    Grafo G = ConstruirGrafo();

    u32 *Orden = calloc(NumeroDeVertices(G), sizeof(u32));
    for (u32 i=0; i < NumeroDeVertices(G); i++){
        Orden[i] = i;
    }

    color colores = Greedy(G, Orden);
    free(Orden);
    color *colores_extraidos = calloc(NumeroDeVertices(G), sizeof(color)); 
    ExtraerColor(G, colores_extraidos);
    printf("colores usados para colorear el grado: %u\n", colores);
    printf("--------------COLORES-------------\n");
    for (u32 i=0; i< NumeroDeVertices(G); i++) {
        printf("color de %u: %u\n", i, colores_extraidos[i]);
    }
}

