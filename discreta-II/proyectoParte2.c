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

typedef struct s_colorFreq {
    u32 color;
    u32 freq;
    u32 *vertices;
} * colorFreq;


// falta verificar la biyeccion
u32 Greedy(Grafo G, u32* Orden) {
    color cant_colores = 1;
    color col;
    color *colores_vecinos;
    bool used;
    color curr_color;

    // u32 delta = Delta(G);
    // for (u32 i=0; i < NumeroDeVertices(G)-1; i++) {
    //     if (Orden[i] > Orden[i+1]) {
    //         return (pow(2,32) - 1);
    //     }
    // }
    
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        col = 1;
        u32 grado = Grado(Orden[i], G);
        colores_vecinos = calloc(grado+1, sizeof(color));

        // obtengo los colores de los vecinos
        for (u32 vecino=0; vecino < grado; vecino++) {
            color color_vecino = Color(Vecino(vecino, Orden[i], G), G);
            colores_vecinos[vecino] = color_vecino; 
        }

        curr_color = 1;
        
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
            // col++;
            cant_colores++;
        }
        AsignarColor(col, Orden[i], G);
       
        free(colores_vecinos);
    }

    return cant_colores;
}

int cmpMayorMenor(const void * a, const void *b) {
 
        u32 x = *(u32 *)a;
        u32 y = *(u32 *)b;
        
        if (x > y) return -1;
        else if (x < y) return 1;
        else return 0;
}

// faltaria el error checking
char GulDukat(Grafo G, u32* Orden) {
    u32 idx = 0;
    u32 start = 0;
    u32 cant_elemts = 0;

    // primero, divisibles por 4 
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        if ((Color(i, G) % 4) == 0) {
            Orden[idx] = i;
            idx++;
            cant_elemts++;
        }
    }
    // Ordeno de mayor a menor
    qsort(Orden + start, cant_elemts, sizeof(u32), &cmpMayorMenor);

    // segundo, no divisibles por 4
    cant_elemts = 0;
    start = idx;
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        if (((Color(i, G) % 4) != 0) && ((Color(i, G) % 2) == 0)) {
            Orden[idx] = i;
            idx++;
            cant_elemts++;
        }
    }

    // Ordeno de mayor a menor
    qsort((Orden + start), cant_elemts, sizeof(u32), &cmpMayorMenor);

    // finalmente, los impares
    start = idx;
    cant_elemts = 0;
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        if ((Color(i, G) % 2) != 0) {
            Orden[idx] = i;
            idx++;
            cant_elemts++;
        }
    }

    // Ordeno de mayor a menor
    qsort((Orden + start), cant_elemts, sizeof(u32), &cmpMayorMenor);
    
    return '0';
}

int cmpFreq(const void* a, const void* b) {
    colorFreq freqA = *(colorFreq *) a;
    colorFreq freqB = *(colorFreq *) b;

    if (freqA->freq < freqB->freq) return -1;
    else if (freqA->freq > freqB->freq) return 1;
    else return 0;
}


char ElimGarak(Grafo G, u32 *Orden) {
    // u32 max_color = 1;
    u32 idx = 0;

    colorFreq *freq_colores = calloc(Delta(G)+1, sizeof(struct s_colorFreq));

    // incializo colorFreq
    for (color col=1; col <= Delta(G)+1; col++) {
        freq_colores[col] = calloc(1, sizeof(struct s_colorFreq));
        freq_colores[col]->color = col;
        freq_colores[col]->freq = 0;
        freq_colores[col]->vertices = calloc(NumeroDeVertices(G), sizeof(u32));
    }

    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        color col = Color(i, G);
        freq_colores[col]->freq++;
        freq_colores[col]->vertices[freq_colores[col]->freq-1] = i;
    }

    qsort(freq_colores+1, Delta(G)+1, sizeof(colorFreq), &cmpFreq);

    idx = 0;
    color color1 = 0, color2 = 0;
    
    // primero pongo los vertices que no sean de color 1 y 2, pero guardo la pos de color 1 y 2
    // para luego agregarlos al final y evitar tener que hacer otro for anidado (aunque la complejidad de ambos aproaches sean iguales)
    for (color col = 1; col <= Delta(G)+1; col++) {
        if (freq_colores[col]->color == 1) {
            color1 = col;
        }
        else if (freq_colores[col]->color == 2) {
            color2 = col;
        }
        else {
            for (u32 i = 0; i < freq_colores[col]->freq; i++) {
                Orden[idx] = freq_colores[col]->vertices[i];
                idx++;
            }
        }
    }

    // pongo los vertices del color 1 y 2 al final de Orden

    for (u32 i = 0; i < freq_colores[color2]->freq; i++) {
        Orden[idx] = freq_colores[color2]->vertices[i];
        idx++;
    }
    for (u32 i = 0; i < freq_colores[color1]->freq; i++) {
        Orden[idx] = freq_colores[color1]->vertices[i];
        idx++;
    }

    // destruyo freq_colores
    for (color col=1; col <= Delta(G)+1; col++) {
        free(freq_colores[col]->vertices);
        free(freq_colores[col]);
    }

    free(freq_colores);
    return '0';
}

// main para testear
int main(void) {
    Grafo G = ConstruirGrafo();

    u32 *Orden = calloc(NumeroDeVertices(G), sizeof(u32));
    for (u32 i=0; i < NumeroDeVertices(G); i++){
        Orden[i] = i;
    }

    color colores = Greedy(G, Orden);
    
    color *colores_extraidos = calloc(NumeroDeVertices(G), sizeof(color)); 
    ExtraerColor(G, colores_extraidos);
    printf("colores usados para colorear el grado: %u\n", colores);
    free(colores_extraidos);
    // printf("--------------COLORES-------------\n");
    // for (u32 i=0; i< NumeroDeVertices(G); i++) {
    //     printf("color de %u: %u\n", i, colores_extraidos[i]);
    // }
    GulDukat(G, Orden);
    printf("Terminado GulDukat\n");
    // printf("Vertices ordenados con GalDukat (vertice:color) \n");
    // for (u32 i=0; i < NumeroDeVertices(G); i++){
    //     printf(" %u:%u ", Orden[i], Color(Orden[i], G));
    // }
    // printf("\n");
    ElimGarak(G, Orden);
    printf("Terminado ElimGarak\n");
    // printf("Vertices ordenados con ElimGarak (vertice:color) \n");
    // for (u32 i=0; i < NumeroDeVertices(G); i++){
    //     printf(" %u:%u ", Orden[i], Color(Orden[i], G));
    // }
    // printf("\n");

    free(Orden);
    DestruirGrafo(G);
}


/* 
int main(void) {
    Grafo G = ConstruirGrafo();
    u32 idx;
    u32 *Orden = calloc(NumeroDeVertices(G), sizeof(u32));
    color *colores_extraidos = calloc(NumeroDeVertices(G), sizeof(color)); 
    color colores;


    // orden natural
    for (u32 i=0; i < NumeroDeVertices(G); i++){
        Orden[i] = i;
    }

    colores = Greedy(G, Orden);
    
    ExtraerColor(G, colores_extraidos);
    printf("colores usados para colorear el grado: %u\n", colores);
    printf("--------------COLORES-------------\n");
    for (u32 i=0; i< NumeroDeVertices(G); i++) {
        printf("color de %u: %u\n", i, colores_extraidos[i]);
    }

    GulDukat(G, Orden);

    printf("Vertices ordenados con GalDukat\n");
    for (u32 i=0; i < NumeroDeVertices(G); i++){
        printf(" %u ", Orden[i]);
    }
    printf("\n");

    // orden natural inverso
    idx = 0;
    for (int i=NumeroDeVertices(G)-1; i >= 0; i--){
        Orden[idx] = i;
        idx++;
    }

    colores = Greedy(G, Orden);
    
    ExtraerColor(G, colores_extraidos);
    printf("colores usados para colorear el grado: %u\n", colores);
    printf("--------------COLORES-------------\n");
    for (u32 i=0; i< NumeroDeVertices(G); i++) {
        printf("color de %u: %u\n", i, colores_extraidos[i]);
    }
    

    // primero pares decrecientes, 
    idx = 0;
    for (u32 i=0; i < NumeroDeVertices(G); i++){
        if ((i % 2) == 0) {
            Orden[idx] = i;
            idx++;
        }
    }

    // segundo impares crecientes
    // idx = 0;
    for (u32 i=0; i < NumeroDeVertices(G); i++){
        if ((i % 2) != 0) {
            Orden[idx] = i;
            idx++;
        }
    }

    colores = Greedy(G, Orden);
    ExtraerColor(G, colores_extraidos);
    printf("colores usados para colorear el grado: %u\n", colores);
    printf("--------------COLORES-------------\n");
    for (u32 i=0; i< NumeroDeVertices(G); i++) {
        printf("color de %u: %u\n", i, colores_extraidos[i]);
    }

    // orden decreciente segun grado


}

 */