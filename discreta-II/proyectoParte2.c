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
} * colorFreq;


// orden ascendente = 0, orden descendente = 1 
void merge(u32 *arr, u32 l, u32 m, u32 r)
{
    u32 i, j, k;
    u32 n1 = m - l + 1;
    u32 n2 = r - m;

    // Create temp arrays
    u32 L[n1], R[n2];

    for (i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (j = 0; j < n2; j++)
        R[j] = arr[m + 1 + j];

    i = 0;
    j = 0;
    k = l;
    while (i < n1 && j < n2) {
        if (L[i] < R[j]) {
            arr[k] = L[i];
            i++;
        }
        else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }


    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

void mergeSort(u32 *arr, u32 l, u32 r)
{
    if (l < r) {
        u32 m = l + (r - l) / 2;

        mergeSort(arr, l, m);
        mergeSort(arr, m + 1, r);

        merge(arr, l, m, r);
    }
}

void mergeGarak(colorFreq *arr, u32 l, u32 m, u32 r)
{
    u32 i, j, k;
    u32 n1 = m - l + 1;
    u32 n2 = r - m;

    // Create temp arrays
    colorFreq L[n1], R[n2];

    for (i = 0; i < n1; i++)
        L[i] = arr[l + i];
    for (j = 0; j < n2; j++)
        R[j] = arr[m + 1 + j];

    i = 0;
    j = 0;
    k = l;
    while (i < n1 && j < n2) {
        if (L[i]->freq < R[j]->freq) {
            arr[k] = L[i];
            i++;
        }
        else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }


    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

void mergeSortGarak(colorFreq *arr, u32 l, u32 r)
{
    if (l < r) {
        u32 m = l + (r - l) / 2;

        mergeSortGarak(arr, l, m);
        mergeSortGarak(arr, m + 1, r);

        mergeGarak(arr, l, m, r);
    }
}

u32 Greedy(Grafo G, u32* Orden) {
    color cant_colores = 1;
    color col;
    color *colores_vecinos;
            bool used;

    // u32 delta = Delta(G);
    for (u32 i=0; i < NumeroDeVertices(G)-1; i++) {
        if (Orden[i] > Orden[i+1]) {
            return (pow(2,32) - 1);
        }
    }
    
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
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
            cant_colores++;
        }
        
        AsignarColor(col, Orden[i], G);
        /*
        if (col > cant_colores) {
            cant_colores = col;
        }
        */
        free(colores_vecinos);
    }

    return cant_colores;
}

// faltaria el error checking
char GalDukat(Grafo G, u32* Orden) {
    u32 idx = 0;
    u32 start = 0;
    // primero, divisibles por 4 
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        if ((Color(i, G) % 4) == 0) {
            Orden[idx] = i;
            idx++;
        }
    }
    // Ordeno de mayor a menor
    mergeSort(Orden, start, idx-1);

    // segundo, no divisibles por 4
    start = idx;
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        if (((Color(i, G) % 4) != 0) && ((Color(i, G) % 2) == 0)) {
            Orden[idx] = i;
            idx++;
        }
    }

    // Ordeno de mayor a menor
    mergeSort(Orden, start, idx-1);

    // finalmente, los impares
    start = idx;
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        if ((Color(i, G) % 2) != 0) {
            Orden[idx] = i;
            idx++;
        }
    }

    // Ordeno de mayor a menor
    mergeSort(Orden, start, idx-1);
    
    return '0';
}


char ElimGarak(Grafo G, u32 *Orden) {
    colorFreq *freq_colores = calloc(NumeroDeVertices(G), sizeof(struct s_colorFreq));
    u32 max_color = 1;
    
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        u32 color = Color(i, G);
        freq_colores[color] = calloc(1, sizeof(struct s_colorFreq));
        freq_colores[color]->color = color;
        freq_colores[color]->freq = 0;
        if (color > max_color) {
            max_color = color;
        }
    }
    for (u32 i=0; i < NumeroDeVertices(G); i++) {
        u32 color = Color(i, G);
        freq_colores[color]->freq++;
    }

    mergeSortGarak(freq_colores, 1, max_color);

    printf("Color: cantidad de vertices\n");
    for (u32 i=1;i <= max_color; i++) {
        printf("%u:%u\n", freq_colores[i]->color, freq_colores[i]->freq);
    }
    printf("\n");

    free(freq_colores);
    return '0';    
}

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
    // printf("--------------COLORES-------------\n");
    // for (u32 i=0; i< NumeroDeVertices(G); i++) {
    //     printf("color de %u: %u\n", i, colores_extraidos[i]);
    // }
    GalDukat(G, Orden);

    // printf("Vertices ordenados con GalDukat\n");
    // for (u32 i=0; i < NumeroDeVertices(G); i++){
    //     printf(" %u ", Orden[i]);
    // }
    printf("\n");
    ElimGarak(G, Orden);
    free(Orden);
}

