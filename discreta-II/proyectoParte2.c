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

typedef struct s_gradosVertices {
    u32 vertice;
    u32 grado;
} * gradosVertices;

typedef struct s_coloresGrados {
    color color;
    u32 grado;
    u32 *vertices;
    u32 num_vertices;
} * coloresGrados;


u32 Greedy(Grafo G, u32* Orden) {
    color cant_colores = 1;
    color *colores_vecinos;
    u32 n = NumeroDeVertices(G);
    color *colores_asignados = calloc(n, sizeof(color));
    assert(colores_asignados != NULL);

    color curr_color;
    u32 cant_elemts = 0;

    // chequeo biyeccion
    for (u32 v = 0; v < n; v++) {
        cant_elemts++;
    }
    
    assert(cant_elemts == n);
    
    for (u32 i=0; i < n; i++) {
        u32 curr_vertice = Orden[i];
        if (Color(curr_vertice, G) == 0) {
            // obtengo los colores de los vecinos
            u32 grado = Grado(Orden[i], G);
            colores_vecinos = calloc(grado, sizeof(color));
            assert(colores_vecinos != NULL);
            
            for (u32 vecino = 0; vecino < grado; vecino++) {
                u32 vecino_vertice = Vecino(vecino, curr_vertice, G);
                color color_vecino = Color(vecino_vertice, G);
                colores_vecinos[vecino] = color_vecino;
            }

            curr_color = 1;
            bool color_libre = false;
            while (!color_libre) {
                bool color_usado = false;
                for (u32 vecino = 0; vecino < grado; vecino++) {
                    if (colores_vecinos[vecino] == curr_color) {
                        color_usado = true;
                        break;
                    }
                }
                if (!color_usado) {
                    color_libre = true;
                } 
                else {
                    curr_color++;
                    if (curr_color > cant_colores) {
                        cant_colores++;
                    }
                }
            }
            colores_asignados[curr_vertice] = curr_color;
            
            AsignarColor(curr_color, curr_vertice, G);
        
            free(colores_vecinos);
        }
        
    }
    free(colores_asignados);
    assert(cant_colores <= Delta(G)+1);
    return cant_colores;
}


int cmpMayorMenor(const void * a, const void *b) {
 
        u32 x = *(u32 *)a;
        u32 y = *(u32 *)b;
        
        if (x > y) return -1;
        else if (x < y) return 1;
        else return 0;
}

int cmpMayorMenorGrado(const void * a, const void *b) {
 
        coloresGrados gradoA = *(coloresGrados *) a;
        coloresGrados gradoB = *(coloresGrados *) b;

        if (gradoA->grado > gradoB->grado) return -1;
        else if (gradoA->grado < gradoB->grado) return 1;
        else return 0;
}


// faltaria el error checking
char GulDukat(Grafo G, u32* Orden) {
    u32 idx = 0;
    u32 start = 0;
    u32 cant_elemts = 0;
    u32 n = NumeroDeVertices(G);
    color *colores = calloc(n, sizeof(color));
    assert(colores != NULL);

    u32 cant_colores = 0;
    

    ExtraerColor(G, colores);
    // me fijo cuantos colores se usaron
    for (u32 v = 0; v < n; v++) {
        cant_colores =  colores[v] > cant_colores ? colores[v] : cant_colores;
    }

    coloresGrados *colores_grados = calloc(cant_colores+1, sizeof(struct s_coloresGrados));
    assert(colores_grados != NULL);

    coloresGrados *colores_ordenados = calloc(cant_colores+1, sizeof(struct s_coloresGrados));
    assert(colores_ordenados != NULL);
    
    u32 *max_grados = calloc(cant_colores+1, sizeof(u32));
    assert(max_grados != NULL);
    
    u32 *min_grados = calloc(cant_colores+1, sizeof(u32));
    assert(min_grados != NULL);

    // inicializo grados y ordenados
    for (u32 i=0; i <= cant_colores; i++) {
        colores_grados[i] = calloc(1, sizeof(struct s_coloresGrados));
        assert(colores_grados[i] != NULL);
        colores_grados[i]->vertices = calloc(n, sizeof(u32));
        assert(colores_grados[i]->vertices != NULL);
        colores_grados[i]->num_vertices = 0;
    }

    // calculo M(x) para cada color
    for (u32 v = 0; v < n; v++) {
        color col = Color(v, G);
        u32 grado = Grado(v, G);
        max_grados[col] = grado > max_grados[col] ? grado : max_grados[col];
    }

    // calculo m(x) para cada color
    for (u32 v = 0; v < n; v++) {
        color col = Color(v, G);
        min_grados[col] = Delta(G);
    }

    for (u32 v = 0; v < n; v++) {
        color col = Color(v, G);
        u32 grado = Grado(v, G);
        min_grados[col] = grado > min_grados[col] ? grado : min_grados[col];
    }

    for (u32 v = 0; v < n; v++) {
        color col = Color(v, G);
        if (colores_grados[col]->color == 0) {
            colores_grados[col]->color = col;
            if ((col % 4) == 0 ) {
                colores_grados[col]->grado = max_grados[col];
            }
            else if (((col % 2) == 0) && ((col % 4) != 0)) {
                colores_grados[col]->grado = max_grados[col] + min_grados[col];
            }
            else {
                colores_grados[col]->grado = min_grados[col];
            }
            cant_elemts++;
            idx++;
        }
        colores_grados[col]->vertices[colores_grados[col]->num_vertices] = v;
        colores_grados[col]->num_vertices++;
    }
    
    idx = 0;
    cant_elemts = 0;
    // ordeno colores por bloques
    // primero, colores divisibles por 4
    for (color col = 1; col <= cant_colores; col++) {
        if ((col % 4) == 0 ) {
            colores_ordenados[idx] = colores_grados[col];
            cant_elemts++;
            idx++;
        }
    }

    // Ordeno de acuerdo M(x)
    qsort(colores_ordenados + start, cant_elemts, sizeof(coloresGrados), &cmpMayorMenorGrado);

    // segundo, pares no divisibles por 4
    cant_elemts = 0;
    start = idx;

    for (color col = 1; col <= cant_colores; col++) {
        if (((col % 2) == 0) && ((col % 4) != 0)) {
            colores_ordenados[idx] = colores_grados[col];
            idx++;
            cant_elemts++;
        }
    }

    // Ordeno de mayor a menor    
    qsort(colores_ordenados + start, cant_elemts, sizeof(coloresGrados), &cmpMayorMenorGrado);


    // finalmente, los impares
    start = idx;
    cant_elemts = 0;

    for (color col = 1; col <= cant_colores; col++) {
        if ((col % 2) != 0) {
            colores_ordenados[idx] = colores_grados[col];
            idx++;
            cant_elemts++;
        }
    }
    u32 end = idx;
    // Ordeno de mayor a menor
    qsort(colores_ordenados + start, cant_elemts, sizeof(coloresGrados), &cmpMayorMenorGrado);
    
    idx = 0;
    // una vez que grados esta ordenado, populo orden con los vertices de grado
    for (u32 i=0; i < end; i++) {
        for (u32 v=0; v < colores_ordenados[i]->num_vertices; v++) {
            Orden[idx] = colores_ordenados[i]->vertices[v];
            idx++;
        }
    }
    free(colores);
    // libero grados y ordenados
    for (u32 i=0; i <= cant_colores; i++) {
        free(colores_grados[i]->vertices);
        free(colores_grados[i]);
    }
    free(colores_grados);
    free(colores_ordenados);
    free(max_grados);
    free(min_grados);
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
    u32 idx = 0;
    u32 n = NumeroDeVertices(G);

    colorFreq *freq_colores = calloc(Delta(G)+1, sizeof(struct s_colorFreq));
    assert(freq_colores != NULL);

    // incializo colorFreq
    for (color col=0; col <= Delta(G)+1; col++) {
        freq_colores[col] = calloc(1, sizeof(struct s_colorFreq));
        assert(freq_colores[col] != NULL);
        freq_colores[col]->color = col;
        freq_colores[col]->freq = 0;
        freq_colores[col]->vertices = calloc(n, sizeof(u32));
        assert(freq_colores[col]->vertices != NULL);
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
    for (color col = 0; col <= Delta(G)+1; col++) {
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
    for (color col=0; col <= Delta(G)+1; col++) {
        free(freq_colores[col]->vertices);
        free(freq_colores[col]);
    }

    free(freq_colores);
    return '0';
}
/* 
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

    printf("--------------COLORES-------------\n");
    for (u32 i=0; i< NumeroDeVertices(G); i++) {
        printf("color de %u: %u\n", i, colores_extraidos[i]);
    }
    printf("colores usados para colorear el grado: %u\n", colores);

    free(colores_extraidos);
    GulDukat(G, Orden);
    printf("Terminado GulDukat\n");
    printf("Vertices ordenados con GalDukat (vertice:color) \n");
    for (u32 i=0; i < NumeroDeVertices(G); i++){
        printf(" %u:%u ", Orden[i], Color(Orden[i], G));
    }
    printf("\n");
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

*/ 

int cmpMayoraMenorGrado(const void * a, const void *b) {
 
        gradosVertices gradoA = *(gradosVertices *) a;
        gradosVertices gradoB = *(gradosVertices *) b;

        if (gradoA->grado > gradoB->grado) return -1;
        else if (gradoA->grado < gradoB->grado) return 1;
        else return 0;
}

int cmpMenoraMayorGrado(const void * a, const void *b) {
 
        gradosVertices gradoA = *(gradosVertices *) a;
        gradosVertices gradoB = *(gradosVertices *) b;

        if (gradoA->grado > gradoB->grado) return -1;
        else if (gradoA->grado < gradoB->grado) return 1;
        else return 0;
}

void decolorearGrafo(Grafo G) {
    for (u32 v = 0; v < NumeroDeVertices(G); v++) {
        AsignarColor(0, v, G);
    }
}


int main(void) {
    Grafo G = ConstruirGrafo();
    u32 idx;
    u32 n = NumeroDeVertices(G);
    u32 *Orden = calloc(n, sizeof(u32));
    assert(Orden != NULL);

    color *coloreo_minimo = calloc(n, sizeof(color)); 
    assert(coloreo_minimo != NULL);
    color min_colores = Delta(G);
    
    color colores;
    

    // orden natural
    for (u32 i=0; i < n; i++) {
            Orden[i] = i;
    }

    colores = Greedy(G, Orden);
    printf("colores usados para colorear con orden natural: %u\n", colores);

    if (colores < min_colores) {
        min_colores = colores;
        ExtraerColor(G, coloreo_minimo);
    }
    // printf("colores usados para colorear el grafo: %u\n", colores);

    for (u32 i = 0; i < 50; i++) {
        // colores = Greedy(G, Orden);

        // printf("Orden natural:\n");
        // printf("GulDukat\n");
        GulDukat(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);
        
        colores = Greedy(G, Orden);
        printf("colores usados con GulDukat: %u\n", colores);
        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }        

        ElimGarak(G, Orden);
        
        // decoloreo el grafo
        decolorearGrafo(G);
        
        colores = Greedy(G, Orden);
        printf("colores usados con ElimGarak: %u\n", colores);
        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }
        // printf("colores usados para colorear el grafo: %u\n", colores);

    }
    
    // orden natural inverso
    idx = 0;
    for (int i=n-1; i >= 0; i--){
        Orden[idx] = i;
        idx++;
    }
    colores = Greedy(G, Orden);
    printf("colores usados para colorear con orden natural inverso: %u\n", colores);

    if (colores < min_colores) {
        min_colores = colores;
        ExtraerColor(G, coloreo_minimo);
    }        
    // printf("colores usados para colorear el grafo: %u\n", colores);

    for (u32 i = 0; i < 50; i++) {

        // printf("Orden natural inverso\n");
        // printf("GulDukat\n");
        GulDukat(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);
        colores = Greedy(G, Orden);
        printf("colores usados con GulDukat: %u\n", colores);

        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }
        
        // printf("colores usados para colorear el grafo: %u\n", colores);

        // printf("ElimGarak\n");
        ElimGarak(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);
        colores = Greedy(G, Orden);
        printf("colores usados con ElimGarak: %u\n", colores);

        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }
    }
    
    // primero pares decrecientes, luego impares crecientes
    // primero pares decrecientes
    idx = 0;
    for (u32 i=0; i < n; i++){
        if ((i % 2) == 0) {
            Orden[idx] = i;
            idx++;
        }
    }

    // segundo impares crecientes
    // idx = 0;
    for (u32 i=0; i < n; i++){
        if ((i % 2) != 0) {
            Orden[idx] = i;
            idx++;
        }
    }

    colores = Greedy(G, Orden);
        printf("colores usados con primero pares decrecientes, luego impares crecientes: %u\n", colores);

    if (colores < min_colores) {
        min_colores = colores;
        ExtraerColor(G, coloreo_minimo);
    }
    // printf("colores usados para colorear el grafo: %u\n", colores);

    for (u32 i = 0; i < 50; i++) {
        // printf("primero pares decrecientes, luego impares crecientes\n");
        // printf("GulDukat\n");
        GulDukat(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);

        colores = Greedy(G, Orden);
        printf("colores usados con GulDukat: %u\n", colores);
        
        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }

        ElimGarak(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);

        colores = Greedy(G, Orden);
        printf("colores usados con ElimGarak: %u\n", colores);
        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }
    }
    
   
    
    // orden decreciente segun grado
    gradosVertices *gradosDecrecientes = calloc(n, sizeof(struct s_gradosVertices));
    assert(gradosDecrecientes != NULL);

    // incializo gradosDecrecientes
    for (u32 i = 0; i < n; i++) {
        gradosDecrecientes[i] = calloc(1, sizeof(struct s_gradosVertices));
        assert(gradosDecrecientes != NULL);
        gradosDecrecientes[i]->vertice = i;
        gradosDecrecientes[i]->grado = Grado(i, G);
    }
    
    qsort(gradosDecrecientes, n, sizeof(struct s_gradosVertices), &cmpMayoraMenorGrado);

    for (u32 i = 0; i < n; i++) {
        Orden[i] = gradosDecrecientes[i]->vertice;
    }
    
    for (u32 v = 0; v < n; v++) {
        free(gradosDecrecientes[v]);
    }
    
    free(gradosDecrecientes);

    colores = Greedy(G, Orden);
    printf("colores usados con orden decreciente segun grado: %u\n", colores);
    
    for (u32 i = 0; i < 50; i++) {
        GulDukat(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);

        colores = Greedy(G, Orden);
        printf("colores usados con GulDukat: %u\n", colores);

        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }

        ElimGarak(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);

        colores = Greedy(G, Orden);
        printf("colores usados con ElimGarak: %u\n", colores);

        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }
    }

    // orden creciente segun grado
    gradosVertices *gradosCrecientes = calloc(n, sizeof(struct s_gradosVertices));
    assert(gradosCrecientes != NULL);

    // incializo gradosCrecientes
    for (u32 i = 0; i < n; i++) {
        gradosCrecientes[i] = calloc(1, sizeof(struct s_gradosVertices));
        assert(gradosCrecientes != NULL);
        gradosCrecientes[i]->vertice = i;
        gradosCrecientes[i]->grado = Grado(i, G);
    }
    
    qsort(gradosCrecientes, n, sizeof(struct s_gradosVertices), &cmpMenoraMayorGrado);

    for (u32 i = 0; i < n; i++) {
        Orden[i] = gradosCrecientes[i]->vertice;
    }
    
    for (u32 v = 0; v < n; v++) {
        free(gradosCrecientes[v]);
    }
    
    free(gradosCrecientes);

    colores = Greedy(G, Orden);
    printf("colores usados con orden creciente segun grado: %u\n", colores);
    
    for (u32 i = 0; i < 50; i++) {
        GulDukat(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);

        colores = Greedy(G, Orden);
        printf("colores usados con GulDukat: %u\n", colores);
        
        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }

        ElimGarak(G, Orden);

        // decoloreo el grafo
        decolorearGrafo(G);

        colores = Greedy(G, Orden);
        printf("colores usados con ElimGarak: %u\n", colores);

        if (colores < min_colores) {
            min_colores = colores;
            ExtraerColor(G, coloreo_minimo);
        }
    }
    
    // recoloreo el Grafo con el coloreo con menores colores
    ImportarColores(coloreo_minimo, G);
    for (u32 i = 0; i < 500; i++) {
        if ((rand() % 2) == 0) {
            ElimGarak(G, Orden);
        }
        else {
            GulDukat(G, Orden);
        }
        decolorearGrafo(G);
        colores = Greedy(G, Orden);
        printf("colores usados: %u\n", colores);
    }

    printf("Listo!\n");
    free(coloreo_minimo);
    free(Orden);
    DestruirGrafo(G);
}   
  
