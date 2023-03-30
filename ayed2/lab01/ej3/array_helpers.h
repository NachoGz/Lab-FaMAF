#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

unsigned int array_from_file(int array[],
                             unsigned int max_size,
                             const char *filepath);

void array_dump(int a[], unsigned int length);

bool array_is_sorted(int a[], unsigned int length);
/*
    Dado un array a[] y su tamaño length debe devolver
    true si y sólo si los elementos del arreglo a[] 
    están ordenados de manera ascendente, es decir si:
    a[0] <= a[1] <= ... <= a[length - 1]

*/
