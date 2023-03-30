#include "array_helpers.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "mybool.h"

unsigned int array_from_file(int array[],
                             unsigned int max_size,
                             const char *filepath) {
    //your code here!!!
    FILE* file;
    unsigned int length;
    //abro el archivo
    file = fopen(filepath, "r");

    // itero cada caracter
    for (unsigned int i=0;i<max_size;i++)
    {   
        // indicador EOF
        if (feof(file))
        {
            break;
        }
        // busco el largo del array
        if (i==0)
        {
            fscanf(file, "%u", &length);
        }
        else
        {
            fscanf(file, "%d", &array[i-1]);
        }
    }
    fclose(file);
    return length;
}

void array_dump(int a[], unsigned int length) {
    //your code here!!!
    printf("\n[");
    for (unsigned int i=0;i<length;i++)
    {   
        if (i==(length-1))
        {
            printf("%d", a[i]);
        }
        else
        {
            printf("%d, ", a[i]);       }
    }
    printf("]\n");
}

mybool array_is_sorted(int a[], unsigned int length)
{
    mybool sorted = true;
    for (unsigned int i=0; i<length-1; i++)
    {
        sorted = sorted && (a[i] <= a[i+1]);
    }
    return sorted;
}

void array_swap(int a[], unsigned int i, unsigned int j)
{
  int aux = a[i];
  a[i] = a[j];
  a[j] = aux;
}
