#include <stdio.h>
#include <limits.h>
#define N 5

int minimo_pares(int a[], int tam)
{
    int i=0, min=INT_MAX;
    while (i<tam){
        if ((a[i]%2) == 0 && a[i]<min){
            min = a[i];
        }
        i = i + 1;
    }
    return min;
}

int minimo_impares(int a[], int tam)
{
    int i=0, min=INT_MAX;
    while (i<tam){
        if ((a[i]%2) != 0 && a[i]<min){
            min = a[i];
        }
        i = i + 1;
    }
    return min;
}



int main()
{
    int a[N], i=0, min, min_par,min_impar;
    
    while (i < N){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }

    min_par = minimo_pares(a, N);
    min_impar = minimo_impares(a, N);

    if (min_par < min_impar){
        min = min_par;
    }
    else {
        min = min_impar;
    }

    printf("El elemento más chico del arreglo es: %d\n", min);
    return 0;
}