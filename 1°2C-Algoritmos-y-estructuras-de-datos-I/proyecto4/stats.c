#include <stdio.h>
#include <limits.h>
#define N 5

struct datos_t {
float maximo;
float minimo;
float promedio;
};

struct datos_t stats(float a[], int tam)
{
    struct datos_t tripla;
    tripla.maximo=INT_MIN;
    tripla.minimo=INT_MAX;
    tripla.promedio=0;
    int i=0;
    while (i<tam){
        if (a[i]>tripla.maximo){
            tripla.maximo = a[i];
        }
        if (a[i]<tripla.minimo){
            tripla.minimo = a[i];
            }
        tripla.promedio = tripla.promedio + a[i];
        i = i + 1;
    }   
    tripla.promedio = tripla.promedio / tam;
    return tripla;
}   



int main()
{
    float a[N];
    int i=0;
    struct datos_t info;

    while (i < N){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%f", &a[i]);
        i = i + 1;
    }

    info = stats(a, N);
    printf("Los resultados son:\n");
    printf("El elemento más grande: %f\n", info.maximo);
    printf("El elemento más chico: %f\n", info.minimo);
    printf("El promedio: %f\n", info.promedio);
    return 0;
}