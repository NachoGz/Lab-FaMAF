#include <stdio.h>
#define N 5

int sumatoria(int a[], int tam) 
{
    int i=0, ac=0;
    while (i < tam){
        ac = ac + a[i];
        i = i + 1;
    }
    return ac;
}

int main()
{   
    
    int i=0, a[N], sum;
    while (i < N){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }
    sum = sumatoria(a, N);
    printf("La sumatoria de los elementos del array es: %d\n", sum);
    return 0;
}