#include <stdio.h>
#define N 5

void intercambiar(int a[], int tam, int i, int j)
{   
    int pos=0, aux=0;
    while (pos<tam){
        if (pos==i){
            aux = a[i];
            a[i] = a[j];
            a[j] = aux;
        }
    }
    
}

int main()
{
    int a[N]={1,2,3,4,5}, i=0;
    /*
    while (i < N){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }
    i = 0;
    */
    
    printf("El arreglo es: \n");
    while (i < N)
    {
        printf("%d ", a[i]);
        i = i + 1;
    }
    
    printf("test");
    intercambiar(a, N, 2, 3);
    printf("El arreglo despues de intercambiar valores es: \n");
    while (i < N){
        printf("El arreglo es: \n");
        printf("%d ", a[i]);
        i = i + 1;
    }
    return 0;
}