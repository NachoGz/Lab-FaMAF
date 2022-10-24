#include <stdio.h>
#include <assert.h>
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
        pos = pos + 1;
    }
    
}

int main()
{
    int a[N], i=0, pos1,pos2;
    
    while (i < N){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }
    i = 0;
    
    
    printf("El arreglo es: \n");
    while (i < N)
    {
        printf("%d ", a[i]);
        i = i + 1;
    }
    printf("\n");
    i = 0;
    
    /* Solicito valor de la primer posición*/
    printf("Ingrese la primer posición: \n");
    scanf("%d", &pos1);

    /* Solicito valor de la segunda posición*/
    printf("Ingrese la segunda posición: \n");
    scanf("%d", &pos2);
    
    /* Verifico que las posiciones esten en el rango*/
    assert(0<=pos1 && pos1<N);
    assert(0<=pos2 && pos2<N);
    
    intercambiar(a, N, pos1, pos2);
    
    printf("El arreglo despues de intercambiar valores es: \n");
    while (i < N){
        printf("%d ", a[i]);
        i = i + 1;
    }
    printf("\n");
    return 0;
}
