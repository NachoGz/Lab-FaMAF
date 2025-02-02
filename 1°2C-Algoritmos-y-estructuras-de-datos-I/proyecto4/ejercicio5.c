#include <stdio.h>
#include <assert.h>

void pedirArreglo(int a[], int n_max)
{   
    int i=0;
    while (i < n_max){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }


}

void imprimeArreglo(int a[], int n_max)
{
    int i=0;
    while (i < n_max){
        printf("El valor del arreglo en la posición %d es: %d \n", i, a[i]);
        i = i + 1;
    }


}


int main()
{
    
    int n_max;

    printf("Ingrese el tamaño del arreglo: \n");
    scanf("%d", &n_max);

    
    int a[n_max];

    pedirArreglo(a, n_max);
    
    imprimeArreglo(a, n_max);
    return 0;
}
