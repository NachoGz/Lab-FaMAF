#include <stdio.h>

void pedirArreglo(int a[], int n_max)
{   
    int i=0,x;
    while (i < n_max){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &x);
        a[i] = x;
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
    int n_max, a[n_max];

    printf("Ingrese el tamaño del arreglo: \n");
    scanf("%d", &n_max);
    
    pedirArreglo(a[n_max], n_max);
    
    return 0;
}