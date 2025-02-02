#include <stdio.h>


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
    
    int i=0, sum, tam;
    
    printf("Ingrese el tamaño del arreglo: \n");
    scanf("%d", &tam);

    int a[tam];

    
    while (i < tam){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }
    sum = sumatoria(a, tam);
    printf("La sumatoria de los elementos del array es: %d\n", sum);
    return 0;
}