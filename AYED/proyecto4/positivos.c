#include <stdio.h>
#include <stdbool.h>
#define N 5

bool existe_positivo(int a[], int tam)
{
    int i=0;
    while (i < tam){
        if (a[i]>=0){
            return true;
            
        }
        i = i + 1;
    }
    return false;

}

bool todos_positivos(int a[], int tam)
{
    int i=0;
    while (i < tam)
    {
        if (a[i] < 0){
            return false;
        }
        i = i + 1;
    }
    return true;

}

int main()
{
    int a[N];
    
    int i=0, op=1;
    while (i < N){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }

    printf("--------------------MENU DE OPCIONES--------------------\n");
    printf("1 - Verificar si TODOS los elementos del arreglo son positivos\n");
    printf("2 - Verificar si EXISTE algún elemento del arreglo que sea positivo\n");
    

    printf("Seleccione la opción deseada: \n");
    scanf("%d", &op);
  
    /* Opcion 1*/
    if (op == 1){
        if (todos_positivos(a, N) == true){
            printf("Verdadero\n");
        }
        else {
            printf("Los elementos del arreglo no son todos positivos\n");
        }
    }
    else {
        /* Opcion 2*/
        if (op == 2){
            if (existe_positivo(a, N) == true){
                printf("Verdadero\n");
            }
            else {
                printf("No hay números positivos en el arreglo\n");
            }
        }
        else {
            printf("Ingrese una opción correcta");
        }
    }
    

    return 0;
}