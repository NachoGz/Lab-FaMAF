#include <stdio.h>
#define N 5

struct comp_t {
int menores;
int iguales;
int mayores;
};

struct comp_t cuantos(int a[], int tam, int elem)
{
    int i=0;
    struct comp_t tripla;
    tripla.menores=0;
    tripla.mayores=0;
    tripla.iguales=0;
    while (i<tam){
        if (a[i] < elem){
            tripla.menores = tripla.menores + 1;
        }
        else {
            if (a[i] > elem){
                tripla.mayores = tripla.mayores + 1;
            }
            else {
                tripla.iguales = tripla.iguales + 1;
            }
        }
        i = i + 1;
    }
    return tripla;

}

int main()
{
    int a[N], i=0,elem;
    struct comp_t comp;
    while (i < N){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }

    printf("Ingrese el elemento a comparar:\n");
    scanf("%d", &elem);

    comp = cuantos(a, N, elem);
    printf("Los resultados son:\n");
    printf("Hay %d elementos menores a %d\n", comp.menores, elem);
    printf("Hay %d elementos mayores a %d\n", comp.mayores, elem);
    printf("Hay %d elementos iguales a %d\n", comp.iguales, elem);

    return 0;
}