#include <stdio.h>
#include <stdbool.h>
#define N 5

typedef char clave_t;
typedef int valor_t;

struct asoc {
clave_t clave;
valor_t valor;
};
/*
bool asoc_existe(struct asoc a[], int tam, clave_t c)
{
    return 0;
    
}
*/
int main()
{
    struct asoc a[N];
    int i=0;
    clave_t clave;
    while (i < N){
        printf("Ingrese la clave y el valor respectivamente separados por un espacio: \n");
        scanf("%c%d", &a[i].clave, &a[i].valor);
        i = i + 1;
    }

    printf("Ingrese la clave:\n ");
    scanf("%c", &clave);



    return 0;
}