#include <stdio.h>
#include <stdbool.h>
#define N 5

typedef char clave_t;
typedef int valor_t;

struct asoc {
clave_t clave;
valor_t valor;
};

bool asoc_existe(struct asoc a[], int tam, clave_t c)
{
    int i=0;
    while (i<tam){
        printf("%c %c\n", a[i].clave, c);
        if (a[i].clave == c)
        {
            return true;
        }
        i = i + 1;
    
    }
    return false;
}

int main()
{
    struct asoc a[N];
    int i=0;
    clave_t clave;
    bool existe;
    while (i < N){
        printf("Ingrese la clave y el valor respectivamente separados por un espacio: \n");
        scanf(" %c %d", &a[i].clave, &a[i].valor);
        i = i + 1;
    }

    printf("Ingrese la clave: \n");
    scanf(" %c", &clave);

    existe = asoc_existe(a,N,clave);
    if (existe){
        printf("La clave ingresada está en el arreglo asociado.\n");
    }
    else{
        printf("La clave ingresada no está en el arreglo asociado.\n");
    }


    return 0;
}
