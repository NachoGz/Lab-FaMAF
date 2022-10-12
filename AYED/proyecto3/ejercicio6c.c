#include <stdio.h>

int pedirEntero(void)
{
    int x;
    printf("Ingrese un numero entero:\n");
    scanf("%d",&x);
    return x;
}

void imprimeEntero(int x)
{
    printf("El numero ingresado es: %d\n", x);
}

int main(void)
{   
    int x;
    int y;
    int z;
    int m;
    // Solicito valor para x
    x = pedirEntero();
    // # Solicito valor para y 
    y = pedirEntero();
    // # Solicito valor para z
    z = pedirEntero();
    // # Solicito valor para m
    m = pedirEntero();

    if (x < y) {
         m = x;
    }
    else {
        m = y;
    }
    if (m >= z) {
        m = z;
    }
    
    imprimeEntero(x);
    imprimeEntero(y);
    imprimeEntero(z);
    imprimeEntero(m);
}
