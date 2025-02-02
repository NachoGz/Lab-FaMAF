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
    int num;
    num = pedirEntero();
    imprimeEntero(num);
    return 0;
}
