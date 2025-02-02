#include <stdio.h>

int main()
{
    int x,y,z,k,j;

    // Solicito valor de x
    printf("Ingrese el valor de x: \n");
    scanf("%d", &x);

    // Solicito valor de y
    printf("Ingrese el valor de y: \n");
    scanf("%d", &y);

    // Solicito valor de z
    printf("Ingrese el valor de z: \n");
    scanf("%d", &z);

    // Asignación
    k = x;
    x = y;
    j = y;
    y = y + k + z;
    z = k + j;

    printf("Valor final de x: %d\n", x);
    printf("Valor final de y: %d\n", y);
    printf("Valor final de z: %d\n", z);
    
    return 0;
}