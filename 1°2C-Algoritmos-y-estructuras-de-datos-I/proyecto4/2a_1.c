#include <stdio.h>

int main()
{
    int x,y,z;

    // Solicito valor de x
    printf("Ingrese el valor de x: \n");
    scanf("%d", &x);

    // Solicito valor de y
    printf("Ingrese el valor de y: \n");
    scanf("%d", &y);

    // Asignación
    z = x;
    x = x + 1;
    y = z + y;

    printf("Valor final de x: %d\n", x);
    printf("Valor final de y: %d\n", y);
    
    return 0;
}