#include <stdio.h>
int main()
{
    int x;
    int y;
    // Solicito valor para x
    printf("Ingrese un valor inicial para x:\n");
    scanf("%d",&x);
    // Solicito valor para y
    printf("Ingrese un valor inicial para y:\n");
    scanf("%d",&y);
    y = y + y;
    x = x + y;

    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    return 0;
}