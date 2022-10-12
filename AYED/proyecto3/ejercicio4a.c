#include <stdio.h>
// Traducción del programa 1.e
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
    
    if (x >= y){
        x = 0;
    }
    else {
        x = 2;
    }
    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    
// Traducción del programa 1.f
    int x;
    int y;
    
    // Solicito valor para x
    printf("Ingrese un valor inicial para x:\n");
    scanf("%d",&x);
    // Solicito valor para y
    printf("Ingrese un valor inicial para y:\n");
    scanf("%d",&y);
    
    if (x >= y){
        x = 0;
    }
    else {
        x = 2;
    }
    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    return 0;
}