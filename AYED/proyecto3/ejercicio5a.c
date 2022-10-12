#include <stdio.h>
// Ejercicio 5a)
// Traducción del programa 1.h
int main()
{
    int x;
     // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    
    while (x != 0){
        x = x - 1;
    }

    printf("Valor final de x:%d\n", x);
    
    // Traducción del programa 1.i
    int x;
     // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    
    while (x != 0){
        x = 0;
    }

    printf("Valor final de x:%d\n", x);

    return 0;
}