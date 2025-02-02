#include <stdio.h>
// Ejercicio 5a)
// Traducción del programa 1.h
int main()
{
    int x;
    printf("Traducción del programa 1.h\n");
     // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    
    while (x != 0){
        x = x - 1;
    }

    printf("Valor final de x:%d\n\n", x);
    
    // Traducción del programa 1.i
    printf("Traducción del programa 1.i\n");
     // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    
    while (x != 0){
        x = 0;
    }

    printf("Valor final de x:%d\n", x);

    return 0;
}
