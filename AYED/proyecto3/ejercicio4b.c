#include <stdio.h>
// Ejercicio 4b)
int main()
{
    int x;
    int y;
    int z;
    int m;

     // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    // # Solicito valor para y
    printf("Ingrese un valor para y:\n");
    scanf("%d",&y);
    // # Solicito valor para z
    printf("Ingrese un valor para z:\n");
    scanf("%d",&z);
    // # Solicito valor para m
    printf("Ingrese un valor para m:\n");
    scanf("%d",&m);
    
    if (x < y) {
         m = x;
    }
    else {
        m = y;
    }
    if (m >= z) {
        m = z;
    }

    printf("Valor final de x: %d\n", x);
    printf("Valor final de y: %d\n", y);
    printf("Valor final de z: %d\n", z);
    printf("Valor final de m: %d\n", m);

    return 0;
}

/*
Calcula el menor entre x,y,z
*/
