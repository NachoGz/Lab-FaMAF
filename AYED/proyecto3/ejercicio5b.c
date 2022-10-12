#include <stdio.h>
// Ejercicio 5b)

int main()
{
    int op;
    int x;
    int y;
    int i;
    bool res;
    printf("PROGRAMA 1")
    // Programa 1
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    printf("Ingrese un valor para y:\n");
    scanf("%d",&y);
    printf("Ingrese un valor para i:\n");
    scanf("%d",&i);
    while (x >= y){
        x = x - y;
        i = i + 1;
    }
    printf("PROGRAMA 2")
    // Programa 2
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    printf("Ingrese un valor para y:\n");
    scanf("%d",&y);
    printf("Ingrese un valor para i:\n");
    scanf("%d",&i);
    i = 2;
    res = true;
    while (res && i < x){
        res = res && ((x % i) != 0);
        i = i + 1;
    }
    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    printf("Valor final de i:%d\n", i);
    return 0;
}