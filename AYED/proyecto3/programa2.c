#include <stdio.h>
#include <stdbool.h>
// Ejercicio 5b)

int main()
{   
    int cont;
    int x;
    int i;
    bool res;
    int temp;
    // Programa 2
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    printf("Ingrese un valor para i:\n");
    scanf("%d",&i);
    printf("Ingrese un valor para res (0 para False y 1 para True):\n");
    scanf("%d",&temp);
    res = true;
    i = 2;
    cont = 1;
    while (res && (i < x)){
        res = res && ((x % i) != 0);
        i = i + 1;
        if (cont <= 4){
            printf("Iteracion número %d: x=%d, i=%d, res=%d\n\n", cont,x,i,res);
            cont = cont + 1;
        }
     }
    printf("Valor final de x:%d\n", x);
    printf("Valor final de i:%d\n", i);
    printf("Valor final de res:%d\n", res);

    return 0;
}