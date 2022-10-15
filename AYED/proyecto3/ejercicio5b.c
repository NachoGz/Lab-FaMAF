#include <stdio.h>
#include <stdbool.h>
// Ejercicio 5b)

int main()
{   
    int cont;
    int x;
    int y;
    int i;
    bool res;
    int temp;º
    printf("******PROGRAMA 1******\n");
    // Programa 1
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    printf("Ingrese un valor para y:\n");
    scanf("%d",&y);
    printf("Ingrese un valor para i:\n");
    scanf("%d",&i);
    cont = 1;
    while (x >= y){
        x = x - y;
        i = i + 1;
        if (cont <= 4){
            printf("Iteracion número %d: x=%d, y=%d, i=%d\n\n", cont,x,y,i);
            cont = cont + 1;
        }
        
    }
    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    printf("Valor final de i:%d\n\n", i);

    printf("******PROGRAMA 2******\n");
    // Programa 2
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    printf("Ingrese un valor para i:\n");
    scanf("%d",&i);
    printf("Ingrese un valor para res (0 para False y 1 para True):\n");
    scanf("%d",&temp);
    res = temp;
    cont = 1;
    while (res && i < x){
        res = res && ((x % i) != 0);
        i = i + 1;
        if (cont <= 4){
            printf("Iteracion número %d: x=%d, i=%d, res=%d\n\n", cont,x,i,res);
            cont = cont + 1;
        }
    }
    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    printf("Valor final de i:%d\n", i);
    return 0;
}
