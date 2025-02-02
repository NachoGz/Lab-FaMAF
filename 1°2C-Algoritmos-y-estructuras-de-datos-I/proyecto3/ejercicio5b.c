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
    int temp;
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

/*
El programa 1 calcula la división entera entre x e y, donde el valor final de x es el resto de la división y el valor de i es el cociente.

El programa 2 calcula si un número es primo, es decir, verfica, para todo número desde 2 a x(no inclusive), si x divide algun número.
Si no divide a ninguno, entonces res es true, osea, x es primo. Si x no es primo, entonces res es false
*/
