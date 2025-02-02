#include <stdio.h>

int pedirEntero(void)
{
    int x;
    printf("Ingrese un numero entero:\n");
    scanf("%d",&x);
    return x;
}

void imprimeEntero(int x)
{
    printf("El numero ingresado es: %d\n", x);
}
// Ejercicio 4b
int main(void)
{   
    int x,y,z,m;

    // Solicito valor para x
    x = pedirEntero();
    // # Solicito valor para y 
    y = pedirEntero();
    // # Solicito valor para z
    z = pedirEntero();
    // # Solicito valor para m
    m = pedirEntero();

    if (x < y) {
         m = x;
    }
    else {
        m = y;
    }
    if (m >= z) {
        m = z;
    }
    
    imprimeEntero(x);
    imprimeEntero(y);
    imprimeEntero(z);
    imprimeEntero(m);
}
/*
Ventaja: el código es más compacto y más corto.

Podría escribir otra función para los condicionales, una función que toma 4 enteros y no devuelve nada.
*/

// Ejercicio 1

void ejercicio1(void)
{
    int x, y, z;

    // Solicito valor para x
    x = pedirEntero();
    // # Solicito valor para y 
    y = pedirEntero();
    // # Solicito valor para z
    z = pedirEntero();

    printf("Usted ingresó x=%d, y=%d, z=%d\n\n",x,y,z); 
    
    printf("Operación 1: x + y + 1 = %d\n",x + y + 1);

    printf("Operación 2: z * z + y * 45 - 15 * x = %d\n",(z * z) + (y * 45) - (15 * x));

    printf("Operación 3: y - 2 == (x * 3 + 1) mod 5 = %d\n",(y - 2) == (x * 3 + 1) % 5);

    printf("Operación 4: y / 2 * x = %d\n",y / 2 * x);

    printf("Operación 5: y < x * z = %d\n",y < x * z);

}
/*
void ejercicio2(void)
{

}
*/

void ejercicio3a(void)
{
    int x;
    // Solicito valor para x
    x = pedirEntero();
    x = 5;
    printf("Valor final de x: %d\n", x);

}

void ejercicio3b(void)
{
    int x;
    int y;
    // Solicito valor para x
    x = pedirEntero();
    // Solicito valor para y
    y = pedirEntero();
    x = x + y;
    y = y + y;

    printf("Valor final de x: %d\n", x);
    printf("Valor final de y: %d\n", y);
}

void ejercicio3c(void)
{
    int x;
    int y;
    // Solicito valor para x
    x = pedirEntero();
    // Solicito valor para y
    y = pedirEntero();
    y = y + y;
    x = x + y;

    printf("Valor final de x: %d\n", x);
    printf("Valor final de y: %d\n", y);
}

void ejercicio4a(void)
{
    int x;
    int y;
    printf("Traducción del programa 1.e\n");
    // Solicito valor para x
    x = pedirEntero();
    // Solicito valor para y
    y = pedirEntero();
    
    if (x >= y){
        x = 0;
    }
    else {
        x = 2;
    }
    printf("Valor final de x: %d\n", x);
    printf("Valor final de y: %d\n\n", y);
    
// Traducción del programa 1.f
    printf("Traducción del programa 1.f\n");
    // Solicito valor para x
    x = pedirEntero();
    // Solicito valor para y
    y = pedirEntero();
    
    if (x >= y){
        x = 0;
    }
    else {
        x = 2;
    }
    printf("Valor final de x: %d\n", x);
    printf("Valor final de y: %d\n", y);

}

void ejercicio5a(void)
{
    int x;
    printf("Traducción del programa 1.h\n");
    // Solicito valor para x
    x = pedirEntero();
    
    while (x != 0){
        x = x - 1;
    }

    printf("Valor final de x:%d\n\n", x);
    
    // Traducción del programa 1.i
    printf("Traducción del programa 1.i\n");
     // Solicito valor para x
    x = pedirEntero();
    
    while (x != 0){
        x = 0;
    }

    printf("Valor final de x:%d\n", x);
}

void ejercicio5b(void)
{   
    int cont,x,y,i,res,temp;
    printf("******PROGRAMA 1******\n");
    // Programa 1
    x = pedirEntero();
    y = pedirEntero();
    i = pedirEntero();
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
    x = pedirEntero();
    i = pedirEntero();
    temp = pedirEntero();
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

}
