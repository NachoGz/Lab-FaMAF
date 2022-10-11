#include <stdio.h>
#include <stdbool.h>

int ej1()
{
    int x, y, z;
    // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    // # Solicito valor para y
    printf("Ingrese un valor para y:\n");
    scanf("%d",&y);
    // # Solicito valor para z
    printf("Ingrese un valor para z:\n");
    scanf("%d",&z);

    printf("Usted ingresó x=%d, y=%d, z=%d\n",x,y,z); 
    
    printf("Operación 1: x + y + 1 = %d\n",x + y + 1);

    printf("Operación 2: z * z + y * 45 - 15 * x = %d\n",(z * z) + (y * 45) - (15 * x));

    printf("Operación 3: y - 2 == (x * 3 + 1) mod 5 = %d\n",(y - 2) == (x * 3 + 1) % 5);

    printf("Operación 4: y / 2 * x = %d\n",y / 2 * x);

    printf("Operación 5: y < x * z = %d\n",y < x * z);

    return 0;
}

int ej2()
{
    int x, y, z, b, w;
    // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    // # Solicito valor para y
    printf("Ingrese un valor para y:\n");
    scanf("%d",&y);
    // # Solicito valor para z
    printf("Ingrese un valor para z:\n");
    scanf("%d",&z);
    // Solicito valor para b
    printf("Ingrese un valor para b:\n");
    scanf("%d",&b);
    // # Solicito valor para w
    printf("Ingrese un valor para w:\n");
    scanf("%d",&w);

    printf("Usted ingresó x=%d, y=%d, z=%d, b=%d, w=%d\n",x,y,z,b,w); 

    printf("Operación 1: x mod 4 == 0 = %d\n", x % 4 == 0);

    printf("Operación 2: x + y == 0 && y - x == (-1) * z = %d\n", x + y == 0 && y - x == (-1) * z );

    printf("Operación 3: not b && w = %d", ! b && w);

    return 0;
}

int ej3a()
{
    int x;
    // Solicito valor para x
    printf("Ingrese un valor inicial para x:\n");
    scanf("%d",&x);
    x = 5;
    printf("Valor final de x:%d\n", x);
    return 0;

}

int ej3b()
{
    int x;
    int y;
    // Solicito valor para x
    printf("Ingrese un valor inicial para x:\n");
    scanf("%d",&x);
    // Solicito valor para y
    printf("Ingrese un valor inicial para y:\n");
    scanf("%d",&y);
    x = x + y;
    y = y + y;

    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    return 0;
}

int ej3c()
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
// Ejercicio 4a)
// Traducción del programa 1.e
int ej4e()
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
    return 0;
}
// Traducción del programa 1.f
int ej4f()
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
    return 0;
}
// Ejercicio 4b)
int ej4()
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

    printf("Valor final de x:%d\n", x);
    printf("Valor final de y:%d\n", y);
    printf("Valor final de z:%d\n", z);
    printf("Valor final de m:%d\n", m);

    return 0;
}
// Ejercicio 5a)
// Traducción del programa 1.h
int ej5h()
{
    int x;
     // Solicito valor para x
    printf("Ingrese un valor para x:\n");
    scanf("%d",&x);
    
    while (x != 0){
        x = x - 1;
    }

    printf("Valor final de x:%d\n", x);
    
    return 0;
}
// Traducción del programa 1.i
int ej5i()
{
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
// Ejercicio 5b)

int ej5b()
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


int main()
{   
    char x;
    x = '0';
    while (x != 'q') {
        printf("Ingrese el ejercicio que quiere ejecutar:\n1. Ejercicio 1\n2. Ejercicio 2\n3. Ejercicio 3\n4. Ejercicio 4\n5. Ejercicio 5\n6. Ejercicio 6\nPresione q para salir.\n");
        scanf("%c", &x);
        if (x == '1'){
            ej1();
        }
        else {
            if (x == '2'){
                ej2();
            }
            else {
                if ( x == '3'){
                    printf("Ejercicio 1.a\n");
                    ej3a();
                    printf("Ejercicio 1.b\n");
                    ej3b();
                    printf("Ejercicio 1.c\n");
                    ej3c();
                }
                else {
                    if (x == '4'){
                        printf("4.a\n");
                        printf("Ejercicio 1.e\n");
                        ej4e();
                        printf("Ejercicio 1.f\n");
                        ej4f();
                        printf("4.b\n");
                        ej4();
                    }
                    else {
                        if (x == '5'){
                            printf("5.a\n");
                            printf("Ejercicio 1.h\n");
                            ej5h();
                            printf("Ejercicio 1.i\n");
                            ej5i();
                            printf("4.b\n");
                            ej5b();
                        }
                    }
                }
            }
            }
        }
        
    return 0;
}