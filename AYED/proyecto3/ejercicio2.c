#include <stdio.h>
#include <stdbool.h>
int main()
{
    int x, y, z;
    bool b,w;
    // Expresión 1
    printf("Expresión 1: x mod 4 == 0\n");
    x = 1;
    while (((x % 4) == 0) == false){
        // Solicito valor para x
        printf("Ingrese un valor para x para que la expresión 1 sea True:\n");
        scanf("%d",&x);
        // printf("Expresión 1: x mod 4 == 0 = %d\n", x % 4 == 0);
        if (((x % 4) == 0) == false) {
            printf("El valor x=%d no es correcto!\n", x);
        }
        else {
            printf("Correcto! El valor de x para que la expresión 1 sea True es:%d\n\n", x);
        }
    }
    
    // Expresión 2
    printf("Expresión 2: x + y == 0 && y - x == (-1) * z\n");
    x = 1;
    y = 1;
    z = 1;
    while ((((x + y == 0) && (y - x)) == (-1) * z) == false){
        // Solicito valor para x
        printf("Ingrese un valor para x para que la expresión 2 sea True:\n");
        scanf("%d",&x);
        // # Solicito valor para y
        printf("Ingrese un valor para y para que la expresión 2 sea True:\n");
        scanf("%d",&y);
        // # Solicito valor para z
        printf("Ingrese un valor para z para que la expresión 2 sea True:\n");
        scanf("%d",&z);
        // Evalúo expresion
        // printf("Expresión 2: x + y == 0 && y - x == (-1) * z =%d\n", x + y == 0 && y - x == (-1) * z);

        if ((((x + y == 0) && (y - x)) == ((-1) * z)) == false) {
             printf("Los valores x=%d,y=%d,z=%d no son correctos!\n", x,y,z);
        }
        else {
             printf("Correcto! Los valores de x,y,z para que la expresión 2 sea True son x=%d, y=%d, z=%d\n", x,y,z);
        }
    }
    // Expresión 3
    printf("Expresión 3: not b && w\n");
    int temp_b,temp_w;

    b = false;
    w = true;
    while ((! b && w) == true) {
        // Solicito valor para b
        printf("Ingrese un valor para b para que la expresión sea False (0 para False y 1 para True):\n");
        scanf("%d",&temp_b);
        // # Solicito valor para w
        printf("Ingrese un valor para w para que la expresión sea False (0 para False y 1 para True):\n");
        scanf("%d",&temp_w);
        b = temp_b;
        w = temp_w;

        if ((! b && w) == true) {
            printf("Los valores b=%d y w=%d no es correcto!\n", b,w);
        }
        else {
            printf("Correcto! El valor de b y w para que la expresión 3 sea False son b=%d y w=%d\n", b,w);
        }
    }
    // printf("El valor de b y w para que la expresión 3 sea False son b=%d y w=%d\n", b,w);
    
    //printf("Usted ingresó x=%d, y=%d, z=%d, b=%d, w=%d\n",x,y,z,b,w); 

    // printf("Operación 1: x mod 4 == 0 = %d\n", x % 4 == 0);

    // printf("Operación 2: x + y == 0 && y - x == (-1) * z = %d\n", x + y == 0 && y - x == (-1) * z );

    // printf("Operación 3: not b && w = %d", ! b && w);

    return 0;
}