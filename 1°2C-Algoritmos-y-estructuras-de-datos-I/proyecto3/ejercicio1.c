#include <stdio.h>
#include <stdbool.h>

int main()
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

    printf("Usted ingresó x=%d, y=%d, z=%d\n\n",x,y,z); 
    
    printf("Operación 1: x + y + 1 = %d\n",x + y + 1);

    printf("Operación 2: z * z + y * 45 - 15 * x = %d\n",(z * z) + (y * 45) - (15 * x));

    printf("Operación 3: y - 2 == (x * 3 + 1) mod 5 = %d\n",(y - 2) == (x * 3 + 1) % 5);

    printf("Operación 4: y / 2 * x = %d\n",y / 2 * x);

    printf("Operación 5: y < x * z = %d\n",y < x * z);

    return 0;
}

 
/*¿En la  ́ultima expresión, que tipo tiene el resultado en lenguaje “C”?
Tipo Int porque C por defecto no tiene booleanos pero se interpreta el 0 como False y el 1 como True. Se puede usar la librería estandar stdbool.h
pero lo unico que hace es asignar el valor cero a un valor false y el 1 a un valor true.
*/
