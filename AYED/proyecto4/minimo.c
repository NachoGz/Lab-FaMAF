#include <stdio.h>

int main()
{
    int x,y;
    int min = 0;
    
    printf("Ingrese el valor de x: \n");
    scanf("%d",&x);
    printf("Ingrese el valor de y \n");
    scanf("%d",&y);
    
    if (x>=y){
        min = y;
    }
    else {
        min = x;
    }
    printf("El mínimo entre %d y %d es: %d\n",x,y,min);
    
    return 0;
}
