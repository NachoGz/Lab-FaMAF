#include <stdio.h>

int main()
{
    int n;
    printf("Ingrese el valor de n: \n");
    scanf("%d",&n);
    
    if (n<0) {
        n = n * (-1);
    
    }
    
    printf("Valor final de n: %d\n", n);

    return 0;
}
