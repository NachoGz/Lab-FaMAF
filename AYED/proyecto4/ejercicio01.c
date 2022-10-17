#include <stdio.h>
#include <assert.h>

void holaHasta(int n)
{   
    int i = 1;
    while (i <= n){
        printf("hola\n");
        i = i + 1;
    }
    
}

int main()
{      
    int n;
    printf("Ingresar el valor de n: \n");
    scanf("%d",&n);
    assert (n > 0);
    holaHasta(n);
    return 0;
}
