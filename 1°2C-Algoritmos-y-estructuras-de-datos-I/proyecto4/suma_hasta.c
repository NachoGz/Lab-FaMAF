#include <stdio.h>
#include <assert.h>

int suma_hasta(int N)
{   
    int a=0,n=1;
    while (n!=(N+1)){
        a = a + n; 
        n = n + 1;
    }
    return a;
}

int main()
{   
    int N;
    // Solicito valor de N
    printf("Ingrese un número entero: \n");
    scanf("%d", &N);
    
    assert(N>=0);
    
    printf("La suma de los primeros %d números naturales es: %d\n",N, suma_hasta(N));

    return 0;
}