#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

bool esPrimo(int n)
{
    int i = 2; 
    bool primo=true;
    while (i<n)
    {
        if ((n%i)!=0){
            primo=true;
        }
        else{
            return false;
        }
        i += 1;
    }
    return primo;

}

int nesimo_primo(int N)
{   
    int i=2, k=1, primo=0;
    while (k<=N)
    {
        if (esPrimo(i))
        {
            k += 1;
            primo = i;
        }
        i += 1;
    }
    return primo;
}


int main()
{
    int n, primo;
    printf("Ingrese un número entero: \n");
    scanf("%d", &n);
    assert(n>0);
    primo = nesimo_primo(n);
    printf("El numero primo número %d es: %d\n", n, primo);
    return 0;
}
