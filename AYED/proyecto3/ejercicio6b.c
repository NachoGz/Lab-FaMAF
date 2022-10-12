#include <stdio.h>
#include <stdbool.h>

bool pedirBooleano(void)
{   
    int temp;
    bool x;
    printf("Ingrese un booleano (0 para false y 1 para true): \n");
    scanf("%d", &temp);
    x = temp;
    
    return x;
}

void imprimeBooleano(bool x)
{   
    char* valor;
    if (x == true){
        valor = "verdadero";
    }
    else {
        valor = "falso";
    }
    printf("El valor de booleano es: %s", valor);
}

int main(void)
{   
    bool booleano;
    booleano = pedirBooleano();
    imprimeBooleano(booleano);
    return 0;
}