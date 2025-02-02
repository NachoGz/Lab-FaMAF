#include <stdio.h>
#include <assert.h>

struct div_t {
    int cociente;
    int resto;

};

struct div_t division(int x, int y){

    struct div_t dupla;
    dupla.cociente = x / y;
    dupla.resto = x % y;

    return dupla;

}


int main(){

    int x,y;
    struct div_t div;
    // Solicito valor de x
    printf("Ingrese el dividendo: \n");
    scanf("%d", &x);
    
    // Solicito valor de N
    printf("Ingrese el divisor: \n");
    scanf("%d", &y);

    // Verifico que y no sea 0
    assert(y!=0);
    
    // Obtengo el struct div_t resultante de aplicar la funcion divison
    div = division(x,y);

    printf("La division %d/%d tiene como cociente %d y de resto %d \n", x, y, div.cociente, div.resto);
    return 0;
}