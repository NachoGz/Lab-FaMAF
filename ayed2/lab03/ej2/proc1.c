#include <stdlib.h>
#include <stdio.h>

// gcc -Wall -Werror -pedantic -std=c99 proc1.c -o abs1

void absolute(int x, int y) {
    if (x >= 0) {
        y = x;
    }
    else {
        y = -x;
    }
}

int main(void) {
    int a=0, res=0;

    a = -10;
    absolute(a, res);
    printf("%d\n", res);
    return EXIT_SUCCESS;
}

// Resultado: 0. No coincide con valor del teorico

