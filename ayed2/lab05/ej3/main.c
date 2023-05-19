#include <stdio.h>
#include <stdlib.h>
#include "hanoi.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: ./solve-hanoi <disk-count>\n");
        exit(EXIT_FAILURE);
    }
    int count = atoi(argv[1]);
    if (count < 0) {
        printf("Negative disk-count is not allowed\n");
        exit(EXIT_FAILURE);
    }
    if (count > 20) {
        printf("> 20 is too slow!\n");
        exit(EXIT_FAILURE);
    }
    hanoi_t hanoi = hanoi_init(count);
    hanoi_print(hanoi);
    hanoi_solve(hanoi);
    hanoi = hanoi_destroy(hanoi);
    return 0;
}

/* 
Bugs: el bug estaba en stack_size que daba error al pedir el length de un stack vacío ya que
		la función no estaba definida para ese caso. Ya está solucionado.
Valgrind:
        memory leaks: el memory leak estaba en la función hanoi_destroy donde solo liberaba el 
        puntero que apunta a la torre y no estaba liberando el stack hanoi->target. Por eso 	
        faltaban siempre tantos free como la cantidad de discos.
        Conditional jump or move depends on uninitialised value(s): hanoi->source no estaba 	 
        inicializado en null en hanoi_init.
		

*/
