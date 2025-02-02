#include <stdlib.h>  /* EXIT_SUCCESS... */
#include <stdio.h>   /* printf()...     */
#include "pair.h"    /* TAD Par         */

struct s_pair_t {
    int fst;
    int snd;
};

static void show_pair(pair_t p) {
    printf("(%d, %d)\n", p->fst, p->snd);
}

int main(void) {
    pair_t p, q;
    // Nuevo par p
    p = pair_new(3, 4);
    // Se muestra el par por pantalla
    printf("p = ");
    show_pair(p);
    // Nuevo para q con elementos de p intercambiados
    q = pair_swapped(p);
    // Se muestra q
    printf("q = ");
    show_pair(q);
    // Se destruyen p y q
    p = pair_destroy(p);
    q = pair_destroy(q);
    return EXIT_SUCCESS;
}

/*
Si hay encapsulamiento porque no puede acceder a la representación interna del TAD, ya que
se uso typedef struct s_pair_t * pair_t. Esto significa que hay un puntero que apunta al struct.
Con eso, C ya puede compilar bien porque sabe cuanta memoria guardar para ese archivo. Depende del programador que importe
esa librería definir correctamente el TAD.
*/