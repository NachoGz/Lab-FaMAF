#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "stack.h"

struct _s_stack {
    stack_elem *elems;      // Arreglo de elementos
    unsigned int size;      // Cantidad de elementos en la pila
    unsigned int capacity;  // Capacidad actual del arreglo elems
};

void print_pila(stack s) {

    for (int i=s->size-1; i>=0; i--) {
        printf("%d\n", (s->elems)[i]);
    }
}

int main() {

    stack s = stack_empty();

    // test stack push
    for (int i=0; i < 5; i++) {
        s = stack_push(s, i);
    }

    // visualizo pila
    printf("La pila es:\n");
    print_pila(s);

    // test top
    printf("Elemento al tope:%d\n", stack_top(s));
    
    // test pop
    printf("Elimino elemento al tope\n");
    s = stack_pop(s);

    // visualizo pila
    printf("La pila es:\n");
    print_pila(s);

    printf("El array es:\n");
    stack_elem *array = NULL;
    array = stack_to_array(s);

    printf("[");
    for (unsigned int i=0; i<s->size; i++) {
        printf("%d ", array[i]);
    }
    printf("]\n");

    s = stack_destroy(s);
    free(array);

    return  0;
}