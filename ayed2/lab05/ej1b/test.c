#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "stack.h"


struct _s_stack {
    stack_elem value;
    stack  next;
};


void print_pila(stack s) {
    stack p = NULL;
    p = s;
    while (p != NULL)
    {
        printf("%d\n", p->value);
        p = p->next;
    }
    
}


int main() {
    // creo pila vacía
    stack s = stack_empty();

    

    s = stack_push(s, 1);
    s = stack_pop(s);
    for (int i=0;i<5;i++) {
        s = stack_push(s, i);
    }
    
    printf("La pila es:\n\n");
    print_pila(s);
    printf("\n");

    stack_elem *array = NULL;
    array = stack_to_array(s);

    printf("La pila en array:\n");
    printf("[");
    for (unsigned int i=0; i < stack_size(s); i++) {
        printf(" %d ", array[i]);
    }
    printf("]\n");
    stack_destroy(s);
    free(array);
    return 0;
}
 