#include <stdlib.h>
#include <assert.h>
#include "stack.h"

struct _s_stack {
    stack_elem *elems;      // Arreglo de elementos
    unsigned int size;      // Cantidad de elementos en la pila
    unsigned int capacity;  // Capacidad actual del arreglo elems
};


stack stack_empty() {
    stack empty_stack = NULL;
    
    empty_stack = malloc(sizeof(struct _s_stack));
    assert(empty_stack != NULL);
    empty_stack->size = 0;
    empty_stack->capacity = 10;
    empty_stack->elems = calloc(empty_stack->capacity, sizeof(stack_elem));
    assert((empty_stack->elems) != NULL);
    
    return empty_stack;
}


stack stack_push(stack s, stack_elem e) {
    if (s->capacity == s->size) { // me fijo si la pila esta llena
        s->capacity *= 2;
        s = realloc(s, sizeof(stack_elem));
        assert(s != NULL);
    }
    else {
        (s->elems)[s->size] = e;
        s->size++;
    }
    return s;
}


stack stack_pop(stack s) {
    if (!stack_is_empty(s)) {
        (s->elems)[s->size-1] = 0;
        s->size--;
    }
    return s;
}


unsigned int stack_size(stack s) {
    return s->size;
}


stack_elem stack_top(stack s) {
    return (s->elems)[s->size-1];
}


bool stack_is_empty(stack s) {
    return (s->size == 0);
}


stack_elem *stack_to_array(stack s) {
    stack_elem *array = NULL;
    
    array = calloc(s->capacity, sizeof(stack_elem));
    assert(array != NULL);
    for (unsigned int i=0; i < s->size; i++) {
        array[i] = (s->elems)[i];
    }
    return array;
}


stack stack_destroy(stack s) {
    assert(s != NULL);
    free(s->elems);
    free(s);
    return NULL;
}


