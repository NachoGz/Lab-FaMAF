#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include "stack.h"

 
struct _s_stack {
    stack_elem value;
    stack  next;
};

stack stack_empty() {
    stack s = NULL;
    return s;
}


bool invrep(stack s) {
    return s != NULL;
}


stack stack_push(stack s, stack_elem e) {
    stack new_stack = NULL;

    new_stack = malloc(sizeof(struct _s_stack));
    assert(new_stack != NULL);
    new_stack->value = e;
    new_stack->next = s;

    return new_stack;
}


stack stack_pop(stack s) {
    assert(invrep(s));
    stack aux = NULL;
    aux = s;
    s = s->next;
    free(aux);
    aux = NULL;
    return s;
}


unsigned int stack_size(stack s) {
    unsigned int c = 0;
    stack current = NULL;
    
    current = s;
    while (current != NULL)
    {   
        current = current->next;
        c++;
    }
    
    return c;
}


stack_elem stack_top(stack s) {
    assert(invrep(s));
    return s->value;
}


bool stack_is_empty(stack s) {
    return (s == NULL);
}


stack_elem *stack_to_array(stack s) {
    unsigned int size = stack_size(s);
    int *array = NULL;
    if (!stack_is_empty(s)) {
        array = calloc(size, sizeof(int));
        assert(array != NULL);

        for (int i=size - 1; i >= 0; i--) {
            array[i] = stack_top(s);
            s = stack_pop(s);
        }   
    }
    
    
    return array;
}


stack stack_destroy(stack s) {
    stack current = NULL;
    stack next = NULL;
    current = s;
    
    while (current != NULL)
    {   
        next = current->next;
        free(current);
        current = next;
    }
    s = NULL;
    return NULL;
}
