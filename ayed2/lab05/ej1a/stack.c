#include <stdlib.h>
#include <assert.h>
#include "stack.h"

 
struct _s_stack {
    stack_elem value;
    stack  next;
};

stack stack_empty() {
    stack s = NULL;
    return s;
}


stack stack_push(stack s, stack_elem e) {
    stack p = NULL;

    p = malloc(sizeof(struct _s_stack));
    assert(p != NULL);
    p->value = e;
    p->next = s;
    return p;
}


stack stack_pop(stack s) {
    stack p = NULL;
    p = s->next;
    free(s);
    s = NULL;
    return p;
}


unsigned int stack_size(stack s) {
    unsigned int c = 0;
    stack p = NULL;
    
    p = s;
    while (p != NULL)
    {   
        p = p->next;
        c++;
    }
    
    return c;
}


stack_elem stack_top(stack s) {
    return s->value;
}


bool stack_is_empty(stack s) {
    return (s == NULL);
}


stack_elem *stack_to_array(stack s) {
    unsigned int size = stack_size(s);
    int *array = NULL;
    unsigned int i = size - 1; 
    stack p = NULL;

    array = calloc(size, sizeof(int));
    p = s; 
    while (p != NULL)
    {
        array[i] = p->value;
        p = p->next;
        i--;
    }
    
    return array;
}


// stack stack_copy(stack s) {
//     stack p = NULL;
//     stack copy = stack_empty();
    
//     p = s;
//     // copy = malloc(sizeof(struct _s_stack));
//     while (p != NULL)
//     {
//         copy = stack_push(copy, p->value);
//         p = p->next;
//     }
//     return copy;
// }

stack stack_destroy(stack s) {

    assert(s != NULL);
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
