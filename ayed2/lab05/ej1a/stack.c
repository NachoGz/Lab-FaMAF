#include <stdlib.h>
#include <assert.h>
#include "stack.h"
 
typedef struct _s_stack {
    stack_elem value;
    stack  next;
} * stack;

stack stack_empty() {
    stack s = NULL;
}


stack stack_push(stack s, stack_elem e) {
    stack p = NULL;
    p = calloc(1, sizeof(stack));
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

/* 
stack_elem *stack_to_array(stack s) {
    unsigned int size = stack_size(s);
    int array[size];
    unsigned int i = size - 1; 
    stack p = NULL;

    while (p != NULL)
    {
        array[i] = p->value;
        p = p->next;
        i--;
    }
    
    return array;
}
 */

stack stack_destroy(stack s) {
    stack p = NULL;
    stack q = NULL;
    p = s;
    while (p != NULL)
    {   
        q = p;
        p = p->next;
        free(q);
    }
    q = NULL;
    s = q;
    return s;
}
