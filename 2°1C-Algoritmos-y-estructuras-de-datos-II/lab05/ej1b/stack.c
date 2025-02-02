#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include "stack.h"

 
struct _s_stack {
    struct s_node *first;
    unsigned int size;
};

struct s_node {
    stack_elem elem;
    struct s_node *next;
};


static struct s_node *create_node(stack_elem e) {
    struct s_node *new_node=malloc(sizeof(struct s_node));
    assert(new_node!=NULL);
    new_node->elem = e;
    new_node->next = NULL;
    return new_node;
}


static struct s_node *destroy_node(struct s_node *node) {
    node->next=NULL;
    free(node);
    node=NULL;
    return node;
}


bool invrep(stack s) {
    return (s != NULL);
}

stack stack_empty() {
    stack s = NULL;
    s = malloc(sizeof(struct s_node));
    s->first = NULL;
    s->size = 0;
    return s;
}



stack stack_push(stack s, stack_elem e) {
    assert(invrep(s));
    struct s_node *new_stack = create_node(e);

    if (s->first == NULL) {
        s->first = new_stack;
        s->size = 1;
    }
    else {
        new_stack->next = s->first;
        s->first = new_stack;
        s->size++;
    }
    assert(invrep(s) && !stack_is_empty(s) && stack_top(s) == e);
    return s;
}


stack stack_pop(stack s) {
    assert(invrep(s) && !(stack_is_empty(s)));
    
    struct s_node *current = s->first;
    s->first = s->first->next;
    
    // current->next = NULL;
    // free(current);
    // current = NULL;
    current = destroy_node(current);

    assert(invrep(s));
    return s;
}


unsigned int stack_size(stack s) {
    assert(invrep(s));

    return s->size;
}


stack_elem stack_top(stack s) {
    assert(invrep(s) && !stack_is_empty(s));
    return s->first->elem;
}


bool stack_is_empty(stack s) {
    assert(invrep(s));
    return (s->first == NULL);
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
    assert(invrep(s));

    struct s_node *current = s->first;
    
    while (current != NULL) {
        struct s_node *next = current;   
        current = current->next;
        next = destroy_node(next);        
    }
    free(s);
    s = NULL;
    assert(s == NULL);

    return s;
}
