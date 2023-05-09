#include <stdio.h>
#include <stdlib.h>

#include "list.h"

struct _node_t
{
    list_elem value;
    node_t * next;
};

// struct _list 
// {
//     node_t * pointer;
// };


list empty(void) {
    list l = NULL;
    // l = calloc(1, sizeof(node_t *));
    return l;
}


list addl(list_elem e, list l) {
    node_t * p = NULL;
    p = calloc(1, sizeof(node_t));
    p->value = e;
    p->next = l;
    l = p;
    return l;
}


bool is_empty(list l) {
    return (l == NULL);
}


list_elem head(list l) {
    return (l->value);
}


list tail(list l) {
    node_t * p = NULL;
    p = l;
    l = l->next;
    free(p);
    return l;
}


list addr(list l, list_elem e) {
    node_t * p = NULL;
    node_t * q = NULL;
    
    q = calloc(1, sizeof(node_t));
    q->value = e;
    q->next = NULL;

    if (!(is_empty(l))) {
        p = l;
        while (p->next != NULL)
        {
            p = p->next;
        }
        p->next = q;
    }
    else {
        l = q;
    }
    return l;
}


unsigned int length(list l) {
    node_t * p = NULL;
    unsigned int c = 0;

    if (!(is_empty(l))) {
        p = l;
        while (p != NULL)
        {
            p = p->next;
            c += 1;
        }   
    }
    return c;
}


list concat(list l, list l0) {
    node_t * p = NULL;

    if (!(is_empty(l))) {
        p = l;
        while (p->next != NULL)
        {
            p = p->next;
        }
        p->next = l0;   
    }
    return l;
}


/* {PRE: length(l) > n} */
list_elem list_index(list l, unsigned int n) {
    node_t * p = NULL;
    unsigned int i = 0;

    if (!(is_empty(l))) {
        p = l;
        while (p != NULL)
        {   
            if (i == n) {
                return p->value;
            }
            else {
                p = p->next;
                i += 1;
            }
        }   
    }
}


list take(list l, unsigned int n) {
    
}

int main() {
    list l = empty();
    for (int i; i < 5; i++) {
        l = addl(i, l);
    }

    printf("Elemento a eliminar: %d", head(l));
    
    return 0;
}