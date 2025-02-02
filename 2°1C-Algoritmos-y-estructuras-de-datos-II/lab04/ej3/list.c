#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "list.h"

struct _node_t
{
    list_elem value;
    node_t * next;
};


list empty(void) {
    list l = NULL;
    return l;
}


list addl(list_elem e, list l) {
    list p = NULL;

    p = malloc(sizeof(struct _node_t));
    p->value = e;
    p->next = l;
    l = p;

    return l;
}


bool is_empty(list l) {
    return (l == NULL);
}


list_elem head(list l) {
    assert(!is_empty(l)); // Pre-condición
    return (l->value);
}


list tail(list l) {
    assert(!is_empty(l)); // Pre-condición

    list p = NULL;

    p = l;
    l = l->next;

    p = destroy_list(p);
    return l;
}


list addr(list l, list_elem e) {
    list p = NULL;
    list q = NULL;
    

    q = malloc(sizeof(struct _node_t));
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
    list p = NULL;
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
    list p = NULL;

    if (!(is_empty(l))) {
        p = l;
        while (p->next != NULL)
        {
            p = p->next;
        }
        p->next = l0;   
    }
    else {
        l = l0;
    }
    return l;
}


list_elem list_index(list l, unsigned int n) {
    assert(length(l) > n); // Pre-condición
    list p = NULL;
    unsigned int i = 0;
    list_elem elem;

    p = l;
    while (p != NULL)
    {   
        if (i == n) {
            elem = p->value;
            break;
        }
        else {
            p = p->next;
            i += 1;
        }
    }
    return elem;
}


list take(list l, unsigned int n) {
    list p = NULL;
    unsigned int i = 0;
    unsigned int pos = length(l) - n;
    list l2 = empty();

    p = l;
    while ((p != NULL)) 
    {   
        if (i < pos) {
            l2 = addr(l2, head(l));
        }
        p = p->next;
        i++;
        l = tail(l);
    }
    l = destroy_list(l);
    return l2;
}


list drop(list l, unsigned int n) {
    unsigned int i = 0;

    
    while ((l != NULL) && (i < n)) 
    {
        tail(l);
        l = l->next;
        i++;
    }
    return l;
}


list copy_list(list l1) {
    list p = NULL;
    list l2 = empty();

    p = l1;
    while ((p != NULL)) 
    {   
        l2 = addr(l2, p->value);
        p = p->next;
    }

    return l2;

}


list destroy_list(list l) {
    list p = NULL;
    while (l != NULL)
    {   
        p = l;
        l = l->next;
        free(p);
    }

    return NULL;
}
