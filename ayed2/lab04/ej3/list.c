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
    else {
        l = l0;
    }
    return l;
}


/* {PRE: length(l) > n} */
list_elem list_index(list l, unsigned int n) {
    node_t * p = NULL;
    unsigned int i = 0;

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
    printf("n debe ser menor al largo de la lista");
    exit(EXIT_FAILURE);   
}


list drop(list l, unsigned int n) {
    node_t * p = NULL;
    node_t * q = NULL;
    unsigned int i = 0;

    p = l;
    
    while ((p != NULL) && (i < n)) 
    {
        q = p;
        p = p->next;
        free(q);
        i++;
    }
    l = p;
    return l;
}


list take(list l, unsigned int n) {
    node_t * p = NULL;
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
    return l2;
}


list copy_list(list l1) {
    node_t * p = NULL;
    list l2 = empty();

    p = l1;
    while ((p != NULL)) 
    {   
        l2 = addr(l2, p->value);
        p = p->next;
    }
    return l2;

}


void print_list(list l) {
    node_t * p = NULL;
    
    p = l;
    
    while (p != NULL)
    {
        printf("%d ", p->value);
        p = p->next;
    }
    printf("\n");
}
/* 

int main() {
    list l = empty();
    for (int i=0; i < 5; i++) {
        l = addr(l, i);
    }
    printf("La lista es: ");
    print_list(l);
    printf("\n\n");

    printf("Elemento a eliminar: %d\n", head(l));
    l = tail(l);

    printf("La nueva lista es: ");
    print_list(l);
    printf("\n\n");
    
    printf("Agrego de nuevo el elemento\n");
    l = addl(0, l);
    printf("La nueva lista es: ");
    print_list(l);
    printf("\n\n");

    printf("Agrego un nuevo elemento\n");
    l = addr(l, 5);
    printf("La nueva lista es: ");
    print_list(l);
    printf("\n\n");

    printf("La nueva lista tiene una longitud de: %d\n", length(l));
    printf("\n\n");

    printf("El elemento en la posición 3 es: %d\n", list_index(l, 3));
    printf("\n\n");

    list l2 = empty();
    l2 = copy_list(l);
    printf("Elimino los primeros 3 elementos\n");
    l2 = drop(l2, 3);
    print_list(l2);
    printf("\n\n");

    list l3 = empty();
    l3 = copy_list(l);
    print_list(l3);
    printf("Elimino los últimos 3 elementos\n");
    l3 = take(l3, 3);
    print_list(l3);
    printf("\n\n");
    
    return 0;
} */