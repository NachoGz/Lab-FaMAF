#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "queue.h"

struct s_queue {
    unsigned int size;
    struct q_node *first;
};

struct q_node {
    queue_elem elem;
    struct q_node *next;
};

static struct q_node *create_node(queue_elem e) {
    struct q_node *new_node=malloc(sizeof(struct q_node));
    assert(new_node!=NULL);
    new_node->elem = e;
    new_node->next = NULL;
    return new_node;
}

static struct q_node *destroy_node(struct q_node *node) {
    node->next=NULL;
    free(node);
    node=NULL;
    return node;
}


static bool invrep(queue q) {
    return q != NULL;
}

queue queue_empty(void) {
    queue q=NULL;
    q = malloc(sizeof(struct s_queue));
    
    q->first=NULL;
    q->size=0;

    assert(invrep(q) && queue_is_empty(q));
    return q;
}

queue queue_enqueue(queue q, queue_elem e) {
    assert(invrep(q));
    struct q_node *new_node = create_node(e);
    
    if (q->first==NULL) {
        q->first = new_node;
        q->size = 1;
    } 
    else {
        struct q_node *node = q->first;
        while (node->next != NULL) {
            node = node->next;
        }
        node->next = new_node;
        q->size++;
    }
    // assert(invrep(q) && !queue_is_empty(q) && queue_first(q) == e);
    assert(invrep(q) && !queue_is_empty(q));
    return q;
}

bool queue_is_empty(queue q) {
    assert(invrep(q));
    return q->first == NULL;
}

queue_elem queue_first(queue q) {
    assert(invrep(q) && !queue_is_empty(q));
    return q->first->elem;
}
unsigned int queue_size(queue q) {
    assert(invrep(q));
    // unsigned int size=0;
    
    return q->size;
}

queue queue_dequeue(queue q) {
    assert(invrep(q) && !queue_is_empty(q));
    struct q_node * killme=q->first;
    q->first = q->first->next;
    killme = destroy_node(killme);
    assert(invrep(q));
    return q;

}


queue queue_disscard(queue q, unsigned int n) {
    assert(invrep(q) && !queue_is_empty(q));

    unsigned int i = 0;
    struct q_node *node = q->first;
    queue new_queue = queue_empty();

    while (node != NULL) {
        if (i != n) {
            new_queue = queue_enqueue(new_queue, node->elem);
        }
        node = node->next;
        i++;
    }

    assert(invrep(q) && (queue_size(new_queue) == (queue_size(q) - 1)));

    return new_queue;
}

void queue_dump(queue q, FILE *file) {
    file = file==NULL ? stdout: file;
    struct q_node *node=q->first;
    fprintf(file, "[ ");
    while(node!=NULL) {
        fprintf(file, "%d", node->elem);
        node = node->next;
        if (node != NULL) {
            fprintf(file, ", ");
        }
    }
    fprintf(file, "]\n");
}

queue queue_destroy(queue q) {
    assert(invrep(q));
    struct q_node *node=q->first;
    while (node != NULL) {
        struct q_node *killme=node;
        node = node->next;
        killme = destroy_node(killme);
    }
    free(q);
    q = NULL;
    assert(q == NULL);
    return q;
}

