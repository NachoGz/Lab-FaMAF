#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "abb.h"

struct _s_abb {
    abb_elem elem;
    struct _s_abb *left;
    struct _s_abb *right;
};

static bool elem_eq(abb_elem a, abb_elem b) {
    return a == b;
}

static bool elem_less(abb_elem a, abb_elem b) {
    return a < b;
}


static bool invrep(abb tree) {

    if (abb_is_empty(tree)) {
        return true;
    }
    if (tree->left && tree->right != NULL) {
        if (elem_less(tree->left->elem, tree->elem) && elem_less(tree->elem, tree->right->elem)) {
            return invrep(tree->left) && invrep(tree->right);        
        } 
        else {
            return false;
        }
    }

    return true;
    
}

abb abb_empty(void) {
    abb tree;
    tree = NULL;
    assert(abb_is_empty(tree));
    return tree;
}

abb abb_add(abb tree, abb_elem e) {
    assert(invrep(tree));
    if (abb_is_empty(tree)) {
        tree = malloc(sizeof(struct _s_abb));
        assert(tree != NULL);
        tree->elem = e;
        tree->left = NULL;
        tree->right = NULL;
    }
    if (elem_eq(e, tree->elem)) {
        return tree;
    }
    else if (elem_less(e, tree->elem)) {
        tree->left = abb_add(tree->left, e);
    }
    else {
        tree->right = abb_add(tree->right, e);
    }
    
    assert(invrep(tree) && abb_exists(tree, e));
    return tree;
}

bool abb_is_empty(abb tree) {
    // assert(invrep(tree));
    return (tree == NULL);
}

bool abb_exists(abb tree, abb_elem e) {
    bool exists=false;
    assert(invrep(tree));
    if (!abb_is_empty(tree)) {
        if (elem_eq(tree->elem, e)) {
            return true;
        }
        else if (elem_less(e, tree->elem)) {
            exists = abb_exists(tree->left, e);
        }
        else if ((elem_less(tree->elem, e))) {
            exists = abb_exists(tree->right, e);
        }

        return exists;
    }
    return exists;
}

unsigned int abb_length(abb tree) {
    unsigned int length=0;
    assert(invrep(tree));

    if (!abb_is_empty(tree)) {
        length = abb_length(tree->right) + abb_length(tree->left) + 1;
    }
    
    assert(invrep(tree) && (abb_is_empty(tree) || length > 0));
    return length;
}


abb abb_minimo_nodo(abb tree) { 
    assert(invrep(tree) && !abb_is_empty(tree));
    // encuentro el nodo que esté más a la izquierda
    abb current = tree;
    while (current && current->left != NULL) {
        current = current->left;
    }
    return current;

    assert(invrep(tree));
}


abb abb_remove(abb tree, abb_elem e) {
    assert(invrep(tree));

    if (abb_is_empty(tree)) {
        return tree;
    }

    if (elem_less(e, tree->elem)) {
        tree->left = abb_remove(tree->left, e);
    }
    else if (e > tree->elem) {
        tree->right = abb_remove(tree->right, e);
    }

    else {
        // si el nodo tiene un hijo o ninguno
        if (tree->left == NULL) {
            abb aux = tree->right;
            free(tree);
            return aux;
        }
        else if (tree->right == NULL) {
            abb aux = tree->left;
            free(tree);
            return aux;
        }

        // su el nodo tiene dos hijos
        // obtengo el sucesor en orden
        abb aux = abb_minimo_nodo(tree->right);

        // Sustituyo el valor por ser eliminado por el sucesor
        tree->elem = aux->elem;

        // elimino el sucesor
        tree->right = abb_remove(tree->right, aux->elem);
    }
    
    assert(invrep(tree) && !abb_exists(tree, e));
    return tree;
}



abb_elem abb_root(abb tree) {
    abb_elem root;
    assert(invrep(tree) && !abb_is_empty(tree));
    root = tree->elem;

    assert(abb_exists(tree, root));
    return root;
}

abb_elem abb_max(abb tree) {
    abb_elem max_e;
    assert(invrep(tree) && !abb_is_empty(tree));
    // encuentro el nodo que esté más a la derecha
    abb current = tree;
    while (current && current->right != NULL) {
        current = current->right;
    }
    max_e = current->elem;

    assert(invrep(tree) && abb_exists(tree, max_e));
    return max_e;
}

abb_elem abb_min(abb tree) {
    abb_elem min_e;
    assert(invrep(tree) && !abb_is_empty(tree));

    min_e = abb_minimo_nodo(tree)->elem;

    assert(invrep(tree) && abb_exists(tree, min_e));
    return min_e;
}

// void abb_dump(abb tree) {
//     assert(invrep(tree));
//     if (tree != NULL) {
//         abb_dump(tree->left);
//         printf("%d ", tree->elem);
//         abb_dump(tree->right);
//     }
// }

void abb_dump(abb tree) {
    assert(invrep(tree));
    if (tree != NULL) {
        printf("%d ", tree->elem);
        abb_dump(tree->left);
        abb_dump(tree->right);
    }
}


abb abb_destroy(abb tree) {
    assert(invrep(tree));
    if (tree != NULL) {
        abb_destroy(tree->left);
        abb_destroy(tree->right);
        free(tree);
    }
    tree = NULL;    
    assert(tree == NULL);
    return tree;
}

