#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "dict.h"
#include "key_value.h"
#include <stdbool.h>
#include "string.h"

struct _node_t {
    dict_t left;
    dict_t right;
    key_t key;
    value_t value;
};

static bool invrep(dict_t dict) {
    if (dict == NULL) {
        return true;
    }

    if (dict->left != NULL) {
        if (!string_less(dict->left->key, dict->key)) {
            return false;
        }
        if (!invrep(dict->left)) {
            return false;
        }
    }

    if (dict->right != NULL) {
        if (string_less(dict->right->key, dict->key) || string_eq(dict->right->key, dict->key)) {
            return false;
        }
        if (!invrep(dict->right)) {
            return false;
        }
    }

    return true;
}

dict_t dict_empty(void) {
    dict_t dict = NULL;
    return dict;
}

dict_t dict_add(dict_t dict, key_t word, value_t def) {
    assert(invrep(dict));

    if (dict == NULL) {
        dict = malloc(sizeof(struct _node_t));
        assert(dict != NULL);
        dict->key = word;
        dict->value = def;
        dict->left = dict->right = NULL;
    }
    else if (string_less(word, dict->key)) {
        dict->left = dict_add(dict->left, word, def);
    }
    else if (string_less(dict->key, word)){
        dict->right = dict_add(dict->right, word, def);
    }
    else {
        dict->value = def;
    }

    return dict;
    assert(invrep(dict));
}

value_t dict_search(dict_t dict, key_t word) {
    assert(invrep(dict));
    if (dict_exists(dict, word)) {
        if (string_eq(dict->key, word)) {
            return dict->value;
        }

        if (string_less(word, dict->key)) {
            return dict_search(dict->left, word);
        }

        return dict_search(dict->right, word);
    }
    else {
        return NULL;
    }
    

}

bool dict_exists(dict_t dict, key_t word) {
    bool exists=false;
    assert(invrep(dict));

    if (dict != NULL) {
        if (string_eq(dict->key, word)) {
            return true;
        }
        else if (string_less(word, dict->key)) {
            exists = dict_exists(dict->left, word);
        }
        else {
            exists = dict_exists(dict->right, word);
        }

        return exists;
    }
    return exists;
}

unsigned int dict_length(dict_t dict) {
    unsigned int length=0;
    assert(invrep(dict));

    if (dict != NULL) {
        length = dict_length(dict->right) + dict_length(dict->left) + 1;
    }
    
    assert(invrep(dict) && ((dict == NULL) || length > 0));
    return length;
}

dict_t dict_minimo_nodo(dict_t dict) {
    assert(invrep(dict));
    // encuentro el nodo que esté más a la izquierda
    dict_t current = dict;
    while (current && current->left != NULL) {
        current = current->left;
    }
    return current;

    assert(invrep(dict));
}


dict_t dict_remove(dict_t dict, key_t word) {
    assert(invrep(dict));

    if (dict_exists(dict, word)) {
        if (string_eq(word, dict->key)) {
            // si el node tiene un hijo o ninguno
            if (dict->left == NULL) {
                dict_t aux = dict->right;
                free(dict);
                return aux;
            }
            else if (dict->right == NULL) {
                dict_t aux = dict->left;
                free(dict);
                return aux;
            }

            // si el node tiene dos hijos
            // obtengo el sucesor en orden
            dict_t aux = dict_minimo_nodo(dict->right);

            // // Sustituyo el par (key, value) por ser eliminado por el sucesor
            dict->key = aux->key;
            dict->value = aux->value;

            // elimino el sucesor
            dict->right = dict_remove(dict->right, dict->key);
        }

        else if (string_less(word, dict->key)) {
            dict->left = dict_remove(dict->left, word);
        }
        else {
            dict->right = dict_remove(dict->right, word);
        }

    }

    return dict;
}

dict_t dict_remove_all(dict_t dict) {
    if (dict != NULL) {
        dict_remove_all(dict->left);
        dict_remove_all(dict->right);
        dict->key = string_destroy(dict->key);
        dict->value = string_destroy(dict->value);
        free(dict);
        dict = NULL;
    }
    return dict;
}

void dict_dump(dict_t dict, FILE *file) {
    if (dict != NULL) {
        dict_dump(dict->left, file);
        string_dump(dict->key, file);
        fprintf(file, " : ");
        string_dump(dict->value, file);
        fprintf(file, "\n");
        dict_dump(dict->right, file);
    }
}

dict_t dict_destroy(dict_t dict) {
    if (dict != NULL) {
        dict_remove_all(dict->left);
        dict_remove_all(dict->right);
        dict->key = string_destroy(dict->key);
        dict->value = string_destroy(dict->value);
        free(dict);
        dict = NULL;
    }
    return NULL;
}

