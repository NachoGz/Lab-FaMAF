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

static bool invrep(dict_t d) {
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
    else if (string_less(word, dict->key) || string_eq(word, dict->key)) {
        dict->left = dict_add(dict->left, word, def);
    }
    else {
        dict->right = dict_add(dict->right, word, def);
    }

    assert(invrep(dict));
}

value_t dict_search(dict_t dict, key_t word) {
    key_t def=NULL;
    /* needs implementation */
    return NULL;
}

bool dict_exists(dict_t dict, key_t word) {
    /* needs implementation */
    return false;
}

unsigned int dict_length(dict_t dict) {
    /* needs implementation */
    return 0u;
}

dict_t dict_remove(dict_t dict, key_t word) {
    /* needs implementation */
    return dict;
}

dict_t dict_remove_all(dict_t dict) {
    /* needs implementation */
    return dict;
}

void dict_dump(dict_t dict, FILE *file) {
    /* needs implementation */
}

dict_t dict_destroy(dict_t dict) {
    /* needs implementation */
    return dict;
}

