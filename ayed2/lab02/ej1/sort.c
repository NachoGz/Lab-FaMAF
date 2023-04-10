#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include "array_helpers.h"
#include "sort_helpers.h"
#include "sort.h"

// gcc -Wall -Werror -Wextra -pedantic -std=c99 -c array_helpers.c sort.c main.c
// gcc -Wall -Werror -Wextra -pedantic -std=c99 -no-pie array_helpers.o sort.o sort_helpers.o main.o -o sorter

static void insert(int a[], unsigned int i) {
    int j;
    j = i;

    while (j > 0 && a[j] < a[j-1])
    {
        swap(a, j-1, j);
        j -= 1;
    }
    
}


void insertion_sort(int a[], unsigned int length) {
    for (unsigned int i = 1u; i < length; ++i) {
        insert(a, i);
        // assert(array_is_sorted(a, i));
    }
}

// falta parte b

