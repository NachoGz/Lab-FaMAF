#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "array_helpers.h"
#include "sort_helpers.h"
#include "sort.h"
#include "fixstring.h"

static unsigned int partition(fixstring a[], unsigned int izq, unsigned int der) {
    int i, j, ppiv;
    ppiv = izq;
    i = izq + 1;
    j = der;

    while (i <= j)
    {
        if (fstring_less_eq(a[i], a[ppiv]))
        {
            i += 1;
        }
        else
        {
            if (!(fstring_less_eq(a[j], a[ppiv])) || fstring_eq(a[j], a[ppiv]))
            {
                j -= 1;
            }
            else
            {
                if (!(fstring_less_eq(a[i], a[j])) || fstring_less_eq(a[j], a[ppiv]))
                {
                    swap(a,i,j);
                }
            }
        }
    }
    fstring_swap(a[ppiv], a[j]);
    return j;
}

static void quick_sort_rec(fixstring a[], unsigned int izq, unsigned int der) {
    unsigned int ppiv;
    if (der > izq)
    {   
        ppiv = partition(a, izq, der);
        quick_sort_rec(a, izq, (ppiv == 0) ? 0 : ppiv - 1);
        quick_sort_rec(a, ppiv+1, der);
    }
}

void quick_sort(fixstring a[], unsigned int length) {
    quick_sort_rec(a, 0, (length == 0) ? 0 : length - 1);
}

