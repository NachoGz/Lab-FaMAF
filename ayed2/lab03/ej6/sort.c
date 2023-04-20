/*
  @file sort.c
  @brief sort functions implementation
*/

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include "helpers.h"
#include "sort.h"
#include "player.h"

bool goes_before(player_t x, player_t y){
    return (((*x).rank) <= ((*y).rank));
}


bool array_is_sorted(player_t atp[], unsigned int length) {
    unsigned int i = 1u;
    while (i < length && goes_before(atp[i - 1u], atp[i])) {
        i++;
    }
    return (i == length);
}

void swap_player(player_t a[], unsigned int i, unsigned int j) {
    player_t aux;
    aux = a[i];
    a[i] = a[j];
    a[j] = aux;
}

// insertion sort
/*
void sort(player_t a[], unsigned int length) {
    
    for (unsigned int i = 1u; i < length; ++i) {
        unsigned int j = i;
    
        while (j > 0 && goes_before(a[j], a[j-1]))
        {   
            swap_player(a, j, j-1);
            j--;
        }
        assert(array_is_sorted(a, i));
        

    }
}
*/

// quick sort

static unsigned int partition(player_t a[], unsigned int izq, unsigned int der) {
    int i, j, ppiv;
    ppiv = izq;
    i = izq + 1;
    j = der;

    while (i <= j)
    {
        if (goes_before(a[i], a[ppiv]))
        {
            i += 1;
        }
        else
        {
            if (goes_before(a[ppiv], a[j]))
            {
                j -= 1;
            }
            else
            {
                swap_player(a, i, j);
            }
        }
    }
    swap_player(a, ppiv, j);
    return j;
}

void sort_rec(player_t a[], unsigned int izq, unsigned int der) {
    unsigned int ppiv;
    
    if (der > izq)
    {   
        ppiv = partition(a, izq, der);
        sort_rec(a, izq, (ppiv == 0u) ? 0u : ppiv - 1u);
        sort_rec(a, ppiv+1, der);
    }    
}

void sort(player_t a[], unsigned int length) {
    sort_rec(a, 0, (length == 0) ? 0 : length - 1);
}


