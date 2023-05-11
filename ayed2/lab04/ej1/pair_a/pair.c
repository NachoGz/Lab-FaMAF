#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "pair.h"

pair_t pair_new(int x, int y) {
    pair_t p = {x,y};
    
    assert((x == p.fst) && (y == p.snd));
    return p;
}
// POS: {p --> (x, y)}


// PRE: {p --> (x, y)}
int pair_first(pair_t p) {
    return p.fst;
}
// POS: {fst == x}


// PRE: {p --> (x, y)}
int pair_second(pair_t p) {
    return p.snd;
}
// POS: {snd == y}


// PRE: {p --> (x, y)}
pair_t pair_swapped(pair_t p) {    
    pair_t q = {p.snd, p.fst};
    assert((pair_first(q) == pair_second(p)) && (pair_second(q) == pair_first(p)));
    return q;   
}
// POS: {pair_first(q) == pair_second(p) && pair_second(q) == pair_first(p)}



// pair_t pair_destroy(pair_t p) {

// }

