#include <stdbool.h>
#include <assert.h>

#include "fixstring.h"

unsigned int fstring_length(fixstring s) {
    unsigned int length = 0;
    for (unsigned int i=0;i<FIXSTRING_MAX; i++)
    {
        if (s[i] == '\0')
        {
            break;
        } 
        else
        {
            length += 1;
        }
    }
    return length;
}

bool fstring_eq(fixstring s1, fixstring s2) {
    bool eq = true;
    for (unsigned int i; i<FIXSTRING_MAX; i++)
    {
        if (s1[i] != s2[i])
        {
            eq = false;
            break;
        }
    }
    return eq;
}


bool fstring_less_eq(fixstring s1, fixstring s2) {
    bool leq;

    if (s1[0] <= s2[0])
    {
        leq = true;
    }
    else
    {
        leq = false;
    }
    return leq;
}


