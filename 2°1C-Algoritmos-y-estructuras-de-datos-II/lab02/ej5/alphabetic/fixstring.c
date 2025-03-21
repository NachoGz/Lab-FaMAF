#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

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
    for (unsigned int i=0; i<FIXSTRING_MAX; i++)
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
    bool leq = true;
    int len;
    len = fstring_length(s1) < fstring_length(s2) ? fstring_length(s1) : fstring_length(s2);
    for (int i=0; i<len; i++)
    {
        if (s1[i] != s2[i])
        {
            leq = (s1[i] <= s2[i]);
            break;
        }
        
    }
    return leq;
}


void fstring_set(fixstring s1, const fixstring s2) {
    int i=0;
    while (i<FIXSTRING_MAX && s2[i]!='\0') {
        s1[i] = s2[i];
        i++;
    }
    s1[i] = '\0';
}

void fstring_swap(fixstring s1,  fixstring s2) {
    fixstring aux;
    fstring_set(aux, s2);
    fstring_set(s2, s1);
    fstring_set(s1, aux);
    
}
