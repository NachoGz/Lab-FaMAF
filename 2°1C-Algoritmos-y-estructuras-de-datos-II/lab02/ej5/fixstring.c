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


/*
int main()
{   
    fixstring word1[2]={"hola", "chau"};
    fixstring word2[2] = {"casa", "perro"};

    // pre-swap
    printf("Before swap:\n\n");
    printf("Word 1\n");
    for (int i=0; i<2; i++)
    {
        printf("%s ", word1[i]);
    }
    printf("\n---------------------\n\n");
    printf("Word 2\n");
    for (int i=0; i<2; i++)
    {
        printf("%s ", word2[i]);
    }
    printf("\n---------------------\n\n");
    
    for (int i=0; i<2; i++)
    {
        fstring_swap(word1[i], word2[i]);
    }
    
    
    // after-swap
    printf("After swap:\n");
    printf("Before swap:\n\n");
    printf("Word 1\n");
    for (int i=0; i<2; i++)
    {
        printf("%s ", word1[i]);
    }
    printf("\n---------------------\n\n");
    printf("Word 2\n");
    for (int i=0; i<2; i++)
    {
        printf("%s ", word2[i]);
    }
    printf("\n---------------------\n\n");
    
    
    return 0;
}
*/
