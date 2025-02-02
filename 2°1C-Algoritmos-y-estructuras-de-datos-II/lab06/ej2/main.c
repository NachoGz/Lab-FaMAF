#include <stdio.h>
#include "string.h"
#include <string.h>

struct _s_string {
    char *content;
    unsigned int length;
};

int main(void) {

    

    string s1 = string_create("arbol");
    string s2 = string_create("arbusto");

    string_dump(s1, stdout);
    string_dump(s2, stdout);
    
    if (string_less(s1, s2)) {
        printf("%s es menor alfabeticamente que %s\n", s1->content, s2->content);
    }
    else {
        printf("%s es menor alfabeticamente que %s\n", s2->content, s1->content);
    }


    if (string_eq(s1, s2)) {
        printf("%s es igual a %s\n", s1->content, s2->content);
    }
    else {
        printf("%s no es igual a %s\n", s1->content, s2->content);
    }

    s1 = string_destroy(s1);
    s2 = string_destroy(s2);
    return 0;
}