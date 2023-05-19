#include <stdio.h>
#include <stdlib.h>

#include "strfuncs.h"

size_t string_length(const char *str) {
    size_t i = 0;
    while (*(str + i) != '\0')
    {
        i++;
    }
    return i;
}


char *string_filter(const char *str, char c) {
    char *filtered_string = NULL;
    filtered_string = malloc(sizeof(char)*string_length(str));
    unsigned int pos = 0;

    for (unsigned long i=0; i<string_length(str); i++) {
        if (*(str + i) != c) {
            *(filtered_string + pos) = *(str + i);
            pos++;
        }
    }

    return filtered_string;
}


int main(void) {

    char *some_str=NULL;
    some_str = "h.o.l.a m.u.n.d.o.!";
    char *filtered=NULL;

    filtered = string_filter(some_str, '.');
    printf("original: '%s' (%lu)\n"
           "filtrada: '%s' (%lu)\n",
           some_str, string_length(some_str),
           filtered, string_length(filtered));

    free(filtered);
    filtered = NULL;

    return EXIT_SUCCESS;
}

