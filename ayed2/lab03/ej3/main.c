// gcc -Wall -Werror -Wextra -pedantic -std=c99 main.c -o main.out

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#define MAX_SIZE 1000

static void dump(char a[], unsigned int length) {
    printf("\"");
    for (unsigned int j=0u; j < length; j++) {
        printf("%c", a[j]);
    }
    printf("\"");
    printf("\n\n");
}

unsigned int data_from_file(const char *path, 
                            unsigned int indexes[], 
                            char letters[], 
                            unsigned int max_size) 
{
    FILE *file;
    file = fopen(path, "r");
    unsigned int index;
    char letter;
    unsigned int length=0;
    
    if (file == null) {
        printf("aaaa");
        exit(EXIT_FAILURE);
    }
    while (!feof(file))
    {   
        if (length >= max_size) {
            printf("\nMax amount of letters reached");
            exit(EXIT_FAILURE);
        }
        
        is_correct = fscanf(file, "%u -> *%c*\n, &index, &letter);
        
        if (!is_correct) {
            printf("\nIncompatible file format\n\n");
            exit(EXIT_FAILURE);
        }
                            
        fscanf(file, "%u", &index);
        fscanf(file, "%c", &letter);
        if (index > max_size) {
            printf("Error. La cantidad máxima de elementos a almacenar es %d\n", max_size);
            exit(EXIT_FAILURE);
        }
        if (letter != '>' && letter != '*')
        {
            letters[index] = letter;
            indexes[index] = index;
        }
                            /*
        if (index > length)
        {
            length = index;
        }
        */
        length++;
    }
    fclose(file);
    return length;
}

int main(int argc, char *argv[]) {
    // FILE *file;
    unsigned int indexes[MAX_SIZE];
    // char letters[MAX_SIZE];
    char sorted[MAX_SIZE];
    unsigned int length=0; 
    //  .----------^
    //  :
    // Debe guardarse aqui la cantidad de elementos leidos del archivo

    /* Parse the filepath given by command line argument. */
    char *path = NULL;
    // Program takes exactly two arguments
    // (the program's name itself and the input-filepath)
    bool valid_args_count = (argc == 2);

    if (!valid_args_count) {
        printf("Usage: %s <input file path>\n\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    path = argv[1];

    length = data_from_file(path, indexes, sorted, MAX_SIZE);

    dump(sorted, length);

    return EXIT_SUCCESS;
}

