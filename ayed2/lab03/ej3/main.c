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
    while (!feof(file))
    {
        fscanf(file, "%u", &index);
        fscanf(file, "%c", &letter);
        if (letter != '>' && letter != '*')
        {
            letters[index] = letter;
        }
        if (index > length)
        {
            length = index;
        }
    }
    fclose(file);
    return length+1;
}

int main(int argc, char *argv[]) {
    FILE *file;
    unsigned int indexes[MAX_SIZE];
    char letters[MAX_SIZE];
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
        exit(EXIT_FAILURE);
    }

    path = argv[1];

    length = data_from_file(path, indexes, sorted, MAX_SIZE);

    /* -- completar -- */

    dump(sorted, length);

    return EXIT_SUCCESS;
}

;