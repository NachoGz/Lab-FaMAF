// gcc -Wall -Werror -Wextra -pedantic -std=c99 main.c -o main.out

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#define MAX_SIZE 1000

void print_help(char *program_name) {
    /* Print the usage help of this program. */
    printf("Usage: %s <input file path>\n\n"
           "Loads an array given in a file in disk and prints it on the screen."
           "\n\n",
           program_name);
}

char *parse_filepath(int argc, char *argv[]) {
    /* Parse the filepath given by command line argument. */
    char *result = NULL;
    // Program takes exactly two arguments
    // (the program's name itself and the input-filepath)
    bool valid_args_count = (argc == 2);

    if (!valid_args_count) {
        print_help(argv[0]);
        exit(EXIT_FAILURE);
    }

    result = argv[1];

    return result;
}

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
    unsigned int cant_leidos = 0;
    bool is_valid;

    while (!feof(file))
    {
        is_valid = fscanf(file, "%u -> *%c*", &index, &letter);
        
        if (!is_valid) {
            printf("Error! Invalid format\n");
            exit(EXIT_FAILURE);
        }

        if (index > max_size) {
            printf("Error. La cantidad máxima de elementos a almacenar es %d\n", max_size);
            exit(EXIT_FAILURE);
        }

        letters[index] = letter;
        indexes[index] = index;
        cant_leidos++;
    }
    fclose(file);
    return cant_leidos;
}

int main(int argc, char *argv[]) {
    FILE *file;
    unsigned int indexes[MAX_SIZE];
    // char letters[MAX_SIZE];
    char sorted[MAX_SIZE];
    unsigned int length=0; 
    //  .----------^
    //  :
    // Debe guardarse aqui la cantidad de elementos leidos del archivo

    /* Parse the filepath given by command line argument. */
    char *filepath = NULL;
 
    filepath = parse_filepath(argc, argv);

    file = fopen(filepath, "r");
    if (file == NULL) {
        fprintf(stderr, "File does not exist.\n");
        exit(EXIT_FAILURE);
    }
    length = data_from_file(filepath, indexes, sorted, MAX_SIZE);

    dump(sorted, length);

    return EXIT_SUCCESS;
}

