#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

char *parse_filepath(int argc, char *argv[]) {
    /* Parse the filepath given by command line argument. */
    char *result = NULL;

    if (argc < 2) {
        // print_help(argv[0]);
        exit(EXIT_FAILURE);
    }

    result = argv[1];

    return (result);
}

int main(int argc, char *argv[]){

    char *filepath = NULL;
    FILE* file;

    /* parse the filepath given in command line arguments */
    filepath = parse_filepath(argc, argv);

    file = fopen(filepath, "r");
    if (file == NULL)
    {
        fprintf(stderr, "File does not exist.\n");
        exit(EXIT_FAILURE);
    }

    int i=0;
    char c;
    /* Completar aqui */
    while (!feof(file))
    {
      fscanf(file, "%c", &c);
      printf("%c", c); 
      
    }

    return 0;
}