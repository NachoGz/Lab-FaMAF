#include <stdio.h>
#include <stdlib.h>

#include "data.h"

void print_data(data_t d) {
    printf("NOMBRE: %s\n"
           "EDAD  : %d años\n"
           "ALTURA: %d cm\n\n",
           d.name, d.age, d.height);
}

int main(void) {

    data_t messi = {"Leo Messi", 35, 169};
    print_data(messi);

    printf("name-size  : %lu bytes\n"
           "age-size   : %lu bytes\n"
           "height-size: %lu bytes\n"
           "data_t-size: %lu bytes\n", sizeof(messi.name), sizeof(messi.age), sizeof(messi.height), sizeof(messi));

    return EXIT_SUCCESS;
}

/*
La suma de los miembros no coincide con el total. El tamaño del campo name no depende del nombre que contiene sino que es un
array de char de tamaño 30, por lo tanto siempre va a tener tamaño de 30 bytes independientemente del largo del nombre.
*/