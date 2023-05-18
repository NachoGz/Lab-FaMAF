#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LENGTH 20

int main(void) {
    char user_input[MAX_LENGTH];

    printf("Ingrese su nombre y apellido: ");
    fgets(user_input, MAX_LENGTH, stdin);
    // scanf("%s", user_input);
    
    // for (int i=0; i< strlen(user_input); i++) {
    //     if (user_input[i] == '\n') {
    //         user_input[i] = '\0';
    //     }
    // }
    user_input[strlen(user_input) - 1] = '\0';


    printf("Te damos la bienvenida %s a este maravilloso programa!\n", user_input);

    return EXIT_SUCCESS;
}

