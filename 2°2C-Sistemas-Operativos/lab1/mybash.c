#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include "command.h"
#include "execute.h"
#include "parser.h"
#include "parsing.h"
#include "builtin.h"

#define MAX_PATH 256 // largo máximo de un path(255) + 1('\0')
#define GREEN "\033[0;32m"   // colores de caracteres
#define BLUE "\033[0;34m"
#define RESET_COLORS "\033[0m"

static void show_prompt(void) {
    // Obtengo el nombre de usuario y hostname
    char hostname[_SC_HOST_NAME_MAX + 1];
    char* username = getenv("USER");
    gethostname(hostname, _SC_HOST_NAME_MAX+1);
    // Obtengo el actual directorio de trabajo con el que se inició la shell
    char pwd[MAX_PATH];
    char rel_pwd[MAX_PATH];
    if (getcwd(pwd, sizeof(pwd)) != NULL) {
        strncpy(rel_pwd, pwd+(strlen(hostname)+strlen(username)), MAX_PATH);
        printf(GREEN"mybash>"BLUE"~%s"RESET_COLORS"$ ", rel_pwd); // formato: mybash>pwd
    }

    fflush(stdout);
}

static void sigint_handler(int signum) {
    signal(SIGINT, sigint_handler);
    printf("\n");
    show_prompt();
    fflush(stdout);
}


int main(int argc, char *argv[]) {
    signal(SIGINT, sigint_handler);
    pipeline pipe;
    Parser input;
    bool quit = false;
    
    input = parser_new(stdin);
    while (!quit) {
        show_prompt();
        pipe = parse_pipeline(input);   
        quit = parser_at_eof(input);
        
        if (pipe != NULL) {
            execute_pipeline(pipe);
        }
    }
    parser_destroy(input); input = NULL;
    return EXIT_SUCCESS;
}

