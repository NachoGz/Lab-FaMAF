#include <stdbool.h>
#include <assert.h>
#include <unistd.h> // Para chdir
#include <stdio.h>
#include <string.h> // Para strcmp
#include <stdlib.h>

#include "builtin.h"
#include "command.h"

#include "tests/syscall_mock.h"


bool builtin_is_internal(scommand cmd){
// Comandos internos cd", "help", "exit"
/*
 * Indica si el comando alojado en `cmd` es un comando interno
 *
 * REQUIRES: cmd != NULL
 *
 */
    assert(cmd != NULL);
    if (scommand_is_empty(cmd)){
        return false;
    }
    char * comando = scommand_front(cmd);
    return (strcmp(comando, "cd") == 0 || strcmp(comando, "exit") == 0 || strcmp(comando, "help") == 0);
}

bool builtin_alone(pipeline p){
/*
 * Indica si el pipeline tiene solo un elemento y si este se corresponde a un
 * comando interno.
 *
 * REQUIRES: p != NULL
 *
 * ENSURES:
 *
 * builtin_alone(p) == pipeline_length(p) == 1 &&
 *                     builtin_is_internal(pipeline_front(p))
 *
 *
 */
    assert(p != NULL);
    scommand cmd = pipeline_front(p);
    return (pipeline_length(p) == 1 && builtin_is_internal(cmd));

}

void builtin_run(scommand cmd){
/*
 * Ejecuta un comando interno
 *
 * REQUIRES: {builtin_is_internal(cmd)}
 *
cd: Se implementa de manera directa con la syscall chdir() 
help: Debe mostrar un mensaje por la salida estándar indicando el nombre del shell, el nombre de sus autores y listar los comandos internos implementados con una breve descripción de lo que hace cada uno.
exit: Es conceptualmente el más sencillo pero requiere un poco de planificación para que el shell termine de manera limpia.
 */ 
    assert(builtin_is_internal(cmd));
    char *comando = calloc(scommand_length(cmd), sizeof(char *));
    strcpy(comando, scommand_front(cmd));
    scommand_pop_front(cmd);
    if (strcmp(comando, "cd") == 0){
        if (scommand_length(cmd) == 0) {
            // Si no hay argumento, regresa al directorio de inicio del usuario
            const char *home_dir = getenv("HOME");
            if (home_dir != NULL) {
                int status_code = chdir(home_dir);
                if (status_code != 0) {
                    perror("Error al cambiar al directorio de inicio");
                }
            } else {
                perror("Error: Variable de entorno HOME no está definida");
            }
        } else {
            char *argumento = scommand_front(cmd);
            int status_code = chdir(argumento);
            if (status_code != 0) {
                perror("Error al cambiar de directorio");
            }
        }
    }else if (strcmp(comando, "exit") == 0){
        exit(0);
    } else if (strcmp(comando, "help") == 0) {
        // Mostrar información de ayuda
        printf("MiShell - Un simple shell personalizado\n");
        printf("Autores: \n");
        printf("Comandos internos:\n");
        printf("  cd <directorio>: Cambia de directorio.\n");
        printf("  exit: Sale de la shell.\n");
        printf("  help: Muestra esta ayuda.\n");
    }
    // scommand_destroy(cmd);
}
