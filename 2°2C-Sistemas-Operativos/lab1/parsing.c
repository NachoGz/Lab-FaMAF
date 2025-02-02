#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

#include "parsing.h"
#include "parser.h"
#include "command.h"

static scommand parse_scommand(Parser p) {
    scommand cmd = scommand_new();
    char *arg;
    arg_kind_t type;

    while (!parser_at_eof(p)) {
        arg = parser_next_argument(p, &type);
        if (type == ARG_NORMAL) {
            if (arg == NULL) {
                free(arg);
                return cmd;
            }
            scommand_push_back(cmd, arg);
            // has_arguments = true; 
        } else if (type == ARG_INPUT) { // <
            if (arg == NULL) {
                printf("Error: Comando inválido. Falta el archivo de entrada.\n");
                scommand_destroy(cmd);
                free(arg);
                return NULL;
            }
            scommand_set_redir_in(cmd, arg);
        } else if (type == ARG_OUTPUT) { // >
            if (arg == NULL) {
                printf("Error: Comando inválido. Falta el archivo de salida.\n");
                scommand_destroy(cmd);
                free(arg);
                return NULL;
            }
            scommand_set_redir_out(cmd, arg);
        } else {
            printf("Error: Comando inválido. Tipo de argumento desconocido.\n");
            scommand_destroy(cmd);
            free(arg);
            return NULL;
        }
    }
    if (scommand_is_empty(cmd)) {
        printf("Error: Comando simple vacío.\n");
        scommand_destroy(cmd);
        return NULL;
    }
    return cmd;
}

pipeline parse_pipeline(Parser p) {
    pipeline result = pipeline_new();
    scommand cmd = NULL;
    bool error = false, another_pipe = true;
    unsigned int aux = 1u;
    while (another_pipe && !error) {
        cmd = parse_scommand(p);
        if (cmd == NULL) {
            error = true;
            break;
        }

        bool is_background = false;
        parser_op_background(p, &is_background);
        
        parser_op_pipe(p, &another_pipe);
        // Verificar si el comando simple está vacío (sin argumentos) y no es el primero.
        if (scommand_is_empty(cmd) && aux != 1u) {
            printf("Error: Error de sintaxis. Comando invalido.\n");
            // scommand_destroy(cmd);
            error = true;
            break;
        }
        aux++;
        pipeline_set_wait(result, !is_background);
        pipeline_push_back(result, cmd);
    }

    /* Tolerancia a espacios posteriores */
    parser_skip_blanks(p);
    /* Consumir todo lo que hay inclusive el \n */
    bool garbage;
    parser_garbage(p, &garbage);

    if (error) {
        // Si hubo un error en el análisis de algún comando, limpiamos la memoria.
        pipeline_destroy(result);
        return NULL;
    }
    return result;
}

