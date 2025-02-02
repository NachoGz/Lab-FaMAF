#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <assert.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>

#include "execute.h"
#include "command.h"
#include "builtin.h"
#include "strextra.h"

#include "tests/syscall_mock.h"


static void execute_scommand(scommand scmd) {
    assert(scmd != NULL);

    char *output_file = scommand_get_redir_out(scmd); // verifico si hay output redirection
    if (output_file != NULL) {
        int output_file_fd = open(output_file, O_CREAT|O_WRONLY|O_TRUNC, S_IWUSR+S_IRUSR+S_IWGRP+S_IRGRP+S_IROTH);
        if (output_file_fd < 0) {
            printf("Error abriendo el output file\n");
            exit(EXIT_FAILURE);
        }
        dup2(output_file_fd, STDOUT_FILENO);
        if (close(output_file_fd) < 0) {
            printf("Error cerrando el file descriptor del input file\n");
            exit(EXIT_FAILURE);
        }
    }
    char *input_file = scommand_get_redir_in(scmd); // verifico si hay input redirection
    if (input_file != NULL) {
        int input_file_fd = open(input_file, O_RDONLY, S_IRUSR);
        if (input_file_fd < 0) {
            printf("Error abriendo el input file");
            exit(EXIT_FAILURE);
        }
        dup2(input_file_fd, STDIN_FILENO);
        if (close(input_file_fd) < 0) {
            printf("Error cerrando el file descriptor del input file\n");
            exit(EXIT_FAILURE);
        }
    }
    char **args = malloc((scommand_length(scmd) + 1) * sizeof(char *));
    char *arg = calloc(scommand_length(scmd), sizeof(char *));
    arg = scommand_to_string(scmd);
    char *token = strtok(arg, " ");
    unsigned int i = 0;
    while (token != NULL) {
        if (*token != '>' && *token != '<') {
            args[i] = token;
            i++;
        }
        token = strtok(NULL, " ");
        
    } 
    args[i] = NULL; 
    
    if (execvp(args[0], args) == -1) {
        fprintf(stderr, "%s: command not found\n", args[0]);
        exit(EXIT_FAILURE);
    }
}


void execute_pipeline(pipeline apipe) {
    assert(apipe != NULL);
    if (!pipeline_is_empty(apipe)) {
        unsigned int pipe_len = pipeline_length(apipe);
        unsigned int num_pipes = 0;
        scommand cmd = NULL;
        int rc;
        int status;
        int bg_pid = getpid();
        if (pipe_len > 0) {
            num_pipes = pipe_len-1;
        }

        int *fds = (int *)malloc(2 * num_pipes * sizeof(int)); // Asignación dinámica para descriptores de archivo

        for (unsigned int i = 0; i < num_pipes; i++) {
            if (pipe(fds + i*2) == -1) {
                fprintf(stderr, "Error abriendo los pipes\n");
                exit(EXIT_FAILURE);
            }
        }
        
        for (unsigned int i = 0; i < pipe_len; i++)
        {   
            cmd = pipeline_front(apipe);
            if (builtin_alone(apipe)) {
                builtin_run(cmd);
                pipeline_pop_front(apipe); 
                break;
            }

            rc = fork();
            if (rc < 0) {
                fprintf(stderr, "Error en el fork\n");
                exit(EXIT_FAILURE);
            }
            else if (rc == 0) { // proceso hijo
                if (i != 0) { // no es primer cmd
                    dup2(fds[(i-1)*2], STDIN_FILENO); 
                }
                if (i != pipe_len-1) { // no es último cmd
                    dup2(fds[i*2+1], STDOUT_FILENO); 
                }
                for (unsigned int i = 0; i < 2*num_pipes; i++) { // cierro todos los pipes en los hijos
                    if (close(fds[i]) < 0) {
                        printf("Error cerrando el pipe número %u\n", i);
                        exit(EXIT_FAILURE);
                    }
                }
                execute_scommand(cmd);  
                exit(EXIT_FAILURE);
                
            }
            pipeline_pop_front(apipe);
            bg_pid = rc; 
        }
        // proceso padre
        for (unsigned int i = 0; i < 2*num_pipes; i++) { // cierro todos los pipes en el padre
            if (close(fds[i]) < 0) {
                fprintf(stderr, "Error cerrando el pipe número %u\n", i);
                exit(EXIT_FAILURE);
            }
        }
        if (pipeline_get_wait(apipe)) {
            for (unsigned int i = 0; i < pipe_len-1; i++) { // para cada hijo, espero.
                wait(NULL);    
            }
            
            waitpid(bg_pid, &status, 0); 
        }
        else {
            fprintf(stdout, "Background: [%d]\n", bg_pid);
        }
    }    
}  
