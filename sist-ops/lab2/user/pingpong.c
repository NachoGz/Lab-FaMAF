#include "kernel/types.h"
#include "kernel/stat.h"
#include "user/user.h"
#include "kernel/spinlock.h"

#define MAX_SEM_VALUE 30

int main(int argc, char *argv[]) {
    int rally = atoi(argv[1]);
    int rc;
    int sem_code;

    int PING = 0;
    int PONG = 1;

    if (argv[1][0] == '-' || rally == 0 || argc == 1) {
        printf("ERROR: El número de rounds tiene que ser mayor a 1\n");
    }

    sleep(5);
    sem_code = sem_open(PING,1); // semáforo ping

    while (sem_code == 0) {
        PING++;
        if (PING > MAX_SEM_VALUE) { 
            printf("ERROR: la cantidad máxima de semaforos es 30\n");
            exit(1);
        }
        sem_code = sem_open(PING,1);
    }

    sleep(5);
    sem_code = sem_open(PONG,1); // semáforo pong
    
    while (sem_code == 0) {
        PONG++;
        if (PONG > MAX_SEM_VALUE) { 
            printf("ERROR: la cantidad máxima de semaforos es 30\n");
            exit(1);
        }
        sem_code = sem_open(PONG,1);
    }
    sem_down(PONG);

    rc = fork();
    if (rc < 0) {
        printf("Error haciendo fork\n");
        exit(1);
    }
    else if (rc == 0) { // hijo --> pong
        for (unsigned int i = 0; i < rally; i++) {
            sem_code = sem_down(PONG);
            if (sem_code == 0) {
                printf("ERROR: la cantidad máxima de semaforos es 30\n");
                exit(1);
            }

            printf("    pong\n");

            sem_code = sem_up(PING);
            if (sem_code == 0) {
                printf("ERROR: la cantidad máxima de semaforos es 30\n");
                exit(1);
            }
        }

    }
    else { // padre --> ping
        for (unsigned int i = 0; i < rally; i++) {
            sem_code = sem_down(PING);
            if (sem_code == 0) {
                printf("ERROR: la cantidad máxima de semaforos es 30\n");
                exit(1);
            }

            printf("ping\n");
            
            sem_code = sem_up(PONG);
            if (sem_code == 0) {
                printf("ERROR: la cantidad máxima de semaforos es 30\n");
                exit(1);
            }
        }

        wait(0); // (int *)  ???

        // de esta forma me aseguro que ambos semaforos no esten más en uso al momento de cerrarlos
        sem_code = sem_close(PONG);
        if (sem_code == 0) { 
            printf("ERROR: la cantidad máxima de semaforos es 30\n");
            exit(1);
        }

        sem_code = sem_close(PING);
        if (sem_code == 0) {
            printf("ERROR: la cantidad máxima de semaforos es 30\n");
            exit(1);
        }
    }
    exit(0);
}

