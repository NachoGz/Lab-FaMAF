#include <stdlib.h>  /* exit() y EXIT_FAILURE */
#include <stdio.h>   /* printf(), scanf()     */
#include <stdbool.h> /* Tipo bool             */

#include <assert.h>  /* assert() */

// elijo el tamaño del tablero
#define size 4
#define CELL_MAX (size * size - 1)

void print_sep(int length) {
    printf("\t ");
    for (int i=0; i < length;i++) printf("................");
    printf("\n");

}

void print_board(char board[size][size])
{
    int cell = 0;

    print_sep(size);
    for (int row = 0; row < size; ++row) {
        for (int column = 0; column < size; ++column) {
            printf("\t | %d: %c ", cell, board[row][column]);
            ++cell;
        }
        printf("\t | \n");
        print_sep(size);
    }
}
char get_winner(char board[size][size])
{
    char winner = '-';
     // flags que indican si alguna fila, columna, diagonal1 ó diagonal2 tienen elementos iguales
    bool eqFila = true;
    bool eqCol = true;
    // diagonal1 va desde [0][0] a [size-1][size-1]
    bool eqDiag1 = true;
    // diagonal2 va desde [size-1][0] a [0][size-1]
    bool eqDiag2 = true;

    for (int i=0;i<size;i++)
    {   
        for (int j=0; j<size-1; j++)
        {   
            // por fila
            eqFila = eqFila && (board[i][j] == board[i][j+1]);
            // por columna
            eqCol = eqCol && (board[j][i] == board[j+1][i]);
        }
        if (eqFila)
        {
            winner = board[i][0];
            return winner;
        }
        else
        {
            eqFila = false;
        }
        if (eqCol)
        {
            winner = board[0][i];
            return winner;
        }
        else
        {
            eqCol = false;
        }
    }
    // por diagonal
    for (int i=0; i<size-1;i++)
    {
        // por diagonal
        eqDiag1 = eqDiag1 && (board[i][i] == board[i+1][i+1]);
        eqDiag2 = eqDiag2 && (board[size-(i+1)][i] == board[size-(i+2)][i+1]);
    }
    if (eqDiag1)
    {
        winner = board[0][0];
        return winner;
    }
    else
    {
        eqDiag1 = false;
    }
    if (eqDiag2)
    {
        winner = board[size-1][0];
        return winner;
    }
    else
    {
        eqDiag2 = false;
    }
    return winner;
}

bool has_free_cell(char board[size][size])
{
    bool free_cell=false;
    for (int i=0; i<size; ++i)
    {
        for (int j=0;j<size;++j)
        {
            if (board[i][j] == '-')
            {
                free_cell = true;
            }
        }
    }
    return free_cell;
}

int main(void)
{
    printf("TicTacToe [InCoMpLeTo :'(]\n");

    char board[size][size];
    for (int i=0;i<size; i++)
    {
        for (int j=0;j<size; j++)
        {
            board[i][j] = '-';
        }
        
    }
    char turn = 'X';
    char winner = '-';
    int cell = 0;
    
    while (winner == '-' && has_free_cell(board)) {
        print_board(board);
        printf("\nTurno %c - Elija posición (número del 0 al %d): ", turn,
               CELL_MAX);
        int scanf_result = scanf("%d", &cell);
        if (scanf_result <= 0) {
            printf("Error al leer un número desde teclado\n");
            exit(EXIT_FAILURE);
        }
        if (cell >= 0 && cell <= CELL_MAX) {
            int row = cell / size;
            int colum = cell % size;
            if (board[row][colum] == '-') {
                board[row][colum] = turn;
                turn = turn == 'X' ? 'O' : 'X';
                winner = get_winner(board);
            } else {
                printf("\nCelda ocupada!\n");
            }
        } else {
            printf("\nCelda inválida!\n");
        }
    }
    print_board(board);
    if (winner == '-') {
        printf("Empate!\n");
    } else {
        printf("Ganó %c\n", winner);
    }
    return 0;
}
