// gcc -Wall -Werror -Wextra -pedantic -std=c99 -c array_helpers.c weather.c main.c
// gcc -Wall -Werror -Wextra -pedantic -std=c99 array_helpers.o weather.o main.o -o weather

// PARTE B:
// gcc -Wall -Werror -Wextra -pedantic -std=c99 -c weather_utils/weather_utils.c array_helpers.c weather.c main.c
// gcc -Wall -Werror -Wextra -pedantic -std=c99 weather_utils.o array_helpers.o weather.o main.o -o weather


/*
  @file main.c
  @brief Defines main program function
*/

/* First, the standard lib includes, alphabetically ordered */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/* Then, this project's includes, alphabetically ordered */
#include "array_helpers.h"
#include "weather_utils/weather_utils.h"

/**
 * @brief print usage help
 * @param[in] program_name Executable name
 */
void print_help(char *program_name) {
    /* Print the usage help of this program. */
    printf("Usage: %s <input file path>\n\n"
           "Load climate data from a given file in disk.\n"
           "\n"
           "The input file must exist in disk and every line in it must have the following format:\n\n"
           "<year> <month> <day> <temperature> <high> <low> <pressure> <moisture> <precipitations>\n\n"
           "Those elements must be integers and will be copied into the multidimensional integer array 'a'.\n"
           "The dimensions of the array are given by the macro tclimate.\n"
           "\n\n",
           program_name);
}

/**
 * @brief reads file path from command line
 *
 * @param[in] argc amount of command line arguments
 * @param[in] argv command line arguments
 *
 * @return An string containing read filepath
 */
char *parse_filepath(int argc, char *argv[]) {
    /* Parse the filepath given by command line argument. */
    char *result = NULL;

    if (argc < 2) {
        print_help(argv[0]);
        exit(EXIT_FAILURE);
    }

    result = argv[1];

    return (result);
}

/**
 * @brief Main program function
 *
 * @param[in] argc amount of command line arguments
 * @param[in] argv command line arguments
 *
 * @return EXIT_SUCCESS when programs executes correctly, EXIT_FAILURE otherwise
 */
int main(int argc, char *argv[]) {
    char *filepath = NULL;

    /* parse the filepath given in command line arguments */
    filepath = parse_filepath(argc, argv);

    /* create an array with the type of tclimate */
    WeatherTable array;

    /* parse the file to fill the array and obtain the actual length */
    array_from_file(array, filepath);

    /* show the ordered array in the screen */
    array_dump(array);

    /* menor temperatura mínima histórica registrada en la ciudad de Córdoba*/
    int min = min_temp(array);
    printf("La menor temperatura mínima histórica registrada en la ciudad de Córdoba: %d\n", min);

    /* Arreglo que registra para cada año entre 1980 y 2016 la mayor temperatura máxima registrada durante ese año */
    int maximas_temps[YEARS];
    max_temp_año(array, maximas_temps);
    for (int year=0; year<YEARS; year++) {
        printf("La mayor temperatura máxima del año %d es: %d\n", (1980 - year)*-1, maximas_temps[year]);
    }

    /* Arreglo que registra para cada año entre 1980 y 2016 el mes de ese año en que se registró la mayor cantidad mensual de precipitaciones. */

    int maximas_prec[YEARS][MONTHS];
    max_prec_mes(array, maximas_prec);
    for (int year=0; year<YEARS; year++) {
        for (month_t month = january; month <= december; ++month) {
            printf("La mayor temperatura máxima del año %d y mes %d es: %d\n", (1980 - year)*-1, month, maximas_temps[year][month]);
        }   
    }
    return (EXIT_SUCCESS);
}
