#include <stdlib.h>



/**
 * @brief Calcula la longitud una cadena
 * @param[in] str puntero que apunta a una cadena
*/
size_t string_length(const char *str);


/**
 * @brief Devuelve una nueva cadena(en memoria dinámica) que contiene aquellos caracteres de la cadena de input que son
 * distintos a un caracter que paso como parámetro
 * @param[in] str punter que apunta a una cadena
 * @param[in] c caracter
*/
char *string_filter(const char *str, char c);

