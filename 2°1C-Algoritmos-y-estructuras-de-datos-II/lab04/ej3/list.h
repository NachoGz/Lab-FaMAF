#ifndef _LIST_H_
#define _LIST_H_

#include <stdbool.h>

/* list as a pointer to structs to guarantee encapsulation */
typedef struct _node_t node_t;
typedef int list_elem;
typedef node_t * list;

/* constructors */

/**
 * @brief Crea una lista vacía. 
*/
list empty(void);

/**
 * @brief Agrega un elemento al comienzo de una lista
 * @param[in] e elemento para agregar a la lista
 * @param[in] l lista a la que voy a agregar el elemento
*/
list addl(list_elem e, list l);


/* operations */
/**
 * @brief Devuelve True si una lista es vacía
 * @param[in] l lista a evaluar
*/
bool is_empty(list l);

/**
 * @brief Devuelve el primer elemento de una lista
 * @param[in] l lista de entrada
 * @details {PRE: !is_empty(l)}
*/
list_elem head(list l);

/**
 * @brief Elimina el primer elemento de una lista
 * @param[in] l lista de entrada
 * @details {PRE: !is_empty(l)}
*/
list tail(list l);

/**
 * @brief Agrega una elemento al final de una lista
 * @param[in] l lista de entra
 * @param[in] e elemento que voy a agregar
*/
list addr(list l, list_elem e);

/**
 * @brief Devuelve la cantidad de elementos que contiene una lista
 * @param[in] l lista de entrada
*/
unsigned int length(list l);

/**
 * @brief Agrega al final de una lista todos los elementos de otra lista en el mismo orden
 * @param[in] l lista a la que agrego los elementos de la segunda lista el último de la primera
 * @param[in] l0 lista que va a ser concatenada a la primer lista
*/
list concat(list l, list l0);

/**
 * @brief Devuelve el n-ésimo elemento de una lista
 * @param[in] l lista de entrada
 * @param[in] n posición del elemento que quiero obtener
 * @details {PRE: length(l) > n}
*/
list_elem list_index(list l, unsigned int n);

/**
 * @brief Conserva solo los primeros n elementos de una lista, eliminando el resto
 * @param[in] l lista de entrada
 * @param[in] n cantidad de elementos a conservar
*/
list take(list l, unsigned int n);

/**
 * @brief Elimina solo los primeros n elementos de una lista, conservando el resto
 * @param[in] l lista de entrada
 * @param[in] n cantidad de elementos a eliminar
*/
list drop(list l, unsigned int n);

/**
 * @brief Copia todos elementos de una lista de entrada en una nueva lista de salida
 * @param[in] l1 lista de entrada a ser copiada
*/
list copy_list(list l1);

/**
 * @brief Libera memoria en caso de que sea necesario
 * @param[in] l lista de entrada a ser destruida
*/
list destroy_list(list l);

#endif