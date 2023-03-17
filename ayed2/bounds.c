#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#define ARRAY_SIZE 4

struct bound_data {
    bool is_upperbound;
    bool is_lowerbound;
    bool exists;
    unsigned int where;
};

struct bound_data check_bound(int value, int arr[], unsigned int length) {
    struct bound_data res;
    res.is_upperbound = true;
    res.is_lowerbound = true;
    res.exists = false;
    for (unsigned int i=0; i<length; ++i)
    {
        if (value >= arr[i])
        {
            res.is_lowerbound = false;
        }
        else
        {
            if (value <= arr[i])
            {
                res.is_upperbound = false;
            }
        }
        if (value == arr[i])
        {
            res.exists = true;
            res.where = i;
        }
    };
    return res;
}

int main(void) {
    int a[ARRAY_SIZE];
    int value;
    int i=0;
    while (i < ARRAY_SIZE){
        printf("Ingrese el valor del arreglo en la posición %d: \n", i);
        scanf("%d", &a[i]);
        i = i + 1;
    }
    printf("Ingrese el valor a verificar:\n");
    scanf("%d", &value);

    struct bound_data result = check_bound(value, a, ARRAY_SIZE);


    printf("¿Es cota superior? %d\n", result.is_upperbound); 
    printf("¿Es cota inferior? %d\n", result.is_lowerbound); 
    printf("¿El valor se encuentra en el array? %u\n", result.exists); 
    if (result.exists == true)
    {
        printf("El valor se encuentra en la posición: %u\n", result.where);  
    }
    

    return EXIT_SUCCESS;
}

