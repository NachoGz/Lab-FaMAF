#include "array_helpers.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

unsigned int length = array_from_file(array, MAX_SIZE, filepath);
/*dumping the array*/
array_dump(array, length);