#ifndef _WEATHER_UTILS_H
#define _WEATHER_UTILS_H

#include <stdio.h>
#include "../array_helpers.h"


int min_temp(WeatherTable array);

void max_temp_año(WeatherTable a, int output[YEARS]);

void max_prec_mes(WeatherTable a, int output[YEARS][MONTHS]);

#endif