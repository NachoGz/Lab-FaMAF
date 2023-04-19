#include <stdio.h>
#include "../array_helpers.h"
#include "weather_utils.h"

int min_temp(WeatherTable array) {
    int temp;
    int min_temp = array[0][0][0]._min_temp;
    for (unsigned int year = 0u; year < YEARS; ++year)
    {
        for (month_t month = january; month <= december; ++month)
        {
            for (unsigned int day = 0u; day < DAYS; ++day)
            {   
                temp = array[year][month][day]._min_temp;
                if (min_temp > temp) {
                    min_temp = temp;
                }
            }
        }
    }
    return min_temp;
}

void max_temp_año(WeatherTable a, int output[YEARS]) {
    int temp;
    int max_temp = a[0][0][0]._max_temp;
    for (unsigned int year = 0u; year < YEARS; ++year)
    {
        for (month_t month = january; month <= december; ++month)
        {
            for (unsigned int day = 0u; day < DAYS; ++day)
            {   
                temp = a[year][month][day]._max_temp;
                if (max_temp > temp) {
                    max_temp = temp;
                }
            }
        }
        output[year] = max_temp;
    }
}

void max_prec_mes(WeatherTable a, int output[YEARS][MONTHS]) {
    int temp;
    int max_prec = a[0][0][0]._rainfall;
    for (unsigned int year = 0u; year < YEARS; ++year)
    {
        for (month_t month = january; month <= december; ++month)
        {
            for (unsigned int day = 0u; day < DAYS; ++day)
            {   
                temp = a[year][month][day]._max_temp;
                if (max_prec > temp) {
                    max_prec = temp;
                }
            }
            output[year][month] = max_prec;
        }
    }
}


