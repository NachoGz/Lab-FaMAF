## Ejercicio 1
Usando cat /proc/cpuinfo | grep "model name", me dio el siguiente resultado: model name	: Intel(R) Core(TM) i5-9400F CPU @ 2.90GHz

## Ejercicio 2
Usando cat /proc/cpuinfo | grep "model name" | wc -l, me dio: 2

## Ejercicio 3
Usando curl https://www.gutenberg.org/files/11/11-0.txt | sed 's/Alice/Nacho/g' > nacho_in_wonderland.txt, no tengo que eliminar el archivo orignal porque nunca lo descargué.

## Ejercicio 4
cat weather_cordoba.in | sort -nk 5 | head -1 
cat weather_cordoba.in | sort -nk 5 | tail -1 

## Ejercicio 5
cat atpplayers.in | sort -nk 3 

## Ejercicio 6
ip addr | grep ether | awk '{print $2}'

## Ejercicio 7
mkdir succession && cd succession; for episode in {1..10}; do touch "fma_S01E$episode""_es.srt"; done
for file in $(ls); do mv $file $(echo $file | awk -F "_es" '{print $1 $2}') ; done

## Ejercicio opcional
### a)
simplescreenrecorder --start-recording
record-save
quit
ffmpeg -ss 00:00:04 -to 00:00:32 -i simplescreenrecorder-2023-08-20_10.27.10.mp4 -c copy cut_video.mp4
### b)
ffmpeg -i hola.flac -i chau.flac -filter_complex amix=inputs=2:duration=longest merged.flac

