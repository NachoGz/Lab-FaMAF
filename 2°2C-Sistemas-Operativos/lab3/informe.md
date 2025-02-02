# INFORME: Round Robin
## **PRIMERA PARTE**

### PREGUNTA 1: ¿Qué política de planificación utiliza xv6-riscv para elegir el próximo proceso a ejecutarse?

En xv6-riscv se utiliza como política de planificación a Round-Robin. Esto se puede ver en el archivo proc.c, en la función scheduler() donde hay bucle infinito que va buscando procesos en el array de procesos (proc[NPROC]) que esten en estado RUNNABLE. Si lo están, son seleccionados para que corran por un tiempo determinado (quantum).

### PREGUNTA 2: ¿Cuánto dura un *quantum* en xv6-riscv?

Un quantum en xv6-riscv dura 1000000 ticks (aproximadamente 100 ms), esto se puede encontrar en la línea 69 del archivo start.c

### PREGUNTA 3: ¿Cuánto dura un cambio de contexto en xv6-riscv?

Los pasos que se deben realizar al hacer un context switch son: 
1. make room to save registers
2. save the registers
3. call the C trap handler in trap.c
4. restore registers
5. return to whatever we were doing in the kernel
Además, el proceso de elegir que proceso va a ejecutarse, es decir el scheduling, forma parte del context switch. Entonces, antes de hacer todos los pasos enumerados previamente, la función scheduler() recorre infinitamente la proc-table en busca de algún proceso que cumpla con los criterios de la política de scheduling que el S.O utilice (en caso de xv6, Round Robin) y lo elige para correr (esto se puede ver en la línea 471 donde se hace c->proc = p donde c es el struct cpu y p es el proceso elegido). Luego, llama a la función swtch(&c->context, &p->context), donde *context* es el campo que contiene los registros tanto para el struct cpu como el struct proc. En el archivo swtch.S, muestra al prototipo de swtch() como "void swtch(struct context *old, struct context *new);". Osea que lo que hace swtch() en este caso es cargar los registros del proceso recién elegido al cpu para que el proceso pueda correr.
Una forma de intentar de aproximar que tomamos fue disminuir el quantum progresivamente, al principio dividiendo por 10, para ver si esto influía en como se comportaba xv6. Llegamos al número límite 305 ticks, donde si bajaba incluso a 304 qemu se quedaba clavado en la última línea de compilación previa que xv6 corra y diga *"xv6 kernel is booting"*. Nosotros creemos que como el quantum es menor o igual 304 ticks y no se pudo ejecutar la shell (proceso "sh"), entonces el context switch esta utilizando el quantum en su totalidad y no deja tiempo para que el proceso que fue elegido para ejecutarse se ejecute. Por lo tanto, estimamos que el tiempo que dura un context switch es menor ó igual a 304 ticks (0.0304 ms ó 30.4 nanosegundos)

### PREGUNTA 4: ¿El cambio de contexto consume tiempo de un *quantum*?

Gracias a la pregunta 3, podemos decir que efectivamente el context switch si usa tiempo del quantum. De ser lo contrario, sería extraño que con un quantum de 304 ticks no se pueda ejecutar ni si quiera la shell.

### PREGUNTA 5: ¿Hay alguna forma de que a un proceso se le asigne menos tiempo?

En la funcion sys_sleep de sysproc.c tenemos while(ticks - ticks0 < n), si se achicase el valor de n tendriamos menos tiempo por proceso.

### PREGUNTA 6: ¿Cúales son los estados en los que un proceso pueden permanecer en xv6-riscv y que los hace cambiar de estado?

Los estados que en los que un proceso pueden permanecer son: UNUSED, USED, SLEEPING, RUNNABLE, RUNNING, ZOMBIE
Para cada uno de estos, las acciones que los hace cambiar de estado son:
* __UNUSED__: Cuando se ubican los procesos en la proc table, es decir cuando recien se crean y cuando se libera uno también
* __USED__: En la funcion allocproc() de proc.c tenemos que busca los procesos UNUSED y les hace allocpid() para poder usarlos.
* __RUNNABLE__: Cuando se configura el primer proceso de usuario, cuando se realiza la funcion fork que haria que el proceso este listo para ejecutarse. Tambien cuando se cede la cpu. 
* __ZOMBIE__: Cuando se sale de un proceso este queda en modo zombie hasta que el padre llame wait() 
* __RUNNING__: Cuando se ejecuta el scheduler este busca los procesos con estado RUNABLE y lo pasa a este 
* __SLEEPING__: Se usa cuando se llama a la funcion sleep, que suspenderia la actividad de ese proceso hasta que se llame wakeup(), que lo pondria en RUNNABLE

## **SEGUNDA PARTE**

## **Quantum original (1000000)**                     vs      **Quantum 10 veces más chico (100000)**

### CASO 1: $ iobench
<table>
<tr>
<th> Quantum original </th>
<th> Quantum 10 veces más chico </th>
</tr>
<tr>
<td>

```shell
$ iobench
					3: 6912 OPW100T, 6912 OPR100T
					3: 6784 OPW100T, 6784 OPR100T
					3: 6848 OPW100T, 6848 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6976 OPW100T, 6976 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6976 OPW100T, 6976 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6848 OPW100T, 6848 OPR100T
					3: 6848 OPW100T, 6848 OPR100T
					3: 6784 OPW100T, 6784 OPR100T
					3: 6848 OPW100T, 6848 OPR100T
					3: 6784 OPW100T, 6784 OPR100T
					3: 6784 OPW100T, 6784 OPR100T
					3: 6784 OPW100T, 6784 OPR100T

Termino iobench 3: total ops 261120u -->
Promedio de OPW: 6871
Promedio de OPR: 6871

pid: 3 prio: 2 cantselect: 429278 lastexec: 2125
```
</td>
<td>

```shell
$ iobench
					3: 6592 OPW100T, 6592 OPR100T
					3: 6400 OPW100T, 6400 OPR100T
					3: 6528 OPW100T, 6528 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 7040 OPW100T, 7040 OPR100T
					3: 6976 OPW100T, 6976 OPR100T
					3: 7040 OPW100T, 7040 OPR100T
					3: 6976 OPW100T, 6976 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6976 OPW100T, 6976 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 7040 OPW100T, 7040 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 7040 OPW100T, 7040 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 7040 OPW100T, 7040 OPR100T
					3: 6912 OPW100T, 6912 OPR100T
					3: 6848 OPW100T, 6848 OPR100T
					3: 7040 OPW100T, 7040 OPR100T
Termino iobench 3: total ops 262016u -->	
Promedio de OPW: 6895
Promedio de OPR: 6895

pid: 3 prio: 2 cantselect: 431668 lastexec: 21200
```
</td>
</tr>
</table>

#### Comparación

Para ir a la tabla para caracterizar experimentos del **Caso 1** usar este [link](https://docs.google.com/spreadsheets/d/1pEw_8e0mO8ZSU8SgQDTV4f6N5n7r8bIPVn3bLvkw9Ko/edit#gid=0).<br>
Con respecto a los promedios de OPW y OPR no se nota mucho cambio, probablemente porque *iobench* es el único proceso que esta corriendo y no está compitiendo por tener tiempo de CPU con otros procesos. Podemos decir que por las mismas razones no notamos cambios entre cantselect del quantum original y el reducido. El único cambio notable que vemos es el aumento en la varibale lastexec en las mediciones con el quantum reducido. explico porque. Realizamos un iobench con un quantum de 10000 (100 veces más chico que el original) y nos dió lastexec: 196800, osea, más grande todavía. Cabe notar que todos los procesos corrieron el mismo tiempo según un cronometro físico externo a la computadora. Creemos que esta diferencia se debe por como define xv6 a los ticks, esto se puede ver en la función clockintr() en el archivo trap.c . Entonces, cada vez que hay una interrupción por timer aumenta la variable ticks en uno y entonces si hay más interrupciones por timer debido a un menor quantum, la variable ticks va a ser mucho mayor que lo que sería si hubiera menos interrupciones. Se puede ver una claramente que la variable lastexec (y cualquier otra medición que utilice **ticks**) es directamente proporcional al valor del quantum. Esto se va a ver en todas en las mediciones que siguen sin importar que proceso se esta midiendo.


### CASO 2: $ cpubench
<table>
<tr>
<th> Quantum original </th>
<th> Quantum 10 veces más chico </th>
</tr>
<tr>
<td>

```shell
$ cpubench
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 845 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 853 MFLOP100T
6: 845 MFLOP100T
6: 860 MFLOP100T
6: 853 MFLOP100T
Termino cpubench 6: total ops 3221225472u --> 
Promedio de measurments: 852

pid: 6 prio: 2 cantselect: 2106 lastexec: 8735

```
</td>
<td>

```shell
$ cpubench
7: 831 MFLOP100T
7: 831 MFLOP100T
7: 838 MFLOP100T
7: 838 MFLOP100T
7: 838 MFLOP100T
7: 838 MFLOP100T
7: 825 MFLOP100T
7: 838 MFLOP100T
7: 838 MFLOP100T
7: 838 MFLOP100T
7: 831 MFLOP100T
7: 838 MFLOP100T
7: 838 MFLOP100T
7: 831 MFLOP100T
7: 838 MFLOP100T
7: 831 MFLOP100T
Termino cpubench 7: total ops 3221225472u --> 
Promedio de measurments: 835

pid: 7 prio: 2 cantselect: 21207 lastexec: 123782

```
</td>
</tr>
</table>

#### Comparación

Para ir a la tabla para caracterizar experimentos del **Caso 2** usar este [link](https://docs.google.com/spreadsheets/d/1pEw_8e0mO8ZSU8SgQDTV4f6N5n7r8bIPVn3bLvkw9Ko/edit#gid=404281329).<br>
En este caso, los primero que podemos notar es la gran diferencia que entre la cantidad de veces que el proceso **cpubench** fue seleccionado por el scheduler utilizando un el quantum reducido y la cantidad de veces que fue elegido con el quantum original. Con el quantum 10 veces más chico, el scheduler eligió correr **cpubench** 21207 veces mientras que, con el quantum original, el scheduler lo eligió 2106 veces. Osea, **cantselect** fue 10 veces mayor con el quantum reducido. Nosotros creemos que esto se debe a que, como hay más interrupciones por timer (debido al quantum más chico), se eligen más veces cada proceso. Pareciera ser que existe alguna relación de proporcionalidad entre la reducción del quantum y la cantidad de veces seleccionado un proceso. Probamos reducir el quantum a 100 veces más chico y el **cantselect** resultante fue de 100559, osea, no 100 veces el cantselect del quantum original pero por lo menos en el mismo orden.

### CASO 3: $ iobench &; cpubench
<table>
<tr>
<th> Quantum original </th>
<th> Quantum 10 veces más chico </th>
</tr>
<tr>
<td>

```shell
$ iobench &; cpubench
3: 867 MFLOP100T
3: 860 MFLOP100T
3: 860 MFLOP100T
					5: 59 OPW100T, 59 OPR100T
3: 860 MFLOP100T
3: 867 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 860 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 860 MFLOP100T
3: 860 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 867 MFLOP100T
3: 867 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 860 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 860 MFLOP100T
3: 867 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 860 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 867 MFLOP100T
3: 860 MFLOP100T
					5: 33 OPW100T, 33 OPR100T
3: 860 MFLOP100T

Termino cpubench 3: total ops 4227858432u --> 
Promedio de measurments: 862

pid: 3 prio: 2 cantselect: 2111 lastexec: 2185
					5: 33 OPW100T, 33 OPR100T

Termino iobench 5: total ops 1408u -->	

Promedio de OPW: 35
Promedio de OPR: 35

pid: 5 prio: 2 cantselect: 2232 lastexec: 2186
```
</td>
<td>

```shell
$ iobench &; cpubench
3: 789 MFLOP100T
					5: 382 OPW100T, 382 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
					5: 336 OPW100T, 336 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 781 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
3: 789 MFLOP100T
					5: 336 OPW100T, 336 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
					5: 336 OPW100T, 336 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 781 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
					5: 336 OPW100T, 336 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
3: 781 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
					5: 333 OPW100T, 333 OPR100T
3: 789 MFLOP100T
					5: 336 OPW100T, 336 OPR100T
Termino iobench 5: total ops 13184u -->	
Promedio de OPW: 336
Promedio de OPR: 336

pid: 5 prio: 2 cantselect: 21051 lastexec: 22265
Termino cpubench 3: total ops 2415919104u --> 
Promedio de measurments: 787

pid: 3 prio: 2 cantselect: 21135 lastexec: 22345

```
</td>
</tr>
</table>

#### Comparación

Para ir a la tabla para caracterizar experimentos del **Caso 3** usar este [link](https://docs.google.com/spreadsheets/d/1pEw_8e0mO8ZSU8SgQDTV4f6N5n7r8bIPVn3bLvkw9Ko/edit#gid=1910770345).<br>
Al igual que el caso 2, podemos notar una diferencia no tan grande entre el promedio de MFLOP100T con el quantum original y el quantum reducido en 10. Creemos que esto puede ser debido a que el quantum es menor, entonces la cantidad de operaciones que se hacen en 100 ticks es menor. También, notamos un gran salto del promedio de OPW y OPR en el quantum más chico, creemos que esto pasa porque el proceso iobench se selecciona más veces y, al ser un proceso IO, es más probable que no consuma todo el quantum y así ejecuta muchas más operaciones. También como el iobench cede la CPU muy rápido, con un quantum más chico es más probable que tenga su tiempo de CPU antes que con el quantum original, por lo tanto, poder hacer más operaciones en en periodo de los 100 ticks. Además, se pueden ver muchos más prints de MFLOP100T, OPW y OPR. Esto probablemente sea porque hay más context switches debido al quantum reducido. La cantitad de prints de OPW y OPR pasó de 9 en el quantum original a 17 prints, prácticamente el doble y la cantidad de prints de MFLOP100T pasó de 17 en el quantum original a 19.

### CASO 4: $ cpubench &; cpubench
<table>
<tr>
<th> Quantum original </th>
<th> Quantum 10 veces más chico </th>
</tr>
<tr>
<td>

```shell
$ cpubench &; cpubench
11: 437 MFLOP100T
13: 434 MFLOP100T
11: 906 MFLOP100T
13: 915 MFLOP100T
11: 1059 MFLOP100T
13: 1050 MFLOP100T
11: 1078 MFLOP100T
13: 1059 MFLOP100T
13: 1088 MFLOP100T
11: 1050 MFLOP100T
11: 1088 MFLOP100T
13: 1050 MFLOP100T
11: 1059 MFLOP100T
13: 1078 MFLOP100T
13: 1078 MFLOP100T
11: 1050 MFLOP100T
13: 1068 MFLOP100T
11: 1041 MFLOP100T
13: 1078 MFLOP100T
11: 1059 MFLOP100T
13: 1078 MFLOP100T
11: 1059 MFLOP100T
13: 1059 MFLOP100T
11: 1078 MFLOP100T
13: 1059 MFLOP100T
11: 1078 MFLOP100T
13: 1050 MFLOP100T
11: 1059 MFLOP100T
13: 1059 MFLOP100T
11: 1068 MFLOP100T
13: 1068 MFLOP100T
11: 1068 MFLOP100T
13: 1059 MFLOP100T
11: 1068 MFLOP100T

Termino cpubench 13: total ops 2550136832u --> 
Promedio de measurments: 1019

pid: 13 prio: 2 cantselect: 1052 lastexec: 7892

Termino cpubench 11: total ops 2550136832u --> 
Promedio de measurments: 1017

pid: 11 prio: 2 cantselect: 1055 lastexec: 7894
```
</td>
<td>

```shell
$ cpubench &; cpubench
3: 853 MFLOP100T
5: 853 MFLOP100T
3: 1032 MFLOP100T
5: 1032 MFLOP100T
5: 1023 MFLOP100T
3: 1023 MFLOP100T
5: 1041 MFLOP100T
3: 1023 MFLOP100T
5: 1032 MFLOP100T
3: 1032 MFLOP100T
5: 1032 MFLOP100T
3: 1032 MFLOP100T
5: 1032 MFLOP100T
3: 1032 MFLOP100T
5: 1032 MFLOP100T
3: 1023 MFLOP100T
5: 1032 MFLOP100T
3: 1032 MFLOP100T
5: 1041 MFLOP100T
3: 1032 MFLOP100T
5: 1041 MFLOP100T
3: 1023 MFLOP100T
5: 1023 MFLOP100T
3: 1032 MFLOP100T
5: 1041 MFLOP100T
3: 1023 MFLOP100T
5: 1032 MFLOP100T
3: 1032 MFLOP100T
5: 1032 MFLOP100T
3: 1032 MFLOP100T
5: 1032 MFLOP100T
3: 1023 MFLOP100T
5: 1032 MFLOP100T
3: 1023 MFLOP100T

Termino cpubench 5: total ops 3154116608u --> 
Promedio de measurments: 1022

pid: 5 prio: 2 cantselect: 10529 lastexec: 21689

Termino cpubench 3: total ops 3154116608u --> 
Promedio de measurments: 1017

pid: 3 prio: 2 cantselect: 10574 lastexec: 21736
```
</td>
</tr>
</table>

#### Comparación

Para ir a la tabla para caracterizar experimentos del **Caso 4** usar este [link](https://docs.google.com/spreadsheets/d/1pEw_8e0mO8ZSU8SgQDTV4f6N5n7r8bIPVn3bLvkw9Ko/edit#gid=240014969).<br>
En este caso, podemos ver que no hay una diferencia significante entre el promedio de MFLOPS100T entre el quantum original y el quantum reducido. Además, se puede ver que tanto con el quantum original como el quantum reducido, hay prácticamente el mismo promedio de MFLOPS100T de cada proceso cpubench paralelo. También, podemos observar que con el quantum reducido hay un print de MFLOP100T menos que con el quantum original. Pensamos que esto ocurre porque al haber un quantum más chico, hay más interrupciones y context switches. Puede ser que al haber más interrupciones, a veces se interrumpa justo antes de que se cumpla el límite de 100 ticks antes de printear el mensaje. Sin embargo, esa diferencia no siempre está, notamos que muchas veces tienen la misma cantidad prints. También se puede ver que hay una diferencia entre el total ops entre quantum original y quantum reducido, teniendo más ops el caso con el quantum reducido pero siguen siendo del mismo orden. La cantidad de veces que veces que el scheduler eligió al proceso es prácticamente 10 veces mayor con quantum reducido que con el quantum original. Como ya remarcamos en el caso 2, esto seguramente se deba a que hay más cambios de contexto provocado por el quantum reducido y, por lo tanto, se eligen más veces los procesos.

### CASO 5: $ cpubench &; cpubench &; iobench
<table>
<tr>
<th> Quantum original </th>
<th> Quantum 10 veces más chico </th>
</tr>
<tr>
<td>

```shell
$ cpubench &; cpubench &; iobench
23: 1068 MFLOP100T
21: 1041 MFLOP100T
23: 1068 MFLOP100T
21: 1059 MFLOP100T
21: 1068 MFLOP100T
23: 1050 MFLOP100T
23: 1059 MFLOP100T
21: 1050 MFLOP100T
23: 1059 MFLOP100T
21: 1059 MFLOP100T
23: 1068 MFLOP100T
21: 1041 MFLOP100T
23: 1078 MFLOP100T
21: 1050 MFLOP100T
					19: 30 OPW100T, 30 OPR100T
23: 1059 MFLOP100T
21: 1059 MFLOP100T
23: 1050 MFLOP100T
21: 1059 MFLOP100T
23: 1059 MFLOP100T
21: 1068 MFLOP100T
					19: 16 OPW100T, 16 OPR100T
23: 1059 MFLOP100T
21: 1041 MFLOP100T
23: 1068 MFLOP100T
21: 1050 MFLOP100T
23: 1059 MFLOP100T
21: 1068 MFLOP100T
					19: 16 OPW100T, 16 OPR100T
23: 1068 MFLOP100T
21: 1041 MFLOP100T
23: 1068 MFLOP100T
21: 1059 MFLOP100T
23: 1059 MFLOP100T
21: 1050 MFLOP100T
23: 1068 MFLOP100T
21: 1059 MFLOP100T
					19: 16 OPW100T, 16 OPR100T

Termino cpubench 21: total ops 3355443200u --> 
Promedio de measurments: 1054

pid: 21 prio: 2 cantselect: 1053 lastexec: 13684

Termino cpubench 23: total ops 3355443200u --> 
Promedio de measurments: 1062

pid: 23 prio: 2 cantselect: 1053 lastexec: 13685
					19: 16 OPW100T, 16 OPR100T

Termino iobench 19: total ops 768u -->	
Promedio de OPW: 18
Promedio de OPR: 18

pid: 19 prio: 2 cantselect: 1226 lastexec: 13686
```
</td>
<td>

```shell
$ cpubench &; cpubench &; iobench
17: 1006 MFLOP100T
15: 1015 MFLOP100T
					13: 215 OPW100T, 215 OPR100T
17: 1032 MFLOP100T
15: 967 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
15: 977 MFLOP100T
17: 1032 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
15: 986 MFLOP100T
17: 1006 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
15: 1006 MFLOP100T
17: 1023 MFLOP100T
					13: 168 OPW100T, 168 OPR100T
15: 958 MFLOP100T
17: 1041 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
15: 1006 MFLOP100T
17: 1032 MFLOP100T
15: 949 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 1032 MFLOP100T
15: 996 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 1006 MFLOP100T
15: 996 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 1023 MFLOP100T
15: 977 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 1050 MFLOP100T
15: 977 MFLOP100T
					13: 168 OPW100T, 168 OPR100T
17: 1006 MFLOP100T
15: 986 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 1059 MFLOP100T
15: 967 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
15: 986 MFLOP100T
17: 1015 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 1006 MFLOP100T
15: 998 MFLOP100T
					13: 168 OPW100T, 168 OPR100T
15: 967 MFLOP100T
17: 1059 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 996 MFLOP100T
15: 998 MFLOP100T
					13: 166 OPW100T, 166 OPR100T
17: 1059 MFLOP100T

Termino cpubench 15: total ops 1543503872u --> 
Promedio de measurments: 984

pid: 15 prio: 2 cantselect: 10510 lastexec: 73275

Termino cpubench 17: total ops 3355443200u --> 
Promedio de measurments: 1026

pid: 17 prio: 2 cantselect: 10564 lastexec: 73330

Termino iobench 13: total ops 6656u -->	
Promedio de OPW: 169
Promedio de OPR: 169

pid: 13 prio: 2 cantselect: 10649 lastexec: 73337
```
</td>
</tr>
</table>

#### Comparación

Para ir a la tabla para caracterizar experimentos del **Caso 5** usar este [link](https://docs.google.com/spreadsheets/d/1pEw_8e0mO8ZSU8SgQDTV4f6N5n7r8bIPVn3bLvkw9Ko/edit#gid=1608793782).<br>
Nuevamente, podemos ver que no hay mucha diferencia entre el promedio de MFLOPS100T entre las mediciones con distinto valor del quantum. Sin embargo, hay una diferencia notable entre el promedio de OPW y OPR entre los distintos quantums. Con el quantum reducido hay un gran salto, paso de tener un promedio de 18 para OPW y OPR con el quantum original a un promedio de 169 OPW y OPR. Creemos que esto pasa porque al haber un quantum más chico, hay interrupciones por timer, osea cada proceso se ejecuta menos tiempo antes de un timer interrupt. Al pasar esto, el scheduler elije más procesos y entre esos se encuentra el iobench que rara vez consume todo el quantum. En conclusión, como el iobench es seleccionado muchas más veces por el scheduler (cantselect: 10649) con el quantum 10 veces más chico que con el quantum original (cantselect: 1226), se realizan más operaciones de escritura y escritura. Como se puede ver, la cantidad de veces que los procesos fueron seleccionados por el scheduler con el quantum reducido es casi 10 veces mayor a la cantidad de veces seleccionados con el quantum original. Cabe aclarar que probablemente la razón por la que el promedio de OPW y OPR haya sido de 169 en este caso y, 336 en el caso 3 sea porque en este caso hay 3 procesos en paralelo (dos de ellos cpu-intensive) mientras que en el caso 3 solo hay dos procesos paralelos.