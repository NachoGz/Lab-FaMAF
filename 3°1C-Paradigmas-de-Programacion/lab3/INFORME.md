# Informe Laboratorio 3: Computaci��n Distribuida con Apache Spark y Java
author: Alvarez Ernesto, Frattini Nicolas, Gomez Ignacio

Se puede ver la consigna del laboratorio en https://docs.google.com/document/d/e/2PACX-1vQn5BpCPQ6jKMN-sz46261Qot82KbDZ1RUx8jNzAN4kBEAq_i97T3R6ZA0_yRA5elN66e-EArXQXuAh/pub

## Pasos a seguir para ejecutar el laboratorio
- [ ] Instalar [maven](https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html) Es probable que sea un paquete de tu distribuci��n (`$ sudo apt install maven` si est��s en Ubuntu, Debian o derivados).
- [ ] Descargar [spark 3.5.1](https://www.apache.org/dyn/closer.lua/spark/spark-3.5.1/spark-3.5.1-bin-hadoop3.tgz) y descomprimirlo en el directorio `DIR`.
- [ ] Definir variable de entorno `export SPARK_HOME=<DIR>` (ah�� `<DIR>` es el directorio donde descomprimieron spark). 

> �9�8 Aclaraci��n
> Nosotros configuramos maven para que pueda ser corrido en otro directorio fuera del `APP_HOME` pero en caso de fallar si ejecutar los binarios de spark con la variable de entorno `SPARK_HOME` desde el directorio del laboratorio.

## C��mo compilarlo

Sea `APP_HOME` el directorio donde est�� este el laboratorio

```bash
$ cd $APP_HOME
$ mvn install
```
Eso descarga las dependencias que necesitamos y crea el directorio `target` donde queda el jar que nos interesa.
En nuestro caso:
```
$ ls target
classes  generated-sources  lab3-D50-0.1.jar  maven-archiver  maven-status
```
Si se hace alg��n cambio al programa, se recomienda compilar de nuevo con `mvn clean install`.

## C��mo usarlo

En el directorio `$SPARK_HOME`, siendo n el n��mero de worker threads que quiera utilizar:
```bash
$ ./bin/spark-submit --master local[n] $APP_HOME/target/lab3-D50-0.1.jar <args>
```
Por ejemplo, para computar todos los feeds con la heuristica CapitalWordHeuristic, donde la "big data" ser��an todos los feeds (title + description)
```bash
$ ./bin/spark-submit --master local[n] $APP_HOME/target/lab3-D50-0.1.jar -ne cwh
```

Si no se quiere ver la informaci��n de spark puede redirigir `stderr` a `/dev/null`:
```bash
$ ./bin/spark-submit --master local[n] $APP_HOME/target/lab3-D50-0.1.jar <args> 2>/dev/null
```

En caso de querer computar un archivo de texto externo,
```bash
$ ./bin/spark-submit --master local[n] $APP_HOME/target/lab3-D50-0.1.jar -ne <heuristica> --spark <file> 2>/dev/null
```
Tambi��n se puede ejecutar en modo cluster,
```bash
$ ./bin/spark-submit --master spark://<hostname>:7077 $APP_HOME/target/lab3-D50-0.1.jar -ne <heuristica> --spark <file> 2>/dev/null 
```
Luego para cada worker va a necesitar una shell diferente y ejecutar:
```bash
$ ./bin/spark-class org.apache.spark.deploy.worker.Worker spark://<hostname>:7077 -m <cant-memoria> -c <cant-cores>
```

> �9�5 Observaci��n
> Notar que si usa `zsh`, puede ser que para `local[n]` tenga que ponerlos entre comillas.

## Introducci��n
La programaci��n distribuida es una t��cnica fundamental en la computaci��n moderna que permite distribuir tareas y procesos entre m��ltiples nodos en una red. Este enfoque mejora la eficiencia, escalabilidad y tolerancia a fallos de las aplicaciones. Los frameworks de programaci��n distribuida proporcionan las herramientas y abstracciones necesarias para desarrollar aplicaciones distribuidas de manera m��s sencilla y efectiva. Este informe explora los conceptos clave de la programaci��n distribuida y los frameworks, utilizando el ejemplo de una aplicaci��n en Java que procesa feeds de noticias y computa entidades nombradas. Adem��s, se incluye una secci��n sobre c��mo configurar Apache Spark en un proyecto Maven.
## Conceptos B��sicos de Programaci��n Distribuida

La programaci��n distribuida se basa en la idea de dividir una tarea compleja en sub-tareas m��s peque�0�9as que pueden ser ejecutadas en paralelo en diferentes nodos de una red. Esto permite aprovechar la capacidad de procesamiento de m��ltiples m��quinas para completar tareas de manera m��s r��pida y eficiente. Los componentes clave de la programaci��n distribuida incluyen:

1. **Nodos y Cl��steres**: Los nodos son las unidades individuales de procesamiento en una red. Un cl��ster es un conjunto de nodos que trabajan juntos para ejecutar una aplicaci��n distribuida.
2. **Comunicaci��n entre Nodos**: Los nodos en un sistema distribuido deben comunicarse entre s�� para coordinar la ejecuci��n de tareas. Esto se logra a trav��s de protocolos de comunicaci��n como RPC (Remote Procedure Call) y mensajes.
3. **Coordinaci��n y Sincronizaci��n**: Para asegurar que las tareas se ejecuten de manera ordenada y eficiente, es necesario coordinar y sincronizar las operaciones entre los nodos. Herramientas como Apache Zookeeper pueden ser utilizadas para esta finalidad.
4. **Tolerancia a Fallos**: En un sistema distribuido, es fundamental manejar fallos de nodos individuales sin afectar la operaci��n general del sistema. Esto se logra mediante la replicaci��n de datos y la re-distribuci��n de tareas fallidas a otros nodos.
    
## Frameworks de Programaci��n Distribuida

Los frameworks de programaci��n distribuida proporcionan las herramientas y abstracciones necesarias para desarrollar y gestionar aplicaciones distribuidas. Algunos de los frameworks m��s utilizados incluyen:

1. **Apache Hadoop**: Framework para el procesamiento de grandes conjuntos de datos en cl��steres de computadoras. Utiliza el modelo de programaci��n MapReduce.
2. **Apache Spark**: Framework de procesamiento distribuido que permite realizar operaciones de an��lisis de datos en memoria. Es conocido por su velocidad y capacidad para manejar tanto batch processing como stream processing.
3. **Akka**: Toolkit para la construcci��n de aplicaciones concurrentes, distribuidas y resilientes en la JVM. Utiliza el modelo de actores para manejar la concurrencia y distribuci��n.

## Funcionamiento del programa con Apache Spark
En el programa, las funcionalidades de Spark estan implementadas de forma que se permita el manejo de grandes volumenes de datos pero a la vez intentando mantener el entendimiento del mismo, por eso, algunas facilidades se encuentran en el archivo SparkNamedEntityProcessor, el cual se encarga de gran parte del proceso de estadisticas en la extracci��n de entidades nombradas. Este guarda los candidatos en un RDD para luego filtrar con las heuristicas seg��n el input de usuario, imprimiendo las estadisticas computadas cuando sea llamada. Tambi��n estan implementados los m��todos de conteo e impresi��n de estadisticas por categoria y t��pico optimizados al trabajar parelelamente en el Cluster.
Como SparkNamedEntityProcessor es llamado en el m��todo principal es el responsable de que la aplicaci��n de Apache Spark en el programa sea realmente significativa a la hora de paralelizar las tareas. Adem��s modularizamos `App.java` para que no sea tan larga y ahora el manejo de cualquier flag que tenga que ver con los feeds (como `-f`, `pf`, etc.) lo maneja la nueva clase `FeedManager` pero la flag de las heuristicas se sigue manejando en `App.java`.

## Aplicaci��n Distribuida en Java con Apache Spark
Consideremos nuestro programa en Java que procesa feeds de noticias y computa entidades nombradas. Esta aplicaci��n puede beneficiarse de la programaci��n distribuida utilizando Apache Spark. Para mantener la abstracci��n, creamos una nueva clase `SparkNamedEntityProcessor` dentro del paquete `named_entities.utils` que hereda de `NameEntityProcessor` para reutilizar funciones como `extractNamedEntitiesByCategory` y `extractNamedEntitiesByTopic`. Adem��s implementamos los m��todos est��ticos `countEntitiesByCategory` y `countEntitiesByTopic` que si bien tienen el mismo nombre que en su superclase `NameEntityProcessor` ambos m��todos son est��ticos y tienen distintos tipos de retorno y argumentos (no es *method overriding* sino *method hiding*).
El constructor de la clase toma un objeto `Config` y un string que contiene el nombre de un archivo de texto (pueden ser todos los feeds u otro).

A continuaci��n, se presenta un an��lisis de c��mo se estructura esta aplicaci��n:

El m��todo `processNamedEntities` es el n��cleo del procesamiento de entidades nombradas en el archivo de texto. Este m��todo comienza creando una sesi��n de Spark mediante la llamada a createSparkSession(). Una sesi��n de Spark es esencialmente el punto de entrada principal para interactuar con las capacidades de Spark, proporcionando una interfaz para trabajar con los datos distribuidos.

![Mi Imagen](images/incio.png)

Una vez que la sesi��n de Spark est�� creada, el m��todo procede a instanciar un JavaSparkContext a partir del contexto de Spark de la sesi��n. El JavaSparkContext es una clase que permite a las aplicaciones Java interactuar con Spark y es necesario para realizar operaciones de RDD (Resilient Distributed Dataset).

A continuaci��n, el m��todo lee el archivo de datos especificado en el atributo dataFile y lo carga en un RDD de Spark utilizando el m��todo sc.textFile(dataFile). Este m��todo convierte el archivo de texto en un JavaRDD<String>, donde cada elemento del RDD es una l��nea del archivo.

![Mi Imagen](images/RDDbytext.png)

El siguiente paso es calcular los posibles candidatos a entidades nombradas a partir de las l��neas de texto. Esto se logra llamando al m��todo `computeCandidates(textRDD)`, que aplica una heur��stica configurada para identificar posibles entidades. Este m��todo devuelve un JavaRDD<List<String>>, donde cada elemento es una lista de posibles entidades nombradas extra��das de una l��nea de texto.


![Mi Imagen](images/candidates.png)


El m��todo `computeCandidates` obtiene la heur��stica configurada desde el objeto config usando config.getOptionsValue("-ne"). Dependiendo del valor de esta configuraci��n, se aplica una de las tres heur��sticas disponibles: CapitalWordHeuristic, CapitalVowelsHeuristic o CapitalConsonantsHeuristic. Si la heur��stica configurada no es reconocida, el m��todo imprime un mensaje de error y termina la ejecuci��n.


![Mi Imagen](images/computecandidates.png)


Despu��s de obtener los candidatos a entidades nombradas, el m��todo `processNamedEntities` llama a `computeNamedEntities(candidatesRDD, sc)` para procesar estas listas de candidatos y generar estad��sticas sobre las entidades nombradas. Este procesamiento puede incluir la categorizaci��n y el conteo de entidades, dependiendo de la configuraci��n y las heur��sticas aplicadas.

El m��todo `computeNamedEntities` determina el formato de las estad��sticas a generar bas��ndose en una configuraci��n obtenida con config.getStatsFormatkey(). Si la clave de formato no est�� configurada, se usa "cat" como valor predeterminado. Dependiendo del valor de statsFormatKey, el m��todo realiza diferentes acciones.


![Mi Imagen](images/computednamedentyties.png)


Finalmente, el m��todo `printStatisticsByCategory` imprime las estad��sticas de las entidades nombradas agrupadas por categor��a y `printStatisticsByTopic` imprime las estad��sticas de las entidades nombradas agrupadas por topico, ambos de manera sincronizada para garantizar una salida ordenada y coherente, incluso en entornos distribuidos y paralelos como Spark.


![Mi Imagen](images/printbycategory.png)


![Mi Imagen](images/printbytopyc.png)


## Configuraci��n de Maven para Apache Spark

Maven es una herramienta de gesti��n y comprensi��n de proyectos en Java. Proporciona una forma est��ndar de crear proyectos Java y manejar sus dependencias de manera eficiente. Para configurar un proyecto Maven que utilice Apache Spark, es necesario agregar las dependencias de Spark en el archivo pom.xml.
### Configuraci��n del archivo pom.xml
A continuaci��n se muestra un ejemplo de configuraci��n del archivo pom.xml para un proyecto que utiliza Apache Spark:

![Mi Imagen](images/configuracionpomxml.png)

Este archivo pom.xml configura las dependencias necesarias para utilizar Apache Spark en un proyecto Java. La versi��n de Spark se especifica en las propiedades y se utiliza en las dependencias para spark-core y spark-sql.
Ventajas de Maven

1. **Gesti��n de Dependencias**: Maven permite gestionar las dependencias del proyecto de manera declarativa en el archivo pom.xml. Las dependencias se descargan autom��ticamente desde repositorios centrales.
2. **Estandarizaci��n de Proyectos**: Proporciona una estructura de proyecto est��ndar que facilita la comprensi��n y el mantenimiento del c��digo.
3. **Automatizaci��n del Ciclo de Vida**: Maven automatiza el ciclo de vida del desarrollo, incluyendo la compilaci��n, pruebas y empaquetado del c��digo.
4. **Integraci��n con Herramientas de Construcci��n**: Se integra f��cilmente con herramientas de construcci��n y CI/CD (Integraci��n Continua/Despliegue Continuo).

## Diferencias observadas entre modo cluster y modo local
En modo cluster ejecutamos la aplicaci��n de la siguiente manera:
```bash
./bin/spark-submit --master spark://DESKTOP-DQE8BRN.:7077 $APP_HOME/target/lab3-D50-0.1.jar -ne cwh --spark data/wiki_dump_parcial.txt 2>/dev/null
```
El problema de ejecutar en modo cluster es que el comportamiento del `stdout`es diferente al modo local. En modo cluster, el driver program (el archivo `.jar`) corre en la m��quina del cliente, mientras que los workers corren en distintos nodos en el cluster. Entonces, el output de los `System.out.println` dentro de las transformaciones de RDD, como `foreach`, y acciones ejecutadas en los workers no aparecen en la consola del driver program. No obstante, el output si se logra capturar en los logs de cada worker �� `executer`. Estos se pueden acceder en `http://localhost:4040/executors/`, debajo se puede ver una tabla que dice `Executors` que lista informaci��n sobre cada uno y la columna `logs` se puede acceder al `stdout` de cada executor. Una forma de "traer" un RDD al driver program es con el m��todo `collect()` que carga en memoria el RDD en forma de lista pero si el RDD es muy grande eso puede hacer que el driver program se quede sin memoria. Una alternativa m��s segura de `collect()` es `take(n)` que selecciona n elementos del RDD para cargar en memoria entonces as�� se puede controlar m��s sobre cuantos datos se cargan en memoria. Como eliminamos la categor��a `OTHER`, no tuvimos problemas de memoria tanto con `collect()` como con `take()` con al [archivo de texto](!https://drive.google.com/file/d/195gLyxOYFffCcv30tuD1AB_ZJe7MBQaN/view) que mando el profesor Pagano por Zulip (con 3 workers, 2 cores cada uno) ya que al ser un archivo muy grande la categor��a `OTHER` se llenaba de elementos. 

En el modo cluster, el master contaba con 3 workers, 2 cores cada uno. La duraci��n de la ejecuci��n (obtenida en la web UI de Spark) fue de 7 minutos con el uso de `collect()` y con `take(10)` fue de 6.2 minutos. Sin el uso de `collect()` la duraci��n fue de 6.5 minutos. Adem��s, tanto con `collect()` como con `take()` la muestra de estad��sticas no suele ser correcta como por ejemplo una entidad nombrada aparece m��s de una vez con distinto conteo �� que aparezca m��s de una vez una categor��a con otros conteos y distintas entidades nombradas, algo que se puede ver debajo con **MercadoLibre**:
```bash
Category: ORGANIZATION
        MercadoLibre (2)
        FMI (178)
        Apple (816)
        SMN (21)
        MercadoLibre (3)
        MercadoLibre (1)
        Google (469)
        BocaJuniors (1217)
        SMN (38)
        Google (553)
        Instituto (3602)
        LLA (6)
        SMN (22)
        Talleres (264)
```
Por el otro lado, ejecutamos la aplicaci��n localmente con 6 worker threads de la siguiente manera sin el uso de `collect()` obviamente 
En modo local ejecutamos la aplicaci��n de la siguiente manera:
```bash
./bin/spark-submit --master "local[6]" $APP_HOME/target/lab3-D50-0.1.jar -ne cwh --spark data/wiki_dump_parcial.txt 2>/dev/null
```
y tambi��n la duraci��n fue de aproximadamente 7 minutos. En este caso no sucede el comportamiento err��tico en la muestra de estad��sticas, ver debajo la misma categor��a:
```bash
Category: ORGANIZATION
        Shell (501)
        MercadoLibre (6)
        CorteSuprema (3364)
        Belgrano (2391)
        FMI (594)
        RiverPlate (3252)
        LLA (23)
        Apple (2521)
        BocaJuniors (3988)
        Talleres (916)
        Google (1438)
        Instituto (10636)
        SMN (81)
```
Creemos que el comportamiento err��tico se debe a un problema de sincronizaci��n de outputs entre los workers, mientras que en modo local los problemas de sincronizaci��n los solucionamos con la instrucci��n `synchonized(System.out)`. A��n as�� se puede ver que si, el output del modo cluster, se suma los conteos de cada ocurrencia de **MercadoLibre** se obtiene el mismo conteo que en el output del modo local. 

Cabe aclarar que las medidas de tiempo se realizaron pocas veces en una sola computadora para tener m��s o menos una idea de como afectan (si es que afectan) ciertas transformaciones a los RDD sobre el redimiento del programa. Tendr��amos que realizar una an��lisis m��s exhaustivo para tener una idea clara si existe una relacion entre el rendimiento y la transformaci��n de RDD usada.

## Conclusi��n
La programaci��n distribuida, apoyada por frameworks como Apache Spark, ofrece una soluci��n robusta y escalable para el procesamiento de grandes vol��menes de datos. La modularizaci��n del c��digo y el uso de abstracciones proporcionadas por estos frameworks permiten desarrollar aplicaciones distribuidas de manera m��s eficiente y con menor esfuerzo. Maven, como herramienta de gesti��n de proyectos, simplifica la configuraci��n y gesti��n de dependencias en proyectos Java, facilitando el desarrollo de aplicaciones distribuidas. Sin embargo, es crucial manejar los desaf��os asociados con la complejidad y la tolerancia a fallos para aprovechar plenamente las ventajas de la programaci��n distribuida.
