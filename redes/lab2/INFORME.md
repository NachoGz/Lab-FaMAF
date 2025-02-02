# Laboratorio 2: Desarrollo de una aplicación servidor

[link al video 👉🎥](https://drive.google.com/file/d/1olQL5lRAabZsVAI606YpG2zN0G4ZOjai/view?usp=sharing)

## Introducción
Este trabajo tiene como objetivo implementar un programa que realice una comunicación cliente/servidor mediante sockets, desde la perspectiva del servidor. Se utiliza el protocolo HFTP (Homemade File Transfer Protocol) una versión "casera" de FTP creado especialmente para este laboratorio el cual funciona en la capa de aplicación. HFTP hace uso de TCP como protocolo de transporte. El servidor HFTP escucha pedidos en el puerto 19500

## Objetivos
* Aplicar la comunicación cliente/servidor por medio de la programación de sockets,
desde la perspectiva del servidor.
* Familiarizarse con un protocolo de aplicación diseñado en casa.
* Comprender, diseñar e implementar un programa servidor de archivos en Python.

## Como funciona el paradigma cliente/servidor?
El paradigma cliente-servidor es un modelo de la capa de aplicación en el cual existen proveedores de recursos o servicios, llamados servidores, y los demandantes de estos recursos, llamados clientes. Un cliente realiza peticiones a otro programa, el servidor, quien le da respuesta.

El funcionamiento básico del paradigma cliente-servidor implica los siguientes pasos:

1. *Inicio de la solicitud*: un cliente inicia una solicitud enviando una petición al servidor.

2. *Envío de la solicitud al servidor*: usando protocolos, como HTTP para aplicaciones web o TCP/IP para comunicaciones en red, como en nuestro caso.

3. *Procesamiento de la solicitud*: una vez que el servidor recibe la solicitud del cliente, la procesa y determina cómo debe responder. Esto puede implicar acceder a una base de datos, ejecutar una operación específica o recuperar recursos del sistema.

4. *Generación de una respuesta por parte del servidor*: el servidor genera una respuesta que contiene los resultados solicitados o información relevante además de los códigos acordados para cada situación (como en nuestro caso 0 es OK, 100 bad eol. etc ).

5. *Recepción y procesamiento de la respuesta por parte del cliente*: una vez que el cliente recibe la respuesta del servidor, la procesa según sea necesario, haciendo cosas como, decodificar la información o mostrarla por pantalla.

Este proceso de solicitud-respuesta puede repetirse varias veces durante la interacción entre el cliente y el servidor, hasta que ambos acuerden cerrar la conexión.

## ¿En que se diferencian Stream (TCP) y Datagram (UDP)?
Conexión o No conexión:
* Stream (TCP): establece una conexión antes de transferir datos a través del Three-Way Handshake. La conexión proporciona garantías de entrega secuencial, confiable y sin errores.
* Datagram (UDP): no hay establecimiento de conexión previo ni handshake. Esto significa que no hay garantías de entrega secuencial, confiable ni sin errores; los mensajes pueden perderse, duplicarse o llegar en un orden diferente al enviado.
Manejo de los datos:
* Stream (TCP): los datos se transmiten de manera continua y secuencial. Se garantiza que los datos lleguen en el mismo orden en que se enviaron. TCP gestiona automáticamente la segmentación de datos en paquetes más pequeños, su reensamblaje en el orden correcto y la retransmisión en caso de pérdida o corrupción.
* Datagram (UDP): Cada mensaje enviado es tratado como una unidad independiente. No hay garantías de que los mensajes lleguen en el orden correcto, ni de que lleguen en absoluto. UDP no gestiona la segmentación, el ordenamiento ni la retransmisión de los datos; estas
responsabilidades deben ser manejadas por la aplicación si son necesarias
Overhead y eficiencia:
* Stream (TCP): tiende a tener un mayor overhead debido a los mecanismos que emplea para tener establecer y mantener una conexión segura. Sin embargo, esto también significa que TCP es más adecuado para aplicaciones que requieren transferencias de datos fiables y en orden.
* Datagram (UDP): tiene menor overhead ya que no emplea los mecanismos de TCP. Es más adecuado para aplicaciones que priorizan la velocidad y pueden tolerar pérdidas ocasionales de datos, como la transmisión de video en tiempo real o juegos en línea.

## ¿Qué es el protocolo FTP?
FTP (File Transfer Protocol) es un protocolo estándar de la capa de aplicación utilizado para la transferencia de archivos entre un cliente y un servidor en una red de computadoras, como Internet. Fue desarrollado originalmente en la década de 1970 y ha sido ampliamente utilizado.

El protocolo FTP permite a los usuarios transferir archivos de una computadora a otra a través de una red TCP/IP. Funciona en un modelo cliente-servidor, donde un cliente realiza solicitudes de transferencia de archivos a un servidor FTP donde están alojados.

## ¿Que es el base64?
Base64 es un sistema de codificación que permite representar datos binarios de manera legible y transferible a través de medios que pueden manejar solo texto, como documentos HTML o nuestro HFTP. Funciona convirtiendo datos binarios en una cadena de caracteres ASCII de 64 caracteres, de ahí su nombre.
Esta codificación nos permite incorporar a nuestro archivo caracteres especiales como lo son el ‘\r\n’ o que escapen del estándar ascii como lo son las letras con acentos, además de ayudar a fraccionar los datos para transmitirlos modularmente por la red pudiendolos volver a unir con una asegurada integridad.

## Implementación
Para lograr nuestro objetivo, trabajamos a partir de un esqueleto otorgado por la cátedra, en el cual debíamos modificar los módulos 'server.py' y 'connection.py' para realizar la conexión y las tareas de recibir, procesar y enviar respuestas al cliente. Esta implementación consiste en una clase Server que toma como argumentos una dirección IP, un puerto y un directorio. Por default, estos valores son iguales a las constantes DEFAULT_ADDR, DEFAULT_PORT y DEFAULT_DIR (definidas en 'constants.py') respectivamente. Dentro de esta clase están `__init__()` que es la función constructora que deben tener todas las clases. Luego está la función serve() la cual está escuchando a nuevas conexiones, una vez aceptada crea un hilo (lo que permite que haya múltiples usuarios conectados al servidor simultáneamente) para esa conexion llamando como target a user_connection(). Esta última se encarga de crear las conexiones únicas para cada cliente a medida que estos se van conectando, esto lo hace creando un objeto de la clase Connection y llamando al método handle para que el servidor comience a responder pedidos del cliente.

En el archivo 'connection.py' esta definida la clase Connection() que toma como argumento un objeto socket y una string que contenga el directorio donde está montado el servidor. Dentro de esta clase esta nuevamente `__init__()` que, además de inicializar las variables socket y directory a sus respectivos argumentos, inicializa tres variables que son connected (flag que indica si la conexion al cliente sigue), buffer_in y buffer_out que son los buffer que almacenan los datos que entran al servidor y salen del servidor respectivamente.
En este archivo se definen las funciones que van a responder a los comandos get_file_listing, get_metadata, get_slice y quit. Si el comando se realiza con éxito, el servidor devuelve primero un 0 OK\r\n.
### get_file_listing
Este comando no toma argumentos y retorna una lista de los archivos disponibles en el directorio donde está montado el servidor. La función que realizamos para satisfacer ese pedido es:
```python
def get_file_listing(self):
	self.buffer_out = "%s %s %s" % (CODE_OK,
                                	error_messages[CODE_OK],
                                	EOL)
	self.send()
	try:
    	self.buffer_out = '\r\n'.join(os.listdir(self.directory)) +\
                        	"\r\n" + EOL
    	self.send()
	except FileNotFoundError:
    	self.buffer_out = "%s %s %s" % (FILE_NOT_FOUND,
                                    	error_messages[FILE_NOT_FOUND],
                                    	EOL)
    	self.send()
```
```shell
get_file_listing
0 OK
archivo2.txt
archivo1.txt
archivo3.txt
```

### get_metadata FILENAME
Este comando toma el nombre de un archivo como argumento y retorna el tamaño de ese archivo en bytes. La función que realizamos para este comando es:
```python
def get_metadata(self, filename):
	try:
    	aux = os.stat(self.directory + '/' + filename)
    	self.buffer_out = "%s %s %s" % (CODE_OK,
                                    	error_messages[CODE_OK],
                                    	EOL)
    	self.send()
    	self.buffer_out = "%d" % aux.st_size + EOL
    	self.send()
	except FileNotFoundError:
    	self.buffer_out = "%s %s %s" % (FILE_NOT_FOUND,
                                    	error_messages[FILE_NOT_FOUND],
                                    	EOL)
    	self.send()
```
```shell
get_metadata archivo1.txt
0 OK
46
```

### get_slice FILENAME OFFSET SIZE
Este comando toma el nombre de un archivo y dos valores enteros, y retorna el pedazo del archivo desde la posición OFFSET hasta OFFSET+SIZE. La función que realizamos para este comando es:
```python
def get_slice(self, file, offset, size):
	if offset + size >= os.path.getsize(os.path.join(self.directory,
                                                    	file)):
    	self.buffer_out = "%s %s%s" % (BAD_OFFSET,
                                    	error_messages[BAD_OFFSET],
                                    	EOL)
    	self.send()
	else:
    	toread = open(os.path.join(self.directory, file), "rb")
    	toread.seek(offset)
    	res = toread.read(size)
    	toread.close()
    	self.buffer_out = "%s %s%s" % (CODE_OK, error_messages[CODE_OK],
                                    	EOL)
    	self.send()
    	self.buffer_out = "%s%s" % (b64encode(res).decode("ascii"), EOL)
    	self.send()
```
```shell
get_slice archivo1.txt 5 20
0 OK
Y2Fsb3IgcXVlIGhhY2UgaG95LCA=
```

### quit
Este comando ni toma ni retorna ningún valor, simplemente termina la conexión con el servidor. La función implementada para esto es:
```python
def quit(self):
	response = f"{CODE_OK} OK{EOL}"
	self.socket.sendall(response.encode('utf-8'))
	self.connected = False
	self.socket.close()
```
```shell
quit
0 OK
Connection closed by foreign host.
```

Para asegurarnos que el servidor sea robusto y capaz de responder adecuadamente a cada error que suceda, implementamos ciertos chequeos y mecanismos sobre que hacer cuando surjan los siguientes errores. Estos errores y sus códigos de respuestas son:
* 100: se encontró un carácter \n fuera de un terminador de pedido \r\n.
* 101: alguna malformación del pedido impidió procesarlo.
* 199: el servidor tuvo algún fallo interno al intentar procesar el pedido.
* 200: el comando no está en la lista de comandos aceptados.
* 201: la cantidad de argumentos no corresponde o no tienen la forma correcta.
* 202: el pedido se refiere a un archivo inexistente.
* 203: el pedido se refiere a una posición inexistente en un archivo.

Cada código de error que devuelve el servidor viene acompañado con un mensaje de error definido en el diccionario `error_messages` en el archivo `constants.py`.

Luego están las funciones `send()` y `read_line()` que se encargan de mandar datos al cliente y recibir datos del cliente, respectivamente. Finalmente, la funcion `handle()` es la principal funcion que llama el servidor para atender a los pedidos del cliente, y `call_handle()` y `handle_command()` son las principales que reciben los datos del cliente y determinan si es un comando que el servidor puede realizar y si lo es lo ejecuta.

## Errores y dificultades presentadas en el laboratorio
Una las dificultades que se nos presento en este laboratorio fue como manejar el hecho que el cliente mande apriete CTRL + C debido a que esta señal no puedo ser decodificada por la función `decode('ascii')` y salta un error de *UnicodeDecodeError*. Pudimos solucionar este problema verificando si el mensaje que mando el cliente es igual a la constate *CTRL_C* (definida al final de `constants.py`), si lo es no lo decodificamos a ascii y luego hay un bloque de codigo en `handle()` que se ocupa de terminar la conexion con el servidor. Y si no, se decofica el mensaje en ascii y luego este se trata acorde a lo que contenga el mensaje.
Otra dificultad fue hacer que el servidor sea los mas resistente a errores o a hechos no comunes que puedan romper el funcionamiento del mismo. Por suerte contábamos con los tests unitarios que podrían guiarnos para encontrar y solucionar los casos "no felices".

## Conclusión
### En conclusión, en este laboratorio logramos:
* Construir un servidor que utilice HFTP (Home-made File Transfer Protocol) y establecer una comunicación cliente/servidor mediante sockets.
* Que el servidor reciba y responda adecuadamente a los comandos que manda el cliente.
* Que el servidor sea capaz de manejar errores que puedan ocurrir.
* Que el servidor sea capaz de manejar múltiples clientes mediante hilos

Además pudimos notar cierta relación entre este laboratorio y el anterior. Mientras que las APIs, como la desarrollada en el lab01, proporcionan un conjunto de funciones y protocolos para interactuar con un sistema o servicio, los sockets son utilizados para establecer y manejar conexiones de red entre diferentes programas o dispositivos, permitiendo la transferencia de datos a través de esa conexión. De hecho, las APIs se respaldan en los sockets para facilitar la comunicación entre el software y otros sistemas o servicios remotos. Cabe aclarar que ambos servicios corren en la capa de aplicación

## Preguntas
1. ¿Qué estrategias existen para poder implementar este mismo servidor pero con capacidad de atender múltiples clientes simultáneamente?
 Existen distintas estrategias para que este servidor pueda atender múltiples clientes simultáneamente, entre ellas, multi-threading (ya implementada), forking, poll, etc. En multi-threading cada cliente es manejado por un hilo distinto. En cambio, forking el servidor crea un nuevo proceso para cada nuevo cliente entonces en el código debería reemplazar la creación de un hilo en `serve()` por la creación de un proceso. Sin embargo, esta última estrategia puede exigir muchos recursos al servidor si se conectan muchos clientes. Finalmente, en poll el servidor se fija periódicamente si llegaron datos del cliente en vez de esperar a que lleguen. Para esta estrategia se podría utilizar el método `select()` o `poll()`. La diferencia entre estos radica en que `select()` requiere que la aplicación pase un array de bits donde un bit es usado para representar cada un file descriptor (en este caso, del socket) pero este tiene un límite, indicado en el macro *FD_SETSIZE*, de cuantos file descriptors puede monitorear. En cambio, `poll()` toma un array de structs (pollfd) propios de cada file descriptor, donde cada uno ocupa hasta 8 bytes, entonces pueden haber file descriptors con números más grandes.
 2. `localhost` es un hostname que se refiere a la computadora, normalmente se traduce a la dirección 127.0.0.1. Entonces, estas dos direcciones refieren a lo mismo. Sin embargo, la dirección `0.0.0.0` es para representar todas las direcciones IPv4 en la máquina o asociados a esta entonces puede aceptar conexiones de IPs externas a la red. Entonces, usar como dirección a 127.0.0.1 y localhost no hay diferencia en el funcionamiento del servidor. Sin embargo, usar 0.0.0.0 como direccion abre la posibilidad de que computadoras distintas a la del servidor (de las misma red o externa) pueda conectarse y eso expone al servidor a algún agente malicioso o negligente que pueda ocasionar problemas en el servidor, sobre todo en uno donde no se contempló mucho la seguridad durante el diseño e implementación del mismo.
 Abajo hay un ejemplo de cómo puede acceder una máquina de la misma red:
#### Servidor
```shell
nacho@ubuntu:~$ ifconfig
wlp2s0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
    	inet 192.168.0.111  netmask 255.255.255.0  broadcast 192.168.0.255
    	inet6 fe80::168f:b84d:3dc7:c9ba  prefixlen 64  scopeid 0x20<link>
    	ether 40:5b:d8:01:17:f5  txqueuelen 1000  (Ethernet)
    	RX packets 57167  bytes 49277763 (49.2 MB)
    	RX errors 0  dropped 2  overruns 0  frame 0
    	TX packets 37638  bytes 10647217 (10.6 MB)
    	TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

(.venv) nacho@dell:~/Documents/famaf/redes/labs/redes24lab2g13$ python server.py -d servidor/
Serving servidor/ on 0.0.0.0:19500.
Connected by: ('192.168.0.109', 54297)
servidor/
get_file_listing

COMANDO: get_file_listing
```
```shell
(.venv) nacho@dell:~/Documents/famaf/redes/labs/redes24lab2g13$ ifconfig
ens33: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
	inet 192.168.153.128  netmask 255.255.255.0  broadcast 192.168.153.255
	inet6 fe80::495b:5b7e:9dfe:5e91  prefixlen 64  scopeid 0x20<link>
	ether 00:0c:29:d6:d9:f2  txqueuelen 1000  (Ethernet)
	RX packets 5110  bytes 7149466 (7.1 MB)
	RX errors 0  dropped 0  overruns 0  frame 0
	TX packets 1740  bytes 166542 (166.5 KB)
	TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
```
#### Cliente
```shell
nacho@ubuntu:~$ telnet 192.168.0.111 19500
Trying 192.168.0.111...
Connected to 192.168.0.111.
Escape character is '^]'.
get_file_listing
0 OK
archivo2.txt
archivo1.txt
archivo3.txt

quit
0 OK
Connection closed by foreign host.
```