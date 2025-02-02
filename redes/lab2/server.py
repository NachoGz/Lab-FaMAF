#!/usr/bin/env python
# encoding: utf-8
# Revisión 2019 (a Python 3 y base64): Pablo Ventura
# Revisión 2014 Carlos Bederián
# Revisión 2011 Nicolás Wolovick
# Copyright 2008-2010 Natalia Bidart y Daniel Moisset
# $Id: server.py 656 2013-03-18 23:49:11Z bc $

import optparse
import socket
# import connection
from connection import *
from constants import *
import sys
from threading import Thread
import threading

class Server(object):
    """
    El servidor, que crea y atiende el socket en la dirección y puerto
    especificados donde se reciben nuevas conexiones de clientes.
    """

    def __init__(self, addr=DEFAULT_ADDR, port=DEFAULT_PORT,
                 directory=DEFAULT_DIR):
        print("Serving %s on %s:%s." % (directory, addr, port))
        self.directory = directory
        # Crea y enlaza el socket en dirección y puerto.
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        while True:
            try:
                self.sock.bind((addr, port))
                break
            except socket.error as err:
                if err.args[0] != 98:
                    raise err
                    break
                print("Port %s already in use" % port)
                port += 1

    def serve(self):
        """
        Loop principal del servidor. Se acepta una conexión a la vez
        y se espera a que concluya antes de seguir.
        """
        self.sock.listen()
        while True:
            try:
                conn, conn_addr = self.sock.accept()
                print(f"Connected by: {conn_addr}")
                t = Thread(target=self.user_connection, args=(conn, conn_addr))
                t.setDaemon(True)
                t.start()
                print(f"Clientes conectados: {threading.active_count()-1}")
            except socket.error:
                self.sock.close()
                break
            except KeyboardInterrupt:
                self.sock.close()
                break

    def user_connection(self, *args):
        connection, addr = args
        new_connection = Connection(connection, self.directory)
        new_connection.handle()


def main():
    """Parsea los argumentos y lanza el server"""

    # parser permite especificar opciones en la convencional
    # sintaxis GNU/POSIX, y además genera mensajes de uso y ayuda.
    parser = optparse.OptionParser()
    parser.add_option(
        "-p", "--port",
        help="Número de puerto TCP donde escuchar", default=DEFAULT_PORT)
    parser.add_option(
        "-a", "--address",
        help="Dirección donde escuchar", default=DEFAULT_ADDR)
    parser.add_option(
        "-d", "--datadir",
        help="Directorio compartido", default=DEFAULT_DIR)

    # Parsea las opciones de línea de comandos.
    options, args = parser.parse_args()
    if len(args) > 0:
        parser.print_help()
        sys.exit(1)
    try:
        port = int(options.port)
    except ValueError:
        sys.stderr.write(
            "Numero de puerto invalido: %s\n" % repr(options.port))
        parser.print_help()
        sys.exit(1)

    # Inicia el Servidor
    server = Server(options.address, port, options.datadir)
    server.serve()

if __name__ == '__main__':
    main()
