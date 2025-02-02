# encoding: utf-8
# Revisión 2019 (a Python 3 y base64): Pablo Ventura
# Copyright 2014 Carlos Bederián
# $Id: connection.py 455 2011-05-01 00:32:09Z carlos $

import socket
from constants import *
from base64 import b64encode
import os


class Connection(object):
    """
    Conexión punto a punto entre el servidor y un cliente.
    Se encarga de satisfacer los pedidos del cliente hasta
    que termina la conexión.
    """

    def __init__(self, socket, directory):
        # Inicializa atributos de Connection
        self.socket = socket
        self.directory = directory
        self.connected = True
        self.buffer_in = ''
        self.buffer_out = ''

    def read_line(self):
        msg = self.socket.recv(4096)
        if msg == CTRL_C:
            return msg
        else:
            return msg.decode("ascii")

    def check_bad_eol(self):
        index = self.buffer_in.find('\n')
        if index != -1:
            if self.buffer_in[index - 1] != '\r':
                self.buffer_out = "%s %s" %\
                                    (BAD_EOL,
                                     error_messages[BAD_EOL]) + EOL
                self.send()
                return True
        return False

    def handle(self):
        """Atiende eventos de la conexión hasta que termina."""
        print(self.directory)
        data = None
        while self.connected:
            try:
                data = self.read_line()
            except UnicodeDecodeError:
                self.buffer_out = f"{BAD_REQUEST} " +\
                              f"{error_messages[BAD_REQUEST]}{EOL}"
                self.send()
                data = None
            if data:
                if isinstance(data, bytes) and data.startswith(CTRL_C):
                    self.buffer_out = "CTRL+C Signal"
                    self.quit()
                else:
                    print(data)
                    self.buffer_in += data
                    bad_eol = self.check_bad_eol()
                    if bad_eol:
                        self.connected = False
                    if EOL in self.buffer_in:
                        self.call_handle()

    def call_handle(self):
        command, self.buffer_in = self.buffer_in.split(EOL, 1)
        print(f"COMANDO: {command}")
        try:
            self.handle_command(command.split())
        except Exception as e:
            print(f"Surgio la excepcion: {e}")
            self.buffer_out = f"{INTERNAL_ERROR} " +\
                              f"{error_messages[INTERNAL_ERROR]}{EOL}"
            self.send()
            self.connected = False

    def handle_command(self, command):
        """Determina y ejecuta el comando adecuado basado en la entrada,
           validando la cantidad de argumentos."""
        if len(command) == 0:
            pass
        elif command[0] == "quit":
            if len(command) == 1:
                self.quit()
            else:
                self.buffer_out = f"{INVALID_ARGUMENTS} " +\
                                  f"{error_messages[INVALID_ARGUMENTS]}{EOL}"
                self.send()
        elif command[0] == "get_file_listing" and len(command) == 1:
            self.get_file_listing()
        elif command[0] == "get_metadata":
            if len(command) == 2:
                self.get_metadata(command[1])
            else:
                self.buffer_out = f"{INVALID_ARGUMENTS} " +\
                                  f"{error_messages[INVALID_ARGUMENTS]}{EOL}"
                self.send()
        elif command[0] == "get_slice" and len(command) == 4:
            try:
                start, end = int(command[2]), int(command[3])
                self.get_slice(command[1], start, end)
            except ValueError:
                self.buffer_out = f"{INVALID_ARGUMENTS} " +\
                                  f"{error_messages[INVALID_ARGUMENTS]}{EOL}"
                self.send()
        else:
            self.buffer_out = f"{INVALID_COMMAND} " +\
                              f"{error_messages[INVALID_COMMAND]}{EOL}"
            self.send()

    def send(self):
        while self.buffer_out:
            bytes_sent = self.socket.send(self.buffer_out.encode("ascii"))
            assert bytes_sent > 0
            print(self.buffer_out)
            self.buffer_out = self.buffer_out[bytes_sent:]

    def quit(self):
        response = f"{CODE_OK} OK{EOL}"
        self.socket.sendall(response.encode('ascii'))
        self.connected = False
        self.socket.close()

    def get_file_listing(self):
        self.buffer_out = "%s %s%s" % (CODE_OK,
                                       error_messages[CODE_OK],
                                       EOL)
        self.send()
        try:
            self.buffer_out = '\r\n'.join(os.listdir(self.directory)) +\
                              "\r\n" + EOL
            self.send()
        except:
            self.buffer_out = "%s %s%s" % (FILE_NOT_FOUND,
                                           error_messages[FILE_NOT_FOUND],
                                           EOL)
            self.send()

    def get_metadata(self, filename):
        try:
            msg = os.stat(self.directory + '/' + filename)
            self.buffer_out = "%s %s%s" % (CODE_OK,
                                           error_messages[CODE_OK],
                                           EOL)
            self.send()
            self.buffer_out = "%d" % msg.st_size + EOL
            self.send()
        except:
            self.buffer_out = "%s %s%s" % (FILE_NOT_FOUND,
                                           error_messages[FILE_NOT_FOUND],
                                           EOL)
            self.send()

    def get_slice(self, file, offset, size):
        if offset + size > os.path.getsize(os.path.join(self.directory,
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
