FROM gcc:latest
COPY . /usr/src/proyecto
WORKDIR /usr/src/proyecto
RUN gcc -Wall -Wextra -O3 -std=c99 -o proyecto proyectoParte1.c
CMD ["sh", "-c", "./proyecto < test_graphs/grafo_grande.txt"]
