import random
from flask import Flask, jsonify, request
from proximo_feriado import NextHoliday
from unidecode import unidecode


app = Flask(__name__)
peliculas = [
    {'id': 1, 'titulo': 'Indiana Jones', 'genero': 'Acción'},
    {'id': 2, 'titulo': 'Star Wars', 'genero': 'Acción'},
    {'id': 3, 'titulo': 'Interstellar', 'genero': 'Ciencia ficción'},
    {'id': 4, 'titulo': 'Jurassic Park', 'genero': 'Aventura'},
    {'id': 5, 'titulo': 'The Avengers', 'genero': 'Acción'},
    {'id': 6, 'titulo': 'Back to the Future', 'genero': 'Ciencia ficción'},
    {'id': 7, 'titulo': 'The Lord of the Rings', 'genero': 'Fantasía'},
    {'id': 8, 'titulo': 'The Dark Knight', 'genero': 'Acción'},
    {'id': 9, 'titulo': 'Inception', 'genero': 'Ciencia ficción'},
    {'id': 10, 'titulo': 'The Shawshank Redemption', 'genero': 'Drama'},
    {'id': 11, 'titulo': 'Pulp Fiction', 'genero': 'Crimen'},
    {'id': 12, 'titulo': 'Fight Club', 'genero': 'Drama'}
]


@app.route('/peliculas', methods=['GET'])
def obtener_peliculas():
    # Función para obtener todas las películas
    return jsonify(peliculas)


@app.route('/peliculas/<int:id_pelicula>', methods=['GET'])
def obtener_pelicula(id_pelicula):
    # Función para obtener una película específica
    pelicula = next((p for p in peliculas if p['id'] == id_pelicula), None)
    if pelicula:
        return jsonify(pelicula), 200
    return jsonify({'mensaje': 'Película no encontrada'}), 404


@app.route('/peliculas', methods=['POST'])
def agregar_pelicula():
    # Función para agregar películas
    nueva_pelicula = {
        'id': obtener_nuevo_id(),
        'titulo': request.json['titulo'],
        'genero': request.json['genero']
    }
    peliculas.append(nueva_pelicula)
    print(peliculas)
    return jsonify(nueva_pelicula), 201


@app.route('/peliculas/<int:id_pelicula>', methods=['PUT'])
def actualizar_pelicula(id_pelicula):
    # Función para actualizar los datos de una película
    pelicula = next((p for p in peliculas if p['id'] == id_pelicula), None)
    if pelicula:
        data = request.get_json()
        pelicula.update(data)
        return jsonify(pelicula), 200
    return jsonify({'mensaje': 'Película no encontrada'}), 404


@app.route('/peliculas/<int:id_pelicula>', methods=['DELETE'])
def eliminar_pelicula(id_pelicula):
    # Función para eliminar los datos de una película
    pelicula = next((p for p in peliculas if p['id'] == id_pelicula), None)
    if pelicula:
        peliculas.remove(pelicula)
        return jsonify({'mensaje': 'Película eliminada'}), 200
    return jsonify({'mensaje': 'Película no encontrada'}), 404


@app.route('/peliculas/genero/<string:genero>', methods=['GET'])
def peliculas_por_genero(genero):
    # Función para obtener las película de un genero específico
    filtradas = [p for p in peliculas if unidecode(p['genero']).lower() == unidecode(genero).lower()]
    return jsonify(filtradas), 200


@app.route('/peliculas/buscar/<string:query>', methods=['GET'])
def buscar_peliculas(query):
    # Función para obtener aquellas películas que contengan el substring query
    resultado = [p for p in peliculas if query.lower() in p['titulo'].lower()]
    return jsonify(resultado), 200


@app.route('/peliculas/sugerencia', methods=['GET'])
def sugerir_pelicula():
    # Función para sugerir una película aleatoria
    pelicula = random.choice(peliculas)
    return jsonify(pelicula), 200


@app.route('/peliculas/sugerencia/<string:genero>', methods=['GET'])
def sugerir_pelicula_por_genero(genero):
    # Función para sugerir una película aleatoria de un genero específico
    peliculas_genero = [p for p in peliculas if p['genero'].lower() == genero.lower()]
    if peliculas_genero:
        pelicula = random.choice(peliculas_genero)
        return jsonify(pelicula), 200
    return jsonify({'mensaje': 'No se encontraron películas del género solicitado'}), 404


@app.route('/recomendar_pelicula_feriado/<string:genero>', methods=['GET'])
def recomendar_pelicula_feriado(genero):
    # Función para sugerir una película aleatoria de un genero específico para un él próximo feriado
    next_holiday = NextHoliday()
    next_holiday.fetch_holidays()
    if not next_holiday.holiday:
        return jsonify({'mensaje': 'No se encontró el próximo feriado'}), 404
    peliculas_genero = [p for p in peliculas if p['genero'].lower() == genero.lower()]
    if not peliculas_genero:
        return jsonify({'mensaje': 'No se encontraron películas del género solicitado'}), 404
    pelicula_recomendada = random.choice(peliculas_genero)
    respuesta = {
        'proximo_feriado': {
            'fecha': f"{next_holiday.holiday['dia']} del {next_holiday.holiday['mes']}",
            'motivo': next_holiday.holiday['motivo'],
            'tipo': next_holiday.holiday['tipo']
        },
        'pelicula_recomendada': {
            'id': pelicula_recomendada['id'],
            'titulo': pelicula_recomendada['titulo'],
            'genero': pelicula_recomendada['genero']
        }
    }

    return jsonify(respuesta), 200


def obtener_nuevo_id():
    # Función para obtener un nuevo id único
    if len(peliculas) > 0:
        ultimo_id = peliculas[-1]['id']
        return ultimo_id + 1
    return 1


if __name__ == '__main__':
    app.run()
