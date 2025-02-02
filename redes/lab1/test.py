"""Modulo utilizado para testing"""
import requests

# Obtener todas las películas
response = requests.get('http://localhost:5000/peliculas', timeout=2500)
peliculas = response.json()
print("Películas existentes:")

for pelicula in peliculas:
    print(f"ID: {pelicula['id']}, Título: {pelicula['titulo']}, Género: {pelicula['genero']}")
print()

# Agregar una nueva película
nueva_pelicula = {
    'titulo': 'Pelicula de prueba',
    'genero': 'Acción'
}
response = requests.post('http://localhost:5000/peliculas', json=nueva_pelicula, timeout=2500)
if response.status_code == 201:
    pelicula_agregada = response.json()
    print("Película agregada:")
    print(f"ID: {pelicula_agregada['id']}, Título: {pelicula_agregada['titulo']}," 
          f"Género: {pelicula_agregada['genero']}")
else:
    print("Error al agregar la película.")
print()

# Obtener detalles de una película específica
id_pelicula = 1  # ID de la película a obtener
response = requests.get(f'http://localhost:5000/peliculas/{id_pelicula}', timeout=2500)
if response.status_code == 200:
    pelicula = response.json()
    print("Detalles de la película:")
    print(f"ID: {pelicula['id']}, Título: {pelicula['titulo']}, Género: {pelicula['genero']}")
else:
    print("Error al obtener los detalles de la película.")
print()

# Actualizar los detalles de una película
id_pelicula = 1  # ID de la película a actualizar
datos_actualizados = {
    'titulo': 'Nuevo título',
    'genero': 'Comedia'
}
response = requests.put(f'http://localhost:5000/peliculas/{id_pelicula}', 
                        json=datos_actualizados, timeout=2500)
if response.status_code == 200:
    pelicula_actualizada = response.json()
    print("Película actualizada:")
    print(f"ID: {pelicula_actualizada['id']}, Título: {pelicula_actualizada['titulo']},"
          f"Género: {pelicula_actualizada['genero']}")
else:
    print("Error al actualizar la película.")
print()

# Eliminar una película
id_pelicula = 1  # ID de la película a eliminar
response = requests.delete(f'http://localhost:5000/peliculas/{id_pelicula}', timeout=2500)
if response.status_code == 200:
    print("Película eliminada correctamente.")
else:
    print("Error al eliminar la película.")


# Obtener peliculas de un género específico
genero = "Acción"

response = requests.get(f"http://localhost:5000/peliculas/genero/{genero}", timeout=2500)
if response.status_code == 200:
    peliculas = response.json()
    print(f"Películas existentes del género {genero}")
    for pelicula in peliculas:
        print(f"ID: {pelicula['id']}, Título: {pelicula['titulo']}, Género: {pelicula['genero']}")
    print()
else:
    print(f"Error al obtener los detalles de las películas de {genero}.")


# Obtener peliculas que contengan una string específica
string = "In"

response = requests.get(f"http://localhost:5000/peliculas/buscar/{string}", timeout=2500)
if response.status_code == 200:
    peliculas = response.json()
    print(f"Se encontraron al menos {len(peliculas)} coincidencias para {string}")
    for pelicula in peliculas:
        print(f"ID: {pelicula['id']}, Título: {pelicula['titulo']}, Género: {pelicula['genero']}")
    print()
else:
    print(f"Error al obtener los detalles de las películas que contienen la string {string}.")


# Sugerir una pelicula random
response = requests.get('http://localhost:5000/peliculas/sugerencia', timeout=2500)
if response.status_code == 200:
    pelicula = response.json()
    print("Detalles de la película aleatoria:")
    print(f"ID: {pelicula['id']}, Título: {pelicula['titulo']}, Género: {pelicula['genero']}")
    print()
else:
    print("Error al obtener los detalles de la película aleatoria.")


# Sugerir una pelicula random según genero
genero = "Acción"

response = requests.get(f"http://localhost:5000/peliculas/sugerencia/{genero}", timeout=2500)
if response.status_code == 200:
    pelicula = response.json()
    print(f"Detalles de la película aleatoria del genero {genero}:")
    print(f"ID: {pelicula['id']}, Título: {pelicula['titulo']}, Género: {pelicula['genero']}")
    print()
else:
    print(f"Error al obtener los detalles de la película aleatoria de {genero}.")


# Sugerir una pelicula random según genero para ver en el próximo feriado
genero = "Acción"

response = requests.get(f"http://localhost:5000/recomendar_pelicula_feriado/{genero}", timeout=2500)
if response.status_code == 200:
    pelicula = response.json()
    print(f"Detalles de la película aleatoria del genero {genero} en el próximo feriado:")
    print(f"ID: {pelicula['pelicula_recomendada']['id']},"
    f"Título: {pelicula['pelicula_recomendada']['titulo']},"
    f"Género: {pelicula['pelicula_recomendada']['genero']},"
    f"Feriado: {pelicula['proximo_feriado']['fecha']} - {pelicula['proximo_feriado']['motivo']}"
    f" - {pelicula['proximo_feriado']['tipo']}")
    print()
else:
    print(f"Error al obtener los detalles de las películas de {genero} recomendadas para el próximo feriado.")