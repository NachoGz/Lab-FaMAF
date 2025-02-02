import unittest
from main import app
import json

class FlaskTest(unittest.TestCase):

    def setUp(self):
        # Setup que se ejecuta antes de cada test.
        app.testing = True
        self.client = app.test_client()

    def test_obtener_peliculas(self):
        # Prueba para verificar que el endpoint /peliculas funciona.
        response = self.client.get('/peliculas')
        self.assertEqual(response.status_code, 200)
        self.assertIsInstance(json.loads(response.data), list)

    def test_obtener_pelicula_por_id(self):
        # Prueba para obtener una película específica por ID.
        response = self.client.get('/peliculas/1')
        self.assertEqual(response.status_code, 200)
        self.assertIn('Indiana Jones', response.data.decode('utf-8'))

    def test_agregar_pelicula(self):
        # Prueba para agregar una nueva película.
        response = self.client.post('/peliculas', json={
            'titulo': 'Prueba',
            'genero': 'Test'
        })
        self.assertEqual(response.status_code, 201)
        self.assertIn('Prueba', response.data.decode('utf-8'))

    def test_actualizar_pelicula(self):
        # Prueba para actualizar una película existente.
        response = self.client.put('/peliculas/1', json={
            'titulo': 'Indiana Jones - Actualizado',
            'genero': 'Acción'
        })
        self.assertEqual(response.status_code, 200)
        self.assertIn('Indiana Jones - Actualizado', response.data.decode('utf-8'))

    def test_eliminar_pelicula(self):
        # Prueba para eliminar una película.
        response = self.client.delete('/peliculas/3')
        self.assertEqual(response.status_code, 200)
        data = json.loads(response.data)
        self.assertEqual(data['mensaje'], 'Película eliminada')

    def test_peliculas_por_genero(self):
        # Prueba para filtrar películas por género.
        response = self.client.get('/peliculas/genero/Acción')
        self.assertEqual(response.status_code, 200)
        self.assertTrue(len(json.loads(response.data)) > 0)

    def test_buscar_peliculas(self):
        # Prueba para buscar películas por título.
        response = self.client.get('/peliculas/buscar/Indiana')
        self.assertEqual(response.status_code, 200)
        self.assertTrue(len(json.loads(response.data)) > 0)

    def test_sugerir_pelicula(self):
        # Prueba para obtener una sugerencia de película aleatoria.
        response = self.client.get('/peliculas/sugerencia')
        self.assertEqual(response.status_code, 200)
        self.assertTrue(isinstance(json.loads(response.data), dict))

    def test_sugerir_pelicula_por_genero(self):
        # Prueba para obtener una sugerencia de película aleatoria por género.
        response = self.client.get('/peliculas/sugerencia/Acción')
        self.assertEqual(response.status_code, 200)
        self.assertIn('genero', json.loads(response.data))

if __name__ == '__main__':
    unittest.main()
