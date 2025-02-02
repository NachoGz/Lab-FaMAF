## Configuración y Ejecución de Flask

Antes de comenzar, asegúrate de tener tu entorno virtual activado y de haber instalado Flask y las demás dependencias listadas en tu `requirements.txt`.

Para ejecutar tu aplicación Flask, navega a la carpeta que contiene tu `main.py` y ejecuta los siguientes comandos en tu terminal:

```bash
export FLASK_APP=main.py
export FLASK_ENV=development
flask run
```

Este comando inicia tu aplicación Flask en modo de desarrollo, lo que te permite ver los cambios en tiempo real y proporciona un depurador.

Prueba de Funcionalidades:


Obtener detalles de una película por ID:
```
curl http://localhost:5000/peliculas/1
```

Actualizar detalles de una película:
```
curl -X PUT http://localhost:5000/peliculas/1 -H "Content-Type: application/json" -d '{"titulo": "Nuevo título"}'
```

Eliminar una película:
```
curl -X DELETE http://localhost:5000/peliculas/1
```

Devolver películas de un género específico:
```
curl http://localhost:5000/peliculas/genero/Acción
```

Buscar películas por título:
```
curl http://localhost:5000/peliculas/buscar/Star
```

Sugerir una película aleatoria:
```
curl http://localhost:5000/peliculas/sugerencia
```

Sugerir una película aleatoria por género:
```
curl http://localhost:5000/peliculas/sugerencia/Acción
```

## Para correr nuestros test:
```
python -m unittest tests/test_app.py
```