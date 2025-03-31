/* PARTE 1 */

/**
 * 1. Insertar 5 nuevos usuarios en la colección users. Para cada nuevo usuario creado, insertar al menos un comentario
 * realizado por el usuario en la colección comments
*/

use('mflix')

db.users.find({
    "email": "alice@example.com"
})

// Insert users
db.users.insertMany([
    { name: 'Alice Johnson', email: 'alice@example.com', password: 'hashed_password1', joinedDate: new Date() },
    { name: 'Bob Smith', email: 'bob@example.com', password: 'hashed_password2', joinedDate: new Date() },
    { name: 'Charlie Brown', email: 'charlie@example.com', password: 'hashed_password3', joinedDate: new Date() },
    { name: 'Diana Prince', email: 'diana@example.com', password: 'hashed_password4', joinedDate: new Date() },
    { name: 'Ethan Hunt', email: 'ethan@example.com', password: 'hashed_password5', joinedDate: new Date() }
]);

    // Insert comments
db.comments.insertMany([
    { userEmail: 'alice@example.com', movie_id: '64f7e2c9c2d2a87b96bf6c4d', text: 'Amazing movie! Highly recommend it.', date: new Date() },
    { userEmail: 'bob@example.com', movie_id: '64f7e2c9c2d2a87b96bf6c4e', text: 'Not as good as I expected, but still entertaining.', date: new Date() },
    { userEmail: 'charlie@example.com', movie_id: '64f7e2c9c2d2a87b96bf6c4f', text: 'A masterpiece! The acting was superb.', date: new Date() },
    { userEmail: 'diana@example.com', movie_id: '64f7e2c9c2d2a87b96bf6c50', text: 'Loved every minute of it!', date: new Date() },
    { userEmail: 'ethan@example.com', movie_id: '64f7e2c9c2d2a87b96bf6c51', text: 'Action-packed and thrilling.', date: new Date() }
]);


/**
 * 2. Listar el título, año, actores (cast), directores y rating de las 10 películas con mayor 
 * rating (“imdb.rating”) de la década del 90. ¿Cuál es el valor del rating de la película que tiene mayor rating? 
 * (Hint: Chequear que el valor de “imdb.rating” sea de tipo “double”).
 */
use('mflix')


// Query to find the top 10 movies of the 90s by IMDb rating
db.movies.find(
    {
      year: { $gte: 1990, $lte: 1999 }, // Movies from 1990 to 1999
      "imdb.rating": { $type: "double" } // Check that IMDb rating is of type double
    },
    {
      title: 1, // Include title
      year: 1, // Include year
      cast: 1, // Include cast
      directors: 1, // Include directors
      "imdb.rating": 1 // Include IMDb rating
    }
  ).sort({ "imdb.rating": -1 }) // Sort by IMDb rating in descending order
    .limit(10); // Limit to top 10 movies
  

    /**
 * 3. Listar el nombre, email, texto y fecha de los comentarios que la película con id 
 * (movie_id) ObjectId("573a1399f29313caabcee886") recibió entre los años 2014 y 2016
 * inclusive. Listar ordenados por fecha. Escribir una nueva consulta (modificando la
 * anterior) para responder ¿Cuántos comentarios recibió?
 */

use('mflix')

// Remove the use statement when running in VSCode
db.comments.find(
    {
      movie_id: ObjectId("573a1399f29313caabcee886"), // Ensure correct ObjectId format
      date: { 
        $gte: ISODate("2014-01-01T00:00:00Z"), // Start of 2014
        $lte: ISODate("2016-12-31T23:59:59Z")  // End of 2016
      }
    },
    {
      name: 1, // Include name
      email: 1, // Include email
      text: 1, // Include text
      date: 1 // Include date
    }
  ).sort({ date: -1 }); // Sort by date in descending order
  

use('mflix')

// Remove the use statement when running in VSCode
db.comments.find(
    {
      movie_id: ObjectId("573a1399f29313caabcee886"), // Ensure correct ObjectId format
      date: { 
        $gte: ISODate("2014-01-01T00:00:00Z"), // Start of 2014
        $lte: ISODate("2016-12-31T23:59:59Z")  // End of 2016
      }
    },
    {
      name: 1, // Include name
      email: 1, // Include email
      text: 1, // Include text
      date: 1 // Include date
    }
  ).count();


/**
 * 4. Listar el nombre, id de la película, texto y fecha de los 3 comentarios 
 * más recientes realizados por el usuario con email patricia_good@fakegmail.com
 */


use('mflix')

db.comments.find(
    {
        "email": "patricia_good@fakegmail.com"
    },
    {
        name: 1, // Include name
        email: 1, // Include email
        text: 1, // Include text
        date: 1 // Include date
    }
).sort({ date: -1 }).limit(3);


/**
 * 5. Listar el título, idiomas (languages), géneros, fecha de lanzamiento (released) y 
 * número de votos (“imdb.votes”) de las películas de géneros Drama y Action 
 * (la película puede tener otros géneros adicionales), que solo están disponibles 
 * en un único idioma y por último tengan un rating (“imdb.rating”) mayor a 9 o 
 * bien tengan una duración (runtime) de al menos 180 minutos. Listar ordenados 
 * por fecha de lanzamiento y número de votos.
 */

use('mflix')

db.movies.find(
    {
        "genres": {$in: ["Drama", "Action"]},
        "languages": {$size: 1},
        $or: [
            {"imdb.rating": {$gte: 9} },
            {"runtime": {$gte: 180} }
        ]
    },
    {
        title: 1,
        released: 1,
        languages: 1,
        "imdb.votes": 1,
        genres: 1
    }
).sort({ relesead: 1, "imdb.votes": -1 });


/**
 * 6. Listar el id del teatro (theaterId), estado (“location.address.state”), ciudad
 * (“location.address.city”), y coordenadas (“location.geo.coordinates”) de los teatros que
 * se encuentran en algunos de los estados "CA", "NY", "TX" y el nombre de la ciudades
 * comienza con una ‘F’. Listar ordenados por estado y ciudad.
 */

use('mflix')

db.theaters.find(
    {
        "location.address.state": {$in: ["CA", "NY", "TX"]},
        "location.address.city": {$regex: "^F", $options: "i"}
    },
    {
        theaterId: 1,
        "location.address.state": 1,
        "location.address.city": 1,
        "location.geo.coordinates": 1
    }
).sort({ "location.address.state": 1, "location.address.city": 1});


/**
 * 7. Actualizar los valores de los campos texto (text) y fecha (date) del comentario cuyo id es 
 * ObjectId("5b72236520a3277c015b3b73") a "mi mejor comentario" y fecha actual respectivamente.
 */

use('mflix')

db.comments.updateOne(
    {
        "_id": ObjectId("5b72236520a3277c015b3b73")
    },
    {
        $set: {
            text: "mi mejor comentario",
            date: new Date()
        }
    }
);


/**
 * 8. Actualizar el valor de la contraseña del usuario cuyo email es 
 * joel.macdonel@fakegmail.com a "some password". La misma consulta debe poder 
 * insertar un nuevo usuario en caso que el usuario no exista. Ejecute la consulta dos veces. 
 * ¿Qué operación se realiza en cada caso? (Hint: usar upserts).
 */

use('mflix')
db.comments.find(
    {
        "email":"joel.macdonel@fakegmail.com"
    }
);


db.comments.updateOne(
    {
        "email": "joel.macdonel@fakegmail.com"
    },
    {
        $set: {
            password: "some password"
        }
    },
    {
        upsert: true
    }
);


/**
 * 9. Remover todos los comentarios realizados por el usuario cuyo email es 
 * victor_patel@fakegmail.com durante el año 1980
 */

use('mflix')
db.comments.find(
    {
        "email": "victor_patel@fakegmail.com",
        date: {
            $gte: ISODate("1980-01-01T00:00:00Z"), // Start of 1980
            $lt: ISODate("1981-01-01T00:00:00Z")  // Start of 1981 (exclusive)
        }
    }
);

db.comments.deleteMany(
    {
        "email": "victor_patel@fakegmail.com",
        date: {
            $gte: ISODate("1980-01-01T00:00:00Z"), // Start of 1980
            $lt: ISODate("1981-01-01T00:00:00Z")  // Start of 1981 (exclusive)
        }
    }
);

/* PARTE 2 */

/**
 * 10. Listar el id del restaurante (restaurant_id) y las calificaciones de los restaurantes donde
 * al menos una de sus calificaciones haya sido realizada entre 2014 y 2015 inclusive, y
 * que tenga una puntuación (score) mayor a 70 y menor o igual a 90.
 */

use('restaurantdb')


db.restaurants.find(
    {
        "grades": {
            $elemMatch: {
                "date": {
                    $gte: ISODate("2014-01-01T00:00:00Z"),
                    $lte: ISODate("2015-12-31T23:59:59Z")
                },
                "score": { $gt: 70, $lte: 90 }
            }
        }
    },
    {
        restaurant_id: 1,
        grades: 1
    }
);

// db.restaurants.find(
//     {
//         "grades.date": {
//             $gte: ISODate("2014-01-01T00:00:00Z"),
//             $lte: ISODate("2015-12-31T23:59:59Z")
//         },
//         "grades.score": {$gt: 70, $lte: 90}
//     },
//     {
//         restaurant_id: 1,
//         grades: 1
//     }
// ).count();

/**
 * 11. Agregar dos nuevas calificaciones al restaurante cuyo id es "50018608". A continuación 
 * se especifican las calificaciones a agregar en una sola consulta.
 * {
 * "date" : ISODate("2019-10-10T00:00:00Z"),
 * "grade" : "A",
 * "score" : 18
 * }
 * {
 * "date" : ISODate("2020-02-25T00:00:00Z"),
 * "grade" : "A",
 * "score" : 21
 * }
 */

use('restaurantdb')

db.restaurants.updateOne(
  { restaurant_id: "50018608" },
  { 
    $push: {
      grades: {
        $each: [
          { date: ISODate("2019-10-10T00:00:00Z"), grade: "A", score: 18 },
          { date: ISODate("2020-02-25T00:00:00Z"), grade: "A", score: 21 }
        ]
      }
    }
  }
);




