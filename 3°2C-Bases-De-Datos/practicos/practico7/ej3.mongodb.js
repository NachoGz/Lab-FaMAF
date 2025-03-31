/**
 * Listar el nombre, email, texto y fecha de los comentarios que la película con id 
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