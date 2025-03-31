/**
 * Listar el título, año, actores (cast), directores y rating de las 10 películas con mayor 
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
  