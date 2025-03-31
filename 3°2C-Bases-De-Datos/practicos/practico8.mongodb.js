/* 1. Cantidad de cines (theaters) por estado */
use('mflix')
db.theaters.aggregate([
    {
        $group: {
            _id: "$location.address.state",
            total_theaters: { $sum: 1}
        }
    },
    {
        $sort: {total_theaters: -1}
    }
])


/**
 * 2. Cantidad de estados con al menos dos cines (theaters) registrados
 */

use('mflix')
db.theaters.aggregate([
    {
        $group: {
            _id: "$location.address.state",
            total_theaters: { $sum: 1}
        }
    },
    {
        $match: {
          total_theaters: {$gte: 2}
        }
    },
    {
        $count: "states_with_two_or_more_theaters"
    }
])


/**
 * 3. Cantidad de películas dirigidas por "Louis Lumière". Se puede responder sin pipeline de 
 * agregación, realizar ambas queries
 */
use('mflix')
db.movies.aggregate([
    {
        $match: {
            directors: "Louis Lumière"
        }
    },
    {
        $count: "total_movies"
    }
])

/* sin pipeline */
db.movies.countDocuments({ directors: "Louis Lumière" })

db.movies.find({ directors: "Louis Lumière" }).count()


/**
 * 4. Cantidad de películas estrenadas en los años 50 (desde 1950 hasta 1959). Se puede 
 * responder sin pipeline de agregación, realizar ambas queries
 */
use('mflix')
db.movies.aggregate([
    {
        $match: {
            year: {$gte: 1950, $lte: 1959}
        }
    },
    {
        $count: "total_movies"
    }
])

/* sin pipeline */
db.movies.countDocuments({ year: { $gte: 1950, $lte: 1959 } })

db.movies.find({ year: { $gte: 1950, $lte: 1959 } }).count()


/**
 * 5. Listar los 10 géneros con mayor cantidad de películas (tener en cuenta que las películas
 * pueden tener más de un género). Devolver el género y la cantidad de películas. 
 * Hint: unwind puede ser de utilidad
*/

use('mflix')
db.movies.aggregate([
    {
        $unwind: "$genres"
    },
    {
        $group: {
            _id: "$genres",
            total_movies: { $sum: 1}
        }
    },
    {
        $sort: {total_movies: -1}
    },
    {
        $limit: 10
    },
    {
        $project: {
          genre: "$_id",
          total_movies: 1,
          _id: 0
        }
    }
])

/**
 * 6. Top 10 de usuarios con mayor cantidad de comentarios, 
 * mostrando Nombre, Email y Cantidad de Comentarios.
 */
use('mflix')
db.comments.aggregate([
    {
        $group: {
            _id: "$name",
            total_comments: { $sum: 1}
        }
    },
    {
        $sort: {total_comments: -1}
    },
    {
        $limit: 10
    },
    {
        $project: {
          user: "$_id",
          total_comments: 1,
          _id: 0
        }
    }
])

/**
 * 7. Ratings de IMDB promedio, mínimo y máximo por año de las películas estrenadas en
 * los años 80 (desde 1980 hasta 1989), ordenados de mayor a menor por promedio del año.
 */
use('mflix')
db.movies.aggregate([
    {
        $match: {
            year: {$gte: 1980, $lte: 1989}
        }
    },
    {
        $group: {
            _id: "$year",
            avgRating: {$avg: "$imdb.rating"},
            maxRating: {$max: "$imdb.rating"},
            minRating: {$min: "$imdb.rating"}
        }
    },
    {
        $project: {
            year: "$_id",
            _id: 0,
            avgRating: 1,
            maxRating: 1,
            minRating: 1
        }
    },
    {
        $sort: {avgRating: -1}
    }
])

/**
 * 8. Título, año y cantidad de comentarios de las 10 películas con más comentarios.
 */
use('mflix')
db.movies.aggregate([
    {
      $lookup: {
        from: "comments",
        localField: "_id",
        foreignField: "movie_id",
        as: "comments"
      }
    },
    {
      $project: {
        title: 1,
        year: 1,
        num_comments: { $size: "$comments" }  // Counts the number of joined comments
      }
    },
    {
      $sort: { num_comments: -1 }  // Sorts by the number of comments in descending order
    },
    {
      $limit: 10  // Limits to the top 10 movies with the most comments
    }
  ])
  
/**
 * 9. Crear una vista con los 5 géneros con mayor cantidad de comentarios, junto con la cantidad de comentarios.
 */
use('mflix')
db.createView("TopGenresByComments", "movies", [
    {
      $lookup: {
        from: "comments",
        localField: "_id",
        foreignField: "movie_id",
        as: "comments"
      }
    },
    {
      $unwind: "$genres"  // Unwind the genres array
    },
    {
      $project: {
        genre: "$genres",
        num_comments: { $size: "$comments" }  // Count the number of comments per movie
      }
    },
    {
      $group: {
        _id: "$genre",
        total_comments: { $sum: "$num_comments" }  // Sum the comments count for each genre
      }
    },
    {
      $sort: { total_comments: -1 }  // Sort by total comments in descending order
    },
    {
      $limit: 5  // Limit to the top 5 genres
    },
    {
      $project: {
        genre: "$_id",
        _id: 0,
        total_comments: 1
      }
    }
  ])


/**
 * 10. Listar los actores (cast) que trabajaron en 2 o más películas dirigidas por "Jules Bass".
 * Devolver el nombre de estos actores junto con la lista de películas (solo título y año)
 * dirigidas por “Jules Bass” en las que trabajaron.
 */
use('mflix')
db.movies.aggregate([
    {
        $match: {
            directors: "Jules Bass" // Filtro peliculas solo dirigidas por Jules Bass
        }
    },
    {
        $unwind: "$cast" // Descompongo el array de cast para tratar a cada actor individualmente
    },
    {
       $group: {
          _id: "$cast", // agrupo por actor,
          movies: {
            $push: {title: "$title", year: "$year"},
          },
          count: { $sum: 1}
       }
    },
    {
        $match: {
          count: { $gte: 2 }
        }
    },
    {
        $project: {
          _id: 0,
          actor: "$_id",
          movies: 1
        }
    }
])

// alternative
use('mflix')
db.movies.aggregate([
    {
        $match: {
            directors: "Jules Bass" // Filtro peliculas solo dirigidas por Jules Bass
        }
    },
    {
        $unwind: "$cast" // Descompongo el array de cast para tratar a cada actor individualmente
    },
    {
       $group: {
          _id: "$cast", // agrupo por actor,
          movies: {
            $addToSet: {title: "$title", year: "$year"}
          }
       }
    },
    {
        $match: {
          "movies.1": { $exists: true }
        }
    },
    {
        $project: {
          _id: 0,
          actor: "$_id",
          movies: 1
        }
    }
])


/**
 * 11. Listar los usuarios que realizaron comentarios durante el mismo mes de lanzamiento de
 * la película comentada, mostrando Nombre, Email, fecha del comentario, título de la
 * película, fecha de lanzamiento. HINT: usar $lookup con multiple condiciones
 */
use('mflix')
db.comments.aggregate([
    {
      $lookup: {
        from: "movies",
        let: { movieId: "$movie_id", commentDate: "$date" },
        pipeline: [
          {
            $match: {
              $expr: {
                $and: [
                  { $eq: ["$_id", "$$movieId"] },  // Match movie by `movie_id`
                  { $eq: [{ $year: "$released" }, { $year: "$$commentDate" }] },  // Match year
                  { $eq: [{ $month: "$released" }, { $month: "$$commentDate" }] } // Match month
                ]
              }
            }
          },
          {
            $project: { title: 1, released: 1 }  // Only include title and release date in movie data
          }
        ],
        as: "movieDetails"  // Name the joined array
      }
    },
    {
      $unwind: "$movieDetails"  // Unwind to include only comments with matching movies
    },
    {
      $project: {
        _id: 0,
        name: 1,
        email: 1,
        commentDate: "$date",
        movieTitle: "$movieDetails.title",
        releaseDate: "$movieDetails.released"
      }
    }
  ])
  

  /**
   * 12. Listar el id y nombre de los restaurantes junto con su puntuación máxima, mínima y la
   * suma total. Se puede asumir que el restaurant_id es único.
   * * a. Resolver con $group y accumulators.
   * * b. Resolver con expresiones sobre arreglos (por ejemplo, $sum) pero sin $group.
   * * c. Resolver como en el punto b) pero usar $reduce para calcular la puntuación total.
   * * d. Resolver con find.
   */
 // a.
use('restaurantdb')
db.restaurants.aggregate([
    {
        $unwind: "$grades"
    },
    {
        $group: {
            _id: {_id: "$_id", name:"$name",},
            totalScore: {$sum: {$sum: "$grades.score"} },
            maxScore: {$max: "$grades.score"},
            minscore: {$min: "$grades.score"}
        }
    },
    {
        $project: {
            restaurantId: "$_id.id",
            name: "$_id.name",
            _id: 0,
            totalScore: 1,
            maxScore: 1,
            minscore: 1
        }
    }
])

// b.
use('restaurantdb')
db.restaurants.aggregate([
    {
        $project: {
            restaurantId: "$_id",
            name: "$name",
            _id: 0,
            maxScore: {$max: "$grades.score"},
            minscore: {$min: "$grades.score"},
            totalScore: { $sum: "$grades.score"}
        }
    }
])

// c.
use('restaurantdb')
db.restaurants.aggregate([
    {
        $project: {
            restaurantId: "$_id",
            name: "$name",
            _id: 0,
            maxScore: {$max: "$grades.score"},
            minscore: {$min: "$grades.score"},
            totalScore: { 
                $reduce: {
                    input: "$grades.score",
                    initialValue: 0,
                    in: {$add: ["$$value", "$$this"] }
                }
            }
        }
    }
])

// d.
use('restaurantdb')
db.restaurants.find(
    {},
    {
        "_id": 1,
        name: 1,
        "grades.score": 1
    }
)

/**
 * 13. Actualizar los datos de los restaurantes añadiendo dos campos nuevos.
 * * a. "average_score": con la puntuación promedio
 * * b. "grade": con "A" si "average_score" está entre 0 y 13, con "B" si "average_score" está entre 14 y 27 
 * * * con "C" si "average_score" es mayor o igual a 28
 * Se debe actualizar con una sola query.
 * * a. HINT1. Se puede usar pipeline de agregación con la operación update
 * * b. HINT2. El operador $switch o $cond pueden ser de ayuda.
 */
db.restaurants.updateMany(
    {},
    [
      {
        $set: {
          average_score: { $avg: "$grades.score" },  // Calculate the average score
          grade: {
            $switch: {
              branches: [
                { case: { $lte: ["$average_score", 13] }, then: "A" },
                { case: { $and: [{ $gt: ["$average_score", 13] }, { $lte: ["$average_score", 27] }] }, then: "B" },
                { case: { $gte: ["$average_score", 28] }, then: "C" }
              ],
              default: "N/A"  // Optional: Default if no condition matches
            }
          }
        }
      }
    ]
  )
  