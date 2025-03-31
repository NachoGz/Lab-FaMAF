/**
 * 1. Buscar los documentos donde el alumno tiene:
 * * (i) un puntaje mayor o igual a 80 en "exam" o bien un puntaje mayor o igual a 90 en "quiz" y
 * * (ii) un puntaje mayor o igual a 60 en todos los "homework" (en otra palabras no tiene 
 * un puntaje menor a 60 en algun "homework")
 * Se debe mostrar todos los campos excepto el _id, ordenados por el id de la clase y
 * id del alumno en orden descendente y ascendente respectivamente.
 */
use('university')
db.grades.find(
    {
        $and: [
            {
                $or: [
                    {"scores": { $elemMatch: {type: "exam", score: {$gte: 80 } } } },
                    {"scores": { $elemMatch: {type: "quiz", score: {$gte: 90 } } } },
                ]
            },
            {
                "scores": {
                    $not: {
                        $elemMatch: { type: "homework", score: { $lt: 60 } }
                    }
                }
            }
        ]
    },
    {
        _id: 0
    }
).sort({ class_id: -1, student_id: 1 });


/**
 * 2. Calcular el puntaje mínimo, promedio, y máximo que obtuvo el alumno en las clases
 * 20, 220, 420. El resultado debe mostrar además el id de la clase y el id del alumno,
 * ordenados por alumno y clase en orden ascendentes.
 */
use('university')
db.grades.aggregate([
    {
        $match: {
            class_id: {$in: [20, 220, 420]}
        }
    },
    {
        $unwind: "$scores"
    },
    {
        $group: {
          _id: "$class_id",
          avgScore: { $avg: "$scores.score"},
          maxScore: { $max: "$scores.score"},
          minScore: { $min: "$scores.score"}
        }
    },
    {
        $project: {
          class_id: "$_id",
          student_id: 1,
          _id: 0,
          avgScore: 1,
          maxScore: 1,
          minScore: 1
        }
    },
    {
        $sort: {
            class_id: -1,
            student_id: -1
          }
    }
])


/**
 * 3. Para cada clase listar el puntaje máximo de las evaluaciones de tipo "exam" y el
 * puntaje máximo de las evaluaciones de tipo "quiz". Listar en orden ascendente por el
 * id de la clase. HINT: El operador $filter puede ser de utilidad.
 */
use('university')
db.grades.aggregate([
  {
    $unwind: "$scores" // Decompose the scores array
  },
  {
    $match: {
      "scores.type": { $in: ["exam", "quiz"] } // Filter for exam and quiz types
    }
  },
  {
    $group: {
      _id: {
        class_id: "$class_id",
        type: "$scores.type"
      },
      maxScore: { $max: "$scores.score" } // Calculate the maximum score for each type
    }
  },
  {
    $group: {
      _id: "$_id.class_id",
      maxExamScore: {
        $max: {
          $cond: [{ $eq: ["$_id.type", "exam"] }, "$maxScore", null]
        }
      },
      maxQuizScore: {
        $max: {
          $cond: [{ $eq: ["$_id.type", "quiz"] }, "$maxScore", null]
        }
      }
    }
  },
  {
    $project: {
      _id: 0,
      class_id: "$_id",
      maxExamScore: 1,
      maxQuizScore: 1
    }
  },
  {
    $sort: { class_id: 1 } // Sort by class_id in ascending order
  }
]);

/**
 * 4. Crear una vista "top10students" que liste los 10 estudiantes con los mejores promedios.
 */
use('university')
db.createView("top10students", "grades", [
    {
        $unwind: "$scores"
    },
    {
        $group: {
          _id: "$class_id",
          avgScore: { $avg: "$scores.score"}
        }
    },
    {
        $project: {
          class_id: "$_id",
          student_id: 1,
          _id: 0,
          avgScore: 1,
        }
    },
    {
        $sort: {
            avgScore: -1
        }
    },
    {
        $limit: 10
    }
])

use('university')
db.top10students.find({}, { _id: 0 } )


/**
 * 5. Actualizar los documentos de la clase 339, agregando dos nuevos campos: el
 * campo "score_avg" que almacena el puntaje promedio y el campo "letter" que tiene
 * el valor "NA" si el puntaje promedio está entre [0, 60), el valor "A" si el puntaje
 * promedio está entre [60, 80) y el valor "P" si el puntaje promedio está entre [80, 100].
 * HINTS: (i) para actualizar se puede usar pipeline de agregación. (ii) El operador
 * $cond o $switch pueden ser de utilidad
 */
use('university')
db.grades.updateMany(
    { class_id: 339 },
    [
        {
            $set: {
                score_avg: { $avg: "$scores.score" },
            }
        },
        {
            $set: {
                letter: {
                    $switch: {
                        branches: [
                            {
                                case: { $lt: ["$score_avg", 60] },
                                then: "NA"
                            },
                            {
                                case: { $lt: ["$score_avg", 80] },
                                then: "A" 
                            },
                            {
                                case: { $lte: ["$score_avg", 100] },
                                then: "P"
                            }
                        ]
                    }
                }
            }
        }
    ]
)
