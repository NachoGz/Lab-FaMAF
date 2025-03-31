// Ejercicio 1
use('university')
db.grades.aggregate([
    {
        $unwind: "$scores"
    },
    {
        $group: {
            _id: "$class_id",
            average_score: { $avg: "$scores.score"}
        }
    },
    {
        $lookup: {
            from: "grades",
            pipeline: [
                {
                    $unwind: "$scores"
                },
                {
                    $group: {
                        _id: null,
                        overall_average: { $avg: "$scores.score"}
                    }
                }
            ],
            as: "overall_average"
        }
    },
    {
        $project: {
            _id: 0,
            class_id: "$_id",
            average_score: 1,
            overall_average: { $first: "$overall_average.overall_average"},
            comparison_to_overall_average: {
                $switch: {
                    branches: [
                        { case: { $gt: ["$average_score", { $first: "$overall_average.overall_average" }]}, then: "above"},
                        { case: { $eq: ["$average_score", { $first: "$overall_average.overall_average"}]}, then: "equal"},
                        { case: { $lt: ["$average_score", { $first: "$overall_average.overall_average" }]}, then: "below"},
                    ]
                }
            }
        }
    }
])


// Ejercicio 2
use('university')
db.grades.updateMany(
    {},
    [
        {
            $set: {
                scores: {
                    $map: {
                        input: "$scores.score",
                        as: "score",
                        in: {
                            $divide: [{ $multiply: ["$$score", 7] }, 100]
                        }
                    }
                } 
                
            }
        }
    ]
)


// Ejercicio 3
use('university')
db.createView("top10students_homework", "grades", [
    {
        $unwind: "$scores"
    },
    {
        $match: {
            "scores.type": "homework"
        }
    },
    {
        $group: {
          _id: "$student_id",
          avgScore: { $avg: "$scores.score"}
        }
    },
    {
        $project: {
          student_id: "$_id",
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
db.top10students_homework.find({}, { _id: 0 } )


// Ejercicio 4
use('university')
db.runCommand({
    collMod: "grades",
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["scores"],
            properties: {
                class_id: {
                    bsonType: "int",
                    description: "Debe ser un numero entero."
                },
                scores: {
                    bsonType: "object",
                    required: ["score", "type"],
                    properties: {
                        score: {
                            bsonType: "double",
                            description: "Debe ser un numero entero."
                        },
                        type: {
                            bsonType: "string",
                            enum: ["exam", "quiz", "homework"],
                            description: "Debe ser de tipo 'exam', 'quiz' o 'homework'"
                        }
                    },
                    description: "Debe tener los valores score y type."
                },
                student_id: {
                    bsonType: "int",
                    description: "Debe ser un numero entero."
                }
            }
        }
    },
    validationLevel: "strict",
    validationAction: "error"
});
