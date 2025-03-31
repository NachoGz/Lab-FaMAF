/**
 * 1. Listar el nombre (name) y barrio (borough) de todos los restaurantes de cocina
 * (cuisine) tipo "Italian" y que entre sus notas (grades) tengan al menos una
 * entrada con nota (grade) "A" y puntaje (score) mayor o igual a 10. La lista final
 * sólo deberá mostrar 1 entrada por restaurante y deberá estar ordenada de manera
 * alfabética por el barrio primero y el nombre después. Hint: Revisar operadores
 * $regex y $elemMatch.
 */


use('restaurantdb')
db.restaurants.find(
    {
        "cuisine": "Italian",
        "grades": { 
            $elemMatch: {
                "grade": "A",
                "score": { $gte: 10 }
            }
        }
    },
    {
        name: 1,
        borough: 1,
        _id: 0
    }
).sort( {"borough": 1, "name": 1});


/**
 * 2. Actualizar las panaderías (cuisine ~ Bakery) y las cafeterías (cuisine ~ Coffee) 
 * agregando un nuevo campo discounts que sea un objeto con dos campos:
 * day y amount. Si el local se ubica en Manhattan, el día será "Monday" y el
 * descuento será "%10". En caso contrario el día será "Tuesday" y el descuento será
 * "5%". Hint: Revisar el operador $cond
 */

use('restaurantdb')
db.restaurants.updateMany(
    {
        "cuisine": { $regex: "^(Bakery|Coffee)", $options: "i" } // Coincidir con Bakery o Coffee
    },
    [
        {
            $set: {
                discounts:  {
                    day: { $cond: [{ $eq: ["$borough", "Manhattan"] }, "Monday", "Tuesday"]},
                    amount: { $cond: [{ $eq: ["$borough", "Manhattan"]}, "%10", "%5"] }
                }
            }
        }
    ]
);


/**
 * 3. Contar la cantidad de restaurantes cuyo address.zipcode se encuentre entre
 * 10000 y 11000. Tener en cuenta que el valor original es un string y deberá ser
 * convertido. También tener en cuenta que hay casos erróneos que no pueden ser
 * convertidos a número, en cuyo caso el valor será reemplazado por 0. Hint: Revisar
 * el operador $convert.
 */

use('restaurantdb')
db.restaurants.countDocuments(
    {
        $expr: {
            $and: [
                {
                    $gte: [
                        { $convert: { input: "$address.zipcode", to: "int", onError: 0, onNull: 0 } },
                        10000
                    ]
                },
                {
                    $lte: [
                        { $convert: { input: "$address.zipcode", to: "int", onError: 0, onNull: 0 } },
                        11000
                    ]
                }
            ]
        }
    }
);


/**
 * 4. Por cada tipo de cocina (cuisine), contar la cantidad de notas distintas recibidas
 * (grades.grade) en el segundo semestre de 2013. Ordenar por tipo de cocina y nota.
 */
use('restaurantdb')
db.restaurants.aggregate([
    {
        $unwind: "$grades"
    },
    {
        $match: {
            "grades.date": {
                $gte: ISODate("2013-07-01T00:00:00Z"), 
                $lte: ISODate("2013-12-31T23:59:59Z")
            }
        }
    },
    {
        $group: {
            _id: { cuisine: "$cuisine", grade: "$grades.grade" },
            count: { $sum: 1}
        }
    },
    {
        $group: {
            _id: "$_id.cuisine",
            grades: { $push: { grade: "$_id.grade", count: "$count" } }
        }
    },
    {
        $sort: {
            _id: 1
        }
    },
    {
        $project: {
            _id: 0,
            "cuisine": "$_id",
            grades: 1
        }
    }
]);

/**
 * 5. Data la siguiente tabla de conversión de notas (grades.grade):
 * | A | 5 |
 * | B | 4 |
 * | C | 3 |
 * | D | 2 |
 * | * | 1 |
 * 
 * Donde "*" sería el resto de los casos posibles. Transformar las notas de los
 * restaurantes de acuerdo a la tabla. Luego, calcular la nota promedio, máxima y
 * mínima por tipo de cocina (cuisine). El resultado final deberá mostrar la cocina, la
 * nota promedio, la nota máxima y la nota mínima, ordenadas de manera descendente
 * por la nota promedio. Hint: Revisar el operador $switch
 */

use('restaurantdb')
db.restaurants.aggregate([
    {
        $unwind: "$grades"
    },
    {
        $addFields: {
            numericGrade: {
                $switch: {
                    branches: [
                        { case: { $eq: ["$grades.grade", "A"]}, then: 5 },
                        { case: { $eq: ["$grades.grade", "B"]}, then: 4 },
                        { case: { $eq: ["$grades.grade", "C"]}, then: 3 },
                        { case: { $eq: ["$grades.grade", "D"]}, then: 2 },
                    ],
                    default: 1
                }
            }
        }
    },
    {
        $group: {
            _id: "$cuisine",
            avg_grade: { $avg: "$numericGrade"},
            max_grade: { $max: "$numericGrade"},
            min_grade: { $min: "$numericGrade"}    
        }
    },
    {
        $sort: {
            avg_grade: -1
        }
    },
    {
        $project: {
            _id: 0,
            cuisine: "$_id",
            avg_grade: 1,
            max_grade: 1,
            min_grade: 1
        }
    }
]);

/**
 * 6. Especificar reglas de validación para la colección restaurant utilizando JSON Schema. 
 * Tener en cuenta los campos: address (con sus campos anidados), borough, cuisine, grades 
 * (con sus campos anidados), name, restaurant_id, y discount (con sus campos anidados). 
 * Inferir tipos y otras restricciones que considere adecuadas (incluyendo campos requeridos). 
 * Agregar una regla de validación para que el zipcode, aún siendo un string, verifique 
 * que el rango esté dentro de lo permitido para New York City (i.e. 10001-11697). 
 * Finalmente dejar 2 casos de falla ante el esquema de validación y 1 caso de éxito. 
 * Hint: Deberán hacer conversión con $convert en el caso de la regla de validación. 
 * Los casos no deben ser triviales (i.e. sólo casos de falla por un error de tipos)
 */

use('restaurantdb')
db.runCommand({
    collMod: "restaurants",
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["address", "borough", "cuisine", "grades", "name", "restaurant_id"],
            properties: {
                address: {
                    bsonType: "object",
                    required: ["building", "street", "zipcode", "coord"],
                    properties: {
                        building: {
                            bsonType: "string",
                            description: "Debe ser un string y es obligatorio."
                        },
                        street: {
                            bsonType: "string",
                            description: "Debe ser un string y es obligatorio."
                        },
                        zipcode: {
                            bsonType: "string",
                            description: "Debe ser un string representando un código postal."
                        },
                        coord: {
                            bsonType: "array",
                            items: { bsonType: "double" },
                            minItems: 2,
                            maxItems: 2,
                            description: "Las coordenadas deben ser un array de 2 números."
                        }
                    },
                    description: "Debe incluir todos los campos requeridos de address."
                },
                borough: {
                    bsonType: "string",
                    enum: ["Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"],
                    description: "Debe ser uno de los 5 barrios de Nueva York."
                },
                cuisine: {
                    bsonType: "string",
                    description: "Debe ser un string y es obligatorio."
                },
                grades: {
                    bsonType: "array",
                    items: {
                        bsonType: "object",
                        required: ["date", "grade", "score"],
                        properties: {
                            date: {
                                bsonType: "date",
                                description: "Debe ser una fecha válida y es obligatoria."
                            },
                            grade: {
                                bsonType: "string",
                                enum: ["A", "B", "C"],
                                description: "Debe ser una letra A, B o C."
                            },
                            score: {
                                bsonType: "int",
                                minimum: 0,
                                description: "Debe ser un número entero positivo."
                            }
                        }
                    }
                },
                name: {
                    bsonType: "string",
                    minLength: 1,
                    description: "Debe ser una cadena no vacía."
                },
                restaurant_id: {
                    bsonType: "string",
                    pattern: "^[0-9]+$",
                    description: "Debe ser una cadena numérica."
                },
                discount: {
                    bsonType: "object",
                    required: ["day", "amount"],
                    properties: {
                        day: {
                            bsonType: "string",
                            enum: ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
                            description: "Debe ser un día válido de la semana."
                        },
                        amount: {
                            bsonType: "string",
                            pattern: "^[0-9]+%$",
                            description: "Debe ser un porcentaje (por ejemplo, '10%')."
                        }
                    }
                }
            }
        },
        $expr: {
            $let: {
                vars: {
                    zipcodeConverted: {
                        $convert: {
                            input: "$address.zipcode",
                            to: "int",
                            onError: 0,
                            onNull: 0
                        }
                    }
                },
                in: {
                    $and: [
                        { $gte: ["$$zipcodeConverted", 10001] },
                        { $lte: ["$$zipcodeConverted", 11697] }
                    ]
                }
            }
        }
    },
    validationLevel: "strict",
    validationAction: "error"
});




// caso positivo
use('restaurantdb')
db.restaurants.insertOne({
    address: {
        building: "123",
        street: "Main St",
        zipcode: "10005", // Zipcode válido dentro del rango 10001-11697
        coord: [-73.856077, 40.848447] // Coordenadas válidas (array de dos números)
    },
    borough: "Bronx", // Cadena de texto válida
    cuisine: "Italian", // Cadena de texto válida
    grades: [
        { date: ISODate("2023-01-01"), grade: "A", score: 10 } // Notas válidas
    ],
    name: "Luigi's Pizzeria", // Nombre válido (no vacío)
    restaurant_id: "123456", // ID válido (cadena numérica)
    discount: { day: "Monday", amount: "10%" } // Descuento válido
});


// caso negativo 1
use('restaurantdb')
db.restaurants.insertOne({
    address: {
        building: "456",
        street: "Broadway",
        zipcode: "99999",
        coord: [-73.856077, 40.848447]
    },
    borough: "Manhattan",
    cuisine: "Chinese",
    grades: [
        { date: ISODate("2023-02-15"), grade: "C", score: 5 }
    ],
    name: "Golden Wok",
    restaurant_id: "789101",
    discount: { day: "Friday", amount: "15%" }
});

// caso negativo 2
use('restaurantdb')
db.restaurants.insertOne({
    address: {
        building: "789",
        street: "Wall St",
        zipcode: "not-a-zipcode",
        coord: [-73.856077, 40.848447]
    },
    borough: "Brooklyn",
    cuisine: "Mexican",
    grades: [
        { date: ISODate("2023-05-15"), grade: "A", score: 8 }
    ],
    name: "Taco Fiesta",
    restaurant_id: "111213",
    discount: { day: "Saturday", amount: "20%" }
});


/**
 * 7. Se desean agregar "client reviews", dados por los clientes de los restaurantes. Los 
 * reviews cuentan de un título de menos de 50 caracteres, un puntaje entero entre 0 y 
 * 5, una reseña de máximo 250 caracteres (que es opcional) y una fecha y un cliente 
 * que lo realizó (con información de nombre y correo electrónico del cliente). Cada 
 * review está asociado a un restaurante y un mismo restaurante puede tener varios 
 * reviews. Asimismo, un cliente puede hacer reviews de varios restaurantes distintos. 
 * Teniendo en cuenta esto, decida la mejor manera de agregar esta información a la 
 * base de datos (y justifique su decisión en un comentario), genere un esquema de 
 * validación para dicha información y agregue algunos documentos de ejemplo
 */

use('restaurantdb')
db.createCollection("reviews", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["restaurant_id", "client", "title", "score", "date"],
            properties: {
                restaurant_id: {
                    bsonType: "string",
                    description: "Debe ser el ID del restaurante asociado al review."
                },
                title: {
                    bsonType: "string",
                    maxLength: 50,
                    description: "Debe ser un título de menos de 50 caracteres."
                },
                score: {
                    bsonType: "int",
                    minimum: 0,
                    maximum: 5,
                    description: "Debe ser un puntaje entero entre 0 y 5."
                },
                review: {
                    bsonType: "string",
                    maxLength: 250,
                    description: "Debe ser una reseña opcional de máximo 250 caracteres."
                },
                date: {
                    bsonType: "date",
                    description: "Debe ser una fecha válida del review."
                },
                client: {
                    bsonType: "object",
                    required: ["name", "email"],
                    properties: {
                        name: {
                            bsonType: "string",
                            minLength: 1,
                            description: "Debe ser un nombre no vacío."
                        },
                        email: {
                            bsonType: "string",
                            pattern: "^[\\w.-]+@[\\w.-]+\\.[a-zA-Z]{2,}$",
                            description: "Debe ser un correo electrónico válido."
                        }
                    },
                    description: "Debe contener información del cliente que realizó el review."
                }
            }
        }
    },
    validationLevel: "strict",
    validationAction: "error"
});


/**
 * Justificación:
 * * Relación muchos a muchos: 
 * * * Un cliente puede hacer reviews de varios restaurantes.
 * * * Un restaurante puede recibir reviews de varios clientes.
 * * * Usar una colección separada permite modelar esta relación sin redundancia.
 * Escalabilidad:
 * * Si los reviews fueran anidados dentro de restaurants, los documentos podrían crecer significativamente, complicando actualizaciones y almacenamiento.
 * Consultas eficientes:
 * * Con una colección separada, podemos indexar campos clave como restaurant_id y client.email para optimizar consultas específicas. por ejemplo, para obtener
 * todas las reviews de un restaurante -> db.reviews.find({ restaurant_id: "123456" }); o todas las reviews hechas por un cliente -> 
 * db.reviews.find({ "client.email": "john.doe@example.com" });
 */

// caso positivo 1
use('restaurantdb')
db.reviews.insertOne({
    restaurant_id: "123456",
    title: "Excellent Food!",
    score: 5,
    review: "The food was amazing, and the staff were very friendly.",
    date: ISODate("2024-11-20T18:30:00Z"),
    client: {
        name: "John Doe",
        email: "john.doe@example.com"
    }
});

// caso positivo 2
use('restaurantdb')
db.reviews.insertOne({
    restaurant_id: "654321",
    title: "Not bad",
    score: 3,
    date: ISODate("2024-11-19T12:00:00Z"),
    client: {
        name: "Jane Smith",
        email: "jane.smith@example.com"
    }
});

// caso negativo 1
use('restaurantdb')
db.reviews.insertOne({
    restaurant_id: "123456",
    title: "This is a very long title that exceeds the allowed limit of 50 characters",
    score: 4,
    review: "Good food but service could improve.",
    date: ISODate("2024-11-20T19:00:00Z"),
    client: {
        name: "Alice Johnson",
        email: "alice.johnson@example.com"
    }
});
// Error: El título excede los 50 caracteres.

// caso negativo 2
use('restaurantdb')
db.reviews.insertOne({
    restaurant_id: "654321",
    title: "Terrible experience",
    score: 6, // Error: Puntaje fuera de rango (máximo es 5)
    review: "Food was cold and service was rude.",
    date: ISODate("2024-11-20T20:00:00Z"),
    client: {
        name: "Bob Brown",
        email: "bob.brown@example.com"
    }
});

