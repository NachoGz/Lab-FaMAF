/**
 * 1. Buscar las ventas realizadas en "London", "Austin" o "San Diego"; a un customer con
 * edad mayor-igual a 18 años que tengan productos que hayan salido al menos 1000
 * y estén etiquetados (tags) como de tipo "school" o "kids" (pueden tener más etiquetas). 
 * Mostrar el id de la venta con el nombre "sale", la fecha (“saleDate"), el storeLocation,
 * y el "email del cliente. No mostrar resultados anidados
 */

use('supplies')
db.sales.find(
    {
        storeLocation: { $in: ["London", "Austin", "San Diego"] },
        "customer.age": { $gte: 18 },
        "items": {
            $elemMatch: {
            "price": { $gte: 1000 },
            "tags": { $in: ["school", "kids"] }
            }
        }
    },
    {
        sale: "$_id",
        saleDate: 1,
        storeLocation: 1,
        "customer.email": 1,
        _id: 0
    }
);


/**
 * 2. Buscar las ventas de las tiendas localizadas en Seattle, donde el método de compra
 * sea 'In store' o 'Phone' y se hayan realizado entre 1 de febrero de 2014 y 31 de enero
 * de 2015 (ambas fechas inclusive). Listar el email y la satisfacción del cliente, y el
 * monto total facturado, donde el monto de cada item se calcula como 'price *
 * quantity'. Mostrar el resultado ordenados por satisfacción (descendente), frente a
 * empate de satisfacción ordenar por email (alfabético).
 */
use('supplies')
db.sales.aggregate([
    // 1. Filtrar las ventas en Seattle con métodos de compra válidos y fechas en el rango
    {
        $match: {
            storeLocation: "Seattle",
            purchaseMethod: { $in: ["In store", "Phone"]},
            saleDate: {
                $gte: ISODate('2014-02-01T00:00:00.000Z'),
                $lte: ISODate('2015-01-31T23:59:59.999Z')
            }
        }
    },
    // 2. Calcular el monto total facturado por venta
    {
        $addFields: {
          total_facturado: {
            $sum: {
              $map: {
                input: "$items",
                as: "item",
                in: { $multiply: ["$$item.price", "$$item.quantity"] }
              }
            }
          }
        }
    },
    // 3. Proyectar los campos necesarios: email, satisfacción y monto total facturado
    {
        $project: {
          _id: 0,
          email: 1,
          customerSatisfaction: "$customer.satisfaction",
          total_facturado: 1
        }
    },
    // 4. Ordenar los resultados
    {
        $sort: {
            customerSatisfaction: -1,   // Descendente por satisfacción 
            email: 1                    // Alfabéticamente por email en caso de empate
            
        }
    }
])

/**
 * 3. Crear la vista salesInvoiced que calcula el monto mínimo, monto máximo, monto
 * total y monto promedio facturado por año y mes. Mostrar el resultado en orden
 * cronológico. No se debe mostrar campos anidados en el resultado.
*/

use('supplies')
db.createView("salesInvoiced", "sales", [
    {
        $unwind: "$items"
    },
    {
        $group: {
            _id: {
                year: { $year: "$saleDate" },
                month: { $month: "$saleDate" }
            },
            total_amount: { $sum: "$items.price" },
            avg_amount: { $avg: "$items.price"},
            min_amount: { $min: "$items.price"},
            max_amount: { $max: "$items.price"},
        }
    },
    {
        $project: {
            year: "$_id.year",
            month: "$_id.month",
            total_amount: { $toDouble: "$total_amount" },
            avg_amount: { $toDouble: "$avg_amount" },
            min_amount: { $toDouble: "$min_amount" },
            max_amount: { $toDouble: "$max_amount" },
            _id: 0
        }
    },
    {
        $sort: { year: 1, month: 1 }
    }
])

// db.sales.aggregate([
//     {
//         $unwind: "$items"
//     },
//     {
//         $group: {
//             _id: {
//                 year: { $year: "$saleDate" },
//                 month: { $month: "$saleDate" }
//             },
//             total_amount: { $sum: "$items.price" },
//             avg_amount: { $avg: "$items.price"},
//             min_amount: { $min: "$items.price"},
//             max_amount: { $max: "$items.price"},
//         }
//     },
//     {
//         $project: {
//             year: "$_id.year",
//             month: "$_id.month",
//             total_amount: { $toDouble: "$total_amount" },
//             avg_amount: { $toDouble: "$avg_amount" },
//             min_amount: { $toDouble: "$min_amount" },
//             max_amount: { $toDouble: "$max_amount" },
//             _id: 0
//         }
//     },
//     {
//         $sort: { year: 1, month: 1 }
//     }
// ])


/**
 * 4. Mostrar el storeLocation, la venta promedio de ese local, el objetivo a cumplir de
 * ventas (dentro de la colección storeObjectives) y la diferencia entre el promedio y el
 * objetivo de todos los locales.
 */
use('supplies')
db.sales.aggregate([
    {
        $lookup: {
          from: "storeObjectives",
          localField: "storeLocation",
          foreignField: "_id",
          as: "sales"
        }
    },
    {
        $unwind: "$sales"
    },
    {
        $unwind: "$items"
    },
    {
        $group: {
            _id: "$storeLocation",
            avg_sold: { $avg: "$items.price"},
            objective: { $first: "$sales.objective" }
        }
    },
    {
        $project: {
            storeLocation: "$_id",
            avg_sold: { $toDouble: "$avg_sold" },
            objective: 1,
            diff: { $abs: { $subtract: [{ $toDouble: "$avg_sold" }, { $toDouble: "$objective" }] } },
            _id: 0
        }
    }
])

/**
 * 5. Especificar reglas de validación en la colección sales utilizando JSON Schema.
 * a. Las reglas se deben aplicar sobre los campos: saleDate, storeLocation, 
 * * purchaseMethod, y customer ( y todos sus campos anidados ). Inferir los 
 * * tipos y otras restricciones que considere adecuados para especificar las reglas a partir de los documentos de la colección.
 * b. Para testear las reglas de validación crear un caso de falla en la regla de
 * * validación y un caso de éxito (Indicar si es caso de falla o éxito
 */
use('supplies')
db.runCommand({
    collMod: "sales",
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["saleDate", "storeLocation", "purchaseMethod", "customer"],
            properties: {
                saleDate: {
                    bsonType: "date",
                    description: "Debe ser una fecha válida y es obligatoria."
                },
                storeLocation: {
                    bsonType: "string",
                    enum: ["London", "New York", "Denver", "San Diego", "Austin", "Seattle"],
                    description: "Debe ser una de las ubicaciones disponibles y es obligatoria."
                },
                purchaseMethod: {
                    bsonType: "string",
                    enum: ["Online", "Phone", "In Store"],
                    description: "Debe ser uno de los metodos posibles y es obligatorio."
                },
                customer: {
                    bsonType: "object",
                    required: ["age", "email", "satisfaction"],
                    properties: {
                        age: {
                            bsonType: "int",
                            minimum: 18,
                            maximum: 120,
                            description: "Debe ser un numero entero entre 18 y 120."
                        },
                        email: {
                            bsonType: "string",
                            pattern: "^(.*)@(.*)\\.(.{2,4})$",
                            description: "Debe ser un email valido de tipo user@mail.com y es obligatorio."
                        },
                        satisfaction: {
                            bsonType: "int",
                            minimum: 1,
                            maximum: 5,
                            description: "Debe ser un numero entero entre 1 y 5."
                        }
                    },
                    description: "Debe incluir todos los campos requeridos del cliente."
                }
            }
        }
    },
    validationLevel: "strict",
    validationAction: "error"
});


// caso positivo
use('supplies')
db.sales.insertOne({
    saleDate: new Date("2024-01-15"),
    storeLocation: "Seattle",
    purchaseMethod: "Online",
    customer: {
        email: "example@test.com",
        age: 30,
        satisfaction: 5
    }
});

// caso negativo
use('supplies')
db.sales.insertOne({
    saleDate: "2024-01-15", // Error: No es un tipo date.
    storeLocation: "Unknown", // Error: Valor fuera de los permitidos.
    purchaseMethod: "Fax", // Error: Valor fuera de los permitidos.
    customer: {
        email: "invalid-email", // Error: No cumple con el patrón.
        age: 17, // Error: Edad menor al mínimo.
        satisfaction: 10 // Error: Valor fuera del rango permitido.
    }
});
