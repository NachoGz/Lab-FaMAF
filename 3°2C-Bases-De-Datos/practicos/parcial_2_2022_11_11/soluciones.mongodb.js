/**
 * 1. Buscar los clientes que no tengan el campo active y que o bien posean más de 4 
 * cuentas o bien nacieron entre Abril de 1995 y Marzo de 1997 inclusives. Listar el 
 * nombre, email, fecha de nacimiento y cantidad de cuentas. Limitar el resultado a los 
 * 50 primeros clientes de acuerdo al orden alfabético.
 */
use('analytics')
db.customers.aggregate([
    {
        $match: {
            active: { $exists: false },
            $or: [
                { "accounts.4": { $exists: true } },
                { birthdate: { $gte: ISODate('1995-04-01T00:00:00Z'), $lte: ISODate('1997-03-31T23:59:59Z') } }
            ]
        }
    },
    {
        $project: {
            name: 1,
            email: 1,
            birthdate: 1,
            numberOfAccounts: { $size: "$accounts" },
            _id: 0
        }
    },
    {
        $sort: { name: 1 }
    },
    {
        $limit: 50
    }
]);

// alt con $size
use('analytics')
db.customers.aggregate([
    {
        $match: {
            active: { $exists: false },
            $or: [
                { $expr: { $gte: [{ $size: "$accounts" }, 5] } }, // At least 5 elements in "accounts"
                { birthdate: { $gte: ISODate('1995-04-01T00:00:00Z'), $lte: ISODate('1997-03-31T23:59:59Z') } }
            ]
        }
    },
    {
        $project: {
            name: 1,
            email: 1,
            birthdate: 1,
            numberOfAccounts: { $size: "$accounts" },
            _id: 0
        }
    },
    {
        $sort: { name: 1 }
    },
    {
        $limit: 50
    }
]);


/**
 * 2. Actualizar las cuentas que tengan un límite entre 8000 y 9000 inclusives, 
 * agregando un nuevo campo "class" con el valor "A" si la cuenta tiene hasta dos 
 * productos y con el valor "B" si tiene 3 o más productos.
 */
use('analytics')
db.accounts.updateMany(
    {
        limit: { $gte: 8000, $lte: 9000}
    },
    [
        {
            $set: {
                class: {
                    $cond: {
                        if: { $lte: [ { $size:  "$products" }, 2] },
                        then: "A",
                        else: "B"
                    }
                }
            }
        }
    ]
);

/**
 * 3. Buscar las transacciones donde la cantidad de transacciones sea mayor a 94. 
 * Listar id de transacción, id de la cuenta, y solo aquellas transacciones que tengan el 
 * código de transacción igual a "buy" y con "total" mayor a 500000. Mostrar el 
 * resultado ordenados por el id de la cuenta en orden decreciente. 
 * HINTS: (i) El operador $filter puede ser de utilidad. (ii) Notar que el valor del campo 
 * total está en string y requiere conversión.
 */

use('analytics')
db.transactions.aggregate([
    {
        $match: {
            transaction_count: { $gt: 94 }
        }
    },
    {
        $project: {
            _id: 1,
            account_id: 1,
            filtered_transactions: {
                $filter: {
                    input: "$transactions",
                    as: "transaction",
                    cond: {
                        $and: [
                            { $eq: ["$$transaction.transaction_code", "buy"] },
                            { 
                                $gt: [
                                    { $convert: { input: "$$transaction.total", to: "double", onError: 0 } },
                                    500000
                                ]
                            }
                        ]
                    }
                }
            }
        }
    },
    {
        $match: {
            "filtered_transactions.0": { $exists: true }
        }
    },
    {
        $sort: {
            account_id: -1
        }
    }
]);

/**
 * 4. Crear la vista "transactionCountByCode" que lista el id de transacción, id de la 
 * cuenta, cantidad de transacciones, cantidad de transacciones de compra 
 * (transacciones con transaction_code igual a buy) y cantidad de transacciones de 
 * venta (transacciones con transaction_code igual a sell). Listar el resultado 
 * ordenados por cantidad de transacciones (orden decreciente).
 */
use('analytics')
db.createView(
    "transactionCountByCode",
    "transactions",
    [
        {
            $project: {
                transaction_id: 1,
                account_id: 1,
                // total_transactions: { $size: "$transactions" },
                transaction_count: 1,
                buy_transactions: {
                    $size: {
                        $filter: {
                            input: "$transactions",
                            as: "transaction",
                            cond: { $eq: ["$$transaction.transaction_code", "buy"] }
                        }
                    }
                },
                sell_transactions: {
                    $size: {
                        $filter: {
                            input: "$transactions",
                            as: "transaction",
                            cond: { $eq: ["$$transaction.transaction_code", "sell"] }
                        }
                    }
                }
            }
        },
        {
            $sort: { transaction_count: -1 }
        }
    ]
);


/**
 * 5. Calcular la suma total, suma total de ventas y suma total de compras de las
 * transacciones realizadas por año y mes. Mostrar el resultado en orden cronológico.
 * No se debe mostrar resultados anidados en el resultado.
 * HINT: El operador $cond o $switch puede ser de utilidad 
 */
use('analytics')
db.transactions.aggregate([
    {
        $unwind: "$transactions"
    },
    {
        $addFields: {
            year: { $year: "$transactions.date" },
            month: { $month: "$transactions.date" },
            total_as_double: {
                $convert: { input: "$transactions.total", to: "double", onError: 0 }
            }
        }
    },
    {
        $group: {
            _id: { year: "$year", month: "$month" },
            total_sum: { $sum: "$total_as_double" },
            total_buy: {
                $sum: {
                    $cond: [
                        { $eq: ["$transactions.transaction_code", "buy"] },
                        "$total_as_double",
                        0
                    ]
                }
            },
            total_sell: {
                $sum: {
                    $cond: [
                        { $eq: ["$transactions.transaction_code", "sell"] },
                        "$total_as_double",
                        0
                    ]
                }
            }
        }
    },
    {
        $project: {
            year: "$_id.year",
            month: "$_id.month",
            total_sum: 1,
            total_buy: 1,
            total_sell: 1,
            _id: 0
        }
    },
    {
        $sort: { year: 1, month: 1 }
    }
]);


/**
 * 6. Especificar reglas de validación en la colección transactions (a) usando JSON
 * Schema a los campos: account_id, transaction_count, bucket_start_date,
 * bucket_end_date y transactions ( y todos sus campos anidados ). Inferir los tipos y
 * otras restricciones que considere adecuados para especificar las reglas a partir de
 * los documentos de la colección. (b) Luego añadir una regla de validación tal que
 * bucket_start_date debe ser menor o igual a bucket_end_date. (c) Testear la regla de
 * validación generando dos casos de falla en la regla de validación y dos casos donde
 * cumple la regla de validación. Aclarar en la entrega cuales son los casos que fallan y
 * cuales cumplen la regla de validación. Los casos no deben ser triviales
 */
use('analytics')
db.runCommand({
    collMod: "transactions",
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["account_id", "transaction_count", "bucket_start_date", "bucket_end_date", "transactions"],
            properties: {
                account_id: { bsonType: "int", description: "Debe ser un entero." },
                transaction_count: { bsonType: "int", minimum: 0, description: "Debe ser un entero positivo." },
                bucket_start_date: { bsonType: "date", description: "Debe ser una fecha válida." },
                bucket_end_date: { bsonType: "date", description: "Debe ser una fecha válida." },
                transactions: {
                    bsonType: "array",
                    items: {
                        bsonType: "object",
                        required: ["transaction_code", "total", "date", "amount", "price", "symbol"],
                        properties: {
                            transaction_code: { bsonType: "string", description: "Debe ser un string." },
                            total: { bsonType: "string", description: "Debe ser un string numérico." },
                            date: { bsonType: "date", description: "Debe ser una fecha válida." },
                            amount: { bsonType: "int", minimum: 0, description: "Debe ser un entero positivo." },
                            price: { bsonType: "string", description: "Debe ser un string numérico." },
                            total: { bsonType: "string", description: "Debe ser un string." },   
                        }
                    }
                }
            }
        }
    },
    validationLevel: "strict",
    validationAction: "error"
});


/**
 * 7. Listar el username del cliente, cuentas y sus transacciones más recientes de cada cuenta asociada
 */

use('analytics')
db.customers.aggregate([
    {
        $lookup: {
            from: "transactions", 
            localField: "accounts", 
            foreignField: "account_id", 
            as: "customer_transactions" 
        }
    },
    {
        $project: {
            username: 1, 
            accounts: 1, 
            most_recent_transactions: { 
                $map: { 
                    input: "$accounts", 
                    as: "account", 
                    in: { 
                        account_id: "$$account", 
                        most_recent_transaction: { 
                            $arrayElemAt: [ 
                                { 
                                    $sortArray: { 
                                        input: {
                                            $filter: {
                                                input: "$customer_transactions", 
                                                cond: { $eq: ["$$this.account_id", "$$account"] } 
                                            }
                                        },
                                        sortBy: { date: -1 }
                                    }
                                }, 
                                0 
                            ] 
                        } 
                    } 
                } 
            } 
        }
    },
    { $sort: { username: 1 } }
]);
