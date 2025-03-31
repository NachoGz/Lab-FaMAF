/**
 * 1. Especificar en la colección users las siguientes reglas de validación: El campo name
 * (requerido) debe ser un string con un máximo de 30 caracteres, email (requerido) debe
 * ser un string que matchee con la expresión regular: "^(.*)@(.*)\\.(.{2,4})$" ,
 * password (requerido) debe ser un string con al menos 50 caracteres.
 */

db.users.drop()
db.createCollection("users", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["name", "email", "password"],
            properties: {
                name: {
                    bsonType: "string",
                    maxLength: 30,
                    description: "must be a string with a maximum of 30 characters"
                },
                email: {
                    bsonType: "string",
                    pattern: "^(.*)@(.*)\\.(.{2,4})$",
                    description: "must be a valid email address"
                },
                password: {
                    bsonType: "string",
                    minLength: 50,
                    description: "must be a string with at least 50 characters"
                }
            }
        }
    },
    validationLevel: "strict",
    validationAction: "error"
})

use('mflix')
db.runCommand({
    collMod: "users",
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["name", "email", "password"],
            properties: {
                name: {
                    bsonType: "string",
                    maxLength: 30,
                    description: "must be a string with a maximum of 30 characters"
                },
                email: {
                    bsonType: "string",
                    pattern: "^(.*)@(.*)\\.(.{2,4})$",
                    description: "must be a valid email address"
                },
                password: {
                    bsonType: "string",
                    minLength: 50,
                    description: "must be a string with at least 50 characters"
                }
            }
        }
    },
    validationLevel: "strict",
    validationAction: "error"
})

// example valid inserts
use('mflix')
db.users.insertMany([
    { name: "Alice", email: "alice@example.com", password: "x".repeat(50) },
    { name: "Bob", email: "bob@example.net", password: "y".repeat(50) },
    { name: "Charlie", email: "charlie@example.org", password: "z".repeat(50) },
    { name: "Dana", email: "dana@example.co", password: "p".repeat(50) },
    { name: "Eve", email: "eve@example.us", password: "q".repeat(50) }
])

// example invalid inserts
use('mflix')
db.users.insertMany([
    { name: "This name is way too long for the validation rules", email: "longname@example.com", password: "x".repeat(50) },  // Invalid name length
    { name: "Frank", email: "frankexample.com", password: "y".repeat(50) },  // Invalid email format
    { name: "Grace", email: "grace@example.co", password: "short" },  // Invalid password length
    { name: "Henry", email: "henry@exam", password: "z".repeat(50) },  // Invalid email format (missing TLD)
    { email: "missingname@example.org", password: "q".repeat(50) }  // Missing required "name" field
])

/**
 * 2. Obtener metadata de la colección users que garantice que las reglas de validación
 * fueron correctamente aplicadas
 */
use('mflix')
db.getCollectionInfos({ name: "users" })

/**
 * 3. Especificar en la colección theaters las siguientes reglas de validación: El campo 
 * theaterId (requerido) debe ser un int y location (requerido) debe ser un object con:
 * * a. un campo address (requerido) que sea un object con campos street1, city, state y 
 * * * zipcode todos de tipo string y requeridos
 * * b. un campo geo (no requerido) que sea un object con un campo type, con valores 
 * * * posibles “Point” o null y coordinates que debe ser una lista de 2 doubles

Por último, estas reglas de validación no deben prohibir la inserción o actualización de
documentos que no las cumplan sino que solamente deben advertir
 */
use('mflix')
db.runCommand({
    collMod: "theater",
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["theaterId", "location"],
            properties: {
                theaterId: {
                    bsonType: "int"
                },
                location: {
                    bsonType: "object",
                    required: ["address"],
                    properties: {
                        address: {
                            bsonType: "object",
                            required: ["street1", "city", "state", "zipcode"],
                            properties: {
                                street1: { bsonType: "string", description: "Debe ser un string y es requerido" },
                                city: { bsonType: "string", description: "Debe ser un string y es requerido" },
                                state: { bsonType: "string", description: "Debe ser un string y es requerido" },
                                zipcpde: { bsonType: "string", description: "Debe ser un string y es requerido" }
                            }
                        },
                        geo: {
                            bsonType: "object",
                            required: ["type", "coordinates"],
                            properties: {
                                type: {
                                    enum: ["Point", null],
                                    description: "Debe ser 'Point' o null"
                                },
                                coordinates: {
                                    bsonType: "array",
                                    items: [
                                        { bsonType: "double"},
                                        { bsonType: "double"}
                                    ],
                                    description: "Debe ser una lista de dos doubles"
                                }
                            }
                        }
                    }
                }

            }
        }
    },
    validationLevel: "strict",
    validationAction: "warn"
})

/**
 * 4. Especificar en la colección movies las siguientes reglas de validación: El campo title
 * (requerido) es de tipo string, year (requerido) int con mínimo en 1900 y máximo en 3000,
 * y que tanto cast, directors, countries, como genres sean arrays de strings sin duplicados.
 * * a. Hint: Usar el constructor NumberInt() para especificar valores enteros a la hora
 * * * de insertar documentos. Recordar que mongo shell es un intérprete javascript y
 * * * en javascript los literales numéricos son de tipo Number (double)
 */
use('mflix')
db.runCommand({
    collMod: "movies",
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["title", "year"],
            properties: {
                title: {
                    bsonType: "string",
                    description: "Debe ser una string",
                },
                year: {
                    bsonType: "int",
                    minimum: 1900,
                    maximum: 3000,
                    description: "Debe ser un entero entre 1900 y 3000"
                },
                cast: {
                    bsonType: "array",
                    uniqueItems: true,
                    items: [
                        { bsonType: "string", description: "Cada elemento debe ser un string" }
                    ],
                    description: "Debe ser un array de strings sin duplicados"
                },
                directors: {
                    bsonType: "array",
                    uniqueItems: true,
                    items: [
                        { bsonType: "string", description: "Cada elemento debe ser un string" }
                    ],
                    description: "Debe ser un array de strings sin duplicados"
                },
                countries: {
                    bsonType: "array",
                    uniqueItems: true,
                    items: [
                        { bsonType: "string", description: "Cada elemento debe ser un string" }
                    ],
                    description: "Debe ser un array de strings sin duplicados"
                },
                genres: {
                    bsonType: "array",
                    uniqueItems: true,
                    items: [
                        { bsonType: "string", description: "Cada elemento debe ser un string" }
                    ],
                    description: "Debe ser un array de strings sin duplicados"
                }
            }
        }
    }
})


/**
 * 5. Crear una colección userProfiles con las siguientes reglas de validación: Tenga un
 * campo user_id (requerido) de tipo “objectId”, un campo language (requerido) con alguno
 * de los siguientes valores [ “English”, “Spanish”, “Portuguese” ] y un campo
 * favorite_genres (no requerido) que sea un array de strings sin duplicados.
 */
use('mflix')
db.createCollection("userProfiles", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["user_id", "language"],
            properties: {
                user_id: {
                    bsonType: "objectId",
                    description: "Deber ser un objectId"
                },
                language: {
                    enum: ["English", "Spanish", "Portuguese"],
                    description: "Debe ser 'English', 'Spanish' o 'Portuguese'"
                },
                favorite_genres: {
                    bsonType: "array",
                    uniqueItems: true,
                    items: [
                        { bsonType: "string", description: "Cada elemento debe ser un string" }
                    ],
                    description: "Debe ser un array de strings sin duplicados"
                }
            }
        }
    }
})
