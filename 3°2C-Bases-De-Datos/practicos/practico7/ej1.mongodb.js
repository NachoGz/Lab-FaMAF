/**
 * Insertar 5 nuevos usuarios en la colección users. Para cada nuevo usuario creado, insertar al menos un comentario
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


