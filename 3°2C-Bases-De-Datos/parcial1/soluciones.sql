-- 1.
SELECT
	p.*, count(r.id) AS amntReviews
FROM
	properties p 
JOIN reviews r ON p.id = r.property_id 
WHERE year(r.created_at) = 2024
GROUP BY p.id
ORDER BY amntReviews DESC
LIMIT 7;

-- 2.
SELECT 
	p.*, sum(DATEDIFF(b.check_out, b.check_in) * p.price_per_night) AS totalIncome
FROM
	properties p 
JOIN bookings b ON p.id = b.property_id 
GROUP BY 
	p.id;

-- 3.
SELECT 
	u.*, sum(p.amount) AS totalPaid
FROM 
	users u 
JOIN 
	payments p ON u.id = p.user_id 
GROUP BY 
	u.id, u.name
ORDER BY 
	totalPaid DESC 
LIMIT 10;


-- 4.
DELIMITER $$
CREATE TRIGGER notify_host_after_booking
AFTER INSERT ON bookings
FOR EACH ROW
BEGIN
	-- Agrego una entrada en la table messages
	INSERT INTO messages (sender_id, receiver_id, property_id, content)
	SELECT 
		NEW.user_id, p.owner_id, p.id, "New booking on property"
	FROM 
		properties p
	WHERE NEW.property_id = p.id;
END;$$

DELIMITER ;


-- 5. 
DELIMITER $$
CREATE PROCEDURE add_new_booking
(
	property_id int,
	user_id int,
	new_check_in date,
	new_check_out date
)
BEGIN
	IF property_id NOT IN (SELECT 
								b.property_id 
							FROM 
								bookings b 
							WHERE 
								new_check_in 
							-- me fijo que el que no haya una fecha ya asignada para esa propiedad
							BETWEEN b.check_in AND b.check_out
							-- me fijo que no haya una reserva cancelada
							AND b.status != "canceled") THEN 
		INSERT INTO 
			bookings (property_id, user_id, check_in, check_out, total_price)
		VALUES 
			(property_id, user_id, new_check_in, new_check_out, 0, "confirmed"); -- total_price en 0 porque dijo el profesor Ramiro
	END IF;
END;$$

DELIMITER ;


-- 6.
-- Creo el rol admin
CREATE ROLE "admin";

-- Le asigno permisos
GRANT CREATE ON airbnb_like_db.properties TO "admin";
GRANT UPDATE (status) ON airbnb_like_db.property_availability TO "admin";


-- 7.
/* 
 * Las propiedas ACID son por Atomicity, Consistency, Isolation y Durability
 * Al realizar la operacion UPDATE dentro de una transacciones y luego hacer COMMIT, estoy indicando que quiero que los cambios realizados dentro del bloque de transaction sean
 * permanentes en la base de datos, esto me asegura atomicidad. Ahora, luego de que se realiza una transaccion la base de datos vuelve a un estado consistente. Luego, todas las 
 * operaciones dentro las transacciones estan aisladas del resto y no son afectadas por otras transacciones y de nuevo como con el commit estoy indicando que quiero que estos 
 * cambios se realizen permanentemente, esto asgura la durabilidad. En conclusion, como la operacion de update esta siendo realizada dentro del bloque TRANSACTION y luego se 
 * usa la operacion COMMIT, no esta contradiciendo los principios ACID.
 */

-- testing
-- 4
-- insert into bookings (property_id, user_id, check_in, check_out, total_price, status, created_at)
-- values (1618, 1747, '2024-10-04', '2024-10-6', 500, "confirmed", '2024-10-03 16:24:46')
-- 
-- -- 5
-- call add_new_booking(1604, 1747, '2024-10-04', '2024-10-6')
-- 
-- select * from bookings b 
-- where b.property_id = 1603;
-- 
-- SELECT 
-- 	b.property_id 
-- FROM 
-- 	bookings b 
-- WHERE 
-- 	b.check_out BETWEEN '2024-10-04' and '2024-10-6';
-- 
-- -- 6
-- show grants for "admin";


