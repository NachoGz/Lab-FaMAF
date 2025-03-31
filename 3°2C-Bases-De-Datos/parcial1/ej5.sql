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
							BETWEEN b.check_in AND b.check_out
							AND (b.status != "confirmed"
							OR b.status != "pending")) THEN 
		INSERT INTO 
			bookings (property_id, user_id, check_in, check_out, total_price)
		VALUES 
			(property_id, user_id, new_check_in, new_check_out, 1);
	END IF;
END;$$

DELIMITER ;
