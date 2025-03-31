-- 1.
CREATE table directors (
	first_name varchar(255),
	last_name varchar(255),
	numberOfFilms int
	)


-- 2.
insert into directors (first_name, last_name, numberOfFilms)
select a.first_name, a.last_name, count(fa.actor_id) as totalFilms
from actor a
join film_actor fa on a.actor_id = fa.actor_id 
group by fa.actor_id, a.first_name, a.last_name 
order by totalFilms desc 
limit 5;

-- 3.
alter table customer 
add column premium_customer enum('T', 'F') NOT NULL DEFAUlT 'F'

-- 4.
with topCustomers as (
	select c.customer_id
	from customer c
	join payment p 
	on c.customer_id = p.customer_id 
	group by c.customer_id , c.first_name, c.last_name
	order by sum(p.amount) desc
	limit 10	
)
update customer 
set premium_customer = 'T'
where customer_id in (select customer_id from topCustomers);

-- 5.
select f.rating, count(f.film_id) as numberOfFilms
from film f
group by f.rating 
order by numberOfFilms Desc

-- 6.
select min(p.payment_date) as firstPayment, max(p.payment_date) as lastPayment
from payment p 

-- 7.
select monthname(p.payment_date) as month, avg(p.amount) as avgPay
from payment p 
group by month
order by month asc

-- 8.
SELECT a.district, count(r.rental_id) as totalRentals
from customer c 
join rental r on c.customer_id = r.customer_id 
join address a on c.address_id = a.address_id 
GROUP by a.district 
order by totalRentals DESC 
limit 10;

-- 9.
ALTER table inventory 
add column stock int not null default 5

-- 10.
DELIMITER $$

CREATE TRIGGER update_stock
AFTER INSERT ON rental
FOR EACH ROW
BEGIN
  -- Actualizar el stock de la película rentada en la tabla inventory
  UPDATE inventory
  SET stock = stock - 1  -- Restar una copia al stock
  WHERE inventory_id = NEW.inventory_id;
END$$

DELIMITER ;

-- 11.
create table fines (
	rental_id int,
	amount DECIMAL(10,2),
	primary key (rental_id),
	constraint fk_rental
		foreign key (rental_id) references rental(rental_id)
		on delete cascade
);

-- 12.
DELIMITER $$

CREATE PROCEDURE check_date_and_fine()
BEGIN
  -- Insertar multas para los alquileres con retraso
  INSERT INTO fines (rental_id, amount)
  SELECT rental_id, (DATEDIFF(return_date, rental_date) - 3) * 1.5 AS fine_amount
  FROM rental
  WHERE return_date IS NOT NULL
    AND DATEDIFF(return_date, rental_date) > 3;
END$$

DELIMITER ;

-- 13.
-- Crear el rol `employee`
CREATE ROLE 'employee';

-- Asignar permisos de inserción, eliminación y actualización a `employee` sobre la tabla `rental`
GRANT INSERT, DELETE, UPDATE ON sakila.rental TO 'employee';

-- 14.
-- Revocar el permiso de eliminación al rol `employee`
REVOKE DELETE ON sakila.rental FROM 'employee';

-- Crear el rol `administrator`
CREATE ROLE 'administrator';

-- Asignar todos los privilegios al rol `administrator` sobre la base de datos `sakila`
GRANT ALL PRIVILEGES ON sakila.* TO 'administrator';

-- 15.
-- Crear dos usuarios
CREATE USER 'employee1'@'localhost' IDENTIFIED BY 'password1';
CREATE USER 'employee2'@'localhost' IDENTIFIED BY 'password2';

-- Asignar el rol `employee` al primer usuario
GRANT 'employee' TO 'employee1'@'localhost';

-- Asignar el rol `administrator` al segundo usuario
GRANT 'administrator' TO 'employee2'@'localhost';

-- Activar el rol correspondiente a cada usuario por defecto
SET DEFAULT ROLE 'employee' TO 'employee1'@'localhost';
SET DEFAULT ROLE 'administrator' TO 'employee2'@'localhost';




