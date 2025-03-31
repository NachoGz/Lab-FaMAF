-- 1.
select e.officeCode, count(e.officeCode) as amountEmployees
from employees e 
group by e.officeCode

-- 2. ¿Cuál es el promedio de órdenes hechas por oficina?
select o.officeCode, count(ord.orderNumber) / count(DISTINCT o.officeCode) as avgOrderPerOffice
from offices o
join employees e on e.officeCode = o.officeCode
join customers c on c.salesRepEmployeeNumber = e.employeeNumber 
join orders ord on ord.customerNumber = c.customerNumber 
group by o.officeCode 

-- 2. ¿Qué oficina vendió la mayor cantidad de productos? (opcion 1)
explain analyze select off.officeCode, sum(ord.quantityOrdered) totalQuantity
from offices off
join employees e on e.officeCode = off.officeCode
join customers c on c.salesRepEmployeeNumber = e.employeeNumber 
join orders o on o.customerNumber = c.customerNumber 
join orderdetails ord on ord.orderNumber = o.orderNumber 
GROUP by off.officeCode
order by totalQuantity DESC 
limit 1;

-- 2. ¿Qué oficina vendió la mayor cantidad de productos? (opcion 2)
explain analyze SELECT officeCode, SUM(quantityOrdered) AS total_products_sold
FROM (
    SELECT e.officeCode, od.quantityOrdered
    FROM orders ord
    JOIN customers c ON ord.customerNumber = c.customerNumber
    JOIN employees e ON c.salesRepEmployeeNumber = e.employeeNumber
    JOIN orderdetails od ON ord.orderNumber = od.orderNumber
) AS product_sales_per_office
GROUP BY officeCode
ORDER BY total_products_sold DESC
LIMIT 1;

-- 3.
explain analyze SELECT monthname(p.paymentDate) as month, avg(p.amount) as avgPay, max(p.amount) as maxPay, min(p.amount) as minPay
from payments p 
group by month

-- 4
delimiter $$
CREATE procedure update_credit
(
	customer_id int, 
	new_credit_limit decimal(10,2)
)
begin
	update customers
	set creditLimit = new_credit_limit
	where customerNumber = customer_id;
end;
delimiter;

-- 5.
create view premium_customers as
select c.customerName, c.city, sum(p.amount) as totalSpent
from customers c 
join payments p on c.customerNumber = p.customerNumber
group by c.customerName, c.city
order by totalSpent DESC 
limit 10;

-- 6.
delimiter $$
create function employee_of_the_month
(
	month varchar(20),
	year int
)
returns varchar(255)
deterministic
begin
	declare full_name varchar(255);
	select concat(e.firstName, ' ', e.lastName) into full_name
	from employees e 
	join customers c on c.salesRepEmployeeNumber = e.employeeNumber 
	join orders ord on c.customerNumber = ord.customerNumber 
	where MONTHNAME(ord.orderDate) = month
	and YEAR(ord.orderDate) = yeat
	group by e.firstName, e.lastName 
	order by amountOrders desc
	limit 1;

	return full_name;
end;
delimiter ;

-- 7.
drop table if exists product_refillment;
create table product_refillment
(
	refillmentID int not null auto_increment,
	productCode varchar(15) not null,
	orderDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
	quantity int not null,
	primary key (refillmentID),
	foreign key (productCode) references products(productCode)
);

-- 8.
create trigger restock_product
after insert on orderdetails
for each row
BEGIN
	-- Obtengo el stock actual
	declare current_stock int;
	select quantityInStock into current_stock
	from products p 
	where productCode = NEW.productCode;

	-- Verifico si es menor a 10 y si es asi genero un pedido de 10 nuevos productos
	if current_stock < 10 then
		insert into product_refillment (productCode, quantity)
		values (new.productCode, 10);
	end if;
END;


-- 9.
create role 'empleado';

grant SELECT on classicmodels.* to 'empleado';

grant create view on classicmodels.* to 'empleado';

show grants for 'empleado';

-- 10.
select 
	c.customerName, 
	c.contactFirstName, 
	min(DATEDIFF(p1.paymentDate, p2.paymentDate)) as minDiffPayment, 
	max(DATEDIFF(p1.paymentDate, p2.paymentDate)) as maxDiffPayment
from customers c 
join payments p1 on c.customerNumber = p1.customerNumber 
join payments p2 on c.customerNumber = p2.customerNumber
where 
	c.city like "N%"
-- 	and p1.paymentDate != p2.paymentDate
	and p1.paymentDate > p2.paymentDate 
group by c.customerName, c.contactFirstName;

-- 11.
select 
	p.productName, 
	sum(od.quantityOrdered) as totalProducts
from 
	products p 
join 
	orderdetails od 
	on p.productCode = od.productCode
GROUP by 
	p.productName
having 
	(sum(od.quantityOrdered) / (
		select sum(od2.quantityOrdered)
		from orderdetails od2
		)) * 100 >= 4
order by 
	totalProducts DESC;
	
select *
from (
	select 
		ROW_NUMBER() over(order by sum(od.quantityOrdered) desc) as rowNumber,
		p.productName, 
		sum(od.quantityOrdered) as totalProducts
	from 
		products p 
	join 
		orderdetails od 
		on p.productCode = od.productCode
	GROUP by 
		p.productName
	order by
		totalProducts DESC 
) as rankedProducts
where 
	rowNumber <= 10;
	
select sum(quantityOrdered) as totalProducts
from
	orderdetails o 
