DELIMITER $$
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
END$$

DELIMITER ;
