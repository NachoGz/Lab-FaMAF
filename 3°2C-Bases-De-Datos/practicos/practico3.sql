-- 1.
select city.Name, country.Name, country.Region, country.GovernmentForm from city join country on city.CountryCode = country.Code order by city.Population desc limit 10;

-- 2.
select country.Name as CountryName, city.Name as CityName from city join country on city.CountryCode = country.Code where city.ID = country.Capital order by city.Population limit 10;

-- 3.
select country.Name, country.Continent, Language, IsOfficial from country join countrylanguage on country.Code = countrylanguage.CountryCode where IsOfficial = 'T';

-- 4.
select country.Name as CountryName, city.Name as CityName from city join country on city.CountryCode = country.Code where city.ID = country.Capital order by country.SurfaceArea desc limit 20;

-- 5.
select Name, Language, Percentage from city join countrylanguage on city.CountryCode = countrylanguage.CountryCode where countrylanguage.IsOfficial = 'T' order by city.Population;

-- 6.
(select * from country order by Population desc limit 10) union (select * from country where Population > 100 order by Population limit 10);

-- 7.
select Name
from country 
join countrylanguage on country.Code = countrylanguage.CountryCode 
where countrylanguage.IsOfficial = 'T' AND countrylanguage.Language = 'English'
union
select Name
from country 
join countrylanguage on country.Code = countrylanguage.CountryCode 
where countrylanguage.IsOfficial = 'T' AND countrylanguage.Language = 'French';

-- 8.
select Name
from country 
join countrylanguage on country.Code = countrylanguage.CountryCode 
where countrylanguage.Language = 'English' and countrylanguage.Language != 'Spanish';

-- PARTE II - PREGUNTAS
-- 1. Si devuelven lo mismo porque el country.Name = 'Argentina' en el on del join entra en la condicion de union de las filas. En la segunda query se unen en la condicion
-- de country code y luego se filtran por el country.Name = 'Argentina'. En ambos casos, el resultado es el mismo pero distintas formas de hacerlo. 

-- 2. Para la primer query para las ciudades que no estan en Argentina muestra NULL porque el country.Name esta en en el on del left join. Luego, para la segunda query, se hace el
-- join y luego se filtra por pais entonces no cambia nada el left join o inner join.


