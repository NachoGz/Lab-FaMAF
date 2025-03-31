-- 1.
select city.Name, country.Name from city join country on city.CountryCode = country.Code where country.Population < 10000

-- 2.
SELECT * from city where Population > (SELECT avg(Population) from city)

-- 3.
select city.Name, city.Population, country.Continent 
from city 
join country 
on city.CountryCode = country.code 
where country.Continent != "Asia" 
AND city.Population >= SOME (
	SELECT country.Population 
	from country 
	where Continent = "Asia");

-- 4.
SELECT country.Name, countrylanguage.Language 
from country
join countrylanguage
on country.Code = countrylanguage.CountryCode 
where countrylanguage.IsOfficial ="F"
and countrylanguage.Percentage > ALL (
	SELECT countrylanguage.Percentage 
	from country
	join countrylanguage
	on country.Code = countrylanguage.CountryCode 
	where countrylanguage.IsOfficial ="V")
	
-- 5.
SELECT DISTINCT country.Region 
from country
where country.SurfaceArea < 1000
AND 1 <= (
	SELECT count(*)
	from country
	join city
	on country.Code = city.CountryCode
	where city.Population >= 100000)
	
-- 6.

select country.Name, max(city.Population) as maxPopulation
from country
join city
on country.Code = city.CountryCode
GROUP BY country.Name

-- 7.
SELECT country.Name, countrylanguage.Language 
from country
join countrylanguage
on country.Code = countrylanguage.CountryCode 
where countrylanguage.IsOfficial ="F"
and countrylanguage.Percentage > (
	SELECT avg(countrylanguage.Percentage) 
	from country
	join countrylanguage
	on country.Code = countrylanguage.CountryCode 
	where countrylanguage.IsOfficial ="V")
 
-- 8.
SELECT sum(country.Population) as PopCount, country.Continent 
from country
group by country.Continent 
ORDER by PopCount Desc

-- 9.
select avg(country.LifeExpectancy) as avgLifeExpectancy, country.Continent
from country
group by country.Continent 
HAVING 40 <= avgLifeExpectancy <= 70

-- 10.
select max(country.Population) as maxPop, 
	min(country.Population) as minPop, 
	avg(country.Population) as avgPop, 
	sum(country.Population) as totalPop,
	country.Continent 
from country 
group by country.Continent 


-- Parte 2
	-- 1.
	-- agrupaciones
SELECT c.Name as countryName,
	   ci.Name as cityName,
	   ci.Population as cityPopulation
from city ci
join country c on c.Code = ci.CountryCode 
join (
	Select city.CountryCode, max(city.Population) as cityMaxPop
	from city
	group by city.CountryCode 
	) as  maxCities on ci.CountryCode = maxCities.countryCode
and ci.Population = maxCities.cityMaxPop;

	-- subquery escalar
SELECT country.Name as countryName,
	   (select city.Name
	   	from city
	   	where country.Code = city.CountryCode
	   	order by city.Population DESC
	   	limit 1) as cityName,
	   	(select max(city.Population)
	   	from city 
	   	where country.Code = city.CountryCode) as maxPopulation
from country;
