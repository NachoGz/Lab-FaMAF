-- 2.
create table country (
	Code varchar(255),
	Name varchar(255),
	Continent varchar(255),
	Region varchar(255),
	SurfaceArea int,
	IndepYear int,
	Population float,
	LifeExpectancy int,
	GNP int,
	GNPOld int,
	LocalName varchar(255),
	GovernmentForm varchar(255),
	HeadOfState varchar(255),
	Capital int,
	Code2 varchar(255),
	primary key (Code)
);

create table city (
	ID int,
	Name varchar(255),
	CountryCode varchar(255),
	District varchar(255),
	Population float,
	primary key (ID),
	foreign key (CountryCode) references country(Code)
);

create table countrylanguage (
	CountryCode varchar(255),
	Language varchar(255),
	IsOfficial varchar(255),
	Percentage float,
	primary key (CountryCode, Language)
);

-- 3. 
-- source /world-data.sql

-- 4.
create table continent (
	Name varchar(255),
	Area float,
	PercentTotalMass float,
	MostPopulusCity varchar(255),
	primary key (Name)
);

-- 5.
insert into continent values ('Africa', 30370000, 20.4, 'Cairo, Egypt');
insert into continent values ('Antarctica', 14000000.0, 9.2, 'McMurdo Station*');
insert into continent values ('Asia', 445790000.0, 29.5, 'Mumbai, India');
insert into continent values ('Europe', 10180000.0, 6.8, 'Instanbul, Turquia');
insert into continent values ('North America', 24709000.0, 16.5, 'Ciudad de Mexico, Mexico');
insert into continent values ('Oceania', 8600000.0, 5.9, 'Sydney, Australia');
insert into continent values ('South America', 17840000.0, 12, 'Sao Paulo, Brazil');

-- 6.
ALTER table country add foreign key (Continent) references continent(Name);

-- PARTE 2 - QUERIES
-- 1.
select Name, Region from country order by Name, Region;

-- 2.
select Name, Population  from city order by Population desc limit 10;

-- 3.
select Name, Region, SurfaceArea, GovernmentForm from country order by SurfaceArea limit 10;

-- 4.
select * from country where GovernmentForm != 'Republic';

-- 5.
select Language, Percentage from countrylanguage where IsOfficial = 'T';

-- Adicionales
-- 6.
update countrylanguage set Percentage = 100 where CountryCode = 'AIA';
select CountryCode, Language, Percentage from countrylanguage where CountryCode = 'AIA';

-- 7.
select Name from city where District = 'Cordoba';
select * from city WHERE ID > 2275;

-- 8.

-- 9.
select * from country where HeadOfState like "%John%";

-- 10.
select * from country where Population BETWEEN 35000000 and 40000000;

-- 11.


