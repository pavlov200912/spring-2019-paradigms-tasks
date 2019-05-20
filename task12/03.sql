-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT City.Name FROM Country JOIN Capital, City ON Capital.CountryCode = Country.Code AND Country.Code = City.CountryCode WHERE City.Id = Capital.CityId AND Country.Name = 'Malaysia';
