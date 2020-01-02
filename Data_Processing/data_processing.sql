 	-- CHARGE
CREATE TABLE charge_2 AS
    SELECT id, 
        case_type, 
        case_number, 
        filing_date, 
        offense_date, 
        charge_number, 
        charge_degree, 
        charge, 
        date_charge_filed, 
        filing_type, 
        filing_agency, 
        name, 
        days_since_compas, 
        statute, 
        person_id
    FROM charge;
DROP TABLE charge;
ALTER TABLE charge_2 RENAME TO charge;

CREATE INDEX IF NOT EXISTS index_charge ON charge(charge);
CREATE INDEX IF NOT EXISTS charge_case_numer on charge(case_number);
CREATE INDEX IF NOT EXISTS charge_id ON charge(id);

ALTER TABLE charge ADD COLUMN violent_offense TEXT;

UPDATE charge
SET violent_offense = 
    CASE WHEN (charge.charge LIKE '%robb%' OR
        	   charge.charge LIKE '%agg%ass%' OR
        	   charge.charge LIKE '%murder%' OR
        	   charge.charge LIKE '%rape%')
    		   THEN 1 
    	     ELSE 0 END;

                            -- CASEARREST --
-- recidivism

-- Deleting likely invalid arrests (seem to take place later than the end of the 
-- bulk of the data)
DELETE FROM casearrest where arrest_date > '2016-03-30 00:00:00.000000';

-- Resetting casearrest
CREATE TABLE casearrest_2 AS 
    SELECT id, name, case_number, arrest_id, arrest_date, charge_degree, days_since_compas_arrest, person_id
    FROM casearrest;
DROP TABLE casearrest;
ALTER TABLE casearrest_2 RENAME TO casearrest;

-- Creating indeces
CREATE INDEX IF NOT EXISTS index_arrest_date ON casearrest(arrest_date);
CREATE INDEX IF NOT EXISTS index_person_id ON casearrest(person_id);
CREATE INDEX IF NOT EXISTS casearrest_case_number on casearrest(case_number);
CREATE INDEX IF NOT EXISTS casearrest_id ON casearrest(id);

-- why does adding this index destroy performance on populating recidivated_violent?
-- CREATE INDEX IF NOT EXISTS index_violent_offense ON charge(violent_offense);

-- denormalizing casearrest: adding violent_offense column
ALTER TABLE casearrest ADD COLUMN violent_offense INTEGER;

UPDATE casearrest
SET violent_offense = 
(SELECT charge.violent_offense
FROM charge WHERE casearrest.id = charge.id);

-- Adding recidivism variable
ALTER TABLE casearrest ADD COLUMN recidivated INTEGER;
ALTER TABLE casearrest ADD COLUMN recidivated_violent INTEGER;

-- Calculating 2-year recidivism window
WITH casearrest_temp AS
(SELECT *, date(arrest_date, '-2 year') AS arrest_date_2yr
    FROM casearrest
    JOIN charge USING(case_number))
UPDATE casearrest
-- for each arrest, is there an arrest involving the same person, at a 
-- later date, but no more than 2 years later?
SET recidivated = EXISTS
    (SELECT * from casearrest_temp 
        WHERE casearrest.person_id = casearrest_temp.person_id AND
			  casearrest.arrest_date < casearrest_temp.arrest_date AND
			  casearrest.arrest_date > casearrest_temp.arrest_date_2yr AND
              casearrest_temp.charge_degree != '(0)'
		),
-- for each arrest, is there an arrest involving the same person, charged 
-- as a violent offense, at a  later date, but no more than 2 years later?
	recidivated_violent = EXISTS
	(SELECT * from casearrest_temp 
        WHERE casearrest.person_id = casearrest_temp.person_id AND
			  casearrest_temp.violent_offense = 1 AND
			  casearrest.arrest_date < casearrest_temp.arrest_date AND
			  casearrest.arrest_date > casearrest_temp.arrest_date_2yr AND
              casearrest_temp.charge_degree != '(0)'
		);

-- if an arrest occurred <2 years before the end of our data, we can't say
-- for certain that they didn't recidivate. So if recidivated is 0 and 
-- arrest_date > max(casearrest.arrest_date) - 2 years, then we set
-- recidivated to null.
WITH max_arrest_date AS 
(SELECT date(max(arrest_date), '-2 year') AS max_arrest_date FROM casearrest)
UPDATE casearrest
SET recidivated =
CASE WHEN (recidivated = 0 AND arrest_date > (SELECT * FROM max_arrest_date)) THEN NULL
          ELSE recidivated END,
    recidivated_violent = 
CASE WHEN (recidivated_violent = 0 AND arrest_date > (SELECT * FROM max_arrest_date)) THEN NULL
          ELSE recidivated_violent END;

-- count prior offenses, calculate arrest age


ALTER TABLE casearrest ADD COLUMN prior_arrest_overall INTEGER;
ALTER TABLE casearrest ADD COLUMN prior_arrest_2yr INTEGER;
ALTER TABLE casearrest ADD COLUMN prior_arrest_5yr INTEGER;
ALTER TABLE casearrest ADD COLUMN prior_arrest_overall_violent INTEGER;
ALTER TABLE casearrest ADD COLUMN prior_arrest_2yr_violent INTEGER;
ALTER TABLE casearrest ADD COLUMN prior_arrest_5yr_violent INTEGER;
ALTER TABLE casearrest ADD COLUMN arrest_age INTEGER;
ALTER TABLE casearrest ADD COLUMN arrest_age_category TEXT;


WITH casearrest_temp AS
	(SELECT casearrest.person_id, arrest_date, charge.violent_offense,
            date(arrest_date, '+2 year') AS arrest_date_2yr,
            date(arrest_date, '+5 year') AS arrest_date_5yr
		FROM casearrest JOIN charge USING(id)
		WHERE charge_degree != '(0)')
		
UPDATE casearrest
SET prior_arrest_overall = 
(SELECT count(distinct arrest_date)
		FROM casearrest_temp c2
		WHERE c2.person_id = casearrest.person_id AND
		c2.arrest_date < casearrest.arrest_date),
prior_arrest_2yr = 
    (SELECT count(distinct arrest_date)
		FROM casearrest_temp c2
		WHERE c2.person_id = casearrest.person_id AND
		c2.arrest_date < casearrest.arrest_date AND
		c2.arrest_date_2yr > casearrest.arrest_date),
prior_arrest_5yr = 
    (SELECT count(distinct arrest_date)
		FROM casearrest_temp c2
		WHERE c2.person_id = casearrest.person_id AND
		c2.arrest_date < casearrest.arrest_date AND
		c2.arrest_date_5yr > casearrest.arrest_date),
prior_arrest_overall_violent = 
	(SELECT count(distinct arrest_date)
		FROM casearrest_temp c2
		WHERE c2.person_id = casearrest.person_id AND
		c2.arrest_date < casearrest.arrest_date AND
		violent_offense = 1),
prior_arrest_2yr_violent = 
    (SELECT count(distinct arrest_date)
		FROM casearrest_temp c2
		WHERE c2.person_id = casearrest.person_id AND
		c2.arrest_date < casearrest.arrest_date AND
		c2.arrest_date_2yr > casearrest.arrest_date AND
		violent_offense = 1),
prior_arrest_5yr_violent = 
    (SELECT count(distinct arrest_date)
		FROM casearrest_temp c2
		WHERE c2.person_id = casearrest.person_id AND
		c2.arrest_date < casearrest.arrest_date AND
		c2.arrest_date_5yr > casearrest.arrest_date AND
		violent_offense = 1),
arrest_age = 
	(select cast((julianday(arrest_date) - julianday(dob)) / 365.5 as int)
	FROM people
	WHERE casearrest.person_id = people.id),
arrest_age_category = 
	(select CASE WHEN casearrest.arrest_age < 25 THEN 'Less than 25'
				 WHEN casearrest.arrest_age BETWEEN 25 AND 45 THEN '25 - 45'
				 WHEN casearrest.arrest_age > 45 THEN 'Greater than 45' END);


                            -- JAILHISTORY --

CREATE TABLE jailhistory_2 AS
	SELECT id,
		first,
		last,
		dob,
		in_custody,
		out_custody,
		person_id
	FROM jailhistory;
DROP TABLE jailhistory;
ALTER TABLE jailhistory_2 RENAME TO jailhistory;

CREATE INDEX IF NOT EXISTS jailhistory_index on jailhistory(person_id, in_custody, out_custody);

ALTER TABLE jailhistory ADD COLUMN staylength_days REAL;

UPDATE jailhistory
SET staylength_days = JULIANDAY(out_custody) - JULIANDAY(in_custody);

                            -- PRISONHISTORY --

CREATE TABLE prisonhistory_2 AS
	SELECT id,
		name,
		first,
		middle,
		last,
		dob,
		in_custody,
		out_custody,
		person_id
	FROM prisonhistory;
DROP TABLE prisonhistory;
ALTER TABLE prisonhistory_2 RENAME TO prisonhistory;

CREATE INDEX IF NOT EXISTS prisonhistory_index on prisonhistory(person_id, in_custody, out_custody);

ALTER TABLE prisonhistory ADD COLUMN staylength_days REAL;

UPDATE prisonhistory
SET staylength_days = JULIANDAY(out_custody) - JULIANDAY(in_custody);

                            -- CASEARREST --

ALTER TABLE casearrest ADD COLUMN prior_prisontime_days REAL;
ALTER TABLE casearrest ADD COLUMN prior_jailtime_days REAL;

UPDATE casearrest
SET prior_prisontime_days = (SELECT SUM(staylength_days)
	FROM prisonhistory
	WHERE casearrest.person_id = prisonhistory.person_id AND
		casearrest.arrest_date > prisonhistory.out_custody),
prior_jailtime_days = (SELECT SUM(staylength_days)
	FROM jailhistory
	WHERE casearrest.person_id = jailhistory.person_id AND
		casearrest.arrest_date > jailhistory.out_custody);

UPDATE casearrest
SET prior_prisontime_days = IFNULL(prior_prisontime_days, 0),
prior_jailtime_days = IFNULL(prior_jailtime_days, 0);