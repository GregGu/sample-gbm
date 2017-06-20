PROC SQL;
CREATE TABLE DEMO.AGG AS
	SELECT
		  ACCMON AS MONTH
		, CASEYEAR AS YEAR
/* 		, WEEEEEEEEEKDATE ALSO COUNT NUMBER OF PEOPLE/VEH */
		, HEAVYTRUCK
		, CASE WHEN MISSING(RTESIGN) THEN .
			WHEN RTESIGN IN ('INTERSTATE') THEN 1
			ELSE 0
			END AS INTERSTATE
		, CASE WHEN MISSING(ALCRES) THEN .
			WHEN ALCRES > 80 THEN 1
			ELSE 0
			END AS DRUNK
		, CASE WHEN MISSING(RESTRAINT) .
			WHEN RESTRAINT IN (3,4,8) THEN 1
			ELSE 0
			END AS SEATBELT
		, CASE WHEN MISSING(SEX) .
			WHEN SEX = 1 THEN 1
			ELSE 0
			END AS MALE
		, CASE WHEN MISSING(SEX) .
			WHEN SEX = 0 THEN 1
			ELSE 0
			END AS FEMALE
		, CASE WHEN MISSING(BODY) .
			WHEN BODY IN (2,5) THEN 1
			ELSE 0
			END CAR
		, CASE WHEN MISSING(BODY) .
			WHEN BODY IN (2,5) THEN 1
			ELSE 0
			END SUV		
		, CASE WHEN MISSING(BODY) .
			WHEN BODY IN (2,5) THEN 1
			ELSE 0
			END TRUCK
		, CASE WHEN MISSING(DRIDISTRACT) .
			WHEN DRIDISTRACT (5,6,15) THEN 1
			ELSE 0
			END AS CELLPHONE
		, CASE WHEN MISSING(SPDLIM) .
			WHEN SPDLIM IN (80,85) THEN 1
			ELSE 0
			END AS 80+
		, CASE WHEN MISSING(SPDLIM) .
			WHEN SPDLIM = 75 THEN 1
			ELSE 0
			END AS 75
		, CASE WHEN MISSING(SPDLIM) .
			WHEN SPDLIM = 70 THEN 1
			ELSE 0
			END AS 70
