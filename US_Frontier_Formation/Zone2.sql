# -------Gaining access for file privildges in the host to export results into a .csv format-------
SHOW VARIABLES LIKE 'secure_file_priv';
GRANT FILE ON *.* TO root@localhost;
# -------------------------------------------------------------------------------------------------
USE shaly_sands;
CREATE TABLE Zone2
AS
SELECT * 
FROM shaly_sands.mojavefederal_44f AS V
WHERE V.Depth >= 9110 AND V.Depth <= 9232;

SELECT *
INTO OUTFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Zone2.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
FROM Zone2;