# -------Gaining access for file privildges in the host to export results into a .csv format-------
SHOW VARIABLES LIKE 'secure_file_priv';
GRANT FILE ON *.* TO root@localhost;
# -------------------------------------------------------------------------------------------------
USE shaly_sands;
CREATE TABLE CondensateZone
AS
SELECT * 
FROM shaly_sands.nswell15_9_12 AS NS
WHERE NS.Depth >= 3400 AND NS.Depth <= 3800;

SELECT *
INTO OUTFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/CondensateZone.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
FROM CondensateZone;