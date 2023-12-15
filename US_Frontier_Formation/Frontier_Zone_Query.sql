# -------Gaining access for file privildges in the host to export results into a .csv format-------
SHOW VARIABLES LIKE 'secure_file_priv';
GRANT FILE ON *.* TO root@localhost;
# -------------------------------------------------------------------------------------------------
USE shaly_sands;
CREATE TABLE Frontier_Formation
AS
SELECT * 
FROM shaly_sands.mojavefederal_44f AS V
WHERE V.Depth >= 12480 AND V.Depth <= 12580;

SELECT *
INTO OUTFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/UpperFrontierForm.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
FROM Frontier_Formation;