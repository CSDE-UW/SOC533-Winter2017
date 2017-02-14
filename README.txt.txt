SOC 533
Assignment 5
Xiaxia Yang 

Step 1-Preparing the Data in Excel
(1)	Life table
Go to The Human Mortality Database, click ¡°Sweden¡± to navigate to the Complete Data Series. Under ¡°Cohort data¡±, choose ¡°Life tables-Females-5¡Á5¡±. Download the data as .txt format. While opening the data in Excel, choose ¡°Delimited¡± in the Text Import Wizard, and choose ¡°Space¡± as the delimiter. The result is an Excel table with all the data. Clear the title of the table. Assign each column with the correct data format - year and age as ¡°Text¡± and the others as ¡°Number¡± with the appropriate decimal places. Save the file as SWEtable.xlsx. 
(2)	Period Age-specific fertility
Go to The Human Fertility Database, click ¡°Sweden¡± to navigate to the Complete Data Series. Under the ¡°Age-Specific Data¡± tab, choose ¡°Birth Counts-year,age¡±. Download the data as .txt format. While opening the data in Excel, choose ¡°Delimited¡± in the Text Import Wizard, and choose ¡°Space¡± as the delimiter. Since the data is in a year-by-year rather than 5-year form, I add up the number of births from 1900-1904 with respect to age. Then I add up the age-specific birth count according to the 5-year age interval in SWEtable.xlsx, and put the final result as a new column ¡°Bx¡± in SWEtable.xlsx. Finally, I divide Bx by Lx to get the new column Fx.

Step 2-Create Leslie Matrix in Stata
(1)	Import SWEtable.xlsx into Stata.
(2)	Change the column names of the data.
(3)	Loop over to calculate Leslie matrix.
(4)	Output the Leslie matrix to Excel as SWEles.xls. 
(5)	Save SWEles.xls as SWEles.cvs and change the content of the first column and first row as ages.
