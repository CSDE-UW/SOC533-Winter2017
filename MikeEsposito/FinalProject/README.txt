### General Information

The files included in this folder were used to create a measure of healthy
life expectancy for period data for 2011.

### File Guide 

lifetable.txt contains life table data for women in the US in 2011;
nhis11.rds contains health survey data for 2011 from the National Health Interview Survey;
code.r contains the R script that will compute healthy life expectancy;
SOC533_FinalProject.pptx is a presentation on what healthy life expectancy is and how to calculate it;

### Creating a measure of Healthy Life Expectancy

One can compute a measure of health life expectancy by following the subsequent 
steps:

Part I: Read in Data

1: read/open NHIS person sample file of interest (e.g. 2011) 
2: read/open life table data of interest (e.g. 5x1 female period life table for 2011) 

Part II: Create a function to calculate healthy life expectancy

1: Function Components:
nhis.dat     == NHIS data file 
lt.dat       == HMD period lifetable
year	     == year of interest
sex	     == sex category 
health.var   == variable used for determining p(health)
healthy.status == values of the health variable that indicate health
new_col_name == background; for standard eval
x.start	     == background; starting age (fixed for this)
x.end 	     == background; ending age (fixed)
n	     == background; interval length
labs         == optional; custom labels
2: Create labels for function

Part III: Run Function

