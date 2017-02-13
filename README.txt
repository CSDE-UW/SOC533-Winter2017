### General Information

The files included in this folder were used to create 'lesliematrix.csv', which is a Leslie Matrix for the cohort of Swedish women born in 1900, 
with a 5-year age interval and 5-year projection step. 

### File Guide 

SWEbirthsVH.txt contains data on the number of births to Swedish women, by cohort;
SWEexposVH.txt contains data on the female population exposure for Swedish women, by cohort; 
swedenlifetable.txt contains cohort life tables for different cohorts of Swedish women
lesliematrix.csv is a 5-year age interaval Leslie Matrix for the 1900 Swedish female cohort
HW5_SL02132017.r contains R script that will produce lesliematrix.csv 

### Creating lesliematrix.csv 

One can produce the Leslie Matrix of interest by following the subsequent steps:

Part I: The Leslie Matrix Sub-Diagonal 

1: read/open/attach the 'swedenlifetable.txt' data file 
2: filter the data file to only include the life table for the 1900 cohort
3: select the column titled "Lx" 
4: sum Lx values for the first two rows
	- Note: Age groups for most all rows of the life-table are in 5 year intervals. This is necessary and consistent w/ our goal of	creating a matrix for 5-year projections. The first two age-groups in this file (i.e., [0], and [1-4]) are not, and need to be combined into a single 5-year interval (i.e., [0-4]). Since Lx indicates the life years lived in the age interval, summing the Lx values for the first two rows works for this. 	
5: calculate nLx+n/nLx for each entry on the (from, to) element on the sub-diagonal

Part II: The Leslie Matrix First Row

1: read the files 'SWEbirthsVH.txt' and 'SWEexposVH.txt'
2: filter the data to only include the 1900 cohort
3: sum births across 5 year intervals 
	- Note: the births file is given in 1 year intervals. To get the data to where it needs to be for 5-year projections, sum births for each age within an interval (e.g., for the births from [20-24], sum (births at 20, births at 21, ... births at 24)). 
4: sum exposure across 5-year intervals (see above)  
5: divide Births/Exposure for each age group for ASFRs (fx here)
6: calculate A(1,j) = [nL0/(2*lo)]*(fx[j] + fx[j + 1]*sub-diagonal element[j])*0.4886 for each j element on the first row. 

Part III: Getting it All Together

1: fill in first row, and off-diag. w/ relevant information.
2: put 0s everywhere else
3: extract dataframe to a .csv file (lesliematrix.csv)