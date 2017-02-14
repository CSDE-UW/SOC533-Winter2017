Lee Fiorio
SOC533
Homework 5

Goal: create a Leslie Matrix using data from the Human Mortality and Human Fertility databases from the cohort of women born in 1900 in Sweden.

Input data:
/data/fltcoh_5x5.txt
Cohort life table for women

/data/SWEbirthsRR.txt
Count of births by year and by age

/data/SWEexposRR.txt
Count of women "exposed to risk of pregnancy" by year and by age

Output data:
/final_leslie_matrix_Lee.csv

Code outline:
The first step is to read in the cohort life table, clean it and then extract data pertinent to the 1900 Swedish cohort (vars Year, Age and Lx)

The second step is to read in the count of births and exposure tables, clean them, and merge them. The life table data is in a 5 year format, so we need to aggregate the 1 year format data from these tables to approximate 5 year data. To do this we bin our data by five year ages {(12-,13,14),(15,16,17,18,19),(20,21,22,23,24),...,(50,51,52,53,54),(55+)} and sum the total births and total women exposed and then divide births by exposure to get age bin specific fertility rates. We also need to only grab data for our cohort. In hindsight, it would have been better to use the age x year x cohort file instead of the age x year file. The code extracts data that approximate the birth counts and exposure counts of the 1900 cohort.

The last step is to create and populate the Leslie matrix. It is a 23x23 matrix, one row/column for each of the 5-year age bins from 0-4 to 110+.

The first row is populated using the following equation where Lx are the life years from the life table for the 1900 cohort, Fx is the age bin specific mortality for the 1900 cohort, and l0 is 100,000.

A[1,j] = Lx[1]/(2*l0)) * (Fx[i]+(Fx[j+1] * (Lx[j+1]/Lx[j]))) * 0.4886

The sub diagonal is populated using the following equation where Lx are the life years from the 1900 life table for the 1900 cohort.

A[i+1,i] = Lx[i+1]/Lx[i]
