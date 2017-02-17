---
author: Zhihang Dong
date: 2/11/17
class: Csde 533
---
This is a way to produce the Leslie matrix for Homework 5 using the data from the Human Mortality Database (HMD)
and Human Fertility Database (HFD).

# Prepare the Data

You first download the data from HFD the cohort-level fertility data for Sweden from the website below:
http://www.humanfertility.org/cgi-bin/country.php?country=SWE&tab=asfr

Then, you download a 5x5 lifetable from the HMD for the leslie matrix
http://www.mortality.org/cgi-bin/hmd/country.php?cntr=SWE&level=1

Remember downloading data for 5x5 life table for FEMALE only.

# Step 1
Just follow the annotations on my R code, or read my html file, it is simple and easy!
After you have the data from the two databases, simply create a lifetable in the tabular format. In my code, the lifetable is labeled 'lt'.

Then, get the data for the cohort birth from the cohort year 1900, this variable is anmed as 'bc'.

# Step 2

Now that we have the exposure data for cohort year 1900's fertility...
We then have a variable called 'expo'

# Step 3

Examine if the variables are in the numeric form, since the string form will not work for the 
next following steps.

# Step 4

Then, we are able to create a leslie matrix. We call them 'les'. Then, produce the first 
row of the leslie matrix. It is easily done for the age between 15 and 55.

## Special Treatment for other age groups:
Because the births given for age 55+ of cohort 1900 is only 2. This is too trivial.
We will then assume the age-specific fertility rate for age 55-110 is 0.

Because we cannot imagine any births given prior to the age 10, we will assume all births
given for age 12- is given within the age group 10-14.

# Step 5
Now that we are ready to calculate the diagnoal of the leslie matrix.
Simply following the equation: _5L_x/ _5L_(x+5)...

# Step 6

Importing all data to their locations. We finish producing our Leslie matrix.

