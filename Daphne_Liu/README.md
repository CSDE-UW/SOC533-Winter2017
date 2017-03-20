Daphne Liu

To create our Leslie matrix, we first went to the Human Mortality Database and downloaded 5x5 life tables for Swedish women. We then went to the Human Fertility Database and downloaded data on  cohort birth counts and female population exposure exposure (all birth orders combined). We imported these data files into R and extracted the rows corresponding to the cohort of women born in 1900.

Next, we set up our Leslie matrix. As there are 24 different 5-year age intervals for our cohort of women born in 1900, we created a 24 x 24 matrix of zeroes to begin with. The majority of these zeroes represent structural zeroes in our matrix, while others will be replaced in our following steps.

We first considered the fertility elements of our Leslie matrix (i.e. the first row). In order to calculate the values of these elements, we need to first calculate the age-specific fertility rates. We do this using the birth count and exposure data by taking the sum of birth counts over the 5 year period of interest and then dividing by the sum of exposure over the same 5 year period. The first three elements correspond to the age groups 0, 1-4, and 5-9, so we see the age-specific fertility is 0. For the age 10-14 group, we were only able to use information for ages 12, 13, and 14 to calculate our age-specific fertility. For age groups 15-29 to 50-54, we were able to proceed using the formula above to find age-specific fertility. To calculate age-specific fertility for age groups 55-59 to 110+, we used the birth and exposure data for the 55+ age. Once we found our age-specific fertility, we were able to compute the Leslie matrix entries using the age-specific fertility and elements from our life table.

Next, we computed the survival elements of the Leslie matrix (i.e. the off-diagonals). These were computing using information purely from the life table. 

Finally, we extracted our Leslie matrix as a .csv file ("dhliu_leslie.csv"). 
