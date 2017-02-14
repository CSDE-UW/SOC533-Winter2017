SOC 533
Homework 5 README File: Description of Steps Taken
Authors: Christine Leibbrand and Erin Carll


1. First, we downloaded Human Mortality Database (HMD) data (http://www.mortality.org/hmd/SWE/STATS/fltcoh_5x5.txt) and Human Fertility Database (HFD) data (http://www.humanfertility.org/cgi-bin/getfile.plx?f=SWE\20160321\SWEasfrRR.txt) for Sweden, specifically utilizing the Swedish datasets that employ 5 year intervals for their data.

2. Then we uploaded the data to R and subsetted the data so that it only included the cohort born in 1900 for the HFD data and the cohort born from 1900-1904 in the HMD data.

3. We then created a Leslie Matrix with an empty first row, calculations for the transition probabilities on the subdiagonal, and 0s everywhere else (specifically, everywhere where it was impossible to have either a transition probability or fertility rate)

4. We then added 0s to the first row where fertility probabilities were 0 (for children under 10)

5. Finally, we calculated projected females for the rest of the cells in the first row by using the calculations provided on page 107 of the textbook A1j = (nL0/2l0)(nFx + nFx+n(nLx+n/nLx))ffab and printed the Leslie Matrix to a .csv file.
