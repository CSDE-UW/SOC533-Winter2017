SUBJECT: Description of Steps Taken to Complete Homework 5, SOC 533: Demographic Methods
AUTHORS: Christine Leibbrand, Erin Carll
DATE: 02/12/2017
DATA FILES USED: 
1. Human Mortality Database Swedish 5x5 life table for females, 1751-1923, http://www.mortality.org/hmd/SWE/STATS/fltcoh_5x5.txt
2. Human Fertility Database Swedish Age-specific fertility rates, 1891-2014, all birth orders combined: year, age, cohort
http://www.humanfertility.org/cgi-bin/getfile.plx?f=SWE\20160321\SWEasfrTR.txt
**Note: user accounts required to download data**
STATISTICAL PACKAGE USED: R-Studio
-----------------------------------------------------------------------------
1. Download and read in data files: http://www.mortality.org/hmd/SWE/STATS/fltcoh_5x5.txt, http://www.humanfertility.org/cgi-bin/getfile.plx?f=SWE\20160321\SWEasfrVH.txt

2. Subset data files to keep those born 1900-1904 in the life-table data file (http://www.mortality.org/hmd/SWE/STATS/fltcoh_5x5.txt) and the 1900 cohort from the fertility data file (http://www.humanfertility.org/cgi-bin/getfile.plx?f=SWE\20160321\SWEasfrRR.txt)

3. Generate a Leslie Matrix so that the last three columns of the first row read as NAs, the first two columns of the first row read as zeroes (since no babies are born to females in these age groups, the subdiagonal consists of values calculated according to the equation nLx+n/nLx, and all other cells consist of structural zeroes.

4. Calculate 5-year ASFRs by averaging the 1-year ASFRs over the corresponding 5-year periods

5. Calculate cells a13, a14, a15 of Leslie Matrix using the following equation: A1,j(x)=(nL0/(2*l0)(nFx+nF(x+n)(nL(x+n)/nLx)ffab

6. Add values for cells a13, a14, and 15 to Leslie Matrix.

7. Print out Leslie Matrix to .csv file
