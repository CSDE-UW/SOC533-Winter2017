First, we downloaded Human Mortality Database (HMD) data and Human Fertility Database (HFD) data for Sweden, specifically utilizing the Swedish datasets that employ 5 year intervals for their data.
Then we uploaded the data to R and subsetted the data so that it only included the cohort born in 1900.
We then created a Leslie Matrix with an empty first row, calculations for the transition probabilities on the subdiagonal, and 0s everywhere else (specifically, everywhere where it was impossible to have a transition probability or fertility rates)
We then added 0s to the first row where fertility probabilities were 0 (for children under 10)
Finally, we calculated projected females for the rest of the cells in the first row by using the calculations provided on page 107 of the textbook A1j = (nL0/2l0)(nFx + nFx+n(nLx+n/nLx))ffab
