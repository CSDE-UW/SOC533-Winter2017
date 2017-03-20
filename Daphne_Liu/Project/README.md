# README File for Daphne Liu's Project

We have created functions that compute various period fertility measures, namely period NRR, period TFR, period GRR, age-standardized birth rates, Princeton indices, and Coale and Trussell's model. For the purposes of this demonstration/assignment, we will be using example data provided in our textbook (Essential Demographic Methods) so that we have a means of checking if our functions calculate these measures correctly. 


### Period Net Reproduction Rate:
Ratio of total number of daughters borne by synthetic cohort members to the initial number of women in the synthetic cohort calculated using period data.

**Usage:** periodNRR(nFx, nLx, l0, ffab)

**Arguments:**

* nFx: Vector of period age-specific fertility rates (ASFR), of same length as nLx
* nLx: Vector of period person-years lived, of same length as nFx
* l0: Scalar of the radix of the period life table
* ffab: Scalar of fraction female at birth, default is 0.4886


### Period Total Fertility Rate:
Total fertility rate calculated using period data.

**Usage:** periodTFR(nFx, n)

**Arguments:**

* nFx: Vector of period age-specific fertility rates (ASFR), of same length as n
* n: Vector of age-group widths, of same length as nFx


### Period Gross Reproductive Ratio:
Ratio of total number of daughters borne by synthetic cohort members to the initial number of women in the synthetic cohort calculated using period data, excluding losses due to mortality. 

**Usage:** periodGRR(nFx, n, ffab)

**Arguments:**

* nFx: Vector of period age-specific fertility rates (ASFR), of same length as n
* n: Vector of age-group widths, of same length as nFx
ffab: Scalar of fraction female at birth, default is 0.4886


### Age-Standardized Birth Rates:
A measure that allows us to retain the simplicity of the crude birth rate while removing the effects of the observed age distribution by applying age-specific rates to standard population counts.

**Usage:** ASR.birth(nFx, x, nKx)

**Arguments:**

* nFx: Vector of period age-specific fertility rates (ASFR)
* x: Vector of ages corresponding to age-specific fertility rates in nFx, default is from Table 6.3 in Essential Demographic Methods, if specified user must also supply nKx vector
* nKx: Vector of standard population counts, default is from Table 6.3 in Essential Demographic Methods (must be provided by user if x is specified)


### Princeton Indices
Ansley Coale's four fertility indices that can be computed from administrative data. If is an index of overall fertility, Ig is an index of marital fertility, Ih is an index of non-marital fertility, and Im is the Princeton Index of Marriage (measures how conducive the marriage pattern is to high fertility). These indices allow for comparison across fine-grained levels of geography. Indices are calculated based on Hutterite age-specific fertility rates, as provided in Table 6.4 in Essential Demographic Methods. 

**Usage:** princeton(fiveKx, fiveKx.married, B.overall, B.marital)

**Arguments:**

* fiveKx: Vector of counts of women where age-group width is 5 years, must correspond to age vector x=(15, 20, 25, 30, 35, 40, 45)
* fiveKx.married: Vector of counts of married women where age-group width is 5 years, must correspond to age vector x=(15, 20, 25, 30, 35, 40, 45)
* B.overall: Scalar of total births to all women observed in the actual population
* B.marital: Scalar of total births to married women in the actual population


### Coale and Trussell's Model
A set of two functions that allow computation of estimates of Coale and Trussell's M and m from data and computation of predicted values of age-specific marital fertility rates from a model based on given indice values m and M. The Coale and Trussell model is based on the spine constants provided in Table 6.5 in Essential Demographic Methods. The "little m" index sets the strength of parity-specific fertility limitation while the "big M" index sets a background level of natural fertility. 

**Usage:** 

* ct.estimates(ASMFR)
* ct.predictions(m, M)

**Arguments:**

* ASMFR: Vector of age-specific marital fertility rates
* m: Scalar of "little m" index for Coale and Trussell's model 
* M: Scalar of "big M" index for Coale and Trussell's model