---
author: Connor Gilroy
date: 2/14/17
---

The code in `sweden_leslie_matrix_5x5.R` calculates a Leslie matrix for the
cohort of Swedish women born in 1900, with the output in `sweden_leslie_matrix_5x5.csv`.

The inputs are data from the Human Mortality Database and the Human Fertility Database.
Specifically, the person-years lived by the cohort come from `fltcoh_5x5.txt` on the HMD.
I believe this contains the person-years lived for the 5-year cohort born from 1900-1904,
not only the 1-year cohort from 1900, which makes the estimates an approximation. The
age-specific fertility rates for the 1900 cohort come from `SWEasfrVH.txt` on the HFD.
These are cohort fertility rates, even though the approximation of the Leslie matrix
conventionally uses period fertility rates.

The data are downloaded and stored in a `data/` directory, which is ignored and not
committed (It is possible to have additional .gitignore files in subdirectories of a repo:
http://stackoverflow.com/questions/9730486/can-you-have-multiple-gitignore-files-within-a-single-repo)

For both data sets, I first filter the data down to the relevant cohort. In order
to calculate the matrix, I need to extract Lx (cohort person-years lived)
and fx (cohort age-specific fertility rate) and process these into five-year intervals.
For Lx, this just means combining the 0-year CPYL with the 1-4-year CPYL. For fx,
this means taking an average of rates for the 5-year interval.

The size of the matrix is the number of age bins available, from 0-4 up to 110+.
Filling in all empty values as 0s initially simplifies calculations. The off-diagonal
is simply the ratio Lx+n/Lx. The values of the first row are
L0/(2l0) * (Lx*Fx + Lx+n*Fx+n) / Lx * f_fab. (For fraction female at birth, I use
the default 0.4886.)

Note that the decision to take a five-year average for fertility rates produces a
small, nonzero value for the 5-9 bin of the first row of the Leslie matrix (because
the fertility rate for 14-year-olds is nonzero.)

I write the constructed matrix to a csv file, `sweden_leslie_matrix_5x5.csv`. I
use the base `write.csv()` function rather than `readr::write_csv()` because
the latter is optimized for data frames and does not handle row names for
matrices well. 

Finally, I note that if you open the csv file contain the matrix in Excel, two of
the age bins will display as dates. This is because Excel is antithetical to science
(https://www.sciencemag.org/news/sifter/one-five-genetics-papers-contains-errors-thanks-microsoft-excel)
