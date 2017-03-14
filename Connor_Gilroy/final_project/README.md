---
title: Visualization of Lee-Carter model estimates
author: Connor Gilroy
date: 3/7/17
---

## Synopsis

This project refines the Lee-Carter model example presented in class, using packages from the [Tidyverse](http://tidyverse.org/) to transform data from the [Human Mortality Database](http://www.mortality.org/). It then visualizes the resulting estimates in an interactive Shiny app. The example data are mortality rates from Sweden (not included in the GitHub repository).

## Helper functions

The bulk of the project is a series of helper functions in `lc_functions.R`, which are designed to be used in a pipe chain.

The main chain of functions, which is wrapped in `wrap_lc_total()`, is as follows:

```{r}
mort_mut <-
  mort %>%
  age_to_factor() %>%
  filter_age(n = 18)

d <-
  make_nmx_total(mort_mut) %>%
  estimate_lee_carter() %>%
  lc_model_to_matrix() %>%
  lc_matrix_to_df() %>%
  combine_total(mort_mut) %>%
  combined_df_to_long()
```

The first code block takes a nmx data frame and processes and filters the Age variable. The second code block turns the data frame into an nmx matrix, does a singular value decomposition to calculate ax, bx, and kt, turns the model values into a new matrix of estimates, and then converts that *back* into a data frame, so the estimates can be compared with the original values through visualization.

## Shiny app

The plotting functions use the transformed data above to visualize nMx by age and by year. They are embedded in an interactive Shiny app, so that you can change the year (for the age plot) and the age group (for the year plot) shown.

To run the app, download the server, UI, and function files into a directory. You will need to create a `data/` subdirectory and add a `Mx_5x1.txt` file to it. Then open the server or UI file in RStudio and click Run App.

To visualize a different data set, upload it using the file button. Currently only optimized for Mx 5x1 files from different countries.
