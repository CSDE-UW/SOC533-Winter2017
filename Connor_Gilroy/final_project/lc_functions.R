library(dplyr)
library(tidyr) 
library(forcats)
library(magrittr)
library(stringr)
library(ggplot2)

age_to_factor <- function(mort) { 
  ## first step of cleaning the original nmx data frame
  ## turns Age column into a factor with levels in the right order
  mutate(mort, 
         Age = as.factor(Age), 
         Age = fct_inorder(Age))  
}

filter_age <- function(mort_mut, n = 18) {
  ## second step of cleaning the original nmx data frame
  ## filters out age groups above a given threshold
  ## and drops filtered levels of factor
  ## assumes that Age is already a factor
  mort_mut %>%
    filter(Age %in% levels(Age)[1:n]) %>%
    mutate(Age = fct_drop(Age))
}

nmx_df_to_matrix <- function(nmx_df) {
  ## helper function for make_nmx_<var> below
  ## last step of pipeline
  Years <- nmx_df$Year   
  
  nmx_df %>%
    select(-Year) %>%
    as.matrix() %>%
    set_rownames(Years)
}

make_nmx_total <- function(mort_mut) {
  ## produces an nmx matrix using the nmx value from the Total column
  ## retains Age as column names and Year as rownames
  mort_mut %>%
    select(-c(Male, Female)) %>%
    spread(key = Age, value = Total) %>%
    nmx_df_to_matrix()
}

make_nmx_male <- function(mort_mut) {
  ## produces an nmx matrix using the nmx value from the Male column
  ## retains Age as column names and Year as rownames
  mort_mut %>%
    select(-c(Total, Female)) %>%
    spread(key = Age, value = Male) %>%
    nmx_df_to_matrix()
}

make_nmx_female <- function(mort_mut) {
  ## produces an nmx matrix using the nmx value from the Female column
  ## retains Age as column names and Year as rownames
  mort_mut %>%
    select(-c(Male, Total)) %>%
    spread(key = Age, value = Female) %>%
    nmx_df_to_matrix()
}

estimate_lee_carter <- function(nmx) {
  ## the core of this code is taken from Emilio Zagheni's
  ## estimate.leecarter() function
  ## the difference is that this model retains 
  ## the column names (age groups) and row names (years)
  ## input nmx should be a matrix with dimnames
  ## note: ax automatically retains the right names
  log.nmx <- log(nmx)
  ax <- apply(log.nmx, 2, mean)
  swept.mx <- sweep(log.nmx, 2, ax)
  svd.mx <- svd(swept.mx)
  bx <- svd.mx$v[, 1]/sum(svd.mx$v[, 1])
  kt <- svd.mx$d[1] * svd.mx$u[, 1] * sum(svd.mx$v[, 1])
  
  names(bx) <- colnames(nmx)
  names(kt) <- rownames(nmx)
  
  list(ax = ax, bx = bx, kt = kt)
}

lc_model_to_matrix <- function(model){
  ## converts model values back into a matrix 
  ## of estimates of log(nmx)
  ## note: **log** values. do not compare to original nmx values
  with(model, { 
    lc_log_nmx <- matrix(NA, nrow = length(kt), ncol = length(bx))
    rownames(lc_log_nmx) <- names(kt)
    colnames(lc_log_nmx) <- names(bx)
    for (i in seq_along(kt)) {
      lc_log_nmx[i, ] <- ax + bx*kt[i]
    }
    lc_log_nmx
  })
}

lc_matrix_to_df <- function(lc_log_nmx) {
  ## converts matrix of log(nmx) estimates to a data frame 
  ## of Year, Age, and estimated log(nmx)
  ## note: **log** values. do not compare to original nmx values
  Year <- as.numeric(rownames(lc_log_nmx))
  bind_cols(data_frame(Year), as_data_frame(lc_log_nmx)) %>%
    gather(key = Age, value = lc_log_nmx, -Year) %>%
    age_to_factor() %>%
    arrange(Year)
}

lc_model_to_df <- function(model, mort_mut) {
  ## TODO: write function that skips creating a matrix of
  ## Lee-Carter estimates, and instead left-joins the 
  ## model ax, bx, and kt values to the original data frame
  ## then calculates the estimates using mutate()
  ## Returns: same result as combine_total, 
  ## but with columns for ax, bx, and kt as well
}

combine_total <- function(lc_log_nmx_df, mort_mut) {
  ## combines estimated Lee-Carter values of log(nmx)
  ## with original (but modified!) mortality data frame
  ## calculates actual log(nmx) in the process
  ## the arguments are ordered the way they are to both
  ## facilitate usage in the expected pipeline and
  ## produce a data frame with columns in a logical order,
  ## with estimated log(nmx) last, next to true log(nmx)
  mort_mut %>%
    mutate(log_nmx = log(Total)) %>%
    full_join(lc_log_nmx_df, by = c("Year", "Age"))
}

combined_df_to_long <- function(combined_df) {
  ## takes the combined data frame in wide form 
  ## (i.e., with actual and estimated log(nmx) values in separate columns)
  ## and puts it in long form, with a separate column that labels
  ## whether a log(nmx) value is a Lee-Carter estimate or a true value
  ## this is the "tidiest" form of the data, that is most amenable to
  ## plotting using ggplot2
  combined_df %>%
    gather(key = source, value = log_nmx, log_nmx, lc_log_nmx) %>%
    mutate(source = ifelse(source == "lc_log_nmx", 
                           "Lee-Carter estimate", 
                           "true value"), 
           source = factor(source, 
                           levels = c("true value", "Lee-Carter estimate")))
}

wrap_lc_total <- function(mort, n = 18) {
  ## wraps the entire pipeline for producing Lee-Carter estimates
  ## for the Total column of the mortality data frame
  mort_mut <- 
    mort %>%
    age_to_factor() %>%
    filter_age(n = 18)
  
  make_nmx_total(mort_mut) %>%
    estimate_lee_carter() %>%
    lc_model_to_matrix() %>%
    lc_matrix_to_df() %>%
    combine_total(mort_mut) %>%
    combined_df_to_long()
}

## plotting functions

plot_nmx_by_age_for_year <- function(df, filter_year) {
  ## replicates Emilio's plot of log(nmx) for each age group
  ## for the year given in filter_year
  ageplot_title <- str_c("Year: ", filter_year)
  df %>%
    filter(Year == filter_year) %>%
    ggplot(aes(x = Age, y = log_nmx, group = source, color = source)) + 
    geom_line() + 
    theme_minimal() +
    ylim(floor(min(df$log_nmx)), ceiling(max(df$log_nmx))) +
    labs(y = "log(nmx)", 
         title = ageplot_title) +
    theme(legend.title = element_blank(), 
          legend.position = "bottom", 
          legend.text = element_text(size = rel(1.2)))
}

plot_nmx_by_year_for_age <- function(df, filter_age) {
  ## plots log(nmx) for each year 
  ## for the age group given in filter_age
  yearplot_title <- str_c("Age group: ", filter_age)
  df %>%
    filter(Age == filter_age) %>%
    ggplot(aes(x = Year, y = log_nmx, group = source, color = source)) + 
    geom_line() + 
    theme_minimal() +
    ylim(floor(min(df$log_nmx)), ceiling(max(df$log_nmx))) +
    labs(y = "log(nmx)", 
         title = yearplot_title) +
    theme(legend.title = element_blank(), 
          legend.position = "bottom", 
          legend.text = element_text(size = rel(1.2)))
}
