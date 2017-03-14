## Leslie Matrix
## Sweden
## Cohort of 1900 females
## 5-year period, 5-year age interval

# setwd("~/Documents/Coursework/winter_2017/demo_methods/SOC533-Winter2017/Connor_Gilroy")
# if (!dir.exists("data")) dir.create("data")

library(readr)
library(dplyr)
sweden_hmd <- read_table("data/fltcoh_5x5.txt", skip = 2, na = ".")

sweden_hmd_1900 <- 
  sweden_hmd %>%
  filter(Year == "1900-1904") %>%
  select(Age, Lx)

## they separate out year 0 in their "five"-year projections
## so, I need to fix that
sweden_hmd_1900_fixed <- 
  data_frame(Age = "0-4", 
           Lx = sweden_hmd_1900[1, ]$Lx + sweden_hmd_1900[2, ]$Lx) %>%
  bind_rows(sweden_hmd_1900[-c(1,2), ])

## they use a different notation for age ranges: 
## "5-9" in the database = "5-10" in the book
## http://www.mortality.org/Public/ExplanatoryNotes.php#CompleteDataSeries

## nFx; nLx

## apparently, one uses nFx (period age-specific fertility rate)
## by convention (book, p107) 
## but it doesn't actually matter
## so, I just need some sort of age-specific fertility rate
## these data are nfx (cohort age-specific fertility rate)
sweden_hfd <- read_table("data/SWEasfrVH.txt", skip = 2, na = ".")
## alternatively, nFx would be SWEasfrRR.txt

sweden_hfd_1900 <- 
  sweden_hfd %>%
  filter(Cohort == 1900) 

## to get a 5-year age-specific fertility rate
## from a 1-year ASFR, take an average

## 5f10
F10 <- data_frame(Age = "10-14", 
                  Fx = sum(sweden_hfd_1900[1:3, ]$ASFR/5))

## 5f15 through 5f50
## group data frame into 5-row data frames according to
## http://stackoverflow.com/questions/7060272/split-up-a-dataframe-by-number-of-rows
F15_F50 <- 
  sweden_hfd_1900[4:43, ] %>%
  split(rep(1:8, each = 5)) %>%
  lapply(function(x) { 
    data_frame(Age = paste0(x$Age[1], "-", x$Age[5]), 
               Fx = sum(x$ASFR)/5)
  }) %>% 
  bind_rows()

## put it back together
sweden_hfd_1900_5year <- bind_rows(F10, F15_F50)

## join data frames with Lx and Fx
## for 55+ and above or below 10, Fx = 0
sweden <- 
  full_join(sweden_hmd_1900_fixed, sweden_hfd_1900_5year, by = "Age") %>%
  mutate(Fx = ifelse(is.na(Fx), 0, Fx))

## create Leslie matrix
leslie_matrix <- matrix(data = 0, nrow = nrow(sweden), ncol = nrow(sweden))
rownames(leslie_matrix) <- sweden$Age
colnames(leslie_matrix) <- sweden$Age

## calculate diagonal
## a_j+1,j = nLx+n/nLx
for (j in seq(nrow(sweden) - 1)) {
  leslie_matrix[j+1, j] <- sweden$Lx[j+1]/sweden$Lx[j]
}

## calculate first row
f_fab <- 0.4886  ## don't multiply by ffab twice!
l0 <- 100000
for (j in seq(nrow(sweden) - 1)) {
  leslie_matrix[1, j] <- 
    sweden$Lx[1]/(2*l0) * (sweden$Lx[j] * sweden$Fx[j] + 
                             sweden$Lx[j+1] * sweden$Fx[j + 1])/sweden$Lx[j] *
    f_fab
}

## note: non-zero value for 5-9 age group
## because they experience fertility rate for 10-14 age group
## for part of the time
write.csv(leslie_matrix, "sweden_leslie_matrix_5x5.csv")

## if you open the csv in Excel, it interprets some of the names as dates
## because Excel is the spawn of satan
