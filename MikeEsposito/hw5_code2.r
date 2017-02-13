#############################
# Demographic Methods Course 
# HW 5 Code, .R 
#############################

## Set-up
# Attach packages/data, set_wd etc. 
library(dplyr)
setwd("C:/Users/Mike/Dropbox/DemoMethods/HW5/SOC533-Winter2017/MikeEsposito")

# Cohort life table
# Lx values for 5-year intervals 
# i.e., first two intervals [0 & 1-4] changed to single [0 - 4]
Lx = read.table('swede_lt.txt', 
				 skip = 2, 
				 header = TRUE,
				 stringsAsFactors = FALSE) %>%
	tbl_df() %>%
	filter(Year == 1900) %>% 
	mutate(Age2 = ifelse(Age %in% c('0', '1-4'), '0-4', Age)) %>%
	group_by(Age2) %>%
	summarise(Lx2 = sum(Lx)) %>%
	arrange(desc(Lx2)) %>%	 
	select(Age2, Lx2) 

# Cohort birth counts (in 5 year intervals)
cb = read.table('swede_births.txt', 
				 skip = 2, 
				 header = TRUE,
				 stringsAsFactors = FALSE) %>% 	
	 filter(Cohort == 1900) %>%
  	 mutate(Age2 = as.numeric(gsub("[^0-9]","", Age))) %>%
  	 select(Age2, Total) %>%
  	 group_by(gr = cut(Age2, 
  	 				   breaks = seq(0, 110, by = 5), 
  	 				   right = FALSE)) %>%
  	 summarise(B = sum(as.numeric(Total))) 

# Cohort female population exposure
fe = read.table('swede_expo.txt', 
				 skip = 2, 
				 header = TRUE,
				 stringsAsFactors = FALSE) %>%
	 filter(Cohort == 1900) %>%
  	 mutate(Age2 = as.numeric(gsub("[^0-9]","", Age))) %>%
  	 select(Age2, Exposure) %>%
  	 group_by(gr = cut(Age2, 
  	 				   breaks = seq(0, 110, by = 5), 
  	 				   right  = FALSE)) %>%
  	 summarise(Ex = sum(as.numeric(Exposure))) 
	
# Fix up the age labels
lab = c(paste(
			  seq(10, 60 -5, by = 5),
 			  seq(10 + 5 - 1, 60 - 1, by = 5),
 			  sep = '-')
)

# Calaculate ASFRs and bind everything together
dat = cb %>% 
	  cbind(lab) %>%
	  rename(Age = lab) %>%
	  mutate(Age2 = as.character(Age)) %>%
	  inner_join(fe, 'gr') %>%
	  right_join(Lx, 'Age2') %>%
	  mutate(fx = ifelse(is.na(B), 0, B/Ex)) %>%
	  select(Age2, Lx2, B, Ex, fx)

## Constructing the Leslie Matrix 
# 1: Set up empty matrix 
A = matrix(0, nrow = nrow(Lx), ncol = nrow(Lx));
	rownames(A) = Lx$Age2;
	colnames(A) = Lx$Age2

# 2: Calculate the sub-diagonal 
for (i in 1:ncol(A) - 1){
	diag(A[-1,])[i] = (dat$Lx2[i + 1]/dat$Lx2[i])
}

# 3: Calculate the first-row
# note that l0 = 10,000
for (j in 1:ncol(A) - 1){
	A[1, j] = (dat$Lx2[1]/(2*100000))*
			  (dat$fx[j] + dat$fx[j+1]*diag(A[-1,])[j])*
			  0.4886
}

# Take a look
A %>% round(4)