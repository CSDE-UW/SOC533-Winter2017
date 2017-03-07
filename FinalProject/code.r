#########################
# Demographic Methods
# Final project code, .R
#########################

## 1: Set up; i.e., load packages, setwd
library(dplyr)
library(foreign)
library(SAScii)
library(lazyeval)

#setwd("[my-top-level]/SOC 533 Final Project")
setwd("C:/Users/Mike/Dropbox/SOC 533 Final Project")

## 2: Attach data
# Will need a (1) NHIS person file & (2) Period Life-table

# For NHIS:
# Can plug your own urls in here; takes awhile to read, so we just attach a pre-loaded one 

# person_x.file <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/YEAR/personsx.sas"
# person_sas_instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/YEAR/personsx.zip"##
#
# nhis.data <- read.SAScii(person_x.file, 
#						 person_sas_instructions, 
#						 zipped = T)

## E.g., for NHIS  2011
nhis11_instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/personsx.sas"
nhis11_personx 		<- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2011/personsx.zip"##

## store the NHIS file as an R data frame
NHIS.11.personsx.df <- read.SAScii (nhis11_instructions, 
									nhis11_personx, 
									zipped = T)

# Saved 2011 dataframe for demo
# saveRDS(NHIS.11.personsx.df, 'nhis11.rds')
nhis11 = readRDS('nhis11.rds')

# For period life table:
# Pull from HMD: http://www.mortality.org/ (make sure strings aren't factors)

# For example: we pull 5x1 female period life table for 2011 
# see: http://www.mortality.org/cgi-bin/hmd/country.php?cntr=USA&level=1
lt = read.table('lifetable.txt', 
			     skip = 2, 
			     header = TRUE,
			     stringsAsFactors = FALSE) 

# Function for HLE from NHIS and HMD Data
hle.fun <- function(nhis.dat, 
					lt.dat, 
					year, 
					sex, 
					health.var, 
					healthy.status, 
					new_col_name = 'var.bin',
					x.start = 0,
					x.end   = 85, 
					int.length = 5,
					labs = NULL){

	mutate_call = lazyeval::interp(~ifelse(y %in% c(x), 0, 1), 
								   y = as.name(health.var),
								   x = healthy.status)
	
	tmp2 = nhis.dat %>%
		   filter(SEX  == sex) %>% 
		   mutate_(.dots = setNames(list(mutate_call), new_col_name)) %>%
		   group_by(Age2 = cut(AGE_P,
					breaks = c(seq(x.start, x.end, by = int.length), x.end + 1),
					right  = FALSE, 
					labels = labs)) %>%
		   summarise(unhealthy = sum(var.bin)/n()) %>%
		   mutate(healthy = 1 - unhealthy)

	lt = lt.dat %>%
		 tbl_df() %>%
		 filter(Year == year) %>%
		 mutate(Age2 = ifelse(Age %in% c('0', '1-4'), '0-4', Age)) %>%
	 	 group_by(Age2) %>%
	 	 select(Year, Age2, lx, Lx, Tx, ex)

	lt2       = lt[-1,]
	lt2$lx[1] = lt$lx[1] 			
	lt2$Lx[1] = lt$Lx[1] + lt$Lx[2] 
	lt2$Tx[1] = lt$Tx[1]			
	lt2$ex[1] = lt$Tx[1]/lt$lx[1]  

	holder = as.character(x.end) %>% nchar()
	last = lt[ which(substr(as.character(x.end), 1, holder) == substr(lt$Age2, 1, holder)),]
	row.hold = which(substr(as.character(x.end), 1, holder) == substr(lt$Age2, 1, holder))

	last$year = paste0(year)
	last$Age2 = paste0(x.end, '+')
	last$Lx   = sum(lt[row.hold:nrow(lt),]$Lx)
	last$ex   = last$Tx/last$lx

	dat = lt2 %>%
	bind_rows(last) %>%
	right_join(tmp2, 'Age2') 

	HLE = rep(NA, nrow(dat))

	for(i in 1:nrow(dat)){
 		HLE[i]<-(1/dat$lx[i])*sum(dat$healthy[i]*dat$Lx[i:nrow(dat)])
	}

	fin = cbind(dat$Age2, dat$ex, HLE) %>%
		  tbl_df();
		  colnames(fin) = c('x', 'e_x', "HLE_x") 

	fin2 = fin %>% 
		   mutate(e_x2 	= as.numeric(e_x), 
		  		  HLE_x2  = as.numeric(HLE_x)) %>%
		   select(x, e_x2, HLE_x2);
		  colnames(fin2) = c('x', 'e_x', "HLE_x") 

	return(fin2)
}

lab = c(paste(0, 4, sep = '-'),
		paste(
	      seq(5, 85 -5, by = 5),
 	      seq(5 + 5 - 1, 85 - 1, by = 5),
 	      sep = '-'),
		paste('85+'))

# A test run for women, self-rated health in US in 2011
test = hle.fun(readRDS('nhis11.rds'), 
			   lt.dat = lt,
			   year = 2011,
			   sex = 2, 
			   health.var = 'PHSTAT', 
			   healthy.status = c(1,2),
			   labs = lab)

# Change to male p(srh) 
#(note: need to change to male lifetable for acutal values; just an illustration)
test2 = hle.fun(readRDS('nhis11.rds'), 
			   lt.dat = lt,
			   year = 2011,
			   sex = 1, 
			   health.var = 'PHSTAT', 
			   healthy.status = c(1,2),
			   labs = lab)

# Change health limit to good (so less stringent health restriction)
test3 = hle.fun(readRDS('nhis11.rds'), 
			   lt.dat = lt,
			   year = 2011,
			   sex = 2, 
			   health.var = 'PHSTAT', 
			   healthy.status = c(1,2,3),
			   labs = lab)