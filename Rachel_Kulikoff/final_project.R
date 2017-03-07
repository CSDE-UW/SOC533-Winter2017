#############################################################
## Author: Rachel Kulikoff (with lots of help from Matt Coates)
## Date : 12/3/2015, edited through 6/9/2016
## Purpose: Add Shocks to Mortality Envelope and Lifetables
## Current as so 6.9.2016
## Background/Context/Outline: We have a shocks numbers of deaths file and the "envelope file," which gives number of deaths. 
## This code adds shock deaths to evelope death to generate with-shock envelope.We also have a lifetable file that has 
## mx (mortality rate), qx (probability of death), ax, px, and other lifetable variables, and we need to add shocks to those. 
## This is more complicated-- steps follow:
##    1. This code is parallel by location. That means all inputs need to contain ALL locations, including SDI aggregates, location aggregates
##       and must contain both sexes (or be aggregated to both sexes)
##    2. Most of this is done at the draw level
##    3. Split out enn, lnn, pnn, 1-4 becuase lt has under 1 age group but env has nns. 
##            A.) Pull q_enn, lnn, pnn from age-sex.
##            B.) Convert to m_enn,lnn,pnn
##            C.) add shocks using scalar in mx space
##            D.) Calculate with-shock q_enn,lnn,pnn
##            E.) Aggregate nns into 1q0 using conditional probabilites/equation. 
##            F.) back-calculate 1q0
##            G.) To create mean-level with-shock age-sex outputs to ensure that mean 5q0, q_enn,lnn,pnn,4q1,1q0 with shock are higher than nonshock:
##                      i.)   use data.table to collapse to mean qx values and 95% ui
##                      ii.)  if the location is in the modeling hierarchy, use enn, lnn, pnn, and 1-4 scalars produced from age-sex code,
##                            in which these values are scaled to nonshock 5q0. Multipy with-shock mean q_enn,lnn,pnn, and 1-4 by these values & replace.
##                      iii.) aggregate up q_enn,lnn,pnn,1-4 to q_nn, 1q0, and 5q0 at the mean level, and replace the mean 5q0 calcuated from step Gi with 
##                            this aggregated result    
##    4. Generate a scalar = shocks+deaths/deaths. This scalar * mx gives the with-shock mx for adults. 
##            A.) the lifetables have age-specific groups up to 110 while the envelope only has 80+ so the scalar is not generated for those groups.
##                   i) use the same country-year specific scalar for each of the specific 80+ groups as for the 80+ aggregate
##    5. Use equation to get with-shock qx, use lifetable function to get whole lifetable at draw and mean level
##            A.) Calculate mean mx, ax, and qx
##            B.) Use function to generate mean lifetable
##            C.) Mean life expectancy is the mean of the life expectancy draws not the life expectancy from the mean life table; mean of 4q1 and 1q4 swapped in from step 3



## Required Inputs:
##      - draw level post-reckoning life table all locations
##      - draw level post-reckoning envelope all locations
##      - draw level shock numbers all locations
##      - draw level age-sex results all locations 
##              -(age-sex needs to rerun for aggregate locations post-reckoning to pull from post-reckoning for ax values since aggregation is in mx space)
##      - age-sex scalars for modelled locations


## Produced Outputs: 
##      - draw level with-shock envelope by location
##      - mean level with-shock envelope by location
##      - draw level age-sex results by location
##      - mean level age-sex results with 95% UI by location
##      - mean level 45q15 results with 95% UI by locations
##      - draw level life table by location
##      - mean level life expectancy at 0 and 50 with UI
##      - mean levle life table

############################################################

###########################################################
## Setting up to run on cluster and on computer, arguments
###########################################################

rm(list=ls())
library(data.table)
library(plyr)
library(haven)
library(rhdf5)

if (Sys.info()[1] == 'Windows') {
  username <- "xrkulik"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
  loc <- commandArgs()[3] 
  loc_id <- as.integer(commandArgs()[4])
  output_version_id <- commandArgs()[5] ## shocks version
  env_version <- commandArgs()[5]
  
} 

source(paste0(root,"/Project/Mortality/shared/functions/get_locations.r"))
source(paste0(root,"/Project/Mortality/shared/functions/get_age_map.r"))
source(paste0(root, "Project/Mortality/shared/functions/lt_functions_AT_test.R")) ## generates the rest of the LT from mx, qx, and ax

###########################
## Lifetable functions 
###########################

mx_to_qx <- function(n, mx){
  qx <- 1- exp(-n*mx)
  return(qx)
}

mxax_to_qx <- function(n, ax, mx){
  qx <- (n*mx) / (1+(mx*(n-ax)))
  return(qx)
}

qx_to_mx <- function(n, qx){
  mx <- (-log(1-qx))/n
  return(mx)
}

qxax_to_mx <- function(n, ax, qx){
  mx <- qx/(n-(n*qx) + (ax*qx))
  return(mx)
}

## death numbers
data2 <- fread("J:/temp/xrkulik/class_project/hti_envelope.csv")
agemap <- unique(data2[,c("age_group_id", "age_group_name")])

## shock death numbers
data <- fread("J:/temp/xrkulik/class_project/haiti_shocks.csv")
data <- data[,draw_117:=NULL]
data <- data[,age_group_name := NULL]
setnames(data, "draw_17", "deaths")

## without shock lifetable 
lt <- fread("J:/temp/xrkulik/class_project/hti_lt.csv")

# reshape envelope and lt data wide by draw
data2 <- dcast(data2, location_id + year_id + sex_id + age_group_id +age_group_name ~ paste0("draw_", draw), value.var = "mx_avg_whiv")


## aggregate up the shock deaths to both sexes
agg_shocks <- copy(data)
agg_shocks <- data.table(agg_shocks)
agg_shocks <- melt(agg_shocks, value.name="deaths", id.vars=c("sex_id", "age_group_id", "year_id"), measure.vars="deaths", variable.name="draw")
setkey(agg_shocks, age_group_id, year_id, draw)
agg_shocks <- agg_shocks[,list(deaths=sum(deaths)), by=key(agg_shocks)]
agg_shocks <- dcast(agg_shocks, age_group_id+year_id~draw, value.var="deaths")
agg_shocks <- data.frame(agg_shocks)
agg_shocks$sex_id <- 3

data <- rbind(data, agg_shocks)

## rename the draws of the envelope and shocks so that they match 
setnames(data2, "draw_0", "deaths")
data2 <- data2[,!colnames(data2) %in% c("location_id", "age_group_name")]

#####################################
## Generate the with-shock death numbers
#####################################

## drops the aggregate age groups in the non-shock envelope in order to collapse properly 
data2 <- data2[data2$age_group_id %in% c(2:20, 30:32, 235),]

data <- rbind(data, data2)
data <- as.data.table(data)
setkey(data, age_group_id, sex_id, year_id)
data <- data[,lapply(.SD, sum), by = key(data)]
data <- as.data.frame(data)


##########################################
## Make scalars
## Merge with non-shock LT
## data cleaning
##########################################

## merging the evelope with the envelope+shock to generate scalars
data2 <- melt(data2, id.vars = c("age_group_id", "sex_id", "year_id"), variable.name = "draw", value.name = "env_number") # data 2 is envelope; data is (envelope +shock) from above
data <- melt(data, id.vars = c("age_group_id", "sex_id", "year_id"), variable.name = "draw", value.name = "shocks_plus_env_number")
data <- merge(data, data2, by=c("age_group_id", "sex_id", "year_id", "draw"), all=T)

stopifnot(!is.na(c(data$env_number, data$shocks_plus_env_number))) ## breaks code if there's not a 1:1 match

data$draw = substr(data$draw, 6, 11)
data$shocks_plus_env_number[data$age_group_id %in% c(235, 30, 31, 32) & data$shocks_plus_env_number==0] <- 0.01
data$env_number[data$age_group_id %in% c(235, 30, 31, 32) & data$env_number==0] <- 0.01
data$scalar <- data$shocks_plus_env_number / data$env_number

## bringing in life table flie merging on to scalar file

stopifnot(!is.na(data$age_group_id)) ## breaks code if there are missing age_group_ids

## merging LT with scalars and shocks

lt <- join(lt, data, by= c("year_id", "sex_id", "age_group_id", "draw"), type="full") 

###################################################
###################################################
## Generate the With-Shock Lifetable
###################################################
###################################################

########################################################
## (Step 1) NN and Child LT
########################################################

## Subsetting to make Neonatal and 1-4 dataset
lt <- as.data.frame(lt)
lt_nn <- lt[lt$age_group_id==28 | lt$age_group_id==2 | lt$age_group_id==3 | lt$age_group_id==4,]

## some data prep
qx$year <- qx$year-0.5
qx <- qx[,!colnames(qx) %in% c("ihme_loc_id", "q_u5","location_id")]
qx <- qx[qx$year>=1970,]
qx$sex_id[qx$sex=="male"] <- 1
qx$sex_id[qx$sex=="female"] <- 2
qx$sex_id[qx$sex=="both"] <- 3
qx <- rename(qx, c("simulation" = "draw"))
qx <- qx[,!colnames(qx) %in% "sex"]
qx <- melt(qx, id.vars=c("sex_id", "year", "draw"), variable.name="age", value.name="qx")
qx$age <- substr(qx$age, 3,5)
qx <- qx[qx$age!="ch",] 
qx$age_group_id[qx$age=="enn"] <- 2
qx$age_group_id[qx$age=="lnn"] <- 3
qx$age_group_id[qx$age=="pnn"] <- 4
qx <- qx[,!colnames(qx) %in% "age"]
qx <- rename(qx, c("year"="year_id"))
qx <- qx

## merging qx onto the neonatal LT
lt_nn <- lt_nn[,!colnames(lt_nn) %in% c("qx", "age")]
lt_nn <- join(lt_nn, qx, by=c("age_group_id", "sex_id", "draw", "year_id"), type="full")
if(nrow(lt_nn[is.na(lt_nn$qx) & lt_nn$age_group_id!=28,])!=0) stop ("Missing qx values")
lt_nn$mx <- NA
lt_nn$n[lt_nn$age_group_id==2] <- 7/365 
lt_nn$n[lt_nn$age_group_id==3] <- 21/365
lt_nn$n[lt_nn$age_group_id==4] <- 337/365

lt_nn$mx_non_shock <- qx_to_mx(n=lt_nn$n, qx=lt_nn$qx) ## this is the non-shock mx
lt_nn$mx <- lt_nn$scalar * lt_nn$mx_non_shock ## this gives the with-shock mx
lt_nn <- lt_nn[,!colnames(lt_nn) %in% "mx_non_shock"]
lt_nn$qx <- NA
lt_nn$qx <- mx_to_qx(n=lt_nn$n, mx=lt_nn$mx) ## with-shock qx
if(nrow(lt_nn[is.na(lt_nn$qx) & lt_nn$age_group_id!=28,])!=0) stop("Missing qx values")

lt_nn$px <- 1 - lt_nn$qx ## caluclates px
lt_nn <- lt_nn[order(lt_nn$year_id, lt_nn$sex_id, lt_nn$draw, lt_nn$age_group_id),]

## A bunch of checks to make sure I'm not crazy
if(length(lt_nn[lt_nn$age_group_id==2,])!= length(lt_nn[lt_nn$age_group_id==3,])) stop("Different number of observations per age")
if(length(lt_nn[lt_nn$age_group_id==3,])!= length(lt_nn[lt_nn$age_group_id==28,])) stop("Different number of observations per age")
if(length(lt_nn[lt_nn$age_group_id==28,])!= length(lt_nn[lt_nn$age_group_id==4,])) stop("Different number of observations per age")
stopifnot(is.na(lt_nn$qx[lt_nn$age_group_id==28])) # AT: be careful here...1/13/17

## Conditional Probabilities to get 1q0
lt_nn$qx[lt_nn$age_group_id==28] <- 1-(lt_nn$px[lt_nn$age_group_id==2] * lt_nn$px[lt_nn$age_group_id==3] * lt_nn$px[lt_nn$age_group_id==4]) ## aggregating enn. lnn, pnn into under1
if(nrow(lt_nn[is.na(lt_nn$qx) & lt_nn$age_group_id==28,])!=0) stop("Missing qx values in <1 age group") ## WHY DOESNT THIS WORK
stopifnot(!is.na(lt_nn$qx[lt_nn$age_group_id==28])) 

## 1q0 to 1m0 calculation
lt_nn$mx[lt_nn$age_group_id==28] <-NA ## coerces under 1 mx to missing to make sure that the values are properly replaced 
lt_nn$n[lt_nn$age_group_id==28] <- 1
lt_nn$mx[lt_nn$age_group_id==28] <- qxax_to_mx(n=lt_nn$n[lt_nn$age_group_id==28], qx=lt_nn$qx[lt_nn$age_group_id==28], ax=lt_nn$ax[lt_nn$age_group_id==28]) ## gives under 1 with shock mx
stopifnot(!is.na(lt_nn$mx[lt_nn$age_group_id==28])) ## breaks code if mx are missing
lt_nn <- lt_nn[,!colnames(lt_nn) %in% "px"]

## This is probably redundant but this file is used later
lt_young <- lt_nn[lt_nn$age_group_id %in% c(2,3,4),]
lt_young <- lt_young[,colnames(lt_young) %in% c("year_id", "sex_id", "qx", "draw", "age_group_id", "location_id")]

lt_nn <- lt_nn[lt_nn$age_group_id %in% c(28),]

## Now need to create with-shock mx and qx non neonatal life table so that the under 1 and 1-4 can be appended
lt <- lt[!lt$age_group_id %in% c(28, 2, 3, 4),]

#####################
## Step 2: Adult LT
#####################

if(!loc_id %in% lowest_level$location_id){
  lt <- lt[!lt$age_group_id %in% c(1, 22:26),]
}
calar_95 <- lt[lt$age_group_id==235,] # use 90-94 for 95+... there is no age_group_ie == 235 in the lt AT 1/13/17
scalar_95 <- scalar_95[,colnames(scalar_95) %in% c("sex_id", "year_id", "draw", "scalar")]
scalar_95 <- scalar_95[order(scalar_95$year_id, scalar_95$sex_id, scalar_95$draw),]
scalar_95 <- rename(scalar_95, c("scalar" = "scalar_95"))
lt <- merge(lt, scalar_95, by=c("year_id", "sex_id", "draw"), all.x=T) # this takes a long time
ninetyfive_plus_ages <- c(33,44,45,148)

lt$scalar[(lt$age_group_id %in% ninetyfive_plus_ages) & is.na(lt$scalar)] <- 
  lt$scalar_95[(lt$age_group_id %in% ninetyfive_plus_ages) & is.na(lt$scalar)]
lt <- lt[!colnames(lt) %in% c("scalar_95")]
if(nrow(lt[is.na(lt$scalar),])!=0) stop("Missing scalar values")

lt <- lt[!lt$age_group_id==235,] 

## creates with-shock mx, calculates with-shock qx
lt <- rename(lt, c("mx"="non_shock_mx"))
lt$mx <- NA
lt$mx <- lt$scalar * lt$non_shock_mx
lt <- lt[,!colnames(lt) %in% "non_shock_mx"]
lt$qx <- NA
lt$n[lt$age_group_id!=5] <- 5
lt$n[lt$age_group_id==5] <- 4
lt$qx <- mxax_to_qx(n=lt$n, mx=lt$mx, ax=lt$ax)

if(nrow(lt[is.na(lt$qx),])!=0) stop("Adult qx values are missing")

## append file with with-shock under1 and 1-4 mx and qx to file with the rest of with-shock mx,qx
lt <- rbind(lt, lt_nn)

lt$draw <- as.numeric(lt$draw)
lt <- lt[order(lt$year_id, lt$draw, lt$age_group_id, lt$sex),]
lt <- lt[,colnames(lt) %in% c("year_id", "sex_id", "age_group_id", "draw", "mx", "ax", "qx")]  


##################################
## calcuating with-shock age-sex
##################################

child <- copy(lt)
child <- child[child$age_group_id==5 | child$age_group_id==28,]
child <- child[,!colnames(child) %in% c("mx", "ax")]
child <- rbind(child, lt_young)

## creating q_nn 
nn <- child[child$age_group_id %in% c(2,3),]
nn$px <- 1-nn$qx
nn <- data.table(nn)
setkey(nn,sex_id,year_id,draw)
nn <- as.data.frame(nn[,list(px = prod(px)),by=key(nn)])
nn$qx <- 1 - nn$px
nn$px <- NULL
nn$age_group_id <- 42
child <- rbind(child,nn)

## creating 5q0
u5 <- child[child$age_group_id %in% c(28,5),]
u5$px <- 1-u5$qx
u5 <- data.table(u5)
setkey(u5,sex_id,year_id,draw)
u5 <- as.data.frame(u5[,list(px = prod(px)),by=key(u5)])
u5$qx <- 1 - u5$px
u5$px <- NULL
u5$age_group_id <- 1
child <- rbind(child,u5)

if(nrow(child[is.na(child$qx),])!=0) stop("missing child qx values")

## saving draws
write.csv(child, paste0("/ihme/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/qx/qx_", loc_id, ".csv"), row.names=F)


## mean level age-sex
summ <- child
summ <- data.table(summ)
setkey(summ,sex_id,year_id,age_group_id)
summ <- as.data.frame(summ[,list(qx_mean=mean(qx),qx_lower=quantile(qx,probs=c(.025)),qx_upper=quantile(qx,probs=c(.975))),by=key(summ)])

## replacing iwth mean of middle 99% (or 95% if necessary) if upper > mean
if(nrow(summ[summ$qx_mean>summ$qx_upper,])>0){
  high_draws <- copy(summ)
  high_draws <- summ[summ$qx_mean>summ$qx_upper,]
  high_draws <- high_draws[,colnames(high_draws) %in% c("sex_id", "year_id", "age_group_id")] # why wasn't there age_group_id in this line?
  replace <- copy(child)
  replace <- join(high_draws, replace, type="left")
  low <- quantile(replace$qx, .01) 
  high <- quantile(replace$qx, .99)
  replace <- replace[replace$qx>low,]
  replace <- replace[replace$qx<high,]
  replace <- data.table(replace)
  setkey(replace,sex_id,year_id,age_group_id)
  replace <- as.data.frame(replace[,list(qx_mean_new=mean(qx)),by=key(replace)])
  summ <- join(summ, replace)
  summ$qx_mean[!is.na(summ$qx_mean_new)] <- summ$qx_mean_new[!is.na(summ$qx_mean_new)]
  summ <- summ[,!colnames(summ) %in% c("qx_mean_new")]
  # if that wasn't enough, take the middle 80% of what remains.
  if(nrow(summ[summ$qx_mean>summ$qx_upper,])>0) {
    print("After taking the middle 99% of draws, the upper ui is still less than the mean. Taking the middle 95%.")
    high_draws <- copy(summ)
    high_draws <- summ[summ$qx_mean>summ$qx_upper,]
    high_draws <- high_draws[,colnames(high_draws) %in% c("sex_id", "year_id", "age_group_id")]
    replace <- copy(child)
    replace <- join(high_draws, replace, type="left")
    low <- quantile(replace$qx, .025) 
    high <- quantile(replace$qx, .975)
    replace <- replace[replace$qx>low,]
    replace <- replace[replace$qx<high,]
    replace <- data.table(replace)
    setkey(replace,sex_id,year_id,age_group_id)
    replace <- as.data.frame(replace[,list(qx_mean_new=mean(qx)),by=key(replace)])
    summ <- join(summ, replace)
    summ$qx_mean[!is.na(summ$qx_mean_new)] <- summ$qx_mean_new[!is.na(summ$qx_mean_new)]
    summ <- summ[,!colnames(summ) %in% c("qx_mean_new")] 
  } 
  if(nrow(summ[summ$qx_mean>summ$qx_upper,])>0) stop("Even after taking the middle 95% of draws, the upper ui is still less than the mean")
}


##############################################
## Scaling Under 5 to fix consistency issues
##############################################

mean <- copy(summ)
mean$qx_mean[!mean$age_group_id %in% c(2,3,4,5)] <- NA

# model_locs <- get_locations(level="estimate")
model_locs <- read.csv(paste0(root, "/temp/xrkulik/est.csv"))
if(loc_id %in% model_locs$location_id){
  scale <- read_dta(paste0("/ihme/gbd/WORK/02_mortality/03_models/3_age_sex/add_gpr_feature/data/scaling_numbers/", loc, "_scaling_numbers.dta"))
  scale <- melt(scale, id.vars=c("ihme_loc_id", "sex", "year"), measure.vars=grep("scale", names(scale), value=T), variable.name="age", value.name="scale")
  scale$age_group_id[scale$age=="scale_enn"] <- 2
  scale$age_group_id[scale$age=="scale_lnn"] <- 3
  scale$age_group_id[scale$age=="scale_pnn"] <- 4
  scale$age_group_id[scale$age=="scale_ch"] <- 5
  
  scale$sex_id[scale$sex=="male"] <- 1
  scale$sex_id[scale$sex=="female"] <- 2
  scale$sex_id[scale$sex=="both"] <- 3
  scale$year_id <- floor(scale$year)
  scale <- scale[,!colnames(scale) %in% c("ihme_loc_id", "sex", "year", "age")]
  mean <- merge(mean, scale, by=c("year_id", "sex_id", "age_group_id"), all.x=T)
  if(nrow(mean[is.na(mean$scale) & mean$age_group_id %in% c(2,3,4,5),])!=0) stop("Missing scalar values under 5 lt")
  
  ## scaling with shock most specific age group qx
  mean$qx_mean[mean$age_group_id %in% c(2,3,4,5)] <- mean$qx_mean[mean$age_group_id %in% c(2,3,4,5)] * mean$scale[mean$age_group_id %in% c(2,3,4,5)]
}

## using conditional probabilities to calculate 5q0, q_ch and q_nn
mean$px <- 1 -mean$qx_mean

## under 1
mean_inf <-mean[mean$age_group_id %in% c(2,3,4),]
mean_inf <- data.table(mean_inf)
setkey(mean_inf, year_id, sex_id)
mean_inf <- mean_inf[,c("qx_mean", "qx_upper", "qx_lower", "scale"):=NULL]
mean_inf <- mean_inf[,list(px=prod(px)), by=key(mean_inf)]
mean_inf$qx_inf <- 1-mean_inf$px
mean_inf <- mean_inf[,px:=NULL]

mean <- merge(mean, mean_inf, by=c("year_id","sex_id"), all=T)
mean$qx_mean[mean$age_group_id==28] <- mean$qx_inf[mean$age_group_id==28]
mean$qx_inf <- NULL

## neonatal 
mean_nn <-mean[mean$age_group_id %in% c(2,3),]
mean_nn <- data.table(mean_nn)
setkey(mean_nn, year_id, sex_id)
mean_nn <- mean_nn[,c("qx_mean", "qx_upper", "qx_lower", "scale"):=NULL]
mean_nn <- mean_nn[,list(px=prod(px)), by=key(mean_nn)]
mean_nn$qx_nn <- 1-mean_nn$px
mean_nn <- mean_nn[,px:=NULL]

mean <- merge(mean, mean_nn, by=c("year_id","sex_id"), all=T)
mean$qx_mean[mean$age_group_id==42] <- mean$qx_nn[mean$age_group_id==42]
mean$qx_nn <- NULL

## 5q0
mean_u5 <-mean[mean$age_group_id %in% c(28,5),]
mean_u5$px <- 1-mean_u5$qx_mean
mean_u5 <- data.table(mean_u5)
setkey(mean_u5, year_id, sex_id)
mean_u5 <- mean_u5[,c("qx_mean", "qx_upper", "qx_lower", "scale"):=NULL]
mean_u5 <- mean_u5[,list(px=prod(px)), by=key(mean_u5)]
mean_u5$qx_u5 <- 1-mean_u5$px
mean_u5 <- mean_u5[,px:=NULL]

mean <- merge(mean, mean_u5, by=c("year_id","sex_id"), all=T)
mean$qx_mean[mean$age_group_id==1] <- mean$qx_u5[mean$age_group_id==1]

mean <- mean[,!colnames(mean) %in% c("scale", "px", "qx_u5")]

if(nrow(mean[is.na(mean$qx_mean),])>0) stop("Missing qx values age-sex")
# if(nrow(mean[mean$qx_mean>mean$qx_upper,])>0){
#   n <- nrow(mean)
#   temp <- mean[mean$qx_mean>mean$qx_upper,]
#   temp$qx_mean <- (temp$qx_upper+ temp$qx_lower)/2
#   mean <- mean[!mean$qx_mean>mean$qx_upper,]
#   mean <- rbind(mean, temp)
#   if(n!=nrow(mean)) stop("you lost rows doing hacky thing number 2")
# }
# if(nrow(mean[mean$qx_mean<mean$qx_lower,])>0){
#   n <- nrow(mean)
#   temp <- mean[mean$qx_mean<mean$qx_upper,]
#   temp$qx_mean <- (temp$qx_upper+ temp$qx_lower)/2
#   mean <- mean[!mean$qx_mean<mean$qx_upper,]
#   mean <- rbind(mean, temp)
#   if(n!=nrow(mean)) stop("you lost rows doing hacky thing number 2")
# } 

mean$ihme_loc_id <- loc
############

write.csv(mean,paste0("/ihme/gbd/WORK/02_mortality/03_models/5_lifetables/results/with_shock_products/age_sex/country_files/as_",loc_id,"_summary.csv"),row.names=F)

###############################################

############# calculating 45q15################
# 
adult <- copy(lt)
adult <- adult[adult$age_group_id %in% 8:16,]
adult$px <- 1 - adult$qx
adult <- data.table(adult)
setkey(adult, sex_id, year_id, draw)
adult <- adult[,list(px=prod(px)), by=key(adult)]
adult[,qx_adult:=1-px]
setkey(adult, year_id, sex_id)
adult <- adult[,list(qx_adult_mean = mean(qx_adult), qx_adult_upper=quantile(qx_adult, probs=0.975), qx_adult_lower=quantile(qx_adult, probs=0.025)), by=key(adult)]
adult <- data.frame(adult)
adult$ihme_loc_id <- loc
write.csv(adult, paste0("/ihme/gbd/WORK/02_mortality/03_models/5_lifetables/results/with_shock_products/adult_mortality/45q15_", loc_id, ".csv"),row.names=F)

################################################

## Applying the lifetable function to generate the rest of the lt
## making the format for the lifetable function

# lt_ages <- get_age_map(type="lifetable")
# lt_ages <- lt_ages[,colnames(lt_ages) %in% c("age_group_id", "age_group_name_short")]
lt_ages <- read.csv(paste0(root, "temp/xrkulik/ages.csv"))
lt_ages <- rename(lt_ages, c("age_group_name_short"="age"))
lt <- join(lt, lt_ages, by="age_group_id")
lt <- rename(lt, c("year_id"="year", "draw"="id"))
lt$sex[lt$sex_id==1] <- "male"
lt$sex[lt$sex_id==2] <- "female"
lt$sex[lt$sex_id==3] <- "both"
lt$age <- as.numeric(lt$age)

lt <- lt[!colnames(lt) %in% c("pop", "n", "sex_id", "age_group_id")]
lt <- as.data.frame(lt)
lt_full <- lifetable(data=lt, preserve_u5 = 1, output_anyway=T) ## The with shock LT!! --  removed "cap_qx=1, force_cap=T" AT 2/17/17

if(nrow(lt_full[lt_full$qx>=1 & lt_full$age!=110,]) > 0){
  check_n <- nrow(lt_full) ## make sure we don't lose rows
  fix <- lt_full[lt_full$qx>=1 & lt_full$age!=110,]
  
  fix$qx <- NULL
  fix$qx <- exp(-fix$n * fix$mx)
  if(nrow(fix[is.na(fix$qx),])>0) stop("your fix created missing qx values (1)")
  
  fix$ax <- NULL
  fix$ax <- (fix$qx + (fix$mx * fix$n * (fix$qx-1))) / (fix$mx * fix$qx)
  if(nrow(fix[is.na(fix$ax),])>0) stop("your fix created missing ax values (1)")
  
  lt_full <- lt_full[!(lt_full$qx>=1 & lt_full$age!=110),] ## dropping problematic points to appned back on
  lt_full <- rbind(lt_full, fix)
  if(nrow(lt_full)!=check_n) stop("your fix resulted in a different number of rows than expected (1)")
  
}

lt_full <- lifetable(data=lt_full, preserve_u5 = 1) # call LT function again with fixed qx and ax values


lt_full <- rename(lt_full, c("id"="draw", "year"="year_id"))
lt_full <- lt_full[, !duplicated(colnames(lt_full))] # because the line above duplicates "dx" for some reason.

## calculating life expectancy uncertainty intervals#############
le <- copy(lt_full)
le$sex_id[le$sex=="male"] <- 1
le$sex_id[le$sex=="female"] <- 2
le$sex_id[le$sex=="both"] <- 3
le <- le[le$age %in% c(0, 50, 65), colnames(le) %in% c("draw", "age", "year_id", "sex_id", "ex")]
le <- data.table(le)
setkey(le, year_id, sex_id, age)
le <- le[,list(ex_upper=quantile(ex, probs=0.975), ex_lower=quantile(ex, probs=0.025)), by=key(le)]
#############################################

## and now going back to the standard variables....
lt_full <- join(lt_full, lt_ages, by="age")
lt_full$sex_id[lt_full$sex=="male"] <- 1
lt_full$sex_id[lt_full$sex=="female"] <- 2
lt_full$sex_id[lt_full$sex=="both"] <- 3
lt_full <- lt_full[,!colnames(lt_full) %in% c("age", "sex")]

## adding location variable
lt_full$location_id <- loc_id

## writing draw level lifetables
write.csv(lt_full, paste0("/ihme/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/lt_", loc_id, ".csv"),row.names=F )


## Summary Lt file

## swapping in scaled 1q0 and 4q1 values instead of having them be values of the MLT
q28 <- summ[summ$age_group_id==28,]  ## 1q0
q28 <- q28[,!colnames(q28) %in% "location_id"]
q28 <- rename(q28, c("qx_mean"="qx28"))
q28 <- q28[,!colnames(q28) %in% c("qx_upper", "qx_lower")]

q14 <- summ[summ$age_group_id==5,] ## 4q1
q14 <- q14[,!colnames(q14) %in% "location_id"]
q14 <- rename(q14, c("qx_mean"="qx14"))
q14 <- q14[,!colnames(q14) %in% c("qx_upper", "qx_lower")]


lt_sum <- copy(lt_full)
lt_sum <- as.data.table(lt_sum)
setkey(lt_sum, age_group_id, sex_id, year_id)
lt_sum <- lt_sum[,list(mx = mean(mx),
                       ax = mean(ax),  qx=mean(qx)), by=key(lt_sum)]

lt_sum <- join(lt_sum, q28[,c("sex_id","year_id","age_group_id","qx28")], by=c("age_group_id", "sex_id", "year_id"))
lt_sum <- join(lt_sum, q14[,c("sex_id","year_id","age_group_id","qx14")], by=c("age_group_id", "sex_id", "year_id"))

lt_sum$qx[lt_sum$age_group_id==28] <- lt_sum$qx28[lt_sum$age_group_id==28]
lt_sum$qx[lt_sum$age_group_id==5] <- lt_sum$qx14[lt_sum$age_group_id==5]

lt_sum <- as.data.frame(lt_sum)
lt_sum <- lt_sum[,!colnames(lt_sum) %in% c("qx28", "qx14")]

lt_sum$id <- 1
lt_sum <- rename(lt_sum, c("year_id"="year"))
lt_sum <- join(lt_sum, lt_ages, by="age_group_id")

lt_sum$sex[lt_sum$sex_id==1] <- "male"
lt_sum$sex[lt_sum$sex_id==2] <- "female"
lt_sum$sex[lt_sum$sex_id==3] <- "both"
lt_sum <- lt_sum[!colnames(lt_sum) %in% c("sex_id", "age_group_id")]
lt_sum$age <- as.numeric(lt_sum$age)

lt_sum <- lifetable(data=lt_sum, preserve_u5 =1) ## think about this!
lt_sum <- lt_sum[,!colnames(lt_sum) %in% "id"]

lt_sum$sex_id[lt_sum$sex=="male"] <- 1
lt_sum$sex_id[lt_sum$sex=="female"] <- 2
lt_sum$sex_id[lt_sum$sex=="both"] <- 3
lt_sum <- lt_sum[,!colnames(lt_sum) %in% "sex"]

## getting the le from the mean life table#############
le_mean <- copy(lt_sum)
le_mean <- rename(le_mean, c("ex"= "ex_mean", "year"="year_id"))
if(loc=="AFG"){
  write.csv(le_mean, paste0(root, "temp/xrkulik/test65.csv"), row.names=F)
}
le_mean <- le_mean[le_mean$age %in% c(0, 50, 65), colnames(le_mean) %in% c("age", "year_id", "sex_id", "ex_mean")]
le <- join(le, le_mean, type="full", by=c("age", "year_id", "sex_id")) ## CHECK TO MAKE SURE THIS HAS IHME LOC ID
le$ihme_loc_id <- loc

write.csv(le, paste0("/ihme/gbd/WORK/02_mortality/03_models/5_lifetables/results/with_shock_products/life_expectancy/le_", loc_id, ".csv"),row.names=F)

## and now going back to the standard variables....
lt_sum <- join(lt_sum, lt_ages, by="age")
lt_sum <- lt_sum[,!colnames(lt_sum) %in% c("age", "sex")]
lt_sum$ihme_loc_id <- loc
lt_sum$location_id <- loc_id

write.csv(lt_sum, paste0("/ihme/gbd/WORK/02_mortality/03_models/5_lifetables/results/lt_loc/with_shock/summary_lt_", loc_id, ".csv"), row.names=F )


