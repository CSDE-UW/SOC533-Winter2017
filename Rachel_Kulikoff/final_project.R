#############################################################
## Author: Rachel Kulikoff 
## Purpose: Add Shocks to Mortality Envelope and Lifetables
## Background/Context/Outline: We have a shocks numbers of deaths file and the "envelope file," which gives number of deaths. 
## This code adds shock deaths to evelope death to generate with-shock envelope.We also have a lifetable file that has 
## mx (mortality rate), qx (probability of death), ax, px, and other lifetable variables, and we need to add shocks to those. 
##    1. Generate a scalar = shocks+deaths/deaths. This scalar * mx gives the with-shock mx for adults. 
##            A.) the lifetables have age-specific groups up to 110 while the envelope only has 95+ so the scalar is not generated for those groups.
##                   i) use the same country-year specific scalar for each of the specific 95+ groups as for the 95+ aggregate
##    2. Use equation to get with-shock qx, use lifetable function to get whole lifetable at draw and mean level

############################################################

##########################
## Set up R, Read in Data
##########################

rm(list=ls())
library(data.table)
library(plyr)
library(ggplot2)
library(haven)

## lifetable helper function
source("C:/Users/xrkulik/Documents/Classes and Trainings/SOC Research Methods in Demography/SOC533-Winter2017/Rachel_Kulikoff/lifetable_helper_functions.R") ## generates the rest of the LT from mx, qx, and ax

## death numbers
data2 <- fread("J:/temp/xrkulik/class_project/hti_envelope.csv")

## shock death numbers
data <- fread("J:/temp/xrkulik/class_project/haiti_shocks.csv")
setnames(data, "draw_117", "deaths")

## without shock lifetable 
lt <- fread("J:/temp/xrkulik/class_project/hti_lt.csv")

# reshape envelope and lt data wide by draw
data2 <- dcast.data.table(data2, location_id + year_id + sex_id + age_group_id +age_group_name ~ paste0("draw_", draw), value.var = "mx_avg_whiv")

## aggregate up the shock deaths to both sexes
agg_shocks <- copy(data)
setkey(agg_shocks, age_group_id, year_id)
agg_shocks <- agg_shocks[,list(deaths=sum(deaths)), by=key(agg_shocks)]
agg_shocks[,sex_id:=3]

data <- rbind(data, agg_shocks)

## aggregate early neonatal, late neonatal, and post neonatal shocks up to under 1 shocks
u1 <- copy(data)
u1 <- data[age_group_id %in% c(2:4)]
setkey(u1, year_id, sex_id)
u1 <- u1[,.(deaths = sum(deaths)), by=key(u1)]
u1[,age_group_id := 28]

data <- rbind(data, u1)

## rename the draws of the envelope and shocks so that they match 
setnames(data2, "draw_0", "deaths")
data2 <- data2[,c("location_id", "age_group_name"):= NULL]

#####################################
## Generate the with-shock death numbers
#####################################

## drops the aggregate age groups in the non-shock envelope in order to collapse properly 
data2 <- data2[age_group_id %in% c(5:20, 28, 30:32, 235)]
data <- data[!age_group_id %in% c(2:4)]

data <- rbind(data, data2)
setkey(data, age_group_id, sex_id, year_id)
data <- data[,lapply(.SD, sum), by = key(data)]
setnames(data, "deaths", "shocks_plus_env_number")

setnames(data2, "deaths", "env_number")

##########################################
## Make scalars
## Merge with non-shock LT
## data cleaning
##########################################

## merging the evelope with the envelope+shock to generate scalars
data <- merge(data, data2, by=c("age_group_id", "sex_id", "year_id"), all=T)

stopifnot(!is.na(c(data$env_number, data$shocks_plus_env_number))) ## breaks code if there's not a 1:1 match

data[age_group_id %in% c(235, 30, 31, 32) & shocks_plus_env_number==0, shocks_plus_env_number:=0.01]
data[age_group_id %in% c(235, 30, 31, 32) & env_number==0, env_number:=0.01]
data[,scalar := shocks_plus_env_number/env_number]

## bringing in life table flie merging on to scalar file

stopifnot(!is.na(data$age_group_id)) ## breaks code if there are missing age_group_ids

## merging LT with scalars and shocks

lt <- merge(lt, data, by= c("year_id", "sex_id", "age_group_id"), all=T) 

###################################################
###################################################
## Generate the With-Shock Lifetable
###################################################
###################################################

scalar_95 <- copy(lt)
scalar_95 <- scalar_95[age_group_id==235] # use 90-94 for 95+... there is no age_group_ie == 235 in the lt AT 1/13/17
scalar_95 <- scalar_95[,.(sex_id, year_id, scalar)]
scalar_95 <- scalar_95[order(year_id, sex_id),]
scalar_95 <- rename(scalar_95, c("scalar" = "scalar_95"))
lt <- merge(lt, scalar_95, by=c("year_id", "sex_id"), all.x=T) 
ninetyfive_plus_ages <- c(33,44,45,148)

lt[age_group_id %in% ninetyfive_plus_ages & is.na(scalar), scalar:= scalar_95]
lt[,scalar_95 := NULL]
if(nrow(lt[is.na(scalar),])!=0) stop("Missing scalar values")

lt <- lt[!age_group_id==235,] 

## creates with-shock mx, calculates with-shock qx
setnames(lt, "mx", "non_shock_mx")
lt[,mx := scalar * non_shock_mx]
lt[,non_shock_mx := NULL]

lt[,qx := NA]
lt[age_group_id == 5, n:= 4]
lt[age_group_id == 28, n:=1]
lt[!age_group_id %in% c(5,28), n:=5]
lt[,qx := mxax_to_qx(n=n, mx=mx, ax=ax)]

if(nrow(lt[is.na(lt$qx),])!=0) stop("qx values are missing")

## append file with with-shock under1 and 1-4 mx and qx to file with the rest of with-shock mx,qx

lt <- lt[order(year_id, draw, age_group_id, sex_id),]
lt <- lt[,.(year_id, sex_id, age_group_id, mx, ax, qx)]  


##################################
## calcuating with-shock 5q0
##################################

u5 <- copy(lt)
u5 <- u5[age_group_id==5 | age_group_id==28,]
u5[,c("mx", "ax"):= NULL]

## creating 5q0
u5[,px := 1-qx]
setkey(u5,sex_id,year_id)
u5 <- u5[,list(px = prod(px)),by=key(u5)]
u5[,qx := 1-px]
u5[,px := NULL]
if(nrow(u5[is.na(qx),])!=0) stop("missing child qx values")

## formatting to save
setnames(u5, "qx", "with_shock_5q0")
u5[,location_name := "Haiti"]
u5[sex_id==1, sex := "male"]
u5[sex_id==2, sex := "femlae"]
u5[sex_id==3, sex := "both"]
u5[,sex_id := NULL]

write.csv(u5, "C:/Users/xrkulik/Documents/Classes and Trainings/SOC Research Methods in Demography/with_shock_5q0.csv", row.names=F)


##################################
## Calculating with shock 45q15
#################################

adult <- copy(lt)
adult <- adult[age_group_id %in% 8:16,]
adult[,px := 1 - qx]
setkey(adult, sex_id, year_id)
adult <- adult[,list(px=prod(px)), by=key(adult)]
adult[,qx_adult:=1-px]
adult[,px := NULL]
if(nrow(adult[is.na(qx_adult),])!=0) stop("missing adult qx values")

## formatting to save
setnames(adult, "qx_adult", "with_shock_45q15")
adult[,location_name := "Haiti"]
adult[sex_id==1, sex := "male"]
adult[sex_id==2, sex := "femlae"]
adult[sex_id==3, sex := "both"]
adult[,sex_id := NULL]

write.csv(adult, "C:/Users/xrkulik/Documents/Classes and Trainings/SOC Research Methods in Demography/with_shock_45q15.csv", row.names=F)

################################################

## Applying the lifetable function to generate the rest of the lt
## making the format for the lifetable function

lt_ages <- fread("J:/temp/xrkulik/ages.csv")
setnames(lt_ages, "age_group_name_short" ,"age")
lt_ages <- lt_ages[,.(age,age_group_id)]

lt[,id := 114]
lt_full <- lifetable(data=lt) ## The with shock LT!! --  removed "cap_qx=1, force_cap=T" AT 2/17/17

lt_full <- merge(lt_full, lt_ages, by="age_group_id")

lt_full[,location_name := "Haiti"]
lt_full[sex_id==1, sex := "male"]
lt_full[sex_id==2, sex := "female"]
lt_full[sex_id==3, sex := "both"]
lt_full[,sex_id := NULL]
lt_full[,age_group_id := NULL]
lt_full[,id := NULL]

write.csv(lt_full, "C:/Users/xrkulik/Documents/Classes and Trainings/SOC Research Methods in Demography/with_shock_lifetable.csv", row.names=F)


############################
## Let's do some graphing
############################

nonshock_lt <- fread("J:/temp/xrkulik/class_project/hti_lt.csv")
nonshock_lt[,location_id := NULL]
setnames(nonshock_lt, "draw", "id")
nonshock_lt <- lifetable(data=nonshock_lt)
nonshock_lt <- nonshock_lt[age_group_id %in% c(28, 18)]
nonshock_lt[,id := NULL]
nonshock_lt[,type := "non_shock"]
nonshock_lt[sex_id==1, sex:="male"]
nonshock_lt[sex_id==2, sex:="female"]
nonshock_lt[sex_id==3, sex:="both"]
nonshock_lt[,sex_id := NULL]
nonshock_lt <- merge(nonshock_lt, lt_ages, by="age_group_id")
nonshock_lt[,age_group_id:=NULL]
nonshock_lt[,location_name := "Haiti"]

lt_full[,type := "with-shock"]
lt_full <- lt_full[age %in% c(0,65)]

gdata <- rbind(lt_full, nonshock_lt)

# ## how about 5q0? 
# 
# child_noshock <-  fread("J:/temp/xrkulik/class_project/hti_lt.csv")
# child_noshock <- child_noshock[age_group_id==5 | age_group_id==28,]
# child_noshock[,c("mx", "ax"):= NULL]
# 
# child_noshock[,px := 1-qx]
# setkey(child_noshock,sex_id,year_id)
# child_noshock <- child_noshock[,list(px = prod(px)),by=key(child_noshock)]
# child_noshock[,qx := 1-px]
# child_noshock[,px := NULL]
# if(nrow(child_noshock[is.na(qx),])!=0) stop("missing child qx values")
# 
# child_noshock[,location_name := "Haiti"]
# child_noshock[sex_id==1, sex := "male"]
# child_noshock[sex_id==2, sex := "femlae"]
# child_noshock[sex_id==3, sex := "both"]
# child_noshock[,sex_id := NULL]
# 

pdf("C:/Users/xrkulik/Documents/Classes and Trainings/SOC Research Methods in Demography/lifetable_graphs.pdf",width=15,height=10) 
  for(sexes in c("male", "female", "both")){
    temp <- gdata[sex==sexes]
      p= ggplot() + geom_line(data=temp, aes(x=year_id, y=ex, color=type)) + 
        facet_wrap(~age, scales="free") +
        labs(title=paste0("Life Expectancy in Haiti ", sexes))
      print(p)
  }  

for(sexes in c("male", "female", "both")){
  temp <- gdata[sex==sexes]
  p= ggplot() + geom_line(data=temp, aes(x=year_id, y=mx, color=type)) + 
    facet_wrap(~age, scales="free") +
    labs(title=paste0("Mortality rate in Haiti ", sexes))
  print(p)
}  

for(sexes in c("male", "female", "both")){
  temp <- gdata[sex==sexes]
  p= ggplot() + geom_line(data=temp, aes(x=year_id, y=qx, color=type)) + 
    facet_wrap(~age, scales="free") +
    labs(title=paste0("Probability of Death in Haiti ", sexes))
  print(p)
}  


dev.off()
