
rm(list=ls())

##directions:
# Please complete the exercise that you started in class about producing a Leslie Matrix.
# More specifically, you are asked to produce a Leslie Matrix for the cohort of Swedish women born in 1900, with a 5-year age interval and 5-year projection step. The data are available in the Human Mortality Database and Human Fertility Database.
# You can use any software you are comfortable with (e.g. Excel, R) including a mix of software (e.g., R to read the data and excel to create the matrix). Please also prepare a "readme" file where you explain the steps that you took to get the data and create the matrix.


library(data.table)
library(popReconstruct)
class_dir <- "C:/Users/xrkulik/Documents/Classes and Trainings/SOC Research Methods in Demography/"

asfr <- fread(paste0(class_dir, "SWEasfrVH.txt"), skip=2)
pop <- fread(paste0(class_dir, "SWE_pop.txt"))
lt <- fread(paste0(class_dir, "SWE_lifetable.txt"))


## prepping population, just keeping folks in the 1900 birth cohort

setnames(pop, names(pop), tolower(names(pop)))
pop[age=="110+", age:= 110]
pop[,age := as.numeric(age)]

# keeping 0 year olds in 1900, 1 year olds in 1901, etc. 
temp <- list()
for(n in 0:110){
  temp[[n+1]] <- pop[year==1900+n & age==0+n]
}
pop <- rbindlist(temp)

# use these populations to female-pop-weight the yearly asfr to 5 year asfr
# even though the age is given as 55+ it probably makes more sense to weight it by the 55 population
# since the fertility is probably 0 for anyone older

setnames(asfr, names(asfr), tolower(names(asfr)))
asfr[age=="12-", age:="12"]
asfr[age=="55+", age:="55"]
asfr[,age := as.numeric(age)]
asfr <- asfr[cohort==1900]

asfr <- merge(asfr, pop, by=c("age"), all=T)
asfr[,cohort:=1900]
# ## start age at 15
# asfr  <- asfr[age>=15]

## collapse to 5 year age groups
for(n in seq(5,110, by=5)){
  five_years <- seq(n,n+4)
  age_var <- paste(n)
  asfr[age %in% five_years, age2:=age_var]
}
asfr[age %in% c("1", "2", "3", "4"), age2 := "1"]
asfr[age == "0", age2:="0"]
asfr[age2 =="110 to 114", age2 := "110"]
asfr[,asfr:=as.numeric(asfr)]
asfr[,cohort := 1900]
asfr[,asfr:=asfr*female]

setkey(asfr, age2, cohort)
asfr <- asfr[,.(asfr=sum(asfr), female=sum(female)), by=key(asfr)]

asfr[,asfr:=asfr/female]
setnames(asfr, "age2", "age")

## the lifetable downloaded was labelled as a cohort lifetable, so I'm assuming that the "Year" 
## variable is the year of the cohort, not the period
setnames(lt, "Lx", "nLx")
setnames(lt, names(lt), tolower(names(lt)))
lt <- lt[year==1900]
lt[,lx:=lx/100000]
lt[,age := sapply(strsplit(age,split= "-"),'[',1)]
lt[age=="110+", age:="110"]
lt <- lt[,.(age, lx)]

d <- merge(asfr, lt, all=T, by="age")

## filling in cohort values, making asfr for 0-14 year olds 0 as well as 60+ year olds
d[,cohort := 1900]
d[is.na(asfr), asfr := 0]
d <- d[age!="0"]
d[age == "1", age:="0"]
d[age=="0", lx := 1]
d[age=="0", female:=60917 + 224727]
d[,age := as.numeric(age)]
d[,asfr := as.numeric(asfr)]
d[,female := as.numeric(female)]
d <- d[order(age)]
d <- d[age<=85]
d <- as.matrix(d)
rownames(d) <- seq(0, 85, by=5)
## running make.leslie.matrix from the PopReconstruct package

les <- make.leslie.matrix(pop = d[,4], surv=d[,5], fert=d[,3],age.int = 5)

make.leslie.matrix(pop = d[,4]
                   ,surv = d[,5]
                   ,fert = d[31]
                   ,srb = 1.05
                   ,age.int = 5)

n.age.grps <- length(pop)
n.surv <- length(surv)

## Make Leslie matrix
lesM <- matrix(0, nrow = n.age.grps, ncol = n.age.grps)

## first row = fert and birth survival
k <- 1/(1+srb) * surv[1] * 0.5
dbl.fert <- age.int*fert + c(age.int*fert[-1], 0) * surv[-1]
lesM[1,] <- k * dbl.fert

## rows 2:(n.age.grps) = survival ratios
lesM[2:n.age.grps,1:(n.age.grps-1)] <- diag(surv[-c(1,n.surv)])
lesM[n.age.grps,n.age.grps] <- surv[n.surv]

if(label.dims) {
  age.labs <- seq(from = 0, by = 5, length = n.age.grps)
  dimnames(lesM) <- list(age.labs, age.labs)
}

## return
return(lesM)


