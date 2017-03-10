#Reading in data
library(data.table)
hmd_data <- read.table("c:/users/leibbrce/Documents/Population2.txt", header=T)

#Creating input variables
age <- as.numeric(as.character(hmd_data$Age))
age[is.na(age)] <- 110
year <- hmd_data$Year
nmx <- hmd_data$mx

#Lifetable function
lt <- function(year,age, nmx){
interval <- c(diff(age),999)
nax <- (c(diff(age),999))/2
age_lead <- c(tail(age,-1),rep(NA,1)) 
age_lead[is.na(age_lead)] <- 0
nax[age_lead==1] <- (.07+(1.7*nmx[age==min(age)]))    #Keyfitz and Flieger 1968 equation for calculating nax in first year of life 
nax[age==max(age)] <- 1/nmx[age==max(age)]     #Calculating nax for last interval
nqx <- (interval*nmx)/(1+(nax*nmx))
nqx[age==max(age)] <- 1    #nqx for last interval=1 because everyone dies in last interval
for(i in year){
lx[year==i] <- (c(1,cumprod(1-nqx[year==i])))*100000
  }
ndx <- -(c(diff(lx),999))
ndx[age==max(age)] <- lx[age==max(age)]     #because everyone dies in last interval, death count in last interval=number of initial survivors in that period
lx_lead <- c(tail(lx,-1),rep(NA,1))    #creating lead variable for lx (lx[_n+1])
nLx <- (interval*lx_lead)+(nax*ndx)
nLx[age==max(age)] <- (nax[age==max(age)]*ndx[age==max(age)])   #There is no lead lx for the last interval, so the first half of the equation is removed
for(i in year){
Tx[year==i] <- rev(cumsum(rev(nLx[year==i])))
}
ex <- Tx/lx
ex_plusx <- ex+age
lifetable <- data.frame(year=year, age=age, mx=nmx, qx=nqx,
                        ax=nax, lx=lx, dx=ndx, Lx=nLx, Tx=Tx, ex=ex, lifeexp=ex_plusx)
}

#Creating lifetable
life <- lt(year,age, nmx)

#Comparing results
tail(life)
tail(survivors)

head(life)
head(survivors)
