## Savannah Larimore
## Homework 5
## Demographic Methods
## 02/13/2017

setwd("H:/SOC 533 Dem Methods/Homeworks")

rm(list=ls())

#install.packages("dplyr")
library(dplyr)

## Mortality
## read in the data:
mortdata <- read.table("swedenlifetable.txt",
                   skip=2, #to skip the first two lines
                   header=TRUE
)

# Subet 1900 data and only the variables you need to calculate survival
mortdata_1900 <- mortdata %>% filter(Year == "1900") %>% select(Age, Lx)

# Make 0-4 the first five year interval
ages <- matrix(NA,24,2)
ages[1,1] <- "0-4"
ages[1,2] <- 93738 + 349992
as.numeric(ages[1,])
mortdata_1900 <- rbind(as.numeric(ages[1,]),mortdata_1900)
mortdata_1900[,1]<-as.character(mortdata_1900[,1])
mortdata_1900[1,1]<-"0-4"
mortdata_1900<-mortdata_1900[-c(2,3),]

# create empty survival vector
survival <- rep(NA, nrow(mortdata_1900))

# Calculater survival for the sub-diagonal
for(i in 1:nrow(mortdata_1900)){
  survival[i]<-mortdata_1900[i+1,2]/mortdata_1900[i,2]
}

# Check data:
survival

###########################

## Fertility
## read in the data: 
# Number of births
birthdata <- read.table("SWEbirthsVH.txt",
                       skip=2, #to skip the first two lines
                       header=TRUE
)

# Number of person years (exposure)
exposdata <- read.table("SWEexposVH.txt",
                        skip=2, #to skip the first two lines
                        header=TRUE
)

# Subet 1900 data
birthdata_1900 <- birthdata %>% filter(Cohort == "1900")
exposdata_1900 <- exposdata %>% filter(Cohort == "1900")

# Convert to numeric vectors for calculations
birthdata_1900[,3]<-as.numeric(as.character(birthdata_1900[,3]))
exposdata_1900[,3]<-as.numeric(as.character(exposdata_1900[,3])) 

# Create empty matrix for fertility rates:
fertility <- matrix(NA, nrow=nrow(mortdata_1900), ncol=2)

# Fill it in
fertility[,1]<-mortdata_1900[,1]
fertility[1,2]<-0
fertility[2,2]<-0
fertility[3,2]<- (birthdata_1900[1,3]+birthdata_1900[2,3]+birthdata_1900[3,3])/(exposdata_1900[1,3]+exposdata_1900[2,3]+exposdata_1900[3,3])
fertility[4,2]<- sum(birthdata_1900[4:8,3])/sum(exposdata_1900[4:8,3])
fertility[5,2]<- sum(birthdata_1900[9:13,3])/sum(exposdata_1900[9:13,3])
fertility[6,2]<- sum(birthdata_1900[14:18,3])/sum(exposdata_1900[14:18,3])
fertility[7,2]<- sum(birthdata_1900[19:23,3])/sum(exposdata_1900[19:23,3])
fertility[8,2]<- sum(birthdata_1900[24:28,3])/sum(exposdata_1900[24:28,3])
fertility[9,2]<- sum(birthdata_1900[29:33,3])/sum(exposdata_1900[29:33,3])
fertility[10,2]<- sum(birthdata_1900[34:38,3])/sum(exposdata_1900[34:38,3])
fertility[11,2]<- sum(birthdata_1900[39:43,3])/sum(exposdata_1900[39:43,3])
fertility[12:23,2]<- 0


## Calculate values for the first row of the Leslie Matrix:
# Set up a matrix with only the values you need for calculations
calcmatrix <- cbind.data.frame(fertility, mortdata_1900[,2])
head(calcmatrix)

# caculate the constants
c <-calcmatrix[1,3]/(2*100000)
FAB<-0.4886 

# Convert to a numerica vector for calculations
calcmatrix[,2]<-as.numeric(as.character(calcmatrix[,2]))

#leslie_matrix[1,1] <- c*(calcmatrix[1,2]+(calcmatrix[2,2]*(calcmatrix[2,3]/calcmatrix[1,3])))*FAB
#leslie_matrix[1,2] <- c*(calcmatrix[2,2]+(calcmatrix[3,2]*(calcmatrix[3,3]/calcmatrix[2,3])))*FAB
# Set up an empty vector to store results
row1<-rep(NA,23)

for(i in 1:nrow(calcmatrix)){
  row1[i]<- c*(calcmatrix[i,2]+(calcmatrix[i+1,2]*(calcmatrix[i+1,3]/calcmatrix[i,3])))*FAB
}

# Check calculations
row1

########################

# Set up an empty Leslie Matrix
leslie_matrix<-matrix(NA,23,23)

# Fill in the first row:
leslie_matrix[1,] <- row1
leslie_matrix[1,23] <- 0

# Fill int the subdiagonal:
leslie_matrix[2,1] <- survival[1]
leslie_matrix[3,2] <- survival[2]
leslie_matrix[4,3] <- survival[3]
leslie_matrix[5,4] <- survival[4]
leslie_matrix[6,5] <- survival[5]
leslie_matrix[7,6] <- survival[6]
leslie_matrix[8,7] <- survival[7]
leslie_matrix[9,8] <- survival[8]
leslie_matrix[10,9] <- survival[9]
leslie_matrix[11,10] <- survival[10]
leslie_matrix[12,11] <- survival[11]
leslie_matrix[13,12] <- survival[12]
leslie_matrix[14,13] <- survival[13]
leslie_matrix[15,14] <- survival[14]
leslie_matrix[16,15] <- survival[15]
leslie_matrix[17,16] <- survival[16]
leslie_matrix[18,17] <- survival[17]
leslie_matrix[19,18] <- survival[18]
leslie_matrix[20,19] <- survival[19]
leslie_matrix[21,20] <- survival[20]
leslie_matrix[22,21] <- survival[21]
leslie_matrix[23,22] <- survival[22]

# Change all remaining NA to zeros
leslie_matrix[is.na(leslie_matrix)]<-0

# Check to make sure everything looks okay:
View(leslie_matrix)

# export to csv:
# library(xlsx)
write.csv(leslie_matrix, "H:/SOC 533 Dem Methods/Homeworks/lesliematrix.csv")
