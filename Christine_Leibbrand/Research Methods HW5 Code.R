setwd("c:users/leibbrce/Documents/SOC533-Winter2017/Christine_Leibbrand")

library(data.table)
#Loading mortality data
hmd_data_hw <- read.table("c:/users/leibbrce/Documents/Demographic Methods/fltcoh_5x5.txt", header=T)
#Loading fertility data
hfd_data_hw <- read.table("c:/users/leibbrce/Documents/Demographic Methods/SWEasfrRR.txt", header=T)

#Subsetting to only include people born in the year 1900
hmd_data_sub <- subset(hmd_data_hw, Year=="1900-1904")
hfd_data_sub <- subset(hfd_data_hw, Year=="1900")
head(hmd_data_sub)
head(hfd_data_sub)

#Creating Leslie matrix
#Creating diagonal for Leslie Matrix
LeslieMat <- c(NA,NA,NA,NA,NA,(hmd_data_sub$Lx[3]/(hmd_data_sub$Lx[2]+hmd_data_sub$Lx[1])),0,0,0,0,0,(hmd_data_sub$Lx[4]/hmd_data_sub$Lx[3]),0,0,0,0,0,(hmd_data_sub$Lx[5]/hmd_data_sub$Lx[4]),0,0,0,0,0,(hmd_data_sub$Lx[6]/hmd_data_sub$Lx[5]),0)
LeslieMat <- t(matrix(LeslieMat, nrow=5, ncol=5))
colnames(LeslieMat) <- c("Ages 0-4","Ages 5-9","Ages 10-14","Ages 15-19","Ages 20-24")
rownames(LeslieMat) <- c("Ages 0-4","Ages 5-9","Ages 10-14","Ages 15-19","Ages 20-24")

#Creating first row of Leslie matrix
#Turning 1-year ASFRs to 5-year ASFRs
asfr_5yr <- c(0,0,(sum(hfd_data_sub$ASFR[1:3])/5),(sum(hfd_data_sub$ASFR[4:8])/5),(sum(hfd_data_sub$ASFR[9:13])/5),sum(hfd_data_sub$ASFR[14:18])/5)

#First row for 0-4-year-olds, no children
a11 <- 0
#First row for 5-9-year-olds, no children
a12 <- 0
#First row for 10-14-year-olds, children for 14-year-olds
a13 <- ((hmd_data_sub$Lx[2]+hmd_data_sub$Lx[1])/(2*hmd_data_sub$lx[1]))*
  (asfr_5yr[3]+(asfr_5yr[4]*LeslieMat[4,3]))*.4886
#First row for 15-19-year-olds
a14 <- ((hmd_data_sub$Lx[2]+hmd_data_sub$Lx[1])/(2*hmd_data_sub$lx[1]))*
  (asfr_5yr[4]+(asfr_5yr[5]*LeslieMat[5,4]))*.4886
#First row for 20-24-year-olds
a15 <- ((hmd_data_sub$Lx[2]+hmd_data_sub$Lx[1])/(2*hmd_data_sub$lx[1]))*
  (asfr_5yr[5]+(asfr_5yr[6]*(hmd_data_sub$Lx[7]/hmd_data_sub$Lx[6])))*.4886

LeslieMat[1,1] <- a11
LeslieMat[1,2] <- a12
LeslieMat[1,3] <- a13
LeslieMat[1,4] <- a14
LeslieMat[1,5] <- a15

LeslieMat

write.csv(LeslieMat, file="LeslieMat.csv", quote=TRUE, eol="\n", na="NA")
