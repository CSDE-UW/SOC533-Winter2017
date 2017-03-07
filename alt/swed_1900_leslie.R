rm(list=ls())
library(dplyr)

##########################################
#Step 1: Retreive Lx data from life table
##########################################

#read in life table data for Sweden
lt <- read.fwf("data/fltper_5x5.txt",
               widths=c(13,11,8,10,6,9,7,8,9,5),
               skip=3)
#assign column names
names(lt) <- c("Year","Age","mx","qx","ax",
               "lx","dx","Lx","Tx","ex")
#clean
i <- sapply(lt, is.factor)
lt[i] <- lapply(lt[i], as.character)
lt[i] <- lapply(lt[i], trimws)

#extract relevant data
lt <- lt %>% filter(Year=="1900-1904") %>% select(Age,Lx)

###################################################################
#Step 2: Retrieve fertility data and calculate relevant Fx estimates
###################################################################
#read in raw birth x age data
br <- read.fwf("data/SWEbirthsRR.txt",
               widths=c(8,4,11),
               skip=3)
names(br) <- c("Year","Age","births")

#read in raw exposure x age data
denom <- read.fwf("data/SWEexposRR.txt",
               widths=c(8,4,15),
               skip=3)
names(denom) <- c("Year","Age","denom")

#clean
i <- c(FALSE,TRUE,FALSE)
br[i] <- lapply(br[i], as.character)
br[i] <- lapply(br[i], trimws)
denom[i] <- lapply(denom[i], as.character)
denom[i] <- lapply(denom[i], trimws)

#extract relevant data and bind together
br <- br %>% subset(Year %in% c(1900,1901,1902,1903,1904))
denom <- denom %>% subset(Year %in% c(1900,1901,1902,1903,1904))
ft <- cbind(br,denom)
ft <- ft[c(1,2,3,6)]

#also bind a categorical variable for summing into 5 year age bins
ft <- cbind(ft,rep(c(0,0,0,rep(1:8,each=5),9),5))
names(ft) <- c("Year","Age","births","denom","AgeBin")

#calculate Fx as sum of births over sum of 'exposed' females by age bin
ft <- ft %>% group_by(AgeBin) %>% summarise(Fx = sum(births)/sum(denom)) 

#create final Fx and Lx vectors
Fx <- c(0,0,ft$Fx,0,0,0,0,0,0,0,0,0,0,0)
Lx <- c(sum(lt$Lx[1:2]),lt$Lx[3:24])

################################
#Step 3: Generate Leslie matrix
################################

l0 = 100000

agelab <- c("0-4",lt$Age[-1:-2])

lMat <- matrix(0,nrow = length(Fx), ncol = length(Fx))
rownames(lMat) = agelab
colnames(lMat) = agelab

for (i in 1:length(Fx)-1){
  #first row
  lMat[1,i] <- (Lx[1]/(2*l0))*(Fx[i]+(Fx[i+1]*(Lx[i+1]/Lx[i])))*0.4886
  #sub diagonal
  lMat[i+1,i] <- Lx[i+1]/Lx[i]
}

write.csv(lMat,"final_leslie_matrix_Lee.csv")
