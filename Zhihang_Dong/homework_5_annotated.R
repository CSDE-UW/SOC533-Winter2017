
# create a life table from the Cohort 5x5 data on HMD.
lifetable <- read.table("swe_55_f.txt", skip=2, header=TRUE)
head(lifetable)
# create a table from the HFD indicating the birth counts by cohort
birth <- read.table("swe_bcont_co.txt", skip=2, header=TRUE)
head(birth)
# create a table from the HDF indicating the exposure (total number of females)
exposure <- read.table("swe_expo_co.txt", skip=2, header=TRUE)
head(exposure)

#select only the data for 1900 cohort...
lt <- lifetable[lifetable$Year=="1900-1904",]
bc <- birth[birth$Cohort==1900,]
expo <- exposure[exposure$Cohort==1900,]
head(lt)
head(expo) #Now that we have found it's all from 1900...
#Examine the type of data...
typeof(bc$Total)
typeof(expo$Cohort)
#We need to change them all to the numeric form
bc[,3] <- as.numeric(bc[,3])
expo[,3] <-as.numeric(expo[,3])
typeof(bc$Total)
typeof(expo$Cohort)

#Create a Leslie matrix
les <- matrix(rep(0, 24), nrow=24, ncol=24)
rownames(les) <- lt$Age
colnames(les) <- lt$Age

fert <- rep(0, 24)
names(fert) <- lt$Age

# We arbitrarily set the first three values as 0, assuming no female
# can be pregnant before the age of 10.
fert[4] <- sum(bc[1:3, 3])/sum(expo[1:3, 3])
# Now we just add all of the "middle groups" together - easy to compute!
k <- 4
for(i in 5:12){
  fert[i] <- sum(bc[k:(k+4), 3])/sum(expo[k:(k+4), 3])
  k <- k+5
}
# Note though, because we have divided age groups in lifetable to be 24 
# groups with interval of 5 years, even the fertility data for 55+
# were appended together, because the fertility for 55+ is trivial, we
# assuming it's 0.

for(i in 13:24){
  fert[i] <- 0
}

fert #check out how it is like...

# Compute Leslie matrix entries using age-specific fertility found above and elements from the life table
l0 <- lt[1, "lx"]
for(i in 1:23){
  les[1, i] <- (lt[1, "Lx"]/(2*l0))*(fert[i] + fert[i+1]*(lt[i+1, "Lx"]/lt[i, "Lx"]))
}
les[1, 24] <- 0 # This is just to make sure the last element is right

#Now, the diagnoal is simply calculating L_i+1/L_i
for(i in 1:23){
  les[i+1, i] <- lt[i+1, "Lx"]/lt[i, "Lx"]
}

# Writing a csv file as the output
write.csv(les, file="zdong_les.csv",qmethod = double, fileEncoding = "macroman")