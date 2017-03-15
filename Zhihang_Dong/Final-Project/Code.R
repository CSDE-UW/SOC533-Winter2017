# CSDE 533 Project
# What is the Probability someone of your family is still alive when you are aging?
# Female ONLY
# What is the Probability your husband/wife die before you?
setwd("~/Desktop/R/dtechprj")

ftable <- read.table("sweden.txt", skip=2, header=TRUE) # Choose your country, download your own file!
mtable <- read.table("mtable.txt", skip=2, header=TRUE)
male <- read.table("male.txt", skip=2, header=TRUE)
female <- read.table("female.txt", skip=2, header=TRUE)
ftable$mx<-as.numeric(as.character(ftable$mx)) # Read the file for females (cohort)
mtable$mx<-as.numeric(as.character(ftable$mx)) # Read the file for males (cohort)
female$mx<-as.numeric(as.character(female$mx)) # Read the file for females (period)
male$mx<-as.numeric(as.character(male$mx)) # Read the file for males (period)



head(ftable)
head(mtable)


mdeaths<-function(yob,mage,dage, rate, alpha,beta){
  if (yob>1923){
    cat("The years you selected is not available in this country... Perhaps yuz
        gonna wait for hundreds of years until this lifetable is published, or
        just give up on your research career haha")
  }
  else if (mage<10){
    cat("Don't make fun of me... To say you are a demographer, please have some
        common sense to realize that modern human beings cannot be pregnant and 
        successfully get a child born before age 10... Is it too hard?")
  }
  else{
  subset1<-subset(ftable, Year == yob & Age == mage)
  subset2<-subset(ftable, Year == yob & Age == (dage+mage))
  lratio<-(subset2$lx)/(subset1$lx)
  rate<-rate
  rate<-as.numeric(as.character(rate))
  subset1$lx<-(subset1$lx/10000)
  g<-function(mage){
    lratio*subset1$mx*subset1$lx*exp(-rate*mage)
  }
  results<-integrate(g, lower = alpha, upper = beta)
  cat("The likelihood of your mother is still alive when you aged", dage,"is ")
  print(results)
  }
}

partner<-function(year,husband,wife,t,choice){
  subset1<-subset(female, Year == year & Age == wife )
  subset2<-subset(male, Year == year & Age == husband)
  subset3<-subset(female, Year == year & Age == (wife+t) )
  subset4<-subset(male, Year == year & Age == (husband+t))
  wife<-as.numeric(as.character(wife))
  husband<-as.numeric(as.character(husband))
  t<-as.numeric(as.character(t))
  if(choice=="w"){
  int<-(subset4$lx/subset2$lx)*(subset3$lx/subset1$lx)*subset3$mx
  result<-int
  cat("In", year, ",if your wife is", wife, "years old and you are", husband, "years old. After ",
      t, "years, the probability of your wife die before you is approximately", result, "\n")
  }
  else if (choice=="h"){
    int<-(subset4$lx/subset2$lx)*(subset3$lx/subset1$lx)*subset4$mx
    int
    result2<-int
    cat("In", year, ",if your husband is", husband, "years old and you are", wife, "years old. After ",
        t, "years, the probability of your husband die before you is approximately", result2, "\n")
  }
  else{
    print("Your selection does not match our expectations.")
  }
}
