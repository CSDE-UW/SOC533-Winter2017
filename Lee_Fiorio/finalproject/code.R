#' Author: Lee Fiorio
#' Date: March 7, 2017
#' Course: SOC 533

#' four functions for simulating individual-level spatial-temporal data with 
#' certain mobility/migration properities

library(reshape2)
library(fields)

#function for obtaining mode from a vector
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#function for simulating Twitter-type spatial-temporal data
#each row represents an person and each column represents a time
#the value of each cell is the person's location at a particular time
make.world <- function(home, away, permanent.stay){
  
  world <- NULL
  for (j in 1:1000){ #number of 'persons' in the simulated 'world'
    
    #initializing person-level variables
    location <- c(1,0) #two location options; 'home' location designated by first element
    away.ct <- 0 #count of the number of times a person is away
    person <- sample(location, 1, prob = c(home, 1-home)) #initial location
    
    for (i in 1:500){ #number of times we observe the location of each 'person'
      
      if (tail(person,1) == location[1]) { #if person was 'home' at t-1
        
        person <- c(person, sample(location, 1, prob = c(home, 1-home))) #draw for their location at t
        away.ct <- 0} #set away.ct to zero
      
      else { 
        
        away.ct <- away.ct + 1  #away count goes up 
        
        if (away.ct == permanent.stay){ #if person has been 'away' a certain number of times
          
          location <- rev(location) #'away' becomes 'home' (i.e. preferered location is reversed)
          person <- c(person, sample(location, 1, prob = c(home, 1-home))) #draw for their location at t
          away.ct <- 0} #set away count back to zero
        
        else { #if person was 'away' at time t hasn't triggered the permanent.stay switch
          
          person <- c(person, sample(rev(location), 1, prob = c(away, 1-away))) #draw for their location at time t
          
        }
      }
    }
    world <- rbind(world, person, deparse.level = 0)
  }
  return(world)
}

#function for determining whether persons in the simulated 
#world have migrated given various definitions of migration (interval and duration)
make.mig.mat <- function(world){
  
  mig.mat <- NULL #the object that will be returned
  
  for(n in 1:nrow(world)){ #loop through each 'person'
    
    mig.est <- NULL #vector of estimates specific to each person
    dur <- NULL #for labeling 
    int <- NULL #for labeling
    
    for (interval in seq(4,104,4)){
      
      for(duration in seq(4,interval,4)){
        
        #one or zero depending on whether location at begining of interval matches location at end of interval
        mig.est <- c(mig.est,(mode(world[n,1:duration]) == mode(world[n,(2+interval):(2+interval+duration)]))*1)
        
        dur <- c(dur,duration)
        int <- c(int,interval)
      }
    }
    mig.mat <- rbind(mig.mat,mig.est, deparse.level = 0)
  }
  return(rbind(int,dur,mig.mat))
} 

#function for plotting migration contours
make.contour.dur_by_int <- function(mig.mat){
  
  plot.new()
  
  rate <- as.data.frame(cbind(mig.mat[1,],
                              mig.mat[2,],
                              1-apply(mig.mat[-c(1:2),],2,mean)))
  
  names(rate) <- c("interval","duration","rate")
  
  rangemin <- floor(min(rate$rate)*10)/10
  rangemax <- ceiling(max(rate$rate)*10)/10
  
  rate <- dcast(rate,interval ~duration)
  
  rate.mat <- rate[,-1]
  rate.mat <- t(as.matrix(rate.mat))
  
  rowlabel<- as.character(seq(2,52,2))
  collabel<- as.character(seq(2,52,2))
  
  image.plot(rate.mat,axes=F,legend.lab="Migration Rate",legend.line=3,zlim=c(rangemin,rangemax))
  
  title(main="Estimated Migration Rate by Interval and Duration")
  
  axis(1, at=seq(0,1, length=26), labels=collabel,las=2)
  axis(2, at=seq(0,1, length=26), labels=rowlabel,las=1)
  
  mtext("Duration in weeks",side=1,line=3,cex=1.4)
  mtext("Interval in weeks",side=2,line=3,cex=1.4)
  
}


