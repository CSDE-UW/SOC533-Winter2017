


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


## going from mx, ax, and qx to full lifetable 

lifetable <- function(data,  preserve_u5=0, cap_qx=0) {
  
  ## checking if data table or data frame to return object of same format
  dframe <- F
  if(!is.data.table(data)){
    dframe <- T
    data <- data.table(data)
  }
  
  ## make flexible with regard to age vs age_group_id, check to make sure all the correct age groups are present
  age_group_id <- F
  lt_ages <- fread("C:/Users/xrkulik/Documents/Classes and Trainings/SOC Research Methods in Demography/agemap.csv")
  if("age_group_id" %in% names(data)){
    age_group_id <- T
    if("age" %in% names(data)) stop ("You have more than one age variable, delete one and try again")
    lt_ages <- lt_ages[,list(age_group_id, age_group_name_short)]
    if(!identical(unique(sort(data$age_group_id)), unique(lt_ages$age_group_id))) stop("You don't have the correct age groups")
    setnames(lt_ages, "age_group_name_short", "age")
    data <- merge(lt_ages, data, by=c("age_group_id"), all.y=T)
  } 
  
  ## make flexible with regard to sex and sex_id
  sex_id<-F
  if("sex_id" %in% names(data)){
    sex_id <- T
    if("sex" %in% names(data)) stop ("You have more than one sex identifier, delete one and try again")
    data[sex_id==1, sex:="male"]
    data[sex_id==2, sex:="female"]
    data[sex_id==3, sex:="both"]
    data[,"sex_id":=NULL]
  }
  
  ## make flexible with regard to year and year_id
  year_id <- F
  if(!"year" %in% names(data)){
    year_id <- T
    setnames(data, "year_id", "year")
  }
  
  ## order data just in case
  data <- data[order(id, sex, year, age)]
  
  if("qx" %in% names(data)){
    data[,qx:=as.numeric(qx)]
  }
  
  data[,mx:=as.numeric(mx)]
  data[,ax:=as.numeric(ax)]
  data[,age:=as.numeric(age)]
  data[,sex:=as.character(sex)]
  
  ## can set up more assurances here (certain things uniquely identify, etc.)
  setkeyv(data,c("id", "sex", "year", "age"))
  num_duplicates <- length(duplicated(data)[duplicated(data)==T])
  if(num_duplicates>0) stop(paste0("You have ",num_duplicates," duplicates of your data over id_vars"))
  
  ## get length of intervals
  data[,n:=unlist(tapply(age, list(id, sex, year), function(x) c(x[-1], max(x))-x))]
  
  ## qx
  if (preserve_u5 == 1) {
    data[age>1, qx:=(n*mx)/(1+(n-ax)*mx)]
  }
  if (preserve_u5 == 0){
    data[,qx:=(n*mx)/(1+(n-ax)*mx)]
  } 
  
  ## setting qx to be 1 for the terminal age group  
  data[age==max(age), qx:=1]
  
  ## px
  data[,px:=1-qx]
  
  ## lx
  data[,lx:=0]
  data[age==0, lx:=100000]
  for (i in 1:length(unique(data$age))) {
    temp <- NULL
    temp <- data$lx*data$px
    temp <- c(0,temp[-length(temp)])
    data[,lx:=0]
    data[,lx:=lx+temp]
    data[age==0, lx:=100000]
  }
  
  ## dx
  dx <- data.table(data)
  setkey(dx,id,sex,year)
  dx <- dx[,c(diff(lx),-1*lx[length(lx)]),by=key(dx)]
  dx <- dx$V1*-1
  data <- cbind(data,dx)
  
  ## nLx
  lx_shift <- data.table(data)
  setkey(lx_shift,id,sex,year)
  lx_shift <- lx_shift[,c(lx[-1],0),by=key(lx_shift)]
  lx_shift <- lx_shift$V1
  data <- cbind(data,lx_shift)
  data[,nLx:=(n*lx_shift)+(ax*dx)]
  data[age==max(age), nLx:=lx/mx]
  data[,lx_shift:=NULL]
  
  ## Tx
  Tx1 <- data.table(data)
  setkey(Tx1,id,sex,year)
  Tx1 <- Tx1[,list(Tx=rev(cumsum(rev(nLx)))),key(Tx1)]
  data[,Tx:=Tx1$Tx]
  
  ## ex
  data[,ex:=Tx/lx]
  
  ## returning in same format
  
  if(sex_id==T){
    data[sex=="male", sex_id:=1]
    data[sex=="female", sex_id:=2]
    data[sex=="both", sex_id:=3]
    data[,sex:=NULL]
  }
  
  if(age_group_id==T){
  data[,age:=NULL]
  }
  
  if(year_id==T){
    setnames(data, "year", "year_id")
  }
  
  if(dframe==T){
    data <- as.data.frame(data)
  }
  
  return(data)
}


