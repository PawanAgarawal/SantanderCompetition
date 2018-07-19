install.packages("fitdistrplus")
install.packages("data.table")
library(fitdistrplus)
library(data.table)

setwd("D:/Documents and Settings/Pawan68267/Desktop")
library(data.table)
train <- fread("train.csv",header = TRUE)

dataCleaning <- function(data){
  
  #identify variables with zero variance and remove it from the data
  Condition <- sapply(lapply(data,var), function(x) x == 0)
  zeroVar <- names(lapply(data,var)[Condition])
  cleanData <- data[, -c(zeroVar), with = F] #removing zero variance columns
  print(paste0("There are ",length(zeroVar), " variables with zero variance!"))
  
  #Identifying missing variables
  cnames <- colnames(cleanData) #all the column names
  misvar <- vector(mode="character", length=0)
  for(i in 1:length(cnames)){
      if (is.null(cnames[i]) || is.na(cnames[i])){
        misvar <- cbind(misvar,cnames[i])
    }
  }
  if (length(misvar) == 0){
    print("There are no missing values in the data - missvar will be empty!")
  }
  else{
    cleanData <- cleanData[,-c(misvar), with=F] #deleting missing vars as of now
  }

  #identifying duplicate variables
  totnames <- names(cleanData)
  dupVars <- totnames[!totnames %in% names(cleanData[,!duplicated(lapply(cleanData, summary)), with=F])]
  print(paste0("There are ",length(dupVars), " variables with duplicated values!"))
  cleanData <- cleanData[,-c(dupVars), with = F]
  
  return(cleanData)
}

cleanData <- dataCleaning(data = ind_v)

plotdist(train$target,histo=TRUE,demp=TRUE)
descdist(train$target, boot=1000)

fg <- fitdist(train$target,"gamma")
fw <- fitdist(train$target,"weibull")
fln <- fitdist(train$target,"lnorm")
par(mfrow =c(2,2))
plot.legend <- c("Weibull","LogNormal")

denscomp(list(fw,fln), legendtext = plot.legend)
qqcomp(list(fw,fln), legendtext = plot.legend)
cdfcomp(list(fw,fln), legendtext = plot.legend)
ppcomp(list(fw,fln), legendtext = plot.legend)
