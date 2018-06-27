#setwd("C:/Users/kishor/Documents/specdata")
  best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",header = TRUE, sep=",",colClasses = "character")
        if(state %in% data[,7]){
          outcomeset <- list()
          outcomeset["heart attack"]<-11
          outcomeset["heart failure"]<-17
          outcomeset["pneumonia"]<-23
          if(exists(outcome ,where=outcomeset)){
            dataOnlySingleState<-data[data[,7]==state,]
            filterData<- subset(dataOnlySingleState,select=c(2,outcomeset[[outcome]]))
            dataCleaning<-filterData[filterData[,2]!="Not Available",]
            dataCleaning[,2] <- as.numeric(dataCleaning[,2])
            print(dataCleaning)
            sortdata<- dataCleaning[order(dataCleaning[,2],dataCleaning[,1]),]
            #print(sortdata)
            #result<-sortdata[nrow(sortdata),][1]
            result<-sortdata[1,][1]
          }else{
            stop("invalid outcome")
          }
          
        }else{
          stop("invalid state")
        }
      return(result)
    }