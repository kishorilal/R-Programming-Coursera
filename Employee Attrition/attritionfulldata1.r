attrition_FullData1<-function(){
  library(readxl)
  trn <-read_excel("Employee Attrition Dataset_Problem.xlsx", sheet = "Dataset")
  trn<-trn[,-c(1,10,11,23,28)]
  #View(trn)
  trn$Attrition<-ifelse(trn$Attrition =="No",0,1)
  trn$Attrition<-factor(trn$Attrition,levels <-c(0,1))
  trn$BusinessTravel<-as.numeric(factor(trn$BusinessTravel))
  trn$Department<-as.numeric(factor(trn$Department))
  trn$EducationField<-as.numeric(factor(trn$EducationField))
  trn$Gender<-ifelse(trn$Gender =="Male",1,0)
  
  trn$Gender<-factor(trn$Gender,levels<-c(1,0))
  trn$Gender<-as.numeric(trn$Gender)
  trn$JobRole<-as.numeric(factor(trn$JobRole))
  trn$MaritalStatus<-as.numeric(factor(trn$MaritalStatus))
  trn$OverTime<-ifelse(trn$OverTime =="Yes",1,0)
  trn$OverTime<-as.numeric(factor(trn$OverTime,levels <-c(1,0)))
  
  
  glm_model<-glm(Attrition ~.,data=trn,family = "binomial");
  glm_prob<- predict(glm_model,type = "response")
  res<- predict(glm_model,trn,type = "response")
  #create cut_off range
  cut_off <- seq(1:10)*.1
  
  accuracy <- numeric()
  false_pos <- numeric()
  false_neg <- numeric()
  pred_Yes <- numeric()
  pred_No<- numeric()
  for(i in cut_off) {
    d1<-table(ActualValue=trn$Attrition,PredictValue=res>i)
    #print(d1)
    if(NCOL(d1)>1){
      accuracy<-c(accuracy,(d1[1,1]+d1[2,2])/(d1[1,1]+d1[1,2]+d1[2,1]+d1[2,2]))
      false_pos<-c(false_pos,(d1[2,1])/(d1[1,1]+d1[1,2]+d1[2,1]+d1[2,2]))
      false_neg<-c(false_neg,(d1[1,2])/(d1[1,1]+d1[1,2]+d1[2,1]+d1[2,2]))
      pred_Yes <- c(pred_Yes,(d1[2,2])/(d1[2,1]+d1[2,2]))
      pred_No <- c(pred_No,(d1[1,1])/(d1[1,1]+d1[1,2]))
      #print(pred_Yes)
      print(pred_No)
    }
    if(NCOL(d1)==1){
      accuracy<-c(accuracy,(d1[1,1])/(d1[1,1]+d1[2,1]))
      false_pos<-c(false_pos,(d1[2,1])/(d1[1,1]+d1[2,1]))
      false_neg<-c(false_neg,0)
      pred_Yes <- c(pred_Yes,0)
      pred_No <- c(pred_No,(d1[1,1])/(d1[1,1]))
    }
  }
  result <- data.frame(cut_off,accuracy,false_pos,false_neg,pred_Yes,pred_No)
  
  print(result)
  write.xlsx(result,file = "attrition.xlsx", sheetName = "fulldata", 
             col.names = TRUE, row.names = TRUE, append = TRUE)
}

