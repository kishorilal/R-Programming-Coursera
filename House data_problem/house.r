
house<-function(){
 
  #load read excel library
  library(readxl)
  #load the train data set
  
  trn <-read_excel("Housing Data_Problem.xlsx", sheet = "Train")
  
  #using linear regression model find the independent variavble
  lm_model<-lm(MEDV~.,data=trn)
  
  #removing independent column
  trn$Sno<-NULL
  trn$INDUS<-NULL
  trn$CHAS<-NULL
  trn$AGE<-NULL
  
  #after removing the independent column
  lm_model<-lm(MEDV~.,data=trn)
  
  
  #load the test data set
  evl <-read_excel("Housing Data_Problem.xlsx", sheet = "Evaluation")
  
  #remove the independent column from test data
  evl$Sno<-NULL
  evl$INDUS<-NULL
  evl$CHAS<-NULL
  evl$AGE<-NULL
  
  #using the train data linear model predicting the test data value
  MEDV<-predict(lm_model,evl)
  
  evl$MEDV<-NULL
  evl<-cbind(evl,MEDV)
  
  
  library("xlsx")
  #write the file in disk
  write.xlsx(evl,file = "add.xlsx", sheetName = "predict", 
             col.names = TRUE, row.names = TRUE, append = TRUE)
}