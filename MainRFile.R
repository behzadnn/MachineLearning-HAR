setwd("D:/Google Drive/Coursera MAchine learning/Project-HumanActivityRecognition")
raw_data<-read.table("pml-training.csv",header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!"))
finalTestData<-read.table("pml-testing.csv",header = TRUE, sep = ",", na.strings = c("NA", "#DIV/0!"))

dim(raw_data)
table(raw_data$classe)

is_na_result =is.na(raw_data)
removedColumns = which(colSums(is_na_result) > 0.9*dim(raw_data)[1])
clean_data1 = raw_data[, -removedColumns]
clean_data = clean_data1[, -c(1:7)]
dim(clean_data)

library(caret)
inTrain = createDataPartition(y=clean_data$classe, p=0.7, list=FALSE)
training = clean_data[inTrain,]
testing = clean_data[-inTrain,]
dim(training);dim(testing);
library(randomForest)
set.seed(555)
fittedModel<-train(classe ~. ,method="rf", data=training)
print(fittedModel)
predictions<-predict(fittedModel, testing)
confusionMatrix(predictions, testing$classe)
correctPrediction<-predictions==testing$classe
predictions_Finaltesting<-predict(fittedModel, finalTestData)
result<-as.character(predictions_Finaltesting)
setwd("D:/Google Drive/Coursera MAchine learning/MachineLearning-HAR/Results")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(result)
#B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
