setwd("D:/Google Drive/Coursera MAchine learning/Project-HumanActivityRecognition")
raw_data<-read.csv("pml-training.csv")
finalTestData<-read.csv("pml-testing.csv")
dim(raw_data)
summary(training)
summary(testing)
names(testing)
dim(testing);
dim(training);
table(training$classe)
library(ggplot2);
modFit<-train(classe ~. ,method="rpart", data=training)
print(modFit)
plot(modFit$finalModel,uniform=T,main="classification Tree")
text(modFit$finalModel,use.n = T,all = T,cex=0.8)

#install.packages("rattle")
install.packages("rpart.plot")
library(rattle)
fancyRpartPlot(modFit$finalModel)
prediciton_training<-predict(modFit,newdata = training)
confusionMatrix(prediciton_training,training$classe)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)
library(caret)
training<-vowel.train
testing<-vowel.test

qplot(x.1,x.2,colour=y, data=training)
modFit<-train(y ~. ,method="rf", data=training,prox=T)
print(modFit)
getTree(modFit$finalModel,k=2)

pred<-predict(modFit,testing)
testing$predRight<-pred==testing$Species
table(pred,testing$Species)
qplot(Petal.Width,Sepal.Width,colour=predRight, data=testing)
#B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
