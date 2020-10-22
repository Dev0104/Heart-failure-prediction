install.packages("naivebayes")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caTools)
library(caret)
#read file
train_2v <- read.csv("D:\\DMML\\Data Sets\\healthcare_train_2v.csv", header=T, na.strings=c("","NA"))

#convert to factors
#train_2v$heart_disease <- factor(train_2v$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
#train_2v$hypertension <- factor(train_2v$hypertension, levels = c(0,1), labels = c("No", "Yes"))
train_2v$stroke <- factor(train_2v$stroke, levels = c(0,1), labels = c("No", "Yes"))
#train_2v$smoking_status = as.numeric(train_2v$smoking_status)
summary(train_2v)
str(train_2v$smoking_status)

#Deleting redundant columns.
train_2v <- train_2v[, -c(1,2,6,7,8)]

#any misssing values filled with column mean
anyNA(train_2v)
library(Amelia)
missmap(train_2v)



# Replacing blanks with "unknown", for smoking status
train_2v$smoking_status[train_2v$smoking_status == ""] <- NA
train_2v$smoking_status <- as.factor(ifelse(is.na(train_2v$smoking_status),"Unknown",paste(train_2v$smoking_status)))  


train_2v$bmi<-ifelse(is.na(train_2v$bmi), mean(train_2v$bmi, na.rm = TRUE), train_2v$bmi)

missmap(train_2v)


#Data partitioning
set.seed(123)
ind=sample(2,nrow(train_2v),replace = T,prob=c(0.7,0.3))
train = train_2v[ind==1,]
test = train_2v[ind==2,]
table(train$stroke)
table(test$stroke)
head(train)


library(ROSE)
both = ovun.sample(stroke ~., data = train, method = "both",p = 0.3, N = 1102, seed = 222)$data
prop.table(table(both$stroke))

#Applying Naive bayes
model = naive_bayes(stroke~.,data = both)
model
#plot(model)


#Predict
p = predict(model,train,type = 'prob')
head(cbind(p,train))

#Confusion matrix - train data
p1 = predict(model,train)
(tab1=table(p1,train$stroke))
1-sum(diag(tab1))/sum(tab1)
confusionMatrix(p1,train$stroke)

#Confusion matrix - test data
p2 = predict(model,test)
(tab2=table(p2,test$stroke))
1-sum(diag(tab2))/sum(tab2)
confusionMatrix(p2,test$stroke,mode='everything')


#library(ModelMetrics)
#rms<-rmse(p2,test$stroke)
#rms

#MSE<-mse(p,test$stroke)
#MSE

#mae <- mae (p2,test$stroke)
#mae
